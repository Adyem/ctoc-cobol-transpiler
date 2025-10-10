#include <cstddef>

#include "transpiler_semantics.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "transpiler_logging.hpp"

typedef enum e_transpiler_semantic_data_kind
{
    TRANSPILE_SEMANTIC_DATA_UNKNOWN = 0,
    TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC,
    TRANSPILE_SEMANTIC_DATA_NUMERIC,
    TRANSPILE_SEMANTIC_DATA_FLOATING
}   t_transpiler_semantic_data_kind;

typedef struct s_transpiler_semantic_data_item
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_semantic_data_kind kind;
    size_t declared_length;
    int is_read_only;
}   t_transpiler_semantic_data_item;

typedef struct s_transpiler_semantic_scope
{
    t_transpiler_semantic_data_item *items;
    size_t item_count;
    size_t item_capacity;
}   t_transpiler_semantic_scope;

typedef int (*t_transpiler_semantics_value_classifier)(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind,
    size_t *out_length);

static t_transpiler_data_item_kind transpiler_semantics_convert_kind(t_transpiler_semantic_data_kind kind);
static t_transpiler_semantic_data_kind transpiler_semantics_kind_from_context(t_transpiler_data_item_kind kind);
static int transpiler_semantics_is_numeric_kind(t_transpiler_semantic_data_kind kind);
static int transpiler_semantics_kinds_compatible(t_transpiler_semantic_data_kind left,
    t_transpiler_semantic_data_kind right);
static int transpiler_semantics_register_copybook(const t_ast_node *node,
    t_transpiler_semantic_scope *scope, t_transpiler_context *context);
static int transpiler_semantics_validate_statement_sequence(const t_ast_node *sequence,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context);
static int transpiler_semantics_validate_statement(const t_ast_node *statement,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context);
static int transpiler_semantics_validate_condition(const t_ast_node *condition,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context);
static int transpiler_semantics_classify_condition_value(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length);
static int transpiler_semantics_emit_invalid_expression(t_transpiler_context *context,
    const char *message);
static int transpiler_semantics_classify_arithmetic_expression(const t_ast_node *expression,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantics_value_classifier classifier,
    t_transpiler_semantic_data_kind *out_kind, size_t *out_length);
static int transpiler_semantics_classify_move_value(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length);

static void transpiler_semantics_scope_init(t_transpiler_semantic_scope *scope)
{
    if (!scope)
        return ;
    scope->items = NULL;
    scope->item_count = 0;
    scope->item_capacity = 0;
}

static void transpiler_semantics_scope_dispose(t_transpiler_semantic_scope *scope)
{
    if (!scope)
        return ;
    if (scope->items)
        cma_free(scope->items);
    scope->items = NULL;
    scope->item_count = 0;
    scope->item_capacity = 0;
}

static int transpiler_semantics_scope_reserve(t_transpiler_semantic_scope *scope, size_t desired_capacity)
{
    t_transpiler_semantic_data_item *new_items;
    size_t index;

    if (!scope)
        return (FT_FAILURE);
    if (scope->item_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_items = static_cast<t_transpiler_semantic_data_item *>(cma_calloc(desired_capacity,
        sizeof(*new_items)));
    if (!new_items)
        return (FT_FAILURE);
    index = 0;
    while (index < scope->item_count)
    {
        ft_strlcpy(new_items[index].name, scope->items[index].name, TRANSPILE_IDENTIFIER_MAX);
        new_items[index].kind = scope->items[index].kind;
        new_items[index].declared_length = scope->items[index].declared_length;
        new_items[index].is_read_only = scope->items[index].is_read_only;
        index += 1;
    }
    if (scope->items)
        cma_free(scope->items);
    scope->items = new_items;
    scope->item_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static const t_transpiler_semantic_data_item *transpiler_semantics_scope_find(
    const t_transpiler_semantic_scope *scope, const char *name)
{
    size_t index;

    if (!scope)
        return (NULL);
    if (!name)
        return (NULL);
    index = 0;
    while (index < scope->item_count)
    {
        if (ft_strncmp(scope->items[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (&scope->items[index]);
        index += 1;
    }
    return (NULL);
}

static int transpiler_semantics_emit_error(t_transpiler_context *context, int code, const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, code, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, code);
    return (FT_FAILURE);
}

static int transpiler_semantics_emit_invalid_expression(t_transpiler_context *context,
    const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    return (transpiler_semantics_emit_error(context,
        TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION, message));
}

static int transpiler_semantics_register_data_item(t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *name, t_transpiler_semantic_data_kind kind, size_t declared_length, int is_read_only)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    const t_transpiler_semantic_data_item *existing;
    t_transpiler_semantic_data_item *item;

    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (!name || name[0] == '\0')
    {
        pf_snprintf(message, sizeof(message),
            "data item declaration is missing an identifier");
        return (transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message));
    }
    existing = transpiler_semantics_scope_find(scope, name);
    if (existing)
    {
        pf_snprintf(message, sizeof(message),
            "data item '%s' declared multiple times in WORKING-STORAGE", name);
        return (transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM, message));
    }
    if (scope->item_count >= scope->item_capacity)
    {
        if (transpiler_semantics_scope_reserve(scope, scope->item_capacity == 0 ? 4 : scope->item_capacity * 2)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &scope->items[scope->item_count];
    ft_strlcpy(item->name, name, TRANSPILE_IDENTIFIER_MAX);
    item->kind = kind;
    item->declared_length = declared_length;
    item->is_read_only = is_read_only;
    if (transpiler_context_register_data_item(context, name,
            transpiler_semantics_convert_kind(kind), declared_length, is_read_only) != FT_SUCCESS)
    {
        item->name[0] = '\0';
        item->kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
        item->declared_length = 0;
        item->is_read_only = 0;
        return (FT_FAILURE);
    }
    scope->item_count += 1;
    return (FT_SUCCESS);
}

static const char *transpiler_semantics_kind_to_string(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
        return ("alphanumeric");
    if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        return ("numeric");
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return ("floating");
    return ("unknown");
}

static t_transpiler_semantic_data_kind transpiler_semantics_classify_picture(const char *text)
{
    size_t index;
    int has_alpha;
    int has_numeric;
    int has_decimal;

    if (!text)
        return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
    has_alpha = 0;
    has_numeric = 0;
    has_decimal = 0;
    index = 0;
    while (text[index] != '\0')
    {
        char value;

        value = text[index];
        if (value >= 'a' && value <= 'z')
            value = static_cast<char>(value - ('a' - 'A'));
        if (value == 'X' || value == 'A' || value == 'Z')
            has_alpha = 1;
        if (value == '9' || value == 'S' || value == 'V')
        {
            has_numeric = 1;
            if (value == 'V')
                has_decimal = 1;
        }
        index += 1;
    }
    if (has_decimal && !has_alpha)
        return (TRANSPILE_SEMANTIC_DATA_FLOATING);
    if (has_numeric && !has_alpha)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (has_alpha && !has_numeric)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    if (has_decimal)
        return (TRANSPILE_SEMANTIC_DATA_FLOATING);
    if (has_numeric)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (has_alpha)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
}

static t_transpiler_semantic_data_kind transpiler_semantics_classify_literal(const t_ast_node *literal)
{
    if (!literal)
        return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
    if (literal->token.kind == LEXER_TOKEN_NUMERIC_LITERAL)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (literal->token.kind == LEXER_TOKEN_STRING_LITERAL)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
}

static size_t transpiler_semantics_picture_alphanumeric_length(const char *text)
{
    size_t index;
    size_t length;
    char last_symbol;

    if (!text)
        return (0);
    index = 0;
    length = 0;
    last_symbol = '\0';
    while (text[index] != '\0')
    {
        char value;

        value = text[index];
        if (value >= 'a' && value <= 'z')
            value = static_cast<char>(value - ('a' - 'A'));
        if (value == 'X' || value == 'A' || value == 'Z')
        {
            if (length < SIZE_MAX)
                length += 1;
            last_symbol = value;
            index += 1;
            continue ;
        }
        if (value == '(' && last_symbol != '\0')
        {
            size_t repeat_index;
            size_t repeat_value;
            int has_digits;

            repeat_index = index + 1;
            repeat_value = 0;
            has_digits = 0;
            while (text[repeat_index] >= '0' && text[repeat_index] <= '9')
            {
                size_t digit;

                digit = static_cast<size_t>(text[repeat_index] - '0');
                if (repeat_value > (SIZE_MAX - digit) / 10)
                    repeat_value = SIZE_MAX;
                else
                    repeat_value = repeat_value * 10 + digit;
                has_digits = 1;
                repeat_index += 1;
            }
            if (!has_digits || text[repeat_index] != ')')
            {
                index += 1;
                last_symbol = '\0';
                continue ;
            }
            if (repeat_value == SIZE_MAX)
                return (SIZE_MAX);
            if (repeat_value > 0)
            {
                size_t increment;

                increment = repeat_value - 1;
                if (increment > SIZE_MAX - length)
                    return (SIZE_MAX);
                length += increment;
            }
            index = repeat_index + 1;
            continue ;
        }
        last_symbol = '\0';
        index += 1;
    }
    return (length);
}

static size_t transpiler_semantics_literal_alphanumeric_length(const t_ast_node *literal)
{
    size_t length;

    if (!literal)
        return (0);
    if (literal->kind != AST_NODE_LITERAL)
        return (0);
    if (literal->token.kind != LEXER_TOKEN_STRING_LITERAL)
        return (0);
    if (!literal->token.lexeme)
        return (0);
    length = ft_strlen(literal->token.lexeme);
    if (length >= 2)
    {
        char first;
        char last;

        first = literal->token.lexeme[0];
        last = literal->token.lexeme[length - 1];
        if ((first == '"' && last == '"') || (first == '\'' && last == '\''))
        {
            if (length >= 2)
                length -= 2;
        }
    }
    return (length);
}

static t_transpiler_data_item_kind transpiler_semantics_convert_kind(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
        return (TRANSPILE_DATA_ITEM_ALPHANUMERIC);
    if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        return (TRANSPILE_DATA_ITEM_NUMERIC);
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return (TRANSPILE_DATA_ITEM_FLOATING);
    return (TRANSPILE_DATA_ITEM_UNKNOWN);
}

static t_transpiler_semantic_data_kind transpiler_semantics_kind_from_context(t_transpiler_data_item_kind kind)
{
    if (kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    if (kind == TRANSPILE_DATA_ITEM_NUMERIC)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (kind == TRANSPILE_DATA_ITEM_FLOATING)
        return (TRANSPILE_SEMANTIC_DATA_FLOATING);
    return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
}

static int transpiler_semantics_is_numeric_kind(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        return (1);
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return (1);
    return (0);
}

static int transpiler_semantics_kinds_compatible(t_transpiler_semantic_data_kind left,
    t_transpiler_semantic_data_kind right)
{
    if (left == TRANSPILE_SEMANTIC_DATA_UNKNOWN || right == TRANSPILE_SEMANTIC_DATA_UNKNOWN)
        return (1);
    if (left == right)
        return (1);
    if ((left == TRANSPILE_SEMANTIC_DATA_NUMERIC && right == TRANSPILE_SEMANTIC_DATA_FLOATING)
        || (left == TRANSPILE_SEMANTIC_DATA_FLOATING && right == TRANSPILE_SEMANTIC_DATA_NUMERIC))
        return (1);
    return (0);
}

static int transpiler_semantics_collect_data_items(const t_ast_node *section, t_transpiler_semantic_scope *scope,
    t_transpiler_context *context)
{
    size_t index;
    const t_ast_node *child;
    int status;

    if (!section)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(section))
    {
        child = ast_node_get_child(section, index);
        if (child && child->kind == AST_NODE_DATA_ITEM)
        {
            const t_ast_node *name_node;
            const t_ast_node *picture_node;
            const t_ast_node *level_node;
            size_t name_index;

            name_node = NULL;
            picture_node = NULL;
            level_node = NULL;
            name_index = 0;
            while (name_index < ast_node_child_count(child))
            {
                const t_ast_node *candidate;

                candidate = ast_node_get_child(child, name_index);
                if (candidate && candidate->kind == AST_NODE_IDENTIFIER)
                    name_node = candidate;
                else if (candidate && candidate->kind == AST_NODE_PICTURE_CLAUSE)
                    picture_node = candidate;
                else if (candidate && candidate->kind == AST_NODE_LITERAL && !level_node)
                    level_node = candidate;
                name_index += 1;
            }
            if (name_node && name_node->token.lexeme)
            {
                t_transpiler_semantic_data_kind kind;
                size_t declared_length;
                int is_read_only;

                kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
                declared_length = 0;
                is_read_only = 0;
                if (level_node && level_node->token.lexeme)
                {
                    int level_value;

                    level_value = ft_atoi(level_node->token.lexeme);
                    if (level_value == 78)
                        is_read_only = 1;
                }
                if (picture_node && picture_node->token.lexeme)
                {
                    kind = transpiler_semantics_classify_picture(picture_node->token.lexeme);
                    if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
                        declared_length = transpiler_semantics_picture_alphanumeric_length(picture_node->token.lexeme);
                }
                if (transpiler_semantics_register_data_item(scope, context, name_node->token.lexeme,
                        kind, declared_length, is_read_only) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            else
            {
                char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                pf_snprintf(message, sizeof(message),
                    "data item at line %zu is missing a name", child->token.line);
                transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
                status = FT_FAILURE;
            }
        }
        else if (child && child->kind == AST_NODE_COPYBOOK_INCLUDE)
        {
            if (transpiler_semantics_register_copybook(child, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}

static int transpiler_semantics_register_copybook(const t_ast_node *node,
    t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    const t_transpiler_copybook *copybook;
    const t_ast_node *name_node;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t index;
    int status;

    if (!node)
        return (FT_FAILURE);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    name_node = ast_node_get_child(node, 0);
    if (!name_node || name_node->kind != AST_NODE_IDENTIFIER || !name_node->token.lexeme)
    {
        pf_snprintf(message, sizeof(message),
            "COPY directive is missing a copybook identifier");
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        return (FT_FAILURE);
    }
    copybook = transpiler_context_find_copybook(context, name_node->token.lexeme);
    if (!copybook)
    {
        pf_snprintf(message, sizeof(message),
            "copybook '%s' is not registered in the current compilation context",
            name_node->token.lexeme);
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_UNKNOWN_COPYBOOK, message);
        return (FT_FAILURE);
    }
    status = FT_SUCCESS;
    index = 0;
    while (index < copybook->item_count)
    {
        const t_transpiler_copybook_item *item;
        t_transpiler_semantic_data_kind kind;

        item = &copybook->items[index];
        kind = transpiler_semantics_kind_from_context(item->kind);
        if (transpiler_semantics_register_data_item(scope, context, item->name,
                kind, item->declared_length, item->is_read_only) != FT_SUCCESS)
            status = FT_FAILURE;
        index += 1;
    }
    return (status);
}

static int transpiler_semantics_collect_scope(const t_ast_node *program, t_transpiler_semantic_scope *scope,
    t_transpiler_context *context)
{
    size_t index;
    const t_ast_node *data_division;
    int status;

    if (!program)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    data_division = NULL;
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_DATA_DIVISION)
        {
            data_division = candidate;
            break ;
        }
        index += 1;
    }
    if (!data_division)
        return (FT_SUCCESS);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(data_division))
    {
        const t_ast_node *child;

        child = ast_node_get_child(data_division, index);
        if (child && child->kind == AST_NODE_WORKING_STORAGE_SECTION)
        {
            if (transpiler_semantics_collect_data_items(child, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}

static int transpiler_semantics_validate_identifier_use(const t_transpiler_semantic_scope *scope,
    t_transpiler_context *context, const t_ast_node *identifier, int is_target,
    t_transpiler_semantic_data_kind *out_kind, size_t *out_length, int *out_is_read_only)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    const t_transpiler_semantic_data_item *item;

    if (!identifier)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (!identifier->token.lexeme)
    {
        pf_snprintf(message, sizeof(message),
            "MOVE %s is missing an identifier", is_target ? "target" : "source");
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        return (FT_FAILURE);
    }
    item = transpiler_semantics_scope_find(scope, identifier->token.lexeme);
    if (!item)
    {
        pf_snprintf(message, sizeof(message),
            "identifier '%s' referenced in MOVE is not declared in WORKING-STORAGE", identifier->token.lexeme);
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER, message);
        return (FT_FAILURE);
    }
    if (out_kind)
        *out_kind = item->kind;
    if (out_length)
        *out_length = item->declared_length;
    if (out_is_read_only)
        *out_is_read_only = item->is_read_only;
    return (FT_SUCCESS);
}

static int transpiler_semantics_emit_invalid_condition(t_transpiler_context *context,
    const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    return (transpiler_semantics_emit_error(context,
        TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION, message));
}

static int transpiler_semantics_classify_condition_identifier(const t_ast_node *identifier,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length)
{
    const t_transpiler_semantic_data_item *item;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!identifier)
        return (FT_FAILURE);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (!role)
        return (FT_FAILURE);
    if (!identifier->token.lexeme)
    {
        pf_snprintf(message, sizeof(message),
            "condition %s is missing an identifier", role);
        return (transpiler_semantics_emit_invalid_condition(context, message));
    }
    item = transpiler_semantics_scope_find(scope, identifier->token.lexeme);
    if (!item)
    {
        pf_snprintf(message, sizeof(message),
            "condition %s identifier '%s' is not declared in WORKING-STORAGE",
            role, identifier->token.lexeme);
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER, message);
        return (FT_FAILURE);
    }
    if (out_kind)
        *out_kind = item->kind;
    if (out_length)
        *out_length = item->declared_length;
    return (FT_SUCCESS);
}

static int transpiler_semantics_classify_arithmetic_expression(const t_ast_node *expression,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantics_value_classifier classifier,
    t_transpiler_semantic_data_kind *out_kind, size_t *out_length)
{
    const t_ast_node *left;
    const t_ast_node *operator_node;
    const t_ast_node *right;
    t_transpiler_semantic_data_kind left_kind;
    t_transpiler_semantic_data_kind right_kind;
    size_t left_length;
    size_t right_length;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    char left_role[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    char right_role[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    const char *expression_role;
    t_lexer_token_kind operator_kind;
    const char *operator_text;
    int status;

    if (out_kind)
        *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    if (out_length)
        *out_length = 0;
    expression_role = (role && role[0] != '\0') ? role : "arithmetic expression";
    if (!expression)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (!classifier)
        return (FT_FAILURE);
    if (ast_node_child_count(expression) < 3)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing operands or operator", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    left = ast_node_get_child(expression, 0);
    operator_node = ast_node_get_child(expression, 1);
    right = ast_node_get_child(expression, 2);
    if (role && role[0] != '\0')
    {
        pf_snprintf(left_role, sizeof(left_role), "%s left operand", role);
        pf_snprintf(right_role, sizeof(right_role), "%s right operand", role);
    }
    else
    {
        ft_strlcpy(left_role, "left operand", sizeof(left_role));
        ft_strlcpy(right_role, "right operand", sizeof(right_role));
    }
    left_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    right_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    left_length = 0;
    right_length = 0;
    status = FT_SUCCESS;
    if (classifier(left, scope, context, left_role, &left_kind, &left_length) != FT_SUCCESS)
        status = FT_FAILURE;
    if (classifier(right, scope, context, right_role, &right_kind, &right_length) != FT_SUCCESS)
        status = FT_FAILURE;
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing an arithmetic operator", expression_role);
        if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
            status = FT_FAILURE;
        else
            status = FT_FAILURE;
        return (status);
    }
    operator_kind = operator_node->token.kind;
    operator_text = operator_node->token.lexeme ? operator_node->token.lexeme : "+";
    if (operator_kind == LEXER_TOKEN_PLUS)
    {
        if (left_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && !transpiler_semantics_is_numeric_kind(left_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires numeric or floating operands but %s is %s",
                operator_text, left_role,
                transpiler_semantics_kind_to_string(left_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (right_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && !transpiler_semantics_is_numeric_kind(right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires numeric or floating operands but %s is %s",
                operator_text, right_role,
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS)
        {
            if (out_kind)
            {
                if (left_kind == TRANSPILE_SEMANTIC_DATA_FLOATING
                    || right_kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
                    *out_kind = TRANSPILE_SEMANTIC_DATA_FLOATING;
                else if (transpiler_semantics_is_numeric_kind(left_kind)
                    || transpiler_semantics_is_numeric_kind(right_kind))
                    *out_kind = TRANSPILE_SEMANTIC_DATA_NUMERIC;
                else
                    *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
            }
            if (out_length)
            {
                if (left_length > right_length)
                    *out_length = left_length;
                else
                    *out_length = right_length;
            }
        }
        return (status);
    }
    pf_snprintf(message, sizeof(message),
        "arithmetic operator '%s' is not supported in %s",
        operator_text, expression_role);
    transpiler_semantics_emit_invalid_expression(context, message);
    return (FT_FAILURE);
}

static int transpiler_semantics_classify_condition_value(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length)
{
    t_transpiler_semantic_data_kind kind;
    size_t length;

    if (!role)
        return (FT_FAILURE);
    kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    length = 0;
    if (!value)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "condition %s is missing", role);
        return (transpiler_semantics_emit_invalid_condition(context, message));
    }
    if (value->kind == AST_NODE_IDENTIFIER)
    {
        if (transpiler_semantics_classify_condition_identifier(value, scope, context,
                role, &kind, &length) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (value->kind == AST_NODE_LITERAL)
    {
        kind = transpiler_semantics_classify_literal(value);
        if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
            length = transpiler_semantics_literal_alphanumeric_length(value);
        else
            length = 0;
    }
    else if (value->kind == AST_NODE_ARITHMETIC_EXPRESSION)
    {
        return (transpiler_semantics_classify_arithmetic_expression(value, scope,
            context, role, transpiler_semantics_classify_condition_value, out_kind,
            out_length));
    }
    else
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "condition %s must be an identifier, literal, or arithmetic expression",
            role);
        return (transpiler_semantics_emit_invalid_condition(context, message));
    }
    if (out_kind)
        *out_kind = kind;
    if (out_length)
        *out_length = length;
    return (FT_SUCCESS);
}

static int transpiler_semantics_classify_move_value(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length)
{
    t_transpiler_semantic_data_kind kind;
    size_t length;
    const char *move_role;

    kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    length = 0;
    move_role = (role && role[0] != '\0') ? role : "MOVE source";
    if (out_kind)
        *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    if (out_length)
        *out_length = 0;
    if (!value)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "%s is missing", move_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (value->kind == AST_NODE_IDENTIFIER)
    {
        if (transpiler_semantics_validate_identifier_use(scope, context, value, 0,
                &kind, &length, NULL) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (value->kind == AST_NODE_LITERAL)
    {
        kind = transpiler_semantics_classify_literal(value);
        if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
            length = transpiler_semantics_literal_alphanumeric_length(value);
        else
            length = 0;
    }
    else if (value->kind == AST_NODE_ARITHMETIC_EXPRESSION)
    {
        return (transpiler_semantics_classify_arithmetic_expression(value, scope,
            context, move_role, transpiler_semantics_classify_move_value, out_kind,
            out_length));
    }
    else
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "%s must be an identifier, literal, or arithmetic expression", move_role);
        transpiler_semantics_emit_invalid_expression(context, message);
        return (FT_FAILURE);
    }
    if (out_kind)
        *out_kind = kind;
    if (out_length)
        *out_length = length;
    return (FT_SUCCESS);
}

static int transpiler_semantics_validate_condition(const t_ast_node *condition,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    const t_ast_node *left;
    const t_ast_node *operator_node;
    const t_ast_node *right;
    t_transpiler_semantic_data_kind left_kind;
    t_transpiler_semantic_data_kind right_kind;
    size_t left_length;
    size_t right_length;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    if (!condition)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (ast_node_child_count(condition) < 3)
    {
        pf_snprintf(message, sizeof(message),
            "condition is missing operands or operator");
        return (transpiler_semantics_emit_invalid_condition(context, message));
    }
    left = ast_node_get_child(condition, 0);
    operator_node = ast_node_get_child(condition, 1);
    right = ast_node_get_child(condition, 2);
    left_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    right_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    left_length = 0;
    right_length = 0;
    status = FT_SUCCESS;
    if (transpiler_semantics_classify_condition_value(left, scope, context,
            "left operand", &left_kind, &left_length) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_semantics_classify_condition_value(right, scope, context,
            "right operand", &right_kind, &right_length) != FT_SUCCESS)
        status = FT_FAILURE;
    if (!operator_node || operator_node->kind != AST_NODE_COMPARISON_OPERATOR)
    {
        pf_snprintf(message, sizeof(message),
            "condition is missing a comparison operator");
        if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
            status = FT_FAILURE;
        else
            status = FT_FAILURE;
    }
    if (operator_node && operator_node->kind == AST_NODE_COMPARISON_OPERATOR)
    {
        t_lexer_token_kind op_kind;
        const char *left_name;
        const char *right_name;

        op_kind = operator_node->token.kind;
        if (left && left->token.lexeme)
            left_name = left->token.lexeme;
        else if (left && left->kind == AST_NODE_LITERAL && left->token.lexeme)
            left_name = left->token.lexeme;
        else
            left_name = "<left>";
        if (right && right->token.lexeme)
            right_name = right->token.lexeme;
        else if (right && right->kind == AST_NODE_LITERAL && right->token.lexeme)
            right_name = right->token.lexeme;
        else
            right_name = "<right>";
        if (op_kind == LEXER_TOKEN_EQUALS
            || op_kind == LEXER_TOKEN_NOT_EQUALS
            || op_kind == LEXER_TOKEN_LESS_THAN
            || op_kind == LEXER_TOKEN_LESS_OR_EQUAL
            || op_kind == LEXER_TOKEN_GREATER_THAN
            || op_kind == LEXER_TOKEN_GREATER_OR_EQUAL)
        {
            if (!transpiler_semantics_kinds_compatible(left_kind, right_kind))
            {
                pf_snprintf(message, sizeof(message),
                    "condition compares '%s' (%s) with '%s' (%s)",
                    left_name, transpiler_semantics_kind_to_string(left_kind),
                    right_name, transpiler_semantics_kind_to_string(right_kind));
                if (transpiler_semantics_emit_error(context,
                        TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message) != FT_SUCCESS)
                    status = FT_FAILURE;
                else
                    status = FT_FAILURE;
            }
            if (op_kind == LEXER_TOKEN_LESS_THAN
                || op_kind == LEXER_TOKEN_LESS_OR_EQUAL
                || op_kind == LEXER_TOKEN_GREATER_THAN
                || op_kind == LEXER_TOKEN_GREATER_OR_EQUAL)
            {
                const char *operator_text;

                operator_text = operator_node->token.lexeme ? operator_node->token.lexeme : "<operator>";
                if (left_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
                    && !transpiler_semantics_is_numeric_kind(left_kind))
                {
                    pf_snprintf(message, sizeof(message),
                        "condition operator '%s' requires numeric or floating operands but left operand '%s' is %s",
                        operator_text, left_name, transpiler_semantics_kind_to_string(left_kind));
                    if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
                        status = FT_FAILURE;
                    else
                        status = FT_FAILURE;
                }
                if (right_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
                    && !transpiler_semantics_is_numeric_kind(right_kind))
                {
                    pf_snprintf(message, sizeof(message),
                        "condition operator '%s' requires numeric or floating operands but right operand '%s' is %s",
                        operator_text, right_name, transpiler_semantics_kind_to_string(right_kind));
                    if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
                        status = FT_FAILURE;
                    else
                        status = FT_FAILURE;
                }
            }
            (void)left_length;
            (void)right_length;
        }
    }
    return (status);
}

static int transpiler_semantics_validate_assignment_like_statement(const t_ast_node *statement,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *statement_label, const char *role_prefix, int invalid_code)
{
    const t_ast_node *source;
    const t_ast_node *target;
    t_transpiler_semantic_data_kind target_kind;
    t_transpiler_semantic_data_kind source_kind;
    size_t target_length;
    size_t source_length;
    int target_is_read_only;
    int status;
    char source_role[64];
    char target_role[64];
    const char *label;

    if (!statement)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 2)
        return (FT_FAILURE);
    source = ast_node_get_child(statement, 0);
    target = ast_node_get_child(statement, 1);
    target_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    source_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    target_length = 0;
    source_length = 0;
    target_is_read_only = 0;
    status = FT_SUCCESS;
    if (role_prefix && role_prefix[0] != '\0')
    {
        pf_snprintf(source_role, sizeof(source_role), "%s source", role_prefix);
        pf_snprintf(target_role, sizeof(target_role), "%s target", role_prefix);
    }
    else
    {
        pf_snprintf(source_role, sizeof(source_role), "source");
        pf_snprintf(target_role, sizeof(target_role), "target");
    }
    label = (statement_label && statement_label[0] != '\0') ? statement_label : "assignment";
    if (!target || target->kind != AST_NODE_IDENTIFIER)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "%s statement is missing a valid target identifier", label);
        transpiler_semantics_emit_error(context, invalid_code, message);
        status = FT_FAILURE;
    }
    else if (transpiler_semantics_validate_identifier_use(scope, context, target, 1,
            &target_kind, &target_length, &target_is_read_only) != FT_SUCCESS)
        status = FT_FAILURE;
    if (source)
    {
        if (transpiler_semantics_classify_move_value(source, scope, context,
                source_role, &source_kind, &source_length) != FT_SUCCESS)
            status = FT_FAILURE;
    }
    if (target_is_read_only)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        const char *target_name;

        target_name = (target && target->token.lexeme) ? target->token.lexeme : "<target>";
        pf_snprintf(message, sizeof(message),
            "%s '%s' is read-only and cannot be modified", target_role, target_name);
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_IMMUTABLE_TARGET, message);
        status = FT_FAILURE;
    }
    if (!transpiler_semantics_kinds_compatible(target_kind, source_kind))
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        const char *target_name;
        const char *source_name;

        target_name = (target && target->token.lexeme) ? target->token.lexeme : "<target>";
        if (source && source->token.lexeme)
            source_name = source->token.lexeme;
        else if (source && source->kind == AST_NODE_LITERAL && source->token.lexeme)
            source_name = source->token.lexeme;
        else
            source_name = "<source>";
        pf_snprintf(message, sizeof(message),
            "%s '%s' (%s) is incompatible with %s '%s' (%s)",
            source_role, source_name, transpiler_semantics_kind_to_string(source_kind),
            target_role, target_name, transpiler_semantics_kind_to_string(target_kind));
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message);
        status = FT_FAILURE;
    }
    if (status == FT_SUCCESS
        && target_kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC
        && target_length > 0)
    {
        size_t required_length;

        required_length = 0;
        if (source_kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
            required_length = source_length;
        if (source && source->kind == AST_NODE_LITERAL)
        {
            size_t literal_length;

            literal_length = transpiler_semantics_literal_alphanumeric_length(source);
            if (literal_length > required_length)
                required_length = literal_length;
        }
        if (required_length > target_length)
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
            const char *target_name;
            const char *source_name;

            target_name = (target && target->token.lexeme) ? target->token.lexeme : "<target>";
            if (source && source->token.lexeme)
                source_name = source->token.lexeme;
            else
                source_name = "<source>";
            pf_snprintf(message, sizeof(message),
                "%s '%s' (%zu characters) truncates into %s '%s' (%zu characters)",
                source_role, source_name, required_length, target_role, target_name, target_length);
            transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION, message);
            status = FT_FAILURE;
        }
    }
    return (status);
}

static int transpiler_semantics_validate_move_statement(const t_ast_node *move_node,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    return (transpiler_semantics_validate_assignment_like_statement(move_node, scope, context,
        "MOVE", "MOVE", TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE));
}

static int transpiler_semantics_validate_assignment_statement(const t_ast_node *assignment_node,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    return (transpiler_semantics_validate_assignment_like_statement(assignment_node, scope, context,
        "'=' assignment", "assignment", TRANSPILE_ERROR_SEMANTIC_INVALID_ASSIGNMENT));
}

static int transpiler_semantics_validate_statement(const t_ast_node *statement,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    size_t index;
    int status;

    if (!statement)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (statement->kind == AST_NODE_ASSIGNMENT_STATEMENT)
        return (transpiler_semantics_validate_assignment_statement(statement, scope, context));
    if (statement->kind == AST_NODE_MOVE_STATEMENT)
        return (transpiler_semantics_validate_move_statement(statement, scope, context));
    if (statement->kind == AST_NODE_STATEMENT_SEQUENCE)
        return (transpiler_semantics_validate_statement_sequence(statement, scope, context));
    if (statement->kind == AST_NODE_IF_STATEMENT
        || statement->kind == AST_NODE_PERFORM_UNTIL_STATEMENT
        || statement->kind == AST_NODE_PERFORM_VARYING_STATEMENT)
    {
        status = FT_SUCCESS;
        index = 0;
        while (index < ast_node_child_count(statement))
        {
            const t_ast_node *child;

            child = ast_node_get_child(statement, index);
            if (child && child->kind == AST_NODE_CONDITION)
            {
                if (transpiler_semantics_validate_condition(child, scope, context) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            else if (child && child->kind == AST_NODE_STATEMENT_SEQUENCE)
            {
                if (transpiler_semantics_validate_statement_sequence(child, scope, context) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            index += 1;
        }
        return (status);
    }
    return (FT_SUCCESS);
}

static int transpiler_semantics_validate_statement_sequence(const t_ast_node *sequence,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    size_t index;
    int status;

    if (!sequence)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(sequence))
    {
        const t_ast_node *statement;

        statement = ast_node_get_child(sequence, index);
        if (statement)
        {
            if (transpiler_semantics_validate_statement(statement, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}

static int transpiler_semantics_validate_statements(const t_ast_node *program,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    size_t index;
    const t_ast_node *procedure_division;
    int status;

    if (!program)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    procedure_division = NULL;
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_PROCEDURE_DIVISION)
        {
            procedure_division = candidate;
            break ;
        }
        index += 1;
    }
    if (!procedure_division)
        return (FT_SUCCESS);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(procedure_division))
    {
        const t_ast_node *child;

        child = ast_node_get_child(procedure_division, index);
        if (child && child->kind == AST_NODE_STATEMENT_SEQUENCE)
        {
            if (transpiler_semantics_validate_statement_sequence(child, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}

int transpiler_semantics_analyze_program(t_transpiler_context *context, const t_ast_node *program)
{
    t_transpiler_semantic_scope scope;
    int status;

    if (!context)
        return (FT_FAILURE);
    transpiler_semantics_scope_init(&scope);
    status = FT_SUCCESS;
    if (transpiler_semantics_collect_scope(program, &scope, context) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_semantics_validate_statements(program, &scope, context) != FT_SUCCESS)
        status = FT_FAILURE;
    transpiler_semantics_scope_dispose(&scope);
    if (transpiler_context_has_errors(context))
        status = FT_FAILURE;
    return (status);
}
