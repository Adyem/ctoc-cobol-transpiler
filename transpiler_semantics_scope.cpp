#include <cstddef>

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

#include "transpiler_semantics_internal.hpp"

void transpiler_semantics_scope_init(t_transpiler_semantic_scope *scope)
{
    if (!scope)
        return ;
    scope->items = NULL;
    scope->item_count = 0;
    scope->item_capacity = 0;
    scope->expanded_copybooks = NULL;
    scope->expanded_copybook_count = 0;
    scope->expanded_copybook_capacity = 0;
}

void transpiler_semantics_scope_dispose(t_transpiler_semantic_scope *scope)
{
    if (!scope)
        return ;
    if (scope->items)
        cma_free(scope->items);
    scope->items = NULL;
    scope->item_count = 0;
    scope->item_capacity = 0;
    if (scope->expanded_copybooks)
        cma_free(scope->expanded_copybooks);
    scope->expanded_copybooks = NULL;
    scope->expanded_copybook_count = 0;
    scope->expanded_copybook_capacity = 0;
}

static const char *g_transpiler_semantics_flag_suffixes[] = {
    "FLAG",
    "SWITCH",
    "IND",
    "INDICATOR",
    "BOOL",
    "BOOLEAN",
    NULL
};

static void transpiler_semantics_normalize_identifier(const char *name,
    char *buffer, size_t buffer_size)
{
    size_t index;
    size_t write_index;
    int previous_was_separator;
    int has_significant;

    if (!buffer)
        return ;
    if (buffer_size == 0)
        return ;
    ft_bzero(buffer, buffer_size);
    if (!name)
        return ;
    index = 0;
    write_index = 0;
    previous_was_separator = 0;
    has_significant = 0;
    while (name[index] != '\0' && write_index + 1 < buffer_size)
    {
        char value;

        value = name[index];
        if ((value >= 'a' && value <= 'z')
            || (value >= 'A' && value <= 'Z'))
        {
            if (value >= 'a' && value <= 'z')
                value = static_cast<char>(value - ('a' - 'A'));
            buffer[write_index] = value;
            write_index += 1;
            previous_was_separator = 0;
            has_significant = 1;
        }
        else if (value >= '0' && value <= '9')
        {
            buffer[write_index] = value;
            write_index += 1;
            previous_was_separator = 0;
            has_significant = 1;
        }
        else if (value == '-' || value == '_' || value == ' ')
        {
            if (!previous_was_separator && has_significant)
            {
                buffer[write_index] = '_';
                write_index += 1;
                previous_was_separator = 1;
            }
        }
        index += 1;
    }
    if (write_index < buffer_size)
        buffer[write_index] = '\0';
}

static int transpiler_semantics_identifier_is_flag(const char *name)
{
    char normalized[(TRANSPILE_IDENTIFIER_MAX * 2)];
    size_t suffix_index;

    ft_bzero(normalized, sizeof(normalized));
    transpiler_semantics_normalize_identifier(name, normalized,
        sizeof(normalized));
    suffix_index = 0;
    while (g_transpiler_semantics_flag_suffixes[suffix_index])
    {
        const char *suffix;
        size_t normalized_length;
        size_t suffix_length;

        suffix = g_transpiler_semantics_flag_suffixes[suffix_index];
        normalized_length = ft_strlen(normalized);
        suffix_length = ft_strlen(suffix);
        if (suffix_length > 0 && suffix_length <= normalized_length)
        {
            size_t start_index;

            start_index = normalized_length - suffix_length;
            if (ft_strncmp(&normalized[start_index], suffix, suffix_length) == 0)
                return (1);
        }
        suffix_index += 1;
    }
    return (0);
}

static int transpiler_semantics_identifier_should_be_boolean(const char *name,
    t_transpiler_semantic_data_kind kind, size_t declared_length)
{
    size_t effective_length;

    if (!name)
        return (0);
    if (kind != TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
        return (0);
    effective_length = declared_length;
    if (effective_length == 0)
        effective_length = 1;
    if (effective_length > 1)
        return (0);
    return (transpiler_semantics_identifier_is_flag(name));
}

static int transpiler_semantics_scope_reserve(t_transpiler_semantic_scope *scope,
    size_t desired_capacity)
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
        new_items[index].declared_scale = scope->items[index].declared_scale;
        new_items[index].has_declared_scale = scope->items[index].has_declared_scale;
        new_items[index].is_read_only = scope->items[index].is_read_only;
        new_items[index].read_count = scope->items[index].read_count;
        new_items[index].write_count = scope->items[index].write_count;
        new_items[index].has_initial_value = scope->items[index].has_initial_value;
        index += 1;
    }
    if (scope->items)
        cma_free(scope->items);
    scope->items = new_items;
    scope->item_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_semantics_scope_copybooks_reserve(t_transpiler_semantic_scope *scope,
    size_t desired_capacity)
{
    char (*copybooks)[TRANSPILE_IDENTIFIER_MAX];

    if (!scope)
        return (FT_FAILURE);
    if (scope->expanded_copybook_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    copybooks = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(
            desired_capacity, sizeof(*copybooks)));
    if (!copybooks)
        return (FT_FAILURE);
    if (scope->expanded_copybooks)
    {
        ft_memcpy(copybooks, scope->expanded_copybooks,
            scope->expanded_copybook_count * sizeof(*copybooks));
        cma_free(scope->expanded_copybooks);
    }
    scope->expanded_copybooks = copybooks;
    scope->expanded_copybook_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_semantics_scope_has_copybook(const t_transpiler_semantic_scope *scope,
    const char *name)
{
    size_t index;

    if (!scope)
        return (0);
    if (!name)
        return (0);
    index = 0;
    while (index < scope->expanded_copybook_count)
    {
        if (ft_strncmp(scope->expanded_copybooks[index], name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int transpiler_semantics_scope_remember_copybook(t_transpiler_semantic_scope *scope,
    const char *name)
{
    if (!scope)
        return (FT_FAILURE);
    if (!name)
        return (FT_FAILURE);
    if (transpiler_semantics_scope_has_copybook(scope, name))
        return (FT_SUCCESS);
    if (scope->expanded_copybook_count >= scope->expanded_copybook_capacity)
    {
        if (transpiler_semantics_scope_copybooks_reserve(scope,
                scope->expanded_copybook_capacity == 0 ? 4 : scope->expanded_copybook_capacity * 2)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(scope->expanded_copybooks[scope->expanded_copybook_count], name,
        TRANSPILE_IDENTIFIER_MAX);
    scope->expanded_copybook_count += 1;
    return (FT_SUCCESS);
}

static int transpiler_semantics_parse_size_literal(const t_ast_node *literal, size_t *out_value)
{
    size_t index;
    size_t length;
    size_t value;
    const char *text;

    if (!literal || !out_value)
        return (FT_FAILURE);
    if (literal->kind != AST_NODE_LITERAL)
        return (FT_FAILURE);
    text = literal->token.lexeme;
    length = literal->token.length;
    if (!text)
        return (FT_FAILURE);
    if (length == 0)
        length = ft_strlen(text);
    if (length == 0)
        return (FT_FAILURE);
    value = 0;
    index = 0;
    while (index < length)
    {
        char digit;

        digit = text[index];
        if (!ft_isdigit(static_cast<unsigned char>(digit)))
            return (FT_FAILURE);
        if (value > (SIZE_MAX / 10))
            return (FT_FAILURE);
        value *= 10;
        if (static_cast<size_t>(digit - '0') > (SIZE_MAX - value))
            return (FT_FAILURE);
        value += static_cast<size_t>(digit - '0');
        index += 1;
    }
    *out_value = value;
    return (FT_SUCCESS);
}

static int transpiler_semantics_extract_occurs_metadata(const t_ast_node *occurs_node,
    t_transpiler_data_item_occurs *out_occurs)
{
    size_t index;
    const t_ast_node *lower_literal;
    const t_ast_node *upper_literal;
    const t_ast_node *identifier_node;

    if (!occurs_node || !out_occurs)
        return (FT_FAILURE);
    ft_bzero(out_occurs, sizeof(*out_occurs));
    lower_literal = NULL;
    upper_literal = NULL;
    identifier_node = NULL;
    index = 0;
    while (index < ast_node_child_count(occurs_node))
    {
        const t_ast_node *child;

        child = ast_node_get_child(occurs_node, index);
        if (!child)
            return (FT_FAILURE);
        if (child->kind == AST_NODE_LITERAL)
        {
            if (!lower_literal)
                lower_literal = child;
            else if (!upper_literal)
                upper_literal = child;
            else
                return (FT_FAILURE);
        }
        else if (child->kind == AST_NODE_IDENTIFIER)
        {
            if (identifier_node)
                return (FT_FAILURE);
            identifier_node = child;
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    if (!lower_literal)
        return (FT_FAILURE);
    if (transpiler_semantics_parse_size_literal(lower_literal,
            &out_occurs->minimum) != FT_SUCCESS)
        return (FT_FAILURE);
    if (upper_literal)
    {
        if (transpiler_semantics_parse_size_literal(upper_literal,
                &out_occurs->maximum) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
        out_occurs->maximum = out_occurs->minimum;
    if (out_occurs->maximum < out_occurs->minimum)
        return (FT_FAILURE);
    out_occurs->present = 1;
    out_occurs->has_depending_on = 0;
    ft_bzero(out_occurs->depending_on, sizeof(out_occurs->depending_on));
    if (identifier_node && identifier_node->token.lexeme)
    {
        out_occurs->has_depending_on = 1;
        ft_strlcpy(out_occurs->depending_on, identifier_node->token.lexeme,
            sizeof(out_occurs->depending_on));
    }
    return (FT_SUCCESS);
}

const t_transpiler_semantic_data_item *transpiler_semantics_scope_lookup(
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

static int transpiler_semantics_register_data_item(t_transpiler_semantic_scope *scope,
    t_transpiler_context *context, const char *name,
    t_transpiler_semantic_data_kind kind, size_t declared_length,
    size_t declared_scale, int has_declared_scale, int is_read_only,
    const t_transpiler_data_item_occurs *occurs)
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
        return (transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message));
    }
    if (transpiler_semantics_identifier_should_be_boolean(name, kind,
            declared_length))
        kind = TRANSPILE_SEMANTIC_DATA_BOOLEAN;
    existing = transpiler_semantics_scope_lookup(scope, name);
    if (existing)
    {
        pf_snprintf(message, sizeof(message),
            "data item '%s' declared multiple times in WORKING-STORAGE", name);
        return (transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM, message));
    }
    if (scope->item_count >= scope->item_capacity)
    {
        if (transpiler_semantics_scope_reserve(scope,
                scope->item_capacity == 0 ? 4 : scope->item_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &scope->items[scope->item_count];
    ft_strlcpy(item->name, name, TRANSPILE_IDENTIFIER_MAX);
    item->kind = kind;
    item->declared_length = declared_length;
    item->declared_scale = declared_scale;
    item->has_declared_scale = has_declared_scale;
    item->is_read_only = is_read_only;
    item->read_count = 0;
    item->write_count = 0;
    item->has_initial_value = 0;
    ft_bzero(&item->occurs, sizeof(item->occurs));
    if (occurs)
        item->occurs = *occurs;
    if (is_read_only)
        item->has_initial_value = 1;
    if (transpiler_context_register_data_item(context, name,
            transpiler_semantics_convert_kind(kind), declared_length, is_read_only,
            occurs) != FT_SUCCESS)
    {
        item->name[0] = '\0';
        item->kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
        item->declared_length = 0;
        item->is_read_only = 0;
        item->read_count = 0;
        item->write_count = 0;
        item->has_initial_value = 0;
        return (FT_FAILURE);
    }
    scope->item_count += 1;
    return (FT_SUCCESS);
}

static int transpiler_semantics_register_copybook(const t_ast_node *node,
    t_transpiler_semantic_scope *scope, t_transpiler_context *context);

static int transpiler_semantics_collect_data_items(const t_ast_node *section,
    t_transpiler_semantic_scope *scope, t_transpiler_context *context)
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
            const t_ast_node *value_node;
            const t_ast_node *occurs_node;
            size_t name_index;

            name_node = NULL;
            picture_node = NULL;
            level_node = NULL;
            value_node = NULL;
            name_index = 0;
            occurs_node = NULL;
            while (name_index < ast_node_child_count(child))
            {
                const t_ast_node *candidate;

                candidate = ast_node_get_child(child, name_index);
                if (candidate && candidate->kind == AST_NODE_IDENTIFIER)
                    name_node = candidate;
                else if (candidate && candidate->kind == AST_NODE_PICTURE_CLAUSE)
                    picture_node = candidate;
                else if (candidate && candidate->kind == AST_NODE_LITERAL)
                    level_node = candidate;
                else if (candidate && candidate->kind == AST_NODE_VALUE_CLAUSE)
                    value_node = candidate;
                else if (candidate && candidate->kind == AST_NODE_OCCURS_CLAUSE)
                    occurs_node = candidate;
                name_index += 1;
            }
            if (!name_node)
            {
                char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                pf_snprintf(message, sizeof(message),
                    "data item declaration is missing an identifier");
                transpiler_semantics_emit_error(context,
                    TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
                status = FT_FAILURE;
            }
            else
            {
                t_transpiler_semantic_data_kind kind;
                size_t declared_length;
                size_t declared_scale;
                int has_declared_scale;
                int is_read_only;
                t_transpiler_data_item_occurs occurs_metadata;
                const t_transpiler_data_item_occurs *occurs_pointer;

                kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
                declared_length = 0;
                declared_scale = 0;
                has_declared_scale = 0;
                is_read_only = 0;
                ft_bzero(&occurs_metadata, sizeof(occurs_metadata));
                occurs_pointer = NULL;
                if (picture_node)
                {
                    kind = transpiler_semantics_classify_picture(picture_node->token.lexeme);
                    if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
                        declared_length = transpiler_semantics_picture_alphanumeric_length(picture_node->token.lexeme);
                    else if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
                        declared_length = transpiler_semantics_picture_numeric_length(picture_node->token.lexeme);
                    else if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
                    {
                        declared_length = transpiler_semantics_picture_numeric_length(picture_node->token.lexeme);
                        declared_scale = transpiler_semantics_picture_decimal_scale(picture_node->token.lexeme);
                        has_declared_scale = 1;
                    }
                }
                if (level_node && level_node->token.lexeme)
                {
                    int level_value;

                    level_value = ft_atoi(level_node->token.lexeme);
                    if (level_value == 78)
                        is_read_only = 1;
                }
                if (occurs_node)
                {
                    if (transpiler_semantics_extract_occurs_metadata(occurs_node,
                            &occurs_metadata) != FT_SUCCESS)
                    {
                        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        pf_snprintf(message, sizeof(message),
                            "OCCURS clause on '%s' is invalid", name_node->token.lexeme);
                        transpiler_semantics_emit_error(context,
                            TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
                        status = FT_FAILURE;
                    }
                    else
                    {
                        occurs_pointer = &occurs_metadata;
                        if (occurs_metadata.has_depending_on)
                        {
                            const t_transpiler_semantic_data_item *controller;

                            controller = transpiler_semantics_scope_lookup(scope,
                                    occurs_metadata.depending_on);
                            if (!controller)
                            {
                                char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                                pf_snprintf(message, sizeof(message),
                                    "OCCURS DEPENDING ON references unknown data item '%s'",
                                    occurs_metadata.depending_on);
                                transpiler_semantics_emit_error(context,
                                    TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
                                status = FT_FAILURE;
                            }
                        }
                    }
                }
                if (transpiler_semantics_register_data_item(scope, context,
                        name_node->token.lexeme, kind, declared_length,
                        declared_scale, has_declared_scale, is_read_only,
                        occurs_pointer) != FT_SUCCESS)
                    status = FT_FAILURE;
                else if (value_node)
                {
                    const t_transpiler_semantic_data_item *registered;

                    registered = transpiler_semantics_scope_lookup(scope,
                            name_node->token.lexeme);
                    if (registered)
                    {
                        t_transpiler_semantic_data_item *mutable_item;

                        mutable_item = const_cast<t_transpiler_semantic_data_item *>(registered);
                        if (mutable_item)
                            mutable_item->has_initial_value = 1;
                    }
                }
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
    const char *copybook_name;
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
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        return (FT_FAILURE);
    }
    copybook = transpiler_context_find_copybook(context, name_node->token.lexeme);
    if (!copybook)
    {
        pf_snprintf(message, sizeof(message),
            "copybook '%s' is not registered in the current compilation context",
            name_node->token.lexeme);
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_UNKNOWN_COPYBOOK, message);
        return (FT_FAILURE);
    }
    copybook_name = copybook->name;
    if (transpiler_semantics_scope_has_copybook(scope, copybook_name))
    {
        pf_snprintf(message, sizeof(message),
            "copybook '%s' is included multiple times in this source file; duplicate COPY ignored",
            copybook_name);
        (void)transpiler_semantics_emit_warning_at(context, node,
            TRANSPILE_WARNING_SEMANTIC_DUPLICATE_COPYBOOK_INCLUDE, message,
            "Remove the redundant COPY directive to avoid repeated expansion.");
        return (FT_SUCCESS);
    }
    if (transpiler_semantics_scope_remember_copybook(scope, copybook_name) != FT_SUCCESS)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    index = 0;
    while (index < copybook->item_count)
    {
        const t_transpiler_copybook_item *item;
        t_transpiler_semantic_data_kind kind;

        item = &copybook->items[index];
        kind = transpiler_semantics_kind_from_context(item->kind);
        if (transpiler_semantics_register_data_item(scope, context, item->name,
                kind, item->declared_length, 0, 0, item->is_read_only, NULL) != FT_SUCCESS)
            status = FT_FAILURE;
        index += 1;
    }
    return (status);
}

int transpiler_semantics_collect_scope(const t_ast_node *program,
    t_transpiler_semantic_scope *scope, t_transpiler_context *context)
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

static int transpiler_semantics_file_declared(const t_transpiler_context *context,
    const char *name)
{
    const t_transpiler_file_declaration *files;
    size_t count;
    size_t index;

    files = transpiler_context_get_files(context, &count);
    index = 0;
    while (index < count)
    {
        if (ft_strncmp(files[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

int transpiler_semantics_collect_use_after_error(const t_ast_node *program,
    t_transpiler_context *context)
{
    const t_ast_node *procedure_division;
    size_t index;
    int status;

    if (!context)
        return (FT_FAILURE);
    if (!program)
        return (FT_SUCCESS);
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
        if (child && child->kind == AST_NODE_DECLARATIVES)
        {
            size_t section_index;

            section_index = 0;
            while (section_index < ast_node_child_count(child))
            {
                const t_ast_node *section;

                section = ast_node_get_child(child, section_index);
                if (section && section->kind == AST_NODE_DECLARATIVE_SECTION)
                {
                    const char *section_name;
                    size_t use_index;

                    section_name = section->token.lexeme;
                    if (!section_name)
                        section_name = "";
                    use_index = 0;
                    while (use_index < ast_node_child_count(section))
                    {
                        const t_ast_node *use_node;

                        use_node = ast_node_get_child(section, use_index);
                        if (use_node && use_node->kind == AST_NODE_USE_AFTER_ERROR_PROCEDURE)
                        {
                            size_t target_index;

                            target_index = 0;
                            while (target_index < ast_node_child_count(use_node))
                            {
                                const t_ast_node *target;

                                target = ast_node_get_child(use_node, target_index);
                                if (target && target->kind == AST_NODE_IDENTIFIER
                                    && target->token.lexeme)
                                {
                                    const char *file_name;

                                    file_name = target->token.lexeme;
                                    if (!transpiler_semantics_file_declared(context, file_name))
                                    {
                                        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                                        pf_snprintf(message, sizeof(message),
                                            "file '%s' referenced by USE AFTER ERROR PROCEDURE is not declared",
                                            file_name);
                                        transpiler_semantics_emit_error(context,
                                            TRANSPILE_ERROR_FILE_UNKNOWN, message);
                                        status = FT_FAILURE;
                                    }
                                    else if (transpiler_context_register_use_after_error_binding(context,
                                            section_name, file_name) != FT_SUCCESS)
                                    {
                                        status = FT_FAILURE;
                                    }
                                }
                                target_index += 1;
                            }
                        }
                        use_index += 1;
                    }
                }
                section_index += 1;
            }
        }
        index += 1;
    }
    return (status);
}

int transpiler_semantics_validate_identifier_use(const t_transpiler_semantic_scope *scope,
    t_transpiler_context *context, const t_ast_node *identifier, int is_target,
    t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
    size_t *out_scale, int *out_scale_known, int *out_is_read_only)
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
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        return (FT_FAILURE);
    }
    item = transpiler_semantics_scope_lookup(scope, identifier->token.lexeme);
    if (!item)
    {
        pf_snprintf(message, sizeof(message),
            "identifier '%s' referenced in MOVE is not declared in WORKING-STORAGE",
            identifier->token.lexeme);
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER, message);
        return (FT_FAILURE);
    }
    if (out_kind)
        *out_kind = item->kind;
    if (out_length)
        *out_length = item->declared_length;
    if (out_scale)
        *out_scale = item->declared_scale;
    if (out_scale_known)
        *out_scale_known = item->has_declared_scale;
    if (out_is_read_only)
        *out_is_read_only = item->is_read_only;
    if (item)
    {
        t_transpiler_semantic_data_item *mutable_item;

        mutable_item = const_cast<t_transpiler_semantic_data_item *>(item);
        if (mutable_item)
        {
            if (is_target)
                mutable_item->write_count += 1;
            else
                mutable_item->read_count += 1;
        }
    }
    return (FT_SUCCESS);
}

static int transpiler_semantics_emit_usage_warning(t_transpiler_context *context,
    int code, const char *name)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return (FT_FAILURE);
    if (!name || name[0] == '\0')
        name = "<data item>";
    if (code == TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM)
        pf_snprintf(message, sizeof(message),
            "data item '%s' is declared but never referenced", name);
    else if (code == TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM)
        pf_snprintf(message, sizeof(message),
            "data item '%s' is written but its value is never read", name);
    else if (code == TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE)
        pf_snprintf(message, sizeof(message),
            "data item '%s' is read before any assignments", name);
    else
        return (FT_FAILURE);
    if (transpiler_semantics_emit_warning(context, code, message) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_semantics_analyze_usage(const t_transpiler_semantic_scope *scope,
    t_transpiler_context *context)
{
    size_t index;
    int status;

    if (!context)
        return (FT_FAILURE);
    if (!scope)
        return (FT_SUCCESS);
    status = FT_SUCCESS;
    index = 0;
    while (index < scope->item_count)
    {
        const t_transpiler_semantic_data_item *item;

        item = &scope->items[index];
        if (item->name[0] != '\0')
        {
            if (item->read_count == 0 && item->write_count == 0)
            {
                if (transpiler_semantics_emit_usage_warning(context,
                        TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM,
                        item->name) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            else if (item->write_count > 0 && item->read_count == 0)
            {
                if (transpiler_semantics_emit_usage_warning(context,
                        TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM,
                        item->name) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            else if (item->read_count > 0 && item->write_count == 0
                && item->has_initial_value == 0)
            {
                if (transpiler_semantics_emit_usage_warning(context,
                        TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE,
                        item->name) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
        }
        index += 1;
    }
    return (status);
}
