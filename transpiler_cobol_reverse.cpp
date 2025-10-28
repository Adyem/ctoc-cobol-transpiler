#include <cstddef>

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "cblc_transpiler.hpp"

typedef struct s_cblc_builder
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cblc_builder;

typedef struct s_cobol_reverse_condition_entry
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    char parent[TRANSPILE_IDENTIFIER_MAX];
    const t_ast_node *value_literal;
}   t_cobol_reverse_condition_entry;

static t_cobol_reverse_condition_entry *g_cobol_reverse_conditions = NULL;
static size_t g_cobol_reverse_condition_count = 0;
static size_t g_cobol_reverse_condition_capacity = 0;

static const char *g_cobol_reverse_flag_suffixes[] = {
    "FLAG",
    "SWITCH",
    "IND",
    "INDICATOR",
    "BOOL",
    "BOOLEAN",
    NULL
};

static const char *g_cobol_reverse_character_array_markers[] = {
    "BUFFER",
    "RECORD",
    "FIELD",
    "AREA",
    "TABLE",
    "TEXT",
    "LINE",
    "NAME",
    "KEY",
    "VALUE",
    NULL
};

static int cobol_reverse_identifier_token_is_flag(const t_lexer_token *token);
static int cobol_reverse_identifier_token_matches_any_marker(const t_lexer_token *token,
    const char **markers);
static int cobol_reverse_append_identifier(t_cblc_builder *builder, const t_lexer_token *token);

static void cblc_builder_init(t_cblc_builder *builder)
{
    if (!builder)
        return ;
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static void cblc_builder_dispose(t_cblc_builder *builder)
{
    if (!builder)
        return ;
    if (builder->data)
        cma_free(builder->data);
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static int cblc_builder_reserve(t_cblc_builder *builder, size_t desired_capacity)
{
    char *new_data;

    if (!builder)
        return (FT_FAILURE);
    if (builder->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 64)
        desired_capacity = 64;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (builder->data && builder->length > 0)
        ft_memcpy(new_data, builder->data, builder->length);
    if (builder->data)
        cma_free(builder->data);
    builder->data = new_data;
    builder->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_builder_append_span(t_cblc_builder *builder, const char *text, size_t length)
{
    if (!builder)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (cblc_builder_reserve(builder, builder->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_memcpy(builder->data + builder->length, text, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_builder_append_string(t_cblc_builder *builder, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (cblc_builder_append_span(builder, text, ft_strlen(text)));
}

static int cblc_builder_append_char(t_cblc_builder *builder, char value)
{
    if (!builder)
        return (FT_FAILURE);
    if (cblc_builder_reserve(builder, builder->length + 2) != FT_SUCCESS)
        return (FT_FAILURE);
    builder->data[builder->length] = value;
    builder->length += 1;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_builder_append_unsigned(t_cblc_builder *builder, unsigned long long value)
{
    char digits[32];
    size_t index;
    size_t left;
    size_t right;

    if (!builder)
        return (FT_FAILURE);
    index = 0;
    while (value > 0 && index < sizeof(digits))
    {
        digits[index] = static_cast<char>('0' + (value % 10));
        value /= 10;
        index += 1;
    }
    if (index == 0)
    {
        digits[index] = '0';
        index += 1;
    }
    left = 0;
    if (index > 0)
    {
        right = index - 1;
        while (left < right)
        {
            char temp;

            temp = digits[left];
            digits[left] = digits[right];
            digits[right] = temp;
            left += 1;
            right -= 1;
        }
    }
    return (cblc_builder_append_span(builder, digits, index));
}

static int cblc_builder_append_newline(t_cblc_builder *builder)
{
    return (cblc_builder_append_char(builder, '\n'));
}

static int cblc_builder_append_indentation(t_cblc_builder *builder, size_t indentation)
{
    size_t index;

    if (!builder)
        return (FT_FAILURE);
    index = 0;
    while (index < indentation)
    {
        if (cblc_builder_append_string(builder, "    ") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_builder_append_paragraph_separator(t_cblc_builder *builder)
{
    size_t length;

    if (!builder)
        return (FT_FAILURE);
    length = builder->length;
    if (length == 0)
        return (FT_SUCCESS);
    if (builder->data[length - 1] != '\n')
    {
        if (cblc_builder_append_newline(builder) != FT_SUCCESS)
            return (FT_FAILURE);
        length = builder->length;
    }
    if (length < 2)
        return (FT_SUCCESS);
    if (builder->data[length - 2] == '\n')
        return (FT_SUCCESS);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_token_to_identifier(const t_lexer_token *token,
    char *buffer, size_t buffer_size)
{
    t_cblc_builder builder;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (!token)
        return (FT_FAILURE);
    cblc_builder_init(&builder);
    if (cobol_reverse_append_identifier(&builder, token) != FT_SUCCESS)
    {
        cblc_builder_dispose(&builder);
        return (FT_FAILURE);
    }
    if (!builder.data)
    {
        cblc_builder_dispose(&builder);
        return (FT_FAILURE);
    }
    ft_strlcpy(buffer, builder.data, buffer_size);
    cblc_builder_dispose(&builder);
    return (FT_SUCCESS);
}

static void cobol_reverse_condition_table_reset(void)
{
    if (g_cobol_reverse_conditions)
        cma_free(g_cobol_reverse_conditions);
    g_cobol_reverse_conditions = NULL;
    g_cobol_reverse_condition_count = 0;
    g_cobol_reverse_condition_capacity = 0;
}

static int cobol_reverse_condition_table_reserve(size_t desired_capacity)
{
    t_cobol_reverse_condition_entry *entries;

    if (g_cobol_reverse_condition_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    entries = static_cast<t_cobol_reverse_condition_entry *>(cma_calloc(desired_capacity,
            sizeof(t_cobol_reverse_condition_entry)));
    if (!entries)
        return (FT_FAILURE);
    if (g_cobol_reverse_conditions && g_cobol_reverse_condition_count > 0)
        ft_memcpy(entries, g_cobol_reverse_conditions,
            g_cobol_reverse_condition_count * sizeof(t_cobol_reverse_condition_entry));
    if (g_cobol_reverse_conditions)
        cma_free(g_cobol_reverse_conditions);
    g_cobol_reverse_conditions = entries;
    g_cobol_reverse_condition_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cobol_reverse_condition_table_add(const t_lexer_token *parent_token,
    const t_lexer_token *name_token, const t_ast_node *value_literal)
{
    t_cobol_reverse_condition_entry *entry;

    if (!parent_token || !name_token || !value_literal)
        return (FT_FAILURE);
    if (g_cobol_reverse_condition_count >= g_cobol_reverse_condition_capacity)
    {
        size_t desired_capacity;

        desired_capacity = g_cobol_reverse_condition_capacity;
        if (desired_capacity == 0)
            desired_capacity = 4;
        else
            desired_capacity *= 2;
        if (cobol_reverse_condition_table_reserve(desired_capacity) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    entry = &g_cobol_reverse_conditions[g_cobol_reverse_condition_count];
    if (cobol_reverse_token_to_identifier(parent_token, entry->parent,
            sizeof(entry->parent)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_token_to_identifier(name_token, entry->name,
            sizeof(entry->name)) != FT_SUCCESS)
        return (FT_FAILURE);
    entry->value_literal = value_literal;
    g_cobol_reverse_condition_count += 1;
    return (FT_SUCCESS);
}

static const t_cobol_reverse_condition_entry *cobol_reverse_condition_table_find(
    const t_lexer_token *token)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!token)
        return (NULL);
    if (cobol_reverse_token_to_identifier(token, identifier,
            sizeof(identifier)) != FT_SUCCESS)
        return (NULL);
    index = 0;
    while (index < g_cobol_reverse_condition_count)
    {
        if (ft_strncmp(g_cobol_reverse_conditions[index].name, identifier,
                TRANSPILE_IDENTIFIER_MAX) == 0)
            return (&g_cobol_reverse_conditions[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_builder_ensure_trailing_newline(t_cblc_builder *builder)
{
    if (!builder)
        return (FT_FAILURE);
    if (builder->length == 0)
        return (FT_SUCCESS);
    if (builder->data[builder->length - 1] == '\n')
        return (FT_SUCCESS);
    return (cblc_builder_append_newline(builder));
}

static int cobol_reverse_emit_comment_line(t_cblc_builder *builder, const t_transpiler_comment *comment,
    size_t indentation)
{
    size_t offset;
    size_t trimmed_length;

    if (!builder || !comment)
        return (FT_FAILURE);
    if (!comment->text || comment->length == 0)
        return (FT_SUCCESS);
    offset = 0;
    while (offset < comment->length)
    {
        char value;

        value = comment->text[offset];
        if (value != ' ' && value != '\t')
            break ;
        offset += 1;
    }
    if (offset + 1 < comment->length && comment->text[offset] == '*' && comment->text[offset + 1] == '>')
    {
        offset += 2;
        while (offset < comment->length)
        {
            char value;

            value = comment->text[offset];
            if (value != ' ' && value != '\t')
                break ;
            offset += 1;
        }
    }
    trimmed_length = comment->length - offset;
    while (trimmed_length > 0)
    {
        char value;

        value = comment->text[offset + trimmed_length - 1];
        if (value != ' ' && value != '\t')
            break ;
        trimmed_length -= 1;
    }
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "/*") != FT_SUCCESS)
        return (FT_FAILURE);
    if (trimmed_length > 0)
    {
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_span(builder, comment->text + offset, trimmed_length) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " */") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        if (cblc_builder_append_string(builder, " */") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_comments_before_line(t_transpiler_context *context, t_cblc_builder *builder,
    size_t line, size_t indentation)
{
    if (!context || !builder)
        return (FT_SUCCESS);
    if (context->comment_emit_index >= context->comment_count)
        return (FT_SUCCESS);
    while (context->comment_emit_index < context->comment_count)
    {
        const t_transpiler_comment *comment;

        comment = &context->comments[context->comment_emit_index];
        if (line > 0 && comment->line >= line)
            break ;
        if (cobol_reverse_emit_comment_line(builder, comment, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        context->comment_emit_index += 1;
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_comments_on_line(t_transpiler_context *context, t_cblc_builder *builder,
    size_t line, size_t indentation)
{
    if (!context || !builder)
        return (FT_SUCCESS);
    if (line == 0)
        return (FT_SUCCESS);
    while (context->comment_emit_index < context->comment_count)
    {
        const t_transpiler_comment *comment;

        comment = &context->comments[context->comment_emit_index];
        if (comment->line > line)
            break ;
        if (comment->line < line)
        {
            if (cobol_reverse_emit_comment_line(builder, comment, indentation) != FT_SUCCESS)
                return (FT_FAILURE);
            context->comment_emit_index += 1;
            continue ;
        }
        if (cobol_reverse_emit_comment_line(builder, comment, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        context->comment_emit_index += 1;
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_remaining_comments(t_transpiler_context *context, t_cblc_builder *builder)
{
    if (!context || !builder)
        return (FT_SUCCESS);
    while (context->comment_emit_index < context->comment_count)
    {
        const t_transpiler_comment *comment;

        comment = &context->comments[context->comment_emit_index];
        if (cobol_reverse_emit_comment_line(builder, comment, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        context->comment_emit_index += 1;
    }
    return (FT_SUCCESS);
}

static size_t cobol_reverse_node_line(const t_ast_node *node)
{
    size_t index;
    size_t line;

    if (!node)
        return (0);
    if (node->token.line > 0)
        return (node->token.line);
    index = 0;
    while (index < ast_node_child_count(node))
    {
        const t_ast_node *child;

        child = ast_node_get_child(node, index);
        line = cobol_reverse_node_line(child);
        if (line > 0)
            return (line);
        index += 1;
    }
    return (0);
}

static int cobol_reverse_emit_error(t_transpiler_context *context, const char *message)
{
    if (!context || !message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, FT_FAILURE, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int cobol_reverse_token_equals(const t_lexer_token *token, const char *text)
{
    size_t index;
    size_t length;
    size_t expected_length;

    if (!token)
        return (0);
    if (!text)
        return (0);
    length = token->length;
    expected_length = ft_strlen(text);
    if (length != expected_length)
        return (0);
    index = 0;
    while (index < length)
    {
        char left;
        char right;

        left = token->lexeme[index];
        right = text[index];
        if (left >= 'a' && left <= 'z')
            left = static_cast<char>(left - ('a' - 'A'));
        if (right >= 'a' && right <= 'z')
            right = static_cast<char>(right - ('a' - 'A'));
        if (left != right)
            return (0);
        index += 1;
    }
    return (1);
}

static int cobol_reverse_append_identifier(t_cblc_builder *builder, const t_lexer_token *token)
{
    char *normalized;
    size_t index;
    size_t write_index;
    int previous_was_underscore;
    int has_significant;

    if (!builder)
        return (FT_FAILURE);
    if (!token)
        return (FT_FAILURE);
    if (!token->lexeme)
        return (FT_FAILURE);
    normalized = static_cast<char *>(cma_calloc((token->length * 2) + 4, sizeof(char)));
    if (!normalized)
        return (FT_FAILURE);
    index = 0;
    write_index = 0;
    previous_was_underscore = 0;
    has_significant = 0;
    if (token->length > 0)
    {
        char leading;

        leading = token->lexeme[0];
        if (ft_isdigit(static_cast<unsigned char>(leading)))
        {
            normalized[write_index] = '_';
            write_index += 1;
        }
    }
    while (index < token->length)
    {
        char value;

        value = token->lexeme[index];
        if (ft_isalpha(static_cast<unsigned char>(value)))
        {
            if (ft_islower(static_cast<unsigned char>(value)))
                value = static_cast<char>(value - ('a' - 'A'));
            normalized[write_index] = value;
            write_index += 1;
            previous_was_underscore = 0;
            has_significant = 1;
        }
        else if (ft_isdigit(static_cast<unsigned char>(value)))
        {
            normalized[write_index] = value;
            write_index += 1;
            previous_was_underscore = 0;
            has_significant = 1;
        }
        else if (value == '-' || value == '_' || value == ' ')
        {
            if (!previous_was_underscore && has_significant)
            {
                normalized[write_index] = '_';
                write_index += 1;
                previous_was_underscore = 1;
            }
        }
        index += 1;
    }
    if (!has_significant)
    {
        normalized[write_index] = '_';
        write_index += 1;
    }
    if (write_index > 0 && previous_was_underscore)
        write_index -= 1;
    if (write_index == 0)
    {
        normalized[0] = '_';
        write_index = 1;
    }
    if (cblc_builder_append_span(builder, normalized, write_index) != FT_SUCCESS)
    {
        cma_free(normalized);
        return (FT_FAILURE);
    }
    cma_free(normalized);
    return (FT_SUCCESS);
}

static const char *cobol_reverse_boolean_from_string_literal(const t_lexer_token *token)
{
    char value;

    if (!token)
        return (NULL);
    if (token->kind != LEXER_TOKEN_STRING_LITERAL)
        return (NULL);
    if (token->length != 3)
        return (NULL);
    if (!token->lexeme)
        return (NULL);
    if (token->lexeme[0] != '\'')
        return (NULL);
    if (token->lexeme[2] != '\'')
        return (NULL);
    value = token->lexeme[1];
    if (value == 'Y' || value == 'y')
        return ("true");
    if (value == 'N' || value == 'n')
        return ("false");
    return (NULL);
}

static int cobol_reverse_append_string_literal(t_cblc_builder *builder,
    const t_lexer_token *token, int allow_boolean_coercion)
{
    size_t index;
    const char *boolean_text;

    if (!builder)
        return (FT_FAILURE);
    if (!token)
        return (FT_FAILURE);
    if (allow_boolean_coercion)
    {
        boolean_text = cobol_reverse_boolean_from_string_literal(token);
        if (boolean_text)
            return (cblc_builder_append_string(builder, boolean_text));
    }
    if (token->length < 2)
        return (FT_FAILURE);
    if (cblc_builder_append_char(builder, '"') != FT_SUCCESS)
        return (FT_FAILURE);
    index = 1;
    while (index + 1 < token->length)
    {
        char value;

        value = token->lexeme[index];
        if (value == '\r' || value == '\n')
            return (FT_FAILURE);
        if (value == '"')
        {
            if (cblc_builder_append_char(builder, '\\') != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (value == '\\')
        {
            if (cblc_builder_append_char(builder, '\\') != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_char(builder, value) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    if (cblc_builder_append_char(builder, '"') != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_append_numeric_literal(t_cblc_builder *builder, const t_lexer_token *token)
{
    size_t index;
    size_t start;
    int has_sign;

    if (!builder)
        return (FT_FAILURE);
    if (!token)
        return (FT_FAILURE);
    if (!token->lexeme)
        return (FT_FAILURE);
    start = 0;
    has_sign = 0;
    if (token->length > 0 && (token->lexeme[0] == '+' || token->lexeme[0] == '-'))
    {
        has_sign = 1;
        start = 1;
    }
    index = start;
    while (index + 1 < token->length && token->lexeme[index] == '0')
        index += 1;
    if (has_sign && index >= token->length)
    {
        if (cblc_builder_append_char(builder, token->lexeme[0]) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_char(builder, '0') != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (has_sign)
    {
        if (cblc_builder_append_char(builder, token->lexeme[0]) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (index >= token->length)
        return (cblc_builder_append_char(builder, '0'));
    return (cblc_builder_append_span(builder, token->lexeme + index, token->length - index));
}

static int cobol_reverse_append_value(t_cblc_builder *builder, const t_ast_node *node)
{
    if (!builder)
        return (FT_FAILURE);
    if (!node)
        return (FT_FAILURE);
    if (node->kind == AST_NODE_IDENTIFIER)
        return (cobol_reverse_append_identifier(builder, &node->token));
    if (node->kind == AST_NODE_LITERAL)
    {
        if (node->token.kind == LEXER_TOKEN_STRING_LITERAL)
            return (cobol_reverse_append_string_literal(builder, &node->token, 1));
        if (node->token.kind == LEXER_TOKEN_NUMERIC_LITERAL)
            return (cobol_reverse_append_numeric_literal(builder, &node->token));
        if (node->token.kind == LEXER_TOKEN_KEYWORD_TRUE)
            return (cblc_builder_append_string(builder, "true"));
        if (node->token.kind == LEXER_TOKEN_KEYWORD_FALSE)
            return (cblc_builder_append_string(builder, "false"));
    }
    return (FT_FAILURE);
}

static int cobol_reverse_append_condition_literal(t_cblc_builder *builder,
    const t_ast_node *literal)
{
    const t_lexer_token *token;

    if (!literal)
        return (FT_FAILURE);
    if (literal->kind == AST_NODE_LITERAL
        && literal->token.kind == LEXER_TOKEN_STRING_LITERAL)
    {
        token = &literal->token;
        if (token->length == 3 && token->lexeme
            && token->lexeme[0] == '\''
            && token->lexeme[2] == '\'')
        {
            if (cblc_builder_append_char(builder, '\'') != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_char(builder, token->lexeme[1]) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_char(builder, '\'') != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        return (cobol_reverse_append_string_literal(builder, token, 0));
    }
    return (cobol_reverse_append_value(builder, literal));
}

typedef enum e_cobol_reverse_picture_kind
{
    COBOL_REVERSE_PICTURE_UNKNOWN = 0,
    COBOL_REVERSE_PICTURE_ALPHANUMERIC,
    COBOL_REVERSE_PICTURE_NUMERIC
}   t_cobol_reverse_picture_kind;

typedef enum e_cobol_reverse_usage_kind
{
    COBOL_REVERSE_USAGE_UNKNOWN = 0,
    COBOL_REVERSE_USAGE_DISPLAY,
    COBOL_REVERSE_USAGE_COMP,
    COBOL_REVERSE_USAGE_COMP_1,
    COBOL_REVERSE_USAGE_COMP_2,
    COBOL_REVERSE_USAGE_COMP_3,
    COBOL_REVERSE_USAGE_COMP_5
}   t_cobol_reverse_usage_kind;

typedef struct s_cobol_reverse_picture
{
    t_cobol_reverse_picture_kind kind;
    size_t length;
    size_t fractional_length;
    int is_signed;
    t_cobol_reverse_usage_kind usage;
}   t_cobol_reverse_picture;

typedef struct s_cobol_reverse_data_item_info
{
    const t_ast_node *level_node;
    const t_ast_node *name_node;
    const t_ast_node *picture_node;
    const t_ast_node *occurs_node;
    const t_ast_node *value_node;
}   t_cobol_reverse_data_item_info;

typedef struct s_cobol_reverse_scalar_metadata
{
    const char *type_text;
    int is_array;
    size_t array_length;
    int is_boolean_type;
}   t_cobol_reverse_scalar_metadata;

typedef struct s_cobol_reverse_occurs_metadata
{
    int present;
    size_t minimum;
    size_t maximum;
    int has_depending_on;
    const t_ast_node *controller_node;
}   t_cobol_reverse_occurs_metadata;

static void cobol_reverse_skip_spaces(const char *text, size_t length, size_t *index)
{
    if (!text)
        return ;
    if (!index)
        return ;
    if (*index >= length)
        return ;
    while (*index < length
        && ft_isspace(static_cast<unsigned char>(text[*index])))
        *index += 1;
}

static int cobol_reverse_parse_repeat(const char *text, size_t length, size_t *index, size_t *out_repeat)
{
    unsigned long long value;

    if (!text || !index || !out_repeat)
        return (FT_FAILURE);
    if (*index >= length)
        return (FT_FAILURE);
    cobol_reverse_skip_spaces(text, length, index);
    if (*index >= length)
        return (FT_FAILURE);
    if (!ft_isdigit(static_cast<unsigned char>(text[*index])))
        return (FT_FAILURE);
    value = 0;
    while (*index < length && ft_isdigit(static_cast<unsigned char>(text[*index])))
    {
        value = (value * 10) + static_cast<unsigned long long>(text[*index] - '0');
        if (value > SIZE_MAX)
            return (FT_FAILURE);
        *index += 1;
    }
    cobol_reverse_skip_spaces(text, length, index);
    if (*index >= length)
        return (FT_FAILURE);
    if (text[*index] != ')')
        return (FT_FAILURE);
    *index += 1;
    if (value == 0)
        return (FT_FAILURE);
    *out_repeat = static_cast<size_t>(value);
    return (FT_SUCCESS);
}

static int cobol_reverse_parse_usage_keyword(const char *text,
    t_cobol_reverse_picture *out_picture)
{
    if (!text || !out_picture)
        return (FT_FAILURE);
    if (ft_strcmp(text, "USAGE") == 0)
        return (FT_SUCCESS);
    if (ft_strcmp(text, "DISPLAY") == 0)
    {
        out_picture->usage = COBOL_REVERSE_USAGE_DISPLAY;
        return (FT_SUCCESS);
    }
    if (ft_strcmp(text, "COMP-1") == 0
        || ft_strcmp(text, "COMPUTATIONAL-1") == 0)
    {
        out_picture->usage = COBOL_REVERSE_USAGE_COMP_1;
        return (FT_SUCCESS);
    }
    if (ft_strcmp(text, "COMP-2") == 0
        || ft_strcmp(text, "COMPUTATIONAL-2") == 0)
    {
        out_picture->usage = COBOL_REVERSE_USAGE_COMP_2;
        return (FT_SUCCESS);
    }
    if (ft_strcmp(text, "COMP-3") == 0
        || ft_strcmp(text, "COMPUTATIONAL-3") == 0)
    {
        out_picture->usage = COBOL_REVERSE_USAGE_COMP_3;
        return (FT_SUCCESS);
    }
    if (ft_strcmp(text, "COMP-5") == 0
        || ft_strcmp(text, "COMPUTATIONAL-5") == 0
        || ft_strcmp(text, "BINARY") == 0)
    {
        out_picture->usage = COBOL_REVERSE_USAGE_COMP_5;
        return (FT_SUCCESS);
    }
    if (ft_strcmp(text, "COMP") == 0
        || ft_strcmp(text, "COMPUTATIONAL") == 0)
    {
        out_picture->usage = COBOL_REVERSE_USAGE_COMP;
        return (FT_SUCCESS);
    }
    if (ft_strcmp(text, "SIGN") == 0)
        return (FT_SUCCESS);
    if (ft_strcmp(text, "IS") == 0)
        return (FT_SUCCESS);
    if (ft_strcmp(text, "LEADING") == 0)
        return (FT_SUCCESS);
    if (ft_strcmp(text, "TRAILING") == 0)
        return (FT_SUCCESS);
    if (ft_strcmp(text, "SEPARATE") == 0)
        return (FT_SUCCESS);
    return (FT_FAILURE);
}

static int cobol_reverse_parse_picture(const t_ast_node *picture, t_cobol_reverse_picture *out_picture)
{
    const char *text;
    size_t length;
    size_t index;
    char normalized;
    char last_symbol;
    int last_symbol_fractional;
    int decimal_section;

    if (!picture || !out_picture)
        return (FT_FAILURE);
    ft_bzero(out_picture, sizeof(*out_picture));
    out_picture->kind = COBOL_REVERSE_PICTURE_UNKNOWN;
    out_picture->length = 0;
    out_picture->fractional_length = 0;
    out_picture->is_signed = 0;
    out_picture->usage = COBOL_REVERSE_USAGE_UNKNOWN;
    text = picture->token.lexeme;
    length = picture->token.length;
    if (!text)
        return (FT_FAILURE);
    index = 0;
    cobol_reverse_skip_spaces(text, length, &index);
    if (index < length && (text[index] == 's' || text[index] == 'S'))
    {
        out_picture->is_signed = 1;
        index += 1;
    }
    cobol_reverse_skip_spaces(text, length, &index);
    last_symbol = 0;
    last_symbol_fractional = 0;
    decimal_section = 0;
    while (index < length)
    {
        char value;

        value = text[index];
        if (ft_isspace(static_cast<unsigned char>(value)))
        {
            index += 1;
            continue ;
        }
        if (value == '(')
        {
            size_t repeat;

            if (last_symbol == 0)
                return (FT_FAILURE);
            index += 1;
            if (cobol_reverse_parse_repeat(text, length, &index, &repeat) != FT_SUCCESS)
                return (FT_FAILURE);
            if (repeat == 0)
                return (FT_FAILURE);
            repeat -= 1;
            if (last_symbol_fractional)
            {
                if (out_picture->fractional_length > SIZE_MAX - repeat)
                    return (FT_FAILURE);
                out_picture->fractional_length += repeat;
            }
            else if (last_symbol == 'X')
            {
                if (out_picture->length > SIZE_MAX - repeat)
                    return (FT_FAILURE);
                out_picture->length += repeat;
            }
            else
            {
                if (out_picture->length > SIZE_MAX - repeat)
                    return (FT_FAILURE);
                out_picture->length += repeat;
            }
            continue ;
        }
        normalized = value;
        if (normalized >= 'a' && normalized <= 'z')
            normalized = static_cast<char>(normalized - ('a' - 'A'));
        if (normalized == 'V' || normalized == '.')
        {
            if (out_picture->kind == COBOL_REVERSE_PICTURE_UNKNOWN)
                out_picture->kind = COBOL_REVERSE_PICTURE_NUMERIC;
            else if (out_picture->kind != COBOL_REVERSE_PICTURE_NUMERIC)
                return (FT_FAILURE);
            decimal_section = 1;
            last_symbol = 'V';
            last_symbol_fractional = 0;
            index += 1;
            continue ;
        }
        if (normalized == 'X' || normalized == 'A')
        {
            if (out_picture->kind == COBOL_REVERSE_PICTURE_UNKNOWN)
                out_picture->kind = COBOL_REVERSE_PICTURE_ALPHANUMERIC;
            else if (out_picture->kind != COBOL_REVERSE_PICTURE_ALPHANUMERIC)
                return (FT_FAILURE);
            if (out_picture->length == SIZE_MAX)
                return (FT_FAILURE);
            out_picture->length += 1;
            last_symbol = 'X';
            last_symbol_fractional = 0;
            index += 1;
            continue ;
        }
        if (normalized == '9')
        {
            if (out_picture->kind == COBOL_REVERSE_PICTURE_UNKNOWN)
                out_picture->kind = COBOL_REVERSE_PICTURE_NUMERIC;
            else if (out_picture->kind != COBOL_REVERSE_PICTURE_NUMERIC)
                return (FT_FAILURE);
            if (decimal_section)
            {
                if (out_picture->fractional_length == SIZE_MAX)
                    return (FT_FAILURE);
                out_picture->fractional_length += 1;
                last_symbol_fractional = 1;
            }
            else
            {
                if (out_picture->length == SIZE_MAX)
                    return (FT_FAILURE);
                out_picture->length += 1;
                last_symbol_fractional = 0;
            }
            last_symbol = '9';
            index += 1;
            continue ;
        }
        if ((normalized >= 'A' && normalized <= 'Z')
            || (normalized == '-' && last_symbol != 0))
        {
            size_t word_start;
            size_t word_end;
            size_t word_length;
            char *keyword;
            size_t keyword_index;

            word_start = index;
            word_end = index;
            while (word_end < length)
            {
                char symbol;

                symbol = text[word_end];
                if ((symbol >= 'A' && symbol <= 'Z')
                    || (symbol >= 'a' && symbol <= 'z')
                    || (symbol >= '0' && symbol <= '9')
                    || symbol == '-')
                {
                    word_end += 1;
                    continue ;
                }
                break ;
            }
            if (word_end <= word_start)
                return (FT_FAILURE);
            word_length = word_end - word_start;
            keyword = static_cast<char *>(cma_calloc(word_length + 1, sizeof(char)));
            if (!keyword)
                return (FT_FAILURE);
            keyword_index = 0;
            while (keyword_index < word_length)
            {
                char symbol;

                symbol = text[word_start + keyword_index];
                if (symbol >= 'a' && symbol <= 'z')
                    symbol = static_cast<char>(symbol - ('a' - 'A'));
                keyword[keyword_index] = symbol;
                keyword_index += 1;
            }
            keyword[word_length] = '\0';
            if (cobol_reverse_parse_usage_keyword(keyword, out_picture) != FT_SUCCESS)
            {
                cma_free(keyword);
                return (FT_FAILURE);
            }
            cma_free(keyword);
            index = word_end;
            last_symbol = 0;
            last_symbol_fractional = 0;
            continue ;
        }
        return (FT_FAILURE);
    }
    if (out_picture->kind == COBOL_REVERSE_PICTURE_UNKNOWN)
        return (FT_FAILURE);
    if (out_picture->kind == COBOL_REVERSE_PICTURE_ALPHANUMERIC
        && out_picture->length == 0)
        out_picture->length = 1;
    if (out_picture->kind == COBOL_REVERSE_PICTURE_NUMERIC
        && out_picture->length == 0 && out_picture->fractional_length == 0)
        return (FT_FAILURE);
    if (out_picture->usage == COBOL_REVERSE_USAGE_UNKNOWN)
        out_picture->usage = COBOL_REVERSE_USAGE_DISPLAY;
    return (FT_SUCCESS);
}

static int cobol_reverse_collect_data_item_info(const t_ast_node *item,
    t_cobol_reverse_data_item_info *out_info)
{
    size_t index;

    if (!item || !out_info)
        return (FT_FAILURE);
    ft_bzero(out_info, sizeof(*out_info));
    index = 0;
    while (index < ast_node_child_count(item))
    {
        const t_ast_node *child;

        child = ast_node_get_child(item, index);
        if (child)
        {
            if (child->kind == AST_NODE_LITERAL && !out_info->level_node)
                out_info->level_node = child;
            else if (child->kind == AST_NODE_IDENTIFIER && !out_info->name_node)
                out_info->name_node = child;
            else if (child->kind == AST_NODE_PICTURE_CLAUSE && !out_info->picture_node)
                out_info->picture_node = child;
            else if (child->kind == AST_NODE_OCCURS_CLAUSE && !out_info->occurs_node)
                out_info->occurs_node = child;
            else if (child->kind == AST_NODE_VALUE_CLAUSE && !out_info->value_node)
                out_info->value_node = child;
        }
        index += 1;
    }
    if (!out_info->level_node || !out_info->name_node)
        return (FT_FAILURE);
    if (!out_info->level_node->token.lexeme || !out_info->name_node->token.lexeme)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_get_data_item_level(const t_cobol_reverse_data_item_info *info,
    long *out_level)
{
    long level;

    if (!info || !out_level)
        return (FT_FAILURE);
    if (!info->level_node || !info->level_node->token.lexeme)
        return (FT_FAILURE);
    level = ft_atol(info->level_node->token.lexeme);
    *out_level = level;
    return (FT_SUCCESS);
}

static int cobol_reverse_infer_scalar_metadata(t_transpiler_context *context,
    const t_ast_node *name_node, const t_cobol_reverse_picture *picture,
    const char *error_message, t_cobol_reverse_scalar_metadata *out_metadata)
{
    const char *type_text;
    int is_array;
    size_t array_length;
    int is_boolean_type;

    if (!name_node || !picture || !out_metadata)
        return (FT_FAILURE);
    ft_bzero(out_metadata, sizeof(*out_metadata));
    type_text = NULL;
    is_array = 0;
    array_length = 0;
    is_boolean_type = 0;
    if (picture->kind == COBOL_REVERSE_PICTURE_NUMERIC)
    {
        size_t integer_digits;
        size_t fractional_digits;
        size_t total_digits;

        integer_digits = picture->length;
        fractional_digits = picture->fractional_length;
        total_digits = integer_digits + fractional_digits;
        if (picture->usage == COBOL_REVERSE_USAGE_COMP_1)
            type_text = "float";
        else if (picture->usage == COBOL_REVERSE_USAGE_COMP_2)
            type_text = "double";
        else if (fractional_digits > 0)
        {
            if (total_digits <= 9)
                type_text = "float";
            else
                type_text = "double";
        }
        else
        {
            if (integer_digits <= 9)
                type_text = "int";
            else if (integer_digits <= 18)
                type_text = "long";
            else
                type_text = "long long";
        }
    }
    else if (picture->kind == COBOL_REVERSE_PICTURE_ALPHANUMERIC)
    {
        int treat_as_flag;
        int treat_as_buffer;
        size_t effective_length;
        size_t context_length;
        int has_context_length;

        treat_as_flag = 0;
        treat_as_buffer = 0;
        effective_length = picture->length;
        context_length = 0;
        has_context_length = 0;
        if (context && name_node && name_node->token.lexeme)
        {
            const t_transpiler_data_item *context_item;

            context_item = transpiler_context_find_data_item(context,
                    name_node->token.lexeme);
            if (context_item && context_item->kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && context_item->declared_length > 0)
            {
                context_length = context_item->declared_length;
                has_context_length = 1;
            }
        }
        if (has_context_length)
        {
            if (effective_length == 0 || effective_length < context_length)
                effective_length = context_length;
        }
        if (effective_length == 0)
            effective_length = 1;
        if (effective_length <= 1)
        {
            treat_as_flag = cobol_reverse_identifier_token_is_flag(&name_node->token);
            treat_as_buffer = cobol_reverse_identifier_token_matches_any_marker(&name_node->token,
                    g_cobol_reverse_character_array_markers);
            if (treat_as_flag)
                type_text = "bool";
            else
            {
                type_text = "char";
                if (treat_as_buffer)
                {
                    is_array = 1;
                    array_length = effective_length;
                    if (array_length == 0)
                        array_length = 1;
                }
            }
        }
        else
        {
            type_text = "char";
            is_array = 1;
            array_length = effective_length;
            if (array_length == 0)
                array_length = 1;
        }
    }
    if (!type_text)
    {
        if (context && error_message)
            cobol_reverse_emit_error(context, error_message);
        return (FT_FAILURE);
    }
    if (ft_strcmp(type_text, "bool") == 0)
        is_boolean_type = 1;
    out_metadata->type_text = type_text;
    out_metadata->is_array = is_array;
    out_metadata->array_length = array_length;
    out_metadata->is_boolean_type = is_boolean_type;
    return (FT_SUCCESS);
}

static int cobol_reverse_parse_size_literal(const t_ast_node *literal, size_t *out_value)
{
    const char *text;
    size_t index;
    size_t value;

    if (!literal || !out_value)
        return (FT_FAILURE);
    if (!literal->token.lexeme)
        return (FT_FAILURE);
    text = literal->token.lexeme;
    value = 0;
    index = 0;
    while (index < literal->token.length)
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

static int cobol_reverse_extract_occurs_metadata(const t_ast_node *occurs_node,
    t_cobol_reverse_occurs_metadata *out_metadata)
{
    size_t index;
    const t_ast_node *lower_literal;
    const t_ast_node *upper_literal;
    const t_ast_node *identifier_node;

    if (!out_metadata)
        return (FT_FAILURE);
    ft_bzero(out_metadata, sizeof(*out_metadata));
    if (!occurs_node)
        return (FT_SUCCESS);
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
    if (cobol_reverse_parse_size_literal(lower_literal,
            &out_metadata->minimum) != FT_SUCCESS)
        return (FT_FAILURE);
    if (upper_literal)
    {
        if (cobol_reverse_parse_size_literal(upper_literal,
                &out_metadata->maximum) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
        out_metadata->maximum = out_metadata->minimum;
    if (out_metadata->maximum < out_metadata->minimum)
        return (FT_FAILURE);
    out_metadata->present = 1;
    out_metadata->has_depending_on = 0;
    out_metadata->controller_node = NULL;
    if (identifier_node && identifier_node->token.lexeme)
    {
        out_metadata->has_depending_on = 1;
        out_metadata->controller_node = identifier_node;
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_identifier_has_suffix(const t_lexer_token *token, const char *suffix)
{
    t_cblc_builder builder;
    size_t suffix_length;
    size_t index;
    size_t suffix_index;

    if (!token || !suffix)
        return (0);
    suffix_length = ft_strlen(suffix);
    if (suffix_length == 0)
        return (0);
    cblc_builder_init(&builder);
    if (cobol_reverse_append_identifier(&builder, token) != FT_SUCCESS)
    {
        cblc_builder_dispose(&builder);
        return (0);
    }
    if (builder.length < suffix_length)
    {
        cblc_builder_dispose(&builder);
        return (0);
    }
    index = builder.length;
    suffix_index = suffix_length;
    while (suffix_index > 0)
    {
        char identifier_char;
        char suffix_char;

        identifier_char = builder.data[index - 1];
        suffix_char = suffix[suffix_index - 1];
        if (suffix_char >= 'a' && suffix_char <= 'z')
            suffix_char = static_cast<char>(suffix_char - ('a' - 'A'));
        if (suffix_char >= 'A' && suffix_char <= 'Z')
        {
            if (identifier_char != suffix_char)
            {
                cblc_builder_dispose(&builder);
                return (0);
            }
        }
        else
        {
            if (identifier_char != suffix_char)
            {
                cblc_builder_dispose(&builder);
                return (0);
            }
        }
        index -= 1;
        suffix_index -= 1;
    }
    if (index > 0)
    {
        char preceding;

        preceding = builder.data[index - 1];
        if ((preceding >= 'A' && preceding <= 'Z')
            || (preceding >= '0' && preceding <= '9'))
        {
            cblc_builder_dispose(&builder);
            return (0);
        }
    }
    cblc_builder_dispose(&builder);
    return (1);
}

static int cobol_reverse_identifier_token_matches_any_marker(const t_lexer_token *token,
    const char **markers)
{
    t_cblc_builder builder;
    size_t marker_index;
    int result;

    if (!token || !markers)
        return (0);
    cblc_builder_init(&builder);
    if (cobol_reverse_append_identifier(&builder, token) != FT_SUCCESS)
    {
        cblc_builder_dispose(&builder);
        return (0);
    }
    if (!builder.data)
    {
        cblc_builder_dispose(&builder);
        return (0);
    }
    result = 0;
    marker_index = 0;
    while (markers[marker_index])
    {
        size_t marker_length;
        size_t haystack_index;

        marker_length = ft_strlen(markers[marker_index]);
        if (marker_length == 0)
        {
            marker_index += 1;
            continue ;
        }
        haystack_index = 0;
        while (haystack_index + marker_length <= builder.length)
        {
            if (ft_strncmp(builder.data + haystack_index, markers[marker_index], marker_length) == 0)
            {
                result = 1;
                break ;
            }
            haystack_index += 1;
        }
        if (result)
            break ;
        marker_index += 1;
    }
    cblc_builder_dispose(&builder);
    return (result);
}

static int cobol_reverse_identifier_token_is_flag(const t_lexer_token *token)
{
    size_t index;

    if (!token)
        return (0);
    index = 0;
    while (g_cobol_reverse_flag_suffixes[index])
    {
        if (cobol_reverse_identifier_has_suffix(token, g_cobol_reverse_flag_suffixes[index]))
            return (1);
        index += 1;
    }
    return (0);
}

static int cobol_reverse_append_operator(t_cblc_builder *builder, const t_ast_node *node)
{
    const char *text;

    if (!builder)
        return (FT_FAILURE);
    if (!node)
        return (FT_FAILURE);
    text = NULL;
    if (node->token.kind == LEXER_TOKEN_EQUALS)
        text = "==";
    else if (node->token.kind == LEXER_TOKEN_NOT_EQUALS)
        text = "!=";
    else if (node->token.kind == LEXER_TOKEN_LESS_THAN)
        text = "<";
    else if (node->token.kind == LEXER_TOKEN_LESS_OR_EQUAL)
        text = "<=";
    else if (node->token.kind == LEXER_TOKEN_GREATER_THAN)
        text = ">";
    else if (node->token.kind == LEXER_TOKEN_GREATER_OR_EQUAL)
        text = ">=";
    if (!text)
        return (FT_FAILURE);
    return (cblc_builder_append_string(builder, text));
}

static int cobol_reverse_append_condition(t_cblc_builder *builder, const t_ast_node *condition, int invert)
{
    const t_ast_node *left;
    const t_ast_node *op;
    const t_ast_node *right;
    int negate;
    const t_cobol_reverse_condition_entry *left_condition;
    const t_cobol_reverse_condition_entry *right_condition;
    int handled;

    if (!builder)
        return (FT_FAILURE);
    if (!condition)
        return (FT_FAILURE);
    if (ast_node_child_count(condition) < 3)
        return (FT_FAILURE);
    left = ast_node_get_child(condition, 0);
    op = ast_node_get_child(condition, 1);
    right = ast_node_get_child(condition, 2);
    negate = invert;
    if (condition->token.kind == LEXER_TOKEN_KEYWORD_NOT)
        negate = negate ? 0 : 1;
    if (negate)
    {
        if (cblc_builder_append_string(builder, "!(") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    left_condition = cobol_reverse_condition_table_find(&left->token);
    right_condition = cobol_reverse_condition_table_find(&right->token);
    handled = 0;
    if (left_condition && right && right->kind == AST_NODE_LITERAL
        && (right->token.kind == LEXER_TOKEN_KEYWORD_TRUE
            || right->token.kind == LEXER_TOKEN_KEYWORD_FALSE))
    {
        const char *operator_text;
        int is_true;

        is_true = (right->token.kind == LEXER_TOKEN_KEYWORD_TRUE);
        if (op->token.kind == LEXER_TOKEN_EQUALS || op->token.kind == LEXER_TOKEN_ASSIGN)
            operator_text = is_true ? "==" : "!=";
        else if (op->token.kind == LEXER_TOKEN_NOT_EQUALS)
            operator_text = is_true ? "!=" : "==";
        else
            operator_text = NULL;
        if (!operator_text)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, left_condition->parent) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, operator_text) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, left_condition->name) != FT_SUCCESS)
            return (FT_FAILURE);
        handled = 1;
    }
    else if (right_condition && left && left->kind == AST_NODE_LITERAL
        && (left->token.kind == LEXER_TOKEN_KEYWORD_TRUE
            || left->token.kind == LEXER_TOKEN_KEYWORD_FALSE))
    {
        const char *operator_text;
        int is_true;

        is_true = (left->token.kind == LEXER_TOKEN_KEYWORD_TRUE);
        if (op->token.kind == LEXER_TOKEN_EQUALS || op->token.kind == LEXER_TOKEN_ASSIGN)
            operator_text = is_true ? "==" : "!=";
        else if (op->token.kind == LEXER_TOKEN_NOT_EQUALS)
            operator_text = is_true ? "!=" : "==";
        else
            operator_text = NULL;
        if (!operator_text)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, right_condition->parent) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, operator_text) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, right_condition->name) != FT_SUCCESS)
            return (FT_FAILURE);
        handled = 1;
    }
    if (!handled)
    {
        if (cobol_reverse_append_value(builder, left) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_operator(builder, op) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_value(builder, right) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (negate)
    {
        if (cblc_builder_append_char(builder, ')') != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_statement_sequence(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *sequence, size_t indentation);

static int cobol_reverse_emit_assignment(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *source;
    const t_ast_node *target;

    (void)context;
    if (!builder)
        return (FT_FAILURE);
    if (!statement)
        return (FT_FAILURE);
    if (statement->kind != AST_NODE_MOVE_STATEMENT
        && statement->kind != AST_NODE_ASSIGNMENT_STATEMENT)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 2)
        return (FT_FAILURE);
    source = ast_node_get_child(statement, 0);
    target = ast_node_get_child(statement, 1);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, target) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " = ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, source) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_if(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *condition;
    const t_ast_node *then_sequence;
    const t_ast_node *else_sequence;

    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 2)
        return (FT_FAILURE);
    condition = ast_node_get_child(statement, 0);
    then_sequence = ast_node_get_child(statement, 1);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "if (") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_condition(builder, condition, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ") {") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_emit_statement_sequence(context, builder, then_sequence, indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "}") != FT_SUCCESS)
        return (FT_FAILURE);
    else_sequence = NULL;
    if (ast_node_child_count(statement) > 2)
        else_sequence = ast_node_get_child(statement, 2);
    if (else_sequence)
    {
        if (cblc_builder_append_string(builder, " else {") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_newline(builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_emit_statement_sequence(context, builder, else_sequence, indentation + 1) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, "}") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_perform_until(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *condition;
    const t_ast_node *body;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 2)
        return (FT_FAILURE);
    condition = ast_node_get_child(statement, 0);
    body = ast_node_get_child(statement, 1);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "while (") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_condition(builder, condition, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ") {") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_emit_statement_sequence(context, builder, body, indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "}") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_step_is_unit_increment(const t_ast_node *step, int *direction)
{
    size_t index;
    int sign;

    if (!step)
        return (0);
    if (!direction)
        return (0);
    if (step->kind != AST_NODE_LITERAL)
        return (0);
    if (step->token.kind != LEXER_TOKEN_NUMERIC_LITERAL)
        return (0);
    if (!step->token.lexeme)
        return (0);
    if (step->token.length == 0)
        return (0);
    sign = 1;
    index = 0;
    if (step->token.lexeme[index] == '+' || step->token.lexeme[index] == '-')
    {
        if (step->token.lexeme[index] == '-')
            sign = -1;
        index += 1;
    }
    while (index < step->token.length && step->token.lexeme[index] == '0')
        index += 1;
    if (index >= step->token.length)
        return (0);
    if (step->token.lexeme[index] != '1')
        return (0);
    index += 1;
    if (index < step->token.length)
        return (0);
    *direction = sign;
    return (1);
}

static int cobol_reverse_emit_perform_varying(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *counter;
    const t_ast_node *initial;
    const t_ast_node *step;
    const t_ast_node *condition;
    const t_ast_node *body;
    int direction;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 5)
        return (FT_FAILURE);
    counter = ast_node_get_child(statement, 0);
    initial = ast_node_get_child(statement, 1);
    step = ast_node_get_child(statement, 2);
    condition = ast_node_get_child(statement, 3);
    body = ast_node_get_child(statement, 4);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, counter) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " = ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, initial) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "while (") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_condition(builder, condition, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ") {") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_emit_statement_sequence(context, builder, body, indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_indentation(builder, indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, counter) != FT_SUCCESS)
        return (FT_FAILURE);
    direction = 0;
    if (cobol_reverse_step_is_unit_increment(step, &direction))
    {
        if (direction > 0)
        {
            if (cblc_builder_append_string(builder, "++") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_builder_append_string(builder, "--") != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else
    {
        if (cblc_builder_append_string(builder, " = ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_value(builder, counter) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " + ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_value(builder, step) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "}") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_open(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *file_node;
    const char *mode;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 1)
        return (FT_FAILURE);
    file_node = ast_node_get_child(statement, 0);
    mode = "r";
    if (cobol_reverse_token_equals(&statement->token, "OUTPUT"))
        mode = "w";
    else if (cobol_reverse_token_equals(&statement->token, "EXTEND"))
        mode = "a";
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "open(") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, file_node) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ", \"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, mode) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_char(builder, '"') != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ");") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_close(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *file_node;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 1)
        return (FT_FAILURE);
    file_node = ast_node_get_child(statement, 0);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "close(") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, file_node) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ");") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_read(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *file_node;
    const t_ast_node *target_node;
    const char *call_name;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 1)
        return (FT_FAILURE);
    file_node = ast_node_get_child(statement, 0);
    target_node = NULL;
    if (ast_node_child_count(statement) > 1)
        target_node = ast_node_get_child(statement, 1);
    call_name = "read";
    if ((statement->flags & AST_READ_FLAG_NEXT) != 0)
    {
        if ((statement->flags & AST_READ_FLAG_WITH_LOCK) != 0)
            call_name = "read_next_with_lock";
        else if ((statement->flags & AST_READ_FLAG_WITH_NO_LOCK) != 0)
            call_name = "read_next_with_no_lock";
        else
            call_name = "read_next";
    }
    else if ((statement->flags & AST_READ_FLAG_WITH_LOCK) != 0)
        call_name = "read_with_lock";
    else if ((statement->flags & AST_READ_FLAG_WITH_NO_LOCK) != 0)
        call_name = "read_with_no_lock";
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, call_name) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "(") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, file_node) != FT_SUCCESS)
        return (FT_FAILURE);
    if (target_node)
    {
        if (cblc_builder_append_string(builder, ", ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_value(builder, target_node) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, ");") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_write(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *file_node;
    const t_ast_node *source_node;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 1)
        return (FT_FAILURE);
    file_node = ast_node_get_child(statement, 0);
    source_node = NULL;
    if (ast_node_child_count(statement) > 1)
        source_node = ast_node_get_child(statement, 1);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "write(") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, file_node) != FT_SUCCESS)
        return (FT_FAILURE);
    if (source_node)
    {
        if (cblc_builder_append_string(builder, ", ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_value(builder, source_node) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, ");") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_display(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *value_node;

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (statement->kind != AST_NODE_DISPLAY_STATEMENT)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 1)
        return (FT_FAILURE);
    value_node = ast_node_get_child(statement, 0);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "display(") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, value_node) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, ");") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_stop(t_transpiler_context *context, t_cblc_builder *builder,
    size_t indentation)
{
    (void)context;
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "return ;") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_statement(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    if (!statement)
        return (FT_FAILURE);
    if (statement->kind == AST_NODE_MOVE_STATEMENT
        || statement->kind == AST_NODE_ASSIGNMENT_STATEMENT)
        return (cobol_reverse_emit_assignment(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_IF_STATEMENT)
        return (cobol_reverse_emit_if(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_PERFORM_UNTIL_STATEMENT)
        return (cobol_reverse_emit_perform_until(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_PERFORM_VARYING_STATEMENT)
        return (cobol_reverse_emit_perform_varying(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_OPEN_STATEMENT)
        return (cobol_reverse_emit_open(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_CLOSE_STATEMENT)
        return (cobol_reverse_emit_close(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_READ_STATEMENT)
        return (cobol_reverse_emit_read(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_WRITE_STATEMENT)
        return (cobol_reverse_emit_write(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_DISPLAY_STATEMENT)
        return (cobol_reverse_emit_display(context, builder, statement, indentation));
    if (statement->kind == AST_NODE_STOP_STATEMENT)
        return (cobol_reverse_emit_stop(context, builder, indentation));
    return (FT_FAILURE);
}

static int cobol_reverse_emit_statement_sequence(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *sequence, size_t indentation)
{
    size_t index;

    if (!sequence)
        return (FT_FAILURE);
    index = 0;
    while (index < ast_node_child_count(sequence))
    {
        const t_ast_node *statement;
        size_t statement_line;

        statement = ast_node_get_child(sequence, index);
        statement_line = cobol_reverse_node_line(statement);
        if (context && statement_line > 0)
        {
            if (cobol_reverse_emit_comments_before_line(context, builder, statement_line, indentation) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cobol_reverse_emit_statement(context, builder, statement, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        if (context && statement_line > 0)
        {
            if (cobol_reverse_emit_comments_on_line(context, builder, statement_line, indentation) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_paragraph(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *paragraph)
{
    const t_ast_node *body;
    size_t paragraph_line;

    if (!paragraph)
        return (FT_FAILURE);
    if (paragraph->kind != AST_NODE_PARAGRAPH)
        return (FT_FAILURE);
    paragraph_line = cobol_reverse_node_line(paragraph);
    if (context && paragraph_line > 0)
    {
        if (cobol_reverse_emit_comments_before_line(context, builder, paragraph_line, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, "function void ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_identifier(builder, &paragraph->token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "() {") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (context && paragraph_line > 0)
    {
        if (cobol_reverse_emit_comments_on_line(context, builder, paragraph_line, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (ast_node_child_count(paragraph) < 1)
        return (FT_FAILURE);
    body = ast_node_get_child(paragraph, 0);
    if (cobol_reverse_emit_statement_sequence(context, builder, body, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "}") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_data_item(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *section, size_t *item_index)
{
    t_cobol_reverse_data_item_info info;
    long level_value;
    t_cobol_reverse_picture picture;
    t_cobol_reverse_scalar_metadata metadata;
    t_cobol_reverse_occurs_metadata occurs;
    const t_ast_node *value_literal;
    size_t anchor_line;
    const t_ast_node *item;
    size_t index;
    size_t section_count;
    t_cobol_reverse_data_item_info condition_info;
    t_cobol_reverse_data_item_info *conditions;
    size_t condition_count;
    size_t condition_capacity;

    if (!builder)
        return (FT_FAILURE);
    if (!section)
        return (FT_FAILURE);
    if (!item_index)
        return (FT_FAILURE);
    section_count = ast_node_child_count(section);
    if (*item_index >= section_count)
        return (FT_FAILURE);
    item = ast_node_get_child(section, *item_index);
    if (!item)
        return (FT_FAILURE);
    if (cobol_reverse_collect_data_item_info(item, &info) != FT_SUCCESS)
    {
        if (context)
            cobol_reverse_emit_error(context, "WORKING-STORAGE item missing level or name");
        return (FT_FAILURE);
    }
    anchor_line = cobol_reverse_node_line(info.name_node);
    if (anchor_line == 0)
        anchor_line = cobol_reverse_node_line(item);
    if (context && anchor_line > 0)
    {
        if (cobol_reverse_emit_comments_before_line(context, builder, anchor_line, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cobol_reverse_get_data_item_level(&info, &level_value) != FT_SUCCESS)
    {
        if (context)
            cobol_reverse_emit_error(context, "Invalid WORKING-STORAGE metadata");
        return (FT_FAILURE);
    }
    if (level_value == 66 || level_value == 88)
    {
        if (context)
            cobol_reverse_emit_error(context, "Unsupported WORKING-STORAGE level in reverse translation");
        return (FT_FAILURE);
    }
    if (level_value != 77 && (level_value < 1 || level_value > 49))
    {
        if (context)
            cobol_reverse_emit_error(context, "Unsupported WORKING-STORAGE level in reverse translation");
        return (FT_FAILURE);
    }
    if (!info.picture_node)
    {
        if (context)
            cobol_reverse_emit_error(context, "PIC clause required for reverse data item recovery");
        return (FT_FAILURE);
    }
    if (cobol_reverse_parse_picture(info.picture_node, &picture) != FT_SUCCESS)
    {
        if (context)
            cobol_reverse_emit_error(context, "Unsupported PIC clause in WORKING-STORAGE section");
        return (FT_FAILURE);
    }
    if (cobol_reverse_infer_scalar_metadata(context, info.name_node, &picture,
            "Unsupported WORKING-STORAGE item", &metadata) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(&occurs, sizeof(occurs));
    if (info.occurs_node)
    {
        if (cobol_reverse_extract_occurs_metadata(info.occurs_node,
                &occurs) != FT_SUCCESS)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Unsupported OCCURS clause in WORKING-STORAGE section");
            return (FT_FAILURE);
        }
    }
    if (cblc_builder_append_string(builder, metadata.type_text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_identifier(builder, &info.name_node->token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (occurs.present)
    {
        if (cblc_builder_append_char(builder, '[') != FT_SUCCESS)
            return (FT_FAILURE);
        if (occurs.has_depending_on && occurs.controller_node
            && occurs.controller_node->token.lexeme)
        {
            if (cobol_reverse_append_identifier(builder,
                    &occurs.controller_node->token) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_builder_append_unsigned(builder,
                    static_cast<unsigned long long>(occurs.maximum)) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_char(builder, ']') != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (metadata.is_array)
    {
        if (cblc_builder_append_char(builder, '[') != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_unsigned(builder,
                static_cast<unsigned long long>(metadata.array_length)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_char(builder, ']') != FT_SUCCESS)
            return (FT_FAILURE);
    }
    value_literal = NULL;
    if (info.value_node && ast_node_child_count(info.value_node) > 0)
        value_literal = ast_node_get_child(info.value_node, 0);
    if (value_literal)
    {
        const t_lexer_token *value_token;

        value_token = &value_literal->token;
        if (cblc_builder_append_string(builder, " = ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (value_literal->kind != AST_NODE_LITERAL)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Unsupported VALUE clause literal in WORKING-STORAGE section");
            return (FT_FAILURE);
        }
        if (value_token->kind == LEXER_TOKEN_STRING_LITERAL)
        {
            if (metadata.is_boolean_type)
            {
                const char *boolean_text;

                boolean_text = cobol_reverse_boolean_from_string_literal(value_token);
                if (!boolean_text)
                {
                    if (context)
                        cobol_reverse_emit_error(context,
                            "Unsupported VALUE clause literal in WORKING-STORAGE section");
                    return (FT_FAILURE);
                }
                if (cblc_builder_append_string(builder, boolean_text) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (cobol_reverse_append_string_literal(builder, value_token, 0) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        else if (value_token->kind == LEXER_TOKEN_NUMERIC_LITERAL)
        {
            if (cobol_reverse_append_numeric_literal(builder, value_token) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (value_token->kind == LEXER_TOKEN_KEYWORD_TRUE)
        {
            if (!metadata.is_boolean_type)
            {
                if (context)
                    cobol_reverse_emit_error(context,
                        "Unsupported VALUE clause literal in WORKING-STORAGE section");
                return (FT_FAILURE);
            }
            if (cblc_builder_append_string(builder, "true") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (value_token->kind == LEXER_TOKEN_KEYWORD_FALSE)
        {
            if (!metadata.is_boolean_type)
            {
                if (context)
                    cobol_reverse_emit_error(context,
                        "Unsupported VALUE clause literal in WORKING-STORAGE section");
                return (FT_FAILURE);
            }
            if (cblc_builder_append_string(builder, "false") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Unsupported VALUE clause literal in WORKING-STORAGE section");
            return (FT_FAILURE);
        }
    }
    if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    conditions = NULL;
    condition_count = 0;
    condition_capacity = 0;
    index = (*item_index) + 1;
    while (index < section_count)
    {
        const t_ast_node *candidate;
        long candidate_level;
        const t_ast_node *condition_literal;

        candidate = ast_node_get_child(section, index);
        if (!candidate || candidate->kind != AST_NODE_DATA_ITEM)
            break ;
        if (cobol_reverse_collect_data_item_info(candidate, &condition_info) != FT_SUCCESS)
            break ;
        if (cobol_reverse_get_data_item_level(&condition_info, &candidate_level) != FT_SUCCESS)
            break ;
        if (candidate_level != 88)
            break ;
        if (condition_info.value_node == NULL
            || ast_node_child_count(condition_info.value_node) == 0)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Condition name missing VALUE literal in WORKING-STORAGE section");
            if (conditions)
                cma_free(conditions);
            return (FT_FAILURE);
        }
        condition_literal = ast_node_get_child(condition_info.value_node, 0);
        if (!condition_literal)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Condition name missing VALUE literal in WORKING-STORAGE section");
            if (conditions)
                cma_free(conditions);
            return (FT_FAILURE);
        }
        if (condition_count >= condition_capacity)
        {
            size_t desired_capacity;
            t_cobol_reverse_data_item_info *new_conditions;

            desired_capacity = condition_capacity;
            if (desired_capacity == 0)
                desired_capacity = 2;
            else
                desired_capacity *= 2;
            new_conditions = static_cast<t_cobol_reverse_data_item_info *>(cma_calloc(desired_capacity,
                    sizeof(t_cobol_reverse_data_item_info)));
            if (!new_conditions)
            {
                if (conditions)
                    cma_free(conditions);
                return (FT_FAILURE);
            }
            if (conditions && condition_count > 0)
                ft_memcpy(new_conditions, conditions,
                    condition_count * sizeof(t_cobol_reverse_data_item_info));
            if (conditions)
                cma_free(conditions);
            conditions = new_conditions;
            condition_capacity = desired_capacity;
        }
        conditions[condition_count] = condition_info;
        if (cobol_reverse_condition_table_add(&info.name_node->token,
                &condition_info.name_node->token, condition_literal) != FT_SUCCESS)
        {
            if (conditions)
                cma_free(conditions);
            return (FT_FAILURE);
        }
        condition_count += 1;
        index += 1;
    }
    if (condition_count > 0)
    {
        char parent_identifier[TRANSPILE_IDENTIFIER_MAX];
        char enum_name[(TRANSPILE_IDENTIFIER_MAX * 2)];
        size_t condition_index;

        if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
        ft_bzero(parent_identifier, sizeof(parent_identifier));
        if (cobol_reverse_token_to_identifier(&info.name_node->token, parent_identifier,
                sizeof(parent_identifier)) != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
        ft_bzero(enum_name, sizeof(enum_name));
        pf_snprintf(enum_name, sizeof(enum_name), "%s_CONDITIONS", parent_identifier);
        if (cblc_builder_append_string(builder, "enum ") != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
        if (cblc_builder_append_string(builder, enum_name) != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
        if (cblc_builder_append_string(builder, " {\n") != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
        condition_index = 0;
        while (condition_index < condition_count)
        {
            char condition_identifier[TRANSPILE_IDENTIFIER_MAX];
            const t_ast_node *condition_literal;

            ft_bzero(condition_identifier, sizeof(condition_identifier));
            if (cobol_reverse_token_to_identifier(&conditions[condition_index].name_node->token,
                    condition_identifier, sizeof(condition_identifier)) != FT_SUCCESS)
            {
                cma_free(conditions);
                return (FT_FAILURE);
            }
            if (cblc_builder_append_string(builder, "    ") != FT_SUCCESS)
            {
                cma_free(conditions);
                return (FT_FAILURE);
            }
            if (cblc_builder_append_string(builder, condition_identifier) != FT_SUCCESS)
            {
                cma_free(conditions);
                return (FT_FAILURE);
            }
            if (cblc_builder_append_string(builder, " = ") != FT_SUCCESS)
            {
                cma_free(conditions);
                return (FT_FAILURE);
            }
            condition_literal = ast_node_get_child(conditions[condition_index].value_node, 0);
            if (!condition_literal)
            {
                cma_free(conditions);
                return (FT_FAILURE);
            }
            if (cobol_reverse_append_condition_literal(builder, condition_literal) != FT_SUCCESS)
            {
                cma_free(conditions);
                return (FT_FAILURE);
            }
            if (condition_index + 1 < condition_count)
            {
                if (cblc_builder_append_string(builder, ",\n") != FT_SUCCESS)
                {
                    cma_free(conditions);
                    return (FT_FAILURE);
                }
            }
            else
            {
                if (cblc_builder_append_newline(builder) != FT_SUCCESS)
                {
                    cma_free(conditions);
                    return (FT_FAILURE);
                }
            }
            condition_index += 1;
        }
        if (cblc_builder_append_string(builder, "};\n") != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
        if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        {
            cma_free(conditions);
            return (FT_FAILURE);
        }
    }
    if (conditions)
        cma_free(conditions);
    if (condition_count > 0)
        *item_index = index - 1;
    if (context && anchor_line > 0)
    {
        if (cobol_reverse_emit_comments_on_line(context, builder, anchor_line, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_group_item(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *section, size_t item_index)
{
    t_cobol_reverse_data_item_info group_info;
    long group_level;
    t_cobol_reverse_occurs_metadata group_occurs;
    size_t child_index;
    size_t section_count;
    size_t field_count;
    size_t group_line;
    const t_ast_node *group_node;

    if (!builder || !section)
        return (FT_FAILURE);

    group_node = NULL;
    if (section)
        group_node = ast_node_get_child(section, item_index);
    if (cobol_reverse_collect_data_item_info(group_node,
            &group_info) != FT_SUCCESS)
    {
        if (context)
            cobol_reverse_emit_error(context, "Group item missing identifier");
        return (FT_FAILURE);
    }
    group_line = cobol_reverse_node_line(group_info.name_node);
    if (group_line == 0)
        group_line = cobol_reverse_node_line(group_node);
    if (context && group_line > 0)
    {
        if (cobol_reverse_emit_comments_before_line(context, builder, group_line, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cobol_reverse_get_data_item_level(&group_info, &group_level) != FT_SUCCESS)
    {
        if (context)
            cobol_reverse_emit_error(context, "Group item missing level metadata");
        return (FT_FAILURE);
    }
    if (group_info.picture_node)
    {
        if (context)
            cobol_reverse_emit_error(context, "Group items with PIC clauses are not supported in reverse translation");
        return (FT_FAILURE);
    }
    if (group_info.value_node && ast_node_child_count(group_info.value_node) > 0)
    {
        if (context)
            cobol_reverse_emit_error(context, "Group items with VALUE clauses are not supported in reverse translation");
        return (FT_FAILURE);
    }
    ft_bzero(&group_occurs, sizeof(group_occurs));
    if (group_info.occurs_node)
    {
        if (cobol_reverse_extract_occurs_metadata(group_info.occurs_node,
                &group_occurs) != FT_SUCCESS)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Unsupported OCCURS clause on group item in WORKING-STORAGE section");
            return (FT_FAILURE);
        }
    }
    if (cblc_builder_append_string(builder, "record ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_identifier(builder, &group_info.name_node->token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " {\n") != FT_SUCCESS)
        return (FT_FAILURE);
    section_count = ast_node_child_count(section);
    child_index = item_index + 1;
    field_count = 0;
    while (child_index < section_count)
    {
        const t_ast_node *child;
        t_cobol_reverse_data_item_info field_info;
        long field_level;
        t_cobol_reverse_picture picture;
        t_cobol_reverse_scalar_metadata metadata;
        t_cobol_reverse_occurs_metadata field_occurs;
        size_t field_line;

        child = ast_node_get_child(section, child_index);
        if (!child || child->kind != AST_NODE_DATA_ITEM)
            break;
        if (cobol_reverse_collect_data_item_info(child, &field_info) != FT_SUCCESS)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Group member missing identifier or level");
            return (FT_FAILURE);
        }
        if (cobol_reverse_get_data_item_level(&field_info, &field_level) != FT_SUCCESS)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Group member missing level metadata");
            return (FT_FAILURE);
        }
        if (field_level <= group_level)
            break;
        if (!field_info.picture_node)
        {
            if (field_level == 88)
            {
                child_index += 1;
                continue ;
            }
            if (context)
                cobol_reverse_emit_error(context,
                    "Nested group items are not supported in reverse translation");
            return (FT_FAILURE);
        }
        if (field_info.value_node && ast_node_child_count(field_info.value_node) > 0)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "VALUE clauses on group members are not supported in reverse translation");
            return (FT_FAILURE);
        }
        if (cobol_reverse_parse_picture(field_info.picture_node, &picture) != FT_SUCCESS)
        {
            if (context)
                cobol_reverse_emit_error(context,
                    "Unsupported PIC clause in group item");
            return (FT_FAILURE);
        }
        if (cobol_reverse_infer_scalar_metadata(context, field_info.name_node, &picture,
                "Unsupported group item member", &metadata) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_bzero(&field_occurs, sizeof(field_occurs));
        if (field_info.occurs_node)
        {
            if (cobol_reverse_extract_occurs_metadata(field_info.occurs_node,
                    &field_occurs) != FT_SUCCESS)
            {
                if (context)
                    cobol_reverse_emit_error(context,
                        "Unsupported OCCURS clause on group member in WORKING-STORAGE section");
                return (FT_FAILURE);
            }
        }
        field_line = cobol_reverse_node_line(field_info.name_node);
        if (field_line == 0)
            field_line = cobol_reverse_node_line(child);
        if (context && field_line > 0)
        {
            if (cobol_reverse_emit_comments_before_line(context, builder, field_line, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_indentation(builder, 1) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, metadata.type_text) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_append_identifier(builder, &field_info.name_node->token) != FT_SUCCESS)
            return (FT_FAILURE);
        if (field_occurs.present)
        {
            if (cblc_builder_append_char(builder, '[') != FT_SUCCESS)
                return (FT_FAILURE);
            if (field_occurs.has_depending_on && field_occurs.controller_node
                && field_occurs.controller_node->token.lexeme)
            {
                if (cobol_reverse_append_identifier(builder,
                        &field_occurs.controller_node->token) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (cblc_builder_append_unsigned(builder,
                        static_cast<unsigned long long>(field_occurs.maximum)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cblc_builder_append_char(builder, ']') != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (metadata.is_array)
        {
            if (cblc_builder_append_char(builder, '[') != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_unsigned(builder,
                    static_cast<unsigned long long>(metadata.array_length)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_char(builder, ']') != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_newline(builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (context && field_line > 0)
        {
            if (cobol_reverse_emit_comments_on_line(context, builder, field_line, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        field_count += 1;
        child_index += 1;
    }
    if (field_count == 0)
    {
        if (context)
            cobol_reverse_emit_error(context, "Group item missing subordinate fields");
        return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, "};") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_identifier(builder, &group_info.name_node->token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_identifier(builder, &group_info.name_node->token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (group_occurs.present)
    {
        if (cblc_builder_append_char(builder, '[') != FT_SUCCESS)
            return (FT_FAILURE);
        if (group_occurs.has_depending_on && group_occurs.controller_node
            && group_occurs.controller_node->token.lexeme)
        {
            if (cobol_reverse_append_identifier(builder,
                    &group_occurs.controller_node->token) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_builder_append_unsigned(builder,
                    static_cast<unsigned long long>(group_occurs.maximum)) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_char(builder, ']') != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (context && group_line > 0)
    {
        if (cobol_reverse_emit_comments_on_line(context, builder, group_line, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static const char *cobol_reverse_copybook_item_kind_label(t_transpiler_data_item_kind kind)
{
    if (kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC)
        return ("alphanumeric");
    if (kind == TRANSPILE_DATA_ITEM_NUMERIC)
        return ("numeric");
    if (kind == TRANSPILE_DATA_ITEM_FLOATING)
        return ("floating");
    return ("unknown");
}

static int cobol_reverse_emit_copybook_replacing_text(t_cblc_builder *builder,
    const t_ast_node *text)
{
    int is_delimited;
    int is_word;
    int has_of;
    const t_ast_node *qualifier;

    if (!builder)
        return (FT_FAILURE);
    if (!text)
        return (FT_FAILURE);
    if (text->kind != AST_NODE_COPYBOOK_REPLACING_TEXT)
        return (FT_FAILURE);
    is_delimited = (text->flags & AST_NODE_FLAG_COPYBOOK_TEXT_DELIMITED) != 0;
    is_word = (text->flags & AST_NODE_FLAG_COPYBOOK_TEXT_WORD) != 0;
    has_of = (text->flags & AST_NODE_FLAG_COPYBOOK_TEXT_HAS_OF) != 0;
    qualifier = NULL;
    if (has_of)
    {
        qualifier = ast_node_get_child(text, 0);
        if (!qualifier || qualifier->kind != AST_NODE_IDENTIFIER)
            return (FT_FAILURE);
        if (!qualifier->token.lexeme)
            return (FT_FAILURE);
    }
    if (is_word)
    {
        if (cblc_builder_append_string(builder, "word ") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (is_delimited)
    {
        if (cblc_builder_append_string(builder, "==") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (text->token.lexeme && text->token.length > 0)
    {
        if (cblc_builder_append_span(builder, text->token.lexeme, text->token.length) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (!is_delimited)
        return (FT_FAILURE);
    if (is_delimited)
    {
        if (cblc_builder_append_string(builder, "==") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (has_of)
    {
        if (cblc_builder_append_string(builder, " of ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_span(builder, qualifier->token.lexeme,
                qualifier->token.length) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_copybook_replacing_text_metadata(t_cblc_builder *builder,
    const t_transpiler_copybook_replacing_text *text)
{
    int is_delimited;

    if (!builder)
        return (FT_FAILURE);
    if (!text)
        return (FT_FAILURE);
    if ((text->flags & AST_NODE_FLAG_COPYBOOK_TEXT_WORD) != 0)
    {
        if (cblc_builder_append_string(builder, "word ") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    is_delimited = (text->flags & AST_NODE_FLAG_COPYBOOK_TEXT_DELIMITED) != 0;
    if (is_delimited)
    {
        if (cblc_builder_append_string(builder, "==") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (text->text && text->length > 0)
    {
        if (cblc_builder_append_span(builder, text->text, text->length) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (!is_delimited)
        return (FT_FAILURE);
    if (is_delimited)
    {
        if (cblc_builder_append_string(builder, "==") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if ((text->flags & AST_NODE_FLAG_COPYBOOK_TEXT_HAS_OF) != 0)
    {
        if (cblc_builder_append_string(builder, " of ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, text->qualifier) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_copybook_replacing_pair(t_cblc_builder *builder,
    const t_ast_node *pair)
{
    const t_ast_node *source_text;
    const t_ast_node *target_text;

    if (!builder)
        return (FT_FAILURE);
    if (!pair)
        return (FT_FAILURE);
    if (pair->kind != AST_NODE_COPYBOOK_REPLACING_PAIR)
        return (FT_FAILURE);
    if ((pair->flags & AST_NODE_FLAG_COPYBOOK_PAIR_LEADING)
        && (pair->flags & AST_NODE_FLAG_COPYBOOK_PAIR_TRAILING))
        return (FT_FAILURE);
    source_text = ast_node_get_child(pair, 0);
    target_text = ast_node_get_child(pair, 1);
    if (!source_text || source_text->kind != AST_NODE_COPYBOOK_REPLACING_TEXT)
        return (FT_FAILURE);
    if (!target_text || target_text->kind != AST_NODE_COPYBOOK_REPLACING_TEXT)
        return (FT_FAILURE);
    if (pair->flags & AST_NODE_FLAG_COPYBOOK_PAIR_LEADING)
    {
        if (cblc_builder_append_string(builder, "leading ") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (pair->flags & AST_NODE_FLAG_COPYBOOK_PAIR_TRAILING)
    {
        if (cblc_builder_append_string(builder, "trailing ") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cobol_reverse_emit_copybook_replacing_text(builder, source_text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " by ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_emit_copybook_replacing_text(builder, target_text) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_copybook_replacing_clause(t_cblc_builder *builder,
    const t_ast_node *include, const t_ast_node *clause)
{
    size_t index;
    size_t count;
    size_t valid_pairs;

    (void)include;
    if (!builder)
        return (FT_FAILURE);
    if (!clause)
        return (FT_FAILURE);
    if (clause->kind != AST_NODE_COPYBOOK_REPLACING)
        return (FT_FAILURE);
    count = ast_node_child_count(clause);
    valid_pairs = 0;
    index = 0;
    while (index < count)
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(clause, index);
        if (candidate && candidate->kind == AST_NODE_COPYBOOK_REPLACING_PAIR)
            valid_pairs += 1;
        index += 1;
    }
    if (valid_pairs == 0)
        return (FT_SUCCESS);
    if (cblc_builder_append_string(builder, " replacing") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < count)
    {
        const t_ast_node *pair;

        pair = ast_node_get_child(clause, index);
        if (!pair || pair->kind != AST_NODE_COPYBOOK_REPLACING_PAIR)
        {
            index += 1;
            continue ;
        }
        if (cblc_builder_append_char(builder, ' ') != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_reverse_emit_copybook_replacing_pair(builder, pair) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static const t_ast_node *cobol_reverse_copybook_find_replacing_clause(
    const t_ast_node *include)
{
    size_t child_count;
    size_t child_index;

    if (!include)
        return (NULL);
    child_count = ast_node_child_count(include);
    child_index = 1;
    while (child_index < child_count)
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(include, child_index);
        if (candidate && candidate->kind == AST_NODE_COPYBOOK_REPLACING)
            return (candidate);
        child_index += 1;
    }
    return (NULL);
}

static int cobol_reverse_emit_copybook_replacements_comment(t_cblc_builder *builder,
    const t_transpiler_copybook *copybook, const t_ast_node *include, int *out_emitted)
{
    if (!builder)
        return (FT_FAILURE);
    if (copybook && copybook->replacements && copybook->replacement_count > 0)
    {
        size_t index;

        if (cblc_builder_append_string(builder, "/* copybook replacements:\n") != FT_SUCCESS)
            return (FT_FAILURE);
        index = 0;
        while (index < copybook->replacement_count)
        {
            const t_transpiler_copybook_replacement *replacement;

            replacement = &copybook->replacements[index];
            if (cblc_builder_append_string(builder, " * - ") != FT_SUCCESS)
                return (FT_FAILURE);
            if (replacement->pair_flags & AST_NODE_FLAG_COPYBOOK_PAIR_LEADING)
            {
                if (cblc_builder_append_string(builder, "leading ") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (replacement->pair_flags & AST_NODE_FLAG_COPYBOOK_PAIR_TRAILING)
            {
                if (cblc_builder_append_string(builder, "trailing ") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_reverse_emit_copybook_replacing_text_metadata(builder,
                    &replacement->source) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_string(builder, " -> ") != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_reverse_emit_copybook_replacing_text_metadata(builder,
                    &replacement->target) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_string(builder, "\n") != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        if (cblc_builder_append_string(builder, " */\n") != FT_SUCCESS)
            return (FT_FAILURE);
        if (out_emitted)
            *out_emitted = 1;
        return (FT_SUCCESS);
    }
    if (!include)
    {
        if (out_emitted)
            *out_emitted = 0;
        return (FT_SUCCESS);
    }
    {
        const t_ast_node *clause;
        size_t index;
        size_t count;
        int has_pairs;

        clause = cobol_reverse_copybook_find_replacing_clause(include);
        if (!clause)
        {
            if (out_emitted)
                *out_emitted = 0;
            return (FT_SUCCESS);
        }
        count = ast_node_child_count(clause);
        has_pairs = 0;
        index = 0;
        while (index < count)
        {
            const t_ast_node *candidate;

            candidate = ast_node_get_child(clause, index);
            if (candidate && candidate->kind == AST_NODE_COPYBOOK_REPLACING_PAIR)
            {
                has_pairs = 1;
                break ;
            }
            index += 1;
        }
        if (!has_pairs)
        {
            if (out_emitted)
                *out_emitted = 0;
            return (FT_SUCCESS);
        }
        if (cblc_builder_append_string(builder, "/* copybook replacements:\n") != FT_SUCCESS)
            return (FT_FAILURE);
        index = 0;
        while (index < count)
        {
            const t_ast_node *pair;
            const t_ast_node *source;
            const t_ast_node *target;

            pair = ast_node_get_child(clause, index);
            if (!pair || pair->kind != AST_NODE_COPYBOOK_REPLACING_PAIR)
            {
                index += 1;
                continue ;
            }
            source = ast_node_get_child(pair, 0);
            target = ast_node_get_child(pair, 1);
            if (!source || source->kind != AST_NODE_COPYBOOK_REPLACING_TEXT)
                return (FT_FAILURE);
            if (!target || target->kind != AST_NODE_COPYBOOK_REPLACING_TEXT)
                return (FT_FAILURE);
            if (cblc_builder_append_string(builder, " * - ") != FT_SUCCESS)
                return (FT_FAILURE);
            if (pair->flags & AST_NODE_FLAG_COPYBOOK_PAIR_LEADING)
            {
                if (cblc_builder_append_string(builder, "leading ") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (pair->flags & AST_NODE_FLAG_COPYBOOK_PAIR_TRAILING)
            {
                if (cblc_builder_append_string(builder, "trailing ") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_reverse_emit_copybook_replacing_text(builder, source) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_string(builder, " -> ") != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_reverse_emit_copybook_replacing_text(builder, target) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_string(builder, "\n") != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        if (cblc_builder_append_string(builder, " */\n") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (out_emitted)
        *out_emitted = 1;
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_copybook_items_comment(t_cblc_builder *builder,
    const t_transpiler_copybook *copybook, int *out_emitted)
{
    size_t index;
    int wrote_any;

    if (!builder)
        return (FT_FAILURE);
    if (!copybook)
        return (FT_FAILURE);
    if (!copybook->items || copybook->item_count == 0)
    {
        if (out_emitted)
            *out_emitted = 0;
        return (FT_SUCCESS);
    }
    index = 0;
    wrote_any = 0;
    while (index < copybook->item_count)
    {
        const t_transpiler_copybook_item *item;

        item = &copybook->items[index];
        if (item->name[0] != '\0' || item->declared_length > 0 || item->kind != TRANSPILE_DATA_ITEM_UNKNOWN
            || item->is_read_only)
        {
            wrote_any = 1;
            break ;
        }
        index += 1;
    }
    if (!wrote_any)
    {
        if (out_emitted)
            *out_emitted = 0;
        return (FT_SUCCESS);
    }
    if (cblc_builder_append_string(builder, "/* copybook exports:\n") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < copybook->item_count)
    {
        const t_transpiler_copybook_item *item;

        item = &copybook->items[index];
        if (item->name[0] == '\0' && item->declared_length == 0
            && item->kind == TRANSPILE_DATA_ITEM_UNKNOWN && !item->is_read_only)
        {
            index += 1;
            continue ;
        }
        if (cblc_builder_append_string(builder, " * - ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (item->name[0] != '\0')
        {
            if (cblc_builder_append_string(builder, item->name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_builder_append_string(builder, "<unnamed>") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_string(builder, " (") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder,
                cobol_reverse_copybook_item_kind_label(item->kind)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (item->declared_length > 0)
        {
            if (cblc_builder_append_string(builder, ", len=") != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_builder_append_unsigned(builder,
                    static_cast<unsigned long long>(item->declared_length)) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (item->is_read_only)
        {
            if (cblc_builder_append_string(builder, ", read-only") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (cblc_builder_append_string(builder, ")\n") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    if (cblc_builder_append_string(builder, " */\n") != FT_SUCCESS)
        return (FT_FAILURE);
    if (out_emitted)
        *out_emitted = 1;
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_copybook_dependencies_comment(t_cblc_builder *builder,
    const t_transpiler_copybook *copybook, int *out_emitted)
{
    size_t index;
    int has_dependencies;

    if (!builder)
        return (FT_FAILURE);
    if (!copybook)
        return (FT_FAILURE);
    if (!copybook->dependencies || copybook->dependency_count == 0)
    {
        if (out_emitted)
            *out_emitted = 0;
        return (FT_SUCCESS);
    }
    has_dependencies = 0;
    index = 0;
    while (index < copybook->dependency_count)
    {
        if (copybook->dependencies[index][0] != '\0')
        {
            has_dependencies = 1;
            break ;
        }
        index += 1;
    }
    if (!has_dependencies)
    {
        if (out_emitted)
            *out_emitted = 0;
        return (FT_SUCCESS);
    }
    if (cblc_builder_append_string(builder, "/* copybook dependencies:\n") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < copybook->dependency_count)
    {
        if (copybook->dependencies[index][0] == '\0')
        {
            index += 1;
            continue ;
        }
        if (cblc_builder_append_string(builder, " * - ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, copybook->dependencies[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_append_string(builder, "\n") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    if (cblc_builder_append_string(builder, " */\n") != FT_SUCCESS)
        return (FT_FAILURE);
    if (out_emitted)
        *out_emitted = 1;
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_copybook_include(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *include)
{
    const t_ast_node *name_node;
    const t_transpiler_copybook *copybook;
    const char *display_name;
    const t_ast_node *replacing_node;
    int items_emitted;
    int dependencies_emitted;
    int replacements_emitted;

    if (!builder)
        return (FT_FAILURE);
    if (!include)
        return (FT_FAILURE);
    name_node = ast_node_get_child(include, 0);
    if (!name_node || name_node->kind != AST_NODE_IDENTIFIER || !name_node->token.lexeme)
    {
        if (context)
            cobol_reverse_emit_error(context, "COPY statement missing identifier");
        return (FT_FAILURE);
    }
    if (!context)
        return (FT_FAILURE);
    copybook = transpiler_context_find_copybook(context, name_node->token.lexeme);
    if (!copybook)
    {
        if (context)
            cobol_reverse_emit_error(context, "COPY statement refers to unknown copybook");
        return (FT_FAILURE);
    }
    display_name = NULL;
    if (copybook->canonical_name[0] != '\0')
        display_name = copybook->canonical_name;
    else if (copybook->name[0] != '\0')
        display_name = copybook->name;
    else if (name_node->token.lexeme)
        display_name = name_node->token.lexeme;
    if (!display_name)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "copy \"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, display_name) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "\"") != FT_SUCCESS)
        return (FT_FAILURE);
    replacing_node = cobol_reverse_copybook_find_replacing_clause(include);
    if (replacing_node)
    {
        if (cobol_reverse_emit_copybook_replacing_clause(builder, include, replacing_node) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_builder_append_string(builder, ";") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
    replacements_emitted = 0;
    if (cobol_reverse_emit_copybook_replacements_comment(builder, copybook, include,
            &replacements_emitted) != FT_SUCCESS)
        return (FT_FAILURE);
    items_emitted = 0;
    if (cobol_reverse_emit_copybook_items_comment(builder, copybook, &items_emitted) != FT_SUCCESS)
        return (FT_FAILURE);
    dependencies_emitted = 0;
    if (cobol_reverse_emit_copybook_dependencies_comment(builder, copybook, &dependencies_emitted) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!replacements_emitted && !items_emitted && !dependencies_emitted)
    {
        if (cblc_builder_append_string(builder, "/* copybook had no registered items */\n") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_working_storage_section(t_transpiler_context *context,
    t_cblc_builder *builder, const t_ast_node *section, int *out_emitted)
{
    size_t index;
    size_t count;
    int emitted;

    if (!builder)
        return (FT_FAILURE);
    if (!section)
        return (FT_FAILURE);
    emitted = 0;
    count = ast_node_child_count(section);
    index = 0;
    while (index < count)
    {
        const t_ast_node *child;

        child = ast_node_get_child(section, index);
        if (child && child->kind == AST_NODE_DATA_ITEM)
        {
            t_cobol_reverse_data_item_info info;
            long level_value;

            if (cobol_reverse_collect_data_item_info(child, &info) != FT_SUCCESS)
            {
                if (context)
                    cobol_reverse_emit_error(context, "WORKING-STORAGE item missing metadata");
                return (FT_FAILURE);
            }
            if (cobol_reverse_get_data_item_level(&info, &level_value) != FT_SUCCESS)
            {
                if (context)
                    cobol_reverse_emit_error(context, "WORKING-STORAGE item missing level");
                return (FT_FAILURE);
            }
            if (!info.picture_node)
            {
                if (level_value == 66 || level_value == 77 || level_value == 88)
                {
                    if (context)
                        cobol_reverse_emit_error(context,
                            "Unsupported WORKING-STORAGE level in reverse translation");
                    return (FT_FAILURE);
                }
                if (level_value < 1 || level_value > 49)
                {
                    if (context)
                        cobol_reverse_emit_error(context,
                            "Unsupported WORKING-STORAGE level in reverse translation");
                    return (FT_FAILURE);
                }
                if (cobol_reverse_emit_group_item(context, builder, section, index) != FT_SUCCESS)
                    return (FT_FAILURE);
                emitted = 1;
                index += 1;
                continue;
            }
            if (cobol_reverse_emit_data_item(context, builder, section, &index) != FT_SUCCESS)
                return (FT_FAILURE);
            emitted = 1;
        }
        else if (child && child->kind == AST_NODE_COPYBOOK_INCLUDE)
        {
            if (cobol_reverse_emit_copybook_include(context, builder, child) != FT_SUCCESS)
                return (FT_FAILURE);
            emitted = 1;
        }
        else if (child)
        {
            if (context)
                cobol_reverse_emit_error(context, "Unsupported entry in WORKING-STORAGE section");
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (out_emitted)
        *out_emitted = emitted;
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_data_division(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *division, int *out_emitted)
{
    size_t index;
    int emitted_any;

    if (!builder)
        return (FT_FAILURE);
    if (!division)
        return (FT_FAILURE);
    emitted_any = 0;
    index = 0;
    while (index < ast_node_child_count(division))
    {
        const t_ast_node *child;

        child = ast_node_get_child(division, index);
        if (child && child->kind == AST_NODE_WORKING_STORAGE_SECTION)
        {
            int section_emitted;

            section_emitted = 0;
            if (cobol_reverse_emit_working_storage_section(context, builder, child,
                    &section_emitted) != FT_SUCCESS)
                return (FT_FAILURE);
            if (section_emitted)
                emitted_any = 1;
        }
        index += 1;
    }
    if (out_emitted)
        *out_emitted = emitted_any;
    return (FT_SUCCESS);
}

static const t_ast_node *cobol_reverse_find_procedure_division(const t_ast_node *program)
{
    size_t index;

    if (!program)
        return (NULL);
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_PROCEDURE_DIVISION)
            return (candidate);
        index += 1;
    }
    return (NULL);
}

static const t_ast_node *cobol_reverse_find_data_division(const t_ast_node *program)
{
    size_t index;

    if (!program)
        return (NULL);
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_DATA_DIVISION)
            return (candidate);
        index += 1;
    }
    return (NULL);
}

int transpiler_cobol_program_to_cblc(t_transpiler_context *context, const t_ast_node *program,
    char **out_text)
{
    const t_ast_node *procedure_division;
    const t_ast_node *data_division;
    size_t index;
    t_cblc_builder builder;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    cblc_builder_init(&builder);
    if (!program)
    {
        cblc_builder_dispose(&builder);
        return (FT_FAILURE);
    }
    cobol_reverse_condition_table_reset();
    if (context)
        transpiler_context_reset_comment_iteration(context);
    data_division = cobol_reverse_find_data_division(program);
    if (data_division)
    {
        if (cobol_reverse_emit_data_division(context, &builder, data_division, NULL) != FT_SUCCESS)
        {
            cblc_builder_dispose(&builder);
            if (context && !transpiler_context_has_errors(context))
                cobol_reverse_emit_error(context, "Failed to recover WORKING-STORAGE declarations");
            return (FT_FAILURE);
        }
    }
    procedure_division = cobol_reverse_find_procedure_division(program);
    if (!procedure_division)
    {
        cblc_builder_dispose(&builder);
        if (context)
            cobol_reverse_emit_error(context, "Procedure division missing from COBOL program");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < ast_node_child_count(procedure_division))
    {
        const t_ast_node *paragraph;

        paragraph = ast_node_get_child(procedure_division, index);
        if (paragraph && paragraph->kind == AST_NODE_PARAGRAPH)
        {
            if (cblc_builder_append_paragraph_separator(&builder) != FT_SUCCESS)
            {
                cblc_builder_dispose(&builder);
                return (FT_FAILURE);
            }
            if (cobol_reverse_emit_paragraph(context, &builder, paragraph) != FT_SUCCESS)
            {
                cblc_builder_dispose(&builder);
                if (context)
                    cobol_reverse_emit_error(context, "Failed to recover COBOL paragraph");
                return (FT_FAILURE);
            }
        }
        index += 1;
    }
    if (context)
    {
        if (cobol_reverse_emit_remaining_comments(context, &builder) != FT_SUCCESS)
        {
            cblc_builder_dispose(&builder);
            return (FT_FAILURE);
        }
    }
    if (cblc_builder_ensure_trailing_newline(&builder) != FT_SUCCESS)
    {
        cblc_builder_dispose(&builder);
        return (FT_FAILURE);
    }
    *out_text = builder.data;
    builder.data = NULL;
    builder.length = 0;
    builder.capacity = 0;
    cblc_builder_dispose(&builder);
    return (FT_SUCCESS);
}
