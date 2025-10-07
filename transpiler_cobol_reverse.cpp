#include "transpiler_cobol_reverse.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "transpiler_logging.hpp"

typedef struct s_cblc_builder
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cblc_builder;

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

static int cobol_reverse_append_string_literal(t_cblc_builder *builder, const t_lexer_token *token)
{
    size_t index;
    const char *boolean_text;

    if (!builder)
        return (FT_FAILURE);
    if (!token)
        return (FT_FAILURE);
    boolean_text = cobol_reverse_boolean_from_string_literal(token);
    if (boolean_text)
        return (cblc_builder_append_string(builder, boolean_text));
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
            return (cobol_reverse_append_string_literal(builder, &node->token));
        if (node->token.kind == LEXER_TOKEN_NUMERIC_LITERAL)
            return (cobol_reverse_append_numeric_literal(builder, &node->token));
        if (node->token.kind == LEXER_TOKEN_KEYWORD_TRUE)
            return (cblc_builder_append_string(builder, "true"));
        if (node->token.kind == LEXER_TOKEN_KEYWORD_FALSE)
            return (cblc_builder_append_string(builder, "false"));
    }
    return (FT_FAILURE);
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
    if (negate)
    {
        if (cblc_builder_append_char(builder, ')') != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_statement_sequence(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *sequence, size_t indentation);

static int cobol_reverse_emit_move(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *source;
    const t_ast_node *target;

    (void)context;
    if (!builder)
        return (FT_FAILURE);
    if (!statement)
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

static int cobol_reverse_emit_perform_varying(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *statement, size_t indentation)
{
    const t_ast_node *counter;
    const t_ast_node *initial;
    const t_ast_node *step;
    const t_ast_node *condition;
    const t_ast_node *body;

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
    if (cblc_builder_append_string(builder, " = ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, counter) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, " + ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_value(builder, step) != FT_SUCCESS)
        return (FT_FAILURE);
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

    (void)context;
    if (!statement)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 1)
        return (FT_FAILURE);
    file_node = ast_node_get_child(statement, 0);
    target_node = NULL;
    if (ast_node_child_count(statement) > 1)
        target_node = ast_node_get_child(statement, 1);
    if (cblc_builder_append_indentation(builder, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "read(") != FT_SUCCESS)
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
    if (statement->kind == AST_NODE_MOVE_STATEMENT)
        return (cobol_reverse_emit_move(context, builder, statement, indentation));
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

        statement = ast_node_get_child(sequence, index);
        if (cobol_reverse_emit_statement(context, builder, statement, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cobol_reverse_emit_paragraph(t_transpiler_context *context, t_cblc_builder *builder,
    const t_ast_node *paragraph)
{
    const t_ast_node *body;

    if (!paragraph)
        return (FT_FAILURE);
    if (paragraph->kind != AST_NODE_PARAGRAPH)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "function ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_append_identifier(builder, &paragraph->token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_string(builder, "() {") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_builder_append_newline(builder) != FT_SUCCESS)
        return (FT_FAILURE);
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

int transpiler_cobol_program_to_cblc(t_transpiler_context *context, const t_ast_node *program,
    char **out_text)
{
    const t_ast_node *procedure_division;
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
        if (paragraph)
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
