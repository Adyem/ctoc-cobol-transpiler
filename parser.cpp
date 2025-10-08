#include "parser.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

static int parser_parse_statement(t_parser *parser, t_ast_node *sequence);
static int parser_parse_statement_sequence_until(t_parser *parser, t_ast_node *parent,
    const t_lexer_token_kind *terminators, size_t terminator_count, int allow_paragraph_break);
static int parser_set_error(t_parser *parser);
static int parser_set_success(t_parser *parser);
static int parser_advance(t_parser *parser);
static t_ast_node *parser_create_node(t_parser *parser, t_ast_node_kind kind);

static int parser_picture_buffer_append(char **buffer, size_t *length, size_t *capacity,
    const char *text, size_t text_length)
{
    char *new_buffer;
    size_t new_capacity;
    size_t index;

    if (!buffer || !length || !capacity)
        return (FT_FAILURE);
    if (!text)
        return (FT_FAILURE);
    if (text_length == 0)
        return (FT_SUCCESS);
    if (*capacity <= *length + text_length)
    {
        new_capacity = *capacity;
        if (new_capacity == 0)
            new_capacity = 32;
        while (new_capacity <= *length + text_length)
        {
            if (new_capacity > (SIZE_MAX / 2))
                return (FT_FAILURE);
            new_capacity *= 2;
        }
        new_buffer = static_cast<char *>(cma_calloc(new_capacity + 1, sizeof(char)));
        if (!new_buffer)
            return (FT_FAILURE);
        index = 0;
        while (index < *length)
        {
            new_buffer[index] = (*buffer)[index];
            index += 1;
        }
        if (*buffer)
            cma_free(*buffer);
        *buffer = new_buffer;
        *capacity = new_capacity;
    }
    index = 0;
    while (index < text_length)
    {
        (*buffer)[*length + index] = text[index];
        index += 1;
    }
    *length += text_length;
    (*buffer)[*length] = '\0';
    return (FT_SUCCESS);
}

static int parser_parse_picture_clause(t_parser *parser, t_ast_node **out_node)
{
    t_ast_node *node;
    char *buffer;
    size_t length;
    size_t capacity;
    size_t start_line;
    size_t start_column;
    int has_token;

    if (!parser)
        return (FT_FAILURE);
    if (!out_node)
        return (FT_FAILURE);
    buffer = NULL;
    length = 0;
    capacity = 0;
    start_line = 0;
    start_column = 0;
    has_token = 0;
    while (parser->has_current)
    {
        if (parser->current.kind == LEXER_TOKEN_PERIOD
            || parser->current.kind == LEXER_TOKEN_KEYWORD_VALUE)
            break ;
        if (!parser->current.lexeme || parser->current.length == 0)
        {
            if (buffer)
                cma_free(buffer);
            return (parser_set_error(parser));
        }
        if (!has_token)
        {
            start_line = parser->current.line;
            start_column = parser->current.column;
        }
        if (parser_picture_buffer_append(&buffer, &length, &capacity,
                parser->current.lexeme, parser->current.length) != FT_SUCCESS)
        {
            if (buffer)
                cma_free(buffer);
            return (parser_set_error(parser));
        }
        has_token = 1;
        if (parser_advance(parser) != FT_SUCCESS)
        {
            if (buffer)
                cma_free(buffer);
            return (FT_FAILURE);
        }
    }
    if (!has_token)
    {
        if (buffer)
            cma_free(buffer);
        return (parser_set_error(parser));
    }
    node = parser_create_node(parser, AST_NODE_PICTURE_CLAUSE);
    if (!node)
    {
        if (buffer)
            cma_free(buffer);
        return (FT_FAILURE);
    }
    {
        t_lexer_token token;

        token.kind = LEXER_TOKEN_IDENTIFIER;
        token.lexeme = buffer;
        token.length = length;
        token.line = start_line;
        token.column = start_column;
        if (ast_node_set_token(node, &token) != FT_SUCCESS)
        {
            if (buffer)
                cma_free(buffer);
            ast_node_destroy(node);
            return (parser_set_error(parser));
        }
    }
    if (buffer)
        cma_free(buffer);
    *out_node = node;
    return (parser_set_success(parser));
}

static int parser_set_error(t_parser *parser)
{
    if (!parser)
        return (FT_FAILURE);
    parser->last_error = FT_FAILURE;
    return (FT_FAILURE);
}

static int parser_set_success(t_parser *parser)
{
    if (!parser)
        return (FT_FAILURE);
    parser->last_error = FT_SUCCESS;
    return (FT_SUCCESS);
}

static void parser_record_recoverable_error(t_parser *parser)
{
    if (!parser)
        return ;
    parser->error_count += 1;
    parser->last_error = FT_FAILURE;
}

static int parser_is_statement_start_kind(t_lexer_token_kind kind)
{
    if (kind == LEXER_TOKEN_KEYWORD_MOVE)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_IF)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_PERFORM)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_OPEN)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_CLOSE)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_READ)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_WRITE)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_DISPLAY)
        return (1);
    if (kind == LEXER_TOKEN_KEYWORD_STOP)
        return (1);
    return (0);
}

void parser_init(t_parser *parser, const char *text)
{
    if (!parser)
        return ;
    lexer_init(&parser->lexer, text);
    parser->has_current = 0;
    parser->last_error = FT_SUCCESS;
    parser->error_count = 0;
}

void parser_dispose(t_parser *parser)
{
    if (!parser)
        return ;
    parser->has_current = 0;
    parser->last_error = FT_SUCCESS;
    parser->error_count = 0;
}

static int parser_advance(t_parser *parser)
{
    if (!parser)
        return (FT_FAILURE);
    if (lexer_next_token(&parser->lexer, &parser->current) != FT_SUCCESS)
        return (parser_set_error(parser));
    parser->has_current = 1;
    return (parser_set_success(parser));
}

static int parser_expect(t_parser *parser, t_lexer_token_kind kind, t_lexer_token *out)
{
    if (!parser)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind != kind)
        return (parser_set_error(parser));
    if (out)
        *out = parser->current;
    if (parser_advance(parser) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static t_ast_node *parser_create_node(t_parser *parser, t_ast_node_kind kind)
{
    t_ast_node *node;

    node = ast_node_create(kind);
    if (!node)
    {
        parser_set_error(parser);
        return (NULL);
    }
    return (node);
}

static int parser_add_child(t_parser *parser, t_ast_node *parent, t_ast_node *child)
{
    if (ast_node_add_child(parent, child) != FT_SUCCESS)
    {
        ast_node_destroy(child);
        return (parser_set_error(parser));
    }
    return (parser_set_success(parser));
}

static int parser_token_equals(const t_lexer_token *token, const char *text)
{
    size_t index;
    size_t length;
    size_t expected_length;

    if (!token)
        return (0);
    if (!text)
        return (0);
    if (!token->lexeme)
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
        if (left >= 'a' && left <= 'z')
            left = static_cast<char>(left - ('a' - 'A'));
        right = text[index];
        if (right >= 'a' && right <= 'z')
            right = static_cast<char>(right - ('a' - 'A'));
        if (left != right)
            return (0);
        index += 1;
    }
    return (1);
}

static int parser_parse_identifier_node(t_parser *parser, t_ast_node **out_node)
{
    t_ast_node *node;
    t_lexer_token token;

    if (!out_node)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_IDENTIFIER);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_IDENTIFIER, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    *out_node = node;
    return (parser_set_success(parser));
}

static int parser_parse_literal_node(t_parser *parser, t_ast_node **out_node)
{
    t_ast_node *node;
    t_lexer_token token;

    if (!out_node)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_LITERAL);
    if (!node)
        return (FT_FAILURE);
    if (!parser->has_current)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    if (parser->current.kind != LEXER_TOKEN_STRING_LITERAL
        && parser->current.kind != LEXER_TOKEN_NUMERIC_LITERAL
        && parser->current.kind != LEXER_TOKEN_KEYWORD_TRUE
        && parser->current.kind != LEXER_TOKEN_KEYWORD_FALSE)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    token = parser->current;
    if (parser_advance(parser) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    *out_node = node;
    return (parser_set_success(parser));
}

static int parser_parse_value_node(t_parser *parser, t_ast_node **out_node)
{
    if (!parser)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind == LEXER_TOKEN_IDENTIFIER)
        return (parser_parse_identifier_node(parser, out_node));
    if (parser->current.kind == LEXER_TOKEN_STRING_LITERAL
        || parser->current.kind == LEXER_TOKEN_NUMERIC_LITERAL
        || parser->current.kind == LEXER_TOKEN_KEYWORD_TRUE
        || parser->current.kind == LEXER_TOKEN_KEYWORD_FALSE)
        return (parser_parse_literal_node(parser, out_node));
    return (parser_set_error(parser));
}

static int parser_parse_move_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *move_node;
    t_ast_node *source_node;
    t_ast_node *target_node;

    move_node = parser_create_node(parser, AST_NODE_MOVE_STATEMENT);
    if (!move_node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_MOVE, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser_parse_value_node(parser, &source_node) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, move_node, source_node) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_TO, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser_parse_identifier_node(parser, &target_node) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, move_node, target_node) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(move_node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, move_node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_is_sequence_terminator(t_parser *parser,
    const t_lexer_token_kind *terminators, size_t terminator_count,
    int allow_paragraph_break)
{
    size_t index;

    if (!parser)
        return (1);
    if (!parser->has_current)
        return (1);
    if (parser->current.kind == LEXER_TOKEN_END_OF_FILE)
        return (1);
    if (parser->current.kind == LEXER_TOKEN_PERIOD)
        return (1);
    if (parser->current.kind == LEXER_TOKEN_SEMICOLON)
        return (1);
    index = 0;
    while (index < terminator_count)
    {
        if (terminators && parser->current.kind == terminators[index])
            return (1);
        index += 1;
    }
    if (allow_paragraph_break && parser->current.kind == LEXER_TOKEN_IDENTIFIER)
        return (1);
    return (0);
}

static int parser_parse_comparison_operator(t_parser *parser, t_ast_node **out_node)
{
    t_ast_node *node;
    t_lexer_token token;

    if (!parser)
        return (FT_FAILURE);
    if (!out_node)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind != LEXER_TOKEN_EQUALS
        && parser->current.kind != LEXER_TOKEN_NOT_EQUALS
        && parser->current.kind != LEXER_TOKEN_LESS_THAN
        && parser->current.kind != LEXER_TOKEN_LESS_OR_EQUAL
        && parser->current.kind != LEXER_TOKEN_GREATER_THAN
        && parser->current.kind != LEXER_TOKEN_GREATER_OR_EQUAL)
        return (parser_set_error(parser));
    node = parser_create_node(parser, AST_NODE_COMPARISON_OPERATOR);
    if (!node)
        return (FT_FAILURE);
    token = parser->current;
    if (parser_advance(parser) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    *out_node = node;
    return (parser_set_success(parser));
}

static int parser_parse_condition(t_parser *parser, t_ast_node **out_node)
{
    t_ast_node *condition;
    t_ast_node *left;
    t_ast_node *operator_node;
    t_ast_node *right;
    t_lexer_token not_token;

    if (!out_node)
        return (FT_FAILURE);
    condition = parser_create_node(parser, AST_NODE_CONDITION);
    if (!condition)
        return (FT_FAILURE);
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_KEYWORD_NOT)
    {
        not_token = parser->current;
        if (parser_advance(parser) != FT_SUCCESS)
        {
            ast_node_destroy(condition);
            return (FT_FAILURE);
        }
        if (ast_node_set_token(condition, &not_token) != FT_SUCCESS)
        {
            ast_node_destroy(condition);
            return (parser_set_error(parser));
        }
    }
    if (parser_parse_value_node(parser, &left) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, condition, left) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        return (FT_FAILURE);
    }
    if (parser_parse_comparison_operator(parser, &operator_node) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, condition, operator_node) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        return (FT_FAILURE);
    }
    if (parser_parse_value_node(parser, &right) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, condition, right) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        return (FT_FAILURE);
    }
    *out_node = condition;
    return (parser_set_success(parser));
}

static int parser_synchronize_statement_sequence(t_parser *parser,
    const t_lexer_token_kind *terminators, size_t terminator_count,
    int allow_paragraph_break)
{
    size_t index;

    if (!parser)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind == LEXER_TOKEN_END_OF_FILE)
        return (FT_SUCCESS);
    while (parser->has_current)
    {
        if (parser->current.kind == LEXER_TOKEN_PERIOD
            || parser->current.kind == LEXER_TOKEN_SEMICOLON)
        {
            if (parser_advance(parser) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (parser->current.kind == LEXER_TOKEN_END_OF_FILE)
            return (FT_SUCCESS);
        index = 0;
        while (index < terminator_count)
        {
            if (terminators && parser->current.kind == terminators[index])
                return (FT_SUCCESS);
            index += 1;
        }
        if (allow_paragraph_break && parser->current.kind == LEXER_TOKEN_IDENTIFIER)
            return (FT_SUCCESS);
        if (parser_is_statement_start_kind(parser->current.kind))
            return (FT_SUCCESS);
        if (parser_advance(parser) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (parser_set_error(parser));
}

static int parser_parse_statement_sequence_until(t_parser *parser, t_ast_node *parent,
    const t_lexer_token_kind *terminators, size_t terminator_count, int allow_paragraph_break)
{
    t_ast_node *sequence;
    int status;

    if (!parser)
        return (FT_FAILURE);
    if (!parent)
        return (FT_FAILURE);
    sequence = parser_create_node(parser, AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    while (parser->has_current
        && !parser_is_sequence_terminator(parser, terminators, terminator_count, allow_paragraph_break))
    {
        if (parser_parse_statement(parser, sequence) != FT_SUCCESS)
        {
            parser_record_recoverable_error(parser);
            if (parser_synchronize_statement_sequence(parser, terminators, terminator_count,
                    allow_paragraph_break) != FT_SUCCESS)
            {
                ast_node_destroy(sequence);
                return (FT_FAILURE);
            }
            status = FT_FAILURE;
        }
    }
    if (parser_add_child(parser, parent, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        return (FT_FAILURE);
    }
    if (status != FT_SUCCESS)
        return (FT_SUCCESS);
    return (parser_set_success(parser));
}

static int parser_parse_if_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *if_node;
    t_ast_node *condition;
    t_lexer_token_kind terminators[2];
    size_t terminator_count;

    if (!sequence)
        return (FT_FAILURE);
    if_node = parser_create_node(parser, AST_NODE_IF_STATEMENT);
    if (!if_node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_IF, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(if_node);
        return (FT_FAILURE);
    }
    if (parser_parse_condition(parser, &condition) != FT_SUCCESS)
    {
        ast_node_destroy(if_node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, if_node, condition) != FT_SUCCESS)
    {
        ast_node_destroy(if_node);
        return (FT_FAILURE);
    }
    terminators[0] = LEXER_TOKEN_KEYWORD_ELSE;
    terminators[1] = LEXER_TOKEN_KEYWORD_END_IF;
    terminator_count = 2;
    if (parser_parse_statement_sequence_until(parser, if_node, terminators, terminator_count, 0) != FT_SUCCESS)
    {
        ast_node_destroy(if_node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_KEYWORD_ELSE)
    {
        if (parser_expect(parser, LEXER_TOKEN_KEYWORD_ELSE, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(if_node);
            return (FT_FAILURE);
        }
        terminators[0] = LEXER_TOKEN_KEYWORD_END_IF;
        terminator_count = 1;
        if (parser_parse_statement_sequence_until(parser, if_node, terminators, terminator_count, 0) != FT_SUCCESS)
        {
            ast_node_destroy(if_node);
            return (FT_FAILURE);
        }
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_END_IF, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(if_node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(if_node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, if_node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_perform_until_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *condition;
    t_lexer_token_kind terminator;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_PERFORM_UNTIL_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_UNTIL, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_condition(parser, &condition) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, condition) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    terminator = LEXER_TOKEN_KEYWORD_END_PERFORM;
    if (parser_parse_statement_sequence_until(parser, node, &terminator, 1, 0) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_END_PERFORM, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_perform_varying_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *counter;
    t_ast_node *initial;
    t_ast_node *step;
    t_ast_node *condition;
    t_lexer_token_kind terminator;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_PERFORM_VARYING_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_VARYING, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_identifier_node(parser, &counter) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, counter) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_FROM, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_value_node(parser, &initial) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, initial) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_BY, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_value_node(parser, &step) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, step) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_UNTIL, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_condition(parser, &condition) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, condition) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    terminator = LEXER_TOKEN_KEYWORD_END_PERFORM;
    if (parser_parse_statement_sequence_until(parser, node, &terminator, 1, 0) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_END_PERFORM, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_perform_statement(t_parser *parser, t_ast_node *sequence)
{
    if (!parser)
        return (FT_FAILURE);
    if (!sequence)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_PERFORM, NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_UNTIL)
        return (parser_parse_perform_until_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_VARYING)
        return (parser_parse_perform_varying_statement(parser, sequence));
    return (parser_set_error(parser));
}

static int parser_parse_stop_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_lexer_token token;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_STOP_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_STOP, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_RUN, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_open_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *file_node;
    t_lexer_token mode_token;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_OPEN_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_OPEN, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_IDENTIFIER, &mode_token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, &mode_token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (parser_set_error(parser));
    }
    if (parser_parse_identifier_node(parser, &file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_close_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *file_node;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_CLOSE_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_CLOSE, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_identifier_node(parser, &file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_read_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *file_node;
    t_ast_node *into_node;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_READ_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_READ, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_identifier_node(parser, &file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_IDENTIFIER
        && parser_token_equals(&parser->current, "INTO"))
    {
        if (parser_advance(parser) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
        if (parser_parse_identifier_node(parser, &into_node) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
        if (parser_add_child(parser, node, into_node) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_write_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *file_node;
    t_ast_node *from_node;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_WRITE_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_WRITE, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_identifier_node(parser, &file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, file_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && (parser->current.kind == LEXER_TOKEN_IDENTIFIER
            || parser->current.kind == LEXER_TOKEN_KEYWORD_FROM)
        && parser_token_equals(&parser->current, "FROM"))
    {
        if (parser_advance(parser) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
        if (parser_parse_identifier_node(parser, &from_node) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
        if (parser_add_child(parser, node, from_node) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_display_statement(t_parser *parser, t_ast_node *sequence)
{
    t_ast_node *node;
    t_ast_node *value_node;

    if (!sequence)
        return (FT_FAILURE);
    node = parser_create_node(parser, AST_NODE_DISPLAY_STATEMENT);
    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_DISPLAY, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_parse_value_node(parser, &value_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, node, value_node) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
    {
        if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, sequence, node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_statement(t_parser *parser, t_ast_node *sequence)
{
    if (!parser)
        return (FT_FAILURE);
    if (!sequence)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_MOVE)
        return (parser_parse_move_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_IF)
        return (parser_parse_if_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_PERFORM)
        return (parser_parse_perform_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_OPEN)
        return (parser_parse_open_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_CLOSE)
        return (parser_parse_close_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_READ)
        return (parser_parse_read_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_WRITE)
        return (parser_parse_write_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_DISPLAY)
        return (parser_parse_display_statement(parser, sequence));
    if (parser->current.kind == LEXER_TOKEN_KEYWORD_STOP)
        return (parser_parse_stop_statement(parser, sequence));
    return (parser_set_error(parser));
}

static int parser_parse_paragraph(t_parser *parser, t_ast_node *procedure_division)
{
    t_ast_node *paragraph;
    t_lexer_token name_token;

    if (!procedure_division)
        return (FT_FAILURE);
    paragraph = parser_create_node(parser, AST_NODE_PARAGRAPH);
    if (!paragraph)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_IDENTIFIER, &name_token) != FT_SUCCESS)
    {
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(paragraph, &name_token) != FT_SUCCESS)
    {
        ast_node_destroy(paragraph);
        return (parser_set_error(parser));
    }
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    if (parser_parse_statement_sequence_until(parser, paragraph, NULL, 0, 1) != FT_SUCCESS)
    {
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, procedure_division, paragraph) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_expect_keyword_division_header(t_parser *parser, t_ast_node *node,
    t_lexer_token_kind division_kind)
{
    t_lexer_token token;

    if (!node)
        return (FT_FAILURE);
    if (parser_expect(parser, division_kind, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
        return (parser_set_error(parser));
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_DIVISION, NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_identification_division(t_parser *parser, t_ast_node *program)
{
    t_ast_node *division;
    t_ast_node *program_id;
    t_lexer_token name_token;

    division = parser_create_node(parser, AST_NODE_IDENTIFICATION_DIVISION);
    if (!division)
        return (FT_FAILURE);
    if (parser_expect_keyword_division_header(parser, division, LEXER_TOKEN_KEYWORD_IDENTIFICATION) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    program_id = parser_create_node(parser, AST_NODE_PROGRAM_ID);
    if (!program_id)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_PROGRAM_ID, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(program_id);
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(program_id);
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_IDENTIFIER, &name_token) != FT_SUCCESS)
    {
        ast_node_destroy(program_id);
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(program_id, &name_token) != FT_SUCCESS)
    {
        ast_node_destroy(program_id);
        ast_node_destroy(division);
        return (parser_set_error(parser));
    }
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(program_id);
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, division, program_id) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, program, division) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_skip_environment_division_body(t_parser *parser)
{
    if (!parser)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    while (parser->has_current
        && parser->current.kind != LEXER_TOKEN_KEYWORD_DATA)
    {
        if (parser->current.kind == LEXER_TOKEN_END_OF_FILE)
            return (parser_set_error(parser));
        if (parser_advance(parser) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!parser->has_current)
        return (parser_set_error(parser));
    return (parser_set_success(parser));
}

static int parser_parse_environment_division(t_parser *parser, t_ast_node *program)
{
    t_ast_node *division;

    division = parser_create_node(parser, AST_NODE_ENVIRONMENT_DIVISION);
    if (!division)
        return (FT_FAILURE);
    if (parser_expect_keyword_division_header(parser, division, LEXER_TOKEN_KEYWORD_ENVIRONMENT) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_skip_environment_division_body(parser) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, program, division) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_skip_until_period(t_parser *parser)
{
    if (!parser)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    while (parser->has_current && parser->current.kind != LEXER_TOKEN_PERIOD)
    {
        if (parser_advance(parser) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!parser->has_current)
        return (parser_set_error(parser));
    return (parser_set_success(parser));
}

static int parser_parse_data_item(t_parser *parser, t_ast_node *section)
{
    t_ast_node *item;
    t_ast_node *level_node;
    t_ast_node *name_node;
    t_ast_node *picture_node;
    t_lexer_token level_token;
    t_lexer_token name_token;

    if (!parser)
        return (FT_FAILURE);
    if (!section)
        return (FT_FAILURE);
    item = parser_create_node(parser, AST_NODE_DATA_ITEM);
    if (!item)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_NUMERIC_LITERAL, &level_token) != FT_SUCCESS)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    level_node = parser_create_node(parser, AST_NODE_LITERAL);
    if (!level_node)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(level_node, &level_token) != FT_SUCCESS)
    {
        ast_node_destroy(level_node);
        ast_node_destroy(item);
        return (parser_set_error(parser));
    }
    if (parser_add_child(parser, item, level_node) != FT_SUCCESS)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_IDENTIFIER, &name_token) != FT_SUCCESS)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    name_node = parser_create_node(parser, AST_NODE_IDENTIFIER);
    if (!name_node)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(name_node, &name_token) != FT_SUCCESS)
    {
        ast_node_destroy(name_node);
        ast_node_destroy(item);
        return (parser_set_error(parser));
    }
    if (parser_add_child(parser, item, name_node) != FT_SUCCESS)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    picture_node = NULL;
    if (parser->has_current && parser->current.kind == LEXER_TOKEN_KEYWORD_PIC)
    {
        if (parser_advance(parser) != FT_SUCCESS)
        {
            ast_node_destroy(item);
            return (FT_FAILURE);
        }
        if (parser_parse_picture_clause(parser, &picture_node) != FT_SUCCESS)
        {
            ast_node_destroy(item);
            return (FT_FAILURE);
        }
        if (parser_add_child(parser, item, picture_node) != FT_SUCCESS)
        {
            ast_node_destroy(item);
            return (FT_FAILURE);
        }
    }
    if (!parser->has_current)
    {
        ast_node_destroy(item);
        return (parser_set_error(parser));
    }
    if (parser->current.kind != LEXER_TOKEN_PERIOD)
    {
        if (parser_skip_until_period(parser) != FT_SUCCESS)
        {
            ast_node_destroy(item);
            return (FT_FAILURE);
        }
    }
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, section, item) != FT_SUCCESS)
    {
        ast_node_destroy(item);
        return (FT_FAILURE);
    }
    return (parser_set_success(parser));
}

static int parser_skip_file_section(t_parser *parser)
{
    if (!parser)
        return (FT_FAILURE);
    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind != LEXER_TOKEN_KEYWORD_FILE)
        return (parser_set_success(parser));
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_FILE, NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_SECTION, NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    while (parser->has_current
        && parser->current.kind != LEXER_TOKEN_KEYWORD_WORKING_STORAGE
        && parser->current.kind != LEXER_TOKEN_KEYWORD_PROCEDURE)
    {
        if (parser->current.kind == LEXER_TOKEN_END_OF_FILE)
            return (parser_set_error(parser));
        if (parser_advance(parser) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!parser->has_current)
        return (parser_set_error(parser));
    return (parser_set_success(parser));
}

static int parser_parse_working_storage_section(t_parser *parser, t_ast_node *data_division)
{
    t_ast_node *section;
    t_lexer_token token;

    if (!parser->has_current)
        return (parser_set_error(parser));
    if (parser->current.kind != LEXER_TOKEN_KEYWORD_WORKING_STORAGE)
        return (parser_set_success(parser));
    section = parser_create_node(parser, AST_NODE_WORKING_STORAGE_SECTION);
    if (!section)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_WORKING_STORAGE, &token) != FT_SUCCESS)
    {
        ast_node_destroy(section);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(section, &token) != FT_SUCCESS)
    {
        ast_node_destroy(section);
        return (parser_set_error(parser));
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_SECTION, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(section);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(section);
        return (FT_FAILURE);
    }
    while (parser->has_current && parser->current.kind == LEXER_TOKEN_NUMERIC_LITERAL)
    {
        if (parser_parse_data_item(parser, section) != FT_SUCCESS)
        {
            ast_node_destroy(section);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, data_division, section) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_data_division(t_parser *parser, t_ast_node *program)
{
    t_ast_node *division;

    division = parser_create_node(parser, AST_NODE_DATA_DIVISION);
    if (!division)
        return (FT_FAILURE);
    if (parser_expect_keyword_division_header(parser, division, LEXER_TOKEN_KEYWORD_DATA) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_skip_file_section(parser) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_parse_working_storage_section(parser, division) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, program, division) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_procedure_division(t_parser *parser, t_ast_node *program)
{
    t_ast_node *division;
    t_lexer_token token;

    division = parser_create_node(parser, AST_NODE_PROCEDURE_DIVISION);
    if (!division)
        return (FT_FAILURE);
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_PROCEDURE, &token) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(division, &token) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (parser_set_error(parser));
    }
    if (parser_expect(parser, LEXER_TOKEN_KEYWORD_DIVISION, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
    }
    while (parser->has_current && parser->current.kind != LEXER_TOKEN_END_OF_FILE)
    {
        if (parser->current.kind != LEXER_TOKEN_IDENTIFIER)
        {
            ast_node_destroy(division);
            return (parser_set_error(parser));
        }
        if (parser_parse_paragraph(parser, division) != FT_SUCCESS)
        {
            ast_node_destroy(division);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, program, division) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

int parser_parse_program(t_parser *parser, t_ast_node **out_program)
{
    t_ast_node *program;

    if (!parser)
        return (FT_FAILURE);
    if (!out_program)
        return (FT_FAILURE);
    program = parser_create_node(parser, AST_NODE_PROGRAM);
    if (!program)
        return (FT_FAILURE);
    if (parser_advance(parser) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (parser_parse_identification_division(parser, program) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (parser_parse_environment_division(parser, program) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (parser_parse_data_division(parser, program) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (parser_parse_procedure_division(parser, program) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!parser->has_current)
    {
        ast_node_destroy(program);
        return (parser_set_error(parser));
    }
    if (parser->current.kind == LEXER_TOKEN_IDENTIFIER
        && parser_token_equals(&parser->current, "END"))
    {
        if (parser_advance(parser) != FT_SUCCESS)
        {
            ast_node_destroy(program);
            return (FT_FAILURE);
        }
        while (parser->has_current && parser->current.kind != LEXER_TOKEN_PERIOD
            && parser->current.kind != LEXER_TOKEN_END_OF_FILE)
        {
            if (parser->current.kind != LEXER_TOKEN_IDENTIFIER)
                break ;
            if (parser_advance(parser) != FT_SUCCESS)
            {
                ast_node_destroy(program);
                return (FT_FAILURE);
            }
        }
        if (parser->has_current && parser->current.kind == LEXER_TOKEN_PERIOD)
        {
            if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
            {
                ast_node_destroy(program);
                return (FT_FAILURE);
            }
        }
        if (!parser->has_current)
        {
            ast_node_destroy(program);
            return (parser_set_error(parser));
        }
    }
    while (parser->has_current && parser->current.kind != LEXER_TOKEN_END_OF_FILE)
    {
        if (parser_advance(parser) != FT_SUCCESS)
        {
            ast_node_destroy(program);
            return (FT_FAILURE);
        }
    }
    if (!parser->has_current)
    {
        ast_node_destroy(program);
        return (parser_set_error(parser));
    }
    if (parser->current.kind != LEXER_TOKEN_END_OF_FILE)
    {
        ast_node_destroy(program);
        return (parser_set_error(parser));
    }
    if (parser->error_count > 0)
    {
        ast_node_destroy(program);
        parser_set_error(parser);
        return (FT_FAILURE);
    }
    *out_program = program;
    return (parser_set_success(parser));
}
