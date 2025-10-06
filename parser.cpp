#include "parser.hpp"

#include "libft/Libft/libft.hpp"

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

void parser_init(t_parser *parser, const char *text)
{
    if (!parser)
        return ;
    lexer_init(&parser->lexer, text);
    parser->has_current = 0;
    parser->last_error = FT_SUCCESS;
}

void parser_dispose(t_parser *parser)
{
    if (!parser)
        return ;
    parser->has_current = 0;
    parser->last_error = FT_SUCCESS;
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
    if (parser->current.kind != LEXER_TOKEN_STRING_LITERAL && parser->current.kind != LEXER_TOKEN_NUMERIC_LITERAL)
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
    if (parser->current.kind == LEXER_TOKEN_STRING_LITERAL || parser->current.kind == LEXER_TOKEN_NUMERIC_LITERAL)
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
    if (parser_expect(parser, LEXER_TOKEN_PERIOD, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(move_node);
        return (FT_FAILURE);
    }
    if (parser_add_child(parser, sequence, move_node) != FT_SUCCESS)
        return (FT_FAILURE);
    return (parser_set_success(parser));
}

static int parser_parse_statement_sequence(t_parser *parser, t_ast_node *procedure_division)
{
    t_ast_node *sequence;

    sequence = parser_create_node(parser, AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
        return (FT_FAILURE);
    while (parser->has_current && parser->current.kind != LEXER_TOKEN_END_OF_FILE)
    {
        if (parser->current.kind != LEXER_TOKEN_KEYWORD_MOVE)
        {
            ast_node_destroy(sequence);
            return (parser_set_error(parser));
        }
        if (parser_parse_move_statement(parser, sequence) != FT_SUCCESS)
        {
            ast_node_destroy(sequence);
            return (FT_FAILURE);
        }
    }
    if (parser_add_child(parser, procedure_division, sequence) != FT_SUCCESS)
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
    if (parser_add_child(parser, program, division) != FT_SUCCESS)
        return (FT_FAILURE);
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
    if (parser_parse_statement_sequence(parser, division) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        return (FT_FAILURE);
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
    if (parser->current.kind != LEXER_TOKEN_END_OF_FILE)
    {
        ast_node_destroy(program);
        return (parser_set_error(parser));
    }
    *out_program = program;
    return (parser_set_success(parser));
}
