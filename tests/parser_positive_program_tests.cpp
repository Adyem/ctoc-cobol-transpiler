#include "parser.hpp"

#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

FT_TEST(test_parser_parses_minimal_program)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *division;
    t_ast_node *program_id;
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *move_statement;
    t_ast_node *value_node;
    t_ast_node *target_node;
    size_t count;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    MOVE \"42\" TO RESULT\n"
        "    MOVE INPUT TO OUTPUT\n";
    parser_init(&parser, source);
    program = NULL;
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        return (FT_FAILURE);
    }
    parser_dispose(&parser);
    if (!program)
        return (FT_FAILURE);
    count = ast_node_child_count(program);
    if (count != 4)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    division = ast_node_get_child(program, 0);
    if (!division || division->kind != AST_NODE_IDENTIFICATION_DIVISION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(division) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    program_id = ast_node_get_child(division, 0);
    if (!program_id || program_id->kind != AST_NODE_PROGRAM_ID)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!program_id->token.lexeme)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ft_strncmp(program_id->token.lexeme, "SAMPLE", program_id->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    division = ast_node_get_child(program, 1);
    if (!division || division->kind != AST_NODE_ENVIRONMENT_DIVISION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    data_division = ast_node_get_child(program, 2);
    if (!data_division || data_division->kind != AST_NODE_DATA_DIVISION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(data_division) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    working_storage = ast_node_get_child(data_division, 0);
    if (!working_storage || working_storage->kind != AST_NODE_WORKING_STORAGE_SECTION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    procedure_division = ast_node_get_child(program, 3);
    if (!procedure_division || procedure_division->kind != AST_NODE_PROCEDURE_DIVISION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(procedure_division) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    paragraph = ast_node_get_child(procedure_division, 0);
    if (!paragraph || paragraph->kind != AST_NODE_PARAGRAPH)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!paragraph->token.lexeme)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ft_strncmp(paragraph->token.lexeme, "MAIN", paragraph->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(paragraph) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    sequence = ast_node_get_child(paragraph, 0);
    if (!sequence || sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(sequence) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    move_statement = ast_node_get_child(sequence, 0);
    if (!move_statement || move_statement->kind != AST_NODE_MOVE_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(move_statement) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    value_node = ast_node_get_child(move_statement, 0);
    if (!value_node || value_node->kind != AST_NODE_LITERAL)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!value_node->token.lexeme)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ft_strncmp(value_node->token.lexeme, "\"42\"", value_node->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    target_node = ast_node_get_child(move_statement, 1);
    if (!target_node || target_node->kind != AST_NODE_IDENTIFIER)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    move_statement = ast_node_get_child(sequence, 1);
    if (!move_statement || move_statement->kind != AST_NODE_MOVE_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(move_statement) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    value_node = ast_node_get_child(move_statement, 0);
    if (!value_node || value_node->kind != AST_NODE_IDENTIFIER)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    target_node = ast_node_get_child(move_statement, 1);
    if (!target_node || target_node->kind != AST_NODE_IDENTIFIER)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}
