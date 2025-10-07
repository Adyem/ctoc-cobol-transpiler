#include "parser.hpp"

#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

static int test_parser_parses_minimal_program(void)
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

static int test_parser_parses_control_flow_statements(void)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *if_statement;
    t_ast_node *condition;
    t_ast_node *branch_sequence;
    t_ast_node *perform_node;
    t_ast_node *stop_statement;
    t_ast_node *next_paragraph;
    size_t count;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. CONTROL.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    IF NOT FLAG = \"Y\"\n"
        "        PERFORM UNTIL COUNT > LIMIT\n"
        "            MOVE COUNT TO LIMIT\n"
        "        END-PERFORM\n"
        "    ELSE\n"
        "        PERFORM VARYING INDEX FROM 0 BY 1 UNTIL INDEX >= LIMIT\n"
        "            MOVE INDEX TO RESULT\n"
        "        END-PERFORM\n"
        "    END-IF\n"
        "    STOP RUN.\n"
        "NEXT.\n"
        "    MOVE \"DONE\" TO RESULT\n";
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
    procedure_division = ast_node_get_child(program, 3);
    if (!procedure_division || procedure_division->kind != AST_NODE_PROCEDURE_DIVISION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(procedure_division) != 2)
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
    if (ft_strncmp(paragraph->token.lexeme, "MAIN", paragraph->token.length) != 0)
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
    if_statement = ast_node_get_child(sequence, 0);
    if (!if_statement || if_statement->kind != AST_NODE_IF_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(if_statement) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    condition = ast_node_get_child(if_statement, 0);
    if (!condition || condition->kind != AST_NODE_CONDITION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (condition->token.kind != LEXER_TOKEN_KEYWORD_NOT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(condition) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_get_child(condition, 1)->token.kind != LEXER_TOKEN_EQUALS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    branch_sequence = ast_node_get_child(if_statement, 1);
    if (!branch_sequence || branch_sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(branch_sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    perform_node = ast_node_get_child(branch_sequence, 0);
    if (!perform_node || perform_node->kind != AST_NODE_PERFORM_UNTIL_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(perform_node) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    condition = ast_node_get_child(perform_node, 0);
    if (!condition || condition->kind != AST_NODE_CONDITION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    branch_sequence = ast_node_get_child(perform_node, 1);
    if (!branch_sequence || branch_sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(branch_sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_get_child(branch_sequence, 0)->kind != AST_NODE_MOVE_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    branch_sequence = ast_node_get_child(if_statement, 2);
    if (!branch_sequence || branch_sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(branch_sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    perform_node = ast_node_get_child(branch_sequence, 0);
    if (!perform_node || perform_node->kind != AST_NODE_PERFORM_VARYING_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(perform_node) != 5)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    condition = ast_node_get_child(perform_node, 3);
    if (!condition || condition->kind != AST_NODE_CONDITION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_get_child(condition, 1)->token.kind != LEXER_TOKEN_GREATER_OR_EQUAL)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    branch_sequence = ast_node_get_child(perform_node, 4);
    if (!branch_sequence || branch_sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(branch_sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_get_child(branch_sequence, 0)->kind != AST_NODE_MOVE_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    stop_statement = ast_node_get_child(sequence, 1);
    if (!stop_statement || stop_statement->kind != AST_NODE_STOP_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (stop_statement->token.kind != LEXER_TOKEN_KEYWORD_STOP)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    next_paragraph = ast_node_get_child(procedure_division, 1);
    if (!next_paragraph || next_paragraph->kind != AST_NODE_PARAGRAPH)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ft_strncmp(next_paragraph->token.lexeme, "NEXT", next_paragraph->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    sequence = ast_node_get_child(next_paragraph, 0);
    if (!sequence || sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    count = ast_node_child_count(sequence);
    if (count != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_get_child(sequence, 0)->kind != AST_NODE_MOVE_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}

static int test_parser_rejects_unknown_statement(void)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "PROCEDURE DIVISION.\n"
        "    OPEN FILE.\n";
    parser_init(&parser, source);
    program = NULL;
    status = parser_parse_program(&parser, &program);
    parser_dispose(&parser);
    if (program)
        ast_node_destroy(program);
    if (status == FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_parser_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"parser_parses_minimal_program", test_parser_parses_minimal_program},
        {"parser_parses_control_flow_statements", test_parser_parses_control_flow_statements},
        {"parser_rejects_unknown_statement", test_parser_rejects_unknown_statement}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
