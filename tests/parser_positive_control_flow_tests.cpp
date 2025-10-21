#include "cblc_transpiler.hpp"

#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

FT_TEST(test_parser_parses_control_flow_statements)
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
        "    IF NOT FLAG == \"Y\"\n"
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

FT_TEST(test_parser_parses_read_next_with_lock_clause)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *read_next;
    t_ast_node *read_with_no_lock;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. FILE-TEST.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    READ ORDERS NEXT RECORD INTO CURRENT-REC WITH LOCK.\n"
        "    READ HISTORY WITH NO LOCK.\n"
        "    STOP RUN.\n";
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
    sequence = ast_node_get_child(paragraph, 0);
    if (!sequence || sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(sequence) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    read_next = ast_node_get_child(sequence, 0);
    if (!read_next || read_next->kind != AST_NODE_READ_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if ((read_next->flags & AST_READ_FLAG_NEXT) == 0
        || (read_next->flags & AST_READ_FLAG_WITH_LOCK) == 0
        || (read_next->flags & AST_READ_FLAG_WITH_NO_LOCK) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    read_with_no_lock = ast_node_get_child(sequence, 1);
    if (!read_with_no_lock || read_with_no_lock->kind != AST_NODE_READ_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if ((read_with_no_lock->flags & AST_READ_FLAG_WITH_NO_LOCK) == 0
        || (read_with_no_lock->flags & AST_READ_FLAG_NEXT) != 0
        || (read_with_no_lock->flags & AST_READ_FLAG_WITH_LOCK) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}
