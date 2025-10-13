#include "parser.hpp"

#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

FT_TEST(test_parser_accepts_boolean_character_literals)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *move_statement;
    t_ast_node *literal;
    t_ast_node *if_statement;
    t_ast_node *condition;
    t_ast_node *right_operand;
    t_ast_node *branch_sequence;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. BOOL-LITERALS.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 FLAG PIC X.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    MOVE 'Y' TO FLAG\n"
        "    IF FLAG == 'N'\n"
        "        MOVE 'Y' TO FLAG\n"
        "    END-IF\n";
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
    literal = ast_node_get_child(move_statement, 0);
    if (!literal || literal->token.kind != LEXER_TOKEN_STRING_LITERAL)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!literal->token.lexeme || literal->token.length != 3
        || literal->token.lexeme[1] != 'Y')
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if_statement = ast_node_get_child(sequence, 1);
    if (!if_statement || if_statement->kind != AST_NODE_IF_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(if_statement) != 2)
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
    if (ast_node_child_count(condition) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    right_operand = ast_node_get_child(condition, 2);
    if (!right_operand || right_operand->token.kind != LEXER_TOKEN_STRING_LITERAL)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!right_operand->token.lexeme || right_operand->token.length != 3
        || right_operand->token.lexeme[1] != 'N')
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
    if (ast_node_get_child(branch_sequence, 0)->kind != AST_NODE_MOVE_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}
