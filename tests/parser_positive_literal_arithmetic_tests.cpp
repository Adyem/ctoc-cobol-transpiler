#include "parser.hpp"

#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

FT_TEST(test_parser_accepts_modulo_expression)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *assignment;
    t_ast_node *expression;
    t_ast_node *operator_node;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. MODULO-TEST.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 LEFT PIC 9(4).\n"
        "01 RIGHT PIC 9(4).\n"
        "01 REMAINDER PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    REMAINDER = LEFT MOD RIGHT;\n";
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
    if (ast_node_child_count(sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    assignment = ast_node_get_child(sequence, 0);
    if (!assignment || assignment->kind != AST_NODE_ASSIGNMENT_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(assignment) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    expression = ast_node_get_child(assignment, 0);
    if (!expression || expression->kind != AST_NODE_ARITHMETIC_EXPRESSION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(expression) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    operator_node = ast_node_get_child(expression, 1);
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (operator_node->token.kind != LEXER_TOKEN_KEYWORD_MOD)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!operator_node->token.lexeme
        || ft_strncmp(operator_node->token.lexeme, "MOD", operator_node->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}

FT_TEST(test_parser_accepts_multiplication_expression)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *assignment;
    t_ast_node *expression;
    t_ast_node *operator_node;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. MULTIPLY-TEST.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 LEFT PIC 9(4).\n"
        "01 RIGHT PIC 9(4).\n"
        "01 PRODUCT PIC 9(8).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    PRODUCT = LEFT * RIGHT;\n";
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
    if (ast_node_child_count(sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    assignment = ast_node_get_child(sequence, 0);
    if (!assignment || assignment->kind != AST_NODE_ASSIGNMENT_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(assignment) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    expression = ast_node_get_child(assignment, 0);
    if (!expression || expression->kind != AST_NODE_ARITHMETIC_EXPRESSION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(expression) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    operator_node = ast_node_get_child(expression, 1);
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (operator_node->token.kind != LEXER_TOKEN_STAR)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!operator_node->token.lexeme
        || ft_strncmp(operator_node->token.lexeme, "*", operator_node->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}

FT_TEST(test_parser_accepts_division_expression)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *assignment;
    t_ast_node *expression;
    t_ast_node *operator_node;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. DIVIDE-TEST.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 LEFT PIC 9(4).\n"
        "01 RIGHT PIC 9(4).\n"
        "01 QUOTIENT PIC 9(8).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    QUOTIENT = LEFT / RIGHT;\n";
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
    if (ast_node_child_count(sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    assignment = ast_node_get_child(sequence, 0);
    if (!assignment || assignment->kind != AST_NODE_ASSIGNMENT_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(assignment) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    expression = ast_node_get_child(assignment, 0);
    if (!expression || expression->kind != AST_NODE_ARITHMETIC_EXPRESSION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(expression) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    operator_node = ast_node_get_child(expression, 1);
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (operator_node->token.kind != LEXER_TOKEN_SLASH)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!operator_node->token.lexeme
        || ft_strncmp(operator_node->token.lexeme, "/", operator_node->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}

FT_TEST(test_parser_accepts_subtraction_expression)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *assignment;
    t_ast_node *expression;
    t_ast_node *operator_node;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SUBTRACTION-TEST.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 LEFT PIC 9(4).\n"
        "01 RIGHT PIC 9(4).\n"
        "01 DIFFERENCE PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    DIFFERENCE = LEFT - RIGHT.\n";
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
    if (ast_node_child_count(sequence) != 1)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    assignment = ast_node_get_child(sequence, 0);
    if (!assignment || assignment->kind != AST_NODE_ASSIGNMENT_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(assignment) != 2)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    expression = ast_node_get_child(assignment, 0);
    if (!expression || expression->kind != AST_NODE_ARITHMETIC_EXPRESSION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(expression) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    operator_node = ast_node_get_child(expression, 1);
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (operator_node->token.kind != LEXER_TOKEN_MINUS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!operator_node->token.lexeme
        || ft_strncmp(operator_node->token.lexeme, "-", operator_node->token.length) != 0)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}
