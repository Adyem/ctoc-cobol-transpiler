#include "semantics_test_support.hpp"

#include "semantics_test_groups.hpp"
#include "test_support.hpp"

FT_TEST(test_semantics_accepts_numeric_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SUM-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("LEFT-TERM", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "SUM-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_move_multiplication)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("PRODUCT-TARGET", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-TERM",
        LEXER_TOKEN_STAR, "*", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "PRODUCT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_numeric_move_modulo)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("REMAINDER-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "DIVIDEND", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "DIVISOR", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("DIVIDEND",
        LEXER_TOKEN_KEYWORD_MOD, "MOD", "DIVISOR");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression,
            "REMAINDER-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_numeric_move_subtraction)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("DIFFERENCE-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-TERM",
        LEXER_TOKEN_MINUS, "-", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression,
            "DIFFERENCE-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_mixed_numeric_floating_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SUM-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("NUMERIC-TERM", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "SUM-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_reports_mixed_numeric_floating_addition_message)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SUM-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("NUMERIC-TERM", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "SUM-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
            {
                if (test_expect_cstring_equal(context.diagnostics.items[0].message,
                        "arithmetic operator '+' requires operands of the same type but MOVE source left operand is numeric and MOVE source right operand is floating",
                        "mixed numeric and floating addition should describe operand kinds") == FT_SUCCESS)
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_mixed_numeric_floating_multiplication)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("PRODUCT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("NUMERIC-TERM",
        LEXER_TOKEN_STAR, "*", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "PRODUCT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_reports_mixed_numeric_floating_division_message)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("QUOTIENT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("FLOAT-TERM",
        LEXER_TOKEN_SLASH, "/", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "QUOTIENT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
            {
                if (test_expect_cstring_equal(context.diagnostics.items[0].message,
                        "arithmetic operator '/' requires operands of the same type but MOVE source left operand is floating and MOVE source right operand is numeric",
                        "mixed floating and numeric division should describe operand kinds") == FT_SUCCESS)
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_reports_modulo_requires_integral_message)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("REMAINDER-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "DIVIDEND", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-DIVISOR", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("DIVIDEND",
        LEXER_TOKEN_KEYWORD_MOD, "MOD", "FLOAT-DIVISOR");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "REMAINDER-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
            {
                if (test_expect_cstring_equal(context.diagnostics.items[0].message,
                        "arithmetic operator 'MOD' requires integral operands but MOVE source right operand is floating",
                        "modulo with floating divisor should describe integral requirement") == FT_SUCCESS)
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_numeric_assignment_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SUM-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("LEFT-TERM", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment_node(program, expression, "SUM-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_assignment_multiplication)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("PRODUCT-TARGET", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-TERM",
        LEXER_TOKEN_STAR, "*", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment_node(program, expression,
            "PRODUCT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_move_division)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("QUOTIENT-TARGET", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-TERM",
        LEXER_TOKEN_SLASH, "/", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression,
            "QUOTIENT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_assignment_division)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("QUOTIENT-TARGET", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-TERM",
        LEXER_TOKEN_SLASH, "/", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment_node(program, expression,
            "QUOTIENT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_floating_move_modulo)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-DIVIDEND", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-DIVISOR", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator(
        "FLOAT-DIVIDEND", LEXER_TOKEN_KEYWORD_MOD, "MOD", "NUMERIC-DIVISOR");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression,
            "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_numeric_assignment_subtraction)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("DIFFERENCE-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-TERM",
        LEXER_TOKEN_MINUS, "-", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment_node(program, expression,
            "DIFFERENCE-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("LEFT-FLOAT", "RIGHT-FLOAT");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_move_subtraction)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-DIFFERENCE", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("LEFT-FLOAT",
        LEXER_TOKEN_MINUS, "-", "RIGHT-FLOAT");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression,
            "FLOAT-DIFFERENCE") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_mixed_floating_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-SUM", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("FLOAT-TERM", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-SUM") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_mixed_floating_move_subtraction)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-DIFFERENCE", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("FLOAT-TERM",
        LEXER_TOKEN_MINUS, "-", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression,
            "FLOAT-DIFFERENCE") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-TARGET", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("ALPHA-TERM", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "NUMERIC-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_move_subtraction)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-TARGET", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("ALPHA-TERM",
        LEXER_TOKEN_MINUS, "-", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "NUMERIC-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_move_multiplication)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("ALPHA-TERM",
        LEXER_TOKEN_STAR, "*", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_move_division)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("ALPHA-TERM",
        LEXER_TOKEN_SLASH, "/", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_floating_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("ALPHA-TERM", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_floating_subtraction)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("ALPHA-TERM",
        LEXER_TOKEN_MINUS, "-", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_floating_multiplication)
{
    t_transpiler_context context;
   t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("ALPHA-TERM",
        LEXER_TOKEN_STAR, "*", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_floating_division)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node_with_operator("ALPHA-TERM",
        LEXER_TOKEN_SLASH, "/", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

const t_test_case *get_semantics_arithmetic_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_accepts_numeric_move_addition", test_semantics_accepts_numeric_move_addition},
        {"semantics_accepts_floating_move_multiplication", test_semantics_accepts_floating_move_multiplication},
        {"semantics_accepts_numeric_move_modulo", test_semantics_accepts_numeric_move_modulo},
        {"semantics_accepts_numeric_move_subtraction", test_semantics_accepts_numeric_move_subtraction},
        {"semantics_rejects_mixed_numeric_floating_addition", test_semantics_rejects_mixed_numeric_floating_addition},
        {"semantics_rejects_mixed_numeric_floating_multiplication", test_semantics_rejects_mixed_numeric_floating_multiplication},
        {"semantics_reports_mixed_numeric_floating_addition_message", test_semantics_reports_mixed_numeric_floating_addition_message},
        {"semantics_accepts_numeric_assignment_addition", test_semantics_accepts_numeric_assignment_addition},
        {"semantics_accepts_floating_assignment_multiplication", test_semantics_accepts_floating_assignment_multiplication},
        {"semantics_accepts_floating_move_division", test_semantics_accepts_floating_move_division},
        {"semantics_accepts_floating_assignment_division", test_semantics_accepts_floating_assignment_division},
        {"semantics_rejects_floating_move_modulo", test_semantics_rejects_floating_move_modulo},
        {"semantics_reports_modulo_requires_integral_message", test_semantics_reports_modulo_requires_integral_message},
        {"semantics_accepts_numeric_assignment_subtraction", test_semantics_accepts_numeric_assignment_subtraction},
        {"semantics_accepts_floating_move_addition", test_semantics_accepts_floating_move_addition},
        {"semantics_accepts_floating_move_subtraction", test_semantics_accepts_floating_move_subtraction},
        {"semantics_rejects_mixed_floating_move_addition", test_semantics_rejects_mixed_floating_move_addition},
        {"semantics_rejects_mixed_floating_move_subtraction", test_semantics_rejects_mixed_floating_move_subtraction},
        {"semantics_rejects_alphanumeric_move_addition", test_semantics_rejects_alphanumeric_move_addition},
        {"semantics_rejects_alphanumeric_move_subtraction", test_semantics_rejects_alphanumeric_move_subtraction},
        {"semantics_rejects_alphanumeric_move_multiplication", test_semantics_rejects_alphanumeric_move_multiplication},
        {"semantics_rejects_alphanumeric_move_division", test_semantics_rejects_alphanumeric_move_division},
        {"semantics_rejects_alphanumeric_floating_addition", test_semantics_rejects_alphanumeric_floating_addition},
        {"semantics_rejects_alphanumeric_floating_subtraction", test_semantics_rejects_alphanumeric_floating_subtraction},
        {"semantics_rejects_alphanumeric_floating_multiplication", test_semantics_rejects_alphanumeric_floating_multiplication},
        {"semantics_rejects_alphanumeric_floating_division", test_semantics_rejects_alphanumeric_floating_division},
        {"semantics_reports_mixed_numeric_floating_division_message", test_semantics_reports_mixed_numeric_floating_division_message}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
