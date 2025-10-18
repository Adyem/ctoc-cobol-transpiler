#include "semantics_test_support.hpp"

#include "test_support.hpp"

FT_TEST(test_semantics_accepts_numeric_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-NUMERIC", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-NUMERIC", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-NUMERIC",
            LEXER_TOKEN_EQUALS, "==", "RIGHT-NUMERIC") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_mixed_numeric_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-NUMERIC", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-NUMERIC",
            LEXER_TOKEN_EQUALS, "==", "RIGHT-FLOAT") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_floating_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-FLOAT", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-FLOAT",
            LEXER_TOKEN_EQUALS, "==", "RIGHT-FLOAT") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_mixed_floating_alphanumeric_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-FLOAT", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-ALPHA", "PIC X(12)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-FLOAT",
            LEXER_TOKEN_EQUALS, "==", "RIGHT-ALPHA") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_alphanumeric_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-ALPHA", "PIC X(10)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-ALPHA", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-ALPHA",
            LEXER_TOKEN_EQUALS, "==", "RIGHT-ALPHA") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_numeric_condition_not_equal)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-NUMERIC", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-NUMERIC", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-NUMERIC",
            LEXER_TOKEN_NOT_EQUALS, "<>", "RIGHT-NUMERIC") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_floating_condition_not_equal)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-FLOAT", "PIC 9V9(3)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-FLOAT",
            LEXER_TOKEN_NOT_EQUALS, "<>", "RIGHT-FLOAT") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_rejects_mixed_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-FIELD", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-FIELD", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "NUMERIC-FIELD",
            LEXER_TOKEN_EQUALS, "==", "ALPHA-FIELD") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_mixed_numeric_condition_not_equal)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-NUMERIC", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-NUMERIC",
            LEXER_TOKEN_NOT_EQUALS, "<>", "RIGHT-FLOAT") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_accepts_alphanumeric_condition_not_equal)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-ALPHA", "PIC X(8)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-ALPHA", "PIC X(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-ALPHA",
            LEXER_TOKEN_NOT_EQUALS, "<>", "RIGHT-ALPHA") != FT_SUCCESS)
    {
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

FT_TEST(test_semantics_rejects_mixed_alphanumeric_numeric_condition_not_equal)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-ALPHA", "PIC X(5)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-NUMERIC", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-ALPHA",
            LEXER_TOKEN_NOT_EQUALS, "<>", "RIGHT-NUMERIC") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}
