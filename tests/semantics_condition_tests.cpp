#include "semantics_test_support.hpp"

#include "semantics_test_groups.hpp"
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

FT_TEST(test_semantics_accepts_numeric_condition_less_than)
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
            LEXER_TOKEN_LESS_THAN, "<", "RIGHT-NUMERIC") != FT_SUCCESS)
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

FT_TEST(test_semantics_rejects_mixed_numeric_condition_less_than)
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
            LEXER_TOKEN_LESS_THAN, "<", "RIGHT-FLOAT") != FT_SUCCESS)
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
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_reports_mixed_numeric_condition_less_than_message)
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
            LEXER_TOKEN_LESS_THAN, "<", "RIGHT-FLOAT") != FT_SUCCESS)
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
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
            {
                if (test_expect_cstring_equal(context.diagnostics.items[0].message,
                        "condition operator '<' requires operands of the same type but 'LEFT-NUMERIC' is numeric and 'RIGHT-FLOAT' is floating",
                        "mixed numeric condition should describe operand kinds") == FT_SUCCESS)
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_condition_less_than)
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
            LEXER_TOKEN_LESS_THAN, "<", "RIGHT-ALPHA") != FT_SUCCESS)
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
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

const t_test_case *get_semantics_condition_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_accepts_numeric_condition_equality", test_semantics_accepts_numeric_condition_equality},
        {"semantics_rejects_mixed_condition_equality", test_semantics_rejects_mixed_condition_equality},
        {"semantics_accepts_numeric_condition_less_than", test_semantics_accepts_numeric_condition_less_than},
        {"semantics_rejects_mixed_numeric_condition_less_than", test_semantics_rejects_mixed_numeric_condition_less_than},
        {"semantics_reports_mixed_numeric_condition_less_than_message", test_semantics_reports_mixed_numeric_condition_less_than_message},
        {"semantics_rejects_alphanumeric_condition_less_than", test_semantics_rejects_alphanumeric_condition_less_than}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
