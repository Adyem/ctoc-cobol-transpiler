#include "libft/Libft/libft.hpp"

#include "semantics_test_support.hpp"

#include "semantics_test_groups.hpp"

static size_t semantics_context_warning_count(const t_transpiler_context *context)
{
    size_t index;
    size_t total;

    total = 0;
    if (!context)
        return (0);
    index = 0;
    while (index < context->diagnostics.count)
    {
        if (context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_WARNING)
            total += 1;
        index += 1;
    }
    return (total);
}

static int semantics_context_contains_warning(const t_transpiler_context *context,
    int code)
{
    size_t index;

    if (!context)
        return (0);
    index = 0;
    while (index < context->diagnostics.count)
    {
        if (context->diagnostics.items[index].code == code
            && context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_WARNING)
            return (1);
        index += 1;
    }
    return (0);
}

FT_TEST(test_semantics_warns_on_unused_data_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("UNUSED", "PIC 9(4).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_warning_count(&context) >= 1)
            {
                if (semantics_context_contains_warning(&context,
                        TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM))
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_write_only_data_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TARGET", "PIC X(8).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, NULL, "TARGET", 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_warning_count(&context) >= 1)
            {
                if (semantics_context_contains_warning(&context,
                        TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM))
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_read_without_write)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SOURCE", "PIC 9(4).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "SOURCE",
            LEXER_TOKEN_EQUALS, "=", "SOURCE") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_warning_count(&context) >= 1)
            {
                if (semantics_context_contains_warning(&context,
                        TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE))
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

const t_test_case *get_semantics_usage_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_warns_on_unused_data_item", test_semantics_warns_on_unused_data_item},
        {"semantics_warns_on_write_only_data_item", test_semantics_warns_on_write_only_data_item},
        {"semantics_warns_on_read_without_write", test_semantics_warns_on_read_without_write}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
