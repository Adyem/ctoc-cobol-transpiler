#include "test_suites.hpp"

FT_TEST(test_cblc_register_translation_unit_exports_records_entrypoint_and_helpers)
{
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    const t_transpiler_entrypoint *entrypoint;
    const t_transpiler_function_signature *signature;
    const char *source;
    int status;

    source = "function void helper() {\n"
        "    return;\n"
        "}\n\n"
        "function void main() {\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "translation unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "alpha_mod", &unit),
            "export registration should succeed") != FT_SUCCESS)
        goto cleanup;
    entrypoint = transpiler_context_get_entrypoint(&context);
    if (!entrypoint)
    {
        pf_printf("Assertion failed: expected entrypoint to be registered\n");
        goto cleanup;
    }
    if (test_expect_cstring_equal(entrypoint->name, "main",
            "entrypoint should record main function") != FT_SUCCESS)
        goto cleanup;
    signature = transpiler_context_find_function(&context, "alpha_mod", "main");
    if (!signature)
    {
        pf_printf("Assertion failed: expected main function to be registered\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(signature->visibility), TRANSPILE_SYMBOL_PUBLIC,
            "entrypoint should be exported as public") != FT_SUCCESS)
        goto cleanup;
    signature = transpiler_context_find_function(&context, "alpha_mod", "helper");
    if (!signature)
    {
        pf_printf("Assertion failed: expected helper function to be registered\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(signature->visibility), TRANSPILE_SYMBOL_PUBLIC,
            "helper should be exported as public") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(context.function_count), 2,
            "two functions should be tracked") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_register_translation_unit_exports_skips_entrypoint_without_main)
{
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    const t_transpiler_entrypoint *entrypoint;
    const t_transpiler_function_signature *signature;
    const char *source;
    int status;

    source = "function void show_banner() {\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "translation unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "worker_mod", &unit),
            "export registration should succeed") != FT_SUCCESS)
        goto cleanup;
    entrypoint = transpiler_context_get_entrypoint(&context);
    if (entrypoint)
    {
        pf_printf("Assertion failed: modules without main should not register entrypoints\n");
        goto cleanup;
    }
    signature = transpiler_context_find_function(&context, "worker_mod", "show_banner");
    if (!signature)
    {
        pf_printf("Assertion failed: expected worker function to be registered\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(signature->visibility), TRANSPILE_SYMBOL_PUBLIC,
            "worker function should be exported as public") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(context.function_count), 1,
            "one function should be tracked") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_register_translation_unit_exports_reports_duplicate_entrypoint)
{
    t_transpiler_context context;
    t_cblc_translation_unit first_unit;
    t_cblc_translation_unit second_unit;
    const char *first_source;
    const char *second_source;
    int status;

    first_source = "function void main() {\n"
        "    return;\n"
        "}\n";
    second_source = "function void main() {\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&first_unit);
    cblc_translation_unit_init(&second_unit);
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "alternate_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(first_source, &first_unit),
            "first module should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "main_mod", &first_unit),
            "first module exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(second_source, &second_unit),
            "second module should parse") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "alternate_mod", &second_unit) != FT_FAILURE)
    {
        pf_printf("Assertion failed: duplicate entrypoint should fail to register\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "duplicate entrypoint should emit diagnostic") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(context.diagnostics.items[0].code, TRANSPILE_ERROR_ENTRYPOINT_DUPLICATE,
            "duplicate entrypoint should use dedicated error code") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "duplicate entrypoint should flag context error state") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&first_unit);
    cblc_translation_unit_dispose(&second_unit);
    transpiler_context_dispose(&context);
    return (status);
}

