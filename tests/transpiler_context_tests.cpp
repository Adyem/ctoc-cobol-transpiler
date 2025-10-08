#include "transpiler_context.hpp"

#include "test_suites.hpp"

FT_TEST(test_transpiler_context_registers_void_function)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", NULL),
        "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "worker_mod", "worker",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "void function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_find_function(&context, "worker_mod", "worker");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate registered function signature\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(signature->return_mode, TRANSPILE_FUNCTION_RETURN_VOID,
        "registered function should be void") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
        "context should not record errors for void function") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_registers_value_return_function)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "compute_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "compute_mod", "compute",
            TRANSPILE_FUNCTION_RETURN_VALUE, TRANSPILE_SYMBOL_PRIVATE),
            "value-returning function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_find_function(&context, "compute_mod", "compute");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate value-return function signature\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(signature->return_mode, TRANSPILE_FUNCTION_RETURN_VALUE,
            "registered function should record value return mode") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "context should not record errors for value-return function") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_registers_main_entrypoint)
{
    t_transpiler_context context;
    const t_transpiler_entrypoint *entrypoint;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, "argc", "argv"),
            "entrypoint registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    entrypoint = transpiler_context_get_entrypoint(&context);
    if (!entrypoint)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to retrieve registered entrypoint\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(entrypoint->present, 1,
            "entrypoint should mark presence") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(entrypoint->has_argument_vectors, 1,
            "entrypoint should record argument vectors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(entrypoint->needs_argument_copy, 1,
            "entrypoint should request argument copying") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(entrypoint->argc_identifier, "argc",
            "entrypoint should copy argc identifier") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(entrypoint->argv_identifier, "argv",
            "entrypoint should copy argv identifier") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "context should not record errors for valid entrypoint") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_entrypoint_registers_function_signature)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, "argc", "argv"),
            "entrypoint registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_find_function(&context, "app_mod", "main");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected entrypoint to register function signature\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.function_count), 1,
            "entrypoint should add a single function signature") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(signature->return_mode, TRANSPILE_FUNCTION_RETURN_VOID,
            "registered signature should remain void") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "entrypoint registration should complete without errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_argument_mismatch_keeps_entrypoint_clear)
{
    t_transpiler_context context;
    const t_transpiler_entrypoint *entrypoint;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, "argc", NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected entrypoint registration to fail for partial arguments\n");
        return (FT_FAILURE);
    }
    entrypoint = transpiler_context_get_entrypoint(&context);
    if (entrypoint)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected entrypoint query to return NULL after failure\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.function_count), 0,
            "failing entrypoint should not register function signatures") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "context should report diagnostic for partial argument list") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "partial argument list should emit single diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_ENTRYPOINT_ARGUMENT_MISMATCH,
            "diagnostic should reference argument mismatch error") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_duplicate_function_registration)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "worker_mod", "worker",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "initial function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_function(&context, "worker_mod", "worker",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected duplicate function registration to fail\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.function_count), 1,
            "duplicate registration should not increase function count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "duplicate registration should record an error diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME,
            "diagnostic should report duplicate function name") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "context should report errors after duplicate registration") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_non_main_entrypoint)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_entrypoint(&context, "app_mod", "bootstrap",
            TRANSPILE_FUNCTION_RETURN_VOID, NULL, NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected entrypoint registration to fail for non-main name\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "context should report diagnostic for invalid entrypoint") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "invalid entrypoint should raise diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_ENTRYPOINT_INVALID_NAME,
            "diagnostic should use invalid entrypoint code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_registers_main_without_arguments)
{
    t_transpiler_context context;
    const t_transpiler_entrypoint *entrypoint;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, NULL, NULL),
            "entrypoint registration without arguments should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    entrypoint = transpiler_context_get_entrypoint(&context);
    if (!entrypoint)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to retrieve registered entrypoint\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(entrypoint->present, 1,
            "entrypoint should mark presence") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(entrypoint->has_argument_vectors, 0,
            "entrypoint should report missing argument vectors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(entrypoint->needs_argument_copy, 0,
            "entrypoint should not request argument copying") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(entrypoint->argc_identifier, "",
            "entrypoint should not store argc identifier when omitted") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(entrypoint->argv_identifier, "",
            "entrypoint should not store argv identifier when omitted") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "context should not record errors for argument-less entrypoint") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_argument_mismatch)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, "argc", NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected entrypoint registration to fail for argument mismatch\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "context should report diagnostic for mismatched arguments") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "argument mismatch should raise diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_ENTRYPOINT_ARGUMENT_MISMATCH,
            "diagnostic should use argument mismatch code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_non_void_entrypoint)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VALUE, NULL, NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected entrypoint registration to fail for non-void return\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "context should record error for non-void entrypoint") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "non-void entrypoint should emit diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_FUNCTION_RETURNS_VALUE,
            "diagnostic should use function return error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_duplicate_entrypoint)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "app_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, NULL, NULL),
            "initial entrypoint registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_entrypoint(&context, "app_mod", "main",
            TRANSPILE_FUNCTION_RETURN_VOID, NULL, NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected duplicate entrypoint to be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "context should record error for duplicate entrypoint") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "duplicate entrypoint should emit diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_ENTRYPOINT_DUPLICATE,
            "diagnostic should use duplicate entrypoint code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_allows_private_duplicates)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "first private function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "beta_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "second private function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.function_count), 2,
            "private duplicates should register in both modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "private duplicates should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_public_duplicate_exports)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC),
            "first public function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_function(&context, "beta_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected duplicate public export to be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "duplicate export should emit a diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_FUNCTION_EXPORT_CONFLICT,
            "duplicate export diagnostic should use export conflict code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_scans_imports_and_orders_modules)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    const size_t *order;
    size_t order_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", NULL),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_scan_imports_for_module(&context, "main_mod",
            "import \"worker_mod\";\n"),
            "import scan should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to query registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "two modules should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[0].import_count), 0,
            "worker module should not record imports") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[1].import_count), 1,
            "main module should record one import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 2,
            "module order should include both modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[0]].name, "worker_mod",
            "worker module should initialize before main") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[1]].name, "main_mod",
            "main module should initialize after dependencies") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_orders_modules_deterministically)
{
    t_transpiler_context context;
    const size_t *order;
    size_t order_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "delta_mod", NULL),
            "delta module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "alpha module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "charlie_mod", NULL),
            "charlie module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 3,
            "module order should contain three modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[0]].name, "alpha_mod",
            "alpha should appear first when no dependencies exist") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[1]].name, "charlie_mod",
            "charlie should appear second in lexical order") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[2]].name, "delta_mod",
            "delta should appear last in lexical order") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_empty_module_name)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_register_module(&context, "", NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected empty module name to be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.module_count), 0,
            "empty name should not register a module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "blank module registration should not record diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_empty_import_path)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_module_import(&context, "main_mod", "") != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected empty import path to be rejected\n");
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to inspect registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 1,
            "only one module should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[0].import_count), 0,
            "empty import should not add dependencies") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_resolves_imports_by_path)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    const size_t *order;
    size_t order_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", "modules/worker_mod.cblc"),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", "modules/main_mod.cblc"),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_scan_imports_for_module(&context, "main_mod",
            "import \"modules/worker_mod.cblc\";\n"),
            "import scan should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to inspect registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "two modules should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[1].import_count), 1,
            "main module should record a single path import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 2,
            "module order should contain two entries") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[0]].name, "worker_mod",
            "worker module should run before main via path import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[1].imports[0].resolved_index), 0,
            "path import should resolve to worker module index") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_registers_file_declaration)
{
    t_transpiler_context context;
    const t_transpiler_file_declaration *files;
    size_t count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_file(&context, "input_file", TRANSPILE_FILE_ROLE_INPUT,
            "input.txt", 0), "file registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    files = transpiler_context_get_files(&context, &count);
    if (!files)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected file registry to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(count), 1,
            "file registration should add a single entry") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(files[0].name, "input_file",
            "file registration should copy identifier") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(files[0].path, "input.txt",
            "file registration should copy path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(files[0].role), static_cast<int>(TRANSPILE_FILE_ROLE_INPUT),
            "file registration should store role") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(files[0].explicit_record_length), 0,
            "file should default explicit length to zero when unspecified") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(files[0].inferred_record_length), 0,
            "inferred length should start at zero") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_tracks_record_length_hint)
{
    t_transpiler_context context;
    const t_transpiler_file_declaration *files;
    size_t count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_file(&context, "log_file", TRANSPILE_FILE_ROLE_OUTPUT,
            "log.txt", 0), "file registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_record_file_length_hint(&context, "log_file", 64),
            "initial hint should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_record_file_length_hint(&context, "log_file", 128),
            "larger hint should update inferred length") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    files = transpiler_context_get_files(&context, &count);
    if (!files)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected file registry to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(files[0].inferred_record_length), 128,
            "inferred length should track maximum hint") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_records_multiple_io_paths)
{
    t_transpiler_context context;
    const char *inputs[] = {"first.cblc", "second.cblc"};
    const char *outputs[] = {"first.cob", "second.cob"};

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_set_io_paths(&context, inputs, 2, outputs, 2) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to record multiple IO paths\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.source_count), 2,
            "context should track two source paths") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.target_count), 2,
            "context should track two target paths") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_paths[0], "first.cblc",
            "first source path should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_paths[1], "second.cblc",
            "second source path should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_paths[0], "first.cob",
            "first target path should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_paths[1], "second.cob",
            "second target path should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_path, "first.cblc",
            "context should expose first source via legacy field") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_path, "first.cob",
            "context should expose first target via legacy field") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_mismatched_io_paths)
{
    t_transpiler_context context;
    const char *inputs[] = {"first.cblc", "second.cblc"};
    const char *outputs[] = {"only.cob"};

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_set_io_paths(&context, inputs, 2, outputs, 1) == FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected mismatched counts to fail\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.source_count), 0,
            "context should leave source count empty on failure") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.target_count), 0,
            "context should leave target count empty on failure") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.source_path || context.target_path)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: legacy path aliases should be cleared on failure\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

const t_test_case *get_transpiler_context_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_context_registers_void_function", test_transpiler_context_registers_void_function},
        {"transpiler_context_registers_value_return_function", test_transpiler_context_registers_value_return_function},
        {"transpiler_context_registers_main_entrypoint", test_transpiler_context_registers_main_entrypoint},
        {"transpiler_context_entrypoint_registers_function_signature", test_transpiler_context_entrypoint_registers_function_signature},
        {"transpiler_context_rejects_duplicate_function_registration", test_transpiler_context_rejects_duplicate_function_registration},
        {"transpiler_context_rejects_non_main_entrypoint", test_transpiler_context_rejects_non_main_entrypoint},
        {"transpiler_context_registers_main_without_arguments", test_transpiler_context_registers_main_without_arguments},
        {"transpiler_context_argument_mismatch_keeps_entrypoint_clear", test_transpiler_context_argument_mismatch_keeps_entrypoint_clear},
        {"transpiler_context_rejects_argument_mismatch", test_transpiler_context_rejects_argument_mismatch},
        {"transpiler_context_rejects_non_void_entrypoint", test_transpiler_context_rejects_non_void_entrypoint},
        {"transpiler_context_rejects_duplicate_entrypoint", test_transpiler_context_rejects_duplicate_entrypoint},
        {"transpiler_context_allows_private_duplicates", test_transpiler_context_allows_private_duplicates},
        {"transpiler_context_rejects_public_duplicate_exports", test_transpiler_context_rejects_public_duplicate_exports},
        {"transpiler_context_scans_imports_and_orders_modules", test_transpiler_context_scans_imports_and_orders_modules},
        {"transpiler_context_orders_modules_deterministically", test_transpiler_context_orders_modules_deterministically},
        {"transpiler_context_rejects_empty_module_name", test_transpiler_context_rejects_empty_module_name},
        {"transpiler_context_rejects_empty_import_path", test_transpiler_context_rejects_empty_import_path},
        {"transpiler_context_resolves_imports_by_path", test_transpiler_context_resolves_imports_by_path},
        {"transpiler_context_registers_file_declaration", test_transpiler_context_registers_file_declaration},
        {"transpiler_context_tracks_record_length_hint", test_transpiler_context_tracks_record_length_hint},
        {"transpiler_context_records_multiple_io_paths", test_transpiler_context_records_multiple_io_paths},
        {"transpiler_context_rejects_mismatched_io_paths", test_transpiler_context_rejects_mismatched_io_paths}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
