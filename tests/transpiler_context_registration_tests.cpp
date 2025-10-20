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

