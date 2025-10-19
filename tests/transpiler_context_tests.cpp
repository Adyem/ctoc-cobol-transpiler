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

FT_TEST(test_transpiler_context_registers_copybook_metadata)
{
    t_transpiler_context context;
    t_transpiler_copybook_item items[2];
    const t_transpiler_copybook *copybook;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(items, sizeof(items));
    ft_strlcpy(items[0].name, "COPY-ALPHA", sizeof(items[0].name));
    items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[0].declared_length = 10;
    ft_strlcpy(items[1].name, "COPY-NUMERIC", sizeof(items[1].name));
    items[1].kind = TRANSPILE_DATA_ITEM_NUMERIC;
    items[1].declared_length = 0;
    if (test_expect_success(transpiler_context_register_copybook(&context, "MASTER-COPY", items, 2),
            "copybook registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    copybook = transpiler_context_find_copybook(&context, "MASTER-COPY");
    if (!copybook)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate registered copybook metadata\n");
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(copybook->item_count, 2,
            "copybook should persist item count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(copybook->items[0].name, "COPY-ALPHA",
            "first copybook item name should match") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(copybook->items[0].kind, TRANSPILE_DATA_ITEM_ALPHANUMERIC,
            "first copybook item should be alphanumeric") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(copybook->items[0].declared_length, 10,
            "first copybook item should record declared length") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "copybook registration should not raise errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_duplicate_copybook)
{
    t_transpiler_context context;
    t_transpiler_copybook_item items[1];
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(items, sizeof(items));
    ft_strlcpy(items[0].name, "ONLY-FIELD", sizeof(items[0].name));
    items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[0].declared_length = 5;
    if (transpiler_context_register_copybook(&context, "DUP-COPY", items, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_copybook(&context, "DUP-COPY", items, 1) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1
            && context.diagnostics.count >= 1
            && context.diagnostics.items[0].code == TRANSPILE_ERROR_COPYBOOK_DUPLICATE)
            status = FT_SUCCESS;
    }
    transpiler_context_dispose(&context);
    return (status);
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

FT_TEST(test_transpiler_context_allows_private_access_within_module)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "private helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "alpha_mod", "alpha_mod", "helper");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: private helper should be accessible inside its module\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "private self-access should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "private self-access should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_private_access_across_modules)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

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
    if (test_expect_success(transpiler_context_register_module_import(&context, "beta_mod", "alpha_mod"),
            "dependent module should record import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module ordering should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "private helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "beta_mod", "alpha_mod", "helper");
    if (signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: cross-module private access should be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "private access rejection should emit one diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_FUNCTION_PRIVATE_ACCESS,
            "private access rejection should use dedicated error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "private access rejection should record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_allows_public_access_across_modules)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

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
    if (test_expect_success(transpiler_context_register_module_import(&context, "beta_mod", "alpha_mod"),
            "dependent module should record import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module ordering should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC),
            "public helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "beta_mod", "alpha_mod", "helper");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: cross-module public access should be permitted\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "public access should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "public access should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_requires_import_for_cross_module_access)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

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
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC),
            "public helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "beta_mod", "alpha_mod", "helper");
    if (signature)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: cross-module access should require imports\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "missing import should emit diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_MODULE_IMPORT_REQUIRED,
            "missing import diagnostic should use dedicated code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "missing import should mark context as having errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

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

FT_TEST(test_transpiler_context_resolves_imports_by_file_name)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    const size_t *order;
    size_t module_count;
    size_t order_count;
    size_t worker_index;
    size_t main_index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "metrics_main.cblc",
                "samples/cblc/metrics_main.cblc"),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "metrics_worker.cblc",
                "samples/cblc/workers/metrics_worker.cblc"),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "metrics_main.cblc",
                "metrics_worker.cblc"),
            "import by base file name should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "metrics_main.cblc",
                "samples/cblc/workers/metrics_worker.cblc"),
            "import by relative path should succeed") != FT_SUCCESS)
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
    if (ft_strncmp(modules[0].name, "metrics_worker.cblc", ft_strlen("metrics_worker.cblc") + 1) == 0)
    {
        worker_index = 0;
        main_index = 1;
    }
    else
    {
        worker_index = 1;
        main_index = 0;
    }
    if (test_expect_cstring_equal(modules[worker_index].name, "metrics_worker.cblc",
            "worker module should retain its base file name") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].name, "metrics_main.cblc",
            "main module should retain its base file name") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].import_count), 2,
            "main module should record both imports") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].imports[0].resolved_index),
            static_cast<int>(worker_index),
            "import by base file name should resolve to worker module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].imports[1].resolved_index),
            static_cast<int>(worker_index),
            "import by relative path should resolve to worker module") != FT_SUCCESS)
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
            "module order should include importer and dependency") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order[0]), static_cast<int>(worker_index),
            "worker should initialize before main module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order[1]), static_cast<int>(main_index),
            "main module should initialize after worker") != FT_SUCCESS)
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

FT_TEST(test_transpiler_context_deduplicates_module_imports)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    size_t main_index;

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
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "worker_mod"),
            "first import registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "worker_mod"),
            "duplicate import registration should succeed") != FT_SUCCESS)
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
    main_index = 0;
    while (main_index < module_count
        && ft_strncmp(modules[main_index].name, "main_mod", TRANSPILE_MODULE_NAME_MAX) != 0)
        main_index += 1;
    if (main_index >= module_count)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected main module to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].import_count), 1,
            "duplicate imports should not increase import count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].imports[0].path, "worker_mod",
            "import path should be retained after deduplication") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "duplicate imports should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "duplicate imports should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_sorts_module_imports)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    size_t main_index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "alpha module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "omega_mod", NULL),
            "omega module registration should succeed") != FT_SUCCESS)
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
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "omega_mod"),
            "first import registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "alpha_mod"),
            "second import registration should succeed") != FT_SUCCESS)
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
    main_index = 0;
    while (main_index < module_count
        && ft_strncmp(modules[main_index].name, "main_mod", TRANSPILE_MODULE_NAME_MAX) != 0)
        main_index += 1;
    if (main_index >= module_count)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected main module to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].import_count), 2,
            "two imports should be tracked for main module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].imports[0].path, "alpha_mod",
            "imports should be sorted lexicographically") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].imports[1].path, "omega_mod",
            "imports should retain relative ordering after sort") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "sorted imports should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "sorted imports should not record errors") != FT_SUCCESS)
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

FT_TEST(test_transpiler_context_unit_reset_preserves_registered_modules)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;

    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
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
    transpiler_context_reset_unit_state(&context);
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module registry to remain accessible\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "unit reset should preserve registered modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[0].name, "alpha_mod",
            "first module should remain registered after unit reset") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[1].name, "beta_mod",
            "second module should remain registered after unit reset") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_reset_module_registry(&context);
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module registry pointer to remain valid\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 0,
            "module registry reset should clear registered modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_preserves_caller_declared_length)
{
    t_transpiler_context context;
    const t_transpiler_data_item *item;

    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 12, 0),
            "initial declared length registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 5, 0),
            "caller length should update binding") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 18, 0),
            "wider callee buffers should not override caller length") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 0, 0),
            "zero length updates should preserve prior metadata") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    item = transpiler_context_find_data_item(&context, "SHARED-ARG");
    if (!item)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected shared argument metadata to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(item->declared_length), 5,
            "declared length should match caller-provided width") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_promotes_caller_length_after_callee_metadata)
{
    t_transpiler_context context;
    const t_transpiler_data_item *item;

    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-BUFFER", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 6, 0),
            "callee metadata should register") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-BUFFER", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 12, 0),
            "caller width should override callee metadata") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    item = transpiler_context_find_data_item(&context, "SHARED-BUFFER");
    if (!item)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected shared buffer metadata to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(item->declared_length), 12,
            "declared length should reflect caller width") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(item->has_caller_length, 1,
            "caller metadata flag should be recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-BUFFER", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 12, 0),
            "re-registering caller width should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_data_item(&context,
            "SHARED-BUFFER", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 6, 0) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: narrower metadata should be rejected after caller registration\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "narrower metadata should surface diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_narrower_callee_length)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 12, 0),
            "initial registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 5, 0),
            "caller registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_data_item(&context,
            "SHARED-ARG", TRANSPILE_DATA_ITEM_ALPHANUMERIC, 4, 0) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected narrower callee length to be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "narrower callee binding should record an error") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "narrower length diagnostic should be recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.diagnostics.items[0].code != TRANSPILE_ERROR_DATA_ITEM_PARAMETER_TRUNCATION)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected parameter truncation diagnostic\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_records_read_only_flag)
{
    t_transpiler_context context;
    const t_transpiler_data_item *item;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "IMMUTABLE-ITEM", TRANSPILE_DATA_ITEM_NUMERIC, 0, 1),
            "read-only data item registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_data_item(&context,
                "IMMUTABLE-ITEM", TRANSPILE_DATA_ITEM_NUMERIC, 0, 0),
            "additional registrations should preserve read-only flag") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    item = transpiler_context_find_data_item(&context, "IMMUTABLE-ITEM");
    if (!item)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected immutable data item to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(item->is_read_only, 1,
            "read-only flag should be preserved across registrations") != FT_SUCCESS)
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

FT_TEST(test_transpiler_context_records_source_map_entry)
{
    t_transpiler_context context;
    t_transpiler_source_span cblc_span;
    t_transpiler_source_span cobol_span;
    const t_transpiler_source_map_entry *entry;
    const t_transpiler_source_map_entry *reverse_entry;
    size_t map_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(&cblc_span, sizeof(cblc_span));
    ft_bzero(&cobol_span, sizeof(cobol_span));
    ft_strlcpy(cblc_span.path, "modern.cblc", TRANSPILE_FILE_PATH_MAX);
    cblc_span.start_line = 7;
    cblc_span.start_column = 3;
    cblc_span.end_line = 9;
    cblc_span.end_column = 14;
    ft_strlcpy(cobol_span.path, "legacy.cob", TRANSPILE_FILE_PATH_MAX);
    cobol_span.start_line = 42;
    cobol_span.start_column = 11;
    cobol_span.end_line = 44;
    cobol_span.end_column = 27;
    if (test_expect_success(transpiler_context_record_source_map_entry(&context, &cblc_span, &cobol_span),
            "source map registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    entry = transpiler_context_map_cblc_to_cobol(&context, "modern.cblc", 8, 5);
    if (!entry)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate cblc to cobol mapping\n");
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(entry->cobol_span.path, "legacy.cob",
            "mapped cobol span should expose registered path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    reverse_entry = transpiler_context_map_cobol_to_cblc(&context, "legacy.cob", 43, 13);
    if (!reverse_entry)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate cobol to cblc mapping\n");
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(reverse_entry->cblc_span.path, "modern.cblc",
            "mapped cblc span should expose registered path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_get_source_maps(&context, &map_count);
    if (test_expect_int_equal(static_cast<int>(map_count), 1,
            "context should track a single source map entry") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_invalid_source_map_span)
{
    t_transpiler_context context;
    t_transpiler_source_span cblc_span;
    t_transpiler_source_span cobol_span;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(&cblc_span, sizeof(cblc_span));
    ft_bzero(&cobol_span, sizeof(cobol_span));
    ft_strlcpy(cblc_span.path, "broken.cblc", TRANSPILE_FILE_PATH_MAX);
    cblc_span.start_line = 5;
    cblc_span.start_column = 2;
    cblc_span.end_line = 4;
    cblc_span.end_column = 20;
    ft_strlcpy(cobol_span.path, "broken.cob", TRANSPILE_FILE_PATH_MAX);
    cobol_span.start_line = 8;
    cobol_span.start_column = 1;
    cobol_span.end_line = 9;
    cobol_span.end_column = 30;
    if (transpiler_context_record_source_map_entry(&context, &cblc_span, &cobol_span) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected invalid span registration to fail\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.source_map_count), 0,
            "context should not retain rejected source map entry") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
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
        {"transpiler_context_allows_private_access_within_module", test_transpiler_context_allows_private_access_within_module},
        {"transpiler_context_rejects_private_access_across_modules", test_transpiler_context_rejects_private_access_across_modules},
        {"transpiler_context_allows_public_access_across_modules", test_transpiler_context_allows_public_access_across_modules},
        {"transpiler_context_scans_imports_and_orders_modules", test_transpiler_context_scans_imports_and_orders_modules},
        {"transpiler_context_orders_modules_deterministically", test_transpiler_context_orders_modules_deterministically},
        {"transpiler_context_rejects_empty_module_name", test_transpiler_context_rejects_empty_module_name},
        {"transpiler_context_rejects_empty_import_path", test_transpiler_context_rejects_empty_import_path},
        {"transpiler_context_deduplicates_module_imports", test_transpiler_context_deduplicates_module_imports},
        {"transpiler_context_sorts_module_imports", test_transpiler_context_sorts_module_imports},
        {"transpiler_context_resolves_imports_by_path", test_transpiler_context_resolves_imports_by_path},
        {"transpiler_context_preserves_caller_declared_length", test_transpiler_context_preserves_caller_declared_length},
        {"transpiler_context_promotes_caller_length_after_callee_metadata", test_transpiler_context_promotes_caller_length_after_callee_metadata},
        {"transpiler_context_rejects_narrower_callee_length", test_transpiler_context_rejects_narrower_callee_length},
        {"transpiler_context_registers_file_declaration", test_transpiler_context_registers_file_declaration},
        {"transpiler_context_tracks_record_length_hint", test_transpiler_context_tracks_record_length_hint},
        {"transpiler_context_records_multiple_io_paths", test_transpiler_context_records_multiple_io_paths},
        {"transpiler_context_rejects_mismatched_io_paths", test_transpiler_context_rejects_mismatched_io_paths},
        {"transpiler_context_registers_copybook_metadata", test_transpiler_context_registers_copybook_metadata},
        {"transpiler_context_rejects_duplicate_copybook", test_transpiler_context_rejects_duplicate_copybook},
        {"transpiler_context_records_source_map_entry", test_transpiler_context_records_source_map_entry},
        {"transpiler_context_rejects_invalid_source_map_span", test_transpiler_context_rejects_invalid_source_map_span}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
