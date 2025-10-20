#include "test_suites.hpp"

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

