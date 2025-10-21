#include "test_suites.hpp"

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

FT_TEST(test_transpiler_context_configures_indexed_file_metadata)
{
    t_transpiler_context context;
    const t_transpiler_file_declaration *files;
    size_t count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_file(&context, "orders", TRANSPILE_FILE_ROLE_DATA,
            "orders.dat", 0), "file registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_configure_file_organization(&context, "orders",
                TRANSPILE_FILE_ORGANIZATION_INDEXED),
            "organization configuration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_configure_file_keys(&context, "orders",
                "order-key", "customer-key"),
            "key configuration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_configure_file_lock_mode(&context, "orders",
                TRANSPILE_FILE_LOCK_MODE_AUTOMATIC),
            "lock configuration should succeed") != FT_SUCCESS)
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
            "indexed configuration should keep single entry") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(files[0].organization),
            static_cast<int>(TRANSPILE_FILE_ORGANIZATION_INDEXED),
            "organization should record indexed mode") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(files[0].record_key, "order-key",
            "record key should be stored") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(files[0].alternate_key, "customer-key",
            "alternate key should be stored") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(files[0].lock_mode),
            static_cast<int>(TRANSPILE_FILE_LOCK_MODE_AUTOMATIC),
            "lock mode should record automatic selection") != FT_SUCCESS)
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

