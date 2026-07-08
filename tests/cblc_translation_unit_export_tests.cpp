#include "test_suites.hpp"

FT_TEST(test_cblc_register_translation_unit_exports_records_entrypoint_and_helpers)
{
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    const t_transpiler_entrypoint *entrypoint;
    const t_transpiler_function_signature *signature;
    const char *source;
    int status;

    source = "void helper() {\n"
        "    return;\n"
        "}\n\n"
        "void main() {\n"
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
        std::printf("Assertion failed: expected entrypoint to be registered\n");
        goto cleanup;
    }
    if (test_expect_cstring_equal(entrypoint->name, "main",
            "entrypoint should record main function") != FT_SUCCESS)
        goto cleanup;
    signature = transpiler_context_find_function(&context, "alpha_mod", "main");
    if (!signature)
    {
        std::printf("Assertion failed: expected main function to be registered\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(signature->visibility), TRANSPILE_SYMBOL_PUBLIC,
            "entrypoint should be exported as public") != FT_SUCCESS)
        goto cleanup;
    signature = transpiler_context_find_function(&context, "alpha_mod", "helper");
    if (!signature)
    {
        std::printf("Assertion failed: expected helper function to be registered\n");
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

    source = "void show_banner() {\n"
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
        std::printf("Assertion failed: modules without main should not register entrypoints\n");
        goto cleanup;
    }
    signature = transpiler_context_find_function(&context, "worker_mod", "show_banner");
    if (!signature)
    {
        std::printf("Assertion failed: expected worker function to be registered\n");
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

FT_TEST(test_cblc_register_translation_unit_exports_and_imports_public_data)
{
    t_transpiler_context context;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    const t_transpiler_data_signature *data_signatures;
    const char *provider_source;
    const char *consumer_source;
    size_t data_count;
    int status;

    provider_source = "const int reserve_target = 600000;\n"
        "const int earned_premium = 480000;\n"
        "string title(\"POLICY SNAPSHOT\");\n"
        "string note(16);\n"
        "int title_length;\n"
        "int note_length;\n";
    consumer_source = "import \"provider_mod\";\n"
        "void main()\n"
        "{\n"
        "    display(reserve_target);\n"
        "    display(title);\n"
        "    display(note_length);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    if (test_expect_success(transpiler_context_register_module(&context, "provider_mod", NULL),
            "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "consumer_mod", NULL),
            "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "consumer_mod",
                "provider_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider module should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "provider_mod",
                &provider_unit), "provider exports should register") != FT_SUCCESS)
        goto cleanup;
    data_signatures = transpiler_context_get_data_signatures(&context, &data_count);
    if (test_expect_size_t_equal(data_count, 6,
            "provider should export six top-level data items") != FT_SUCCESS)
        goto cleanup;
    if (!data_signatures || std::strncmp(data_signatures[0].name, "reserve_target",
            sizeof(data_signatures[0].name)) != 0)
    {
        std::printf("Assertion failed: first exported data item should be reserve_target\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "consumer_mod",
                &consumer_unit), "consumer should import public data stubs") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(consumer_source, &consumer_unit),
            "consumer should parse imported data references") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.data_count, 6,
            "consumer should receive imported data declarations") != FT_SUCCESS)
        goto cleanup;
    {
        int saw_reserve_target;
        int saw_title;
        int saw_note_length;
        size_t index;

        saw_reserve_target = 0;
        saw_title = 0;
        saw_note_length = 0;
        index = 0;
        while (index < consumer_unit.data_count)
        {
            if (std::strncmp(consumer_unit.data_items[index].source_name, "reserve_target",
                    sizeof(consumer_unit.data_items[index].source_name)) == 0)
                saw_reserve_target = 1;
            if (std::strncmp(consumer_unit.data_items[index].source_name, "title",
                    sizeof(consumer_unit.data_items[index].source_name)) == 0)
                saw_title = 1;
            if (std::strncmp(consumer_unit.data_items[index].source_name, "note_length",
                    sizeof(consumer_unit.data_items[index].source_name)) == 0)
                saw_note_length = 1;
            index += 1;
        }
        if (!saw_reserve_target || !saw_title || !saw_note_length)
        {
            std::printf("Assertion failed: imported data declarations should be visible in consumer\n");
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
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

    first_source = "void main() {\n"
        "    return;\n"
        "}\n";
    second_source = "void main() {\n"
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
        std::printf("Assertion failed: duplicate entrypoint should fail to register\n");
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
