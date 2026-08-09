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

FT_TEST(test_cblc_register_translation_unit_exports_records_exception_throw_summary)
{
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    const t_transpiler_function_signature *signature;
    int status;

    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (test_expect_success(transpiler_context_register_module(&context, "summary_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(
            "void worker() {\n    throw 7;\n}\n", &unit),
            "throwing translation unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "summary_mod", &unit),
            "export registration should succeed") != FT_SUCCESS)
        goto cleanup;
    signature = transpiler_context_find_function(&context, "summary_mod", "worker");
    if (!signature)
    {
        std::printf("Assertion failed: expected worker function to be registered\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(signature->exception_type_count), 1,
            "known throw should export one exception type") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(signature->exception_type_ids[0] != 0), 1,
            "known throw should export a nonzero exception type ID") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(signature->exception_types_unknown, 0,
            "direct known throw should not be marked unknown") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_propagates_transitive_exception_summary)
{
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    const t_transpiler_function_signature *exported_signature;
    const char *source;
    size_t index;
    int status;

    source = "void leaf() {\n    throw 7;\n}\n"
        "void middle() {\n    leaf();\n}\n"
        "void top() {\n    middle();\n}\n";
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (test_expect_success(transpiler_context_register_module(&context, "effect_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "transitive throwing unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "effect_mod", &unit),
            "export registration should succeed") != FT_SUCCESS)
        goto cleanup;
    exported_signature = transpiler_context_find_function(&context, "effect_mod", "top");
    if (!exported_signature)
    {
        std::printf("Assertion failed: expected top export to be present\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(exported_signature->exception_type_count), 1,
            "export should include transitive known exception type") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "effect_mod", &unit),
            "local calls should resolve") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit.function_count
        && std::strncmp(unit.functions[index].source_name, "top",
            sizeof(unit.functions[index].source_name)) != 0)
        index += 1;
    if (index == unit.function_count)
    {
        std::printf("Assertion failed: expected top function to be present\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(unit.functions[index].exception_type_count), 1,
            "transitive call should propagate known exception type") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.functions[index].exception_types_unknown, 0,
            "transitive known call should remain precise") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_register_translation_unit_exports_records_method_exception_summary)
{
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    const t_transpiler_type_signature *signature;
    const char *source;
    int status;

    source = "class Worker {\n"
        "    public:\n"
        "    Worker() { throw 5; }\n"
        "    void fail() { throw 4; }\n"
        "};\n";
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (test_expect_success(transpiler_context_register_module(&context, "method_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "method source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "method_mod", &unit),
            "method export should succeed") != FT_SUCCESS)
        goto cleanup;
    signature = transpiler_context_find_type(&context, "method_mod", "Worker");
    if (!signature || signature->method_count != 1 || signature->constructor_count != 1)
    {
        std::printf("Assertion failed: expected exported Worker method\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(signature->methods[0].exception_type_count), 1,
            "method should export one known exception type") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(signature->methods[0].exception_types_unknown, 0,
            "direct method throw should remain precise") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(signature->constructors[0].exception_type_count), 1,
            "constructor should export one known exception type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_register_type_signatures_reject_exception_type_id_collision)
{
    t_transpiler_context context;
    t_transpiler_type_signature first;
    t_transpiler_type_signature second;
    int status;

    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_register_module(&context, "first_mod", NULL) != FT_SUCCESS
        || transpiler_context_register_module(&context, "second_mod", NULL) != FT_SUCCESS)
        goto cleanup;
    std::memset(&first, 0, sizeof(first));
    std::memset(&second, 0, sizeof(second));
    ft_strlcpy(first.name, "FirstError", sizeof(first.name));
    ft_strlcpy(second.name, "SecondError", sizeof(second.name));
    first.kind = TRANSPILE_TYPE_CLASS;
    second.kind = TRANSPILE_TYPE_CLASS;
    first.visibility = TRANSPILE_SYMBOL_PUBLIC;
    second.visibility = TRANSPILE_SYMBOL_PUBLIC;
    first.exception_abi_version = CBLC_EXCEPTION_ABI_VERSION;
    second.exception_abi_version = CBLC_EXCEPTION_ABI_VERSION;
    first.exception_type_id = 777;
    second.exception_type_id = 777;
    if (transpiler_context_register_type_signature(&context, "first_mod", &first)
        != FT_SUCCESS
        || transpiler_context_register_type_signature(&context, "second_mod", &second)
            != FT_FAILURE)
    {
        std::printf("Assertion failed: exception type ID collisions should be rejected\n");
        goto cleanup;
    }
    if (context.diagnostics.count == 0
        || context.diagnostics.items[context.diagnostics.count - 1].code
            != TRANSPILE_ERROR_EXCEPTION_TYPE_ID_COLLISION)
    {
        std::printf("Assertion failed: type ID collision should use a dedicated diagnostic\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    transpiler_context_dispose(&context);
    return (status);
}
