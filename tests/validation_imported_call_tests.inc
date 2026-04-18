static int cblc_test_prepare_imported_worker_context(const char *main_source,
    const char *worker_source, t_cblc_translation_unit *main_unit,
    t_cblc_translation_unit *worker_unit, t_transpiler_context *context,
    int *context_initialized, int register_import)
{
    if (!main_source || !worker_source || !main_unit || !worker_unit
        || !context || !context_initialized)
        return (FT_FAILURE);
    if (test_expect_success(cblc_parse_translation_unit(main_source, main_unit),
            "main module should parse") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_parse_translation_unit(worker_source, worker_unit),
            "worker module should parse") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_init(context) != FT_SUCCESS)
        return (FT_FAILURE);
    *context_initialized = 1;
    if (transpiler_context_register_module(context, "worker_mod", "worker_mod") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_register_module(context, "main_mod", "main_mod") != FT_SUCCESS)
        return (FT_FAILURE);
    if (register_import
        && transpiler_context_register_module_import(context, "main_mod", "worker_mod")
            != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_register_translation_unit_exports(context, "worker_mod", worker_unit)
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_register_translation_unit_exports(context, "main_mod", main_unit) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_test_expect_last_diagnostic_code(const t_transpiler_context *context,
    int expected_code, const char *message)
{
    if (!context || context->diagnostics.count == 0)
    {
        std::printf("Assertion failed: %s\n", message);
        return (FT_FAILURE);
    }
    if (context->diagnostics.items[context->diagnostics.count - 1].code != expected_code)
    {
        std::printf("Assertion failed: %s\n", message);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_parameterized_calls)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "void main()\n"
        "{\n"
        "    left_value = 4;\n"
        "    worker(left_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external parameterized call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with parameter should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: external parameterized calls should resolve when imports and signatures match\n");
        goto cleanup;
    }
    if (!main_unit.functions[0].statements[1].call_is_external)
    {
        std::printf("Assertion failed: resolved external parameterized call should stay marked external\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_two_argument_call)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "void main()\n"
        "{\n"
        "    worker(left_value, right_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int left, int right)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "two argument external call should resolve") != FT_SUCCESS)
        goto cleanup;
    if (!main_unit.functions[0].statements[0].call_is_external
        || main_unit.functions[0].statements[0].call_argument_count != 2)
    {
        std::printf("Assertion failed: resolved two argument external call should preserve arguments\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_external_argument_count_mismatch)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: external call with too few arguments should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH,
            "external argument count mismatch should record dedicated diagnostic") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_external_extra_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "void main()\n"
        "{\n"
        "    worker(left_value, right_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: external call with too many arguments should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH,
            "external extra argument mismatch should record dedicated diagnostic") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_external_argument_type_mismatch)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "char *label;\n"
        "void main()\n"
        "{\n"
        "    worker(label);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: external call with wrong argument type should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH,
            "external argument type mismatch should record semantic diagnostic") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_external_void_return_assignment)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker(4);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: assigning void external call should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_FUNCTION_EXTERNAL_RETURN_UNSUPPORTED,
            "void external return assignment should record dedicated diagnostic") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_unimported_external_parameterized_call)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "int value;\n"
        "void main()\n"
        "{\n"
        "    worker(value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 0) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: unimported external parameterized call should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_MODULE_IMPORT_REQUIRED,
            "unimported external parameterized call should require explicit import") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_string_variable_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "string label(16);\n"
        "void main()\n"
        "{\n"
        "    worker(label);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(string value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external string variable argument should resolve") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_string_literal_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker(\"HELLO\");\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(string value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external string literal argument should resolve") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_external_string_argument_type_mismatch)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int value;\n"
        "void main()\n"
        "{\n"
        "    worker(value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(string value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: external string call with int argument should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH,
            "external string argument type mismatch should record semantic diagnostic") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_int_pointer_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    worker(values);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external int pointer argument should resolve") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_void_pointer_from_int_pointer)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    worker(values);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(void *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external void pointer should accept typed pointer argument")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_external_pointer_argument_type_mismatch)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "char *label;\n"
        "void main()\n"
        "{\n"
        "    worker(label);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: external int pointer call with char pointer argument should fail\n");
        goto cleanup;
    }
    if (cblc_test_expect_last_diagnostic_code(&context,
            TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH,
            "external pointer argument type mismatch should record semantic diagnostic") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_parameterized_call_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "void main()\n"
        "{\n"
        "    left_value = 4;\n"
        "    worker(left_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    display(value);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external parameterized call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with external parameter should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "main module with external parameterized call should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "worker(left_value);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit external call arguments directly\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_string_literal_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker(\"HELLO\");\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(string value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "external string literal call should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !ft_strnstr(generated_c, "worker(\"HELLO\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should preserve external string literal argument\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_pointer_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    worker(values);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "external pointer call should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !ft_strnstr(generated_c, "worker(values);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should preserve external pointer argument\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_mixed_parameterized_call_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "void main()\n"
        "{\n"
        "    worker(left_value, 4);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int left, int right)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "external mixed argument call should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !ft_strnstr(generated_c, "worker(left_value, 4);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should preserve external mixed call arguments\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_pointer_argument_by_reference)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    worker(values);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external pointer argument should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY REFERENCE VALUES", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass pointer variable by reference\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_pointer_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    worker(values);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int *value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_cobol),
            "external pointer worker should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "01 WORKER-VALUE USAGE POINTER.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit pointer linkage storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "PROCEDURE DIVISION USING WORKER-VALUE.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept pointer linkage parameter\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_int_return_without_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker()\n"
        "{\n"
        "    return 7;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external int return without arguments should resolve") != FT_SUCCESS)
        goto cleanup;
    if (!main_unit.functions[0].statements[0].call_is_external)
    {
        std::printf("Assertion failed: external return call should be marked external\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_pointer_return_without_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    values = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int *result;\n"
        "int *worker()\n"
        "{\n"
        "    return result;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external pointer return without arguments should resolve")
        != FT_SUCCESS)
        goto cleanup;
    if (!main_unit.functions[0].statements[0].call_is_external)
    {
        std::printf("Assertion failed: external pointer return call should be marked external\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_pointer_return_with_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *input;\n"
        "int *output;\n"
        "void main()\n"
        "{\n"
        "    output = worker(input);\n"
        "    return;\n"
        "}\n";
    worker_source = "int *worker(int *value)\n"
        "{\n"
        "    return value;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_resolve_translation_unit_calls(&context, "main_mod",
                &main_unit), "external pointer return with pointer argument should resolve")
        != FT_SUCCESS)
        goto cleanup;
    if (!main_unit.functions[0].statements[0].call_is_external
        || main_unit.functions[0].statements[0].call_argument_count != 1)
    {
        std::printf("Assertion failed: external pointer return with argument should keep call metadata\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_int_return_without_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker()\n"
        "{\n"
        "    return 7;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "external int return should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !ft_strnstr(generated_c, "total = worker();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should preserve external int return assignment\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_pointer_return_without_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    values = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int *result;\n"
        "int *worker()\n"
        "{\n"
        "    return result;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "external pointer return should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !ft_strnstr(generated_c, "values = worker();",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should preserve external pointer return assignment\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_int_return_without_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker()\n"
        "{\n"
        "    return 7;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external int return should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY REFERENCE TOTAL", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass int return target by reference\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_pointer_return_without_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    values = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int *result;\n"
        "int *worker()\n"
        "{\n"
        "    return result;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external pointer return should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY REFERENCE VALUES", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass pointer return target by reference\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_pointer_return_with_argument)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *input;\n"
        "int *output;\n"
        "void main()\n"
        "{\n"
        "    output = worker(input);\n"
        "    return;\n"
        "}\n";
    worker_source = "int *worker(int *value)\n"
        "{\n"
        "    return value;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external pointer return with argument should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY REFERENCE INPUT BY REFERENCE OUTPUT",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass pointer argument and return target\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_pointer_return_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int *values;\n"
        "void main()\n"
        "{\n"
        "    values = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int *result;\n"
        "int *worker()\n"
        "{\n"
        "    return result;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_cobol),
            "external pointer return worker should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "01 CBLC-RETURN-WORKER USAGE POINTER.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit pointer return linkage storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "PROCEDURE DIVISION USING CBLC-RETURN-WORKER.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept pointer return linkage parameter\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_int_return_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker()\n"
        "{\n"
        "    return 7;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_cobol),
            "external int return worker should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "01 CBLC-RETURN-WORKER PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit int return linkage storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "PROCEDURE DIVISION USING CBLC-RETURN-WORKER.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept int return linkage parameter\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}
