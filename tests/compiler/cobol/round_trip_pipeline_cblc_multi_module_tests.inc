FT_TEST(test_cblc_multi_module_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    char main_buffer[32768];
    char worker_buffer[32768];
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_main_cobol;
    char *generated_worker_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    directory[0] = '\0';
    main_source_path[0] = '\0';
    worker_source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "WORKER READY\n   1\n";
    if (test_read_text_file("samples/cblc/multi_module_main.cblc",
            main_buffer, sizeof(main_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file("samples/cblc/multi_module_worker.cblc",
            worker_buffer, sizeof(worker_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(main_buffer, &main_unit),
            "multi_module_main.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_buffer, &worker_unit),
            "multi_module_worker.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "multi_module_main.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit,
                &generated_worker_cobol),
            "multi_module_worker.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_main_cobol),
            "generated main COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_worker_cobol),
            "generated worker COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "multi_module_main_generated.cob",
            main_source_path, sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_worker_generated.cob",
            worker_source_path, sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "multi_module_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(main_source_path, generated_main_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(worker_source_path, generated_worker_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol_with_module(binary_path, main_source_path,
            worker_source_path, compile_log_path,
            "cobc should compile translated multi_module program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "multi_module_generated.bin",
            "multi_module_generated.txt",
            "translated multi_module binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated multi_module binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(main_source_path, binary_path,
            output_path, log_path);
        if (worker_source_path[0] != '\0')
            test_remove_file(worker_source_path);
        test_remove_directory(directory);
    }
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

FT_TEST(test_cblc_multi_module_parameterized_call_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_main_cobol;
    char *generated_worker_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;
    int context_initialized;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = 7;\n"
        "    worker(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    display(value);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    directory[0] = '\0';
    main_source_path[0] = '\0';
    worker_source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    context_initialized = 0;
    status = FT_FAILURE;
    expected_output = "   7\n";
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "parameterized multi-module main source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "parameterized multi-module worker source should parse") != FT_SUCCESS)
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
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "parameterized multi-module main source should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit,
                &generated_worker_cobol),
            "parameterized multi-module worker source should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_main_cobol),
            "parameterized multi-module generated main COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_worker_cobol),
            "parameterized multi-module generated worker COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "multi_module_param_main_generated.cob",
            main_source_path, sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_param_worker_generated.cob",
            worker_source_path, sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_param_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_param_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "multi_module_param_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(main_source_path, generated_main_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(worker_source_path, generated_worker_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol_with_module(binary_path, main_source_path,
            worker_source_path, compile_log_path,
            "cobc should compile translated parameterized multi-module program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "multi_module_param_generated.bin",
            "multi_module_param_generated.txt",
            "translated parameterized multi-module binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated parameterized multi-module binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(main_source_path, binary_path,
            output_path, log_path);
        if (worker_source_path[0] != '\0')
            test_remove_file(worker_source_path);
        test_remove_directory(directory);
    }
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    if (context_initialized)
        transpiler_context_dispose(&context);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

FT_TEST(test_cblc_multi_module_return_call_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_main_cobol;
    char *generated_worker_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;
    int context_initialized;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker(7);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    directory[0] = '\0';
    main_source_path[0] = '\0';
    worker_source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    context_initialized = 0;
    status = FT_FAILURE;
    expected_output = "   8\n";
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "returning multi-module main source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "returning multi-module worker source should parse") != FT_SUCCESS)
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
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "returning multi-module main source should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit,
                &generated_worker_cobol),
            "returning multi-module worker source should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_main_cobol),
            "returning multi-module generated main COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_worker_cobol),
            "returning multi-module generated worker COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "multi_module_return_main_generated.cob",
            main_source_path, sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_return_worker_generated.cob",
            worker_source_path, sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_return_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_return_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "multi_module_return_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(main_source_path, generated_main_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(worker_source_path, generated_worker_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol_with_module(binary_path, main_source_path,
            worker_source_path, compile_log_path,
            "cobc should compile translated returning multi-module program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "multi_module_return_generated.bin",
            "multi_module_return_generated.txt",
            "translated returning multi-module binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated returning multi-module binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(main_source_path, binary_path,
            output_path, log_path);
        if (worker_source_path[0] != '\0')
            test_remove_file(worker_source_path);
        test_remove_directory(directory);
    }
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    if (context_initialized)
        transpiler_context_dispose(&context);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

FT_TEST(test_cblc_multi_module_parameterized_two_variables_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "void main()\n"
        "{\n"
        "    left_value = 19;\n"
        "    right_value = 23;\n"
        "    worker(left_value, right_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker_total;\n"
        "void worker(int left, int right)\n"
        "{\n"
        "    worker_total = left + right;\n"
        "    display(worker_total);\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_two_variables_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_literal_and_variable_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int right_value;\n"
        "void main()\n"
        "{\n"
        "    right_value = 37;\n"
        "    worker(5, right_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker_total;\n"
        "void worker(int left, int right)\n"
        "{\n"
        "    worker_total = left + right;\n"
        "    display(worker_total);\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_literal_variable_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_return_two_args_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    left_value = 20;\n"
        "    right_value = 22;\n"
        "    total = worker(left_value, right_value);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int left, int right)\n"
        "{\n"
        "    return left + right;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_return_two_args_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_return_literal_arg_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker(40, 2);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int left, int right)\n"
        "{\n"
        "    return left + right;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_return_literal_args_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_three_arg_expression_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int first_value;\n"
        "int second_value;\n"
        "int third_value;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    first_value = 50;\n"
        "    second_value = 10;\n"
        "    third_value = 2;\n"
        "    total = worker(first_value, second_value, third_value);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int first, int second, int third)\n"
        "{\n"
        "    return first - second + third;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_three_arg_expression_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_bool_argument_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "bool should_print;\n"
        "void main()\n"
        "{\n"
        "    should_print = true;\n"
        "    worker(should_print);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(bool flag)\n"
        "{\n"
        "    if (flag) {\n"
        "        display(\"BOOL PARAM OK\");\n"
        "    } else {\n"
        "        display(\"BOOL PARAM BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_bool_generated", "BOOL PARAM OK\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_char_argument_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "char marker;\n"
        "void main()\n"
        "{\n"
        "    marker = 'Z';\n"
        "    worker(marker);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(char value)\n"
        "{\n"
        "    display(value);\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_char_generated", "Z\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_bool_return_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "bool matched;\n"
        "void main()\n"
        "{\n"
        "    left_value = 42;\n"
        "    right_value = 42;\n"
        "    matched = worker(left_value, right_value);\n"
        "    if (matched) {\n"
        "        display(\"BOOL RETURN OK\");\n"
        "    } else {\n"
        "        display(\"BOOL RETURN BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    worker_source = "bool worker(int left, int right)\n"
        "{\n"
        "    return left == right;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_bool_return_generated", "BOOL RETURN OK\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_char_return_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "char value;\n"
        "char result;\n"
        "void main()\n"
        "{\n"
        "    value = 'Q';\n"
        "    result = worker(value);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    worker_source = "char worker(char input)\n"
        "{\n"
        "    return input;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_char_return_generated", "Q\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_condition_uses_arg_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int score_value;\n"
        "void main()\n"
        "{\n"
        "    score_value = 42;\n"
        "    worker(score_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int score)\n"
        "{\n"
        "    if (score == 42) {\n"
        "        display(\"CONDITION PARAM OK\");\n"
        "    } else {\n"
        "        display(\"CONDITION PARAM BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_condition_generated", "CONDITION PARAM OK\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_mutates_int_argument_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int score_value;\n"
        "void main()\n"
        "{\n"
        "    score_value = 41;\n"
        "    worker(score_value);\n"
        "    display(score_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    value = value + 1;\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_mutates_int_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_mutates_two_int_arguments_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    left_value = 19;\n"
        "    right_value = 20;\n"
        "    worker(left_value, right_value);\n"
        "    total = left_value + right_value;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int left, int right)\n"
        "{\n"
        "    left = left + 1;\n"
        "    right = right + 2;\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_mutates_two_ints_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_mutates_bool_argument_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "bool flag_value;\n"
        "void main()\n"
        "{\n"
        "    flag_value = true;\n"
        "    worker(flag_value);\n"
        "    if (flag_value) {\n"
        "        display(\"BOOL MUTATE BAD\");\n"
        "    } else {\n"
        "        display(\"BOOL MUTATE OK\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(bool flag)\n"
        "{\n"
        "    flag = false;\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_mutates_bool_generated", "BOOL MUTATE OK\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_mutates_char_argument_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "char marker_value;\n"
        "void main()\n"
        "{\n"
        "    marker_value = 'A';\n"
        "    worker(marker_value);\n"
        "    display(marker_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(char marker)\n"
        "{\n"
        "    marker = 'Z';\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_mutates_char_generated", "Z\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_return_after_mutation_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int seed_value;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    seed_value = 40;\n"
        "    total = worker(seed_value);\n"
        "    display(seed_value);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int value)\n"
        "{\n"
        "    value = value + 1;\n"
        "    return value + 1;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_return_after_mutation_generated", "+000000041\n+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_repeated_calls_mutate_distinct_args_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int first_value;\n"
        "int second_value;\n"
        "void main()\n"
        "{\n"
        "    first_value = 40;\n"
        "    second_value = 41;\n"
        "    worker(first_value);\n"
        "    worker(second_value);\n"
        "    display(first_value);\n"
        "    display(second_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    value = value + 1;\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_repeated_mutation_generated", "+000000041\n+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_two_literals_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker(20, 22);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker_total;\n"
        "void worker(int left, int right)\n"
        "{\n"
        "    worker_total = left + right;\n"
        "    display(worker_total);\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_two_literals_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_return_variable_and_literal_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int base_value;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    base_value = 37;\n"
        "    total = worker(base_value, 5);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int left, int right)\n"
        "{\n"
        "    return left + right;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_return_variable_literal_generated", "+000000042\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_bool_return_false_branch_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int score_value;\n"
        "bool matched;\n"
        "void main()\n"
        "{\n"
        "    score_value = 7;\n"
        "    matched = worker(score_value);\n"
        "    if (matched) {\n"
        "        display(\"BOOL FALSE BAD\");\n"
        "    } else {\n"
        "        display(\"BOOL FALSE OK\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    worker_source = "bool worker(int score)\n"
        "{\n"
        "    return score == 42;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_bool_return_false_generated", "BOOL FALSE OK\n"));
}

FT_TEST(test_cblc_multi_module_parameterized_condition_after_mutation_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "int score_value;\n"
        "void main()\n"
        "{\n"
        "    score_value = 41;\n"
        "    worker(score_value);\n"
        "    if (score_value == 42) {\n"
        "        display(\"MUTATED CONDITION OK\");\n"
        "    } else {\n"
        "        display(\"MUTATED CONDITION BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int score)\n"
        "{\n"
        "    score = score + 1;\n"
        "    return;\n"
        "}\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
            "multi_module_param_condition_after_mutation_generated", "MUTATED CONDITION OK\n"));
}
