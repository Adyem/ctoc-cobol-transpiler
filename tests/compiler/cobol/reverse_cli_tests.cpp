#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_reverse_cli_tests(size_t *count);

FT_TEST(test_reverse_control_flow_golden_cobol_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[64];
    char fixture_buffer[4096];
    const char *fixture_path;
    const char *expected_output;
    const char *log_path;
    int command_length;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "0011\n";
    fixture_path = "samples/cobol/reverse_control_flow.cob";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_control_flow_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(fixture_path, fixture_buffer, sizeof(fixture_buffer)) != FT_SUCCESS)
    {
        test_remove_file(source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, fixture_buffer) != FT_SUCCESS)
    {
        test_remove_file(source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_generated.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, NULL, NULL, NULL);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_generated.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s > %s 2>&1", binary_path, source_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: cobc should compile generated reverse_control_flow COBOL\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "cd %s && ./reverse_control_flow_generated.bin > reverse_control_flow_generated.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: generated reverse_control_flow binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (std::strncmp(output_buffer, expected_output, std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: generated reverse_control_flow binary should emit expected DISPLAY output\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_reverse_control_flow_multi_module_executes)
{
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[128];
    char main_fixture_buffer[4096];
    char worker_fixture_buffer[4096];
    const char *main_fixture_path;
    const char *worker_fixture_path;
    const char *expected_output;
    const char *log_path;
    int command_length;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    main_source_path[0] = '\0';
    worker_source_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "WORKER READY\n   1\n";
    main_fixture_path = "samples/cobol/multi_module_main.cob";
    worker_fixture_path = "samples/cobol/multi_module_worker.cob";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_control_flow_main.cob", main_source_path,
            sizeof(main_source_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(main_fixture_path, main_fixture_buffer, sizeof(main_fixture_buffer)) != FT_SUCCESS)
    {
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(main_source_path, main_fixture_buffer) != FT_SUCCESS)
    {
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_worker.cob", worker_source_path,
            sizeof(worker_source_path)) != FT_SUCCESS)
    {
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(worker_fixture_path, worker_fixture_buffer, sizeof(worker_fixture_buffer)) != FT_SUCCESS)
    {
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(worker_source_path, worker_fixture_buffer) != FT_SUCCESS)
    {
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_multi.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow_multi.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_multi.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s > %s 2>&1", binary_path, main_source_path, worker_source_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: cobc should compile generated multi-module COBOL\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "cd %s && ./reverse_control_flow_multi.bin > reverse_control_flow_multi.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: generated multi-module COBOL should execute successfully\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (std::strncmp(output_buffer, expected_output, std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: generated multi-module COBOL should emit expected banner and accumulator\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_file(worker_source_path);
        test_remove_file(main_source_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_file(worker_source_path);
    test_remove_file(main_source_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_cli_split_class_multi_module_executes)
{
    const char *declaration_source;
    const char *implementation_source;
    const char *main_source;
    const char *expected_output;
    char directory[256];
    char declaration_source_path[256];
    char implementation_source_path[256];
    char main_source_path[256];
    char declaration_output_path[256];
    char implementation_output_path[256];
    char main_output_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[1024];
    char output_buffer[128];
    const char *log_path;
    int command_length;

    FT_REQUIRE_COBC();
    declaration_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "        void add(int delta);\n"
        "};\n";
    implementation_source = "import \"counter_decl\";\n"
        "\n"
        "void Counter::add(int delta)\n"
        "{\n"
        "    value = value + delta;\n"
        "    return;\n"
        "}\n";
    main_source = "import \"counter_decl\";\n"
        "import \"counter_impl\";\n"
        "\n"
        "Counter counter;\n"
        "\n"
        "void main()\n"
        "{\n"
        "    counter.value = 40;\n"
        "    counter.add(2);\n"
        "    display(counter.value);\n"
        "    return;\n"
        "}\n";
    expected_output = "+000000042\n";
    directory[0] = '\0';
    declaration_source_path[0] = '\0';
    implementation_source_path[0] = '\0';
    main_source_path[0] = '\0';
    declaration_output_path[0] = '\0';
    implementation_output_path[0] = '\0';
    main_output_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "counter_decl.cblc", declaration_source_path,
            sizeof(declaration_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "counter_impl.cblc", implementation_source_path,
            sizeof(implementation_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "main_program.cblc", main_source_path,
            sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(declaration_source_path, declaration_source) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(implementation_source_path, implementation_source) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(main_source_path, main_source) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "counter_decl.cob", declaration_output_path,
            sizeof(declaration_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "counter_impl.cob", implementation_output_path,
            sizeof(implementation_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "main_program.cob", main_output_path,
            sizeof(main_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "split_class_multi.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "split_class_multi.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "split_class_multi.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --direction cblc-to-cobol --input %s --output %s --input %s --output %s --input %s --output %s",
        declaration_source_path, declaration_output_path,
        implementation_source_path, implementation_output_path,
        main_source_path, main_output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: ctoc_cobol_transpiler should generate split-class multi-module COBOL\n");
        goto cleanup;
    }
    command_length = std::snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s %s > %s 2>&1",
        binary_path, main_output_path, implementation_output_path, declaration_output_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: cobc should compile generated split-class multi-module COBOL\n");
        goto cleanup;
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
        goto cleanup;
    command_length = std::snprintf(command, sizeof(command),
        "cd %s && ./split_class_multi.bin > split_class_multi.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: generated split-class multi-module COBOL should execute successfully\n");
        goto cleanup;
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output, std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: generated split-class multi-module COBOL should emit expected output\n");
        goto cleanup;
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_file(main_output_path);
    test_remove_file(implementation_output_path);
    test_remove_file(declaration_output_path);
    test_remove_file(main_source_path);
    test_remove_file(implementation_source_path);
    test_remove_file(declaration_source_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
cleanup:
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_file(main_output_path);
    test_remove_file(implementation_output_path);
    test_remove_file(declaration_output_path);
    test_remove_file(main_source_path);
    test_remove_file(implementation_source_path);
    test_remove_file(declaration_source_path);
    test_remove_directory(directory);
    return (FT_FAILURE);
}

const t_test_case *get_compiler_cobol_reverse_cli_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"reverse_control_flow_golden_cobol_executes", test_reverse_control_flow_golden_cobol_executes},
        {"reverse_control_flow_multi_module_executes", test_reverse_control_flow_multi_module_executes},
        {"cblc_cli_split_class_multi_module_executes", test_cblc_cli_split_class_multi_module_executes}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
