#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_multi_module_tests(size_t *count);

FT_TEST(test_cobol_transpiled_multi_module_fixtures_contain_expected_sections)
{
    const char *main_path;
    const char *worker_path;

    main_path = "samples/cobol/multi_module_main.cob";
    worker_path = "samples/cobol/multi_module_worker.cob";
    if (test_cobol_fixture_contains(main_path, "CALL 'SHOW-BANNER'.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(main_path, "ADD 1 TO ACCUMULATOR.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(worker_path, "DISPLAY WORKER-MESSAGE.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(main_path, "PERFORM ADD-ONCE.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(worker_path, "GOBACK.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_multi_module_main_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/multi_module_main.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MULTI-MODULE-MAIN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 ACCUMULATOR PIC 9(4) VALUE 0000.\n"
        "       01 DISPLAY-BUFFER PIC Z(4).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       MOVE 0 TO ACCUMULATOR.\n"
        "       CALL 'SHOW-BANNER'.\n"
        "       PERFORM ADD-ONCE.\n"
        "       MOVE ACCUMULATOR TO DISPLAY-BUFFER.\n"
        "       DISPLAY DISPLAY-BUFFER.\n"
        "       STOP RUN.\n"
        "\n"
        "ADD-ONCE.\n"
        "       ADD 1 TO ACCUMULATOR.\n"
        "       EXIT.\n"
        "       END PROGRAM MULTI-MODULE-MAIN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_multi_module_worker_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/multi_module_worker.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SHOW-BANNER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WORKER-MESSAGE PIC X(12) VALUE \"WORKER READY\".\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       DISPLAY WORKER-MESSAGE.\n"
        "       GOBACK.\n"
        "       END PROGRAM SHOW-BANNER.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_multi_module_executes)
{
    const char *binary_path;
    const char *output_path;
    char command[256];
    char output_buffer[128];
    const char *expected_output;
    int command_length;

    binary_path = "test_transpiled_multi_module.bin";
    output_path = "test_transpiled_multi_module.txt";
    expected_output = "WORKER READY\n   1\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/multi_module_main.cob samples/cobol/multi_module_worker.cob",
        binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled multi-module program\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: transpiled multi-module program should execute successfully\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: transpiled multi-module program should emit banner and accumulator\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_multi_module_exit_status)
{
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[256];
    char output_buffer[128];
    int command_length;
    int exit_status;

    binary_path = "test_transpiled_multi_module_status.bin";
    output_path = "test_transpiled_multi_module_status.txt";
    expected_output = "WORKER READY\n   1\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/multi_module_main.cob samples/cobol/multi_module_worker.cob",
        binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled multi-module program\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture multi-module COBOL exit status\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: transpiled multi-module program should exit successfully\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: transpiled multi-module program should emit banner and accumulator\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_multi_module_compiles_separately)
{
    const char *module_path;
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[512];
    char output_buffer[128];
    int command_length;

    module_path = "SHOW-BANNER.so";
    binary_path = "test_transpiled_multi_module_separate.bin";
    output_path = "test_transpiled_multi_module_separate.txt";
    expected_output = "WORKER READY\n   1\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -m -free -o %s samples/cobol/multi_module_worker.cob", module_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_remove_file(module_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile worker module separately\n");
        test_remove_file(module_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/multi_module_main.cob %s",
        binary_path, module_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_remove_file(module_path);
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should link main program with worker module\n");
        test_remove_file(module_path);
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "COB_LIBRARY_PATH=. ./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_remove_file(module_path);
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: multi-module binary should execute with separate worker module\n");
        test_remove_file(module_path);
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_remove_file(module_path);
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: separate-module execution should emit banner and accumulator\n");
        test_remove_file(module_path);
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_remove_file(module_path);
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_multi_module_executes_from_library_directory)
{
    char directory[256];
    char module_path[256];
    char binary_path[256];
    char output_path[256];
    char command[512];
    char output_buffer[128];
    const char *expected_output;
    int command_length;
    int exit_status;

    directory[0] = '\0';
    module_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    expected_output = "WORKER READY\n   1\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "SHOW-BANNER.so", module_path, sizeof(module_path)) != FT_SUCCESS)
    {
        test_cleanup_module_directory(directory, NULL, NULL, NULL);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "multi_module.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_module_directory(directory, module_path, NULL, NULL);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "multi_module.txt", output_path, sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_module_directory(directory, module_path, binary_path, NULL);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -m -free -o %s samples/cobol/multi_module_worker.cob", module_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile worker module into library directory\n");
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/multi_module_main.cob %s", binary_path, module_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should link multi-module binary using library directory module\n");
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "COB_LIBRARY_PATH=%s %s > %s", directory, binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: should capture exit status when running with COB_LIBRARY_PATH\n");
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: multi-module binary should exit successfully from library directory\n");
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: multi-module binary should emit banner and accumulator from library directory\n");
        test_cleanup_module_directory(directory, module_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_module_directory(directory, module_path, binary_path, output_path);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_multi_module_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_multi_module_fixtures_contain_expected_sections",
            test_cobol_transpiled_multi_module_fixtures_contain_expected_sections},
        {"cobol_transpiled_multi_module_main_matches_expected_text",
            test_cobol_transpiled_multi_module_main_matches_expected_text},
        {"cobol_transpiled_multi_module_worker_matches_expected_text",
            test_cobol_transpiled_multi_module_worker_matches_expected_text},
        {"cobol_transpiled_multi_module_executes", test_cobol_transpiled_multi_module_executes},
        {"cobol_transpiled_multi_module_exit_status", test_cobol_transpiled_multi_module_exit_status},
        {"cobol_transpiled_multi_module_compiles_separately",
            test_cobol_transpiled_multi_module_compiles_separately},
        {"cobol_transpiled_multi_module_executes_from_library_directory",
            test_cobol_transpiled_multi_module_executes_from_library_directory}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

