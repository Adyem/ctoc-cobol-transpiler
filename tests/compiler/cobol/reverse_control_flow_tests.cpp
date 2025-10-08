#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_reverse_control_flow_tests(size_t *count);

FT_TEST(test_cobol_transpiled_reverse_control_flow_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/reverse_control_flow.cob";
    if (test_cobol_fixture_contains(path, "PROGRAM-ID. REVERSE-CONTROL-FLOW.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "IF NOT CONTROL-FLAG = 'Y'") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "PERFORM UNTIL PROGRESS-METER > PROGRESS-LIMIT") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE 11 TO PROGRESS-METER") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE PROGRESS-METER TO OUTPUT-VALUE") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "PERFORM VARYING PROGRESS-INDEX FROM 0 BY 1 UNTIL PROGRESS-INDEX >= PROGRESS-LIMIT")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE PROGRESS-INDEX TO OUTPUT-VALUE") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "DISPLAY OUTPUT-VALUE.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE 'Y' TO CONTROL-FLAG.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_reverse_control_flow_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/reverse_control_flow.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. REVERSE-CONTROL-FLOW.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 CONTROL-FLAG PIC X VALUE 'N'.\n"
        "       01 PROGRESS-METER PIC 9(4) VALUE 0000.\n"
        "       01 PROGRESS-LIMIT PIC 9(4) VALUE 0010.\n"
        "       01 PROGRESS-INDEX PIC 9(4) VALUE 0000.\n"
        "       01 OUTPUT-VALUE PIC 9(4) VALUE 0000.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       IF NOT CONTROL-FLAG = 'Y'\n"
        "           PERFORM UNTIL PROGRESS-METER > PROGRESS-LIMIT\n"
        "               MOVE 11 TO PROGRESS-METER\n"
        "           END-PERFORM\n"
        "           MOVE PROGRESS-METER TO OUTPUT-VALUE\n"
        "       ELSE\n"
        "           PERFORM VARYING PROGRESS-INDEX FROM 0 BY 1 UNTIL PROGRESS-INDEX >= PROGRESS-LIMIT\n"
        "               MOVE PROGRESS-INDEX TO OUTPUT-VALUE\n"
        "           END-PERFORM\n"
        "       END-IF\n"
        "       DISPLAY OUTPUT-VALUE.\n"
        "       STOP RUN.\n"
        "NEXT-PARAGRAPH.\n"
        "       MOVE 'Y' TO CONTROL-FLAG.\n"
        "       STOP RUN.\n"
        "       END PROGRAM REVERSE-CONTROL-FLOW.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_reverse_control_flow_executes)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[64];
    const char *expected_output;
    const char *log_path;
    int command_length;

    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "0011\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_control_flow_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/reverse_control_flow.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile reverse_control_flow sample\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./reverse_control_flow.bin > reverse_control_flow.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: reverse_control_flow binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: reverse_control_flow sample should emit incremented count\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_reverse_control_flow_exit_status)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[64];
    const char *expected_output;
    const char *log_path;
    int command_length;
    int exit_status;

    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "0011\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_control_flow_status_compile.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow_status.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_status.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/reverse_control_flow.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile reverse_control_flow sample\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./reverse_control_flow_status.bin > reverse_control_flow_status.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture reverse_control_flow exit status\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: reverse_control_flow sample should exit successfully\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: reverse_control_flow sample should emit incremented count\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_reverse_control_flow_compile_logs_clean)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[64];
    const char *expected_output;
    const char *log_path;
    int command_length;

    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "0011\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_control_flow_logs_compile.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow_logs.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_control_flow_logs.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/reverse_control_flow.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile reverse_control_flow sample\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./reverse_control_flow_logs.bin > reverse_control_flow_logs.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: reverse_control_flow binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: reverse_control_flow sample should emit incremented count\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_reverse_control_flow_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_reverse_control_flow_fixture_contains_expected_sections",
            test_cobol_transpiled_reverse_control_flow_fixture_contains_expected_sections},
        {"cobol_transpiled_reverse_control_flow_matches_expected_text",
            test_cobol_transpiled_reverse_control_flow_matches_expected_text},
        {"cobol_transpiled_reverse_control_flow_executes",
            test_cobol_transpiled_reverse_control_flow_executes},
        {"cobol_transpiled_reverse_control_flow_exit_status",
            test_cobol_transpiled_reverse_control_flow_exit_status},
        {"cobol_transpiled_reverse_control_flow_compile_logs_clean",
            test_cobol_transpiled_reverse_control_flow_compile_logs_clean}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
