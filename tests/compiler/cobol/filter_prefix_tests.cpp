#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_filter_prefix_tests(size_t *count);

FT_TEST(test_cobol_transpiled_filter_prefix_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/filter_prefix.cob";
    if (test_cobol_fixture_contains(path,
            "SELECT SOURCE-FILE ASSIGN TO \"source.dat\"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "ORGANIZATION IS LINE SEQUENTIAL.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "IF SOURCE-LINE(1:5) = PREFIX(1:5)") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "WRITE TARGET-RECORD") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_filter_prefix_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/filter_prefix.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. FILTER-PREFIX.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT SOURCE-FILE ASSIGN TO \"source.dat\"\n"
        "               ORGANIZATION IS LINE SEQUENTIAL.\n"
        "           SELECT TARGET-FILE ASSIGN TO \"target.dat\"\n"
        "               ORGANIZATION IS LINE SEQUENTIAL.\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  SOURCE-FILE.\n"
        "       01  SOURCE-RECORD.\n"
        "           05  SOURCE-LINE PIC X(256).\n"
        "       FD  TARGET-FILE.\n"
        "       01  TARGET-RECORD.\n"
        "           05  TARGET-LINE PIC X(256).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  PREFIX PIC X(8) VALUE \"ALLOW\".\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN INPUT SOURCE-FILE.\n"
        "           OPEN OUTPUT TARGET-FILE.\n"
        "           PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "               READ SOURCE-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       IF SOURCE-LINE(1:5) = PREFIX(1:5)\n"
        "                           MOVE SOURCE-RECORD TO TARGET-RECORD\n"
        "                           WRITE TARGET-RECORD\n"
        "                       END-IF\n"
        "               END-READ\n"
        "           END-PERFORM.\n"
        "           CLOSE SOURCE-FILE.\n"
        "           CLOSE TARGET-FILE.\n"
        "           STOP RUN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_filter_prefix_executes)
{
    char directory[256];
    char binary_path[256];
    char source_path[256];
    char target_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[512];
    const char *input_contents;
    const char *expected_output;
    int command_length;
    const char *log_path;

    directory[0] = '\0';
    binary_path[0] = '\0';
    source_path[0] = '\0';
    target_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "filter_prefix_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "filter_prefix.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "source.dat", source_path, sizeof(source_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "target.dat", target_path, sizeof(target_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "ALLOW-FIRST\nDENY-ENTRY\nALLOW-SECOND\nDISCARD\n";
    if (test_write_text_file(source_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/filter_prefix.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile filter_prefix sample\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./filter_prefix.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: filter_prefix binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(target_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    expected_output = "ALLOW-FIRST\nALLOW-SECOND\n";
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: filter_prefix sample should emit only allowed lines\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_filter_prefix_exit_status)
{
    char directory[256];
    char binary_path[256];
    char source_path[256];
    char target_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[512];
    const char *input_contents;
    const char *expected_output;
    int command_length;
    int exit_status;
    const char *log_path;

    directory[0] = '\0';
    binary_path[0] = '\0';
    source_path[0] = '\0';
    target_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "filter_prefix_status_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "filter_prefix_status.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "source.dat", source_path, sizeof(source_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "target.dat", target_path, sizeof(target_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "ALLOW-FIRST\nDENY-ENTRY\nALLOW-SECOND\nDISCARD\n";
    if (test_write_text_file(source_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/filter_prefix.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile filter_prefix sample for status check\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./filter_prefix_status.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: harness should capture filter_prefix exit status\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: filter_prefix binary should exit successfully\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(target_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    expected_output = "ALLOW-FIRST\nALLOW-SECOND\n";
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: filter_prefix binary should emit only allowed lines\n");
        test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(source_path, binary_path, target_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_filter_prefix_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_filter_prefix_fixture_contains_expected_sections",
            test_cobol_transpiled_filter_prefix_fixture_contains_expected_sections},
        {"cobol_transpiled_filter_prefix_matches_expected_text",
            test_cobol_transpiled_filter_prefix_matches_expected_text},
        {"cobol_transpiled_filter_prefix_executes", test_cobol_transpiled_filter_prefix_executes},
        {"cobol_transpiled_filter_prefix_exit_status", test_cobol_transpiled_filter_prefix_exit_status}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

