#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_record_summary_tests(size_t *count);

FT_TEST(test_cobol_transpiled_record_summary_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/record_summary.cob";
    if (test_cobol_fixture_contains(path,
            "SELECT INPUT-FILE ASSIGN TO \"records.dat\"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "READ INPUT-FILE") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "IF RECORD-STATUS = \"A\"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "ADD RECORD-AMOUNT TO TOTAL-AMOUNT") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "STOP RUN.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_record_summary_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/record_summary.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RECORD-SUMMARY.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT INPUT-FILE ASSIGN TO \"records.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  INPUT-FILE.\n"
        "       01  INPUT-RECORD.\n"
        "           05  RECORD-STATUS PIC X.\n"
        "           05  RECORD-AMOUNT PIC 9(6).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       01  TOTAL-AMOUNT PIC 9(7) VALUE 0.\n"
        "       01  ACCEPTED-COUNT PIC 9(4) VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN INPUT INPUT-FILE.\n"
        "           PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "               READ INPUT-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       IF RECORD-STATUS = \"A\"\n"
        "                           ADD 1 TO ACCEPTED-COUNT\n"
        "                           ADD RECORD-AMOUNT TO TOTAL-AMOUNT\n"
        "                       END-IF\n"
        "               END-READ\n"
        "           END-PERFORM.\n"
        "           CLOSE INPUT-FILE.\n"
        "           STOP RUN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_record_summary_executes)
{
    char directory[256];
    char binary_path[256];
    char data_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[8];
    const char *input_contents;
    int command_length;
    const char *log_path;

    directory[0] = '\0';
    binary_path[0] = '\0';
    data_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "record_summary_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "record_summary.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "records.dat", data_path, sizeof(data_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "record_summary_output.txt", output_path, sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "A000150\nR000200\nA000300\n";
    if (test_write_text_file(data_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/record_summary.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile record_summary sample\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./record_summary.bin > record_summary_output.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: record_summary binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (output_buffer[0] != '\0')
    {
        pf_printf("Assertion failed: record_summary sample should not emit unexpected output\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_record_summary_exit_status)
{
    char directory[256];
    char binary_path[256];
    char data_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    int command_length;
    int exit_status;
    const char *log_path;

    directory[0] = '\0';
    binary_path[0] = '\0';
    data_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "record_summary_status_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "record_summary_status.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "records.dat", data_path, sizeof(data_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "record_summary_status_output.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(data_path, "A000150\nA000200\n") != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/record_summary.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile record_summary sample\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./record_summary_status.bin > record_summary_status_output.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture record_summary exit status\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: record_summary sample should exit successfully\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

static const t_test_case g_record_summary_tests[] = {
    {"cobol_transpiled_record_summary_fixture_contains_expected_sections",
        test_cobol_transpiled_record_summary_fixture_contains_expected_sections},
    {"cobol_transpiled_record_summary_matches_expected_text",
        test_cobol_transpiled_record_summary_matches_expected_text},
    {"cobol_transpiled_record_summary_executes",
        test_cobol_transpiled_record_summary_executes},
    {"cobol_transpiled_record_summary_exit_status",
        test_cobol_transpiled_record_summary_exit_status},
};

const t_test_case *get_compiler_cobol_record_summary_tests(size_t *count)
{
    if (count)
        *count = sizeof(g_record_summary_tests) / sizeof(g_record_summary_tests[0]);
    return (g_record_summary_tests);
}
