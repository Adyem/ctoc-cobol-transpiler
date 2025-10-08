#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_record_writer_tests(size_t *count);

FT_TEST(test_cobol_transpiled_record_writer_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/record_writer.cob";
    if (test_cobol_fixture_contains(path,
            "SELECT REPORT-FILE ASSIGN TO \"report.dat\".") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE \"0001\" TO WORK-ID.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE \"INITIAL ENTRY\" TO WORK-NAME.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "WRITE REPORT-RECORD.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "ADD 1 TO REPORT-COUNT.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_record_writer_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/record_writer.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RECORD-WRITER.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT REPORT-FILE ASSIGN TO \"report.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  REPORT-FILE.\n"
        "       01  REPORT-RECORD.\n"
        "           05  REPORT-ID PIC X(4).\n"
        "           05  REPORT-NAME PIC X(24).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  REPORT-COUNT PIC 9(4) VALUE 0.\n"
        "       01  WORK-REPORT.\n"
        "           05  WORK-ID PIC X(4).\n"
        "           05  WORK-NAME PIC X(24).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN OUTPUT REPORT-FILE.\n"
        "           MOVE \"0001\" TO WORK-ID.\n"
        "           MOVE \"INITIAL ENTRY\" TO WORK-NAME.\n"
        "           MOVE WORK-REPORT TO REPORT-RECORD.\n"
        "           WRITE REPORT-RECORD.\n"
        "           ADD 1 TO REPORT-COUNT.\n"
        "           CLOSE REPORT-FILE.\n"
        "           STOP RUN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_record_writer_executes)
{
    char directory[256];
    char binary_path[256];
    char report_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[128];
    const char *expected_output;
    int command_length;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    report_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "record_writer_compile.log", compile_log_path, sizeof(compile_log_path))
        != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "record_writer.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/record_writer.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile record_writer sample\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./record_writer.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: record_writer binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "report.dat", report_path, sizeof(report_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(report_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    expected_output = "0001INITIAL ENTRY           ";
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: record_writer sample should emit formatted record\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_record_writer_exit_status)
{
    char directory[256];
    char binary_path[256];
    char report_path[256];
    char compile_log_path[256];
    char command[512];
    int command_length;
    int exit_status;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    report_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "record_writer_status_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "record_writer_status.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/record_writer.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile record_writer sample for status capture\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./record_writer_status.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture record_writer exit status\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: record_writer program should exit with status 0\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, report_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_record_writer_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_record_writer_fixture_contains_expected_sections",
            test_cobol_transpiled_record_writer_fixture_contains_expected_sections},
        {"cobol_transpiled_record_writer_matches_expected_text",
            test_cobol_transpiled_record_writer_matches_expected_text},
        {"cobol_transpiled_record_writer_executes",
            test_cobol_transpiled_record_writer_executes},
        {"cobol_transpiled_record_writer_exit_status",
            test_cobol_transpiled_record_writer_exit_status}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

