#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_integration_showcase_tests(size_t *count);

FT_TEST(test_cobol_transpiled_integration_showcase_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/integration_showcase.cob";
    if (test_cobol_fixture_contains(path,
            "SELECT TRANSACTIONS ASSIGN TO \"transactions.dat\"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "ORGANIZATION IS LINE SEQUENTIAL.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "SELECT ACCEPTED-LOG ASSIGN TO \"accepted.txt\".") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "SELECT REJECTED-LOG ASSIGN TO \"rejected.txt\".") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "PERFORM PROCESS-TRANSACTIONS.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "WRITE ACCEPTED-RECORD") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "WRITE REJECTED-RECORD") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_integration_showcase_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/integration_showcase.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. INTEGRATION-SHOWCASE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT TRANSACTIONS ASSIGN TO \"transactions.dat\"\n"
        "               ORGANIZATION IS LINE SEQUENTIAL.\n"
        "           SELECT ACCEPTED-LOG ASSIGN TO \"accepted.txt\".\n"
        "           SELECT REJECTED-LOG ASSIGN TO \"rejected.txt\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  TRANSACTIONS.\n"
        "       01  TRANSACTION-RECORD.\n"
        "           05  TRANSACTION-STATUS PIC X.\n"
        "           05  TRANSACTION-AMOUNT PIC 9(6).\n"
        "       FD  ACCEPTED-LOG.\n"
        "       01  ACCEPTED-RECORD PIC X(32).\n"
        "       FD  REJECTED-LOG.\n"
        "       01  REJECTED-RECORD PIC X(32).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       01  ACCEPTED-COUNT PIC 9(4) VALUE 0.\n"
        "       01  REJECTED-COUNT PIC 9(4) VALUE 0.\n"
        "       01  TOTAL-AMOUNT PIC 9(7) VALUE 0.\n"
        "       01  ACCEPTED-MARKER PIC X(32) VALUE \"ACCEPTED ENTRY\".\n"
        "       01  REJECTED-MARKER PIC X(32) VALUE \"REJECTED ENTRY\".\n"
        "       01  DISPLAY-BUFFER PIC Z(7).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "       DISPLAY \"INTEGRATION SHOWCASE\".\n"
        "       PERFORM RESET-STATE.\n"
        "       PERFORM PROCESS-TRANSACTIONS.\n"
        "       MOVE ACCEPTED-COUNT TO DISPLAY-BUFFER.\n"
        "       DISPLAY DISPLAY-BUFFER.\n"
        "       MOVE REJECTED-COUNT TO DISPLAY-BUFFER.\n"
        "       DISPLAY DISPLAY-BUFFER.\n"
        "       MOVE TOTAL-AMOUNT TO DISPLAY-BUFFER.\n"
        "       DISPLAY DISPLAY-BUFFER.\n"
        "       STOP RUN.\n"
        "\n"
        "       RESET-STATE.\n"
        "       MOVE 0 TO ACCEPTED-COUNT.\n"
        "       MOVE 0 TO REJECTED-COUNT.\n"
        "       MOVE 0 TO TOTAL-AMOUNT.\n"
        "       MOVE \"ACCEPTED ENTRY\" TO ACCEPTED-MARKER.\n"
        "       MOVE \"REJECTED ENTRY\" TO REJECTED-MARKER.\n"
        "       MOVE 'N' TO EOF-FLAG.\n"
        "       EXIT.\n"
        "\n"
        "       PROCESS-TRANSACTIONS.\n"
        "       OPEN INPUT TRANSACTIONS.\n"
        "       OPEN OUTPUT ACCEPTED-LOG.\n"
        "       OPEN OUTPUT REJECTED-LOG.\n"
        "       PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "           READ TRANSACTIONS\n"
        "               AT END\n"
        "                   MOVE 'Y' TO EOF-FLAG\n"
        "               NOT AT END\n"
        "                   IF TRANSACTION-STATUS = \"A\"\n"
        "                       ADD 1 TO ACCEPTED-COUNT\n"
        "                       ADD TRANSACTION-AMOUNT TO TOTAL-AMOUNT\n"
        "                       MOVE ACCEPTED-MARKER TO ACCEPTED-RECORD\n"
        "                       WRITE ACCEPTED-RECORD\n"
        "                   ELSE\n"
        "                       ADD 1 TO REJECTED-COUNT\n"
        "                       MOVE REJECTED-MARKER TO REJECTED-RECORD\n"
        "                       WRITE REJECTED-RECORD\n"
        "                   END-IF\n"
        "           END-READ\n"
        "       END-PERFORM.\n"
        "       CLOSE TRANSACTIONS.\n"
        "       CLOSE ACCEPTED-LOG.\n"
        "       CLOSE REJECTED-LOG.\n"
        "       EXIT.\n"
        "       END PROGRAM INTEGRATION-SHOWCASE.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_integration_showcase_executes)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char data_path[256];
    char accepted_path[256];
    char rejected_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[256];
    char accepted_buffer[128];
    char rejected_buffer[128];
    const char *input_contents;
    const char *expected_output;
    const char *expected_accepted;
    const char *expected_rejected;
    const char *log_path;
    int command_length;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    data_path[0] = '\0';
    accepted_path[0] = '\0';
    rejected_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "INTEGRATION SHOWCASE\n      2\n      1\n    205\n";
    expected_accepted =
        "ACCEPTED ENTRY                  ACCEPTED ENTRY                  ";
    expected_rejected = "REJECTED ENTRY                  ";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "integration_showcase_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "integration_showcase.bin", binary_path, sizeof(binary_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "integration_showcase_output.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "transactions.dat", data_path, sizeof(data_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(output_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "accepted.txt", accepted_path, sizeof(accepted_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "rejected.txt", rejected_path, sizeof(rejected_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(accepted_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "A000120\nR000045\nA000085\n";
    if (test_write_text_file(data_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/integration_showcase.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile integration_showcase sample\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./integration_showcase.bin > integration_showcase_output.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: integration_showcase binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: integration_showcase output should include banner and totals\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(accepted_path, accepted_buffer, sizeof(accepted_buffer))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(accepted_buffer, expected_accepted,
            ft_strlen(expected_accepted) + 1) != 0)
    {
        pf_printf("Assertion failed: accepted log should record expected entries\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(rejected_path, rejected_buffer, sizeof(rejected_buffer))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(rejected_buffer, expected_rejected,
            ft_strlen(expected_rejected) + 1) != 0)
    {
        pf_printf("Assertion failed: rejected log should record expected entries\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_file(accepted_path);
        test_remove_file(rejected_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
    test_remove_file(accepted_path);
    test_remove_file(rejected_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_integration_showcase_exit_status)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char data_path[256];
    char compile_log_path[256];
    char command[512];
    const char *input_contents;
    const char *log_path;
    int command_length;
    int exit_status;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    data_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "integration_showcase_status_compile.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "integration_showcase_status.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "integration_showcase_status.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "transactions.dat", data_path, sizeof(data_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(output_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "A000120\nR000045\nA000085\n";
    if (test_write_text_file(data_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/integration_showcase.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile integration_showcase sample\n");
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
        "cd %s && ./integration_showcase_status.bin > integration_showcase_status.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture integration_showcase exit status\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: integration_showcase binary should exit successfully\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_integration_showcase_compile_logs_clean)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char data_path[256];
    char compile_log_path[256];
    char command[512];
    const char *input_contents;
    const char *log_path;
    int command_length;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    data_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "integration_showcase_logs_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "integration_showcase_logs.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "integration_showcase_logs.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "transactions.dat", data_path, sizeof(data_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(output_path, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "A000120\nR000045\nA000085\n";
    if (test_write_text_file(data_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/integration_showcase.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile integration_showcase sample\n");
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
        "cd %s && ./integration_showcase_logs.bin > integration_showcase_logs.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: integration_showcase binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(data_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_integration_showcase_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_integration_showcase_fixture_contains_expected_sections",
            test_cobol_transpiled_integration_showcase_fixture_contains_expected_sections},
        {"cobol_transpiled_integration_showcase_matches_expected_text",
            test_cobol_transpiled_integration_showcase_matches_expected_text},
        {"cobol_transpiled_integration_showcase_executes",
            test_cobol_transpiled_integration_showcase_executes},
        {"cobol_transpiled_integration_showcase_exit_status",
            test_cobol_transpiled_integration_showcase_exit_status},
        {"cobol_transpiled_integration_showcase_compile_logs_clean",
            test_cobol_transpiled_integration_showcase_compile_logs_clean}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
