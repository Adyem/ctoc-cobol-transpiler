#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_copy_file_tests(size_t *count);

FT_TEST(test_cobol_transpiled_copy_file_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/copy_file.cob";
    if (test_cobol_fixture_contains(path,
            "SELECT INPUT-FILE ASSIGN TO \"input.dat\".") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "SELECT OUTPUT-FILE ASSIGN TO \"output.dat\".") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "MOVE INPUT-RECORD TO OUTPUT-RECORD") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "WRITE OUTPUT-RECORD") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "STOP RUN.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_copy_file_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/copy_file.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. COPY-FILE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT INPUT-FILE ASSIGN TO \"input.dat\".\n"
        "           SELECT OUTPUT-FILE ASSIGN TO \"output.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  INPUT-FILE.\n"
        "       01  INPUT-RECORD PIC X(256).\n"
        "       FD  OUTPUT-FILE.\n"
        "       01  OUTPUT-RECORD PIC X(256).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN INPUT INPUT-FILE.\n"
        "           OPEN OUTPUT OUTPUT-FILE.\n"
        "           PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "               READ INPUT-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       MOVE INPUT-RECORD TO OUTPUT-RECORD\n"
        "                       WRITE OUTPUT-RECORD\n"
        "               END-READ\n"
        "           END-PERFORM.\n"
        "           CLOSE INPUT-FILE.\n"
        "           CLOSE OUTPUT-FILE.\n"
        "           STOP RUN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_copy_file_executes)
{
    char directory[256];
    char binary_path[256];
    char input_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[512];
    const char *input_contents;
    const char *expected_output;
    int command_length;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    input_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "copy_file_compile.log", compile_log_path, sizeof(compile_log_path))
        != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "copy_file.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "input.dat", input_path, sizeof(input_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "output.dat", output_path, sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    input_contents = "ALPHA\nBRAVO\n";
    if (test_write_text_file(input_path, input_contents) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/copy_file.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile copy_file sample\n");
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./copy_file.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: copy_file binary should execute successfully\n");
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    expected_output = "ALPHA\nBRAVO\n";
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: copy_file sample should copy all input lines\n");
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_copy_file_exit_status)
{
    char directory[256];
    char binary_path[256];
    char input_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    int command_length;
    int exit_status;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    input_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "copy_file_status_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "copy_file_status.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "input.dat", input_path, sizeof(input_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "output.dat", output_path, sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(input_path, "ONE\nTWO\n") != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -o %s samples/cobol/copy_file.cob > %s 2>&1", binary_path,
        compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile copy_file sample for status capture\n");
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./copy_file_status.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture copy_file exit status\n");
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: copy_file program should exit with status 0\n");
        test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(input_path, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_copy_file_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_copy_file_fixture_contains_expected_sections",
            test_cobol_transpiled_copy_file_fixture_contains_expected_sections},
        {"cobol_transpiled_copy_file_matches_expected_text",
            test_cobol_transpiled_copy_file_matches_expected_text},
        {"cobol_transpiled_copy_file_executes", test_cobol_transpiled_copy_file_executes},
        {"cobol_transpiled_copy_file_exit_status", test_cobol_transpiled_copy_file_exit_status}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

