#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_reverse_normalization_tests(size_t *count);

FT_TEST(test_cobol_transpiled_reverse_normalization_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/reverse_normalization.cob";
    if (test_cobol_fixture_contains(path, "PROGRAM-ID. NORMALIZATION-DEMO.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE 'mixED Case value' TO scratch-note.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE 0000 TO running-total-value.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE 'y' TO status-flag.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "MOVE 'done' TO scratch-note.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_reverse_normalization_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/reverse_normalization.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. NORMALIZATION-DEMO.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 running-total-value PIC 9(4) VALUE 0005.\n"
        "       01 status-flag PIC X VALUE 'n'.\n"
        "       01 scratch-note PIC X(12) VALUE 'raw value'.\n"
        "       PROCEDURE DIVISION.\n"
        "ENTRY-PARAGRAPH.\n"
        "       MOVE 'mixED Case value' TO scratch-note.\n"
        "       MOVE 0000 TO running-total-value.\n"
        "       MOVE 'y' TO status-flag.\n"
        "       STOP RUN.\n"
        "NORMALIZE-VALUES.\n"
        "       MOVE 0007 TO running-total-value.\n"
        "       MOVE 'done' TO scratch-note.\n"
        "       STOP RUN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_reverse_normalization_executes)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[64];
    int command_length;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_normalization_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_normalization.bin", binary_path, sizeof(binary_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_normalization.txt", output_path, sizeof(output_path))
        != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/reverse_normalization.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile reverse_normalization sample\n");
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
        "cd %s && ./reverse_normalization.bin > reverse_normalization.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: reverse_normalization binary should execute successfully\n");
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
    if (ft_strlen(output_buffer) != 0)
    {
        pf_printf("Assertion failed: reverse_normalization sample should not emit output\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_reverse_normalization_exit_status)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[64];
    int command_length;
    int exit_status;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "reverse_normalization_status_compile.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_normalization_status.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "reverse_normalization_status.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/reverse_normalization.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile reverse_normalization sample\n");
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
        "cd %s && ./reverse_normalization_status.bin > reverse_normalization_status.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture reverse_normalization exit status\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: reverse_normalization sample should exit successfully\n");
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
    if (ft_strlen(output_buffer) != 0)
    {
        pf_printf("Assertion failed: reverse_normalization sample should not emit output\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_reverse_normalization_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_reverse_normalization_fixture_contains_expected_sections",
            test_cobol_transpiled_reverse_normalization_fixture_contains_expected_sections},
        {"cobol_transpiled_reverse_normalization_matches_expected_text",
            test_cobol_transpiled_reverse_normalization_matches_expected_text},
        {"cobol_transpiled_reverse_normalization_executes",
            test_cobol_transpiled_reverse_normalization_executes},
        {"cobol_transpiled_reverse_normalization_exit_status",
            test_cobol_transpiled_reverse_normalization_exit_status}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
