#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_return_boolean_tests(size_t *count);

FT_TEST(test_cobol_transpiled_return_boolean_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/return_boolean.cob";
    if (test_cobol_fixture_contains(path, "PROGRAM-ID. RETURN-BOOLEAN.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "CALL 'IS-EVEN' USING BY REFERENCE CANDIDATE-VALUE") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "DISPLAY \"EVEN\"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "DISPLAY \"ODD\"") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "GOBACK.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_boolean_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/return_boolean.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RETURN-BOOLEAN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 CANDIDATE-VALUE PIC 9(4) VALUE 0009.\n"
        "       01 REMAINDER-VALUE PIC 9(1) VALUE 0.\n"
        "       01 IS-EVEN-RESULT PIC X VALUE 'N'.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       CALL 'IS-EVEN' USING BY REFERENCE CANDIDATE-VALUE\n"
        "           BY REFERENCE REMAINDER-VALUE BY REFERENCE IS-EVEN-RESULT.\n"
        "       IF IS-EVEN-RESULT == 'Y'\n"
        "           DISPLAY \"EVEN\"\n"
        "       ELSE\n"
        "           DISPLAY \"ODD\"\n"
        "       END-IF.\n"
        "       STOP RUN.\n"
        "       END PROGRAM RETURN-BOOLEAN.\n"
        "\n"
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. IS-EVEN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WORK-QUOTIENT PIC 9(4).\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-CANDIDATE-VALUE PIC 9(4).\n"
        "       01 LNK-REMAINDER-VALUE PIC 9(1).\n"
        "       01 LNK-IS-EVEN-RESULT PIC X.\n"
        "       PROCEDURE DIVISION USING LNK-CANDIDATE-VALUE LNK-REMAINDER-VALUE\n"
        "           LNK-IS-EVEN-RESULT.\n"
        "       DIVIDE LNK-CANDIDATE-VALUE BY 2 GIVING WORK-QUOTIENT\n"
        "           REMAINDER LNK-REMAINDER-VALUE.\n"
        "       IF LNK-REMAINDER-VALUE == 0\n"
        "           MOVE 'Y' TO LNK-IS-EVEN-RESULT\n"
        "       ELSE\n"
        "           MOVE 'N' TO LNK-IS-EVEN-RESULT\n"
        "       END-IF.\n"
        "       GOBACK.\n"
        "       END PROGRAM IS-EVEN.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_return_boolean_executes)
{
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[256];
    char output_buffer[128];
    int command_length;

    FT_REQUIRE_COBC();
    binary_path = "test_transpiled_return_boolean.bin";
    output_path = "test_transpiled_return_boolean.txt";
    expected_output = "ODD\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_boolean.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled return_boolean program\n");
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
        pf_printf("Assertion failed: transpiled return_boolean program should execute successfully\n");
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
        pf_printf("Assertion failed: transpiled return_boolean program should emit parity label\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_boolean_exit_status)
{
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[256];
    char output_buffer[128];
    int command_length;
    int exit_status;

    FT_REQUIRE_COBC();
    binary_path = "test_transpiled_return_boolean_status.bin";
    output_path = "test_transpiled_return_boolean_status.txt";
    expected_output = "ODD\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_boolean.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled return_boolean program\n");
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
        pf_printf("Assertion failed: test harness should capture return_boolean COBOL exit status\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: transpiled return_boolean program should exit successfully\n");
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
        pf_printf("Assertion failed: transpiled return_boolean program should emit parity label\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_boolean_compile_logs_clean)
{
    char directory[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char command[512];
    char output_buffer[128];
    const char *expected_output;
    int command_length;
    const char *log_path;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    expected_output = "ODD\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "return_boolean_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "return_boolean_compile.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "return_boolean_compile.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_boolean.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile return_boolean sample while capturing log\n");
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
        "cd %s && ./return_boolean_compile.bin > return_boolean_compile.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: return_boolean binary should execute after log validation\n");
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
        pf_printf("Assertion failed: return_boolean binary should emit parity label after log validation\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_return_boolean_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_return_boolean_fixture_contains_expected_sections",
            test_cobol_transpiled_return_boolean_fixture_contains_expected_sections},
        {"cobol_transpiled_return_boolean_matches_expected_text",
            test_cobol_transpiled_return_boolean_matches_expected_text},
        {"cobol_transpiled_return_boolean_executes", test_cobol_transpiled_return_boolean_executes},
        {"cobol_transpiled_return_boolean_exit_status", test_cobol_transpiled_return_boolean_exit_status},
        {"cobol_transpiled_return_boolean_compile_logs_clean",
            test_cobol_transpiled_return_boolean_compile_logs_clean}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

