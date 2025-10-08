#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_return_numeric_tests(size_t *count);

FT_TEST(test_cobol_transpiled_return_numeric_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/return_numeric.cob";
    if (test_cobol_fixture_contains(path,
            "CALL 'COMPUTE-SUM' USING BY REFERENCE ADDEND-A") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path,
            "COMPUTE LNK-SUM-RESULT = LNK-ADDEND-A + LNK-ADDEND-B.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "DISPLAY DISPLAY-BUFFER.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "STOP RUN.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "GOBACK.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_numeric_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/return_numeric.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RETURN-NUMERIC.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 ADDEND-A PIC 9(4) VALUE 0012.\n"
        "       01 ADDEND-B PIC 9(4) VALUE 0030.\n"
        "       01 SUM-RESULT PIC 9(5) VALUE 00000.\n"
        "       01 DISPLAY-BUFFER PIC Z(5).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       CALL 'COMPUTE-SUM' USING BY REFERENCE ADDEND-A\n"
        "           BY REFERENCE ADDEND-B BY REFERENCE SUM-RESULT.\n"
        "       MOVE SUM-RESULT TO DISPLAY-BUFFER.\n"
        "       DISPLAY DISPLAY-BUFFER.\n"
        "       STOP RUN.\n"
        "       END PROGRAM RETURN-NUMERIC.\n"
        "\n"
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. COMPUTE-SUM.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-ADDEND-A PIC 9(4).\n"
        "       01 LNK-ADDEND-B PIC 9(4).\n"
        "       01 LNK-SUM-RESULT PIC 9(5).\n"
        "       PROCEDURE DIVISION USING LNK-ADDEND-A LNK-ADDEND-B LNK-SUM-RESULT.\n"
        "       COMPUTE LNK-SUM-RESULT = LNK-ADDEND-A + LNK-ADDEND-B.\n"
        "       GOBACK.\n"
        "       END PROGRAM COMPUTE-SUM.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_return_numeric_executes)
{
    const char *binary_path;
    const char *output_path;
    char command[256];
    char output_buffer[128];
    const char *expected_output;
    int command_length;

    FT_REQUIRE_COBC();
    binary_path = "test_transpiled_return_numeric.bin";
    output_path = "test_transpiled_return_numeric.txt";
    expected_output = "   42\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_numeric.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled return_numeric program\n");
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
        pf_printf("Assertion failed: transpiled return_numeric program should execute successfully\n");
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
        pf_printf("Assertion failed: transpiled return_numeric program should emit computed sum\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_numeric_exit_status)
{
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[256];
    char output_buffer[128];
    int command_length;
    int exit_status;

    FT_REQUIRE_COBC();
    binary_path = "test_transpiled_return_numeric_status.bin";
    output_path = "test_transpiled_return_numeric_status.txt";
    expected_output = "   42\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_numeric.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled return_numeric program\n");
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
        pf_printf("Assertion failed: test harness should capture COBOL program exit status\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: transpiled return_numeric program should exit successfully\n");
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
        pf_printf("Assertion failed: transpiled return_numeric program should emit computed sum\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_numeric_compile_logs_clean)
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
    expected_output = "   42\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "return_numeric_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    log_path = compile_log_path;
    if (test_join_path(directory, "return_numeric_compile.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, NULL, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "return_numeric_compile.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, NULL, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_numeric.cob > %s 2>&1",
        binary_path, compile_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile return_numeric sample while capturing log\n");
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
        "cd %s && ./return_numeric_compile.bin > return_numeric_compile.txt",
        directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: return_numeric binary should execute after log validation\n");
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
        pf_printf("Assertion failed: return_numeric binary should emit computed sum after log validation\n");
        test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts_with_log(NULL, binary_path, output_path, log_path);
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_return_numeric_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_return_numeric_fixture_contains_expected_sections",
            test_cobol_transpiled_return_numeric_fixture_contains_expected_sections},
        {"cobol_transpiled_return_numeric_matches_expected_text",
            test_cobol_transpiled_return_numeric_matches_expected_text},
        {"cobol_transpiled_return_numeric_executes", test_cobol_transpiled_return_numeric_executes},
        {"cobol_transpiled_return_numeric_exit_status", test_cobol_transpiled_return_numeric_exit_status},
        {"cobol_transpiled_return_numeric_compile_logs_clean",
            test_cobol_transpiled_return_numeric_compile_logs_clean}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

