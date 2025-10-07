#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_cobol_return_character_tests(size_t *count);

FT_TEST(test_cobol_transpiled_return_character_fixture_contains_expected_sections)
{
    const char *path;

    path = "samples/cobol/return_character.cob";
    if (test_cobol_fixture_contains(path, "PROGRAM-ID. RETURN-CHARACTER.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "CALL 'FETCH-GRADE' USING BY REFERENCE CURRENT-GRADE.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "DISPLAY CURRENT-GRADE.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "PROGRAM-ID. FETCH-GRADE.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_cobol_fixture_contains(path, "GOBACK.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_character_matches_expected_text)
{
    const char *path;
    const char *expected;

    path = "samples/cobol/return_character.cob";
    expected =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RETURN-CHARACTER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 CURRENT-GRADE PIC X VALUE 'C'.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       MOVE 'A' TO CURRENT-GRADE.\n"
        "       CALL 'FETCH-GRADE' USING BY REFERENCE CURRENT-GRADE.\n"
        "       DISPLAY CURRENT-GRADE.\n"
        "       STOP RUN.\n"
        "       END PROGRAM RETURN-CHARACTER.\n"
        "\n"
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. FETCH-GRADE.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 STORED-GRADE PIC X VALUE 'A'.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-CURRENT-GRADE PIC X.\n"
        "       PROCEDURE DIVISION USING LNK-CURRENT-GRADE.\n"
        "       MOVE STORED-GRADE TO LNK-CURRENT-GRADE.\n"
        "       GOBACK.\n"
        "       END PROGRAM FETCH-GRADE.\n";
    return (test_expect_file_equals(path, expected));
}

FT_TEST(test_cobol_transpiled_return_character_executes)
{
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[256];
    char output_buffer[128];
    int command_length;

    binary_path = "test_transpiled_return_character.bin";
    output_path = "test_transpiled_return_character.txt";
    expected_output = "A\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_character.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled return_character program\n");
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
        pf_printf("Assertion failed: transpiled return_character program should execute successfully\n");
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
        pf_printf("Assertion failed: transpiled return_character program should emit fetched grade\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_transpiled_return_character_exit_status)
{
    const char *binary_path;
    const char *output_path;
    const char *expected_output;
    char command[256];
    char output_buffer[128];
    int command_length;
    int exit_status;

    binary_path = "test_transpiled_return_character_status.bin";
    output_path = "test_transpiled_return_character_status.txt";
    expected_output = "A\n";
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s samples/cobol/return_character.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile transpiled return_character program\n");
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
        pf_printf("Assertion failed: test harness should capture return_character COBOL exit status\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    if (exit_status != 0)
    {
        pf_printf("Assertion failed: transpiled return_character program should exit successfully\n");
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
        pf_printf("Assertion failed: transpiled return_character program should emit fetched grade\n");
        test_cleanup_generated_artifacts(binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, output_path);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_cobol_return_character_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_transpiled_return_character_fixture_contains_expected_sections",
            test_cobol_transpiled_return_character_fixture_contains_expected_sections},
        {"cobol_transpiled_return_character_matches_expected_text",
            test_cobol_transpiled_return_character_matches_expected_text},
        {"cobol_transpiled_return_character_executes", test_cobol_transpiled_return_character_executes},
        {"cobol_transpiled_return_character_exit_status", test_cobol_transpiled_return_character_exit_status}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

