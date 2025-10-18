#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_min_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-MIN.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-LEFT USAGE COMP-2.\n"
        "       01 LNK-RIGHT USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-LEFT\n"
        "           BY REFERENCE LNK-RIGHT BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION MIN(LNK-LEFT, LNK-RIGHT)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-MIN.\n";
    if (test_expect_success(transpiler_standard_library_generate_min(&program_text),
            "min generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "min generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_min_executes_for_mixed_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_min_mixed_lib.cob";
    driver_path = "stdlib_min_mixed_drv.cob";
    binary_path = "stdlib_min_mixed.bin";
    output_path = "stdlib_min_mixed.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MIN-MIXED-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-LEFT USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-RIGHT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC S9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 12.5 TO OPERAND-LEFT.\n"
        "           MOVE -2.25 TO OPERAND-RIGHT.\n"
        "           CALL 'CBLC-MIN' USING BY REFERENCE OPERAND-LEFT\n"
        "               BY REFERENCE OPERAND-RIGHT BY REFERENCE RESULT-FLOAT\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM MIN-MIXED-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_min(&library_text),
            "min generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -2.25, g_default_float_tolerance,
            "min helper should return smaller operand for mixed inputs") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "min helper should report success for mixed operands") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "min helper should only emit expected transcript lines for mixed operands") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_min_executes_for_equal_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_min_equal_lib.cob";
    driver_path = "stdlib_min_equal_drv.cob";
    binary_path = "stdlib_min_equal.bin";
    output_path = "stdlib_min_equal.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MIN-EQUAL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-LEFT USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-RIGHT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC S9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 3.75 TO OPERAND-LEFT.\n"
        "           MOVE 3.75 TO OPERAND-RIGHT.\n"
        "           CALL 'CBLC-MIN' USING BY REFERENCE OPERAND-LEFT\n"
        "               BY REFERENCE OPERAND-RIGHT BY REFERENCE RESULT-FLOAT\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM MIN-EQUAL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_min(&library_text),
            "min generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 3.75, g_default_float_tolerance,
            "min helper should return shared operand when both inputs match") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "min helper should report success for equal operands") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "min helper should only emit expected transcript lines for equal operands") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
