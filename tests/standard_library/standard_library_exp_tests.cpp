#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_exp_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-EXP.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION EXP(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-EXP.\n";
    if (test_expect_success(transpiler_standard_library_generate_exp(&program_text),
            "exp generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "exp generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_exp_executes_for_positive_and_negative_operands)
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
    library_path = "stdlib_exp_lib.cob";
    driver_path = "stdlib_exp_drv.cob";
    binary_path = "stdlib_exp.bin";
    output_path = "stdlib_exp.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. EXP-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 1 TO OPERAND-POS.\n"
        "           CALL 'CBLC-EXP' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-POS.\n"
        "           MOVE -1 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-EXP' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM EXP-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_exp(&library_text),
            "exp generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 2.718281828, g_default_float_tolerance,
            "exp helper should compute e raised to positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "exp helper should report success for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.367879441, g_default_float_tolerance,
            "exp helper should compute e raised to negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "exp helper should report success for negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "exp helper should only emit expected transcript lines for valid operands") != FT_SUCCESS)
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

FT_TEST(test_standard_library_exp_reports_overflow_for_large_operand)
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
    library_path = "stdlib_exp_overflow_lib.cob";
    driver_path = "stdlib_exp_overflow_drv.cob";
    binary_path = "stdlib_exp_overflow.bin";
    output_path = "stdlib_exp_overflow.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. EXP-OVERFLOW-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-LARGE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-LARGE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-LARGE PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 1000 TO OPERAND-LARGE.\n"
        "           CALL 'CBLC-EXP' USING BY REFERENCE OPERAND-LARGE\n"
        "               BY REFERENCE RESULT-LARGE BY REFERENCE STATUS-LARGE.\n"
        "           MOVE RESULT-LARGE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-LARGE.\n"
        "           STOP RUN.\n"
        "       END PROGRAM EXP-OVERFLOW-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_exp(&library_text),
            "exp generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "0000.0000\n1\n",
            "exp helper should detect overflow and report failure") != FT_SUCCESS)
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

