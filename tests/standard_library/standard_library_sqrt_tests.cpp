#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_sqrt_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-SQRT.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-OPERAND < 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION SQRT(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-SQRT.\n";
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&program_text),
            "sqrt generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "sqrt generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_sqrt_executes_for_positive_operand)
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
    library_path = "stdlib_sqrt_pos_lib.cob";
    driver_path = "stdlib_sqrt_pos_drv.cob";
    binary_path = "stdlib_sqrt_pos.bin";
    output_path = "stdlib_sqrt_pos.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SQRT-POSITIVE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 49 TO OPERAND-FLOAT.\n"
        "           CALL 'CBLC-SQRT' USING BY REFERENCE OPERAND-FLOAT\n"
        "               BY REFERENCE RESULT-FLOAT BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SQRT-POSITIVE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&library_text),
            "sqrt generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 7.0, g_default_float_tolerance,
            "sqrt helper should compute square root of positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "sqrt helper should report success for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "sqrt helper should only emit expected transcript lines for positive operand") != FT_SUCCESS)
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

FT_TEST(test_standard_library_sqrt_rejects_negative_operand)
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
    library_path = "stdlib_sqrt_neg_lib.cob";
    driver_path = "stdlib_sqrt_neg_drv.cob";
    binary_path = "stdlib_sqrt_neg.bin";
    output_path = "stdlib_sqrt_neg.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SQRT-NEGATIVE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -4 TO OPERAND-FLOAT.\n"
        "           CALL 'CBLC-SQRT' USING BY REFERENCE OPERAND-FLOAT\n"
        "               BY REFERENCE RESULT-FLOAT BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SQRT-NEGATIVE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&library_text),
            "sqrt generator should succeed") != FT_SUCCESS)
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
            "sqrt helper should reject negative operands and zero the result") != FT_SUCCESS)
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

