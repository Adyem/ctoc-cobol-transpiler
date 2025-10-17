#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_log_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-LOG.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-OPERAND <= 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION LOG(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-LOG.\n";
    if (test_expect_success(transpiler_standard_library_generate_log(&program_text),
            "log generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "log generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_log_executes_for_positive_operands)
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
    library_path = "stdlib_log_lib.cob";
    driver_path = "stdlib_log_drv.cob";
    binary_path = "stdlib_log.bin";
    output_path = "stdlib_log.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. LOG-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-E USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-E USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-E PIC 9 VALUE 9.\n"
        "       01 STATUS-FRACTION PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 2.718281828 TO OPERAND-E.\n"
        "           CALL 'CBLC-LOG' USING BY REFERENCE OPERAND-E\n"
        "               BY REFERENCE RESULT-E BY REFERENCE STATUS-E.\n"
        "           MOVE RESULT-E TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-E.\n"
        "           MOVE 0.5 TO OPERAND-FRACTION.\n"
        "           CALL 'CBLC-LOG' USING BY REFERENCE OPERAND-FRACTION\n"
        "               BY REFERENCE RESULT-FRACTION BY REFERENCE STATUS-FRACTION.\n"
        "           MOVE RESULT-FRACTION TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FRACTION.\n"
        "           STOP RUN.\n"
        "       END PROGRAM LOG-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_log(&library_text),
            "log generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0, g_default_float_tolerance,
            "log helper should evaluate ln(e)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "log helper should report success for e operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -0.6931471805599453, g_default_float_tolerance,
            "log helper should evaluate ln(0.5)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "log helper should report success for fractional operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "log helper should only emit expected transcript lines for valid operands") != FT_SUCCESS)
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

FT_TEST(test_standard_library_log_rejects_non_positive_operand)
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
    library_path = "stdlib_log_invalid_lib.cob";
    driver_path = "stdlib_log_invalid_drv.cob";
    binary_path = "stdlib_log_invalid.bin";
    output_path = "stdlib_log_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. LOG-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-NONPOSITIVE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NONPOSITIVE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 0 TO OPERAND-NONPOSITIVE.\n"
        "           CALL 'CBLC-LOG' USING BY REFERENCE OPERAND-NONPOSITIVE\n"
        "               BY REFERENCE RESULT-NONPOSITIVE BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-NONPOSITIVE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM LOG-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_log(&library_text),
            "log generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, " 0000.0000\n1\n",
            "log helper should reject non-positive operands and zero the result") != FT_SUCCESS)
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

