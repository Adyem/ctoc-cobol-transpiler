#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_fabs_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-FABS.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION ABS(LNK-OPERAND).\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-FABS.\n";
    if (test_expect_success(transpiler_standard_library_generate_fabs(&program_text),
            "fabs generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "fabs generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_fabs_executes_for_negative_and_positive_operands)
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
    library_path = "stdlib_fabs_lib.cob";
    driver_path = "stdlib_fabs_drv.cob";
    binary_path = "stdlib_fabs.bin";
    output_path = "stdlib_fabs.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. FABS-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 DISPLAY-NEG PIC 9(4).9(4).\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 DISPLAY-POS PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -12.5 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-FABS' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO DISPLAY-NEG.\n"
        "           DISPLAY DISPLAY-NEG.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           MOVE 3.25 TO OPERAND-POS.\n"
        "           CALL 'CBLC-FABS' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO DISPLAY-POS.\n"
        "           DISPLAY DISPLAY-POS.\n"
        "           DISPLAY STATUS-POS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM FABS-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_fabs(&library_text),
            "fabs generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 12.5, g_default_float_tolerance,
            "fabs helper should return magnitude for negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "fabs helper should report success for negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 3.25, g_default_float_tolerance,
            "fabs helper should return magnitude for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "fabs helper should report success for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "fabs helper should only emit expected transcript lines") != FT_SUCCESS)
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

