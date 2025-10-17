#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_tan_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TAN.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION TAN(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TAN.\n";
    if (test_expect_success(transpiler_standard_library_generate_tan(&program_text),
            "tan generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "tan generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tan_executes_for_representative_operands)
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
    library_path = "stdlib_tan_lib.cob";
    driver_path = "stdlib_tan_drv.cob";
    binary_path = "stdlib_tan.bin";
    output_path = "stdlib_tan.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. TAN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-ZERO PIC 9 VALUE 9.\n"
        "       01 STATUS-PI-QUARTER PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG-PI-QUARTER PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 0 TO OPERAND-ZERO.\n"
        "           CALL 'CBLC-TAN' USING BY REFERENCE OPERAND-ZERO\n"
        "               BY REFERENCE RESULT-ZERO BY REFERENCE STATUS-ZERO.\n"
        "           MOVE RESULT-ZERO TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-ZERO.\n"
        "           COMPUTE OPERAND-PI-QUARTER = FUNCTION PI / 4.\n"
        "           CALL 'CBLC-TAN' USING BY REFERENCE OPERAND-PI-QUARTER\n"
        "               BY REFERENCE RESULT-PI-QUARTER BY REFERENCE STATUS-PI-QUARTER.\n"
        "           MOVE RESULT-PI-QUARTER TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-PI-QUARTER.\n"
        "           COMPUTE OPERAND-NEG-PI-QUARTER = 0 - OPERAND-PI-QUARTER.\n"
        "           CALL 'CBLC-TAN' USING BY REFERENCE OPERAND-NEG-PI-QUARTER\n"
        "               BY REFERENCE RESULT-NEG-PI-QUARTER BY REFERENCE STATUS-NEG-PI-QUARTER.\n"
        "           MOVE RESULT-NEG-PI-QUARTER TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG-PI-QUARTER.\n"
        "           STOP RUN.\n"
        "       END PROGRAM TAN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_tan(&library_text),
            "tan generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.0, g_default_float_tolerance,
            "tan helper should evaluate tan(0)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "tan helper should report success for zero operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0, g_default_float_tolerance,
            "tan helper should evaluate tan(pi/4)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "tan helper should report success for pi/4 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -1.0, g_default_float_tolerance,
            "tan helper should evaluate tan(-pi/4)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "tan helper should report success for -pi/4 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "tan helper should only emit expected transcript lines") != FT_SUCCESS)
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

