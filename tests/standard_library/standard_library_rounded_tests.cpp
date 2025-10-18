#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_rounded_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ROUNDED.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 WS-ABS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-REMAINDER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-HALF USAGE COMP-2 VALUE 0.5.\n"
        "       01 WS-TWO USAGE COMP-2 VALUE 2.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE WS-INTEGER = FUNCTION INTEGER-PART(LNK-OPERAND).\n"
        "           COMPUTE WS-FRACTION = FUNCTION ABS(LNK-OPERAND - WS-INTEGER).\n"
        "           MOVE WS-INTEGER TO LNK-RESULT.\n"
        "           IF WS-FRACTION > 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF\n"
        "           IF WS-FRACTION > WS-HALF\n"
        "               IF LNK-OPERAND >= 0\n"
        "                   COMPUTE LNK-RESULT = WS-INTEGER + 1\n"
        "               ELSE\n"
        "                   COMPUTE LNK-RESULT = WS-INTEGER - 1\n"
        "               END-IF\n"
        "           ELSE\n"
        "               IF WS-FRACTION = WS-HALF\n"
        "                   COMPUTE WS-ABS-INTEGER = FUNCTION ABS(WS-INTEGER).\n"
        "                   COMPUTE WS-REMAINDER = FUNCTION MOD(WS-ABS-INTEGER, WS-TWO).\n"
        "                   IF WS-REMAINDER NOT = 0\n"
        "                       IF LNK-OPERAND >= 0\n"
        "                           COMPUTE LNK-RESULT = WS-INTEGER + 1\n"
        "                       ELSE\n"
        "                           COMPUTE LNK-RESULT = WS-INTEGER - 1\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ROUNDED.\n";
    if (test_expect_success(transpiler_standard_library_generate_rounded(&program_text),
            "rounded generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "rounded generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_rounded_executes_for_common_cases)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[256];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_rounded_lib.cob";
    driver_path = "stdlib_rounded_drv.cob";
    binary_path = "stdlib_rounded.bin";
    output_path = "stdlib_rounded.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ROUNDED-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-INT USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-TIE-EVEN USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-TIE-ODD USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-TIE-NEG-ODD USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-INT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-TIE-EVEN USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-TIE-ODD USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-TIE-NEG-ODD USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 STATUS-INT PIC 9 VALUE 9.\n"
        "       01 STATUS-TIE-EVEN PIC 9 VALUE 9.\n"
        "       01 STATUS-TIE-ODD PIC 9 VALUE 9.\n"
        "       01 STATUS-TIE-NEG-ODD PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 5.75 TO OPERAND-POS.\n"
        "           CALL 'CBLC-ROUNDED' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-POS.\n"
        "           MOVE -2.25 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-ROUNDED' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           MOVE 4 TO OPERAND-INT.\n"
        "           CALL 'CBLC-ROUNDED' USING BY REFERENCE OPERAND-INT\n"
        "               BY REFERENCE RESULT-INT BY REFERENCE STATUS-INT.\n"
        "           MOVE RESULT-INT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-INT.\n"
        "           MOVE 2.5 TO OPERAND-TIE-EVEN.\n"
        "           CALL 'CBLC-ROUNDED' USING BY REFERENCE OPERAND-TIE-EVEN\n"
        "               BY REFERENCE RESULT-TIE-EVEN BY REFERENCE STATUS-TIE-EVEN.\n"
        "           MOVE RESULT-TIE-EVEN TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-TIE-EVEN.\n"
        "           MOVE 1.5 TO OPERAND-TIE-ODD.\n"
        "           CALL 'CBLC-ROUNDED' USING BY REFERENCE OPERAND-TIE-ODD\n"
        "               BY REFERENCE RESULT-TIE-ODD BY REFERENCE STATUS-TIE-ODD.\n"
        "           MOVE RESULT-TIE-ODD TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-TIE-ODD.\n"
        "           MOVE -1.5 TO OPERAND-TIE-NEG-ODD.\n"
        "           CALL 'CBLC-ROUNDED' USING BY REFERENCE OPERAND-TIE-NEG-ODD\n"
        "               BY REFERENCE RESULT-TIE-NEG-ODD BY REFERENCE STATUS-TIE-NEG-ODD.\n"
        "           MOVE RESULT-TIE-NEG-ODD TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-TIE-NEG-ODD.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ROUNDED-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_rounded(&library_text),
            "rounded generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 6.0, g_default_float_tolerance,
            "rounded helper should round positive fractional operands to nearest integer") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "rounded helper should report fractional positive operand as adjusted") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -2.0, g_default_float_tolerance,
            "rounded helper should round negative fractional operands to nearest integer") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "rounded helper should report fractional negative operand as adjusted") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 4.0, g_default_float_tolerance,
            "rounded helper should preserve integral operands") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "rounded helper should report success for integral operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 2.0, g_default_float_tolerance,
            "rounded helper should keep even tie results unchanged") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "rounded helper should mark tie-even operand as adjusted") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 2.0, g_default_float_tolerance,
            "rounded helper should round half to odd integer away from zero") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "rounded helper should mark positive tie-odd operand as adjusted") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -2.0, g_default_float_tolerance,
            "rounded helper should round negative half ties toward even magnitude") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "rounded helper should mark negative tie-odd operand as adjusted") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "rounded helper should only emit expected transcript lines") != FT_SUCCESS)
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

