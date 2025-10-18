#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_banker_round_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-BANKER-ROUND.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-SCALE-POWER USAGE COMP-2 VALUE 1.\n"
        "       01 WS-SCALED USAGE COMP-2 VALUE 0.\n"
        "       01 WS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 WS-ABS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-REMAINDER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-HALF USAGE COMP-2 VALUE 0.5.\n"
        "       01 WS-TWO USAGE COMP-2 VALUE 2.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-SCALE PIC S9(4) COMP-5.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-SCALE BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-SCALE < 0 OR LNK-SCALE > 18\n"
        "               MOVE 2 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE WS-SCALE-POWER = 10 ** LNK-SCALE\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 2 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           COMPUTE WS-SCALED = LNK-OPERAND * WS-SCALE-POWER\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 2 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           COMPUTE WS-INTEGER = FUNCTION INTEGER-PART(WS-SCALED).\n"
        "           COMPUTE WS-FRACTION = FUNCTION ABS(WS-SCALED - WS-INTEGER).\n"
        "           MOVE WS-INTEGER TO WS-SCALED.\n"
        "           IF WS-FRACTION > 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF\n"
        "           IF WS-FRACTION > WS-HALF\n"
        "               IF LNK-OPERAND >= 0\n"
        "                   COMPUTE WS-SCALED = WS-INTEGER + 1\n"
        "               ELSE\n"
        "                   COMPUTE WS-SCALED = WS-INTEGER - 1\n"
        "               END-IF\n"
        "           ELSE\n"
        "               IF WS-FRACTION = WS-HALF\n"
        "                   COMPUTE WS-ABS-INTEGER = FUNCTION ABS(WS-INTEGER).\n"
        "                   COMPUTE WS-REMAINDER = FUNCTION MOD(WS-ABS-INTEGER, WS-TWO).\n"
        "                   IF WS-REMAINDER NOT = 0\n"
        "                       IF LNK-OPERAND >= 0\n"
        "                           COMPUTE WS-SCALED = WS-INTEGER + 1\n"
        "                       ELSE\n"
        "                           COMPUTE WS-SCALED = WS-INTEGER - 1\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = WS-SCALED / WS-SCALE-POWER\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 2 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-BANKER-ROUND.\n";
    if (test_expect_success(transpiler_standard_library_generate_banker_round(&program_text),
            "banker-round generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "banker-round generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_banker_round_executes_with_scale_support)
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
    library_path = "stdlib_banker_round_lib.cob";
    driver_path = "stdlib_banker_round_drv.cob";
    binary_path = "stdlib_banker_round.bin";
    output_path = "stdlib_banker_round.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. BANKER-ROUND-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-TIE-EVEN USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-TIE-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-INVALID USAGE COMP-2 VALUE 0.\n"
        "       01 SCALE-REQUEST PIC S9(4) COMP-5 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-TIE-EVEN USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-TIE-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-INVALID USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 STATUS-TIE-EVEN PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 STATUS-TIE-NEG PIC 9 VALUE 9.\n"
        "       01 STATUS-INTEGER PIC 9 VALUE 9.\n"
        "       01 STATUS-INVALID PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 5.755 TO OPERAND-POS.\n"
        "           MOVE 2 TO SCALE-REQUEST.\n"
        "           CALL 'CBLC-BANKER-ROUND' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE SCALE-REQUEST BY REFERENCE RESULT-POS\n"
        "               BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-POS.\n"
        "           MOVE 5.745 TO OPERAND-TIE-EVEN.\n"
        "           MOVE 2 TO SCALE-REQUEST.\n"
        "           CALL 'CBLC-BANKER-ROUND' USING BY REFERENCE OPERAND-TIE-EVEN\n"
        "               BY REFERENCE SCALE-REQUEST BY REFERENCE RESULT-TIE-EVEN\n"
        "               BY REFERENCE STATUS-TIE-EVEN.\n"
        "           MOVE RESULT-TIE-EVEN TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-TIE-EVEN.\n"
        "           MOVE -2.251 TO OPERAND-NEG.\n"
        "           MOVE 2 TO SCALE-REQUEST.\n"
        "           CALL 'CBLC-BANKER-ROUND' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE SCALE-REQUEST BY REFERENCE RESULT-NEG\n"
        "               BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           MOVE -2.255 TO OPERAND-TIE-NEG.\n"
        "           MOVE 2 TO SCALE-REQUEST.\n"
        "           CALL 'CBLC-BANKER-ROUND' USING BY REFERENCE OPERAND-TIE-NEG\n"
        "               BY REFERENCE SCALE-REQUEST BY REFERENCE RESULT-TIE-NEG\n"
        "               BY REFERENCE STATUS-TIE-NEG.\n"
        "           MOVE RESULT-TIE-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-TIE-NEG.\n"
        "           MOVE 4 TO OPERAND-INTEGER.\n"
        "           MOVE 3 TO SCALE-REQUEST.\n"
        "           CALL 'CBLC-BANKER-ROUND' USING BY REFERENCE OPERAND-INTEGER\n"
        "               BY REFERENCE SCALE-REQUEST BY REFERENCE RESULT-INTEGER\n"
        "               BY REFERENCE STATUS-INTEGER.\n"
        "           MOVE RESULT-INTEGER TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-INTEGER.\n"
        "           MOVE 19 TO SCALE-REQUEST.\n"
        "           CALL 'CBLC-BANKER-ROUND' USING BY REFERENCE OPERAND-INVALID\n"
        "               BY REFERENCE SCALE-REQUEST BY REFERENCE RESULT-INVALID\n"
        "               BY REFERENCE STATUS-INVALID.\n"
        "           MOVE RESULT-INVALID TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-INVALID.\n"
        "           STOP RUN.\n"
        "       END PROGRAM BANKER-ROUND-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_banker_round(&library_text),
            "banker-round generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 5.76, g_default_float_tolerance,
            "banker-round helper should round positive fraction up with odd tie") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "banker-round helper should flag fractional positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 5.74, g_default_float_tolerance,
            "banker-round helper should preserve even tie results") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "banker-round helper should report even tie adjustment") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -2.25, g_default_float_tolerance,
            "banker-round helper should round negative fractional operand toward nearest value") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "banker-round helper should mark negative operand as adjusted") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -2.26, g_default_float_tolerance,
            "banker-round helper should drive negative odd tie away from zero") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "banker-round helper should record negative odd tie adjustment") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 4.0, g_default_float_tolerance,
            "banker-round helper should preserve integral values at any scale") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "banker-round helper should report success for integral operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.0, g_default_float_tolerance,
            "banker-round helper should zero result when scale is invalid") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "2",
            "banker-round helper should surface invalid scale status") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "banker-round helper should only emit expected transcript lines") != FT_SUCCESS)
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

