#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_strtod_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRTOD.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 NORMALIZED-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 NORMALIZED-BUFFER PIC X(255) VALUE SPACES.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 HAS-ANY-DIGIT PIC 9 VALUE 0.\n"
        "       01 HAS-DECIMAL PIC 9 VALUE 0.\n"
        "       01 HAS-EXPONENT PIC 9 VALUE 0.\n"
        "       01 EXPONENT-DIGITS PIC 9(9) VALUE 000000000.\n"
        "       01 EXPECT-EXPONENT-SIGN PIC 9 VALUE 0.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO ACTUAL-LENGTH.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE IDX TO ACTUAL-LENGTH\n"
        "           END-PERFORM.\n"
        "           IF ACTUAL-LENGTH = 0\n"
        "               MOVE SCAN-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE 1 TO START-INDEX.\n"
        "           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(START-INDEX:1) NOT = SPACE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACTUAL-LENGTH TO END-INDEX.\n"
        "           PERFORM UNTIL END-INDEX < START-INDEX\n"
        "               MOVE LNK-SOURCE(END-INDEX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR NOT = SPACE AND CURRENT-CHAR NOT = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               COMPUTE END-INDEX = END-INDEX - 1\n"
        "           END-PERFORM.\n"
        "           IF END-INDEX < START-INDEX\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO NORMALIZED-LENGTH.\n"
        "           MOVE SPACES TO NORMALIZED-BUFFER.\n"
        "           MOVE 0 TO HAS-ANY-DIGIT.\n"
        "           MOVE 0 TO HAS-DECIMAL.\n"
        "           MOVE 0 TO HAS-EXPONENT.\n"
        "           MOVE 0 TO EXPONENT-DIGITS.\n"
        "           MOVE 0 TO EXPECT-EXPONENT-SIGN.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > END-INDEX\n"
        "               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE OR CURRENT-CHAR = LOW-VALUE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               EVALUATE TRUE\n"
        "                   WHEN CURRENT-CHAR >= \"0\" AND CURRENT-CHAR <= \"9\"\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-ANY-DIGIT\n"
        "                       IF HAS-EXPONENT = 1\n"
        "                           ADD 1 TO EXPONENT-DIGITS\n"
        "                       END-IF\n"
        "                       MOVE 0 TO EXPECT-EXPONENT-SIGN\n"
        "                   WHEN CURRENT-CHAR = \".\"\n"
        "                       IF HAS-DECIMAL = 1 OR HAS-EXPONENT = 1\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE \".\" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-DECIMAL\n"
        "                   WHEN CURRENT-CHAR = \"E\" OR CURRENT-CHAR = \"e\"\n"
        "                       IF HAS-EXPONENT = 1 OR HAS-ANY-DIGIT = 0\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE \"E\" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-EXPONENT\n"
        "                       MOVE 0 TO EXPONENT-DIGITS\n"
        "                       MOVE 1 TO EXPECT-EXPONENT-SIGN\n"
        "                   WHEN CURRENT-CHAR = \"+\" OR CURRENT-CHAR = \"-\"\n"
        "                       IF NORMALIZED-LENGTH = 0\n"
        "                           IF NORMALIZED-LENGTH >= 255\n"
        "                               MOVE 1 TO LNK-STATUS\n"
        "                               MOVE 0 TO LNK-RESULT\n"
        "                               GOBACK\n"
        "                           END-IF\n"
        "                           ADD 1 TO NORMALIZED-LENGTH\n"
        "                           MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       ELSE\n"
        "                           IF EXPECT-EXPONENT-SIGN = 1\n"
        "                               IF NORMALIZED-LENGTH >= 255\n"
        "                                   MOVE 1 TO LNK-STATUS\n"
        "                                   MOVE 0 TO LNK-RESULT\n"
        "                                   GOBACK\n"
        "                               END-IF\n"
        "                               ADD 1 TO NORMALIZED-LENGTH\n"
        "                               MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                               MOVE 0 TO EXPECT-EXPONENT-SIGN\n"
        "                           ELSE\n"
        "                               MOVE 1 TO LNK-STATUS\n"
        "                               MOVE 0 TO LNK-RESULT\n"
        "                               GOBACK\n"
        "                           END-IF\n"
        "                       END-IF\n"
        "                   WHEN OTHER\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       MOVE 0 TO LNK-RESULT\n"
        "                       GOBACK\n"
        "               END-EVALUATE\n"
        "           END-PERFORM.\n"
        "           IF NORMALIZED-LENGTH = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF HAS-ANY-DIGIT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF HAS-EXPONENT = 1 AND EXPONENT-DIGITS = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF EXPECT-EXPONENT-SIGN = 1\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION NUMVAL(NORMALIZED-BUFFER(1:NORMALIZED-LENGTH))\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRTOD.\n";
    if (test_expect_success(transpiler_standard_library_generate_strtod(&program_text),
            "strtod generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strtod generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strtod_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRTOD-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 NORMALIZED-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 NORMALIZED-BUFFER PIC X(255) VALUE SPACES.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 HAS-ANY-DIGIT PIC 9 VALUE 0.\n"
        "       01 HAS-DECIMAL PIC 9 VALUE 0.\n"
        "       01 HAS-EXPONENT PIC 9 VALUE 0.\n"
        "       01 EXPONENT-DIGITS PIC 9(9) VALUE 000000000.\n"
        "       01 EXPECT-EXPONENT-SIGN PIC 9 VALUE 0.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-SOURCE-LEN TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO ACTUAL-LENGTH.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE-BUF(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE IDX TO ACTUAL-LENGTH\n"
        "           END-PERFORM.\n"
        "           IF ACTUAL-LENGTH = 0\n"
        "               MOVE SCAN-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE 1 TO START-INDEX.\n"
        "           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE-BUF(START-INDEX:1) NOT = SPACE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACTUAL-LENGTH TO END-INDEX.\n"
        "           PERFORM UNTIL END-INDEX < START-INDEX\n"
        "               MOVE LNK-SOURCE-BUF(END-INDEX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR NOT = SPACE AND CURRENT-CHAR NOT = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               COMPUTE END-INDEX = END-INDEX - 1\n"
        "           END-PERFORM.\n"
        "           IF END-INDEX < START-INDEX\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO NORMALIZED-LENGTH.\n"
        "           MOVE SPACES TO NORMALIZED-BUFFER.\n"
        "           MOVE 0 TO HAS-ANY-DIGIT.\n"
        "           MOVE 0 TO HAS-DECIMAL.\n"
        "           MOVE 0 TO HAS-EXPONENT.\n"
        "           MOVE 0 TO EXPONENT-DIGITS.\n"
        "           MOVE 0 TO EXPECT-EXPONENT-SIGN.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > END-INDEX\n"
        "               MOVE LNK-SOURCE-BUF(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE OR CURRENT-CHAR = LOW-VALUE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               EVALUATE TRUE\n"
        "                   WHEN CURRENT-CHAR >= \"0\" AND CURRENT-CHAR <= \"9\"\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-ANY-DIGIT\n"
        "                       IF HAS-EXPONENT = 1\n"
        "                           ADD 1 TO EXPONENT-DIGITS\n"
        "                       END-IF\n"
        "                       MOVE 0 TO EXPECT-EXPONENT-SIGN\n"
        "                   WHEN CURRENT-CHAR = \".\"\n"
        "                       IF HAS-DECIMAL = 1 OR HAS-EXPONENT = 1\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE \".\" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-DECIMAL\n"
        "                   WHEN CURRENT-CHAR = \"E\" OR CURRENT-CHAR = \"e\"\n"
        "                       IF HAS-EXPONENT = 1 OR HAS-ANY-DIGIT = 0\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE \"E\" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-EXPONENT\n"
        "                       MOVE 0 TO EXPONENT-DIGITS\n"
        "                       MOVE 1 TO EXPECT-EXPONENT-SIGN\n"
        "                   WHEN CURRENT-CHAR = \"+\" OR CURRENT-CHAR = \"-\"\n"
        "                       IF NORMALIZED-LENGTH = 0\n"
        "                           IF NORMALIZED-LENGTH >= 255\n"
        "                               MOVE 1 TO LNK-STATUS\n"
        "                               MOVE 0 TO LNK-RESULT\n"
        "                               GOBACK\n"
        "                           END-IF\n"
        "                           ADD 1 TO NORMALIZED-LENGTH\n"
        "                           MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       ELSE\n"
        "                           IF EXPECT-EXPONENT-SIGN = 1\n"
        "                               IF NORMALIZED-LENGTH >= 255\n"
        "                                   MOVE 1 TO LNK-STATUS\n"
        "                                   MOVE 0 TO LNK-RESULT\n"
        "                                   GOBACK\n"
        "                               END-IF\n"
        "                               ADD 1 TO NORMALIZED-LENGTH\n"
        "                               MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                               MOVE 0 TO EXPECT-EXPONENT-SIGN\n"
        "                           ELSE\n"
        "                               MOVE 1 TO LNK-STATUS\n"
        "                               MOVE 0 TO LNK-RESULT\n"
        "                               GOBACK\n"
        "                           END-IF\n"
        "                       END-IF\n"
        "                   WHEN OTHER\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       MOVE 0 TO LNK-RESULT\n"
        "                       GOBACK\n"
        "               END-EVALUATE\n"
        "           END-PERFORM.\n"
        "           IF NORMALIZED-LENGTH = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF HAS-ANY-DIGIT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF HAS-EXPONENT = 1 AND EXPONENT-DIGITS = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF EXPECT-EXPONENT-SIGN = 1\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION NUMVAL(NORMALIZED-BUFFER(1:NORMALIZED-LENGTH))\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRTOD-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strtod_string(&program_text),
            "strtod string generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strtod string generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strtod_parses_scientific_notation)
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
    library_path = "stdlib_strtod_val_lib.cob";
    driver_path = "stdlib_strtod_val_drv.cob";
    binary_path = "stdlib_strtod_val.bin";
    output_path = "stdlib_strtod_val.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRTOD-VALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(255) VALUE \"  -125.0\".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +8.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRTOD' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRTOD-VALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strtod(&library_text),
            "strtod generator should succeed") != FT_SUCCESS)
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -125.0, g_default_float_tolerance,
            "strtod helper should parse signed decimal input accurately") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "strtod helper should report success for valid input") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "strtod helper should only emit expected transcript lines for valid input") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strtod_rejects_invalid_input)
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
    library_path = "stdlib_strtod_inv_lib.cob";
    driver_path = "stdlib_strtod_inv_drv.cob";
    binary_path = "stdlib_strtod_inv.bin";
    output_path = "stdlib_strtod_inv.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRTOD-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(255) VALUE \"123A45\".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRTOD' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRTOD-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strtod(&library_text),
            "strtod generator should succeed") != FT_SUCCESS)
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
            "strtod helper should reject invalid characters and report failure") != FT_SUCCESS)
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

