#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_atol_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ATOL.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 REMAINING-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 DIGIT-COUNT PIC 9(9) VALUE 000000000.\n"
        "       01 NEGATIVE-FLAG PIC 9 VALUE 0.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 DIGIT-VALUE PIC 9 VALUE 0.\n"
        "       01 OVERFLOW-FLAG PIC 9 VALUE 0.\n"
        "       01 ACCUMULATOR PIC S9(36) COMP-3 VALUE 0.\n"
        "       01 MAX-VALUE PIC S9(36) COMP-3 VALUE 999999999999999999.\n"
        "       01 MIN-VALUE PIC S9(36) COMP-3 VALUE -999999999999999999.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(18).\n"
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
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE(START-INDEX:1) TO CURRENT-CHAR.\n"
        "           MOVE 0 TO NEGATIVE-FLAG.\n"
        "           IF CURRENT-CHAR = \"-\"\n"
        "               MOVE 1 TO NEGATIVE-FLAG\n"
        "               ADD 1 TO START-INDEX\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR = \"+\"\n"
        "                   ADD 1 TO START-INDEX\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO DIGIT-COUNT.\n"
        "           MOVE 0 TO OVERFLOW-FLAG.\n"
        "           MOVE 0 TO ACCUMULATOR.\n"
        "           MOVE 0 TO END-INDEX.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR = LOW-VALUE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR < \"0\" OR CURRENT-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               ADD 1 TO DIGIT-COUNT\n"
        "               MOVE LNK-SOURCE(IDX:1) TO DIGIT-VALUE\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR * 10\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR + DIGIT-VALUE\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               MOVE IDX TO END-INDEX\n"
        "           END-PERFORM.\n"
        "           IF DIGIT-COUNT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF END-INDEX = 0\n"
        "               MOVE ACTUAL-LENGTH TO END-INDEX\n"
        "           END-IF.\n"
        "           COMPUTE REMAINING-INDEX = END-INDEX + 1.\n"
        "           IF REMAINING-INDEX < START-INDEX\n"
        "               MOVE START-INDEX TO REMAINING-INDEX\n"
        "           END-IF.\n"
        "           PERFORM VARYING IDX FROM REMAINING-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF NEGATIVE-FLAG = 1\n"
        "               COMPUTE ACCUMULATOR = 0 - ACCUMULATOR\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR > MAX-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR < MIN-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACCUMULATOR TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ATOL.\n";
    if (test_expect_success(transpiler_standard_library_generate_atol(&program_text),
            "atol generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "atol generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atol_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ATOL-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 REMAINING-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 DIGIT-COUNT PIC 9(9) VALUE 000000000.\n"
        "       01 NEGATIVE-FLAG PIC 9 VALUE 0.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 DIGIT-VALUE PIC 9 VALUE 0.\n"
        "       01 OVERFLOW-FLAG PIC 9 VALUE 0.\n"
        "       01 ACCUMULATOR PIC S9(36) COMP-3 VALUE 0.\n"
        "       01 MAX-VALUE PIC S9(36) COMP-3 VALUE 999999999999999999.\n"
        "       01 MIN-VALUE PIC S9(36) COMP-3 VALUE -999999999999999999.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-RESULT PIC S9(18).\n"
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
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE-BUF(START-INDEX:1) TO CURRENT-CHAR.\n"
        "           MOVE 0 TO NEGATIVE-FLAG.\n"
        "           IF CURRENT-CHAR = \"-\"\n"
        "               MOVE 1 TO NEGATIVE-FLAG\n"
        "               ADD 1 TO START-INDEX\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR = \"+\"\n"
        "                   ADD 1 TO START-INDEX\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO DIGIT-COUNT.\n"
        "           MOVE 0 TO OVERFLOW-FLAG.\n"
        "           MOVE 0 TO ACCUMULATOR.\n"
        "           MOVE 0 TO END-INDEX.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               MOVE LNK-SOURCE-BUF(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR = LOW-VALUE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR < \"0\" OR CURRENT-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               ADD 1 TO DIGIT-COUNT\n"
        "               MOVE LNK-SOURCE-BUF(IDX:1) TO DIGIT-VALUE\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR * 10\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR + DIGIT-VALUE\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               MOVE IDX TO END-INDEX\n"
        "           END-PERFORM.\n"
        "           IF DIGIT-COUNT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF END-INDEX = 0\n"
        "               MOVE ACTUAL-LENGTH TO END-INDEX\n"
        "           END-IF.\n"
        "           COMPUTE REMAINING-INDEX = END-INDEX + 1.\n"
        "           IF REMAINING-INDEX < START-INDEX\n"
        "               MOVE START-INDEX TO REMAINING-INDEX\n"
        "           END-IF.\n"
        "           PERFORM VARYING IDX FROM REMAINING-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE-BUF(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE-BUF(IDX:1) NOT = SPACE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF NEGATIVE-FLAG = 1\n"
        "               COMPUTE ACCUMULATOR = 0 - ACCUMULATOR\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR > MAX-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR < MIN-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACCUMULATOR TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ATOL-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_atol_string(&program_text),
            "atol string generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "atol string generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atol_converts_large_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[160];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atol_lib.cob";
    driver_path = "stdlib_atol_drv.cob";
    binary_path = "stdlib_atol.bin";
    output_path = "stdlib_atol.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(32) VALUE \"123456789012345678\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +18.\n"
        "       01 RESULT PIC S9(18) VALUE 000000000000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(18).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOL' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atol(&library_text),
            "atol generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, " 123456789012345678\n0\n",
            "atol helper should convert large positive input") != FT_SUCCESS)
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

FT_TEST(test_standard_library_atol_detects_overflow)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[160];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atol_overflow_lib.cob";
    driver_path = "stdlib_atol_overflow_drv.cob";
    binary_path = "stdlib_atol_overflow.bin";
    output_path = "stdlib_atol_overflow.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOL-OVERFLOW-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(32) VALUE \"1234567890123456789\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +19.\n"
        "       01 RESULT PIC S9(18) VALUE 000000000000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(18).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOL' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOL-OVERFLOW-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atol(&library_text),
            "atol generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, " 000000000000000000\n1\n",
            "atol helper should report overflow for wide input") != FT_SUCCESS)
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

