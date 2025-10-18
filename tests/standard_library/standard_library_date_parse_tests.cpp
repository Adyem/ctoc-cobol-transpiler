#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_date_parse_generates_expected_text)
{
    const char *expected_text;
    char *program_text;
    int status;

    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-DATE-YYYYMMDD.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9 VALUE 1.\n"
        "       01 WS-CHAR PIC X.\n"
        "       01 WS-DATE-DISPLAY PIC 9(8).\n"
        "       01 WS-REMAINDER PIC 9(8).\n"
        "       01 WS-YEAR PIC 9(4).\n"
        "       01 WS-MONTH PIC 9(2).\n"
        "       01 WS-DAY PIC 9(2).\n"
        "       01 WS-MAX-DAY PIC 9(2).\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-INPUT PIC X(8).\n"
        "       01 LNK-YEAR PIC 9(4).\n"
        "       01 LNK-MONTH PIC 9(2).\n"
        "       01 LNK-DAY PIC 9(2).\n"
        "       01 LNK-PACKED PIC 9(8) COMP-3.\n"
        "       01 LNK-SERIAL PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-INPUT\n"
        "           BY REFERENCE LNK-YEAR BY REFERENCE LNK-MONTH\n"
        "           BY REFERENCE LNK-DAY BY REFERENCE LNK-PACKED\n"
        "           BY REFERENCE LNK-SERIAL BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-YEAR.\n"
        "           MOVE 0 TO LNK-MONTH.\n"
        "           MOVE 0 TO LNK-DAY.\n"
        "           MOVE 0 TO LNK-PACKED.\n"
        "           MOVE 0 TO LNK-SERIAL.\n"
        "           MOVE 1 TO IDX.\n"
        "           PERFORM UNTIL IDX > 8\n"
        "               MOVE LNK-INPUT(IDX:1) TO WS-CHAR\n"
        "               IF WS-CHAR < \"0\" OR WS-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   GO TO VALIDATION-EXIT\n"
        "               END-IF\n"
        "               ADD 1 TO IDX\n"
        "           END-PERFORM.\n"
        "           MOVE LNK-INPUT TO WS-DATE-DISPLAY.\n"
        "           DIVIDE WS-DATE-DISPLAY BY 10000 GIVING WS-YEAR\n"
        "               REMAINDER WS-REMAINDER.\n"
        "           DIVIDE WS-REMAINDER BY 100 GIVING WS-MONTH\n"
        "               REMAINDER WS-DAY.\n"
        "           IF WS-MONTH < 1 OR WS-MONTH > 12\n"
        "               MOVE 2 TO LNK-STATUS\n"
        "               GO TO VALIDATION-EXIT\n"
        "           END-IF\n"
        "           MOVE 31 TO WS-MAX-DAY.\n"
        "           IF WS-MONTH = 4 OR WS-MONTH = 6 OR WS-MONTH = 9\n"
        "               OR WS-MONTH = 11\n"
        "               MOVE 30 TO WS-MAX-DAY\n"
        "           END-IF\n"
        "           IF WS-MONTH = 2\n"
        "               MOVE 28 TO WS-MAX-DAY\n"
        "               IF FUNCTION MOD(WS-YEAR, 4) = 0\n"
        "                   MOVE 29 TO WS-MAX-DAY\n"
        "                   IF FUNCTION MOD(WS-YEAR, 100) = 0\n"
        "                       IF FUNCTION MOD(WS-YEAR, 400) NOT = 0\n"
        "                           MOVE 28 TO WS-MAX-DAY\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF\n"
        "           IF WS-DAY < 1 OR WS-DAY > WS-MAX-DAY\n"
        "               MOVE 3 TO LNK-STATUS\n"
        "               GO TO VALIDATION-EXIT\n"
        "           END-IF\n"
        "           MOVE WS-YEAR TO LNK-YEAR.\n"
        "           MOVE WS-MONTH TO LNK-MONTH.\n"
        "           MOVE WS-DAY TO LNK-DAY.\n"
        "           MOVE WS-DATE-DISPLAY TO LNK-PACKED.\n"
        "           COMPUTE LNK-SERIAL = FUNCTION INTEGER-OF-DATE(WS-DATE-DISPLAY).\n"
        "       VALIDATION-EXIT.\n"
        "           IF LNK-STATUS NOT = 0\n"
        "               MOVE 0 TO LNK-YEAR\n"
        "               MOVE 0 TO LNK-MONTH\n"
        "               MOVE 0 TO LNK-DAY\n"
        "               MOVE 0 TO LNK-PACKED\n"
        "               MOVE 0 TO LNK-SERIAL\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-DATE-YYYYMMDD.\n";
    program_text = NULL;
    if (test_expect_success(
            transpiler_standard_library_generate_date_yyyymmdd(&program_text),
            "date parser generator should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    status = test_expect_cstring_equal(program_text, expected_text,
        "date parser program should match expected COBOL source");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_date_parse_executes_for_leap_date)
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
    library_path = "stdlib_date_parse_lib.cob";
    driver_path = "stdlib_date_parse_drv.cob";
    binary_path = "stdlib_date_parse.bin";
    output_path = "stdlib_date_parse.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DATE-PARSE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 INPUT-BUFFER PIC X(8) VALUE '20240229'.\n"
        "       01 YEAR-OUT PIC 9(4) VALUE 0.\n"
        "       01 MONTH-OUT PIC 9(2) VALUE 0.\n"
        "       01 DAY-OUT PIC 9(2) VALUE 0.\n"
        "       01 PACKED-OUT PIC 9(8) COMP-3 VALUE 0.\n"
        "       01 SERIAL-OUT PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 STATUS-OUT PIC 9 VALUE 9.\n"
        "       01 DISPLAY-YEAR PIC 9(4).\n"
        "       01 DISPLAY-MONTH PIC 9(2).\n"
        "       01 DISPLAY-DAY PIC 9(2).\n"
        "       01 DISPLAY-PACKED PIC 9(8).\n"
        "       01 DISPLAY-DATE PIC 9(8).\n"
        "       01 DISPLAY-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-DATE-YYYYMMDD' USING BY REFERENCE INPUT-BUFFER\n"
        "               BY REFERENCE YEAR-OUT BY REFERENCE MONTH-OUT\n"
        "               BY REFERENCE DAY-OUT BY REFERENCE PACKED-OUT\n"
        "               BY REFERENCE SERIAL-OUT BY REFERENCE STATUS-OUT.\n"
        "           MOVE YEAR-OUT TO DISPLAY-YEAR.\n"
        "           MOVE MONTH-OUT TO DISPLAY-MONTH.\n"
        "           MOVE DAY-OUT TO DISPLAY-DAY.\n"
        "           MOVE PACKED-OUT TO DISPLAY-PACKED.\n"
        "           MOVE FUNCTION DATE-OF-INTEGER(SERIAL-OUT) TO DISPLAY-DATE.\n"
        "           MOVE STATUS-OUT TO DISPLAY-STATUS.\n"
        "           DISPLAY DISPLAY-YEAR.\n"
        "           DISPLAY DISPLAY-MONTH.\n"
        "           DISPLAY DISPLAY-DAY.\n"
        "           DISPLAY DISPLAY-PACKED.\n"
        "           DISPLAY DISPLAY-DATE.\n"
        "           DISPLAY DISPLAY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM DATE-PARSE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(
            transpiler_standard_library_generate_date_yyyymmdd(&library_text),
            "date parser generator should succeed") != FT_SUCCESS)
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
    command_length = pf_snprintf(command, sizeof(command),
        "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer))
        != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "2024",
            "parser should return parsed year") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "02",
            "parser should return parsed month with leading zero") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "29",
            "parser should return parsed day") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "20240229",
            "parser should provide packed representation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "20240229",
            "parser should round-trip serial back to date") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "parser should report success for valid leap date") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "parser transcript should contain expected lines") != FT_SUCCESS)
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

FT_TEST(test_standard_library_date_parse_rejects_invalid_day)
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
    library_path = "stdlib_date_parse_invalid_lib.cob";
    driver_path = "stdlib_date_parse_invalid_drv.cob";
    binary_path = "stdlib_date_parse_invalid.bin";
    output_path = "stdlib_date_parse_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DATE-PARSE-INVALID.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 INPUT-BUFFER PIC X(8) VALUE '20240230'.\n"
        "       01 YEAR-OUT PIC 9(4) VALUE 7.\n"
        "       01 MONTH-OUT PIC 9(2) VALUE 7.\n"
        "       01 DAY-OUT PIC 9(2) VALUE 7.\n"
        "       01 PACKED-OUT PIC 9(8) COMP-3 VALUE 7.\n"
        "       01 SERIAL-OUT PIC S9(9) COMP-5 VALUE 7.\n"
        "       01 STATUS-OUT PIC 9 VALUE 9.\n"
        "       01 DISPLAY-YEAR PIC 9(4).\n"
        "       01 DISPLAY-MONTH PIC 9(2).\n"
        "       01 DISPLAY-DAY PIC 9(2).\n"
        "       01 DISPLAY-PACKED PIC 9(8).\n"
        "       01 DISPLAY-SERIAL PIC S9(9).\n"
        "       01 DISPLAY-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-DATE-YYYYMMDD' USING BY REFERENCE INPUT-BUFFER\n"
        "               BY REFERENCE YEAR-OUT BY REFERENCE MONTH-OUT\n"
        "               BY REFERENCE DAY-OUT BY REFERENCE PACKED-OUT\n"
        "               BY REFERENCE SERIAL-OUT BY REFERENCE STATUS-OUT.\n"
        "           MOVE YEAR-OUT TO DISPLAY-YEAR.\n"
        "           MOVE MONTH-OUT TO DISPLAY-MONTH.\n"
        "           MOVE DAY-OUT TO DISPLAY-DAY.\n"
        "           MOVE PACKED-OUT TO DISPLAY-PACKED.\n"
        "           MOVE SERIAL-OUT TO DISPLAY-SERIAL.\n"
        "           MOVE STATUS-OUT TO DISPLAY-STATUS.\n"
        "           DISPLAY DISPLAY-YEAR.\n"
        "           DISPLAY DISPLAY-MONTH.\n"
        "           DISPLAY DISPLAY-DAY.\n"
        "           DISPLAY DISPLAY-PACKED.\n"
        "           DISPLAY DISPLAY-SERIAL.\n"
        "           DISPLAY DISPLAY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM DATE-PARSE-INVALID.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(
            transpiler_standard_library_generate_date_yyyymmdd(&library_text),
            "date parser generator should succeed") != FT_SUCCESS)
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
    command_length = pf_snprintf(command, sizeof(command),
        "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer))
        != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0000",
            "parser should clear year when input is invalid") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "00",
            "parser should clear month when input is invalid") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "00",
            "parser should clear day when input is invalid") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "00000000",
            "parser should clear packed output when input is invalid") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "000000000",
            "parser should clear serial output when input is invalid") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "3",
            "parser should report invalid-day status") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "parser invalid transcript should contain expected lines") != FT_SUCCESS)
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
