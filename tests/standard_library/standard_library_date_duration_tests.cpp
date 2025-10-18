#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_date_duration_generates_expected_text)
{
    const char *expected_text;
    char *program_text;
    int status;

    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-DATE-DURATION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-DIFF PIC S9(9) COMP-5.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-START-SERIAL PIC S9(9) COMP-5.\n"
        "       01 LNK-END-SERIAL PIC S9(9) COMP-5.\n"
        "       01 LNK-DURATION PIC S9(9) COMP-5.\n"
        "       01 LNK-COMPARISON PIC S9 COMP-5.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-START-SERIAL\n"
        "           BY REFERENCE LNK-END-SERIAL BY REFERENCE LNK-DURATION\n"
        "           BY REFERENCE LNK-COMPARISON BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-DURATION.\n"
        "           MOVE 0 TO LNK-COMPARISON.\n"
        "           MOVE LNK-END-SERIAL TO WS-DIFF.\n"
        "           SUBTRACT LNK-START-SERIAL FROM WS-DIFF.\n"
        "           IF WS-DIFF < 0\n"
        "               MOVE -1 TO LNK-COMPARISON\n"
        "               MULTIPLY -1 BY WS-DIFF GIVING LNK-DURATION\n"
        "           ELSE\n"
        "               IF WS-DIFF > 0\n"
        "                   MOVE 1 TO LNK-COMPARISON\n"
        "                   MOVE WS-DIFF TO LNK-DURATION\n"
        "               ELSE\n"
        "                   MOVE 0 TO LNK-COMPARISON\n"
        "                   MOVE 0 TO LNK-DURATION\n"
        "               END-IF\n"
        "           END-IF\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-DATE-DURATION.\n";
    program_text = NULL;
    if (test_expect_success(
            transpiler_standard_library_generate_date_duration(&program_text),
            "date duration generator should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    status = test_expect_cstring_equal(program_text, expected_text,
        "date duration program should match expected COBOL source");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_date_duration_executes_for_forward_range)
{
    const char *duration_library_path;
    const char *parse_library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *duration_library_text;
    char *parse_library_text;
    char command[512];
    char output_buffer[256];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    duration_library_path = "stdlib_date_duration_lib.cob";
    parse_library_path = "stdlib_date_duration_parse_lib.cob";
    driver_path = "stdlib_date_duration_drv.cob";
    binary_path = "stdlib_date_duration.bin";
    output_path = "stdlib_date_duration.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DATE-DURATION-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 START-DATE PIC X(8) VALUE '20240115'.\n"
        "       01 END-DATE PIC X(8) VALUE '20240205'.\n"
        "       01 START-YEAR PIC 9(4) VALUE 0.\n"
        "       01 START-MONTH PIC 9(2) VALUE 0.\n"
        "       01 START-DAY PIC 9(2) VALUE 0.\n"
        "       01 START-PACKED PIC 9(8) COMP-3 VALUE 0.\n"
        "       01 START-SERIAL PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 START-STATUS PIC 9 VALUE 9.\n"
        "       01 END-YEAR PIC 9(4) VALUE 0.\n"
        "       01 END-MONTH PIC 9(2) VALUE 0.\n"
        "       01 END-DAY PIC 9(2) VALUE 0.\n"
        "       01 END-PACKED PIC 9(8) COMP-3 VALUE 0.\n"
        "       01 END-SERIAL PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 END-STATUS PIC 9 VALUE 9.\n"
        "       01 DURATION PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 COMPARISON PIC S9 COMP-5 VALUE 0.\n"
        "       01 DURATION-STATUS PIC 9 VALUE 9.\n"
        "       01 DISPLAY-START-STATUS PIC 9.\n"
        "       01 DISPLAY-END-STATUS PIC 9.\n"
        "       01 DISPLAY-DURATION PIC S9(9).\n"
        "       01 DISPLAY-COMPARISON PIC S9.\n"
        "       01 DISPLAY-DURATION-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-DATE-YYYYMMDD' USING BY REFERENCE START-DATE\n"
        "               BY REFERENCE START-YEAR BY REFERENCE START-MONTH\n"
        "               BY REFERENCE START-DAY BY REFERENCE START-PACKED\n"
        "               BY REFERENCE START-SERIAL BY REFERENCE START-STATUS.\n"
        "           CALL 'CBLC-DATE-YYYYMMDD' USING BY REFERENCE END-DATE\n"
        "               BY REFERENCE END-YEAR BY REFERENCE END-MONTH\n"
        "               BY REFERENCE END-DAY BY REFERENCE END-PACKED\n"
        "               BY REFERENCE END-SERIAL BY REFERENCE END-STATUS.\n"
        "           CALL 'CBLC-DATE-DURATION' USING BY REFERENCE START-SERIAL\n"
        "               BY REFERENCE END-SERIAL BY REFERENCE DURATION\n"
        "               BY REFERENCE COMPARISON BY REFERENCE DURATION-STATUS.\n"
        "           MOVE START-STATUS TO DISPLAY-START-STATUS.\n"
        "           MOVE END-STATUS TO DISPLAY-END-STATUS.\n"
        "           MOVE DURATION TO DISPLAY-DURATION.\n"
        "           MOVE COMPARISON TO DISPLAY-COMPARISON.\n"
        "           MOVE DURATION-STATUS TO DISPLAY-DURATION-STATUS.\n"
        "           DISPLAY DISPLAY-START-STATUS.\n"
        "           DISPLAY DISPLAY-END-STATUS.\n"
        "           DISPLAY DISPLAY-DURATION.\n"
        "           DISPLAY DISPLAY-COMPARISON.\n"
        "           DISPLAY DISPLAY-DURATION-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM DATE-DURATION-DRIVER.\n";
    duration_library_text = NULL;
    parse_library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(
            transpiler_standard_library_generate_date_duration(&duration_library_text),
            "date duration generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(
            transpiler_standard_library_generate_date_yyyymmdd(&parse_library_text),
            "date parser generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(duration_library_path, duration_library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(parse_library_path, parse_library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s %s", binary_path, driver_path,
        duration_library_path, parse_library_path);
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
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "start parse should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "end parse should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 21.0,
            0.0001, "duration helper should return absolute difference in days") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0,
            0.0001, "duration helper should report forward comparison") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "duration helper should report success status") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "duration transcript should contain expected lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (duration_library_text)
        cma_free(duration_library_text);
    if (parse_library_text)
        cma_free(parse_library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(duration_library_path);
    test_remove_file(parse_library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_date_duration_handles_reverse_range)
{
    const char *duration_library_path;
    const char *parse_library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *duration_library_text;
    char *parse_library_text;
    char command[512];
    char output_buffer[256];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    duration_library_path = "stdlib_date_duration_reverse_lib.cob";
    parse_library_path = "stdlib_date_duration_reverse_parse_lib.cob";
    driver_path = "stdlib_date_duration_reverse_drv.cob";
    binary_path = "stdlib_date_duration_reverse.bin";
    output_path = "stdlib_date_duration_reverse.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DATE-DURATION-REVERSE.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 START-DATE PIC X(8) VALUE '20240205'.\n"
        "       01 END-DATE PIC X(8) VALUE '20240115'.\n"
        "       01 START-YEAR PIC 9(4) VALUE 0.\n"
        "       01 START-MONTH PIC 9(2) VALUE 0.\n"
        "       01 START-DAY PIC 9(2) VALUE 0.\n"
        "       01 START-PACKED PIC 9(8) COMP-3 VALUE 0.\n"
        "       01 START-SERIAL PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 START-STATUS PIC 9 VALUE 9.\n"
        "       01 END-YEAR PIC 9(4) VALUE 0.\n"
        "       01 END-MONTH PIC 9(2) VALUE 0.\n"
        "       01 END-DAY PIC 9(2) VALUE 0.\n"
        "       01 END-PACKED PIC 9(8) COMP-3 VALUE 0.\n"
        "       01 END-SERIAL PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 END-STATUS PIC 9 VALUE 9.\n"
        "       01 DURATION PIC S9(9) COMP-5 VALUE 0.\n"
        "       01 COMPARISON PIC S9 COMP-5 VALUE 0.\n"
        "       01 DURATION-STATUS PIC 9 VALUE 9.\n"
        "       01 DISPLAY-DURATION PIC S9(9).\n"
        "       01 DISPLAY-COMPARISON PIC S9.\n"
        "       01 DISPLAY-DURATION-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-DATE-YYYYMMDD' USING BY REFERENCE START-DATE\n"
        "               BY REFERENCE START-YEAR BY REFERENCE START-MONTH\n"
        "               BY REFERENCE START-DAY BY REFERENCE START-PACKED\n"
        "               BY REFERENCE START-SERIAL BY REFERENCE START-STATUS.\n"
        "           CALL 'CBLC-DATE-YYYYMMDD' USING BY REFERENCE END-DATE\n"
        "               BY REFERENCE END-YEAR BY REFERENCE END-MONTH\n"
        "               BY REFERENCE END-DAY BY REFERENCE END-PACKED\n"
        "               BY REFERENCE END-SERIAL BY REFERENCE END-STATUS.\n"
        "           CALL 'CBLC-DATE-DURATION' USING BY REFERENCE START-SERIAL\n"
        "               BY REFERENCE END-SERIAL BY REFERENCE DURATION\n"
        "               BY REFERENCE COMPARISON BY REFERENCE DURATION-STATUS.\n"
        "           MOVE DURATION TO DISPLAY-DURATION.\n"
        "           MOVE COMPARISON TO DISPLAY-COMPARISON.\n"
        "           MOVE DURATION-STATUS TO DISPLAY-DURATION-STATUS.\n"
        "           DISPLAY DISPLAY-DURATION.\n"
        "           DISPLAY DISPLAY-COMPARISON.\n"
        "           DISPLAY DISPLAY-DURATION-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM DATE-DURATION-REVERSE.\n";
    duration_library_text = NULL;
    parse_library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(
            transpiler_standard_library_generate_date_duration(&duration_library_text),
            "date duration generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(
            transpiler_standard_library_generate_date_yyyymmdd(&parse_library_text),
            "date parser generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(duration_library_path, duration_library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(parse_library_path, parse_library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s %s", binary_path, driver_path,
        duration_library_path, parse_library_path);
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
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 21.0,
            0.0001, "reverse duration should report absolute difference") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -1.0,
            0.0001, "reverse duration should report negative comparison") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "reverse duration should report success status") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "reverse duration transcript should contain expected lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (duration_library_text)
        cma_free(duration_library_text);
    if (parse_library_text)
        cma_free(parse_library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(duration_library_path);
    test_remove_file(parse_library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
