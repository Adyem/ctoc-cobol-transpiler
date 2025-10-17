#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_strncpy_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNCPY.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COPY-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION PIC X(255).\n"
        "       01 LNK-DESTINATION-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-REQUEST-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY VALUE LNK-DESTINATION-LENGTH BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY VALUE LNK-REQUEST-LENGTH\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-DESTINATION-LENGTH TO DEST-LIMIT.\n"
        "           IF DEST-LIMIT > 255\n"
        "               MOVE 255 TO DEST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SOURCE-LIMIT.\n"
        "           IF SOURCE-LIMIT > 255\n"
        "               MOVE 255 TO SOURCE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-REQUEST-LENGTH TO COPY-LIMIT.\n"
        "           IF COPY-LIMIT > 255\n"
        "               MOVE 255 TO COPY-LIMIT\n"
        "           END-IF.\n"
        "           IF LNK-REQUEST-LENGTH > DEST-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           IF LNK-REQUEST-LENGTH > SOURCE-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           IF COPY-LIMIT > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO COPY-LIMIT\n"
        "           END-IF.\n"
        "           IF COPY-LIMIT > SOURCE-LIMIT\n"
        "               MOVE SOURCE-LIMIT TO COPY-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DEST-LIMIT\n"
        "               MOVE SPACE TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COPY-LIMIT\n"
        "               IF IDX > SOURCE-LIMIT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-SOURCE(IDX:1) TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNCPY.\n";
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&program_text),
            "strncpy generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strncpy generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNCPY-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 REQUEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COPY-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION.\n"
        "          05 LNK-DESTINATION-LEN PIC 9(4) COMP.\n"
        "          05 LNK-DESTINATION-BUF PIC X(255).\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-REQUEST-LEN PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY REFERENCE LNK-SOURCE BY VALUE LNK-REQUEST-LEN\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-SOURCE-LEN TO SOURCE-LIMIT.\n"
        "           IF SOURCE-LIMIT > 255\n"
        "               MOVE 255 TO SOURCE-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           MOVE LNK-REQUEST-LEN TO REQUEST-LIMIT.\n"
        "           IF REQUEST-LIMIT < 0\n"
        "               MOVE 0 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT > 255\n"
        "               MOVE 255 TO REQUEST-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           MOVE REQUEST-LIMIT TO COPY-LIMIT.\n"
        "           IF COPY-LIMIT > SOURCE-LIMIT\n"
        "               MOVE SOURCE-LIMIT TO COPY-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           MOVE 0 TO LNK-DESTINATION-LEN.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 255\n"
        "               MOVE SPACE TO LNK-DESTINATION-BUF(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COPY-LIMIT\n"
        "               MOVE LNK-SOURCE-BUF(IDX:1) TO LNK-DESTINATION-BUF(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE COPY-LIMIT TO LNK-DESTINATION-LEN.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNCPY-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strncpy_string(&program_text),
            "strncpy string generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strncpy string generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_executes_without_truncation)
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
    library_path = "stdlib_strncpy_lib.cob";
    driver_path = "stdlib_strncpy_drv.cob";
    binary_path = "stdlib_strncpy.bin";
    output_path = "stdlib_strncpy.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLOWORLD\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +4.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">HELL      <\n000000000\n",
            "strncpy helper should copy requested subset without truncation") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strncpy_reports_truncation)
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
    library_path = "stdlib_strncpy_trc_lib.cob";
    driver_path = "stdlib_strncpy_trc_drv.cob";
    binary_path = "stdlib_strncpy_trc.bin";
    output_path = "stdlib_strncpy_trc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-TRUNC-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +3.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLOWORLD\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-TRUNC-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">HEL       <\n000000001\n",
            "strncpy helper should flag truncation when destination is smaller than request") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strncpy_reports_short_source)
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
    library_path = "stdlib_strncpy_shr_lib.cob";
    driver_path = "stdlib_strncpy_shr_drv.cob";
    binary_path = "stdlib_strncpy_shr.bin";
    output_path = "stdlib_strncpy_shr.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-SHORT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"XXXXXXXXXX\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HI        \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +2.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-SHORT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">HI        <\n000000001\n",
            "strncpy helper should pad spaces and flag short source requests") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strncpy_honors_zero_request)
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
    library_path = "stdlib_strncpy_zro_lib.cob";
    driver_path = "stdlib_strncpy_zro_drv.cob";
    binary_path = "stdlib_strncpy_zro.bin";
    output_path = "stdlib_strncpy_zro.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-ZERO-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"XXXXXXXXXX\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"SAMPLE    \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +0.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000123.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \"<\" DEST-BUFFER \">\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-ZERO-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "<          >\n000000000\n",
            "strncpy helper should blank destination when zero bytes requested") != FT_SUCCESS)
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

