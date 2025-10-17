#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_strcpy_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCPY.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION PIC X(255).\n"
        "       01 LNK-DESTINATION-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY VALUE LNK-DESTINATION-LENGTH BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-STATUS.\n"
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
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DEST-LIMIT\n"
        "               MOVE SPACE TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SOURCE-LIMIT\n"
        "               IF IDX > DEST-LIMIT\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-SOURCE(IDX:1) TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCPY.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&program_text),
            "strcpy generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcpy generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcpy_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCPY-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COPY-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION.\n"
        "          05 LNK-DESTINATION-LEN PIC 9(4) COMP.\n"
        "          05 LNK-DESTINATION-BUF PIC X(255).\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY REFERENCE LNK-SOURCE BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-SOURCE-LEN TO SOURCE-LIMIT.\n"
        "           IF SOURCE-LIMIT > 255\n"
        "               MOVE 255 TO SOURCE-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           MOVE SOURCE-LIMIT TO COPY-LIMIT.\n"
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
        "       END PROGRAM CBLC-STRCPY-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcpy_string(&program_text),
            "strcpy string generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcpy string generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcpy_executes_without_truncation)
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
    library_path = "stdlib_strcpy_lib.cob";
    driver_path = "stdlib_strcpy_drv.cob";
    binary_path = "stdlib_strcpy.bin";
    output_path = "stdlib_strcpy.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCPY-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLO     \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCPY-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&library_text),
            "strcpy generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">HELLO     <\n000000000\n",
            "strcpy helper should copy source into destination without truncation") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strcpy_blanks_destination)
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
    library_path = "stdlib_strcpy_blk_lib.cob";
    driver_path = "stdlib_strcpy_blk_drv.cob";
    binary_path = "stdlib_strcpy_blk.bin";
    output_path = "stdlib_strcpy_blk.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCPY-BLANK-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"XXXXXXXXXX\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"TEST      \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +4.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \"<\" DEST-BUFFER \">\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCPY-BLANK-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&library_text),
            "strcpy generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "<TEST      >\n000000000\n",
            "strcpy helper should blank destination before copying") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strcpy_reports_truncation)
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
    library_path = "stdlib_strcpy_trc_lib.cob";
    driver_path = "stdlib_strcpy_trc_drv.cob";
    binary_path = "stdlib_strcpy_trc.bin";
    output_path = "stdlib_strcpy_trc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCPY-TRUNC-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +3.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLOWORLD\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCPY-TRUNC-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&library_text),
            "strcpy generator should succeed") != FT_SUCCESS)
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
            "strcpy helper should flag truncation and preserve destination limit") != FT_SUCCESS)
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

