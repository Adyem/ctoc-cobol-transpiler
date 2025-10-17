#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_strcat_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCAT.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 LEFT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 RIGHT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-OFFSET PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION PIC X(255).\n"
        "       01 LNK-DESTINATION-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-LEFT PIC X(255).\n"
        "       01 LNK-LEFT-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RIGHT PIC X(255).\n"
        "       01 LNK-RIGHT-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       01 LNK-RESULT-LENGTH PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY VALUE LNK-DESTINATION-LENGTH BY REFERENCE LNK-LEFT\n"
        "           BY VALUE LNK-LEFT-LENGTH BY REFERENCE LNK-RIGHT\n"
        "           BY VALUE LNK-RIGHT-LENGTH BY REFERENCE LNK-STATUS\n"
        "           BY REFERENCE LNK-RESULT-LENGTH.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT-LENGTH.\n"
        "           MOVE LNK-DESTINATION-LENGTH TO DEST-LIMIT.\n"
        "           IF DEST-LIMIT > 255\n"
        "               MOVE 255 TO DEST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-LEFT-LENGTH TO LEFT-LIMIT.\n"
        "           IF LEFT-LIMIT > 255\n"
        "               MOVE 255 TO LEFT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-RIGHT-LENGTH TO RIGHT-LIMIT.\n"
        "           IF RIGHT-LIMIT > 255\n"
        "               MOVE 255 TO RIGHT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO DEST-OFFSET.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DEST-LIMIT\n"
        "               IF LNK-DESTINATION(IDX:1) NOT = SPACE\n"
        "                   MOVE IDX TO DEST-OFFSET\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           MOVE DEST-OFFSET TO LNK-RESULT-LENGTH.\n"
        "           ADD 1 TO DEST-OFFSET.\n"
        "           IF DEST-OFFSET > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO DEST-OFFSET\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEFT-LIMIT\n"
        "               IF LNK-RESULT-LENGTH >= DEST-LIMIT\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               ADD 1 TO LNK-RESULT-LENGTH\n"
        "               MOVE LNK-LEFT(IDX:1) TO LNK-DESTINATION(LNK-RESULT-LENGTH:1)\n"
        "           END-PERFORM.\n"
        "           IF LNK-STATUS = 0\n"
        "               MOVE 0 TO IDX\n"
        "               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > RIGHT-LIMIT\n"
        "                   IF LNK-RESULT-LENGTH >= DEST-LIMIT\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       EXIT PERFORM\n"
        "                   END-IF\n"
        "                   ADD 1 TO LNK-RESULT-LENGTH\n"
        "                   MOVE LNK-RIGHT(IDX:1) TO LNK-DESTINATION(LNK-RESULT-LENGTH:1)\n"
        "               END-PERFORM\n"
        "           END-IF.\n"
        "           IF LNK-RESULT-LENGTH > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO LNK-RESULT-LENGTH\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCAT.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcat(&program_text),
            "strcat generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcat generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcat_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCAT-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 LEFT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 RIGHT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION.\n"
        "          05 LNK-DESTINATION-LEN PIC 9(4) COMP.\n"
        "          05 LNK-DESTINATION-BUF PIC X(255).\n"
        "       01 LNK-LEFT.\n"
        "          05 LNK-LEFT-LEN PIC 9(4) COMP.\n"
        "          05 LNK-LEFT-BUF PIC X(255).\n"
        "       01 LNK-RIGHT.\n"
        "          05 LNK-RIGHT-LEN PIC 9(4) COMP.\n"
        "          05 LNK-RIGHT-BUF PIC X(255).\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY REFERENCE LNK-LEFT BY REFERENCE LNK-RIGHT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 255 TO DEST-LIMIT.\n"
        "           MOVE LNK-DESTINATION-LEN TO RESULT-LENGTH.\n"
        "           IF RESULT-LENGTH < 0\n"
        "               MOVE 0 TO RESULT-LENGTH\n"
        "           END-IF.\n"
        "           IF RESULT-LENGTH > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO RESULT-LENGTH\n"
        "           END-IF.\n"
        "           MOVE LNK-LEFT-LEN TO LEFT-LIMIT.\n"
        "           IF LEFT-LIMIT < 0\n"
        "               MOVE 0 TO LEFT-LIMIT\n"
        "           END-IF.\n"
        "           IF LEFT-LIMIT > 255\n"
        "               MOVE 255 TO LEFT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-RIGHT-LEN TO RIGHT-LIMIT.\n"
        "           IF RIGHT-LIMIT < 0\n"
        "               MOVE 0 TO RIGHT-LIMIT\n"
        "           END-IF.\n"
        "           IF RIGHT-LIMIT > 255\n"
        "               MOVE 255 TO RIGHT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEFT-LIMIT\n"
        "               IF RESULT-LENGTH >= DEST-LIMIT\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               ADD 1 TO RESULT-LENGTH\n"
        "               MOVE LNK-LEFT-BUF(IDX:1) TO\n"
        "                   LNK-DESTINATION-BUF(RESULT-LENGTH:1)\n"
        "           END-PERFORM.\n"
        "           IF LNK-STATUS = 0\n"
        "               MOVE 0 TO IDX\n"
        "               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > RIGHT-LIMIT\n"
        "                   IF RESULT-LENGTH >= DEST-LIMIT\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       EXIT PERFORM\n"
        "                   END-IF\n"
        "                   ADD 1 TO RESULT-LENGTH\n"
        "                   MOVE LNK-RIGHT-BUF(IDX:1) TO\n"
        "                       LNK-DESTINATION-BUF(RESULT-LENGTH:1)\n"
        "               END-PERFORM\n"
        "           END-IF.\n"
        "           IF RESULT-LENGTH > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO RESULT-LENGTH\n"
        "           END-IF.\n"
        "           MOVE RESULT-LENGTH TO LNK-DESTINATION-LEN.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCAT-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcat_string(&program_text),
            "strcat string generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcat string generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcat_executes_without_truncation)
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
    library_path = "stdlib_strcat_lib.cob";
    driver_path = "stdlib_strcat_drv.cob";
    binary_path = "stdlib_strcat.bin";
    output_path = "stdlib_strcat.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCAT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(20) VALUE \"HELLO\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +20.\n"
        "       01 LEFT-BUFFER PIC X(5) VALUE \" \".\n"
        "       01 LEFT-LENGTH PIC S9(9) COMP-5 VALUE +1.\n"
        "       01 RIGHT-BUFFER PIC X(10) VALUE \"WORLD\".\n"
        "       01 RIGHT-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 CAT-STATUS PIC 9(9) VALUE 000000009.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCAT' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE LEFT-BUFFER\n"
        "               BY VALUE LEFT-LENGTH BY REFERENCE RIGHT-BUFFER\n"
        "               BY VALUE RIGHT-LENGTH BY REFERENCE CAT-STATUS\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY CAT-STATUS.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCAT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcat(&library_text),
            "strcat generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">HELLO WORLD         <\n000000000\n000000011\n",
            "strcat helper should append both sources and report final length") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strcat_reports_truncation)
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
    library_path = "stdlib_strcat_trc_lib.cob";
    driver_path = "stdlib_strcat_trc_drv.cob";
    binary_path = "stdlib_strcat_trc.bin";
    output_path = "stdlib_strcat_trc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCAT-TRUNC-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(8) VALUE \"HELLO\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +8.\n"
        "       01 LEFT-BUFFER PIC X(5) VALUE \" \".\n"
        "       01 LEFT-LENGTH PIC S9(9) COMP-5 VALUE +1.\n"
        "       01 RIGHT-BUFFER PIC X(10) VALUE \"WORLD\".\n"
        "       01 RIGHT-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 CAT-STATUS PIC 9(9) VALUE 000000000.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCAT' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE LEFT-BUFFER\n"
        "               BY VALUE LEFT-LENGTH BY REFERENCE RIGHT-BUFFER\n"
        "               BY VALUE RIGHT-LENGTH BY REFERENCE CAT-STATUS\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY CAT-STATUS.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCAT-TRUNC-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcat(&library_text),
            "strcat generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">HELLO WO<\n000000001\n000000008\n",
            "strcat helper should truncate when destination is too small") != FT_SUCCESS)
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

