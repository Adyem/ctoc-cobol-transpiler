#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_strcmp_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCMP.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 FIRST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 SECOND-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COMPARE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-FIRST PIC X(255).\n"
        "       01 LNK-FIRST-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SECOND PIC X(255).\n"
        "       01 LNK-SECOND-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-FIRST\n"
        "           BY VALUE LNK-FIRST-LENGTH BY REFERENCE LNK-SECOND\n"
        "           BY VALUE LNK-SECOND-LENGTH BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-FIRST-LENGTH TO FIRST-LIMIT.\n"
        "           IF FIRST-LIMIT > 255\n"
        "               MOVE 255 TO FIRST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SECOND-LENGTH TO SECOND-LIMIT.\n"
        "           IF SECOND-LIMIT > 255\n"
        "               MOVE 255 TO SECOND-LIMIT\n"
        "           END-IF.\n"
        "           MOVE FIRST-LIMIT TO COMPARE-LIMIT.\n"
        "           IF SECOND-LIMIT < COMPARE-LIMIT\n"
        "               MOVE SECOND-LIMIT TO COMPARE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COMPARE-LIMIT\n"
        "               IF LNK-FIRST(IDX:1) < LNK-SECOND(IDX:1)\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-FIRST(IDX:1) > LNK-SECOND(IDX:1)\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF FIRST-LIMIT < SECOND-LIMIT\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF FIRST-LIMIT > SECOND-LIMIT\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCMP.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcmp(&program_text),
            "strcmp generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcmp generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcmp_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCMP-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 FIRST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 SECOND-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COMPARE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-FIRST.\n"
        "          05 LNK-FIRST-LEN PIC 9(4) COMP.\n"
        "          05 LNK-FIRST-BUF PIC X(255).\n"
        "       01 LNK-SECOND.\n"
        "          05 LNK-SECOND-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SECOND-BUF PIC X(255).\n"
        "       01 LNK-RESULT PIC S9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-FIRST\n"
        "           BY REFERENCE LNK-SECOND BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-FIRST-LEN TO FIRST-LIMIT.\n"
        "           IF FIRST-LIMIT > 255\n"
        "               MOVE 255 TO FIRST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SECOND-LEN TO SECOND-LIMIT.\n"
        "           IF SECOND-LIMIT > 255\n"
        "               MOVE 255 TO SECOND-LIMIT\n"
        "           END-IF.\n"
        "           MOVE FIRST-LIMIT TO COMPARE-LIMIT.\n"
        "           IF SECOND-LIMIT < COMPARE-LIMIT\n"
        "               MOVE SECOND-LIMIT TO COMPARE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COMPARE-LIMIT\n"
        "               IF LNK-FIRST-BUF(IDX:1) < LNK-SECOND-BUF(IDX:1)\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-FIRST-BUF(IDX:1) > LNK-SECOND-BUF(IDX:1)\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF FIRST-LIMIT < SECOND-LIMIT\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF FIRST-LIMIT > SECOND-LIMIT\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCMP-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcmp_string(&program_text),
            "string strcmp generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strcmp generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcmp_executes)
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
    library_path = "stdlib_strcmp_lib.cob";
    driver_path = "stdlib_strcmp_drv.cob";
    binary_path = "stdlib_strcmp.bin";
    output_path = "stdlib_strcmp.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCMP-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 FIRST-BUFFER PIC X(10) VALUE \"APPLE     \".\n"
        "       01 SECOND-BUFFER PIC X(10) VALUE \"APPLE     \".\n"
        "       01 FIRST-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 SECOND-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT PIC S9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"APRIC     \" TO SECOND-BUFFER.\n"
        "           MOVE +5 TO SECOND-LENGTH.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-STRCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"APPL      \" TO SECOND-BUFFER.\n"
        "           MOVE +4 TO SECOND-LENGTH.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-STRCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCMP-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcmp(&library_text),
            "strcmp generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "EQUAL\nLESS\nGREATER\n",
            "strcmp helper should report lexicographic ordering") != FT_SUCCESS)
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

