#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_strnlen_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNLEN-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 REQUEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-REQUEST PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-REQUEST BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE LNK-SOURCE-LEN TO ACTUAL-LENGTH.\n"
        "           IF ACTUAL-LENGTH > 255\n"
        "               MOVE 255 TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE LNK-REQUEST TO REQUEST-LIMIT.\n"
        "           IF REQUEST-LIMIT < 0\n"
        "               MOVE 0 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT > 255\n"
        "               MOVE 255 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT < ACTUAL-LENGTH\n"
        "               MOVE REQUEST-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE ACTUAL-LENGTH TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNLEN-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strnlen_string(&program_text),
            "string strnlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strnlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNLEN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 DECLARED-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 REQUEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-DECLARED-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-REQUEST-LIMIT PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-DECLARED-LENGTH BY VALUE LNK-REQUEST-LIMIT\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE 0 TO DECLARED-LIMIT.\n"
        "           IF LNK-DECLARED-LENGTH > 0\n"
        "               MOVE LNK-DECLARED-LENGTH TO DECLARED-LIMIT\n"
        "           END-IF.\n"
        "           IF DECLARED-LIMIT > 255\n"
        "               MOVE 255 TO DECLARED-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO REQUEST-LIMIT.\n"
        "           IF LNK-REQUEST-LIMIT > 0\n"
        "               MOVE LNK-REQUEST-LIMIT TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT > 255\n"
        "               MOVE 255 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT < DECLARED-LIMIT\n"
        "               MOVE REQUEST-LIMIT TO DECLARED-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DECLARED-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE IDX TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNLEN.\n";
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&program_text),
            "strnlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strnlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_respects_request_limit)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_lim_lib.cob";
    driver_path = "stdlib_strnlen_lim_drv.cob";
    binary_path = "stdlib_strnlen_lim.bin";
    output_path = "stdlib_strnlen_lim.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-LIMIT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(12) VALUE \"HELLOWORLD  \".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +12.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-LIMIT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "000000005\n",
            "strnlen helper should clamp result to requested limit") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strnlen_honors_declared_length)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_decl_lib.cob";
    driver_path = "stdlib_strnlen_decl_drv.cob";
    binary_path = "stdlib_strnlen_decl.bin";
    output_path = "stdlib_strnlen_decl.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-DECL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"ABCD      \".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +4.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +9.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000123.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-DECL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "000000004\n",
            "strnlen helper should respect caller-declared length") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strnlen_trims_trailing_spaces)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_spc_lib.cob";
    driver_path = "stdlib_strnlen_spc_drv.cob";
    binary_path = "stdlib_strnlen_spc.bin";
    output_path = "stdlib_strnlen_spc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-SPACES-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(5) VALUE \"AB   \".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000321.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-SPACES-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "000000002\n",
            "strnlen helper should trim trailing spaces within the scan window") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strnlen_stops_at_low_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_nul_lib.cob";
    driver_path = "stdlib_strnlen_nul_drv.cob";
    binary_path = "stdlib_strnlen_nul.bin";
    output_path = "stdlib_strnlen_nul.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-NUL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(6) VALUE \"ABCDEF\".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000111.\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE LOW-VALUE TO SOURCE-BUFFER(3:1).\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-NUL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "000000002\n",
            "strnlen helper should stop scanning at LOW-VALUE bytes") != FT_SUCCESS)
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

