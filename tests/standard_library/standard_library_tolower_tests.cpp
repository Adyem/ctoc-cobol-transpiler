#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_tolower_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TOLOWER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 TARGET-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-TARGET PIC X(255).\n"
        "       01 LNK-TARGET-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-TARGET\n"
        "           BY VALUE LNK-TARGET-LENGTH BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-TARGET-LENGTH TO TARGET-LIMIT.\n"
        "           IF TARGET-LIMIT > 255\n"
        "               MOVE 255 TO TARGET-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TARGET-LIMIT\n"
        "               IF LNK-TARGET(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-TARGET(IDX:1) TO CURRENT-CHAR\n"
        "               INSPECT CURRENT-CHAR CONVERTING \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\n"
        "                   TO \"abcdefghijklmnopqrstuvwxyz\"\n"
        "               MOVE CURRENT-CHAR TO LNK-TARGET(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TOLOWER.\n";
    if (test_expect_success(transpiler_standard_library_generate_tolower(&program_text),
            "tolower generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "tolower generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tolower_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TOLOWER-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 TARGET-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-TARGET.\n"
        "          05 LNK-TARGET-LEN PIC 9(4) COMP.\n"
        "          05 LNK-TARGET-BUF PIC X(255).\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-TARGET\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-TARGET-LEN TO TARGET-LIMIT.\n"
        "           IF TARGET-LIMIT > 255\n"
        "               MOVE 255 TO TARGET-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TARGET-LIMIT\n"
        "               IF LNK-TARGET-BUF(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-TARGET-BUF(IDX:1) TO CURRENT-CHAR\n"
        "               INSPECT CURRENT-CHAR CONVERTING \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\n"
        "                   TO \"abcdefghijklmnopqrstuvwxyz\"\n"
        "               MOVE CURRENT-CHAR TO LNK-TARGET-BUF(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TOLOWER-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_tolower_string(&program_text),
            "tolower string generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "tolower string generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tolower_respects_declared_length)
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
    library_path = "stdlib_tolower_lib.cob";
    driver_path = "stdlib_tolower_drv.cob";
    binary_path = "stdlib_tolower.bin";
    output_path = "stdlib_tolower.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. TOLOWER-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 TARGET-BUFFER PIC X(9) VALUE \"UPPER XYZ\".\n"
        "       01 TARGET-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 STATUS-FLAG PIC 9(9) VALUE 000000005.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-TOLOWER' USING BY REFERENCE TARGET-BUFFER\n"
        "               BY VALUE TARGET-LENGTH BY REFERENCE STATUS-FLAG.\n"
        "           DISPLAY \">\" TARGET-BUFFER \"<\".\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM TOLOWER-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_tolower(&library_text),
            "tolower generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, ">upper XYZ<\n000000000\n",
            "tolower helper should honor declared length and leave excess unchanged") != FT_SUCCESS)
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

