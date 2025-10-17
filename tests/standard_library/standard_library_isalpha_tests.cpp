#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_isalpha_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ISALPHA.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-CHAR PIC X.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-CHAR\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-CHAR TO CURRENT-CHAR.\n"
        "           IF CURRENT-CHAR >= \"A\" AND CURRENT-CHAR <= \"Z\"\n"
        "               MOVE 1 TO LNK-RESULT\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR >= \"a\" AND CURRENT-CHAR <= \"z\"\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ISALPHA.\n";
    if (test_expect_success(transpiler_standard_library_generate_isalpha(&program_text),
            "isalpha generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "isalpha generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_isalpha_identifies_letter_cases)
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
    library_path = "stdlib_isalpha_lib.cob";
    driver_path = "stdlib_isalpha_drv.cob";
    binary_path = "stdlib_isalpha.bin";
    output_path = "stdlib_isalpha.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ISALPHA-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 LOWER-CHAR PIC X VALUE \"g\".\n"
        "       01 LOWER-RESULT PIC 9(9) VALUE 000000004.\n"
        "       01 UPPER-CHAR PIC X VALUE \"Z\".\n"
        "       01 UPPER-RESULT PIC 9(9) VALUE 000000004.\n"
        "       01 OTHER-CHAR PIC X VALUE \"5\".\n"
        "       01 OTHER-RESULT PIC 9(9) VALUE 000000004.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ISALPHA' USING BY REFERENCE LOWER-CHAR\n"
        "               BY REFERENCE LOWER-RESULT.\n"
        "           CALL 'CBLC-ISALPHA' USING BY REFERENCE UPPER-CHAR\n"
        "               BY REFERENCE UPPER-RESULT.\n"
        "           CALL 'CBLC-ISALPHA' USING BY REFERENCE OTHER-CHAR\n"
        "               BY REFERENCE OTHER-RESULT.\n"
        "           DISPLAY LOWER-RESULT.\n"
        "           DISPLAY UPPER-RESULT.\n"
        "           DISPLAY OTHER-RESULT.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ISALPHA-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_isalpha(&library_text),
            "isalpha generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "000000001\n000000001\n000000000\n",
            "isalpha helper should accept uppercase and lowercase letters only") != FT_SUCCESS)
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

