#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_powerof_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-POWEROF.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 EXP-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 EXP-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 FRACTION-TOLERANCE USAGE COMP-2 VALUE 0.0000000000001.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-BASE USAGE COMP-2.\n"
        "       01 LNK-EXPONENT USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-BASE\n"
        "           BY REFERENCE LNK-EXPONENT BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-BASE = 0 AND LNK-EXPONENT <= 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE FUNCTION INTEGER(LNK-EXPONENT) TO EXP-INTEGER.\n"
        "           COMPUTE EXP-FRACTION = FUNCTION ABS(LNK-EXPONENT - EXP-INTEGER).\n"
        "           IF LNK-BASE < 0 AND EXP-FRACTION > FRACTION-TOLERANCE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = LNK-BASE ** LNK-EXPONENT\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-POWEROF.\n";
    if (test_expect_success(transpiler_standard_library_generate_powerof(&program_text),
            "powerof generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "powerof generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_powerof_handles_fractional_exponent)
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
    library_path = "stdlib_powerof_frac_lib.cob";
    driver_path = "stdlib_powerof_frac_drv.cob";
    binary_path = "stdlib_powerof_frac.bin";
    output_path = "stdlib_powerof_frac.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. POWEROF-FRACTIONAL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 BASE-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 EXPONENT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 9 TO BASE-VALUE.\n"
        "           MOVE 0.5 TO EXPONENT-VALUE.\n"
        "           CALL 'CBLC-POWEROF' USING BY REFERENCE BASE-VALUE\n"
        "               BY REFERENCE EXPONENT-VALUE BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM POWEROF-FRACTIONAL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_powerof(&library_text),
            "powerof generator should succeed") != FT_SUCCESS)
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
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 3.0, g_default_float_tolerance,
            "powerof helper should compute square root of positive base") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "powerof helper should report success for fractional exponent with positive base") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "powerof helper should only emit expected transcript lines for valid fractional exponent") != FT_SUCCESS)
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

FT_TEST(test_standard_library_powerof_rejects_invalid_domain)
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
    library_path = "stdlib_powerof_invalid_lib.cob";
    driver_path = "stdlib_powerof_invalid_drv.cob";
    binary_path = "stdlib_powerof_invalid.bin";
    output_path = "stdlib_powerof_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. POWEROF-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 BASE-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 EXPONENT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -2 TO BASE-VALUE.\n"
        "           MOVE 0.5 TO EXPONENT-VALUE.\n"
        "           CALL 'CBLC-POWEROF' USING BY REFERENCE BASE-VALUE\n"
        "               BY REFERENCE EXPONENT-VALUE BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM POWEROF-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_powerof(&library_text),
            "powerof generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "0000.0000\n1\n",
            "powerof helper should reject negative bases with fractional exponents") != FT_SUCCESS)
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

