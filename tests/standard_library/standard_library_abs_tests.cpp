#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_abs_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ABS.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND PIC S9(18) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(18) COMP-5.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-OPERAND TO LNK-RESULT.\n"
        "           IF LNK-RESULT < 0\n"
        "               COMPUTE LNK-RESULT = 0 - LNK-RESULT\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       MOVE 0 TO LNK-RESULT\n"
        "               END-COMPUTE\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ABS.\n";
    if (test_expect_success(transpiler_standard_library_generate_abs(&program_text),
            "abs generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "abs generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_abs_executes_for_common_cases)
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
    library_path = "stdlib_abs_common_lib.cob";
    driver_path = "stdlib_abs_common_drv.cob";
    binary_path = "stdlib_abs_common.bin";
    output_path = "stdlib_abs_common.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ABS-COMMON-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS PIC S9(18) COMP-5 VALUE +123.\n"
        "       01 RESULT-POS PIC S9(18) COMP-5 VALUE +0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 DISPLAY-POS PIC +9(9).\n"
        "       01 OPERAND-NEG PIC S9(18) COMP-5 VALUE -456.\n"
        "       01 RESULT-NEG PIC S9(18) COMP-5 VALUE +0.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 DISPLAY-NEG PIC +9(9).\n"
        "       01 OPERAND-ZRO PIC S9(18) COMP-5 VALUE +0.\n"
        "       01 RESULT-ZRO PIC S9(18) COMP-5 VALUE +999.\n"
        "       01 STATUS-ZRO PIC 9 VALUE 9.\n"
        "       01 DISPLAY-ZRO PIC +9(9).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO DISPLAY-POS.\n"
        "           DISPLAY DISPLAY-POS.\n"
        "           DISPLAY STATUS-POS.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO DISPLAY-NEG.\n"
        "           DISPLAY DISPLAY-NEG.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-ZRO\n"
        "               BY REFERENCE RESULT-ZRO BY REFERENCE STATUS-ZRO.\n"
        "           MOVE RESULT-ZRO TO DISPLAY-ZRO.\n"
        "           DISPLAY DISPLAY-ZRO.\n"
        "           DISPLAY STATUS-ZRO.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ABS-COMMON-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_abs(&library_text),
            "abs generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "+000000123\n0\n+000000456\n0\n+000000000\n0\n",
            "abs helper should return magnitudes and report success") != FT_SUCCESS)
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

FT_TEST(test_standard_library_abs_reports_overflow_for_min_value)
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
    library_path = "stdlib_abs_min_lib.cob";
    driver_path = "stdlib_abs_min_drv.cob";
    binary_path = "stdlib_abs_min.bin";
    output_path = "stdlib_abs_min.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ABS-MIN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-MIN PIC S9(18) COMP-5 VALUE -9223372036854775808.\n"
        "       01 RESULT-MIN PIC S9(18) COMP-5 VALUE +777.\n"
        "       01 STATUS-MIN PIC 9 VALUE 0.\n"
        "       01 DISPLAY-MIN PIC +9(19).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-MIN\n"
        "               BY REFERENCE RESULT-MIN BY REFERENCE STATUS-MIN.\n"
        "           MOVE RESULT-MIN TO DISPLAY-MIN.\n"
        "           DISPLAY DISPLAY-MIN.\n"
        "           DISPLAY STATUS-MIN.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ABS-MIN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_abs(&library_text),
            "abs generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "+0000000000000000000\n1\n",
            "abs helper should flag overflow for minimum negative value") != FT_SUCCESS)
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

