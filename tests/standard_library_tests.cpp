#include "transpiler_standard_library.hpp"

#include "compiler/compiler_test_support.hpp"
#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

FT_TEST(test_standard_library_strlen_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRLEN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-DECLARED-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-DECLARED-LENGTH BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-DECLARED-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE IDX TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRLEN.\n";
    if (test_expect_success(transpiler_standard_library_generate_strlen(&program_text),
            "strlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_lookup_enforces_std_prefix)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::strlen");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strlen should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRLEN", ft_strlen("CBLC-STRLEN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strlen should map to CBLC-STRLEN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strnlen");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strnlen should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNLEN", ft_strlen("CBLC-STRNLEN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strnlen should map to CBLC-STRNLEN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcmp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcmp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCMP", ft_strlen("CBLC-STRCMP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcmp should map to CBLC-STRCMP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcpy");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcpy should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCPY", ft_strlen("CBLC-STRCPY") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcpy should map to CBLC-STRCPY program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strncpy");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strncpy should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNCPY", ft_strlen("CBLC-STRNCPY") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strncpy should map to CBLC-STRNCPY program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::sqrt");
    if (!entry)
    {
        pf_printf("Assertion failed: std::sqrt should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-SQRT", ft_strlen("CBLC-SQRT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::sqrt should map to CBLC-SQRT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strlen");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strlen should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strnlen");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strnlen should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcmp");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcmp should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcpy");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcpy should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strncpy");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strncpy should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("sqrt");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified sqrt should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_lookup_is_case_sensitive)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::STRLEN");
    if (entry)
    {
        pf_printf("Assertion failed: std::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::strlen");
    if (entry)
    {
        pf_printf("Assertion failed: STD::strlen should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::STRLEN");
    if (entry)
    {
        pf_printf("Assertion failed: STD::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_catalog_lists_all_entries)
{
    const t_transpiler_standard_library_entry *entries;
    size_t count;

    entries = transpiler_standard_library_get_entries(&count);
    if (!entries)
    {
        pf_printf("Assertion failed: catalog should return entry table\n");
        return (FT_FAILURE);
    }
    if (count != 6)
    {
        pf_printf("Assertion failed: catalog should report six standard library entries but returned %u\n", static_cast<unsigned int>(count));
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[0].qualified_name, "std::strlen", ft_strlen("std::strlen") + 1) != 0)
    {
        pf_printf("Assertion failed: first catalog entry should be std::strlen\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[1].qualified_name, "std::strnlen", ft_strlen("std::strnlen") + 1) != 0)
    {
        pf_printf("Assertion failed: second catalog entry should be std::strnlen\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[2].qualified_name, "std::strcmp", ft_strlen("std::strcmp") + 1) != 0)
    {
        pf_printf("Assertion failed: third catalog entry should be std::strcmp\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[3].qualified_name, "std::strcpy", ft_strlen("std::strcpy") + 1) != 0)
    {
        pf_printf("Assertion failed: fourth catalog entry should be std::strcpy\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[4].qualified_name, "std::strncpy", ft_strlen("std::strncpy") + 1) != 0)
    {
        pf_printf("Assertion failed: fifth catalog entry should be std::strncpy\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[5].qualified_name, "std::sqrt", ft_strlen("std::sqrt") + 1) != 0)
    {
        pf_printf("Assertion failed: sixth catalog entry should be std::sqrt\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_generators_validate_out_parameter)
{
    if (transpiler_standard_library_generate_strlen(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strlen generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strnlen(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strnlen generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcmp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcmp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcpy(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcpy generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strncpy(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strncpy generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_executes)
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
    library_path = "stdlib_strlen_lib.cob";
    driver_path = "stdlib_strlen_drv.cob";
    binary_path = "stdlib_strlen.bin";
    output_path = "stdlib_strlen.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRLEN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLO     \".\n"
        "       01 DECLARED-LENGTH PIC 9(9) VALUE 000000010.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRLEN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&library_text),
            "strlen generator should succeed") != FT_SUCCESS)
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
            "strlen helper should report trimmed length") != FT_SUCCESS)
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

FT_TEST(test_standard_library_strlen_handles_all_spaces)
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
    library_path = "stdlib_strlen_spc_lib.cob";
    driver_path = "stdlib_strlen_spc_drv.cob";
    binary_path = "stdlib_strlen_spc.bin";
    output_path = "stdlib_strlen_spc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRLEN-SPACES-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DECLARED-LENGTH PIC 9(9) VALUE 000000010.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000999.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRLEN-SPACES-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&library_text),
            "strlen generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "000000000\n",
            "strlen helper should report zero for all-space buffer") != FT_SUCCESS)
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

FT_TEST(test_standard_library_sqrt_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-SQRT.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-OPERAND < 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION SQRT(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-SQRT.\n";
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&program_text),
            "sqrt generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "sqrt generator should emit expected COBOL subprogram");
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

FT_TEST(test_standard_library_sqrt_executes_for_positive_operand)
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
    library_path = "stdlib_sqrt_pos_lib.cob";
    driver_path = "stdlib_sqrt_pos_drv.cob";
    binary_path = "stdlib_sqrt_pos.bin";
    output_path = "stdlib_sqrt_pos.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SQRT-POSITIVE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 49 TO OPERAND-FLOAT.\n"
        "           CALL 'CBLC-SQRT' USING BY REFERENCE OPERAND-FLOAT\n"
        "               BY REFERENCE RESULT-FLOAT BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SQRT-POSITIVE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&library_text),
            "sqrt generator should succeed") != FT_SUCCESS)
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
    if (test_expect_cstring_equal(output_buffer, "0007.0000\n0\n",
            "sqrt helper should compute positive roots and report success") != FT_SUCCESS)
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

FT_TEST(test_standard_library_sqrt_rejects_negative_operand)
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
    library_path = "stdlib_sqrt_neg_lib.cob";
    driver_path = "stdlib_sqrt_neg_drv.cob";
    binary_path = "stdlib_sqrt_neg.bin";
    output_path = "stdlib_sqrt_neg.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SQRT-NEGATIVE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -4 TO OPERAND-FLOAT.\n"
        "           CALL 'CBLC-SQRT' USING BY REFERENCE OPERAND-FLOAT\n"
        "               BY REFERENCE RESULT-FLOAT BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SQRT-NEGATIVE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&library_text),
            "sqrt generator should succeed") != FT_SUCCESS)
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
            "sqrt helper should reject negative operands and zero the result") != FT_SUCCESS)
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

const t_test_case *get_standard_library_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"standard_library_lookup_enforces_std_prefix", test_standard_library_lookup_enforces_std_prefix},
        {"standard_library_lookup_is_case_sensitive", test_standard_library_lookup_is_case_sensitive},
        {"standard_library_catalog_lists_all_entries", test_standard_library_catalog_lists_all_entries},
        {"standard_library_generators_validate_out_parameter", test_standard_library_generators_validate_out_parameter},
        {"standard_library_strlen_generates_expected_text", test_standard_library_strlen_generates_expected_text},
        {"standard_library_strlen_executes", test_standard_library_strlen_executes},
        {"standard_library_strlen_handles_all_spaces", test_standard_library_strlen_handles_all_spaces},
        {"standard_library_strnlen_generates_expected_text", test_standard_library_strnlen_generates_expected_text},
        {"standard_library_strnlen_respects_request_limit", test_standard_library_strnlen_respects_request_limit},
        {"standard_library_strnlen_honors_declared_length", test_standard_library_strnlen_honors_declared_length},
        {"standard_library_strnlen_trims_trailing_spaces", test_standard_library_strnlen_trims_trailing_spaces},
        {"standard_library_strnlen_stops_at_low_value", test_standard_library_strnlen_stops_at_low_value},
        {"standard_library_strcmp_generates_expected_text", test_standard_library_strcmp_generates_expected_text},
        {"standard_library_strcmp_executes", test_standard_library_strcmp_executes},
        {"standard_library_strcpy_generates_expected_text", test_standard_library_strcpy_generates_expected_text},
        {"standard_library_strcpy_executes_without_truncation", test_standard_library_strcpy_executes_without_truncation},
        {"standard_library_strcpy_blanks_destination", test_standard_library_strcpy_blanks_destination},
        {"standard_library_strcpy_reports_truncation", test_standard_library_strcpy_reports_truncation},
        {"standard_library_strncpy_generates_expected_text", test_standard_library_strncpy_generates_expected_text},
        {"standard_library_strncpy_executes_without_truncation", test_standard_library_strncpy_executes_without_truncation},
        {"standard_library_strncpy_reports_truncation", test_standard_library_strncpy_reports_truncation},
        {"standard_library_strncpy_reports_short_source", test_standard_library_strncpy_reports_short_source},
        {"standard_library_strncpy_honors_zero_request", test_standard_library_strncpy_honors_zero_request},
        {"standard_library_sqrt_generates_expected_text", test_standard_library_sqrt_generates_expected_text},
        {"standard_library_sqrt_executes_for_positive_operand", test_standard_library_sqrt_executes_for_positive_operand},
        {"standard_library_sqrt_rejects_negative_operand", test_standard_library_sqrt_rejects_negative_operand}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
