#include "standard_library_test_support.hpp"

#define STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE \
    "       IDENTIFICATION DIVISION.\n" \
    "       PROGRAM-ID. CBLC-STRLEN.\n" \
    "       DATA DIVISION.\n" \
    "       WORKING-STORAGE SECTION.\n" \
    "       01 IDX PIC 9(9) VALUE 000000000.\n" \
    "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n" \
    "       LINKAGE SECTION.\n" \
    "       01 LNK-SOURCE PIC X(%zu).\n" \
    "       01 LNK-DECLARED-LENGTH PIC S9(9) COMP-5.\n" \
    "       01 LNK-RESULT PIC 9(9).\n" \
    "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n" \
    "           BY VALUE LNK-DECLARED-LENGTH BY REFERENCE LNK-RESULT.\n" \
    "       MAIN.\n" \
    "           MOVE 0 TO LNK-RESULT.\n" \
    "           MOVE LNK-DECLARED-LENGTH TO SCAN-LIMIT.\n" \
    "           IF SCAN-LIMIT > %zu\n" \
    "               MOVE %zu TO SCAN-LIMIT\n" \
    "           END-IF.\n" \
    "           MOVE 0 TO IDX.\n" \
    "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n" \
    "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n" \
    "                   EXIT PERFORM\n" \
    "               END-IF\n" \
    "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n" \
    "                   MOVE IDX TO LNK-RESULT\n" \
    "               END-IF\n" \
    "           END-PERFORM.\n" \
    "           GOBACK.\n" \
    "       END PROGRAM CBLC-STRLEN.\n"
#define STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE \
    "       IDENTIFICATION DIVISION.\n" \
    "       PROGRAM-ID. CBLC-STRLEN-STRING.\n" \
    "       DATA DIVISION.\n" \
    "       WORKING-STORAGE SECTION.\n" \
    "       LINKAGE SECTION.\n" \
    "       01 LNK-SOURCE.\n" \
    "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n" \
    "          05 LNK-SOURCE-BUF PIC X(%zu).\n" \
    "       01 LNK-RESULT PIC 9(9).\n" \
    "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n" \
    "           BY REFERENCE LNK-RESULT.\n" \
    "       MAIN.\n" \
    "           MOVE LNK-SOURCE-LEN TO LNK-RESULT.\n" \
    "           GOBACK.\n" \
    "       END PROGRAM CBLC-STRLEN-STRING.\n"

FT_TEST(test_standard_library_strlen_generates_expected_text)
{
    char *program_text;
    char *expected_text;
    size_t limit;
    size_t buffer_size;
    int required_length;
    int status;

    transpiler_standard_library_reset_usage();
    program_text = NULL;
    limit = transpiler_standard_library_get_strlen_limit();
    if (test_expect_success(transpiler_standard_library_generate_strlen(&program_text),
            "strlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
        limit, limit, limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
            limit, limit, limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_resizes_linkage_buffer)
{
    char *program_text;
    char *expected_text;
    size_t limit;
    size_t buffer_size;
    int required_length;
    int status;

    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_usage(10);
    transpiler_standard_library_note_strlen_usage(4);
    limit = transpiler_standard_library_get_strlen_limit();
    if (test_expect_size_t_equal(limit, 10,
            "strlen limit should reflect largest caller length") != FT_SUCCESS)
        return (FT_FAILURE);
    program_text = NULL;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&program_text),
            "strlen generator should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
        limit, limit, limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
            limit, limit, limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strlen generator should size linkage buffer to recorded maximum");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    transpiler_standard_library_reset_usage();
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_updates_limit_after_additional_usage)
{
    char *program_text;
    char *expected_text;
    size_t limit;
    size_t buffer_size;
    int required_length;
    int status;

    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_usage(8);
    limit = transpiler_standard_library_get_strlen_limit();
    if (test_expect_size_t_equal(limit, 8,
            "strlen limit should capture initial usage") != FT_SUCCESS)
        return (FT_FAILURE);
    program_text = NULL;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&program_text),
            "strlen generator should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
        limit, limit, limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
            limit, limit, limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strlen generator should size linkage buffer for initial usage");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    if (status != FT_SUCCESS)
    {
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    transpiler_standard_library_note_strlen_usage(24);
    limit = transpiler_standard_library_get_strlen_limit();
    if (test_expect_size_t_equal(limit, 24,
            "strlen limit should grow to reflect later usage") != FT_SUCCESS)
    {
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    program_text = NULL;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&program_text),
            "strlen generator should succeed after growth") != FT_SUCCESS)
    {
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
        limit, limit, limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_EXPECTED_TEMPLATE,
            limit, limit, limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strlen generator should resize linkage buffer after additional usage");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    transpiler_standard_library_reset_usage();
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_limit_resets_with_context)
{
    t_transpiler_context context;
    size_t limit;
    int status;

    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_usage(48);
    limit = transpiler_standard_library_get_strlen_limit();
    if (test_expect_size_t_equal(limit, 48,
            "strlen limit should reflect recorded usage") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_reset_unit_state(&context);
    limit = transpiler_standard_library_get_strlen_limit();
    status = test_expect_size_t_equal(limit, 255,
        "strlen limit should reset when context unit state resets");
    transpiler_context_dispose(&context);
    transpiler_standard_library_reset_usage();
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_string_generates_expected_text)
{
    char *program_text;
    char *expected_text;
    size_t limit;
    size_t buffer_size;
    int required_length;
    int status;

    transpiler_standard_library_reset_usage();
    program_text = NULL;
    limit = transpiler_standard_library_get_strlen_string_limit();
    if (test_expect_success(transpiler_standard_library_generate_strlen_string(&program_text),
            "string strlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
        limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
            limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_string_updates_limit_after_additional_usage)
{
    char *program_text;
    char *expected_text;
    size_t limit;
    size_t buffer_size;
    int required_length;
    int status;

    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_string_usage(16);
    limit = transpiler_standard_library_get_strlen_string_limit();
    if (test_expect_size_t_equal(limit, 16,
            "string strlen limit should capture initial usage") != FT_SUCCESS)
        return (FT_FAILURE);
    program_text = NULL;
    if (test_expect_success(transpiler_standard_library_generate_strlen_string(&program_text),
            "string strlen generator should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
        limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
            limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strlen generator should size group buffer for initial usage");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    if (status != FT_SUCCESS)
    {
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    transpiler_standard_library_note_strlen_string_usage(64);
    limit = transpiler_standard_library_get_strlen_string_limit();
    if (test_expect_size_t_equal(limit, 64,
            "string strlen limit should grow to reflect later usage") != FT_SUCCESS)
    {
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    program_text = NULL;
    if (test_expect_success(transpiler_standard_library_generate_strlen_string(&program_text),
            "string strlen generator should succeed after growth") != FT_SUCCESS)
    {
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
        limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
            limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strlen generator should resize group buffer after additional usage");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    transpiler_standard_library_reset_usage();
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_string_limit_resets_with_context)
{
    t_transpiler_context context;
    size_t limit;
    int status;

    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_string_usage(88);
    limit = transpiler_standard_library_get_strlen_string_limit();
    if (test_expect_size_t_equal(limit, 88,
            "string strlen limit should reflect recorded usage") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_reset_unit_state(&context);
    limit = transpiler_standard_library_get_strlen_string_limit();
    status = test_expect_size_t_equal(limit, 255,
        "string strlen limit should reset when context unit state resets");
    transpiler_context_dispose(&context);
    transpiler_standard_library_reset_usage();
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_string_resizes_group_buffer)
{
    char *program_text;
    char *expected_text;
    size_t limit;
    size_t buffer_size;
    int required_length;
    int status;

    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_string_usage(72);
    transpiler_standard_library_note_strlen_string_usage(15);
    limit = transpiler_standard_library_get_strlen_string_limit();
    if (test_expect_size_t_equal(limit, 72,
            "string strlen limit should reflect largest caller length") != FT_SUCCESS)
        return (FT_FAILURE);
    program_text = NULL;
    if (test_expect_success(transpiler_standard_library_generate_strlen_string(&program_text),
            "string strlen generator should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    required_length = pf_snprintf(NULL, 0, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
        limit);
    if (required_length < 0)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    buffer_size = static_cast<size_t>(required_length) + 1;
    expected_text = static_cast<char *>(cma_calloc(buffer_size, sizeof(char)));
    if (!expected_text)
    {
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    if (pf_snprintf(expected_text, buffer_size, STANDARD_LIBRARY_STRLEN_STRING_EXPECTED_TEMPLATE,
            limit) < 0)
    {
        cma_free(expected_text);
        if (program_text)
            cma_free(program_text);
        transpiler_standard_library_reset_usage();
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strlen generator should size group buffer to recorded maximum");
    if (program_text)
        cma_free(program_text);
    if (expected_text)
        cma_free(expected_text);
    transpiler_standard_library_reset_usage();
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
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
    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_usage(10);
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
    transpiler_standard_library_reset_usage();
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
    transpiler_standard_library_reset_usage();
    transpiler_standard_library_note_strlen_usage(10);
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
    transpiler_standard_library_reset_usage();
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

