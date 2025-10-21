#include "../test_suites.hpp"

#include "compiler_test_support.hpp"
#include "cobol/round_trip_pipeline_helpers.hpp"
#include "libft/CMA/CMA.hpp"

const t_test_case *get_compiler_differential_tests(size_t *count);

FT_TEST(test_compiler_differential_return_numeric_outputs_match)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char cobol_source_path[256];
    char c_source_path[256];
    char cobol_binary_path[256];
    char c_binary_path[256];
    char cobol_output_path[256];
    char c_output_path[256];
    char cobol_log_path[256];
    char cobol_output_buffer[128];
    char c_output_buffer[128];
    char command[512];
    char *generated_cobol;
    char *generated_c;
    const char *sample_path;
    int command_length;
    int directory_created;
    int status;

    FT_REQUIRE_FORWARD_TRANSLATION();
    FT_REQUIRE_COBC();
    sample_path = "samples/cblc/return_numeric.cblc";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generated_c = NULL;
    directory[0] = '\0';
    cobol_source_path[0] = '\0';
    c_source_path[0] = '\0';
    cobol_binary_path[0] = '\0';
    c_binary_path[0] = '\0';
    cobol_output_path[0] = '\0';
    c_output_path[0] = '\0';
    cobol_log_path[0] = '\0';
    directory_created = 0;
    status = FT_FAILURE;
    if (test_read_text_file(sample_path, cblc_buffer, sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "return_numeric.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "return_numeric.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "return_numeric.cblc should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_numeric_generated.cob", cobol_source_path,
            sizeof(cobol_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_generated.c", c_source_path,
            sizeof(c_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_cobol.bin", cobol_binary_path,
            sizeof(cobol_binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_c_backend.bin", c_binary_path,
            sizeof(c_binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_cobol.txt", cobol_output_path,
            sizeof(cobol_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_c_backend.txt", c_output_path,
            sizeof(c_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_cobol.log", cobol_log_path,
            sizeof(cobol_log_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(cobol_source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(c_source_path, generated_c) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(cobol_binary_path, cobol_source_path, cobol_log_path,
            "cobc should compile generated return_numeric COBOL program")
        != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s",
        c_source_path, c_binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cc should compile generated return_numeric C program\n");
        goto cleanup;
    }
    if (execute_binary(directory, "return_numeric_cobol.bin", "return_numeric_cobol.txt",
            "generated COBOL binary should execute successfully") != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cd %s && ./return_numeric_c_backend.bin > return_numeric_c_backend.txt", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: generated C binary should execute successfully\n");
        goto cleanup;
    }
    if (test_read_text_file(cobol_output_path, cobol_output_buffer,
            sizeof(cobol_output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(c_output_path, c_output_buffer,
            sizeof(c_output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strlen(cobol_output_buffer) != ft_strlen(c_output_buffer)
        || ft_strncmp(cobol_output_buffer, c_output_buffer,
            ft_strlen(cobol_output_buffer) + 1) != 0)
    {
        pf_printf("Assertion failed: COBOL and C backends should emit matching output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_remove_file(c_output_path);
        test_remove_file(c_binary_path);
        test_remove_file(c_source_path);
        test_remove_file(cobol_output_path);
        test_remove_file(cobol_binary_path);
        test_remove_file(cobol_source_path);
        test_remove_file(cobol_log_path);
        test_remove_directory(directory);
    }
    if (generated_c)
        cma_free(generated_c);
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

const t_test_case *get_compiler_differential_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"compiler_differential_return_numeric_outputs_match",
            test_compiler_differential_return_numeric_outputs_match},
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
