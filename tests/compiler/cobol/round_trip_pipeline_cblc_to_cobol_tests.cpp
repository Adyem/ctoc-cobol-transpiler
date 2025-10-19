#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include "libft/CMA/CMA.hpp"

#include "round_trip_pipeline_helpers.hpp"

FT_TEST(test_cblc_copy_file_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char input_path[256];
    char copied_path[256];
    char output_buffer[512];
    char *generated_cobol;
    const char *input_contents;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    input_path[0] = '\0';
    copied_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    input_contents = "ALPHA\nBRAVO\n";
    expected_output = "ALPHA\nBRAVO\n";
    if (test_read_text_file("samples/cblc/copy_file.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "copy_file.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "copy_file.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "copy_file_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "copy_file_generated.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "copy_file_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated copy_file program") != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "input.txt", input_path,
            sizeof(input_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, input_contents) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "output.txt", copied_path,
            sizeof(copied_path)) != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "copy_file_generated.bin", NULL,
            "translated copy_file binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(copied_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated copy_file binary should copy input contents\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, NULL,
            log_path);
        if (input_path[0] != '\0')
            test_remove_file(input_path);
        if (copied_path[0] != '\0')
            test_remove_file(copied_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_filter_prefix_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char input_path[256];
    char filtered_path[256];
    char output_buffer[512];
    char *generated_cobol;
    const char *input_contents;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    input_path[0] = '\0';
    filtered_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    input_contents = "ERR-FIRST\nALLOW-ENTRY\nERR-SECOND\nIGNORE\n";
    expected_output = "ERR-FIRST\nERR-SECOND\n";
    if (test_read_text_file("samples/cblc/filter_prefix.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "filter_prefix.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "filter_prefix.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "filter_prefix_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "filter_prefix_generated.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "filter_prefix_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "input.txt", input_path,
            sizeof(input_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "filtered.txt", filtered_path,
            sizeof(filtered_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, input_contents) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated filter_prefix program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "filter_prefix_generated.bin", NULL,
            "translated filter_prefix binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(filtered_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated filter_prefix binary should only copy matching lines\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, NULL,
            log_path);
        if (input_path[0] != '\0')
            test_remove_file(input_path);
        if (filtered_path[0] != '\0')
            test_remove_file(filtered_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_reverse_control_flow_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[64];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "0011\n";
    if (test_read_text_file("samples/cblc/reverse_control_flow.cblc",
            cblc_buffer, sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "reverse_control_flow.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "reverse_control_flow.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "reverse_control_flow_generated.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_control_flow_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_control_flow_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated reverse_control_flow program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "reverse_control_flow_generated.bin",
            "reverse_control_flow_generated.txt",
            "translated reverse_control_flow binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated reverse_control_flow binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_integration_showcase_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char data_path[256];
    char accepted_path[256];
    char rejected_path[256];
    char output_buffer[256];
    char accepted_buffer[128];
    char rejected_buffer[128];
    char *generated_cobol;
    const char *input_contents;
    const char *expected_output;
    const char *expected_accepted;
    const char *expected_rejected;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    data_path[0] = '\0';
    accepted_path[0] = '\0';
    rejected_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    input_contents = "A000120\nR000045\nA000085\n";
    expected_output = "INTEGRATION SHOWCASE\n      2\n      1\n    205\n";
    expected_accepted =
        "ACCEPTED ENTRY                  ACCEPTED ENTRY                  ";
    expected_rejected = "REJECTED ENTRY                  ";
    if (test_read_text_file("samples/cblc/integration_showcase.cblc",
            cblc_buffer, sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "integration_showcase.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "integration_showcase.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "integration_showcase_generated.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "integration_showcase_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "integration_showcase_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "integration_showcase_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "transactions.dat", data_path,
            sizeof(data_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "accepted.txt", accepted_path,
            sizeof(accepted_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "rejected.txt", rejected_path,
            sizeof(rejected_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(data_path, input_contents) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated integration_showcase program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "integration_showcase_generated.bin",
            "integration_showcase_generated.txt",
            "translated integration_showcase binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated integration_showcase binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    if (test_read_text_file(accepted_path, accepted_buffer,
            sizeof(accepted_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(accepted_buffer, expected_accepted,
            ft_strlen(expected_accepted) + 1) != 0)
    {
        pf_printf("Assertion failed: translated integration_showcase binary should record accepted entries\n");
        goto cleanup;
    }
    if (test_read_text_file(rejected_path, rejected_buffer,
            sizeof(rejected_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(rejected_buffer, expected_rejected,
            ft_strlen(expected_rejected) + 1) != 0)
    {
        pf_printf("Assertion failed: translated integration_showcase binary should record rejected entries\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        if (data_path[0] != '\0')
            test_remove_file(data_path);
        if (accepted_path[0] != '\0')
            test_remove_file(accepted_path);
        if (rejected_path[0] != '\0')
            test_remove_file(rejected_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_multi_module_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    char main_buffer[32768];
    char worker_buffer[32768];
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_main_cobol;
    char *generated_worker_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    directory[0] = '\0';
    main_source_path[0] = '\0';
    worker_source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "WORKER READY\n   1\n";
    if (test_read_text_file("samples/cblc/multi_module_main.cblc",
            main_buffer, sizeof(main_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file("samples/cblc/multi_module_worker.cblc",
            worker_buffer, sizeof(worker_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(main_buffer, &main_unit),
            "multi_module_main.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_buffer, &worker_unit),
            "multi_module_worker.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "multi_module_main.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit,
                &generated_worker_cobol),
            "multi_module_worker.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_main_cobol),
            "generated main COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_worker_cobol),
            "generated worker COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "multi_module_main_generated.cob",
            main_source_path, sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_worker_generated.cob",
            worker_source_path, sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "multi_module_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(main_source_path, generated_main_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(worker_source_path, generated_worker_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol_with_module(binary_path, main_source_path,
            worker_source_path, compile_log_path,
            "cobc should compile translated multi_module program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "multi_module_generated.bin",
            "multi_module_generated.txt",
            "translated multi_module binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated multi_module binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(main_source_path, binary_path,
            output_path, log_path);
        if (worker_source_path[0] != '\0')
            test_remove_file(worker_source_path);
        test_remove_directory(directory);
    }
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

FT_TEST(test_cblc_numeric_precision_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "YEAR ABOVE\n"
        "RATE INCREASED\n"
        "FLOAT SHIFT\n"
        "BONUS REACHED\n"
        "DAY TARGET\n";
    if (test_read_text_file("samples/cblc/numeric_precision.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "numeric_precision.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "numeric_precision.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "numeric_precision_generated.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "numeric_precision_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "numeric_precision_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "numeric_precision_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated numeric_precision program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "numeric_precision_generated.bin",
            "numeric_precision_generated.txt",
            "translated numeric_precision binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated numeric_precision binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_floating_point_mix_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "TREND UP\n"
        " 21.5000\n"
        " 24.0000\n"
        " 42.7500\n";
    if (test_read_text_file("samples/cblc/floating_point_mix.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "floating_point_mix.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "floating_point_mix.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "floating_point_mix_generated.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "floating_point_mix_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "floating_point_mix_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "floating_point_mix_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated floating_point_mix program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "floating_point_mix_generated.bin",
            "floating_point_mix_generated.txt",
            "translated floating_point_mix binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated floating_point_mix binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_mixed_numeric_types_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "LIMIT OK\n"
        "  45\n"
        "   250000\n"
        "   5000250000\n";
    if (test_read_text_file("samples/cblc/mixed_numeric_types.cblc",
            cblc_buffer, sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "mixed_numeric_types.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "mixed_numeric_types.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "mixed_numeric_types_generated.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "mixed_numeric_types_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "mixed_numeric_types_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "mixed_numeric_types_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated mixed_numeric_types program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "mixed_numeric_types_generated.bin",
            "mixed_numeric_types_generated.txt",
            "translated mixed_numeric_types binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated mixed_numeric_types binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_textual_priority_mix_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "SCHEDULED\n"
        "NW-01   \n"
        "B\n"
        "  18\n";
    if (test_read_text_file("samples/cblc/textual_priority_mix.cblc",
            cblc_buffer, sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "textual_priority_mix.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "textual_priority_mix.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "textual_priority_mix_generated.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "textual_priority_mix_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "textual_priority_mix_generated.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "textual_priority_mix_generated.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated textual_priority_mix program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "textual_priority_mix_generated.bin",
            "textual_priority_mix_generated.txt",
            "translated textual_priority_mix binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated textual_priority_mix binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_return_boolean_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[64];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "ODD\n";
    if (test_read_text_file("samples/cblc/return_boolean.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "return_boolean.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "return_boolean.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_boolean_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_boolean_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "return_boolean_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_boolean_generated.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated return_boolean program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "return_boolean_generated.bin",
            "return_boolean_generated.txt",
            "translated return_boolean binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated return_boolean binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_return_character_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "A\n";
    if (test_read_text_file("samples/cblc/return_character.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "return_character.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "return_character.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_character_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_character_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "return_character_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_character_generated.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated return_character program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "return_character_generated.bin",
            "return_character_generated.txt",
            "translated return_character binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated return_character binary should emit fetched grade\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_return_numeric_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "   42\n";
    if (test_read_text_file("samples/cblc/return_numeric.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
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
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_numeric_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "return_numeric_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_generated.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated return_numeric program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "return_numeric_generated.bin",
            "return_numeric_generated.txt",
            "translated return_numeric binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: translated return_numeric binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_reverse_group_items_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[8];
    char *generated_cobol;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    if (test_read_text_file("samples/cblc/reverse_group_items.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "reverse_group_items.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "reverse_group_items.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "reverse_group_items_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_group_items_generated.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_group_items_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_group_items_generated.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated reverse_group_items program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "reverse_group_items_generated.bin",
            "reverse_group_items_generated.txt",
            "translated reverse_group_items binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer))
        != FT_SUCCESS)
        goto cleanup;
    if (output_buffer[0] != '\0')
    {
        pf_printf("Assertion failed: translated reverse_group_items binary should not emit output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}
