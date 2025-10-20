#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

#include "round_trip_pipeline_helpers.hpp"

static int round_trip_cobol_fixture(const char *fixture_path,
    const char *fixture_label, char **round_trip_cobol)
{
    t_transpiler_context context;
    t_parser parser;
    t_ast_node *program;
    t_cblc_translation_unit unit;
    char cobol_buffer[32768];
    char *generated_cblc;
    char *generated_cobol;
    int context_initialized;
    int status;

    if (!round_trip_cobol)
        return (FT_FAILURE);
    *round_trip_cobol = NULL;
    generated_cblc = NULL;
    generated_cobol = NULL;
    program = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_read_text_file(fixture_path, cobol_buffer,
            sizeof(cobol_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_COBOL,
        TRANSPILE_LANGUAGE_CBL_C);
    context.active_source_text = cobol_buffer;
    context.active_source_length = ft_strlen(cobol_buffer);
    transpiler_context_clear_comments(&context);
    parser_init_with_context(&parser, cobol_buffer, &context);
    if (test_expect_success(parser_parse_program(&parser, &program),
            fixture_label) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        goto cleanup;
    }
    parser_dispose(&parser);
    if (test_expect_success(program ? FT_SUCCESS : FT_FAILURE,
            "fixture should produce an AST") != FT_SUCCESS)
        goto cleanup;
    transpiler_context_reset_unit_state(&context);
    context.active_source_text = cobol_buffer;
    context.active_source_length = ft_strlen(cobol_buffer);
    if (test_expect_success(transpiler_semantics_analyze_program(&context,
                program), "semantic analysis should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_cobol_program_to_cblc(&context, program,
                &generated_cblc),
            "fixture should translate to CBL-C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(generated_cblc ? FT_SUCCESS : FT_FAILURE,
            "generated CBL-C should be available") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cblc(generated_cblc),
            "generated CBL-C should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_has_errors(&context)
            ? FT_FAILURE : FT_SUCCESS,
            "context should not record errors during translation")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(generated_cblc, &unit),
            "round trip CBL-C should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "round trip COBOL generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(generated_cobol ? FT_SUCCESS : FT_FAILURE,
            "round trip COBOL should be available") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "round trip COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    *round_trip_cobol = generated_cobol;
    generated_cobol = NULL;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    if (generated_cblc)
        cma_free(generated_cblc);
    if (program)
        ast_node_destroy(program);
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cobol_reverse_control_flow_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[64];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "0011\n";
    if (round_trip_cobol_fixture("samples/cobol/reverse_control_flow.cob",
            "reverse_control_flow.cob should parse", &round_trip_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "reverse_control_flow_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_control_flow_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_control_flow_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_control_flow_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped COBOL") != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "reverse_control_flow_round_trip.bin",
            "reverse_control_flow_round_trip.txt",
            "round-tripped binary should execute successfully") != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_copy_file_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char input_path[256];
    char copied_path[256];
    char output_buffer[512];
    char *round_trip_cobol;
    const char *input_contents;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/copy_file.cob",
            "copy_file.cob should parse", &round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "copy_file_round_trip.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "copy_file_round_trip.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "copy_file_round_trip.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped copy_file program") != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "input.txt", input_path,
            sizeof(input_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, input_contents) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "output.txt", copied_path,
            sizeof(copied_path)) != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "copy_file_round_trip.bin", NULL,
            "round-tripped copy_file binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(copied_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped copy_file binary should copy input contents\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_filter_prefix_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char input_path[256];
    char filtered_path[256];
    char output_buffer[512];
    char *round_trip_cobol;
    const char *input_contents;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    input_path[0] = '\0';
    filtered_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    input_contents = "ALLOW-FIRST\nDENY-ENTRY\nALLOW-SECOND\nDISCARD\n";
    expected_output = "ALLOW-FIRST\nALLOW-SECOND\n";
    if (round_trip_cobol_fixture("samples/cobol/filter_prefix.cob",
            "filter_prefix.cob should parse", &round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "filter_prefix_round_trip.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "filter_prefix_round_trip.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "filter_prefix_round_trip.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped filter_prefix program")
        != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "source.dat", input_path,
            sizeof(input_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, input_contents) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "target.dat", filtered_path,
            sizeof(filtered_path)) != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "filter_prefix_round_trip.bin", NULL,
            "round-tripped filter_prefix binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(filtered_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped filter_prefix binary should only copy allowed lines\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_integration_showcase_round_trips_and_executes)
{
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
    char *round_trip_cobol;
    const char *input_contents;
    const char *expected_output;
    const char *expected_accepted;
    const char *expected_rejected;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/integration_showcase.cob",
            "integration_showcase.cob should parse", &round_trip_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "integration_showcase_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "integration_showcase_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "integration_showcase_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "integration_showcase_round_trip.txt",
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
            "cobc should compile round-tripped integration_showcase program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "integration_showcase_round_trip.bin",
            "integration_showcase_round_trip.txt",
            "round-tripped integration_showcase binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped integration_showcase binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    if (test_read_text_file(accepted_path, accepted_buffer,
            sizeof(accepted_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(accepted_buffer, expected_accepted,
            ft_strlen(expected_accepted) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped integration_showcase binary should record accepted entries\n");
        goto cleanup;
    }
    if (test_read_text_file(rejected_path, rejected_buffer,
            sizeof(rejected_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(rejected_buffer, expected_rejected,
            ft_strlen(expected_rejected) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped integration_showcase binary should record rejected entries\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_multi_module_round_trips_and_executes)
{
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *round_trip_main;
    char *round_trip_worker;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_main = NULL;
    round_trip_worker = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/multi_module_main.cob",
            "multi_module_main.cob should parse", &round_trip_main)
        != FT_SUCCESS)
        goto cleanup;
    if (round_trip_cobol_fixture("samples/cobol/multi_module_worker.cob",
            "multi_module_worker.cob should parse", &round_trip_worker)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "multi_module_main_round_trip.cob",
            main_source_path, sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_worker_round_trip.cob",
            worker_source_path, sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "multi_module_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "multi_module_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(main_source_path, round_trip_main) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(worker_source_path, round_trip_worker)
        != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol_with_module(binary_path, main_source_path,
            worker_source_path, compile_log_path,
            "cobc should compile round-tripped multi_module program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "multi_module_round_trip.bin",
            "multi_module_round_trip.txt",
            "round-tripped multi_module binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped multi_module binary should emit expected DISPLAY output\n");
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
    if (round_trip_worker)
        cma_free(round_trip_worker);
    if (round_trip_main)
        cma_free(round_trip_main);
    return (status);
}

FT_TEST(test_cobol_numeric_precision_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/numeric_precision.cob",
            "numeric_precision.cob should parse", &round_trip_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "numeric_precision_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "numeric_precision_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "numeric_precision_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "numeric_precision_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped numeric_precision program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "numeric_precision_round_trip.bin",
            "numeric_precision_round_trip.txt",
            "round-tripped numeric_precision binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped numeric_precision binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_floating_point_mix_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/floating_point_mix.cob",
            "floating_point_mix.cob should parse", &round_trip_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "floating_point_mix_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "floating_point_mix_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "floating_point_mix_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "floating_point_mix_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped floating_point_mix program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "floating_point_mix_round_trip.bin",
            "floating_point_mix_round_trip.txt",
            "round-tripped floating_point_mix binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped floating_point_mix binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_mixed_numeric_types_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/mixed_numeric_types.cob",
            "mixed_numeric_types.cob should parse", &round_trip_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "mixed_numeric_types_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "mixed_numeric_types_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "mixed_numeric_types_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "mixed_numeric_types_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped mixed_numeric_types program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "mixed_numeric_types_round_trip.bin",
            "mixed_numeric_types_round_trip.txt",
            "round-tripped mixed_numeric_types binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped mixed_numeric_types binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_textual_priority_mix_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
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
    if (round_trip_cobol_fixture("samples/cobol/textual_priority_mix.cob",
            "textual_priority_mix.cob should parse", &round_trip_cobol)
        != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "textual_priority_mix_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "textual_priority_mix_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "textual_priority_mix_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "textual_priority_mix_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped textual_priority_mix program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "textual_priority_mix_round_trip.bin",
            "textual_priority_mix_round_trip.txt",
            "round-tripped textual_priority_mix binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped textual_priority_mix binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_return_boolean_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[64];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "ODD\n";
    if (round_trip_cobol_fixture("samples/cobol/return_boolean.cob",
            "return_boolean.cob should parse", &round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_boolean_round_trip.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_boolean_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "return_boolean_round_trip.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_boolean_round_trip.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped return_boolean program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "return_boolean_round_trip.bin",
            "return_boolean_round_trip.txt",
            "round-tripped return_boolean binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped return_boolean binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_return_character_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "A\n";
    if (round_trip_cobol_fixture("samples/cobol/return_character.cob",
            "return_character.cob should parse", &round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_character_round_trip.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_character_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "return_character_round_trip.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_character_round_trip.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped return_character program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "return_character_round_trip.bin",
            "return_character_round_trip.txt",
            "round-tripped return_character binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped return_character binary should emit fetched grade\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_return_numeric_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[128];
    char *round_trip_cobol;
    const char *expected_output;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = "   42\n";
    if (round_trip_cobol_fixture("samples/cobol/return_numeric.cob",
            "return_numeric.cob should parse", &round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "return_numeric_round_trip.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "return_numeric_round_trip.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "return_numeric_round_trip.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped return_numeric program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "return_numeric_round_trip.bin",
            "return_numeric_round_trip.txt",
            "round-tripped return_numeric binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: round-tripped return_numeric binary should emit expected DISPLAY output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}

FT_TEST(test_cobol_reverse_group_items_round_trips_and_executes)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[8];
    char *round_trip_cobol;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    round_trip_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    if (round_trip_cobol_fixture("samples/cobol/reverse_group_items.cob",
            "reverse_group_items.cob should parse", &round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "reverse_group_items_round_trip.cob",
            source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, round_trip_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_group_items_round_trip.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "reverse_group_items_round_trip.bin",
            binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "reverse_group_items_round_trip.txt",
            output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile round-tripped reverse_group_items program")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "reverse_group_items_round_trip.bin",
            "reverse_group_items_round_trip.txt",
            "round-tripped reverse_group_items binary should execute successfully")
        != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer))
        != FT_SUCCESS)
        goto cleanup;
    if (output_buffer[0] != '\0')
    {
        pf_printf("Assertion failed: round-tripped reverse_group_items binary should not emit output\n");
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
    if (round_trip_cobol)
        cma_free(round_trip_cobol);
    return (status);
}
