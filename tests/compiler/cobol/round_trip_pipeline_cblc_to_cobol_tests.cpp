#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include "compatibility/memory_compat.hpp"

#include "round_trip_pipeline_helpers.hpp"

#include <string>

static int run_inline_cblc_to_cobol_execution_test(const char *source,
    const char *base_name, const char *expected_output)
{
    t_cblc_translation_unit unit;
    char directory[256];
    char source_name[64];
    char binary_name[64];
    char output_name[64];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
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
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        if (unit.parse_error_message[0] != '\0')
            std::printf("CBL-C parser diagnostic: %s\n", unit.parse_error_message);
        test_expect_success(FT_FAILURE, "inline CBL-C sample should parse");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "inline CBL-C sample should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_validate_generated_cobol(generated_cobol) != FT_SUCCESS)
    {
        std::printf("Assertion failed: inline generated COBOL should validate\n");
        {
            t_parser diagnostic_parser;
            t_ast_node *diagnostic_program;

            diagnostic_program = NULL;
            parser_init(&diagnostic_parser, generated_cobol);
            if (parser_parse_program(&diagnostic_parser, &diagnostic_program) != FT_SUCCESS)
            {
                if (diagnostic_parser.has_error_token
                    && diagnostic_parser.error_token.lexeme)
                {
                    std::printf("COBOL parser stopped at line %zu, column %zu near '%.*s'\n",
                        diagnostic_parser.error_token.line,
                        diagnostic_parser.error_token.column,
                        static_cast<int>(diagnostic_parser.error_token.length),
                        diagnostic_parser.error_token.lexeme);
                    {
                        size_t source_line;
                        const char *source_cursor;
                        source_line = 1;
                        source_cursor = generated_cobol;
                        while (source_cursor && *source_cursor != '\0'
                            && source_line < diagnostic_parser.error_token.line)
                        {
                            if (*source_cursor == '\n')
                                source_line += 1;
                            source_cursor += 1;
                        }
                        if (source_cursor && *source_cursor != '\0')
                            std::printf("Generated line %zu: %.*s\n", source_line,
                                static_cast<int>(std::strcspn(source_cursor, "\r\n")),
                                source_cursor);
                    }
                }
                else
                    std::printf("COBOL parser stopped without recording an error token\n");
            }
            if (diagnostic_program)
                ast_node_destroy(diagnostic_program);
            parser_dispose(&diagnostic_parser);
        }
        goto cleanup;
    }
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (std::snprintf(source_name, sizeof(source_name), "%s.cob", base_name) < 0)
        goto cleanup;
    if (std::snprintf(binary_name, sizeof(binary_name), "%s.bin", base_name) < 0)
        goto cleanup;
    if (std::snprintf(output_name, sizeof(output_name), "%s.txt", base_name) < 0)
        goto cleanup;
    if (test_join_path(directory, source_name, source_path, sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, binary_name, binary_path, sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(compile_log_path, sizeof(compile_log_path), "%s/%s.log", directory,
            base_name) < 0)
        goto cleanup;
    if (test_join_path(directory, output_name, output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile generated inline COBOL sample") != FT_SUCCESS)
    {
        if (test_read_text_file(compile_log_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
            std::printf("COBOL compiler diagnostics:\n%s\n", output_buffer);
        goto cleanup;
    }
    if (execute_binary(directory, binary_name, output_name,
            "generated inline COBOL binary should execute successfully") != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: generated inline COBOL binary should emit expected output\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path, log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

static const char *g_nested_replacement_exception_source =
    "void main() {\n"
    "    try { throw 1; }\n"
    "    catch (int first_error) {\n"
    "        try { throw 2; }\n"
    "        catch (int second_error) { display(\"REPLACEMENT CAUGHT\"); }\n"
    "    }\n"
    "    display(\"AFTER REPLACEMENT\");\n"
    "    return;\n"
    "}\n";

FT_TEST(test_cblc_nested_replacement_exception_generates_handler_flow)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(g_nested_replacement_exception_source, &unit)
        != FT_SUCCESS)
    {
        std::printf("nested replacement-exception parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        std::printf("nested replacement-exception COBOL generation failed\n");
        goto cleanup;
    }
    if (transpiler_validate_generated_cobol(generated_cobol) != FT_SUCCESS)
    {
        std::printf("Assertion failed: nested replacement-exception COBOL should pass structural validation\n");
        goto cleanup;
    }
    if (!std::strstr(generated_cobol, "DISPLAY \"REPLACEMENT CAUGHT\""))
    {
        std::printf("Assertion failed: replacement exception should retain its nested handler body\n");
        goto cleanup;
    }
    if (!std::strstr(generated_cobol, "DISPLAY \"AFTER REPLACEMENT\""))
    {
        std::printf("Assertion failed: replacement exception should preserve normal continuation after its handler\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_nested_replacement_exception_executes_and_continues)
{
    return (run_inline_cblc_to_cobol_execution_test(
        g_nested_replacement_exception_source,
        "nested_replacement_exception_generated",
        "REPLACEMENT CAUGHT\nAFTER REPLACEMENT\n"));
}

static const char *g_active_raise_guard_source =
    "void main() {\n"
    "    try { throw 1; }\n"
    "    catch (int caught) { display(\"CATCH RAN\"); }\n"
    "    display(\"AFTER THROW\");\n"
    "}\n";

static int build_active_raise_mutation(const char *source,
    std::string *mutated_cobol)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *call_marker;
    size_t insertion_offset;
    int status;

    if (!source || !mutated_cobol)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || transpiler_validate_generated_cobol(generated_cobol) != FT_SUCCESS)
        goto cleanup;
    call_marker = std::strstr(generated_cobol,
        "           PERFORM CBLC-TRY-CALL-");
    if (!call_marker)
        goto cleanup;
    insertion_offset = static_cast<size_t>(call_marker - generated_cobol);
    *mutated_cobol = generated_cobol;
    mutated_cobol->insert(insertion_offset,
        "           MOVE 'Y' TO CBLC-EX-RAISING.\n");
    if (transpiler_validate_generated_cobol(mutated_cobol->c_str()) != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_active_raise_guard_mutated_cobol_validates)
{
    std::string mutated_cobol;

    if (build_active_raise_mutation(g_active_raise_guard_source,
            &mutated_cobol) != FT_SUCCESS
        || mutated_cobol.find("MOVE 'Y' TO CBLC-EX-RAISING.") == std::string::npos
        || mutated_cobol.find("PERFORM CBLC-TERMINATE") == std::string::npos
        || mutated_cobol.find("STOP RUN.") == std::string::npos)
    {
        std::printf("Assertion failed: forced active-raise mutation should validate with its terminal abort path\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_active_raise_guard_terminates_generated_program)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char output_buffer[2048];
    std::string mutated_cobol;
    const char *log_path;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    if (build_active_raise_mutation(g_active_raise_guard_source,
            &mutated_cobol) != FT_SUCCESS)
    {
        std::printf("active-raise mutation fixture failed to build\n");
        goto cleanup;
    }
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "active_raise_guard.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "active_raise_guard.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, "active_raise_guard.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS
        || std::snprintf(compile_log_path, sizeof(compile_log_path),
            "%s/active_raise_guard.log", directory) < 0
        || test_write_text_file(source_path, mutated_cobol.c_str()) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile the forced active-raise mutation") != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "active_raise_guard.bin", "active_raise_guard.txt",
            "forced active-raise guard program should terminate") != FT_SUCCESS
        || test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (!std::strstr(output_buffer, "Fatal CBL-C double exception")
        || std::strstr(output_buffer, "CATCH RAN")
        || std::strstr(output_buffer, "AFTER THROW"))
    {
        std::printf("Assertion failed: active raise must terminate before catch dispatch or source fallthrough\n");
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
    return (status);
}

static int run_two_module_cblc_to_cobol_execution_test(const char *main_source,
    const char *worker_source, const char *base_name, const char *expected_output)
{
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[512];
    char main_file_name[96];
    char worker_file_name[96];
    char binary_file_name[96];
    char output_file_name[96];
    char *generated_main_cobol;
    char *generated_worker_cobol;
    const char *log_path;
    int status;
    int directory_created;
    int context_initialized;

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
    context_initialized = 0;
    status = FT_FAILURE;
    if (std::snprintf(main_file_name, sizeof(main_file_name), "%s_main.cob", base_name) < 0)
        goto cleanup;
    if (std::snprintf(worker_file_name, sizeof(worker_file_name), "%s_worker.cob", base_name) < 0)
        goto cleanup;
    if (std::snprintf(binary_file_name, sizeof(binary_file_name), "%s.bin", base_name) < 0)
        goto cleanup;
    if (std::snprintf(output_file_name, sizeof(output_file_name), "%s.txt", base_name) < 0)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "two-module worker source should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_import_translation_unit_type_stubs(&context, "main_mod", &main_unit)
            != FT_SUCCESS
        || cblc_import_translation_unit_function_stubs(&context, "main_mod", &main_unit)
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "two-module main source should parse") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "two-module main source should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_worker_cobol),
            "two-module worker source should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_main_cobol),
            "two-module main COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_worker_cobol),
            "two-module worker COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, main_file_name, main_source_path,
            sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, worker_file_name, worker_source_path,
            sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, binary_file_name, binary_path, sizeof(binary_path))
        != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, output_file_name, output_path, sizeof(output_path))
        != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(compile_log_path, sizeof(compile_log_path), "%s/%s.log", directory,
            base_name) < 0)
        goto cleanup;
    log_path = compile_log_path;
    if (test_write_text_file(main_source_path, generated_main_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(worker_source_path, generated_worker_cobol) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol_with_module(binary_path, main_source_path,
            worker_source_path, compile_log_path,
            "cobc should compile translated two-module program") != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, binary_file_name, output_file_name,
            "translated two-module binary should execute successfully") != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output, std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated two-module binary should emit expected output\n");
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
    if (context_initialized)
        transpiler_context_dispose(&context);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated copy_file binary should copy input contents\n");
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

#include "round_trip_pipeline_cblc_string_tests.inc"
#include "round_trip_pipeline_cblc_pointer_tests.inc"

#include "round_trip_pipeline_cblc_inline_feature_tests.inc"
FT_TEST(test_cblc_inline_class_methods_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    if (total == 0) {\n"
        "        display(\"CLASS OK\");\n"
        "    } else {\n"
        "        display(\"CLASS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_methods_generated", "CLASS OK\n"));
}

FT_TEST(test_cblc_owned_char_array_new_delete_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    buffer = new char[16];\n"
        "    buffer[0] = 'A';\n"
        "    delete[] buffer;\n"
        "    display(\"CHAR BUFFER LIFETIME OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "owned_char_array_lifecycle", "CHAR BUFFER LIFETIME OK\n"));
}

FT_TEST(test_cblc_class_owned_char_pointer_field_runs_destructor)
{
    const char *source;

    source = "class OwnedBuffer\n"
        "{\n"
        "    private:\n"
        "    char *data;\n"
        "    public:\n"
        "    OwnedBuffer()\n"
        "    {\n"
        "        data = new char[16];\n"
        "    }\n"
        "    ~OwnedBuffer()\n"
        "    {\n"
        "        delete[] data;\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    OwnedBuffer buffer;\n"
        "    display(\"CLASS OWNED BUFFER LIFETIME OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "owned_buf_lifecycle",
            "CLASS OWNED BUFFER LIFETIME OK\n"));
}

FT_TEST(test_cblc_nested_string_class_field_constructs_and_destroys_in_cobol)
{
    const char *source;

    source = "class Person { string name; };\n"
        "Person person;\n"
        "void main() {\n"
        "    display(\"NESTED STRING CLASS LIFETIME OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "nested_string_class_lifecycle", "NESTED STRING CLASS LIFETIME OK\n"));
}

FT_TEST(test_cblc_class_member_array_constructs_and_destroys_each_element)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *element_two_reference;
    const char *element_one_reference;
    const char *throw_payload;
    const char *completed_count_increment;
    const char *scan_cursor;
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    source = "int constructed;\n"
        "int should_throw;\n"
        "int destroyed;\n"
        "class Leaf {\n"
        "    int payload;\n"
        "    public:\n"
        "    Leaf() {\n"
        "        if (should_throw == 1) { throw 9; }\n"
        "        constructed = constructed + 1;\n"
        "        payload = 21;\n"
        "    }\n"
        "    ~Leaf() { destroyed = destroyed + payload; }\n"
        "    int value() { return payload; }\n"
        "};\n"
        "class Owner {\n"
        "    Leaf values[2];\n"
        "};\n"
        "int make_sum() { Owner owner; return 42; }\n"
        "void main() {\n"
        "    int observed;\n"
        "    observed = make_sum();\n"
        "    if (observed == 42 && constructed == 2 && destroyed == 42) {\n"
        "        display(\"CLASS MEMBER ARRAY LIFETIME OK\");\n"
        "    } else {\n"
        "        display(\"CLASS MEMBER ARRAY LIFETIME BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class member array source should parse") != FT_SUCCESS)
    {
        if (unit.parse_error_message[0] != '\0')
            std::printf("CBL-C parser diagnostic: %s\n", unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class member array source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated class member array COBOL should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(std::strstr(generated_cobol,
                "CBLC-EX-RAISING PIC X") != NULL
                && std::strstr(generated_cobol, "CBLC-TERMINATE.") != NULL,
            1, "constructor throws require exception state and fatal termination paragraphs")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(std::strstr(generated_cobol, "OCCURS 2 TIMES") != NULL,
            1, "class member arrays should be emitted as COBOL tables") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(std::strstr(generated_cobol, "CONSTRUCTED") != NULL
                && std::strstr(generated_cobol, "DESTROYED") != NULL,
            1, "per-element constructor and destructor bodies should be emitted")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(std::strstr(generated_cobol,
                "MAKE-SUM-OWNER-VALUES-CBLC-LIVE-COUNT") != NULL
                && std::strstr(generated_cobol,
                    "IF MAKE-SUM-OWNER-VALUES-CBLC-LIVE-COUNT >= 2") != NULL,
            1, "cleanup should dispatch only constructed array elements") != FT_SUCCESS)
        goto cleanup;
    throw_payload = std::strstr(generated_cobol,
        "MOVE 9 TO CBLC-EX-INT-PAYLOAD");
    completed_count_increment = std::strstr(generated_cobol,
        "COMPUTE MAKE-SUM-OWNER-VALUES-CBLC-LIVE-COUNT");
    if (test_expect_int_equal(throw_payload != NULL && completed_count_increment != NULL
                && throw_payload < completed_count_increment,
            1, "a throwing element must transfer before it is marked constructed")
        != FT_SUCCESS)
        goto cleanup;
    element_two_reference = NULL;
    element_one_reference = NULL;
    scan_cursor = generated_cobol;
    while (scan_cursor)
    {
        const char *match;

        match = std::strstr(scan_cursor, "MAKE-SUM-OWNER-VALUES-PAYLOAD(2)");
        if (!match)
            break ;
        element_two_reference = match;
        scan_cursor = match + 1;
    }
    scan_cursor = generated_cobol;
    while (scan_cursor)
    {
        const char *match;

        match = std::strstr(scan_cursor, "MAKE-SUM-OWNER-VALUES-PAYLOAD(1)");
        if (!match)
            break ;
        element_one_reference = match;
        scan_cursor = match + 1;
    }
    if (test_expect_int_equal(element_two_reference != NULL
                && element_one_reference != NULL
                && element_two_reference < element_one_reference,
            1, "member array elements should be destroyed in reverse order") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_lowercase_string_resolves_to_declared_class)
{
    const char *source;

    source = "class string\n"
        "{\n"
        "    private:\n"
        "    char *data;\n"
        "    int stored_length;\n"
        "    int storage_capacity;\n"
        "    public:\n"
        "    string(int initial_length)\n"
        "    {\n"
        "        data = new char[initial_length];\n"
        "        data[0] = 'S';\n"
        "        stored_length = initial_length;\n"
        "        storage_capacity = initial_length;\n"
        "    }\n"
        "    ~string()\n"
        "    {\n"
        "        delete[] data;\n"
        "    }\n"
        "    int len()\n"
        "    {\n"
        "        return stored_length;\n"
        "    }\n"
        "    char first_byte()\n"
        "    {\n"
        "        return data[0];\n"
        "    }\n"
        "};\n"
        "int observed;\n"
        "void main()\n"
        "{\n"
        "    string text(9);\n"
        "    observed = text.len();\n"
        "    if (observed == 9 && text.first_byte() == 'S') {\n"
        "        display(\"LOWERCASE STRING CLASS OK\");\n"
        "    } else {\n"
        "        display(\"LOWERCASE STRING CLASS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "string_class_type", "LOWERCASE STRING CLASS OK\n"));
}

FT_TEST(test_cblc_pointer_owning_class_parameter_copies_and_cleans_up)
{
    const char *source;

    source = "class Buffer\n"
        "{\n"
        "    private:\n"
        "    char *data;\n"
        "    public:\n"
        "    Buffer(char initial)\n"
        "    {\n"
        "        data = new char[1];\n"
        "        data[0] = initial;\n"
        "    }\n"
        "    Buffer(Buffer source)\n"
        "    {\n"
        "        data = new char[1];\n"
        "        data[0] = source.data[0];\n"
        "    }\n"
        "    ~Buffer()\n"
        "    {\n"
        "        delete[] data;\n"
        "    }\n"
        "    char first()\n"
        "    {\n"
        "        return data[0];\n"
        "    }\n"
        "};\n"
        "int read_first(Buffer value)\n"
        "{\n"
        "    return value.first();\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    Buffer source('B');\n"
        "    int observed;\n"
        "    observed = read_first(source);\n"
        "    if (observed == 66) {\n"
        "        display(\"CLASS PARAMETER OWNERSHIP OK\");\n"
        "    } else {\n"
        "        display(\"CLASS PARAMETER OWNERSHIP BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "class_parameter_owned_buffer",
            "CLASS PARAMETER OWNERSHIP OK\n"));
}

FT_TEST(test_cblc_inline_array_access_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int numbers[3];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    numbers[1] = 5;\n"
        "    total = numbers[0] + numbers[1];\n"
        "    if (total == 12) {\n"
        "        display(\"ARRAY OK\");\n"
        "    } else {\n"
        "        display(\"ARRAY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_array_access_generated", "ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_trivial_struct_array_template_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point { int value; };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"STRUCT ARRAY OK\");\n"
        "    return;\n"
        "}\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "trivial_struct_array_template", "STRUCT ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_char_struct_array_template_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point { char name[8]; };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"CHAR STRUCT ARRAY OK\");\n"
        "    return;\n"
        "}\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "char_struct_array_template", "CHAR STRUCT ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_const_string_struct_array_template_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point { const string name(8); };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"CONST STRING STRUCT ARRAY OK\");\n"
        "    return;\n"
        "}\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "const_string_struct_array_template", "CONST STRING STRUCT ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_mutable_string_struct_array_template_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point { string name(8); };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"MUTABLE STRING STRUCT ARRAY OK\");\n"
        "    return;\n"
        "}\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "mutable_string_struct_array_template", "MUTABLE STRING STRUCT ARRAY OK\n"));
}

FT_TEST(test_cblc_imported_mutable_string_struct_array_template_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    worker_source = "struct Point { string name(8); };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"IMPORTED MUTABLE STRING ARRAY OK\");\n"
        "    return;\n"
        "}\n"
        "void worker() { return; }\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
        "imported_mutable_string_struct_array_template",
        "IMPORTED MUTABLE STRING ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_struct_array_constructor_arguments_translate_to_cobol_and_execute)
{
    const char *source;

    source = "class Point {\n"
        "    public:\n"
        "    int value;\n"
        "    Point(int initial) {\n"
        "        value = initial;\n"
        "    }\n"
        "};\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    if (values[1].value == 7) {\n"
        "        display(\"STRUCT ARRAY CTOR OK\");\n"
        "    } else {\n"
        "        display(\"STRUCT ARRAY CTOR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n"
        "Point points[2](7);\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "struct_array_constructor_arguments", "STRUCT ARRAY CTOR OK\n"));
}

FT_TEST(test_cblc_imported_struct_array_constructor_arguments_translate_to_cobol_and_execute)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "Point points[2](7);\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    worker_source = "class Point {\n"
        "    public:\n"
        "    int value;\n"
        "    Point(int initial) {\n"
        "        value = initial;\n"
        "    }\n"
        "};\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    if (values[1].value == 7) {\n"
        "        display(\"IMPORTED STRUCT ARRAY CTOR OK\");\n"
        "    } else {\n"
        "        display(\"IMPORTED STRUCT ARRAY CTOR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n"
        "void worker() { return; }\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
        "imported_struct_array_constructor_arguments",
        "IMPORTED STRUCT ARRAY CTOR OK\n"));
}

FT_TEST(test_cblc_inline_borrowed_pointer_struct_array_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Holder { borrowed int *ptr; };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    if (values[1].ptr == NULL) {\n"
        "        display(\"BORROWED POINTER ARRAY OK\");\n"
        "    } else {\n"
        "        display(\"BORROWED POINTER ARRAY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n"
        "Holder holders[2];\n"
        "void main() {\n"
        "    consume<Holder[2]>(holders);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
        "borrowed_pointer_struct_array", "BORROWED POINTER ARRAY OK\n"));
}

FT_TEST(test_cblc_imported_borrowed_pointer_struct_array_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "Holder holders[2];\n"
        "void main() {\n"
        "    consume<Holder[2]>(holders);\n"
        "    return;\n"
        "}\n";
    worker_source = "struct Holder { borrowed int *ptr; };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    if (values[1].ptr == NULL) {\n"
        "        display(\"IMPORTED BORROWED POINTER ARRAY OK\");\n"
        "    } else {\n"
        "        display(\"IMPORTED BORROWED POINTER ARRAY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n"
        "void worker() { return; }\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
        "imported_borrowed_pointer_struct_array",
        "IMPORTED BORROWED POINTER ARRAY OK\n"));
}

FT_TEST(test_cblc_imported_trivial_struct_array_template_translates_to_cobol_and_executes)
{
    const char *main_source;
    const char *worker_source;

    main_source = "import \"worker_mod\";\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    worker_source = "struct Point { int value; };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"IMPORTED STRUCT ARRAY OK\");\n"
        "    return;\n"
        "}\n"
        "void worker() { return; }\n";
    return (run_two_module_cblc_to_cobol_execution_test(main_source, worker_source,
        "imported_trivial_struct_array_template", "IMPORTED STRUCT ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_functions_without_keyword_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void helper()\n"
        "{\n"
        "    display(\"NO KEYWORD OK\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_functions_without_keyword_generated", "NO KEYWORD OK\n"));
}

FT_TEST(test_cblc_inline_const_values_translates_to_cobol_and_executes)
{
    const char *source;

    source = "const int answer = 42;\n"
        "const string greeting(12) = \"CONST OK\";\n"
        "void main()\n"
        "{\n"
        "    if (answer == 42) {\n"
        "        display(greeting);\n"
        "    } else {\n"
        "        display(\"CONST BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_const_values_generated", "CONST OK\n"));
}

FT_TEST(test_cblc_inline_struct_field_access_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name(8);\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"READY\";\n"
        "    if (point.x == 7) {\n"
        "        display(point.name);\n"
        "    } else {\n"
        "        display(\"STRUCT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_field_generated", "READY\n"));
}

FT_TEST(test_cblc_inline_nested_struct_field_access_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Address\n"
        "{\n"
        "    string city(16);\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    display(person.address.city);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_struct_generated", "GHENT\n"));
}

FT_TEST(test_cblc_inline_variable_index_array_access_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int numbers[3];\n"
        "int index;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    numbers[index] = 9;\n"
        "    total = numbers[index];\n"
        "    if (total == 9) {\n"
        "        display(\"INDEX OK\");\n"
        "    } else {\n"
        "        display(\"INDEX BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_variable_index_generated", "INDEX OK\n"));
}

FT_TEST(test_cblc_inline_constructor_initializer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Person\n"
        "{\n"
        "    private:\n"
        "    const int id;\n"
        "    string name(8);\n"
        "    public:\n"
        "    Person() : name(\"HI\"), id(7) {\n"
        "    }\n"
        "    int current_id() {\n"
        "        return id;\n"
        "    }\n"
        "    void speak() {\n"
        "        display(name);\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    if (person.current_id() == 7) {\n"
        "        person.speak();\n"
        "    } else {\n"
        "        display(\"CTOR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_constructor_initializer_generated", "HI\n"));
}

FT_TEST(test_cblc_inline_parameterized_class_lifecycle_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    void add(int delta)\n"
        "    {\n"
        "        value = value + delta;\n"
        "        return;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter(7);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.add(2);\n"
        "    total = counter.current();\n"
        "    if (total == 9) {\n"
        "        display(\"PARAM CTOR OK\");\n"
        "    } else {\n"
        "        display(\"PARAM CTOR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_parameterized_class_lifecycle_generated", "PARAM CTOR OK\n"));
}

FT_TEST(test_cblc_inline_constructor_overloads_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter()\n"
        "    {\n"
        "        value = 1;\n"
        "    }\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter first;\n"
        "Counter second(7);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = first.current() + second.current();\n"
        "    if (total == 8) {\n"
        "        display(\"CTOR OVERLOAD OK\");\n"
        "    } else {\n"
        "        display(\"CTOR OVERLOAD BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_constructor_overloads_generated", "CTOR OVERLOAD OK\n"));
}

FT_TEST(test_cblc_inline_copy_constructor_overload_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    Counter(Counter other)\n"
        "    {\n"
        "        this.value = other.value;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter seeded(7);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Counter copy(seeded);\n"
        "    total = copy.current();\n"
        "    if (total == 7) {\n"
        "        display(\"COPY CTOR OK\");\n"
        "    } else {\n"
        "        display(\"COPY CTOR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_copy_constructor_overload_generated", "COPY CTOR OK\n"));
}

FT_TEST(test_cblc_inline_struct_return_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string label(8);\n"
        "};\n"
        "Point point;\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    result.label = \"POINT\";\n"
        "    return result;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    point = make_point();\n"
        "    if (point.x == 7) {\n"
        "        display(point.label);\n"
        "    } else {\n"
        "        display(\"STRUCT RET BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_return_generated", "POINT\n"));
}

FT_TEST(test_cblc_inline_class_return_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    void seed()\n"
        "    {\n"
        "        value = 3;\n"
        "        return;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "Counter build_counter()\n"
        "{\n"
        "    Counter result;\n"
        "    result.seed();\n"
        "    return result;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    counter = build_counter();\n"
        "    total = counter.current();\n"
        "    if (total == 3) {\n"
        "        display(\"CLASS RET OK\");\n"
        "    } else {\n"
        "        display(\"CLASS RET BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_return_generated", "CLASS RET OK\n"));
}

FT_TEST(test_cblc_inline_struct_returning_method_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "class Builder\n"
        "{\n"
        "    public:\n"
        "    Point current()\n"
        "    {\n"
        "        Point result;\n"
        "        result.x = 7;\n"
        "        return result;\n"
        "    }\n"
        "};\n"
        "Builder builder;\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point = builder.current();\n"
        "    if (point.x == 7) {\n"
        "        display(\"METHOD STRUCT OK\");\n"
        "    } else {\n"
        "        display(\"METHOD STRUCT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_returning_method_generated", "METHOD STRUCT OK\n"));
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated filter_prefix binary should only copy matching lines\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated reverse_control_flow binary should emit expected DISPLAY output\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated integration_showcase binary should emit expected DISPLAY output\n");
        goto cleanup;
    }
    if (test_read_text_file(accepted_path, accepted_buffer,
            sizeof(accepted_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(accepted_buffer, expected_accepted,
            std::strlen(expected_accepted) + 1) != 0)
    {
        std::printf("Assertion failed: translated integration_showcase binary should record accepted entries\n");
        goto cleanup;
    }
    if (test_read_text_file(rejected_path, rejected_buffer,
            sizeof(rejected_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(rejected_buffer, expected_rejected,
            std::strlen(expected_rejected) + 1) != 0)
    {
        std::printf("Assertion failed: translated integration_showcase binary should record rejected entries\n");
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

#include "round_trip_pipeline_cblc_multi_module_tests.inc"
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated numeric_precision binary should emit expected DISPLAY output\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated floating_point_mix binary should emit expected DISPLAY output\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated mixed_numeric_types binary should emit expected DISPLAY output\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated textual_priority_mix binary should emit expected DISPLAY output\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated return_boolean binary should emit expected DISPLAY output\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated return_character binary should emit fetched grade\n");
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
    if (std::strncmp(output_buffer, expected_output,
            std::strlen(expected_output) + 1) != 0)
    {
        std::printf("Assertion failed: translated return_numeric binary should emit expected DISPLAY output\n");
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
        std::printf("Assertion failed: translated reverse_group_items binary should not emit output\n");
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
