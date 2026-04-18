#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include "compatibility/memory_compat.hpp"

#include "round_trip_pipeline_helpers.hpp"

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
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "inline CBL-C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "inline CBL-C sample should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "inline generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
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
        goto cleanup;
    if (execute_binary(directory, binary_name, output_name,
            "generated inline COBOL binary should execute successfully") != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(output_buffer, expected_output, std::strlen(expected_output) + 1) != 0)
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
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "two-module main source should parse") != FT_SUCCESS)
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
