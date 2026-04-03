#include "test_suites.hpp"

#include "compatibility/memory_compat.hpp"

FT_TEST(test_transpiler_validation_accepts_valid_cblc)
{
    const char *source;

    source = "function void main()\n"
        "{\n"
        "    display(\"HELLO\");\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept valid CBL-C") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_cblc_without_return)
{
    const char *source;

    source = "function int main()\n"
        "{\n"
        "    display(\"HELLO\");\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject CBL-C without return\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_declaration)
{
    const char *source;

    source = "string greeting[16];\n"
        "function void main()\n"
        "{\n"
        "    greeting = \"HI\";\n"
        "    display(greeting.len);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string declarations") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_assignment_and_length_usage)
{
    const char *source;

    source = "string greeting[12];\n"
        "string copy[12];\n"
        "int total;\n"
        "function void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    copy = greeting;\n"
        "    total = greeting.len + copy.len;\n"
        "    display(copy);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string assignments and length usage")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_const_declarations)
{
    const char *source;

    source = "const int answer = 42;\n"
        "const char marker = 'A';\n"
        "const string greeting[12] = \"HELLO\";\n"
        "function void main()\n"
        "{\n"
        "    display(answer);\n"
        "    display(marker);\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept const declarations") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_without_initializer)
{
    const char *source;

    source = "const int answer;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject const declarations without initializer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_reassignment)
{
    const char *source;

    source = "const int answer = 42;\n"
        "function void main()\n"
        "{\n"
        "    answer = 7;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject reassignment to const declarations\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_const_char_buffer_declaration)
{
    const char *source;

    source = "const char label[8] = \"HELLO\";\n"
        "function void main()\n"
        "{\n"
        "    display(label);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept const char buffer declarations")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_const_string_length_usage)
{
    const char *source;

    source = "const string greeting[12] = \"HELLO\";\n"
        "int total;\n"
        "function void main()\n"
        "{\n"
        "    total = greeting.len;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept reading const string length")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_char_scalar_reassignment)
{
    const char *source;

    source = "const char marker = 'A';\n"
        "function void main()\n"
        "{\n"
        "    marker = 'B';\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject reassignment to const char declarations\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_string_reassignment)
{
    const char *source;

    source = "const string greeting[12] = \"HELLO\";\n"
        "function void main()\n"
        "{\n"
        "    greeting = \"BYE\";\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject reassignment to const string declarations\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_strcpy_into_const_char_buffer)
{
    const char *source;

    source = "const char label[8] = \"HELLO\";\n"
        "function void main()\n"
        "{\n"
        "    std::strcpy(label, \"BYE\");\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject strcpy into const char buffers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_strlen_string_literal_assignment)
{
    const char *source;

    source = "int total;\n"
        "function void main()\n"
        "{\n"
        "    total = std::strlen(\"hello world\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept std::strlen with string literals")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_parse_translation_unit_records_imports)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "import \"helper.cblc\";\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "import statements should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.import_count != 1)
    {
        std::printf("Assertion failed: expected exactly one import\n");
        goto cleanup;
    }
    if (std::strncmp(unit.imports[0].path, "helper.cblc", TRANSPILE_FILE_PATH_MAX) != 0)
    {
        std::printf("Assertion failed: import path should be recorded\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_copy_includes)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "copy \"shared-status\";\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy directives should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.copy_include_count != 1)
    {
        std::printf("Assertion failed: expected exactly one copy include\\n");
        goto cleanup;
    }
    if (std::strncmp(unit.copy_includes[0].name, "shared-status",
            TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: copy include should record source name\\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_tracks_multiple_functions)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "function void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "function void main()\n"
        "{\n"
        "    display(\"SECOND\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "multiple functions should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.function_count != 2)
    {
        std::printf("Assertion failed: expected two functions in translation unit\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[0].source_name, "helper", TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: first function should be named 'helper'\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[1].source_name, "main", TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: second function should be named 'main'\n");
        goto cleanup;
    }
    if (unit.functions[0].statement_count != 2)
    {
        std::printf("Assertion failed: helper function should retain its statements\n");
        goto cleanup;
    }
    if (unit.functions[1].statement_count != 2)
    {
        std::printf("Assertion failed: main function should retain its statements\n");
        goto cleanup;
    }
    if (unit.entry_function_index != 1)
    {
        std::printf("Assertion failed: entry function should resolve to 'main'\n");
        goto cleanup;
    }
    if (std::strncmp(unit.program_name, "MAIN", TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: program name should match entry function\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_string_group)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "string greeting[8];\n"
        "function void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string declarations should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string declarations should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 GREETING.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 GREETING-LEN PIC 9(4) COMP VALUE 8.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 GREETING-BUF PIC X(8).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING buffer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY GREETING-BUF(1:GREETING-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display buffer slice using length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_copy_includes)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "copy \"shared-status\";\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy directives should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "copy directives should regenerate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
    {
        std::printf("Assertion failed: expected COBOL generation to produce text\\n");
        goto cleanup;
    }
    if (!std::strstr(generated_cobol, "       COPY SHARED-STATUS."))
    {
        std::printf("Assertion failed: COBOL should include COPY directive for shared-status\\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_multiple_paragraphs)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "function void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "function void main()\n"
        "{\n"
        "    display(\"SECOND\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "multiple function program should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "multiple function program should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "HELPER.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: helper paragraph should be emitted\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MAIN.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: main paragraph should be emitted\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY \"FIRST\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: helper statements should be scoped to helper paragraph\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY \"SECOND\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: main statements should be scoped to main paragraph\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_perform_and_call_statements)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "function void helper()\n"
        "{\n"
        "    display(\"LOCAL\");\n"
        "    return;\n"
        "}\n"
        "function void main()\n"
        "{\n"
        "    helper();\n"
        "    worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "function void worker()\n"
        "{\n"
        "    display(\"WORKER\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module should parse") != FT_SUCCESS)
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
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "main module should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM HELPER", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: local call should emit PERFORM statement\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "CALL 'WORKER'", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: external call should emit CALL statement\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_handles_string_assignments_and_length_computations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "string greeting[8];\n"
        "string target[8];\n"
        "int total;\n"
        "function void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    target = greeting;\n"
        "    total = greeting.len + target.len;\n"
        "    display(target);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string programs should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string programs should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE \"HELLO\" TO GREETING-BUF",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: literal assignment should target GREETING-BUF\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE 5 TO GREETING-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: literal assignment should update GREETING-LEN\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE GREETING TO TARGET",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: string to string assignment should move groups\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "COMPUTE TOTAL = GREETING-LEN + TARGET-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: length expressions should translate into COMPUTE\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY TARGET-BUF(1:TARGET-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: DISPLAY of string should use buffer slice\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY TOTAL",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: DISPLAY of int should emit standard form\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_resolve_calls_reports_missing_function)
{
    const char *source;
    t_cblc_translation_unit unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    source = "function void main()\n"
        "{\n"
        "    missing();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "module should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: unresolved function should trigger failure\n");
        goto cleanup;
    }
    if (!transpiler_context_has_errors(&context))
    {
        std::printf("Assertion failed: unresolved function should record diagnostics\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_handles_multiplication_and_division)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int left;\n"
        "int right;\n"
        "int product;\n"
        "int quotient;\n"
        "function void main()\n"
        "{\n"
        "    product = left * right;\n"
        "    quotient = product / right;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "numeric declarations should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "numeric declarations should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE PRODUCT = LEFT * RIGHT", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: multiplication should translate into COMPUTE statement\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE QUOTIENT = PRODUCT / RIGHT", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: division should translate into COMPUTE statement\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_folds_strlen_string_literal_to_constant)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int total;\n"
        "function void main()\n"
        "{\n"
        "    total = std::strlen(\"hello world\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "std::strlen with string literal should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "std::strlen with string literal should convert to COBOL")
        != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = 11",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should fold std::strlen string literals to constants\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_cobol, "CALL 'CBLC-STRLEN'",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should not call CBLC-STRLEN for string literals\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_struct_fields)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name[8];\n"
        "};\n"
        "Point point;\n"
        "function void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"HI\";\n"
        "    display(point.x);\n"
        "    display(point.name);\n"
        "    display(point.name.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct program should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 2,
            "builtin string plus one user struct type should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_count, 3,
            "struct instance should register parent plus field items") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[0].source_name, "point",
            sizeof(unit.data_items[0].source_name)) != 0
        || unit.data_items[0].kind != CBLC_DATA_KIND_STRUCT)
    {
        std::printf("Assertion failed: first data item should be the struct instance\n");
        goto cleanup;
    }
    if (std::strncmp(unit.data_items[1].source_name, "point.x",
            sizeof(unit.data_items[1].source_name)) != 0
        || unit.data_items[1].kind != CBLC_DATA_KIND_INT)
    {
        std::printf("Assertion failed: struct int field should be registered as data item\n");
        goto cleanup;
    }
    if (std::strncmp(unit.data_items[2].source_name, "point.name",
            sizeof(unit.data_items[2].source_name)) != 0
        || unit.data_items[2].kind != CBLC_DATA_KIND_STRING)
    {
        std::printf("Assertion failed: struct string field should be registered as data item\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_struct_groups)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name[8];\n"
        "};\n"
        "Point point;\n"
        "function void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"HI\";\n"
        "    display(point.name);\n"
        "    display(point.name.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "struct COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 POINT.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit struct group header\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 POINT-X PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit int struct field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 POINT-NAME.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit string struct field group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "10 POINT-NAME-LEN PIC 9(4) COMP VALUE 0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit struct string length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE POINT-X = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign to struct int field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY POINT-NAME-BUF(1:POINT-NAME-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display struct string field via slice\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY POINT-NAME-LEN", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display struct string length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_main_and_helpers)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "string greeting[8];\n"
        "int counter;\n"
        "function void helper()\n"
        "{\n"
        "    display(\"HI\");\n"
        "    return;\n"
        "}\n"
        "function void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    counter = 1 + 2;\n"
        "    display(greeting);\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "sample CBL-C should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate C code") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "#include <stddef.h>", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should include stddef header\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "static size_t greeting_len = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare greeting length variable\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "static char greeting_buf[8] = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare greeting buffer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void helper(void);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should prototype helper function\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "int main(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should define an int main wrapper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(greeting_buf, 8, &greeting_len, \"HELLO\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign greeting literal via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(greeting_buf, greeting_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display greeting using helper\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_omits_main_for_library)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "function void helper()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "library CBL-C should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate library code") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (ft_strnstr(generated_c, "int main(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: library output should not define main\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void helper(void);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: library output should declare helper prototype\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void helper(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: library output should define helper function\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_struct_types_and_field_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name[8];\n"
        "};\n"
        "Point point;\n"
        "function void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"HELLO\";\n"
        "    display(point.name);\n"
        "    display(point.name.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "struct C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "typedef struct s_Point", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit struct typedef\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "struct { size_t len; char buf[8]; } name;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit nested string storage in struct\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "static t_Point point = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit struct instance global\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "point.x = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign to struct int field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(point.name.buf, 8, &point.name.len, \"HELLO\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign struct string field via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(point.name.buf, point.name.len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display struct string field via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_size(point.name.len);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display struct string length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_nested_struct_fields)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city[16];\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    person.age = 7;\n"
        "    display(person.address.city);\n"
        "    display(person.address.city.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct program should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 3,
            "builtin string plus two user struct types should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_count, 4,
            "nested struct instance should register aggregate path items recursively") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[1].source_name, "person.address",
            sizeof(unit.data_items[1].source_name)) != 0
        || unit.data_items[1].kind != CBLC_DATA_KIND_STRUCT)
    {
        std::printf("Assertion failed: nested struct field should be registered as aggregate data item\n");
        goto cleanup;
    }
    if (std::strncmp(unit.data_items[2].source_name, "person.address.city",
            sizeof(unit.data_items[2].source_name)) != 0
        || unit.data_items[2].kind != CBLC_DATA_KIND_STRING)
    {
        std::printf("Assertion failed: nested primitive field should be registered recursively\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_nested_struct_groups)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city[16];\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    display(person.address.city);\n"
        "    display(person.address.city.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested struct COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "05 PERSON-ADDRESS.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested struct group header\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "10 PERSON-ADDRESS-CITY.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested string group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "15 PERSON-ADDRESS-CITY-LEN PIC 9(4) COMP VALUE 0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested string length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY PERSON-ADDRESS-CITY-BUF(1:PERSON-ADDRESS-CITY-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display nested string field via slice\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_nested_struct_types_and_field_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city[16];\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    display(person.address.city);\n"
        "    display(person.address.city.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "nested struct C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "typedef struct s_Address", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit nested member type typedef\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "t_Address address;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should embed nested struct field type\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(person.address.city.buf, 16, &person.address.city.len, \"GHENT\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign nested string field via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(person.address.city.buf, person.address.city.len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display nested string field via helper\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_class_lifecycle_metadata)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person();\n"
        "    ~Person();\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class lifecycle sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 2,
            "builtin string plus one class type should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].is_class, 1,
            "type should be marked as class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].has_default_constructor, 1,
            "class should record default constructor") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].has_destructor, 1,
            "class should record destructor") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_automatic_class_lifecycle)
{
    const char *source;
    const char *needle;
    const char *scan;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;
    int count;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person();\n"
        "    ~Person();\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class lifecycle C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "class lifecycle C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    needle = "person.name.len = 0;";
    count = 0;
    scan = generated_c;
    while ((scan = std::strstr(scan, needle)) != NULL)
    {
        count += 1;
        scan += std::strlen(needle);
    }
    if (test_expect_int_equal(count, 2,
            "generated C should inject lifecycle before entry and before return") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_c, "memset(person.name.buf, 0, sizeof(person.name.buf));",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should zero string buffer during lifecycle\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_automatic_class_lifecycle)
{
    const char *source;
    const char *needle;
    const char *scan;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;
    int count;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person();\n"
        "    ~Person();\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class lifecycle COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class lifecycle COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    needle = "MOVE 0 TO PERSON-NAME-LEN.";
    count = 0;
    scan = generated_cobol;
    while ((scan = std::strstr(scan, needle)) != NULL)
    {
        count += 1;
        scan += std::strlen(needle);
    }
    if (test_expect_int_equal(count, 2,
            "generated COBOL should inject lifecycle before entry and before return") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE SPACES TO PERSON-NAME-BUF.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should blank string buffer during lifecycle\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_custom_class_lifecycle_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "custom lifecycle C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "custom lifecycle C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(person.name.buf, 8, &person.name.len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit custom constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(person.name.buf, 8, &person.name.len, \"BYE\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit custom destructor body\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_custom_class_lifecycle_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "custom lifecycle COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "custom lifecycle COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit custom constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"BYE\" TO PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit custom destructor body\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_local_class_lifecycle)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "function void main()\n"
        "{\n"
        "    Person person;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local class lifecycle C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "local class lifecycle C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "t_Person main__person = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare local class storage inside function\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(main__person.name.buf, 8, &main__person.name.len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(main__person.name.buf, 8, &main__person.name.len, \"BYE\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local destructor body before return\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_local_class_lifecycle)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "function void main()\n"
        "{\n"
        "    Person person;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local class lifecycle COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "local class lifecycle COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 MAIN-PERSON.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit function-owned class storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO MAIN-PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit local constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"BYE\" TO MAIN-PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit local destructor body before return\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_nested_block_local_class_cleanup_order)
{
    const char *source;
    const char *first_ctor;
    const char *first_dtor;
    const char *second_ctor;
    const char *second_dtor;
    const char *p_first_ctor;
    const char *p_first_dtor;
    const char *p_second_ctor;
    const char *p_second_dtor;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "function void main()\n"
        "{\n"
        "    {\n"
        "        Person first;\n"
        "    }\n"
        "    Person second;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested block local class C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "nested block local class C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    first_ctor = "cblc_string_assign_literal(main__first.name.buf, 8, &main__first.name.len, \"HI\");";
    first_dtor = "cblc_string_assign_literal(main__first.name.buf, 8, &main__first.name.len, \"BYE\");";
    second_ctor = "cblc_string_assign_literal(main__second.name.buf, 8, &main__second.name.len, \"HI\");";
    second_dtor = "cblc_string_assign_literal(main__second.name.buf, 8, &main__second.name.len, \"BYE\");";
    p_first_ctor = std::strstr(generated_c, first_ctor);
    p_first_dtor = std::strstr(generated_c, first_dtor);
    p_second_ctor = std::strstr(generated_c, second_ctor);
    p_second_dtor = std::strstr(generated_c, second_dtor);
    if (!p_first_ctor || !p_first_dtor || !p_second_ctor || !p_second_dtor)
    {
        std::printf("Assertion failed: generated C should emit all nested block lifecycle statements\n");
        goto cleanup;
    }
    if (!(p_first_ctor < p_first_dtor && p_first_dtor < p_second_ctor && p_second_ctor < p_second_dtor))
    {
        std::printf("Assertion failed: generated C should destroy inner block object before later outer object construction/cleanup\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_nested_block_local_class_cleanup_order)
{
    const char *source;
    const char *first_ctor;
    const char *first_dtor;
    const char *second_ctor;
    const char *second_dtor;
    const char *p_first_ctor;
    const char *p_first_dtor;
    const char *p_second_ctor;
    const char *p_second_dtor;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name[8];\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "function void main()\n"
        "{\n"
        "    {\n"
        "        Person first;\n"
        "    }\n"
        "    Person second;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested block local class COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested block local class COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    first_ctor = "MOVE \"HI\" TO MAIN-FIRST-NAME-BUF.";
    first_dtor = "MOVE \"BYE\" TO MAIN-FIRST-NAME-BUF.";
    second_ctor = "MOVE \"HI\" TO MAIN-SECOND-NAME-BUF.";
    second_dtor = "MOVE \"BYE\" TO MAIN-SECOND-NAME-BUF.";
    p_first_ctor = std::strstr(generated_cobol, first_ctor);
    p_first_dtor = std::strstr(generated_cobol, first_dtor);
    p_second_ctor = std::strstr(generated_cobol, second_ctor);
    p_second_dtor = std::strstr(generated_cobol, second_dtor);
    if (!p_first_ctor || !p_first_dtor || !p_second_ctor || !p_second_dtor)
    {
        std::printf("Assertion failed: generated COBOL should emit all nested block lifecycle statements\n");
        goto cleanup;
    }
    if (!(p_first_ctor < p_first_dtor && p_first_dtor < p_second_ctor && p_second_ctor < p_second_dtor))
    {
        std::printf("Assertion failed: generated COBOL should destroy inner block object before later outer object construction/cleanup\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_registers_builtin_string_class)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "string greeting[8];\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string class sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 1,
            "builtin string class should be registered even without user classes") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[0].source_name, "string",
            sizeof(unit.struct_types[0].source_name)) != 0)
    {
        std::printf("Assertion failed: builtin string class should be registered under the string name\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.struct_types[0].is_class, 1,
            "builtin string should be marked as a class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[0].is_builtin, 1,
            "builtin string should be marked as builtin") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[0].declared_type_name, "string",
            sizeof(unit.data_items[0].declared_type_name)) != 0)
    {
        std::printf("Assertion failed: string variables should record the string class as their declared type\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_local_string_lifecycle)
{
    const char *source;
    const char *ctor_reset;
    const char *assign_call;
    const char *dtor_reset;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "function void main()\n"
        "{\n"
        "    string greeting[8];\n"
        "    greeting = \"HI\";\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local string lifecycle sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "local string lifecycle sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "size_t main__greeting_len = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local string length storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "char main__greeting_buf[8] = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local string buffer storage\n");
        goto cleanup;
    }
    ctor_reset = std::strstr(generated_c, "memset(main__greeting_buf, 0, sizeof(main__greeting_buf));");
    if (!ctor_reset)
    {
        std::printf("Assertion failed: generated C should construct local string objects\n");
        goto cleanup;
    }
    assign_call = std::strstr(ctor_reset, "cblc_string_assign_literal(main__greeting_buf, 8, &main__greeting_len, \"HI\");");
    if (!assign_call)
    {
        std::printf("Assertion failed: generated C should preserve local string assignment after construction\n");
        goto cleanup;
    }
    dtor_reset = std::strstr(assign_call, "memset(main__greeting_buf, 0, sizeof(main__greeting_buf));");
    if (!dtor_reset)
    {
        std::printf("Assertion failed: generated C should destroy local string objects before return\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    int value;\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class method sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].method_count, 2,
            "class should record both methods") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[1].methods[0].source_name, "reset",
            sizeof(unit.struct_types[1].methods[0].source_name)) != 0)
    {
        std::printf("Assertion failed: first class method should be reset\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_VOID, "reset should be a void method") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[1].methods[1].source_name, "current",
            sizeof(unit.struct_types[1].methods[1].source_name)) != 0)
    {
        std::printf("Assertion failed: second class method should be current\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.struct_types[1].methods[1].return_kind,
            CBLC_FUNCTION_RETURN_INT, "current should be an int method") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Counter\n"
        "{\n"
        "    int value;\n"
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
        "function void main()\n"
        "{\n"
        "    counter.value = 7;\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class method C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "class method C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "counter.value = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should inline void method body against receiver\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = counter.value;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should inline int-returning method body against receiver\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Counter\n"
        "{\n"
        "    int value;\n"
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
        "function void main()\n"
        "{\n"
        "    counter.value = 7;\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class method COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class method COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-VALUE = 0.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should inline void method body against receiver\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = COUNTER-VALUE.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should inline int-returning method body against receiver\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_constructor_initializer_list_in_declaration_order)
{
    const char *source;
    const char *first_assign;
    const char *second_assign;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Pair\n"
        "{\n"
        "    int first;\n"
        "    int second;\n"
        "    Pair() : second(2), first(1) {\n"
        "    }\n"
        "};\n"
        "Pair pair;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "initializer list order sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "initializer list order sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    first_assign = std::strstr(generated_c, "pair.first = 1;");
    second_assign = std::strstr(generated_c, "pair.second = 2;");
    if (!first_assign || !second_assign)
    {
        std::printf("Assertion failed: generated C should emit all constructor initializer list assignments\n");
        goto cleanup;
    }
    if (!(first_assign < second_assign))
    {
        std::printf("Assertion failed: generated C should initialize members in declaration order\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_parses_const_member_initializer_list)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    string name[8];\n"
        "    Person() : name(\"HI\"), id(7) {\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "const member initializer list sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "const member initializer list sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "person.id = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should initialize const int members from initializer list\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(person.name.buf, 8, &person.name.len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should initialize string members from initializer list\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_const_member_initialized_once_in_constructor_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() {\n"
        "        id = 7;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "const member should be assignable once in constructor body") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_const_member_reassignment_in_constructor)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() : id(7) {\n"
        "        id = 8;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: const member should not be assignable more than once during construction\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_const_member_assignment_outside_constructor)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() : id(7) {\n"
        "    }\n"
        "    void reset() {\n"
        "        id = 0;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: const member should not be assignable outside constructors\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_missing_const_member_initialization)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() {\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "function void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: const member should require exactly one construction-time initialization\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_valid_cobol)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. VALIDATE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           DISPLAY \"HELLO\".\n"
        "           STOP RUN.\n";
    if (test_expect_success(transpiler_validate_generated_cobol(source),
            "validator should accept valid COBOL") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_working_storage_program)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DEMO.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 GREETING PIC X(32).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           MOVE \"HELLO FROM CBL-C\" TO GREETING\n"
        "           DISPLAY GREETING\n"
        "           STOP RUN.\n";
    if (test_expect_success(transpiler_validate_generated_cobol(source),
            "validator should accept COBOL with working-storage items") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_invalid_cobol)
{
    const char *source;

    source = "invalid cobol text";
    if (transpiler_validate_generated_cobol(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject invalid COBOL text\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

const t_test_case *get_validation_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_validation_accepts_valid_cblc", test_transpiler_validation_accepts_valid_cblc},
        {"transpiler_validation_rejects_cblc_without_return", test_transpiler_validation_rejects_cblc_without_return},
        {"transpiler_validation_accepts_string_declaration", test_transpiler_validation_accepts_string_declaration},
        {"transpiler_validation_accepts_string_assignment_and_length_usage",
            test_transpiler_validation_accepts_string_assignment_and_length_usage},
        {"cblc_parse_translation_unit_records_imports", test_cblc_parse_translation_unit_records_imports},
        {"cblc_parse_translation_unit_records_copy_includes", test_cblc_parse_translation_unit_records_copy_includes},
        {"cblc_parse_translation_unit_tracks_multiple_functions",
            test_cblc_parse_translation_unit_tracks_multiple_functions},
        {"cblc_parse_translation_unit_accepts_struct_fields",
            test_cblc_parse_translation_unit_accepts_struct_fields},
        {"cblc_parse_translation_unit_accepts_nested_struct_fields",
            test_cblc_parse_translation_unit_accepts_nested_struct_fields},
        {"cblc_generate_cobol_emits_string_group", test_cblc_generate_cobol_emits_string_group},
        {"cblc_generate_cobol_emits_struct_groups",
            test_cblc_generate_cobol_emits_struct_groups},
        {"cblc_generate_cobol_emits_nested_struct_groups",
            test_cblc_generate_cobol_emits_nested_struct_groups},
        {"cblc_generate_cobol_emits_copy_includes", test_cblc_generate_cobol_emits_copy_includes},
        {"cblc_generate_cobol_emits_multiple_paragraphs",
            test_cblc_generate_cobol_emits_multiple_paragraphs},
        {"cblc_generate_cobol_emits_perform_and_call_statements",
            test_cblc_generate_cobol_emits_perform_and_call_statements},
        {"cblc_generate_cobol_handles_string_assignments_and_length_computations",
            test_cblc_generate_cobol_handles_string_assignments_and_length_computations},
        {"cblc_generate_cobol_handles_multiplication_and_division",
            test_cblc_generate_cobol_handles_multiplication_and_division},
        {"cblc_generate_c_emits_struct_types_and_field_access",
            test_cblc_generate_c_emits_struct_types_and_field_access},
        {"cblc_generate_c_emits_nested_struct_types_and_field_access",
            test_cblc_generate_c_emits_nested_struct_types_and_field_access},
        {"cblc_parse_translation_unit_records_class_lifecycle_metadata",
            test_cblc_parse_translation_unit_records_class_lifecycle_metadata},
        {"cblc_generate_c_emits_automatic_class_lifecycle",
            test_cblc_generate_c_emits_automatic_class_lifecycle},
        {"cblc_generate_cobol_emits_automatic_class_lifecycle",
            test_cblc_generate_cobol_emits_automatic_class_lifecycle},
        {"cblc_generate_c_emits_custom_class_lifecycle_body",
            test_cblc_generate_c_emits_custom_class_lifecycle_body},
        {"cblc_generate_cobol_emits_custom_class_lifecycle_body",
            test_cblc_generate_cobol_emits_custom_class_lifecycle_body},
        {"cblc_generate_c_emits_local_class_lifecycle",
            test_cblc_generate_c_emits_local_class_lifecycle},
        {"cblc_generate_cobol_emits_local_class_lifecycle",
            test_cblc_generate_cobol_emits_local_class_lifecycle},
        {"cblc_generate_c_emits_nested_block_local_class_cleanup_order",
            test_cblc_generate_c_emits_nested_block_local_class_cleanup_order},
        {"cblc_generate_cobol_emits_nested_block_local_class_cleanup_order",
            test_cblc_generate_cobol_emits_nested_block_local_class_cleanup_order},
        {"cblc_parse_translation_unit_registers_builtin_string_class",
            test_cblc_parse_translation_unit_registers_builtin_string_class},
        {"cblc_generate_c_emits_local_string_lifecycle",
            test_cblc_generate_c_emits_local_string_lifecycle},
        {"cblc_parse_translation_unit_records_class_methods",
            test_cblc_parse_translation_unit_records_class_methods},
        {"cblc_generate_c_emits_class_methods",
            test_cblc_generate_c_emits_class_methods},
        {"cblc_generate_cobol_emits_class_methods",
            test_cblc_generate_cobol_emits_class_methods},
        {"cblc_generate_c_emits_constructor_initializer_list_in_declaration_order",
            test_cblc_generate_c_emits_constructor_initializer_list_in_declaration_order},
        {"cblc_generate_c_parses_const_member_initializer_list",
            test_cblc_generate_c_parses_const_member_initializer_list},
        {"cblc_parse_translation_unit_accepts_const_member_initialized_once_in_constructor_body",
            test_cblc_parse_translation_unit_accepts_const_member_initialized_once_in_constructor_body},
        {"cblc_parse_translation_unit_rejects_const_member_reassignment_in_constructor",
            test_cblc_parse_translation_unit_rejects_const_member_reassignment_in_constructor},
        {"cblc_parse_translation_unit_rejects_const_member_assignment_outside_constructor",
            test_cblc_parse_translation_unit_rejects_const_member_assignment_outside_constructor},
        {"cblc_parse_translation_unit_rejects_missing_const_member_initialization",
            test_cblc_parse_translation_unit_rejects_missing_const_member_initialization},
        {"cblc_resolve_calls_reports_missing_function",
            test_cblc_resolve_calls_reports_missing_function},
        {"transpiler_validation_accepts_valid_cobol", test_transpiler_validation_accepts_valid_cobol},
        {"transpiler_validation_accepts_working_storage_program", test_transpiler_validation_accepts_working_storage_program},
        {"transpiler_validation_rejects_invalid_cobol", test_transpiler_validation_rejects_invalid_cobol}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
