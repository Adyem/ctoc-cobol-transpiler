FT_TEST(test_cblc_inline_string_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_generated", "HELLO!\n"));
}

FT_TEST(test_cblc_inline_string_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO!\";\n"
        "    total = greeting.len();\n"
        "    if (total == 6) {\n"
        "        display(\"LEN OK\");\n"
        "    } else {\n"
        "        display(\"LEN BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_len_generated", "LEN OK\n"));
}

FT_TEST(test_cblc_inline_string_clear_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    greeting.clear();\n"
        "    total = greeting.len();\n"
        "    if (total == 0) {\n"
        "        display(\"CLEAR OK\");\n"
        "    } else {\n"
        "        display(\"CLEAR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_clear_generated", "CLEAR OK\n"));
}

FT_TEST(test_cblc_inline_string_empty_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    total = greeting.empty();\n"
        "    if (total == 1) {\n"
        "        display(\"EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_empty_generated", "EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_assign_raw_bytes_translates_to_cobol_and_executes)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int generation_diagnostic_code;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "void main()\n"
        "{\n"
        "    char *bytes;\n"
        "    string text(\"old\");\n"
        "    string expected(\"new\");\n"
        "    int matched;\n"
        "    int unchanged;\n"
        "    int is_empty;\n"
        "    bytes = new char[3];\n"
        "    bytes[0] = 'n';\n"
        "    bytes[1] = 'e';\n"
        "    bytes[2] = 'w';\n"
        "    text.assign(bytes, 3);\n"
        "    delete[] bytes;\n"
        "    bytes = NULL;\n"
        "    matched = text.equals(expected);\n"
        "    try { text.assign(bytes, 1); }\n"
        "    catch (const invalid_argument& error) { matched = matched + 1; }\n"
        "    try { text.assign(bytes, -1); }\n"
        "    catch (const invalid_argument& error) { matched = matched + 1; }\n"
        "    unchanged = text.equals(expected);\n"
        "    if (matched == 3 && unchanged == 1) {\n"
        "        display(\"RAW ASSIGN OK\");\n"
        "    } else {\n"
        "        display(\"RAW ASSIGN BAD\");\n"
        "    }\n"
        "    text.assign(bytes, 0);\n"
        "    is_empty = text.empty();\n"
        "    if (is_empty == 1) {\n"
        "        display(\"RAW EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"RAW EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generation_diagnostic_code = 0;
    generation_diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("raw-byte string assignment parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS)
    {
        std::printf("raw-byte string assignment generation diagnostic (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-TEXT-ASSIGN")
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: raw-byte string assignment should use its CBL-C method and exception dispatch\n");
        goto cleanup;
    }
    cma_free(generated_cobol);
    generated_cobol = NULL;
    cblc_translation_unit_dispose(&unit);
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(source,
        "inline_string_assign_raw_bytes_generated",
        "RAW ASSIGN OK\nRAW EMPTY OK\n"));
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

static const char *g_string_bounded_invalid_counts_source =
    "void main()\n"
    "{\n"
    "    string text(\"keep\");\n"
    "    string original(\"keep\");\n"
    "    string input(\"replacement\");\n"
    "    int caught;\n"
    "    int probe;\n"
    "    int unchanged;\n"
    "    caught = 0;\n"
    "    try { text.strncpy(input, -1); }\n"
    "    catch (const invalid_argument& error) { caught = 1; }\n"
    "    try { probe = text.strnlen(-1); }\n"
    "    catch (const invalid_argument& error) { caught = caught + 1; }\n"
    "    try { probe = text.memcmp(input, -1); }\n"
    "    catch (const invalid_argument& error) { caught = caught + 1; }\n"
    "    try { probe = text.memcmp(input, 5); }\n"
    "    catch (const invalid_argument& error) { caught = caught + 1; }\n"
    "    unchanged = text.equals(original);\n"
    "    if (caught == 4 && unchanged == 1) {\n"
    "        display(\"STRING COUNT ERRORS OK\");\n"
    "    } else {\n"
    "        display(\"STRING COUNT ERRORS BAD\");\n"
    "    }\n"
    "    return;\n"
    "}\n";

FT_TEST(test_cblc_string_bounded_helpers_reject_invalid_counts_generate)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = g_string_bounded_invalid_counts_source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("invalid string count parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("invalid string count generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-STRNCPY")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-STRNLEN")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-MEMCMP"))
    {
        std::printf("Assertion failed: invalid string byte counts should route through native method exception handling\n");
        goto cleanup;
    }
    cma_free(generated_cobol);
    generated_cobol = NULL;
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_bounded_helpers_reject_invalid_counts_executes)
{
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(
        g_string_bounded_invalid_counts_source,
        "string_invalid_byte_counts_generated", "STRING COUNT ERRORS OK\n"));
}

static int run_string_class_equality_pipeline_test(const char *source,
    const char *base_name, const char *expected_output)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("string class equality parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("string class equality generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-GREETING-EQUALS")
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: class equality must dispatch to the native string method with exception handling\n");
        goto cleanup;
    }
    cma_free(generated_cobol);
    generated_cobol = NULL;
    cblc_translation_unit_dispose(&unit);
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(source, base_name,
        expected_output));
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_class_equals_equal_values_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string other(\"HELLO\");\n"
        "    total = greeting.equals(other);\n"
        "    if (total == 1) {\n"
        "        display(\"EQUALS OK\");\n"
        "    } else {\n"
        "        display(\"EQUALS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_string_class_equality_pipeline_test(source,
        "string_class_equal_values_generated", "EQUALS OK\n"));
}

FT_TEST(test_cblc_string_class_equals_different_values_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string expected(\"WORLD\");\n"
        "    total = greeting.equals(expected);\n"
        "    if (total == 0) {\n"
        "        display(\"DIFFERENT CLASS VALUES OK\");\n"
        "    } else {\n"
        "        display(\"DIFFERENT CLASS VALUES BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_string_class_equality_pipeline_test(source,
        "string_class_different_values_generated",
        "DIFFERENT CLASS VALUES OK\n"));
}

static const char *g_string_parse_int_contract_source =
    "void main()\n"
    "{\n"
    "    string maximum(\"999999999\");\n"
    "    string minimum(\"-999999999\");\n"
    "    string overflow(\"1000000000\");\n"
    "    string negative_overflow(\"-1000000000\");\n"
    "    string malformed(\"12x\");\n"
    "    string malformed_copy(\"12x\");\n"
    "    string padded_positive(\" +42 \");\n"
    "    int parsed_maximum;\n"
    "    int parsed_minimum;\n"
    "    int parsed_padded_positive;\n"
    "    int caught_range;\n"
    "    int caught_parse;\n"
    "    int unchanged;\n"
    "    parsed_maximum = maximum.parse_int();\n"
    "    parsed_minimum = minimum.parse_int();\n"
    "    parsed_padded_positive = padded_positive.parse_int();\n"
    "    caught_range = 0;\n"
    "    caught_parse = 0;\n"
    "    try { parsed_maximum = overflow.parse_int(); }\n"
    "    catch (const range_error& error) { caught_range = error.code; }\n"
    "    try { parsed_maximum = negative_overflow.parse_int(); }\n"
    "    catch (const range_error& error) { caught_range = caught_range + error.code; }\n"
    "    try { parsed_maximum = malformed.parse_int(); }\n"
    "    catch (const parse_error& error) { caught_parse = error.code; }\n"
    "    unchanged = malformed.equals(malformed_copy);\n"
    "    if (parsed_maximum == 999999999 && parsed_minimum == -999999999\n"
    "        && parsed_padded_positive == 42 && caught_range == 2\n"
    "        && caught_parse == 2 && unchanged == 1) {\n"
    "        display(\"STRING PARSE INT CONTRACT OK\");\n"
    "    } else {\n"
    "        display(\"STRING PARSE INT CONTRACT BAD\");\n"
    "    }\n"
    "    return;\n"
    "}\n";

FT_TEST(test_cblc_string_parse_int_boundaries_and_errors_generate)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(g_string_parse_int_contract_source,
            &unit) != FT_SUCCESS)
    {
        std::printf("string parse_int contract parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("string parse_int contract generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-MAIN-MAXIMUM-PARSE-INT")
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-MAIN-OVERFLOW-PARSE-INT")
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-MAIN-MALFORMED-PARSE-INT")
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: parse_int boundary and typed-error paths should use native string method dispatch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_parse_int_boundaries_and_errors_executes)
{
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(
        g_string_parse_int_contract_source,
        "string_parse_int_contract_generated",
        "STRING PARSE INT CONTRACT OK\n"));
}

static const char *g_string_substr_utf8_contract_source =
    "void main()\n"
    "{\n"
    "    string text(\"A\");\n"
    "    string accent;\n"
    "    string suffix(\"B\");\n"
    "    string expected;\n"
    "    string original;\n"
    "    string slice;\n"
    "    int split_caught;\n"
    "    int negative_caught;\n"
    "    int oversized_caught;\n"
    "    int text_unchanged;\n"
    "    int slice_matches;\n"
    "    int final_length;\n"
    "    accent.push_codepoint(233);\n"
    "    text.append(accent);\n"
    "    text.append(suffix);\n"
    "    original = text;\n"
    "    expected.append(accent);\n"
    "    slice = text.substr(1, 2);\n"
    "    split_caught = 0;\n"
    "    negative_caught = 0;\n"
    "    oversized_caught = 0;\n"
    "    try { slice = text.substr(2, 1); }\n"
    "    catch (const invalid_argument& error) { split_caught = error.code; }\n"
    "    try { slice = text.substr(-1, 1); }\n"
    "    catch (const out_of_range& error) { negative_caught = error.code; }\n"
    "    try { slice = text.substr(1, 5); }\n"
    "    catch (const out_of_range& error) { oversized_caught = error.code; }\n"
    "    text_unchanged = text.equals(original);\n"
    "    slice_matches = slice.equals(expected);\n"
    "    final_length = text.byte_length();\n"
    "    if (slice_matches == 1 && split_caught == 1\n"
    "        && negative_caught == 1 && oversized_caught == 4\n"
    "        && text_unchanged == 1 && final_length == 4) {\n"
    "        display(\"STRING SUBSTR UTF8 CONTRACT OK\");\n"
    "    } else {\n"
    "        display(\"STRING SUBSTR UTF8 CONTRACT BAD\");\n"
    "    }\n"
    "    return;\n"
    "}\n";

FT_TEST(test_cblc_string_substr_utf8_contract_generates)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(g_string_substr_utf8_contract_source,
            &unit) != FT_SUCCESS)
    {
        std::printf("string substr UTF-8 contract parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("string substr UTF-8 contract generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-MAIN-TEXT-SUBSTR")
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-MAIN-TEXT-EQUALS")
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: string::substr must dispatch as an owning class method with typed exception handling\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_substr_utf8_contract_executes)
{
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(
        g_string_substr_utf8_contract_source,
        "string_substr_utf8_contract_generated",
        "STRING SUBSTR UTF8 CONTRACT OK\n"));
}

static const char *g_string_remove_prefix_suffix_source =
    "void main()\n"
    "{\n"
    "    string text(\"A\");\n"
    "    string accent;\n"
    "    string suffix(\"BC\");\n"
    "    string original;\n"
    "    int caught;\n"
    "    int unchanged;\n"
    "    int accent_matches;\n"
    "    accent.push_codepoint(233);\n"
    "    text.append(accent);\n"
    "    text.append(suffix);\n"
    "    text.remove_prefix(1);\n"
    "    text.remove_suffix(2);\n"
    "    original = text;\n"
    "    caught = 0;\n"
    "    try { text.remove_prefix(1); }\n"
    "    catch (const invalid_argument& error) { caught = caught + 1; }\n"
    "    try { text.remove_suffix(1); }\n"
    "    catch (const invalid_argument& error) { caught = caught + 1; }\n"
    "    try { text.remove_prefix(3); }\n"
    "    catch (const out_of_range& error) { caught = caught + 1; }\n"
    "    unchanged = text.equals(original);\n"
    "    accent_matches = text.equals(accent);\n"
    "    if (caught == 3 && unchanged == 1 && accent_matches == 1) {\n"
    "        display(\"STRING PREFIX SUFFIX OK\");\n"
    "    } else {\n"
    "        display(\"STRING PREFIX SUFFIX BAD\");\n"
    "    }\n"
    "    return;\n"
    "}\n";

FT_TEST(test_cblc_string_remove_prefix_suffix_generates)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(g_string_remove_prefix_suffix_source,
            &unit) != FT_SUCCESS)
    {
        std::printf("string prefix/suffix parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("string prefix/suffix generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-REMOVE-PREFIX")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-REMOVE-SUFFIX")
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: prefix/suffix removal must lower through native string methods and typed exception dispatch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_remove_prefix_suffix_executes)
{
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(
        g_string_remove_prefix_suffix_source,
        "string_remove_prefix_suffix_generated", "STRING PREFIX SUFFIX OK\n"));
}

FT_TEST(test_cblc_inline_string_capacity_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    total = greeting.capacity();\n"
        "    if (total == 16) {\n"
        "        display(\"CAPACITY OK\");\n"
        "    } else {\n"
        "        display(\"CAPACITY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_generated", "CAPACITY OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string prefix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    prefix = \"HE\";\n"
        "    total = greeting.starts_with(prefix);\n"
        "    if (total == 1) {\n"
        "        display(\"STARTS OK\");\n"
        "    } else {\n"
        "        display(\"STARTS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_with_generated", "STARTS OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"LO\";\n"
        "    total = greeting.ends_with(suffix);\n"
        "    if (total == 1) {\n"
        "        display(\"ENDS OK\");\n"
        "    } else {\n"
        "        display(\"ENDS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_with_generated", "ENDS OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"HELLO\";\n"
        "    total = greeting.compare(suffix);\n"
        "    if (total == 0) {\n"
        "        display(\"COMPARE OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_generated", "COMPARE OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string needle(8);\n"
        "    greeting = \"HELLO\";\n"
        "    needle = \"ELL\";\n"
        "    total = greeting.contains(needle);\n"
        "    if (total == 1) {\n"
        "        display(\"CONTAINS OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_generated", "CONTAINS OK\n"));
}

FT_TEST(test_cblc_inline_string_equals_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"HELLO\";\n"
        "    other = \"WORLD\";\n"
        "    total = greeting.equals(other);\n"
        "    if (total == 0) {\n"
        "        display(\"EQUALS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"EQUALS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_equals_false_generated", "EQUALS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_empty_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.empty();\n"
        "    if (total == 0) {\n"
        "        display(\"EMPTY FALSE OK\");\n"
        "    } else {\n"
        "        display(\"EMPTY FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_empty_false_generated", "EMPTY FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.starts_with(\"HE\");\n"
        "    if (total == 1) {\n"
        "        display(\"STARTS LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"STARTS LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_with_literal_generated", "STARTS LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.ends_with(\"LO\");\n"
        "    if (total == 1) {\n"
        "        display(\"ENDS LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"ENDS LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_with_literal_generated", "ENDS LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_less_than_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"ALPHA\";\n"
        "    suffix = \"BRAVO\";\n"
        "    total = greeting.compare(suffix);\n"
        "    if (total == -1) {\n"
        "        display(\"COMPARE LT OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE LT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_less_generated", "COMPARE LT OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_greater_than_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"BRAVO\";\n"
        "    suffix = \"ALPHA\";\n"
        "    total = greeting.compare(suffix);\n"
        "    if (total == 1) {\n"
        "        display(\"COMPARE GT OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE GT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_greater_generated", "COMPARE GT OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.compare(\"HELLO\");\n"
        "    if (total == 0) {\n"
        "        display(\"COMPARE LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_literal_generated", "COMPARE LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string needle(8);\n"
        "    greeting = \"HELLO\";\n"
        "    needle = \"XYZ\";\n"
        "    total = greeting.contains(needle);\n"
        "    if (total == 0) {\n"
        "        display(\"CONTAINS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_false_generated", "CONTAINS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.contains(\"\");\n"
        "    if (total == 1) {\n"
        "        display(\"CONTAINS EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_empty_generated", "CONTAINS EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_constructor_capacity_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    total = greeting.capacity();\n"
        "    if (total == 5) {\n"
        "        display(\"CTOR CAPACITY OK\");\n"
        "    } else {\n"
        "        display(\"CTOR CAPACITY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ctor_capacity_generated", "CTOR CAPACITY OK\n"));
}

FT_TEST(test_cblc_inline_string_capacity_constructor_grows_for_assignment_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(1);\n"
        "    label = \"EXPANDED\";\n"
        "    total = label.len();\n"
        "    if (total == 8) {\n"
        "        display(label);\n"
        "    } else {\n"
        "        display(\"STRING GROW BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_assignment_grows_generated", "EXPANDED\n"));
}

FT_TEST(test_cblc_inline_string_capacity_constructor_grows_for_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(1);\n"
        "    label = \"A\";\n"
        "    label.append(\"BCDE\");\n"
        "    total = label.len();\n"
        "    if (total == 5) {\n"
        "        display(label);\n"
        "    } else {\n"
        "        display(\"APPEND GROW BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_append_grows_generated", "ABCDE\n"));
}

FT_TEST(test_cblc_inline_string_append_then_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    total = greeting.len();\n"
        "    if (total == 6) {\n"
        "        display(\"APPEND LEN OK\");\n"
        "    } else {\n"
        "        display(\"APPEND LEN BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_len_generated", "APPEND LEN OK\n"));
}

FT_TEST(test_cblc_inline_string_clear_then_empty_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    greeting.clear();\n"
        "    total = greeting.empty();\n"
        "    if (total == 1) {\n"
        "        display(\"CLEAR EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"CLEAR EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_clear_empty_generated", "CLEAR EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string prefix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    prefix = \"EL\";\n"
        "    total = greeting.starts_with(prefix);\n"
        "    if (total == 0) {\n"
        "        display(\"STARTS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"STARTS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_false_generated", "STARTS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"HE\";\n"
        "    total = greeting.ends_with(suffix);\n"
        "    if (total == 0) {\n"
        "        display(\"ENDS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"ENDS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_false_generated", "ENDS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.starts_with(\"\");\n"
        "    if (total == 1) {\n"
        "        display(\"STARTS EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"STARTS EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_empty_generated", "STARTS EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.ends_with(\"\");\n"
        "    if (total == 1) {\n"
        "        display(\"ENDS EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"ENDS EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_empty_generated", "ENDS EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_shorter_prefix_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"AL\";\n"
        "    other = \"ALPHA\";\n"
        "    total = greeting.compare(other);\n"
        "    if (total == -1) {\n"
        "        display(\"COMPARE SHORT OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE SHORT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_short_generated", "COMPARE SHORT OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_longer_prefix_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"ALPHA\";\n"
        "    other = \"AL\";\n"
        "    total = greeting.compare(other);\n"
        "    if (total == 1) {\n"
        "        display(\"COMPARE LONG OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE LONG BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_long_generated", "COMPARE LONG OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_literal_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.contains(\"XYZ\");\n"
        "    if (total == 0) {\n"
        "        display(\"CONTAINS LITERAL FALSE OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS LITERAL FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_literal_false_generated", "CONTAINS LITERAL FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_copy_constructor_equals_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string copy(greeting);\n"
        "    total = copy.equals(greeting);\n"
        "    if (total == 1) {\n"
        "        display(\"COPY EQUALS OK\");\n"
        "    } else {\n"
        "        display(\"COPY EQUALS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_copy_equals_generated", "COPY EQUALS OK\n"));
}

FT_TEST(test_cblc_inline_string_literal_constructor_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_literal_constructor_generated", "HELLO\n"));
}

FT_TEST(test_cblc_inline_string_copy_constructor_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string copy(greeting);\n"
        "    display(copy);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_copy_constructor_generated", "HELLO\n"));
}

FT_TEST(test_cblc_string_copy_self_assignment_preserves_value)
{
    const char *errors_source;
    const char *string_source;
    const char *source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int found_self_assignment;
    int status;

    source = "void main()\n"
        "{\n"
        "    string value(\"HELLO\");\n"
        "    string expected(\"HELLO\");\n"
        "    int preserved;\n"
        "    value = value;\n"
        "    preserved = value.equals(expected);\n"
        "    if (preserved == 1) {\n"
        "        display(\"STRING SELF COPY ASSIGNMENT OK\");\n"
        "    } else {\n"
        "        display(\"STRING SELF COPY ASSIGNMENT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    if (!errors_source || !string_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source
        + "\n" + source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("string self-copy-assignment parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    found_self_assignment = 0;
    for (size_t function_index = 0; function_index < unit.function_count;
        ++function_index)
    {
        const t_cblc_function &function = unit.functions[function_index];

        if (std::strcmp(function.source_name, "main") != 0)
            continue ;
        for (size_t statement_index = 0;
            statement_index < function.statement_count; ++statement_index)
        {
            const t_cblc_statement &statement = function.statements[statement_index];

            if (statement.type == CBLC_STATEMENT_ASSIGNMENT
                && statement.target[0] != '\0'
                && std::strcmp(statement.target, statement.source) == 0)
                found_self_assignment = 1;
        }
    }
    if (!found_self_assignment)
    {
        std::printf("Assertion failed: parser must retain the source-level self-copy-assignment operation\n");
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("string self-copy-assignment generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-VALUE-EQUALS")
        || !std::strstr(generated_cobol,
            "CBLC-DESTRUCTOR-MAIN-VALUE."))
    {
        std::printf("Assertion failed: self-assignment must dispatch through the string copy-assignment method and retain normal scope cleanup\n");
        goto cleanup;
    }
    if (!test_cobc_available())
    {
        status = test_require_cobc_dependency(__func__);
        goto cleanup;
    }
    cma_free(generated_cobol);
    generated_cobol = NULL;
    cblc_translation_unit_dispose(&unit);
    return (run_inline_cblc_to_cobol_execution_test(source,
        "string_copy_self_assignment_generated",
        "STRING SELF COPY ASSIGNMENT OK\n"));
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_class_method_can_return_char_by_value)
{
    const char *errors_source;
    const char *source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int found_char_return_method;
    int found_char_call;
    int status;

    source = "class CharacterSource {\n"
        "private:\n"
        "    char *data;\n"
        "public:\n"
        "    CharacterSource() { data = new char[1]; data[0] = 'A'; }\n"
        "    ~CharacterSource() { delete[] data; }\n"
        "    char at(int index) { return data[index]; }\n"
        "};\n"
        "void main() {\n"
        "    CharacterSource source;\n"
        "    char received;\n"
        "    received = source.at(0);\n"
        "    try { received = source.at(-1); }\n"
        "    catch (const out_of_range& error) { received = 'X'; }\n"
        "}\n";
    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!errors_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("class char-return parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    found_char_return_method = 0;
    found_char_call = 0;
    for (size_t type_index = 0; type_index < unit.struct_type_count; ++type_index)
    {
        const t_cblc_struct_type &type = unit.struct_types[type_index];

        if (std::strcmp(type.source_name, "CharacterSource") != 0)
            continue ;
        for (size_t method_index = 0; method_index < type.method_count;
            ++method_index)
        {
            const t_cblc_method &method = type.methods[method_index];

            if (std::strcmp(method.source_name, "at") == 0
                && method.return_kind == CBLC_FUNCTION_RETURN_CHAR
                && method.has_definition)
                found_char_return_method = 1;
        }
    }
    for (size_t function_index = 0; function_index < unit.function_count;
        ++function_index)
    {
        const t_cblc_function &function = unit.functions[function_index];

        if (std::strcmp(function.source_name, "main") != 0)
            continue ;
        for (size_t statement_index = 0;
            statement_index < function.statement_count; ++statement_index)
        {
            const t_cblc_statement &statement = function.statements[statement_index];

            if (statement.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement.call_identifier, "at") == 0)
                found_char_call = 1;
        }
    }
    if (!found_char_return_method || !found_char_call)
    {
        std::printf("Assertion failed: char-return method declaration and caller assignment must remain typed in parser IR\n");
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("class char-return generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated_cobol || generated_cobol[0] == '\0'
        || !std::strstr(generated_cobol, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: char-return class method and its exception path must reach COBOL generation\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_move_assignment_leaves_source_reusable)
{
    const char *string_source;
    const char *errors_source;
    const char *source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int generation_diagnostic_code;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "string expected_original(\"A\");\n"
        "string suffix(\"C\");\n"
        "string expected_reused(\"AC\");\n"
        "void main()\n"
        "{\n"
        "    string greeting(1);\n"
        "    string target(1);\n"
        "    string *source_pointer;\n"
        "    int source_is_empty;\n"
        "    int destination_is_preserved;\n"
        "    int source_was_reused;\n"
        "    greeting = expected_original;\n"
        "    target = expected_original;\n"
        "    source_pointer = &greeting;\n"
        "    target.move_assign(source_pointer);\n"
        "    source_is_empty = greeting.empty();\n"
        "    destination_is_preserved = target.equals(expected_original);\n"
        "    if (source_is_empty == 1) {\n"
        "        greeting.erase(0, 0);\n"
        "        greeting.trim_ascii();\n"
        "        greeting.append(suffix);\n"
        "        source_was_reused = greeting.equals(expected_reused);\n"
        "        if (source_was_reused == 1) {\n"
        "            if (destination_is_preserved == 1) {\n"
        "                display(\"MOVED-FROM STRING REUSED OK\");\n"
        "            } else {\n"
        "                display(\"MOVED-FROM STRING REUSED BAD\");\n"
        "            }\n"
        "        } else {\n"
        "            display(\"MOVED-FROM STRING REUSED BAD\");\n"
        "        }\n"
        "    } else {\n"
        "        display(\"MOVED-FROM STRING STATE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!string_source || !errors_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source
        + "\n" + source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generation_diagnostic_code = 0;
    generation_diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("Moved-from string reuse parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS)
    {
        std::printf("Moved-from string reuse generation diagnostic: code=%d message=%s\n",
            generation_diagnostic_code, generation_diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-TARGET-MOVE-ASSIGN")
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-GREETING-TRIM-ASCII")
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-GREETING-ERASE")
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-GREETING-APPEND"))
    {
        std::printf("Assertion failed: moved-from reuse must lower move assignment and subsequent string append as class method calls\n");
        goto cleanup;
    }
    if (!test_cobc_available())
    {
        status = test_require_cobc_dependency(__func__);
        goto cleanup;
    }
    cma_free(generated_cobol);
    generated_cobol = NULL;
    cblc_translation_unit_dispose(&unit);
    return (run_inline_cblc_to_cobol_execution_test(source,
        "string_move_assignment_source_reuse_generated",
        "MOVED-FROM STRING REUSED OK\n"));
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_operator_plus_returns_independent_value)
{
    const char *errors_source;
    const char *string_source;
    const char *source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    source = "void main()\n"
        "{\n"
        "    string left(\"alpha\");\n"
        "    string right(\"beta\");\n"
        "    string suffix(\"gamma\");\n"
        "    string delta(\"delta\");\n"
        "    string tail(\"epsilon\");\n"
        "    string original(\"alpha\");\n"
        "    string expected(\"alphabetagammadeltaepsilon\");\n"
        "    string joined;\n"
        "    int result_matches;\n"
        "    int left_unchanged;\n"
        "    joined = left + right + suffix + delta + tail;\n"
        "    result_matches = joined.equals(expected);\n"
        "    left_unchanged = left.equals(original);\n"
        "    if (result_matches == 1) {\n"
        "        if (left_unchanged == 1) {\n"
        "            display(\"STRING PLUS OWNED VALUE OK\");\n"
        "        } else {\n"
        "            display(\"STRING PLUS OWNED VALUE BAD\");\n"
        "        }\n"
        "    } else {\n"
        "        display(\"STRING PLUS OWNED VALUE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    if (!errors_source || !string_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source
        + "\n" + source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit)
        != FT_SUCCESS)
    {
        std::printf("string operator+ runtime fixture parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        std::printf("string operator+ runtime fixture failed COBOL generation\n");
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "CBLC-USER-STRING-OPERATOR-PLUS-RESULT")
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-MAIN-LEFT-OPERATOR-PLUS"))
    {
        std::printf("Assertion failed: string operator+ must emit a class method call and owned result storage\n");
        goto cleanup;
    }
    cma_free(generated_cobol);
    generated_cobol = NULL;
    cblc_translation_unit_dispose(&unit);
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(source,
        "string_operator_plus_owned_value_generated",
        "STRING PLUS OWNED VALUE OK\n"));
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_vector_string_lifecycle_translates_to_cobol_and_executes)
{
    const char *errors_source;
    const char *string_source;
    const char *vector_source;
    const char *program_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    t_parser generated_parser;
    t_ast_node *generated_program;
    char *generated_cobol;
    int generation_diagnostic_code;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    char cobol_validation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    program_source = "void main() {\n"
        "    vector<string> source;\n"
        "    vector<vector<string>> groups;\n"
        "    vector<vector<vector<string>>> groups3;\n"
        "    vector<vector<vector<vector<string>>>> deep_groups;\n"
        "    string seed(\"XYZ\");\n"
        "    int outer_size;\n"
        "    int source_length;\n"
        "    int nested_length;\n"
        "    int deep_length;\n"
        "    source.push_back(seed);\n"
        "    groups.push_back(source);\n"
        "    groups3.push_back(groups);\n"
        "    deep_groups.push_back(groups3);\n"
        "    source.at(0)->clear();\n"
        "    groups.at(0)->at(0)->append(seed);\n"
        "    deep_groups.at(0)->at(0)->at(0)->at(0)->append(seed);\n"
        "    outer_size = source.size();\n"
        "    source_length = source.at(0)->len();\n"
        "    nested_length = groups.at(0)->at(0)->len();\n"
        "    deep_length = deep_groups.at(0)->at(0)->at(0)->at(0)->len();\n"
        "    if (outer_size == 1 && source_length == 0 && nested_length == 6\n"
        "        && deep_length == 6) {\n"
        "        display(\"VECTOR STRING LIFECYCLE OK\");\n"
        "    } else {\n"
        "        display(\"VECTOR STRING LIFECYCLE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    if (!errors_source || !string_source || !vector_source)
    {
        std::printf("Assertion failed: canonical errors, string, and vector sources should be available\n");
        return (FT_FAILURE);
    }
    combined_source = std::string(errors_source) + "\n" + string_source + "\n"
        + vector_source + "\n" + program_source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generation_diagnostic_code = 0;
    generation_diagnostic[0] = '\0';
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("vector<string> lifecycle parse diagnostic: %s\n",
            unit.parse_error_message);
        if (unit.template_error_name[0] != '\0')
            std::printf("template instantiation failure: %s<%s> definition=%zu use=%zu\n",
                unit.template_error_name, unit.template_error_arguments,
                unit.template_error_definition_offset,
                unit.template_error_use_offset);
        if (unit.template_error_use_offset < combined_source.size())
            std::printf("template failure source context: %.80s\n",
                combined_source.c_str() + unit.template_error_use_offset);
        test_expect_success(FT_FAILURE,
            "vector<string> lifecycle fixture should parse");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (unit.entry_function_index >= unit.function_count)
    {
        std::printf("Assertion failed: nested vector lifecycle fixture should have an entry function\n");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    {
        const t_cblc_function *entry;
        size_t index;
        size_t at_offsets[4];
        size_t append_offset;
        size_t depth_index;

        entry = &unit.functions[unit.entry_function_index];
        depth_index = 0;
        while (depth_index < 4)
        {
            at_offsets[depth_index] = static_cast<size_t>(-1);
            depth_index += 1;
        }
        append_offset = static_cast<size_t>(-1);
        index = 0;
        while (index < entry->statement_count)
        {
            const t_cblc_statement *statement;

            statement = &entry->statements[index];
            if (std::strcmp(statement->call_identifier, "append") == 0
                && index >= 4
                && std::strcmp(entry->statements[index - 1].call_identifier,
                    "at") == 0
                && std::strcmp(entry->statements[index - 2].call_identifier,
                    "at") == 0
                && std::strcmp(entry->statements[index - 3].call_identifier,
                    "at") == 0
                && std::strcmp(entry->statements[index - 4].call_identifier,
                    "at") == 0)
            {
                at_offsets[0] = entry->statements[index - 4].source_offset;
                at_offsets[1] = entry->statements[index - 3].source_offset;
                at_offsets[2] = entry->statements[index - 2].source_offset;
                at_offsets[3] = entry->statements[index - 1].source_offset;
                append_offset = statement->source_offset;
                break ;
            }
            index += 1;
        }
        depth_index = 0;
        while (depth_index < 4
            && at_offsets[depth_index] != static_cast<size_t>(-1))
        {
            if (at_offsets[depth_index] == append_offset
                || (depth_index > 0
                    && at_offsets[depth_index] <= at_offsets[depth_index - 1]))
                break ;
            depth_index += 1;
        }
        if (depth_index != 4)
        {
            std::printf("Assertion failed: vector depth-4 at()->at()->at()->at()->append() must retain ordered, distinct source offsets\n");
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
    }
    if (test_expect_success(cblc_generate_cobol_with_diagnostic(&unit,
                &generated_cobol, &generation_diagnostic_code,
                generation_diagnostic, sizeof(generation_diagnostic)),
            "vector<string> lifecycle fixture should generate COBOL") != FT_SUCCESS)
    {
        if (generation_diagnostic[0] != '\0')
            std::printf("vector<string> generation diagnostic (%d): %s\n",
                generation_diagnostic_code, generation_diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (std::strstr(generated_cobol, "MAX-SIZE()"))
    {
        std::printf("generated vector<string> COBOL contains an unresolved max_size() expression\n");
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cobol_validation_diagnostic[0] = '\0';
    if (!generated_cobol
        || transpiler_validate_generated_cobol_with_diagnostic(generated_cobol,
            cobol_validation_diagnostic,
            sizeof(cobol_validation_diagnostic)) != FT_SUCCESS)
    {
        generated_program = NULL;
        parser_init(&generated_parser, generated_cobol ? generated_cobol : "");
        if (parser_parse_program(&generated_parser, &generated_program) != FT_SUCCESS)
        {
            std::printf("generated COBOL parser failure at %zu:%zu near '%.120s' (error %d)\n",
                generated_parser.error_token.line,
                generated_parser.error_token.column,
                generated_parser.error_token.lexeme,
                generated_parser.last_error);
            if (generated_cobol)
            {
                const char *line_start;
                const char *line_end;
                size_t line_number;

                line_start = generated_cobol;
                line_number = 1;
                while (*line_start != '\0' && line_number <= generated_parser.error_token.line + 8)
                {
                    line_end = std::strchr(line_start, '\n');
                    if (!line_end)
                        line_end = line_start + std::strlen(line_start);
                    if (line_number + 20 >= generated_parser.error_token.line
                        && line_number <= generated_parser.error_token.line + 8)
                        std::printf("generated COBOL[%zu]: %.*s\n", line_number,
                            static_cast<int>(line_end - line_start), line_start);
                    line_start = *line_end == '\0' ? line_end : line_end + 1;
                    line_number += 1;
                }
            }
        }
        else
        {
            std::printf("generated COBOL parses but fails structural validation: %s\n",
                cobol_validation_diagnostic);
            if (generated_cobol
                && std::strstr(cobol_validation_diagnostic,
                    "unresolved PERFORM range"))
            {
                const char *range_start;
                const char *range_end;
                const char *return_start;
                const char *return_end;
                char paragraph_names[2][TRANSPILE_IDENTIFIER_MAX];
                const char *name_starts[2];
                const char *name_ends[2];
                size_t label_index;

                range_start = std::strchr(cobol_validation_diagnostic, '\'');
                range_end = range_start ? std::strchr(range_start + 1, '\'') : NULL;
                return_start = range_end
                    ? std::strchr(range_end + 1, '\'') : NULL;
                return_end = return_start
                    ? std::strchr(return_start + 1, '\'') : NULL;
                name_starts[0] = range_start ? range_start + 1 : NULL;
                name_ends[0] = range_end;
                name_starts[1] = return_start ? return_start + 1 : NULL;
                name_ends[1] = return_end;
                for (label_index = 0; label_index < 2; ++label_index)
                {
                    size_t label_length;

                    label_length = name_starts[label_index] && name_ends[label_index]
                        ? static_cast<size_t>(name_ends[label_index]
                            - name_starts[label_index]) : 0;
                    if (label_length == 0
                        || label_length >= sizeof(paragraph_names[label_index]))
                        continue ;
                    std::memcpy(paragraph_names[label_index],
                        name_starts[label_index], label_length);
                    paragraph_names[label_index][label_length] = '\0';
                    {
                        const char *line_start;
                        size_t line_number;
                        size_t reference_line_number;
                        int printed_reference;

                        line_start = generated_cobol;
                        line_number = 1;
                        reference_line_number = 0;
                        printed_reference = 0;
                        while (*line_start != '\0')
                        {
                            const char *line_end;
                            const char *token_start;
                            const char *reference_start;

                            line_end = std::strchr(line_start, '\n');
                            if (!line_end)
                                line_end = line_start + std::strlen(line_start);
                            reference_start = line_start;
                            while (reference_start + label_length <= line_end
                                && std::memcmp(reference_start,
                                    paragraph_names[label_index], label_length) != 0)
                                reference_start += 1;
                            if (reference_start + label_length > line_end)
                                reference_start = NULL;
                            token_start = line_start;
                            while (token_start < line_end && *token_start == ' ')
                                token_start += 1;
                            if (static_cast<size_t>(line_end - token_start)
                                    == label_length + 1
                                && std::memcmp(token_start,
                                    paragraph_names[label_index], label_length) == 0
                                && token_start[label_length] == '.')
                                std::printf("expected paragraph %s occurs at COBOL line %zu\n",
                                    paragraph_names[label_index], line_number);
                            if (!printed_reference
                                && reference_start && reference_start < line_end)
                            {
                                std::printf("paragraph reference context at COBOL line %zu: %.*s\n",
                                    line_number,
                                    static_cast<int>(line_end - line_start),
                                    line_start);
                                reference_line_number = line_number;
                                printed_reference = 1;
                            }
                            line_start = *line_end == '\0' ? line_end : line_end + 1;
                            line_number += 1;
                        }
                        if (reference_line_number > 0)
                        {
                            line_start = generated_cobol;
                            line_number = 1;
                            while (*line_start != '\0'
                                && line_number <= reference_line_number + 2)
                            {
                                const char *line_end = std::strchr(line_start, '\n');

                                if (!line_end)
                                    line_end = line_start + std::strlen(line_start);
                                if (line_number + 8 >= reference_line_number)
                                    std::printf("COBOL context[%zu]: %.*s\n",
                                        line_number,
                                        static_cast<int>(line_end - line_start),
                                        line_start);
                                line_start = *line_end == '\0' ? line_end : line_end + 1;
                                line_number += 1;
                            }
                        }
                    }
                }
            }
            if (generated_cobol
                && std::strstr(cobol_validation_diagnostic, "duplicate paragraph"))
            {
                const char *name_start;
                const char *name_end;
                char duplicate_name[TRANSPILE_IDENTIFIER_MAX];
                size_t name_length;
                const char *line_start;
                size_t line_number;

                name_start = std::strchr(cobol_validation_diagnostic, '\'');
                name_end = name_start ? std::strchr(name_start + 1, '\'') : NULL;
                name_length = name_start && name_end
                    ? static_cast<size_t>(name_end - name_start - 1) : 0;
                if (name_length > 0 && name_length < sizeof(duplicate_name))
                {
                    std::memcpy(duplicate_name, name_start + 1, name_length);
                    duplicate_name[name_length] = '\0';
                    line_start = generated_cobol;
                    line_number = 1;
                    while (*line_start != '\0')
                    {
                        const char *line_end;
                        const char *token_start;

                        line_end = std::strchr(line_start, '\n');
                        if (!line_end)
                            line_end = line_start + std::strlen(line_start);
                        token_start = line_start;
                        while (token_start < line_end && *token_start == ' ')
                            token_start += 1;
                        if (static_cast<size_t>(line_end - token_start)
                                == name_length + 1
                            && std::memcmp(token_start, duplicate_name,
                                name_length) == 0
                            && token_start[name_length] == '.')
                        {
                            const char *context_start;
                            const char *context_end;
                            size_t context_line;

                            std::printf("duplicate paragraph occurrence at COBOL line %zu: %.*s\n",
                                line_number, static_cast<int>(line_end - line_start),
                                line_start);
                            context_start = line_end;
                            context_line = line_number;
                            while (*context_start == '\n')
                            {
                                context_start += 1;
                                context_line += 1;
                                if (context_line > line_number + 12)
                                    break ;
                                context_end = std::strchr(context_start, '\n');
                                if (!context_end)
                                    context_end = context_start
                                        + std::strlen(context_start);
                                std::printf("  COBOL[%zu]: %.*s\n", context_line,
                                    static_cast<int>(context_end - context_start),
                                    context_start);
                                context_start = context_end;
                            }
                        }
                        line_start = *line_end == '\0' ? line_end : line_end + 1;
                        line_number += 1;
                    }
                }
            }
        }
        if (generated_program)
            ast_node_destroy(generated_program);
        parser_dispose(&generated_parser);
        test_expect_success(FT_FAILURE,
            "generated vector<string> COBOL should pass structural validation");
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    return (run_inline_cblc_to_cobol_execution_test(combined_source.c_str(),
        "vector_string_lifecycle_generated", "VECTOR STRING LIFECYCLE OK\n"));
}
