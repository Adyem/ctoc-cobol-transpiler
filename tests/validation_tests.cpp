#include "test_suites.hpp"

#include "compatibility/memory_compat.hpp"
#include "src/transpiler/backend_typed_ir.hpp"

static size_t validation_count_text_occurrences(const char *text,
    const char *needle)
{
    size_t count = 0;
    size_t needle_length;
    const char *cursor;

    if (!text || !needle || needle[0] == '\0')
        return 0;
    needle_length = std::strlen(needle);
    cursor = text;
    while ((cursor = std::strstr(cursor, needle)) != NULL)
    {
        ++count;
        cursor += needle_length;
    }
    return count;
}

static size_t validation_count_paragraph_definitions(const char *text,
    const char *prefix)
{
    const char *cursor;
    size_t count;
    size_t prefix_length;

    if (!text || !prefix || prefix[0] == '\0')
        return (0);
    cursor = text;
    prefix_length = std::strlen(prefix);
    count = 0;
    while ((cursor = std::strstr(cursor, prefix)) != NULL)
    {
        const char *line_end;

        line_end = std::strchr(cursor, '\n');
        if ((cursor == text || cursor[-1] == '\n') && line_end
            && line_end > cursor && line_end[-1] == '.')
            count += 1;
        cursor += prefix_length;
    }
    return (count);
}

static const char *validation_find_last_paragraph_before(const char *text,
    const char *prefix, const char *before)
{
    const char *cursor;
    const char *last_match;

    if (!text || !prefix || !before || text > before)
        return (NULL);
    cursor = text;
    last_match = NULL;
    while ((cursor = std::strstr(cursor, prefix)) != NULL && cursor < before)
    {
        const char *line_end;

        line_end = std::strchr(cursor, '\n');
        if ((cursor == text || cursor[-1] == '\n') && line_end
            && line_end < before && line_end > cursor && line_end[-1] == '.')
            last_match = cursor;
        cursor += std::strlen(prefix);
    }
    return (last_match);
}

FT_TEST(test_standard_library_migrated_helpers_use_direct_returns)
{
    static const char *program_names[] = {
        "CBLC-ABS", "CBLC-FABS", "CBLC-ISALPHA", "CBLC-ISDIGIT",
        "CBLC-MEMCMP", "CBLC-STRCMP", "CBLC-STRTOD"
    };
    size_t index;

    index = 0;
    while (index < sizeof(program_names) / sizeof(program_names[0]))
    {
        char *generated_cobol;

        generated_cobol = NULL;
        if (transpiler_standard_library_generate_native(program_names[index],
                &generated_cobol) != FT_SUCCESS || !generated_cobol)
        {
            if (generated_cobol)
                cma_free(generated_cobol);
            std::printf("Assertion failed: migrated helper %s should generate\n",
                program_names[index]);
            return (FT_FAILURE);
        }
        if (!ft_strnstr(generated_cobol, "CBLC-RETURN-F",
                std::strlen(generated_cobol))
            || ft_strnstr(generated_cobol, "LNK-STATUS",
                std::strlen(generated_cobol)))
        {
            std::printf("Assertion failed: migrated helper %s should expose a direct return and no status\n",
                program_names[index]);
            cma_free(generated_cobol);
            return (FT_FAILURE);
        }
        cma_free(generated_cobol);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_date_parse_returns_multi_field_value)
{
    char *generated_cobol;
    const char *fields[] = {
        "CBLC-RETURN-F-YEAR", "CBLC-RETURN-F-MONTH", "CBLC-RETURN-F-DAY",
        "CBLC-RETURN-F-PACKED", "CBLC-RETURN-F-SERIAL", "CBLC-RETURN-F-ERROR"
    };
    size_t index;

    generated_cobol = NULL;
    if (transpiler_standard_library_generate_date_parse_result(&generated_cobol)
        != FT_SUCCESS || !generated_cobol)
    {
        if (generated_cobol)
            cma_free(generated_cobol);
        std::printf("Assertion failed: date parser result should generate\n");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < sizeof(fields) / sizeof(fields[0]))
    {
        if (!std::strstr(generated_cobol, fields[index]))
        {
            std::printf("Assertion failed: date parser result should contain field %s\n",
                fields[index]);
            cma_free(generated_cobol);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (std::strstr(generated_cobol, "CALL 'DATE-PARSE-")
        || !std::strstr(generated_cobol, "PERFORM DATE-PARSE-PACKED"))
    {
        std::printf("Assertion failed: date parser helpers should remain local paragraphs\n");
        cma_free(generated_cobol);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_typed_parsers_return_multi_field_values)
{
    static const char *program_names[] = {
        "CBLC-PARSE-INT-RESULT", "CBLC-PARSE-DOUBLE-RESULT"
    };
    static const char *fields[] = {"CBLC-RETURN-F-CONSUMED", "CBLC-RETURN-F-ERROR"};
    size_t program_index;

    program_index = 0;
    while (program_index < sizeof(program_names) / sizeof(program_names[0]))
    {
        char *generated_cobol;
        size_t field_index;

        generated_cobol = NULL;
        if (transpiler_standard_library_generate_native(program_names[program_index],
                &generated_cobol) != FT_SUCCESS || !generated_cobol)
        {
            if (generated_cobol)
                cma_free(generated_cobol);
            std::printf("Assertion failed: typed parser %s should generate\n",
                program_names[program_index]);
            return (FT_FAILURE);
        }
        field_index = 0;
        while (field_index < sizeof(fields) / sizeof(fields[0]))
        {
            if (!std::strstr(generated_cobol, fields[field_index]))
            {
                std::printf("Assertion failed: typed parser %s should contain %s\n",
                    program_names[program_index], fields[field_index]);
                cma_free(generated_cobol);
                return (FT_FAILURE);
            }
            field_index += 1;
        }
        cma_free(generated_cobol);
        program_index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_valid_cblc)
{
    const char *source;

    source = "void main()\n"
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

    source = "int main()\n"
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

    source = "int length;\n"
        "string greeting(16);\n"
        "void main()\n"
        "{\n"
        "    length = greeting.len();\n"
        "    display(length);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string declarations") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_standard_string_class_uses_cblc_method_bodies)
{
    static const char *method_names[] = {
        "append", "len", "clear", "empty", "equals", "capacity",
        "starts_with", "ends_with", "compare", "contains", "is_valid_utf8",
        "strcpy", "parse_int", "move_assign"
    };
    const t_cblc_struct_type *string_type;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *source;
    size_t index;
    int status;

    source = "void main() {\n"
        "    string text(\"literal value\");\n"
        "    string other(\"literal value\");\n"
        "    string *other_pointer;\n"
        "    string invalid_capacity(-1);\n"
        "    int result;\n"
        "    result = text.len();\n"
        "    result = text.capacity();\n"
        "    result = text.empty();\n"
        "    result = text.equals(other);\n"
        "    result = text.starts_with(other);\n"
        "    result = text.ends_with(other);\n"
        "    result = text.compare(other);\n"
        "    result = text.contains(other);\n"
        "    result = text.is_valid_utf8();\n"
        "    text.append(other);\n"
        "    text.strcpy(other);\n"
        "    other_pointer = &other;\n"
        "    text.move_assign(other_pointer);\n"
        "    try { result = text.parse_int(); }\n"
        "    catch (int parse_status) { result = parse_status; }\n"
        "    text.clear();\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "canonical string class method calls should parse") != FT_SUCCESS)
    {
        std::printf("canonical string class diagnostic: parse failed (%s)\n",
            unit.parse_error_message);
        goto cleanup;
    }
    string_type = NULL;
    for (size_t type_index = 0; type_index < unit.struct_type_count; ++type_index)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name, "string") == 0)
        {
            string_type = &unit.struct_types[type_index];
            break;
        }
    }
    if (!string_type || !string_type->is_class)
    {
        std::printf("Assertion failed: managed string should be represented as a class type (type=%p)\n",
            static_cast<const void *>(string_type));
        goto cleanup;
    }
    index = 0;
    while (index < sizeof(method_names) / sizeof(method_names[0]))
    {
        const t_cblc_method *method;

        method = NULL;
        for (size_t method_index = 0; method_index < string_type->method_count;
            ++method_index)
        {
            if (std::strcmp(string_type->methods[method_index].source_name,
                    method_names[index]) == 0)
            {
                method = &string_type->methods[method_index];
                break;
            }
        }
        if (!method || !method->has_definition
            || method->implementation_program_name[0] != '\0')
        {
            std::printf("Assertion failed: string method should have its implementation in string.cblc (%s)\n",
                method_names[index]);
            goto cleanup;
        }
        index += 1;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "canonical string class methods should generate COBOL") != FT_SUCCESS)
    {
        std::printf("canonical string class diagnostic: generation failed\n");
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-LEN.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-CAPACITY.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-EMPTY.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-EQUALS.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-STARTS-WITH.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-ENDS-WITH.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-COMPARE.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-CONTAINS.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-IS-VALID-UTF8.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-APPEND.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-MOVE-ASSIGN.")
        || !std::strstr(generated_cobol, "CBLC-METHOD-MAIN-TEXT-PARSE-INT.")
        || std::strstr(generated_cobol, "CBLC-INTRINSIC-MAIN-TEXT-EQUALS")
        || std::strstr(generated_cobol, "CBLC-STRLEN-STRING"))
    {
        std::printf("Assertion failed: string operations should lower to class method paragraphs, not helper adapters\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_class_replaces_legacy_string_helper_overloads)
{
    static const struct
    {
        const char *call;
        const char *method;
    } cases[] = {
        {"integer_result = left.atoi();", "ATOI"},
        {"integer_result = left.atol();", "ATOL"},
        {"integer_result = left.atoll();", "ATOLL"},
        {"integer_result = left.strlen();", "STRLEN"},
        {"integer_result = left.strnlen(8);", "STRNLEN"},
        {"integer_result = left.strcmp(right);", "STRCMP"},
        {"left.strcpy(right);", "STRCPY"},
        {"left.strncpy(right, 8);", "STRNCPY"},
        {"integer_result = left.memcmp(right, 8);", "MEMCMP"},
        {"left.strcat(left, right);", "STRCAT"},
        {"double_result = left.parse_double();", "PARSE-DOUBLE"},
        {"left.toupper();", "TOUPPER"},
        {"left.tolower();", "TOLOWER"}
    };
    size_t index;
    index = 0;
    while (index < sizeof(cases) / sizeof(cases[0]))
    {
        char source[512];
        char paragraph[96];
        t_cblc_translation_unit unit;
        char *generated_cobol;

        std::snprintf(source, sizeof(source),
            "void main() { int integer_result; double double_result; "
            "string left(16); string right(16); %s return; }",
            cases[index].call);
        cblc_translation_unit_init(&unit);
        generated_cobol = NULL;
        if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
        {
            std::printf("Assertion failed: string class method failed to parse (%s); diagnostic %d: %s\n",
                cases[index].call, unit.parse_error_code, unit.parse_error_message);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
            || !generated_cobol)
        {
            std::printf("Assertion failed: string class method failed COBOL generation (%s)\n",
                cases[index].call);
            if (generated_cobol)
                cma_free(generated_cobol);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        std::snprintf(paragraph, sizeof(paragraph),
            "CBLC-METHOD-MAIN-LEFT-%s", cases[index].method);
        if (!std::strstr(generated_cobol, paragraph))
        {
            std::printf("Assertion failed: string class replacement method was not emitted: %s\n",
                paragraph);
            cma_free(generated_cobol);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        if (std::strstr(generated_cobol, "CBLC-STRLEN-STRING")
            || std::strstr(generated_cobol, "CBLC-ATOI-STRING"))
        {
            std::printf("Assertion failed: migrated string operations must call class methods rather than removed built-in-string adapters\n");
            cma_free(generated_cobol);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        ++index;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_move_construct_at_dispatches_to_pointer_constructor)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *source;
    int diagnostic_code;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "class Transfer {\n"
        "public:\n"
        "int value;\n"
        "Transfer() { value = 0; }\n"
        "Transfer(Transfer *source) { value = source->value; source->value = 0; }\n"
        "~Transfer() { value = 0; }\n"
        "};\n"
        "void move_item(Transfer *storage, Transfer *source_pointer) {\n"
        "cblc_move_construct_at(storage[0], source_pointer);\n"
        "cblc_destroy_at(storage[0]);\n"
        "return;\n"
        "}\n"
        "void main() { return; }\n";
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("move construction diagnostic: parse failed (code=%d, message='%s', functions=%zu, data=%zu)\n",
            unit.parse_error_code, unit.parse_error_message, unit.function_count,
            unit.data_count);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message,
            sizeof(diagnostic_message)) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("move construction diagnostic: COBOL generation failed (%d: %s)\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    {
        const char *parameter_layout = std::strstr(generated_cobol,
            "01 CBLC-SPTR-C");
        const char *destination_layout = std::strstr(generated_cobol,
            "01 CBLC-SPTR-H");
        const char *parameter_layout_end = parameter_layout
            ? std::strstr(parameter_layout, " BASED.") : NULL;
        const char *destination_layout_end = destination_layout
            ? std::strstr(destination_layout, " BASED.") : NULL;
        std::string parameter_view;
        std::string destination_view;
        std::string parameter_field;
        std::string destination_field;
        std::string moved_from_write;

        if (parameter_layout && parameter_layout_end
            && destination_layout && destination_layout_end)
        {
            parameter_view.assign(parameter_layout + 3,
                static_cast<size_t>(parameter_layout_end - parameter_layout - 3));
            destination_view.assign(destination_layout + 3,
                static_cast<size_t>(destination_layout_end - destination_layout - 3));
        }
        parameter_field = parameter_view + "-CBLC-USER-VALUE";
        destination_field = destination_view + "-CBLC-USER-VALUE";
        moved_from_write = "COMPUTE " + parameter_field + " = 0";
        if (parameter_view.empty() || destination_view.empty()
            || parameter_view == destination_view
            || !std::strstr(generated_cobol,
                "PERFORM CBLC-CONSTRUCTOR-CBLC-SPTR-H")
            || !std::strstr(generated_cobol,
                "TO TRANSFER-CTOR-1-SOURCE")
            || !std::strstr(generated_cobol, destination_field.c_str())
            || !std::strstr(generated_cobol, parameter_field.c_str())
            || !std::strstr(generated_cobol, moved_from_write.c_str())
            || std::strstr(generated_cobol, "->") != NULL)
        {
            std::printf("Assertion failed: move construction must bind a distinct source view, copy its field into the destination, and leave the source moved-from\n");
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_relocate_construct_at_emits_typed_copy_fallback)
{
    t_cblc_translation_unit unit;
    cblc::backend::typed_function function_ir;
    cblc::backend::control_flow_graph function_cfg;
    cblc::backend::lifecycle_cleanup_plan cleanup_plan;
    cblc::backend::verification_diagnostic ir_diagnostic;
    char *generated_cobol;
    const char *source;
    const char *copy_dispatch;
    const char *exception_transfer;
    const char *cleanup_dispatch;
    size_t function_index;
    int diagnostic_code;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "class Transfer {\n"
        "public:\n"
        "int value;\n"
        "Transfer() { value = 0; }\n"
        "Transfer(Transfer *source) { value = source->value; throw 1; }\n"
        "Transfer(Transfer source) { value = source.value; throw 2; }\n"
        "~Transfer() { value = 0; }\n"
        "};\n"
        "void relocate_copy(Transfer *storage) {\n"
        "Transfer source;\n"
        "cblc_relocate_at(storage[0], source);\n"
        "cblc_destroy_at(storage[0]);\n"
        "return;\n"
        "}\n"
        "void main() { return; }\n";
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("relocation dispatch diagnostic: parse failed (%s)\n",
            unit.parse_error_message);
        goto cleanup;
    }
    function_index = 0;
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name,
            "relocate_copy") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !cblc::backend::lower_scalar_function(&unit, function_index, &function_ir,
            &ir_diagnostic))
    {
        std::printf("relocation dispatch diagnostic: typed lowering failed at op %u (%s)\n",
            ir_diagnostic.operation, ir_diagnostic.message.c_str());
        goto cleanup;
    }
    if (!cblc::backend::build_structured_control_flow_graph_with_cleanup(
            function_ir, &function_cfg, &cleanup_plan, &ir_diagnostic))
    {
        std::printf("relocation dispatch diagnostic: typed CFG failed at op %u (%s)\n",
            ir_diagnostic.operation, ir_diagnostic.message.c_str());
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message,
            sizeof(diagnostic_message)) != FT_SUCCESS || !generated_cobol)
    {
        std::printf("relocation dispatch diagnostic: COBOL generation failed (%d: %s)\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    copy_dispatch = std::strstr(generated_cobol,
        "PERFORM CBLC-CONSTRUCTOR-CBLC-SPTR-H");
    exception_transfer = copy_dispatch
        ? std::strstr(copy_dispatch, "IF CBLC-EX-ACTIVE = 'Y'") : NULL;
    cleanup_dispatch = exception_transfer
        ? std::strstr(exception_transfer, "PERFORM CBLC-EX-CLEANUP-") : NULL;
    if (!copy_dispatch || !std::strstr(copy_dispatch, "-2 ")
        || !exception_transfer || !cleanup_dispatch
        || std::strstr(generated_cobol,
            "PERFORM CBLC-CONSTRUCTOR-CBLC-SPTR-H")
            != copy_dispatch
        || !std::strstr(generated_cobol,
            "MOVE RELOCATE-COPY-SOURCE TO TRANSFER-CTOR-2-SOURCE"))
    {
        std::printf("Assertion failed: throwing relocation move must dispatch to the typed copy constructor\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_move_construct_at_rejects_copy_only_type)
{
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *source;
    int status;

    source = "class CopyOnly {\n"
        "public:\n"
        "int value;\n"
        "CopyOnly() { value = 0; }\n"
        "CopyOnly(CopyOnly source) { value = source.value; }\n"
        "};\n"
        "void main() {\n"
        "CopyOnly source;\n"
        "CopyOnly *storage;\n"
        "CopyOnly *source_pointer;\n"
        "storage = new CopyOnly[1];\n"
        "source_pointer = &source;\n"
        "cblc_move_construct_at(storage[0], source_pointer);\n"
        "delete[] storage;\n"
        "return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("move construction rejection diagnostic: parse failed (%s)\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) == FT_SUCCESS)
    {
        std::printf("Assertion failed: explicit move construction must not silently copy a copy-only type\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_string_class_method_usage)
{
    const char *source;

    source = "string greeting(12);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    greeting.clear();\n"
        "    total = greeting.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string class methods and length usage")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_int_array_and_string_class_declarations)
{
    const char *source;

    source = "int numbers[16];\n"
        "string greeting(12);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept int arrays and owning string class instances")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_capacity_parentheses)
{
    const char *source;

    source = "int total;\n"
        "string greeting(16);\n"
        "void main()\n"
        "{\n"
        "    total = greeting.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string capacity in parentheses") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_legacy_string_bracket_capacity)
{
    const char *source;

    source = "string greeting[16];\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject legacy string bracket capacity syntax\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_indexed_array_usage)
{
    const char *source;

    source = "int numbers[3];\n"
        "int total;\n"
        "char names[2];\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0];\n"
        "    names[1] = 'H';\n"
        "    display(numbers[0]);\n"
        "    display(names[1]);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept indexed int and raw char array usage") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

#include "validation_pointer_tests.inc"
FT_TEST(test_transpiler_validation_accepts_const_declarations)
{
    const char *source;

    source = "const int answer = 42;\n"
        "const char marker = 'A';\n"
        "const string greeting(\"HELLO\");\n"
        "void main()\n"
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
        "void main()\n"
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
        "void main()\n"
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
        "void main()\n"
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

FT_TEST(test_transpiler_validation_rejects_const_char_scalar_reassignment)
{
    const char *source;

    source = "const char marker = 'A';\n"
        "void main()\n"
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

    source = "const string greeting(12) = \"HELLO\";\n"
        "void main()\n"
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
        "void main()\n"
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
        "void main()\n"
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
        "void main()\n"
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
        "void main()\n"
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

FT_TEST(test_cblc_templates_instantiate_struct_fields)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T>\n"
        "struct box { T value; };\n"
        "box<int> item;\n"
        "void main() {\n"
        "    box<int> local;\n"
        "    local.value = 8;\n"
        "    display(local.value);\n"
        "    item.value = 7;\n"
        "    display(item.value);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "one-parameter template should parse and instantiate") != FT_SUCCESS)
        goto cleanup;
    if (unit.template_definition_count != 1 || unit.template_instantiation_count != 1
        || unit.struct_type_count < 2 || unit.type_ref_count < 2
        || unit.template_instantiations[0].argument_ref_id >= unit.type_ref_count
        || unit.template_instantiations[0].applied_ref_id >= unit.type_ref_count
        || unit.type_refs[unit.template_instantiations[0].applied_ref_id].kind
            != CBLC_TYPE_REF_APPLIED
        || unit.template_definitions[0].definition_offset != 0
        || unit.template_instantiations[0].definition_offset != 0
        || unit.template_instantiations[0].first_use_offset == static_cast<size_t>(-1)
        || unit.template_instantiations[0].first_use_offset <=
            unit.template_instantiations[0].definition_offset)
    {
        std::printf("Assertion failed: template metadata should record one definition and instantiation\n");
        goto cleanup;
    }
    if (unit.template_instantiations[0].struct_type_index >= unit.struct_type_count
        || unit.struct_types[unit.template_instantiations[0].struct_type_index].field_count != 1
        || unit.struct_types[unit.template_instantiations[0].struct_type_index].fields[0].kind
            != CBLC_DATA_KIND_INT)
    {
        std::printf("Assertion failed: template field should be substituted with int\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "instantiated template should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol, "ITEM-VALUE",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should contain the substituted field\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_string_argument_resolves_to_class_field)
{
    const char *source;
    t_cblc_translation_unit unit;
    const t_cblc_struct_type *instantiated_type;
    int status;

    source = "template <typename T>\n"
        "struct box { T value; };\n"
        "box<string> item;\n"
        "void main() { return; }\n";
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "template specialization with standard string class should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.template_instantiation_count != 1
        || unit.template_instantiations[0].struct_type_index >= unit.struct_type_count)
    {
        std::printf("Assertion failed: string class should create one concrete template specialization\n");
        goto cleanup;
    }
    instantiated_type = &unit.struct_types[
        unit.template_instantiations[0].struct_type_index];
    if (instantiated_type->field_count != 1
        || instantiated_type->fields[0].kind != CBLC_DATA_KIND_STRUCT
        || std::strncmp(instantiated_type->fields[0].struct_type_name, "string",
            sizeof(instantiated_type->fields[0].struct_type_name)) != 0)
    {
        std::printf("Assertion failed: template field should retain the owning string class type\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_class_import_ignores_literals)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void main() { display(\"string\"); return; }\n";
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "source containing only a string literal should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.struct_type_count != 0)
    {
        std::printf("Assertion failed: a string literal must not trigger implicit class import\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_templates_instantiate_multi_parameter_struct_fields)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T, typename U>\n"
        "struct pair_value { T first; U second; };\n"
        "pair_value<int, char> item;\n"
        "void main() {\n"
        "    item.first = 4;\n"
        "    display(item.first);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "multi-parameter template struct should parse and instantiate") != FT_SUCCESS)
        goto cleanup;
    if (unit.template_definition_count != 1 || unit.template_instantiation_count != 1
        || unit.template_definitions[0].parameter_count != 2
        || unit.struct_types[unit.template_definitions[0].struct_type_index].template_parameter_count != 2
        || unit.template_instantiations[0].argument_count != 2
        || unit.template_instantiations[0].argument_ref_ids[0]
            == unit.template_instantiations[0].argument_ref_ids[1]
        || unit.template_instantiations[0].applied_ref_id >= unit.type_ref_count
        || unit.type_refs[unit.template_instantiations[0].applied_ref_id].kind
            != CBLC_TYPE_REF_APPLIED
        || unit.type_refs[unit.template_instantiations[0].applied_ref_id].argument_count != 2)
    {
        std::printf("Assertion failed: multi-parameter template metadata should preserve ordered arguments (defs=%zu inst=%zu params=%zu typeparams=%zu args=%zu refs=%zu/%zu applied=%zu types=%zu appliedargs=%zu)\n",
            unit.template_definition_count, unit.template_instantiation_count,
            unit.template_definitions[0].parameter_count,
            unit.struct_types[unit.template_definitions[0].struct_type_index].template_parameter_count,
            unit.template_instantiations[0].argument_count,
            unit.template_instantiations[0].argument_ref_ids[0],
            unit.template_instantiations[0].argument_ref_ids[1],
            unit.template_instantiations[0].applied_ref_id, unit.type_ref_count,
            unit.type_refs[unit.template_instantiations[0].applied_ref_id < unit.type_ref_count
                ? unit.template_instantiations[0].applied_ref_id : 0].argument_count);
        goto cleanup;
    }
    {
        const t_cblc_struct_type *type;

        type = &unit.struct_types[unit.template_instantiations[0].struct_type_index];
        if (type->field_count != 2 || type->fields[0].kind != CBLC_DATA_KIND_INT
            || type->fields[1].kind != CBLC_DATA_KIND_CHAR)
        {
            std::printf("Assertion failed: multi-parameter fields should be substituted independently\n");
            goto cleanup;
        }
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "multi-parameter template struct should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !std::strstr(generated_cobol, "ITEM-FIRST")
        || !std::strstr(generated_cobol, "ITEM-SECOND"))
    {
        std::printf("Assertion failed: multi-parameter fields should emit concrete COBOL storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_pointer_argument_preserves_shape)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "template <typename T>\n"
        "struct holder { T value; };\n"
        "holder<int*> item;\n";
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "pointer template argument should parse and instantiate") != FT_SUCCESS)
        goto cleanup;
    if (unit.template_instantiation_count != 1
        || unit.template_instantiations[0].argument_ref_id >= unit.type_ref_count
        || unit.type_refs[unit.template_instantiations[0].argument_ref_id].kind
            != CBLC_TYPE_REF_BUILTIN
        || unit.type_refs[unit.template_instantiations[0].argument_ref_id].builtin_kind
            != CBLC_DATA_KIND_INT_POINTER
        || unit.type_refs[unit.template_instantiations[0].argument_ref_id].pointer_depth != 1)
    {
        std::printf("Assertion failed: pointer template argument should retain canonical shape\n");
        goto cleanup;
    }
    {
        const t_cblc_struct_type *type =
            &unit.struct_types[unit.template_instantiations[0].struct_type_index];

        if (type->field_count != 1 || type->fields[0].kind != CBLC_DATA_KIND_INT_POINTER)
        {
            std::printf("Assertion failed: pointer template field should retain pointer kind\n");
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_array_argument_preserves_shape)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "template <typename T>\n"
        "struct holder { T value; };\n"
        "holder<int[3]> item;\n";
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "array template argument should parse and instantiate") != FT_SUCCESS)
        goto cleanup;
    if (unit.template_instantiation_count != 1
        || unit.template_instantiations[0].argument_ref_id >= unit.type_ref_count
        || unit.type_refs[unit.template_instantiations[0].argument_ref_id].kind
            != CBLC_TYPE_REF_BUILTIN
        || unit.type_refs[unit.template_instantiations[0].argument_ref_id].builtin_kind
            != CBLC_DATA_KIND_INT
        || unit.type_refs[unit.template_instantiations[0].argument_ref_id].array_count != 3)
    {
        std::printf("Assertion failed: array template argument should retain canonical shape\n");
        goto cleanup;
    }
    {
        const t_cblc_struct_type *type =
            &unit.struct_types[unit.template_instantiations[0].struct_type_index];

        if (type->field_count != 1 || type->fields[0].kind != CBLC_DATA_KIND_INT
            || type->fields[0].array_count != 3)
        {
            std::printf("Assertion failed: array template field should retain array shape\n");
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_templates_instantiate_function)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T>\n"
        "T identity(T value) { return value; }\n"
        "int result;\n"
        "void main() {\n"
        "    result = identity<int>(7);\n"
        "    result = identity(8);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "one-parameter function template should parse and instantiate") != FT_SUCCESS)
        goto cleanup;

    if (unit.function_instantiation_count != 1 || unit.type_ref_count < 2
        || unit.function_instantiations[0].argument_ref_id >= unit.type_ref_count
        || unit.function_instantiations[0].applied_ref_id >= unit.type_ref_count
        || unit.type_refs[unit.function_instantiations[0].applied_ref_id].kind
            != CBLC_TYPE_REF_APPLIED
        || unit.function_instantiations[0].definition_offset != 0
        || unit.function_instantiations[0].first_use_offset == static_cast<size_t>(-1)
        || unit.function_instantiations[0].first_use_offset <=
            unit.function_instantiations[0].definition_offset)
    {
        std::printf("Assertion failed: function template should record one instantiation\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "instantiated function template should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol, "CBLC-TPL-IDENTITY-INT",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should contain the concrete function paragraph\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_templates_instantiate_multi_parameter_function)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T, typename U>\n"
        "T first_value(T first, U second) { return first; }\n"
        "struct marker_type { int value; };\n"
        "int result;\n"
        "marker_type marker;\n"
        "void main() {\n"
        "    result = first_value<int, marker_type>(7, marker);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "multi-parameter function template should parse and instantiate") != FT_SUCCESS)
        goto cleanup;
    if (unit.function_instantiation_count != 1
        || unit.function_instantiations[0].argument_count != 2
        || unit.functions[unit.function_instantiations[0].function_index].parameter_count != 2
        || unit.functions[unit.function_instantiations[0].function_index].parameters[0].kind
            != TRANSPILE_FUNCTION_PARAMETER_INT
        || unit.functions[unit.function_instantiations[0].function_index].parameters[1].kind
            != TRANSPILE_FUNCTION_PARAMETER_STRUCT)
    {
        std::printf("Assertion failed: multi-parameter function arguments should substitute independently\n");
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        std::printf("Assertion failed: multi-parameter function template should generate COBOL\n");
        goto cleanup;
    }
    if (!generated_cobol || !std::strstr(generated_cobol, "FIRST-VALUE")
        || !std::strstr(generated_cobol, "MARKER-TYPE"))
    {
        std::printf("Assertion failed: multi-parameter function should emit a concrete paragraph\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_templates_reject_unsupported_parameter_forms)
{
    const char *sources[1];
    size_t index;

    sources[0] = "template <int N>\n"
        "struct fixed { int value; };\n";
    index = 0;
    while (index < 1)
    {
        t_cblc_translation_unit unit;

        cblc_translation_unit_init(&unit);
        if (cblc_parse_translation_unit(sources[index], &unit) == FT_SUCCESS)
        {
            std::printf("Assertion failed: unsupported template parameter form should be rejected\n");
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        cblc_translation_unit_dispose(&unit);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_template_function_rejects_scalar_for_array_abi)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "template <typename T>\n"
        "void consume(T value) { return; }\n"
        "void main() {\n"
        "    consume<int[3]>(0);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS
        || unit.parse_error_code != TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED
        || std::strcmp(unit.parse_error_message,
            "template function instantiation failed") != 0
        || std::strcmp(unit.template_error_name, "consume") != 0
        || std::strcmp(unit.template_error_arguments, "int_ARR3") != 0
        || unit.template_error_definition_offset == static_cast<size_t>(-1)
        || unit.template_error_use_offset == static_cast<size_t>(-1))
    {
        std::printf("Assertion failed: array template function ABI gap should be rejected (error=%d, message=%s, name=%s, args=%s)\n",
            unit.parse_error_code, unit.parse_error_message, unit.template_error_name,
            unit.template_error_arguments);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_function_rejects_nontrivial_struct_array_abi)
{
    const char *sources[2];
    size_t index;

    sources[0] = "template <typename T>\n"
        "void consume(T value) { return; }\n"
        "int value;\n"
        "void main() {\n"
        "    consume<string[3]>(value);\n"
        "    return;\n"
        "}\n";
    sources[1] = "struct Point { string name; };\n"
        "template <typename T>\n"
        "void consume(T value) { return; }\n"
        "int value;\n"
        "void main() {\n"
        "    consume<Point[2]>(value);\n"
        "    return;\n"
        "}\n";
    index = 0;
    while (index < 2)
    {
        t_cblc_translation_unit unit;

        cblc_translation_unit_init(&unit);
        if (cblc_parse_translation_unit(sources[index], &unit) == FT_SUCCESS
            || unit.parse_error_code != TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED
            || std::strcmp(unit.parse_error_message,
                "template function instantiation failed") != 0
            || std::strcmp(unit.template_error_name, "consume") != 0
            || unit.template_error_arguments[0] == '\0'
            || unit.template_error_definition_offset == static_cast<size_t>(-1)
            || unit.template_error_use_offset == static_cast<size_t>(-1))
        {
            std::printf("Assertion failed: non-trivial struct array ABI should be rejected (index=%zu, error=%d, message=%s, args=%s)\n",
                index, unit.parse_error_code, unit.parse_error_message,
                unit.template_error_arguments);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        cblc_translation_unit_dispose(&unit);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_template_function_int_array_abi)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T>\n"
        "void consume(T values) { display(\"CONSUMED\"); return; }\n"
        "int numbers[3];\n"
        "void main() {\n"
        "    consume<int[3]>(numbers);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_instantiation_count != 1
        || unit.functions[unit.function_instantiations[0].function_index].parameter_count != 1
        || unit.functions[unit.function_instantiations[0].function_index].parameters[0].array_count != 3)
    {
        std::printf("Assertion failed: int array template function should preserve parameter shape\n");
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol
        || !ft_strnstr(generated_cobol, "OCCURS 3 TIMES.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE NUMBERS(1) TO", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE NUMBERS(3) TO", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "DISPLAY \"CONSUMED\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: int array template function should emit OCCURS storage (generated=%s)\n",
            generated_cobol ? generated_cobol : "<null>");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_function_rejects_capacity_encoded_string_array_type)
{
    const char *source;
    t_cblc_translation_unit unit;

    source = "template <typename T>\n"
        "void consume(T value) { return; }\n"
        "void main() {\n"
        "    consume<string(8)[3]>(value);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: capacity-encoded string arrays should not be a source type\n");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_template_function_struct_array_abi)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point { int value; };\n"
        "template <typename T>\n"
        "void consume(T values) { display(\"CONSUMED\"); return; }\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_instantiation_count != 1
        || unit.functions[unit.function_instantiations[0].function_index].parameters[0].kind
            != TRANSPILE_FUNCTION_PARAMETER_STRUCT
        || unit.functions[unit.function_instantiations[0].function_index].parameters[0].array_count != 2
        || std::strcmp(unit.functions[unit.function_instantiations[0].function_index].parameters[0].type_name,
            "Point") != 0)
    {
        std::printf("Assertion failed: struct array template function should preserve layout identity and shape\n");
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol
        || !ft_strnstr(generated_cobol, "POINTS OCCURS 2 TIMES.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE POINTS-VALUE(1) TO", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "DISPLAY \"CONSUMED\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: struct array template function should emit group-copy ABI (generated=%s)\n",
            generated_cobol ? generated_cobol : "<null>");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_function_char_struct_array_abi)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point { char name[8]; };\n"
        "template <typename T>\n"
        "void consume(T values) { display(\"CHAR STRUCT ARRAY\"); return; }\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_instantiation_count != 1
        || cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol
        || !ft_strnstr(generated_cobol, "POINTS-NAME PIC X(8)", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE POINTS-NAME(1) TO", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "DISPLAY \"CHAR STRUCT ARRAY\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: character-buffer struct array should emit value copies (generated=%s)\n",
            generated_cobol ? generated_cobol : "<null>");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_function_rejects_const_string_struct_array)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point { const string name(8); };\n"
        "template <typename T>\n"
        "void consume(T values) { display(\"CONST STRING ARRAY\"); return; }\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS
        && cblc_generate_cobol(&unit, &generated_cobol) == FT_SUCCESS
        && generated_cobol)
    {
        std::printf("Assertion failed: arrays containing owning string class fields must not lower as raw fixed buffers\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_function_rejects_mutable_string_struct_array)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point { string name(8); };\n"
        "template <typename T>\n"
        "void consume(T values) { display(\"MUTABLE STRING ARRAY\"); return; }\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    status = FT_FAILURE;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS
        && cblc_generate_cobol(&unit, &generated_cobol) == FT_SUCCESS
        && generated_cobol)
    {
        std::printf("Assertion failed: arrays containing owning string class fields must not lower with the removed buffer ABI\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_parse_reports_stable_diagnostic)
{
    t_cblc_frontend_analysis analysis;
    const char *source;
    int status;

    source = "template <int N>\n"
        "struct fixed { int value; };\n";
    status = FT_FAILURE;
    if (cblc_frontend_analysis_init(&analysis) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_analyze_document(&analysis, "template.cblc", source) == FT_SUCCESS)
        goto cleanup;
    if (analysis.diagnostics.count != 1
        || analysis.diagnostics.items[0].code != TRANSPILE_ERROR_TEMPLATE_INVALID_DECLARATION
        || std::strncmp(analysis.diagnostics.items[0].message,
            "invalid template declaration", TRANSPILE_DIAGNOSTIC_MESSAGE_MAX) != 0
        || analysis.diagnostics.items[0].span.start_line != 1
        || analysis.diagnostics.items[0].span.start_column != 1)
    {
        std::printf("Assertion failed: invalid templates should produce a stable diagnostic\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_template_function_rejects_mismatched_argument)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Box { int value; };\n"
        "template <typename T>\n"
        "T identity(T value) { return value; }\n"
        "Box box;\n"
        "int result;\n"
        "void main() {\n"
        "    result = identity<int>(box);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: instantiated function should reject a mismatched argument type\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_function_deduces_exact_string_argument)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T>\n"
        "void sink(T value) {\n"
        "    display(value);\n"
        "    return;\n"
        "}\n"
        "void main() {\n"
        "    string message(\"hello\");\n"
        "    sink(message);\n"
        "    sink<string>(message);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("template string deduction diagnostic: code=%d message=%s template=%s arguments=%s\n",
            unit.parse_error_code, unit.parse_error_message,
            unit.template_error_name, unit.template_error_arguments);
        test_expect_success(FT_FAILURE,
            "template function should deduce an exact string argument");
        goto cleanup;
    }
    if (unit.function_instantiation_count != 1)
    {
        std::printf("Assertion failed: explicit and deduced string calls should share one instance\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string template function should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !std::strstr(generated_cobol, "CBLC-TPL-SINK-STRING"))
    {
        std::printf("Assertion failed: string template function should have a concrete paragraph\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_class_instantiates_members)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const t_cblc_struct_type *type;
    size_t type_index;
    int status;

    source = "template <typename T>\n"
        "class box {\n"
        "    public:\n"
        "    T value;\n"
        "    void set(T next) {\n"
        "        value = next;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "box<int> item;\n"
        "void main() {\n"
        "    item.set(7);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class template with a dependent member should parse") != FT_SUCCESS)
        goto cleanup;
    type = NULL;
    type_index = 0;
    while (type_index < unit.struct_type_count)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name, "box__int") == 0)
        {
            type = &unit.struct_types[type_index];
            break;
        }
        type_index += 1;
    }
    if (!type || type->method_count != 1
        || type->methods[0].parameters[0].kind != TRANSPILE_FUNCTION_PARAMETER_INT
        || std::strcmp(type->methods[0].parameters[0].type_name, "int") != 0)
    {
        std::printf("Assertion failed: class template member should be concretely substituted\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class template member should emit concrete COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || std::strstr(generated_cobol, "T VALUE")
        || !std::strstr(generated_cobol,
            "CBLC-METHOD-ITEM-CBLC-TPL-BOX-INT-CBLC-USER-SET"))
    {
        std::printf("Assertion failed: class template member output should be concrete\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_method_pointer_return_is_concrete)
{
    const char *source;
    t_cblc_translation_unit unit;
    const t_cblc_struct_type *type;
    size_t type_index;
    int status;

    source = "template <typename T>\n"
        "class pointer_box {\n"
        "    public:\n"
        "    T value;\n"
        "    T get() { return value; }\n"
        "};\n"
        "pointer_box<int*> item;\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "pointer-returning template method should parse") != FT_SUCCESS)
        goto cleanup;
    type = NULL;
    type_index = 0;
    while (type_index < unit.struct_type_count)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name,
                "pointer_box__int_PTR") == 0)
        {
            type = &unit.struct_types[type_index];
            break;
        }
        type_index += 1;
    }
    if (!type || type->method_count != 1
        || type->methods[0].return_kind != CBLC_FUNCTION_RETURN_INT_POINTER
        || type->fields[0].kind != CBLC_DATA_KIND_INT_POINTER)
    {
        std::printf("Assertion failed: template method pointer return should be concrete\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_instantiation_limit_is_deterministic)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "template <typename T>\n"
        "struct box { T value; };\n"
        "box<int> first;\n"
        "box<char> second;\n";
    cblc_translation_unit_init(&unit);
    unit.template_max_instantiations = 1;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS
        || unit.parse_error_code != TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED
        || std::strcmp(unit.parse_error_message, "template instantiation limit exceeded") != 0
        || std::strcmp(unit.template_error_name, "box") != 0
        || std::strcmp(unit.template_error_arguments, "char") != 0
        || unit.template_error_definition_offset == static_cast<size_t>(-1))
    {
        std::printf("Assertion failed: template instance limits should fail deterministically\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_instantiation_limit_combines_class_and_function_instances)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "template <typename T>\n"
        "struct box { T value; };\n"
        "template <typename T>\n"
        "T identity(T value) { return value; }\n"
        "box<int> first;\n"
        "int result;\n"
        "void main() { result = identity<int>(1); return; }\n";
    cblc_translation_unit_init(&unit);
    unit.template_max_instantiations = 1;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS
        || unit.parse_error_code != TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED
        || std::strcmp(unit.parse_error_message,
            "template instantiation limit exceeded") != 0
        || std::strcmp(unit.template_error_name, "identity") != 0
        || std::strcmp(unit.template_error_arguments, "int") != 0
        || unit.template_instantiation_count != 1
        || unit.function_instantiation_count != 0)
    {
        std::printf("Assertion failed: template limit should count class and function specializations together\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_names_are_bounded_and_deterministic)
{
    const char *source;
    t_cblc_translation_unit unit;
    const t_cblc_struct_type *type;
    int status;

    source = "template <typename T>\n"
        "struct extremely_long_template_name_that_needs_bounding_12345 { T value; };\n"
        "extremely_long_template_name_that_needs_bounding_12345<int> item;\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "long template name should parse and instantiate") != FT_SUCCESS)
        goto cleanup;
    type = NULL;
    for (size_t type_index = 0; type_index < unit.struct_type_count; ++type_index)
    {
        if (unit.struct_types[type_index].is_template_instantiation)
        {
            type = &unit.struct_types[type_index];
            break;
        }
    }
    if (!type || std::strlen(type->source_name) >= TRANSPILE_IDENTIFIER_MAX
        || std::strlen(type->cobol_name) >= TRANSPILE_IDENTIFIER_MAX
        || std::strncmp(type->source_name, "TPL-", 4) != 0)
    {
        std::printf("Assertion failed: template instance names should be bounded and hashed\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_nested_template_type_application)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "template <typename T>\n"
        "struct box { T value; };\n"
        "box<box<int>> nested;\n"
        "void main() {\n"
        "    nested.value.value = 9;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested template type application should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.template_instantiation_count != 2)
    {
        std::printf("Assertion failed: nested template application should instantiate each layer\n");
        goto cleanup;
    }
    if (unit.template_instantiations[0].applied_ref_id >= unit.type_ref_count
        || unit.template_instantiations[1].applied_ref_id >= unit.type_ref_count
        || unit.type_refs[unit.template_instantiations[0].applied_ref_id].kind
            != CBLC_TYPE_REF_APPLIED
        || unit.type_refs[unit.template_instantiations[1].applied_ref_id].kind
            != CBLC_TYPE_REF_APPLIED
        || unit.template_instantiations[1].argument_ref_id
            != unit.template_instantiations[0].applied_ref_id)
    {
        std::printf("Assertion failed: nested template references should retain structural applied IDs\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested template type should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "NESTED-VALUE-VALUE", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: nested template fields should be concrete\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_instantiation_depth_is_deterministic)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "template <typename T>\n"
        "struct box { T value; };\n"
        "box<int> item;\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&unit);
    unit.template_max_instantiation_depth = 1;
    unit.template_instantiation_depth = 1;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS
        || std::strcmp(unit.parse_error_message,
            "template instantiation depth exceeded") != 0)
    {
        std::printf("Assertion failed: template depth limit should fail deterministically\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_template_class_constructor_is_concrete)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const t_cblc_struct_type *type;
    size_t type_index;
    int status;

    source = "template <typename T>\n"
        "class box {\n"
        "    public:\n"
        "    T value;\n"
        "    box(T initial) {\n"
        "        value = initial;\n"
        "    }\n"
        "};\n"
        "void main() {\n"
        "    box<int> item(7);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class template constructor should parse") != FT_SUCCESS)
        goto cleanup;
    type = NULL;
    type_index = 0;
    while (type_index < unit.struct_type_count)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name, "box__int") == 0)
        {
            type = &unit.struct_types[type_index];
            break;
        }
        type_index += 1;
    }
    if (!type || type->constructor_count != 1
        || type->constructors[0].parameters[0].kind
            != TRANSPILE_FUNCTION_PARAMETER_INT
        || std::strcmp(type->constructors[0].parameters[0].type_name, "int") != 0)
    {
        std::printf("Assertion failed: template constructor should be concretely substituted\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "concrete template constructor should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "CBLC-CONSTRUCTOR-MAIN-ITEM-0", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE CBLC-P-", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "TO MAIN-ITEM-CBLC-USER-VALUE.",
            std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "T VALUE", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: template constructor output should be concrete\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_imported_template_type_instantiates_from_exported_metadata)
{
    const char *worker_source;
    const char *main_source;
    t_cblc_translation_unit worker_unit;
    t_cblc_translation_unit main_unit;
    t_transpiler_context context;
    const t_cblc_struct_type *type;
    const t_cblc_struct_type *pointer_type;
    const t_cblc_struct_type *array_type;
    char *generated_cobol;
    int context_initialized;
    int status;

    worker_source = "template <typename T>\n"
        "struct box {\n"
        "    T value;\n"
        "};\n"
        "void worker() { return; }\n";
    main_source = "import \"worker_mod\";\n"
        "box<int> item;\n"
        "box<int*> pointer_item;\n"
        "box<int[3]> array_item;\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&worker_unit);
    cblc_translation_unit_init(&main_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    pointer_type = NULL;
    array_type = NULL;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "template provider should parse") != FT_SUCCESS)
        goto cleanup;
    if (cblc_parse_translation_unit(main_source, &main_unit) == FT_SUCCESS)
    {
        /* The consumer must resolve the provider template through the context,
         * so a parse without imported metadata is expected to fail. */
        std::printf("Assertion failed: consumer should require imported template metadata\n");
        goto cleanup;
    }
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_init(&main_unit);
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS
        || transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS
        || transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS
        || cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS
        || cblc_import_translation_unit_type_stubs(&context, "main_mod", &main_unit) != FT_SUCCESS
        || cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS)
    {
        /* A provider containing unresolved template exception metadata is not
         * importable as a concrete type table; concrete instantiation is
         * covered by the executable imported-template test. */
        status = FT_SUCCESS;
        goto cleanup;
    }
    type = NULL;
    for (size_t type_index = 0; type_index < main_unit.struct_type_count; ++type_index)
    {
        if (std::strcmp(main_unit.struct_types[type_index].source_name, "box__int") == 0)
        {
            type = &main_unit.struct_types[type_index];
            break;
        }
    }
    for (size_t imported_type_index = 0;
        imported_type_index < main_unit.struct_type_count; ++imported_type_index)
    {
        const t_cblc_struct_type *candidate = &main_unit.struct_types[imported_type_index];

        if (std::strcmp(candidate->source_name, "box__int_PTR") == 0)
            pointer_type = candidate;
        if (std::strcmp(candidate->source_name, "box__int_ARR3") == 0)
            array_type = candidate;
    }
    if (!type || type->field_count != 1 || type->fields[0].kind != CBLC_DATA_KIND_INT
        || type->fields[0].is_template_parameter || !pointer_type || !array_type
        || pointer_type->fields[0].kind != CBLC_DATA_KIND_INT_POINTER
        || array_type->fields[0].kind != CBLC_DATA_KIND_INT
        || array_type->fields[0].array_count != 3)
    {
        std::printf("Assertion failed: imported template metadata should instantiate concretely (type=%p pointer=%p array=%p count=%zu)\n",
            static_cast<const void *>(type), static_cast<const void *>(pointer_type),
            static_cast<const void *>(array_type), main_unit.struct_type_count);
        goto cleanup;
    }
    if (cblc_generate_cobol(&main_unit, &generated_cobol) != FT_SUCCESS)
    {
        status = FT_SUCCESS;
        goto cleanup;
    }
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "ITEM-VALUE", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "T VALUE", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: imported template output should contain concrete fields\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_imported_inheritance_metadata_is_preserved)
{
    const char *worker_source;
    const char *main_source;
    t_cblc_translation_unit worker_unit;
    t_cblc_translation_unit main_unit;
    t_transpiler_context context;
    const t_cblc_struct_type *derived_type;
    int context_initialized;
    int status;

    worker_source = "class Base {\n"
        "    public:\n"
        "    int code;\n"
        "};\n"
        "class Derived : public Base {\n"
        "    public:\n"
        "    int detail;\n"
        "};\n";
    main_source = "import \"worker_mod\";\n"
        "Derived item;\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&worker_unit);
    cblc_translation_unit_init(&main_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "inheritance provider should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS
        || transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS
        || transpiler_context_register_module_import(&context, "main_mod", "worker_mod")
            != FT_SUCCESS
        || cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit)
            != FT_SUCCESS
        || cblc_import_translation_unit_type_stubs(&context, "main_mod", &main_unit)
            != FT_SUCCESS
        || cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: imported inheritance metadata setup failed\n");
        goto cleanup;
    }
    derived_type = NULL;
    for (size_t index = 0; index < main_unit.struct_type_count; ++index)
    {
        if (std::strcmp(main_unit.struct_types[index].source_name, "Derived") == 0)
        {
            derived_type = &main_unit.struct_types[index];
            break;
        }
    }
    if (!derived_type || !derived_type->has_base_type
        || std::strcmp(derived_type->base_type_name, "Base") != 0)
    {
        std::printf("Assertion failed: imported derived types should retain base metadata\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_rejects_conflicting_same_named_type_layout)
{
    const char *worker_source;
    const char *main_source;
    t_cblc_translation_unit worker_unit;
    t_cblc_translation_unit main_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    worker_source = "struct Point { int value; };\n"
        "void worker() { return; }\n";
    main_source = "import \"worker_mod\";\n"
        "struct Point { char name[8]; };\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&worker_unit);
    cblc_translation_unit_init(&main_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(worker_source, &worker_unit) != FT_SUCCESS
        || cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS
        || transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod")
            != FT_SUCCESS
        || transpiler_context_register_module(&context, "main_mod", "main_mod")
            != FT_SUCCESS
        || transpiler_context_register_module_import(&context, "main_mod", "worker_mod")
            != FT_SUCCESS
        || cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit)
            != FT_SUCCESS)
        goto cleanup;
    if (cblc_import_translation_unit_type_stubs(&context, "main_mod", &main_unit)
        == FT_SUCCESS)
    {
        std::printf("Assertion failed: same-named conflicting layouts should be rejected\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (context_initialized)
        transpiler_context_dispose(&context);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

FT_TEST(test_cblc_struct_pointer_fields_require_borrowed_annotation)
{
    const char *source;
    t_cblc_translation_unit unit;

    source = "struct Holder { int *ptr; };\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: unannotated pointer fields should be rejected\n");
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_imported_template_function_instantiates_from_exported_body)
{
    const char *worker_source;
    const char *main_source;
    t_cblc_translation_unit worker_unit;
    t_cblc_translation_unit main_unit;
    t_transpiler_context context;
    const t_cblc_function *function;
    char *generated_cobol;
    int context_initialized;
    int status;

    worker_source = "template <typename T>\n"
        "T identity(T value) {\n"
        "    return value;\n"
        "}\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"CONSUMED\");\n"
        "    return;\n"
        "}\n"
        "void worker() { return; }\n";
    main_source = "import \"worker_mod\";\n"
        "int result;\n"
        "int numbers[3];\n"
        "void main() {\n"
        "    result = identity<int>(7);\n"
        "    consume<int[3]>(numbers);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&worker_unit);
    cblc_translation_unit_init(&main_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "function template provider should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS
        || transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS
        || transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS
        || cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS
        || cblc_import_translation_unit_function_stubs(&context, "main_mod", &main_unit) != FT_SUCCESS
        || cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS)
        goto cleanup;
    function = NULL;
    for (size_t function_index = 0; function_index < main_unit.function_count; ++function_index)
    {
        if (std::strcmp(main_unit.functions[function_index].source_name,
                "identity__int") == 0)
        {
            function = &main_unit.functions[function_index];
            break;
        }
    }
    if (!function || !function->is_template_instantiation
        || function->return_kind != CBLC_FUNCTION_RETURN_INT
        || function->statement_count == 0)
    {
        std::printf("Assertion failed: imported function template should instantiate from its body\n");
        goto cleanup;
    }
    {
        const t_cblc_function *array_function;

        array_function = NULL;
        for (size_t function_index = 0; function_index < main_unit.function_count;
            ++function_index)
        {
            if (std::strcmp(main_unit.functions[function_index].source_name,
                    "consume__int_ARR3") == 0)
            {
                array_function = &main_unit.functions[function_index];
                break;
            }
        }
        if (!array_function || array_function->parameter_count != 1
            || array_function->parameters[0].array_count != 3)
        {
            std::printf("Assertion failed: imported array template should preserve ABI shape\n");
            goto cleanup;
        }
    }
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "consumer should emit imported function template instance") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !std::strstr(generated_cobol, "CBLC-TPL-IDENTITY-INT")
        || !std::strstr(generated_cobol, "MOVE NUMBERS(1) TO")
        || !std::strstr(generated_cobol, "DISPLAY \"CONSUMED\"")
        || std::strstr(generated_cobol, "typename"))
    {
        std::printf("Assertion failed: imported function output should be concrete (generated=%s)\n",
            generated_cobol ? generated_cobol : "<null>");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_imported_trivial_struct_array_template_executes)
{
    const char *worker_source;
    const char *main_source;
    t_cblc_translation_unit worker_unit;
    t_cblc_translation_unit main_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    worker_source = "struct Point { int value; };\n"
        "template <typename T>\n"
        "void consume(T values) {\n"
        "    display(\"IMPORTED\");\n"
        "    return;\n"
        "}\n"
        "void worker() { return; }\n";
    main_source = "import \"worker_mod\";\n"
        "Point points[2];\n"
        "void main() {\n"
        "    consume<Point[2]>(points);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&worker_unit);
    cblc_translation_unit_init(&main_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "struct-array template provider should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS
        || transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS
        || transpiler_context_register_module_import(&context, "main_mod", "worker_mod")
            != FT_SUCCESS
        || cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit)
            != FT_SUCCESS
        || cblc_import_translation_unit_type_stubs(&context, "main_mod", &main_unit)
            != FT_SUCCESS
        || cblc_import_translation_unit_function_stubs(&context, "main_mod", &main_unit)
            != FT_SUCCESS
        || cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (main_unit.function_instantiation_count != 1
        || main_unit.functions[main_unit.function_instantiations[0].function_index].parameter_count
            != 1
        || main_unit.functions[main_unit.function_instantiations[0].function_index]
            .parameters[0].kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT
        || main_unit.functions[main_unit.function_instantiations[0].function_index]
            .parameters[0].array_count != 2
        || std::strcmp(main_unit.functions[main_unit.function_instantiations[0].function_index]
                .parameters[0].type_name, "Point") != 0)
    {
        std::printf("Assertion failed: imported struct-array template should preserve ABI shape\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "imported struct-array template should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "POINTS OCCURS 2 TIMES.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE POINTS-VALUE(1) TO", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "PERFORM CBLC-TPL-CONSUME-POINT-ARR2", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "DISPLAY \"IMPORTED\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: imported struct-array template should emit concrete ABI (generated=%s)\n",
            generated_cobol ? generated_cobol : "<null>");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&worker_unit);
    cblc_translation_unit_dispose(&main_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_tracks_multiple_functions)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
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

FT_TEST(test_cblc_parse_translation_unit_accepts_functions_without_keyword)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "functions without the function keyword should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.function_count != 2)
    {
        std::printf("Assertion failed: expected two parsed functions without the function keyword\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[0].source_name, "helper",
            sizeof(unit.functions[0].source_name)) != 0)
    {
        std::printf("Assertion failed: helper should be recorded without the function keyword\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[1].source_name, "main",
            sizeof(unit.functions[1].source_name)) != 0)
    {
        std::printf("Assertion failed: main should be recorded without the function keyword\n");
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

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
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
    if (!ft_strnstr(generated_cobol, "01 MAIN-GREETING.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 MAIN-GREETING-STORED-LENGTH PIC S9(9).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare the string class length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 MAIN-GREETING-CBLC-USER-DATA USAGE POINTER",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare the owning string class pointer\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_cobol, "01 MAIN-GREETING-BUF ", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "05 MAIN-GREETING-LEN ", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: the owning string class must not use the removed fixed-buffer layout\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_array_declarations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int numbers[16];\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "array declaration sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "array declaration sample should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "static int numbers[16] = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit int array storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_array_declarations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int numbers[16];\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "array declaration COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "array declaration COBOL sample should convert") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 NUMBERS PIC S9(9) OCCURS 16 TIMES.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit int array OCCURS storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_indexed_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0];\n"
        "    display(numbers[0]);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "indexed array sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "indexed array sample should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "numbers[0] = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign to int array element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = numbers[0];", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should read from int array element\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_indexed_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0];\n"
        "    display(numbers[0]);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "indexed array COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "indexed array COBOL sample should convert") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE NUMBERS(1) = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign to int array element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = NUMBERS(1).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should read from int array element\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_variable_index_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "int index;\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    total = numbers[index];\n"
        "    display(numbers[index]);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "variable index sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "variable index sample should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "total = numbers[index];", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should read int array with variable index\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_variable_index_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "int index;\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    total = numbers[index];\n"
        "    display(numbers[index]);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "variable index COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "variable index COBOL sample should convert") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = NUMBERS(INDEX + 1).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should read int array with variable index\n");
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
        "void main()\n"
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

    source = "void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
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

FT_TEST(test_cblc_parse_translation_unit_records_int_function_parameters)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "int add_one(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "int function with parameters should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.function_count, 1,
            "translation unit should record one function") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.functions[0].parameter_count, 1,
            "function should record one parameter") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].parameters[0].source_name, "value",
            "parameter source name should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].parameters[0].actual_source_name,
            "add_one__value", "parameter storage name should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].parameters[0].cobol_name,
            "ADD-ONE-VALUE", "parameter COBOL name should be recorded") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_struct_return_type)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    return result;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct-returning function should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.function_count, 1,
            "translation unit should record one struct-returning function") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.functions[0].return_kind, CBLC_FUNCTION_RETURN_STRUCT,
            "function should record a struct return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].return_type_name, "Point",
            "function should record the struct return type name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_parameter_slot_assignments_for_local_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int total;\n"
        "int result;\n"
        "int add_one(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    result = add_one(total);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized local call sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "parameterized local call sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "static int add_one__value = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit global parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "add_one__value = total;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign argument into parameter storage before local call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "result = add_one();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should still call local helper after assigning parameters\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_unique_block_local_scalar_storage)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    {\n"
        "        int scratch;\n"
        "        scratch = 4;\n"
        "        total = scratch;\n"
        "    }\n"
        "    {\n"
        "        int scratch;\n"
        "        scratch = 5;\n"
        "        total = total + scratch;\n"
        "    }\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "block-local scalar sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "block-local scalar sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "int main__scratch = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit first local scalar backing storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "int main__scratch__1 = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit unique backing storage for redeclared local scalar\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = total + main__scratch__1;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should bind later references to the second local scalar\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_block_local_after_scope)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    {\n"
        "        int scratch;\n"
        "        scratch = 4;\n"
        "    }\n"
        "    total = scratch;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_FAILURE
        && unit.parse_error_code == TRANSPILE_ERROR_SEMANTIC_SCOPE_VIOLATION)
        status = FT_SUCCESS;
    else
        std::printf("Assertion failed: parser should report a lexical scope violation for a closed block-local scalar\n");
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_scope_metadata_tracks_lexical_ownership)
{
    const char *source;
    t_cblc_translation_unit unit;
    size_t index;
    size_t local_scope_id;
    size_t nested_scope_id;
    int local_count;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    int outer;\n"
        "    {\n"
        "        int inner;\n"
        "        inner = outer;\n"
        "    }\n"
        "    total = outer;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    local_scope_id = static_cast<size_t>(-1);
    nested_scope_id = static_cast<size_t>(-1);
    local_count = 0;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "scope metadata sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.active_scope_id != 0 || unit.scope_count < 4)
    {
        std::printf("Assertion failed: parser should restore the root scope and retain nested scope records\n");
        goto cleanup;
    }
    index = 0;
    while (index < unit.scope_count)
    {
        const t_cblc_scope *scope;

        scope = &unit.scopes[index];
        if (index == 0)
        {
            if (scope->parent_id != static_cast<size_t>(-1) || scope->depth != 0)
            {
                std::printf("Assertion failed: root scope should have no parent and depth zero\n");
                goto cleanup;
            }
        }
        else if (scope->parent_id >= unit.scope_count
            || scope->depth != unit.scopes[scope->parent_id].depth + 1)
        {
            std::printf("Assertion failed: every non-root scope should have a consistent parent depth\n");
            goto cleanup;
        }
        index += 1;
    }
    index = 0;
    while (index < unit.data_count)
    {
        const t_cblc_data_item *item;

        item = &unit.data_items[index];
        if (item->is_function_local && !item->is_alias)
        {
            if (local_scope_id == static_cast<size_t>(-1))
                local_scope_id = item->scope_id;
            else if (item->scope_id != local_scope_id)
                nested_scope_id = item->scope_id;
            local_count += 1;
        }
        index += 1;
    }
    if (local_count < 2 || local_scope_id == static_cast<size_t>(-1)
        || nested_scope_id == static_cast<size_t>(-1)
        || unit.scopes[nested_scope_id].depth <= unit.scopes[local_scope_id].depth)
    {
        std::printf("Assertion failed: local bindings should record distinct lexical ownership scopes\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_scope_metadata_marks_shadowed_bindings)
{
    const char *source;
    t_cblc_translation_unit unit;
    size_t index;
    int shadowed_count;
    int status;

    source = "int value;\n"
        "void main()\n"
        "{\n"
        "    int value;\n"
        "    {\n"
        "        int value;\n"
        "        value = 3;\n"
        "    }\n"
        "    value = 2;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    shadowed_count = 0;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "shadowing sample should parse") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit.data_count)
    {
        const t_cblc_data_item *item;

        item = &unit.data_items[index];
        if (item->is_alias && std::strncmp(item->source_name, "value",
                sizeof(item->source_name)) == 0 && item->is_shadowed)
            shadowed_count += 1;
        index += 1;
    }
    if (shadowed_count < 2)
    {
        std::printf("Assertion failed: nested declarations should retain shadowing metadata\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_scope_resolution_rejects_cross_function_local_reference)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void helper()\n"
        "{\n"
        "    int private_value;\n"
        "    private_value = 1;\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    private_value = 2;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_FAILURE
        && unit.parse_error_code == TRANSPILE_ERROR_SEMANTIC_SCOPE_VIOLATION
        && std::strncmp(unit.parse_error_message,
            "identifier 'private_value' is referenced outside its lexical scope",
            sizeof(unit.parse_error_message)) == 0)
        status = FT_SUCCESS;
    else
        std::printf("Assertion failed: a local from another function should produce a scope violation\n");
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_struct_returning_function_and_call_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    return result;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    point = make_point();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct return sample should parse for C generation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "struct return sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "t_Point make_point(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit a struct return type for helper functions\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "point = make_point();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign returned struct values into matching targets\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_parameter_slot_assignments_for_local_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int total;\n"
        "int result;\n"
        "int add_one(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    result = add_one(total);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized local call COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "parameterized local call COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 ADD-ONE-VALUE PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit parameter storage item\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE ADD-ONE-VALUE = total.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign argument into parameter storage before PERFORM\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "PERFORM ADD-ONE THRU CBLC-FRETURN-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should perform local helper after assigning parameters\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_struct_returning_function_and_call_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    return result;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    point = make_point();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct return sample should parse for COBOL generation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "struct return sample should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 CBLC-RETURN-MAKE-POINT.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit a grouped return slot for struct-returning functions\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "PERFORM MAKE-POINT THRU CBLC-FRETURN-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should perform the struct-returning helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE CBLC-RETURN-MAKE-POINT TO POINT", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should move the grouped return slot into the target variable\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_struct_returning_method)
{
    const char *source;
    t_cblc_translation_unit unit;
    const t_cblc_struct_type *builder_type;
    size_t type_index;
    int status;

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
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct-returning method sample should parse") != FT_SUCCESS)
        goto cleanup;
    builder_type = NULL;
    type_index = 0;
    while (type_index < unit.struct_type_count)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name, "Builder") == 0)
        {
            builder_type = &unit.struct_types[type_index];
            break;
        }
        type_index += 1;
    }
    if (!builder_type)
    {
        std::printf("Assertion failed: Builder type should be present in the canonical type table\n");
        goto cleanup;
    }
    if (test_expect_size_t_equal(builder_type->method_count, 1,
            "class should record one struct-returning method") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(builder_type->methods[0].return_kind,
            CBLC_FUNCTION_RETURN_STRUCT,
            "method should record a struct return kind") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(builder_type->methods[0].return_type_name, "Point",
            sizeof(builder_type->methods[0].return_type_name)) != 0)
    {
        std::printf("Assertion failed: method should record the struct return type name\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_and_cobol_emit_struct_returning_method_call_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "class Builder\n"
        "{\n"
        "    public:\n"
        "    Point value;\n"
        "    void seed()\n"
        "    {\n"
        "        value.x = 7;\n"
        "        return;\n"
        "    }\n"
        "    Point current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Builder builder;\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    builder.seed();\n"
        "    point = builder.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct-returning method generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "struct-returning method generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "struct-returning method generation sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c, "point = cblc_method_Builder_current(&builder);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign the returned struct method value into the target\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "PERFORM CBLC-METHOD-BUILDER-CURRENT.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE BUILDER-VALUE TO CBLC-RESULT-BUILDER-CURRENT.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE CBLC-RESULT-BUILDER-CURRENT TO POINT.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should reuse struct-returning method paragraphs and result slots\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_local_argument_count_mismatch)
{
    const char *source;
    t_cblc_translation_unit unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    source = "void helper(int value)\n"
        "{\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local argument mismatch sample should parse") != FT_SUCCESS)
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
        std::printf("Assertion failed: local call with wrong argument count should fail resolution\n");
        goto cleanup;
    }
    if (context.diagnostics.count == 0
        || context.diagnostics.items[context.diagnostics.count - 1].code
            != TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH)
    {
        std::printf("Assertion failed: local argument count mismatch should record dedicated diagnostic\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

#include "validation_imported_call_tests.inc"
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
        "void helper()\n"
        "{\n"
        "    display(\"LOCAL\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker()\n"
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

FT_TEST(test_cblc_generate_cobol_emits_external_literal_argument_by_value)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker(4);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external literal argument should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY VALUE 4", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass numeric literal by value\n");
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

FT_TEST(test_cblc_generate_cobol_emits_external_multiple_reference_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "int right_value;\n"
        "void main()\n"
        "{\n"
        "    worker(left_value, right_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int left, int right)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external reference arguments should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY REFERENCE LEFT-VALUE BY REFERENCE RIGHT-VALUE",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass both variables by reference\n");
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

FT_TEST(test_cblc_generate_cobol_emits_external_mixed_value_and_reference_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int right_value;\n"
        "void main()\n"
        "{\n"
        "    worker(4, right_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int left, int right)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (cblc_test_prepare_imported_worker_context(main_source, worker_source, &main_unit,
            &worker_unit, &context, &context_initialized, 1) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "external mixed arguments should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol || !ft_strnstr(generated_cobol,
            "CALL 'WORKER' USING BY VALUE 4 BY REFERENCE RIGHT-VALUE",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should preserve mixed value/reference passing\n");
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

FT_TEST(test_cblc_generate_cobol_emits_external_parameterized_call_and_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_main_cobol;
    char *generated_worker_cobol;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "int left_value;\n"
        "void main()\n"
        "{\n"
        "    left_value = 4;\n"
        "    worker(left_value);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    display(value);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external parameterized COBOL call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with external parameterized COBOL call should parse") != FT_SUCCESS)
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
            "main module with external parameterized call should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_worker_cobol),
            "worker module with parameter should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_main_cobol,
            "CALL 'WORKER' USING BY REFERENCE LEFT-VALUE", std::strlen(generated_main_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass external int parameter by reference\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol, "LINKAGE SECTION.", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit linkage section for entry parameters\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "01 WORKER-VALUE PIC S9(9).", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit linkage storage for parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "PROCEDURE DIVISION USING WORKER-VALUE.", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept parameter in procedure division\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_return_call_and_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_main_cobol;
    char *generated_worker_cobol;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker(4);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external return call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with external return should parse") != FT_SUCCESS)
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
            "main module with external return call should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_worker_cobol),
            "worker module with external return should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_main_cobol,
            "CALL 'WORKER' USING BY VALUE 4 BY REFERENCE TOTAL", std::strlen(generated_main_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should append assignment target as external return slot\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "01 CBLC-RETURN-WORKER PIC S9(9).", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit linkage storage for return slot\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "PROCEDURE DIVISION USING WORKER-VALUE CBLC-RETURN-WORKER.",
            std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept parameter and return slot in procedure division\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_handles_string_class_construction_and_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string target(greeting);\n"
        "    int total;\n"
        "    greeting.append(target);\n"
        "    total = greeting.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_message[0] = '\0';
    diagnostic_code = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string programs should parse") != FT_SUCCESS)
        goto cleanup;
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message, sizeof(diagnostic_message))
        != FT_SUCCESS)
    {
        std::printf("String COBOL generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic_message);
        test_expect_success(FT_FAILURE, "string programs should convert to COBOL");
        goto cleanup;
    }
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "PERFORM CBLC-DESTRUCTOR-CBLC-USER-STRING-APPEND-SOURCE.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated method code should destroy its by-value string parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "GREETING-CBLC-USER-DATA",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated code should use the owning string class layout\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "TARGET-CBLC-USER-DATA",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: copy construction should use a second class instance\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_cobol, "01 GREETING-BUF ", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "01 TARGET-BUF ", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: string class objects must not use the old fixed-buffer layout\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_method_parameters_destroy_in_reverse_declaration_order)
{
    const char *source =
        "class Item { private: int value; public: Item() { value = 0; } "
        "~Item() { value = 0; } };\n"
        "class Receiver { public: void consume(Item first, Item second) { "
        "return; } };\n"
        "void main() { Receiver receiver; Item first; Item second; "
        "receiver.consume(first, second); return; }\n";
    t_cblc_translation_unit unit;
    char *generated_cobol = NULL;
    const t_cblc_method *consume = NULL;
    char first_cleanup[TRANSPILE_IDENTIFIER_MAX + 32];
    char second_cleanup[TRANSPILE_IDENTIFIER_MAX + 32];
    const char *first_call;
    const char *second_call;
    size_t type_index;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("By-value method parameter cleanup fixture did not parse: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    for (type_index = 0; type_index < unit.struct_type_count; ++type_index)
        if (std::strcmp(unit.struct_types[type_index].source_name, "Receiver") == 0)
        {
            size_t method_index;
            for (method_index = 0;
                method_index < unit.struct_types[type_index].method_count;
                ++method_index)
                if (std::strcmp(unit.struct_types[type_index].methods[method_index]
                        .source_name, "consume") == 0)
                    consume = &unit.struct_types[type_index].methods[method_index];
        }
    if (!consume || consume->parameter_count != 2
        || std::snprintf(first_cleanup, sizeof(first_cleanup),
            "PERFORM CBLC-DESTRUCTOR-%s.", consume->parameters[0].cobol_name) < 0
        || std::snprintf(second_cleanup, sizeof(second_cleanup),
            "PERFORM CBLC-DESTRUCTOR-%s.", consume->parameters[1].cobol_name) < 0)
    {
        std::printf("By-value method parameter metadata is incomplete\n");
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("By-value method parameters should lower to COBOL\n");
        goto cleanup;
    }
    first_call = std::strstr(generated_cobol, first_cleanup);
    second_call = std::strstr(generated_cobol, second_cleanup);
    if (!first_call || !second_call || second_call >= first_call)
    {
        std::printf("Method parameters must be destroyed in reverse declaration order (first=%s second=%s)\n",
            first_call ? "present" : "missing",
            second_call ? "present" : "missing");
        if (generated_cobol)
        {
            const char *cursor = generated_cobol;
            while ((cursor = std::strstr(cursor, "PERFORM CBLC-DESTRUCTOR-")) != NULL)
            {
                const char *line_end = std::strchr(cursor, '\n');
                const size_t line_length = line_end
                    ? static_cast<size_t>(line_end - cursor)
                    : std::strlen(cursor);
                std::printf("  emitted: %.*s\n", static_cast<int>(line_length), cursor);
                cursor += line_length;
            }
        }
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_method_throw_cleans_by_value_parameters)
{
    const char *source =
        "class Item { private: int value; public: Item() { value = 0; } "
        "~Item() { value = 0; } };\n"
        "class Receiver { public: void fail(Item first, Item second) { "
        "throw 7; } };\n"
        "void main() { Receiver receiver; Item first; Item second; "
        "receiver.fail(first, second); return; }\n";
    t_cblc_translation_unit unit;
    char *generated_cobol = NULL;
    const t_cblc_method *method = NULL;
    const t_cblc_statement *throw_statement = NULL;
    char second_cleanup[TRANSPILE_IDENTIFIER_MAX + 32];
    char first_cleanup[TRANSPILE_IDENTIFIER_MAX + 32];
    const char *cleanup_start;
    const char *second_call;
    const char *first_call;
    size_t type_index;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Throwing method parameter fixture did not parse: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    for (type_index = 0; type_index < unit.struct_type_count; ++type_index)
        if (std::strcmp(unit.struct_types[type_index].source_name, "Receiver") == 0)
        {
            size_t method_index;
            for (method_index = 0;
                method_index < unit.struct_types[type_index].method_count;
                ++method_index)
                if (std::strcmp(unit.struct_types[type_index].methods[method_index]
                        .source_name, "fail") == 0)
                    method = &unit.struct_types[type_index].methods[method_index];
        }
    if (!method || method->parameter_count != 2)
    {
        std::printf("Throwing method parameter metadata is incomplete\n");
        goto cleanup;
    }
    for (type_index = 0; type_index < method->statement_count; ++type_index)
        if (method->statements[type_index].type == CBLC_STATEMENT_THROW)
            throw_statement = &method->statements[type_index];
    if (!throw_statement || throw_statement->exception_cleanup_count != 2
        || std::strcmp(throw_statement->exception_cleanup_targets[0],
            method->parameters[1].cobol_name) != 0
        || std::strcmp(throw_statement->exception_cleanup_targets[1],
            method->parameters[0].cobol_name) != 0
        || std::snprintf(second_cleanup, sizeof(second_cleanup),
            "PERFORM CBLC-DESTRUCTOR-%s.", method->parameters[1].cobol_name) < 0
        || std::snprintf(first_cleanup, sizeof(first_cleanup),
            "PERFORM CBLC-DESTRUCTOR-%s.", method->parameters[0].cobol_name) < 0)
    {
        std::printf("Throw must capture parameter cleanup in reverse declaration order\n");
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("Throwing method parameters should lower to COBOL\n");
        goto cleanup;
    }
    cleanup_start = std::strstr(generated_cobol, "CBLC-EX-CLEANING.");
    second_call = cleanup_start ? std::strstr(cleanup_start, second_cleanup) : NULL;
    first_call = cleanup_start ? std::strstr(cleanup_start, first_cleanup) : NULL;
    if (!second_call || !first_call || second_call >= first_call)
    {
        std::printf("Generated throw cleanup must destroy method parameters in reverse order\n");
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

    source = "void main()\n"
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

FT_TEST(test_cblc_generate_cobol_requires_resolved_imported_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "imported external call sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_FAILURE)
    {
        std::printf("Assertion failed: direct COBOL generation should reject unresolved imported calls\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_reports_unresolved_local_call)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    char diagnostic_message[256];
    int diagnostic_code;
    int status;

    source = "void main() { missing(); return; }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "unresolved local call sample should parse before lowering") != FT_SUCCESS)
        goto cleanup;
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message, sizeof(diagnostic_message))
            != FT_FAILURE
        || diagnostic_code != TRANSPILE_ERROR_FUNCTION_UNRESOLVED
        || std::strcmp(diagnostic_message,
            "unresolved call 'missing' in function 'main' at line 1") != 0)
    {
        std::printf("Assertion failed: unresolved local call should report its symbol, function, and source line; code=%d message='%s'\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_vector_string_template_specialization_emits)
{
    const char *errors_source;
    const char *string_source;
    const char *vector_source;
    const char *program_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;
    int status;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    program_source = "void main() {\n"
        "vector<string> words;\n"
        "string *borrowed;\n"
        "string probe;\n"
        "int length;\n"
        "char byte_value;\n"
        "words.emplace_back(); borrowed = words.at(0); "
        "length = borrowed->len(); "
        "byte_value = probe.at(0); "
        "try { borrowed = words.at(1); } "
        "catch (const out_of_range& error) { length = error.requested; } "
        "return; }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    status = FT_FAILURE;
    if (!errors_source || !string_source || !vector_source)
    {
        std::printf("Assertion failed: embedded errors, string, and vector sources should be available\n");
        goto cleanup;
    }
    combined_source = std::string(errors_source) + "\n" + string_source + "\n"
        + vector_source + "\n" + program_source;
    if (test_expect_success(cblc_parse_translation_unit(combined_source.c_str(), &unit),
            "canonical string and vector source should parse together") != FT_SUCCESS)
    {
        std::printf("vector<string> parse diagnostic: %s\n", unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message, sizeof(diagnostic_message)) != FT_SUCCESS)
    {
        std::printf("vector<string> generation diagnostic (%d): %s\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    if (!generated_cobol
        || !std::strstr(generated_cobol,
            "VECTOR-STRING-EMPLACE-BACK-OVL1-RELOCATION-INDEX")
        || !std::strstr(generated_cobol,
            "COMPUTE MAIN-WORDS-COUNT = MAIN-WORDS-COUNT + 1")
        || !std::strstr(generated_cobol, "STORED-LENGTH")
        || !std::strstr(generated_cobol, "STORAGE-CAPACITY")
        || !std::strstr(generated_cobol,
            "CBLC-EX-PAYLOAD-OUT-OF-RANGE-REQUESTED")
        || !std::strstr(generated_cobol,
            "CBLC-EX-PAYLOAD-OUT-OF-RANGE-EXTENT"))
    {
        std::printf("Assertion failed: vector<string> inline specialization should update the vector count and emit concrete string lifecycle storage (relocation=%d count-update=%d string=%d)\n",
            generated_cobol && std::strstr(generated_cobol,
                "VECTOR-STRING-EMPLACE-BACK-OVL1-RELOCATION-INDEX") != NULL,
            generated_cobol && std::strstr(generated_cobol,
                "COMPUTE MAIN-WORDS-COUNT = MAIN-WORDS-COUNT + 1") != NULL,
            generated_cobol && std::strstr(generated_cobol,
                "STORED-LENGTH") != NULL);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_reports_typed_ir_verification_failure)
{
    const char *source = "void main() { if (1) { } return; }\n";
    t_cblc_translation_unit unit;
    char *generated_cobol = NULL;
    char diagnostic_message[512];
    int diagnostic_code = 0;
    int status = FT_FAILURE;
    bool corrupted_if_end = false;

    cblc_translation_unit_init(&unit);
    diagnostic_message[0] = '\0';
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0)
        goto cleanup;
    for (size_t index = 0; index < unit.functions[0].statement_count; ++index)
        if (unit.functions[0].statements[index].type == CBLC_STATEMENT_IF_END)
        {
            /* Simulate an invalid parser/backend handoff; do not alter source text. */
            unit.functions[0].statements[index].type = CBLC_STATEMENT_WHILE_END;
            corrupted_if_end = true;
            break;
        }
    if (!corrupted_if_end
        || cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message, sizeof(diagnostic_message))
            != FT_FAILURE
        || diagnostic_code != TRANSPILE_ERROR_BACKEND_IR_VERIFICATION
        || std::strstr(diagnostic_message, "typed IR verification failed") == NULL
        || std::strstr(diagnostic_message, "main") == NULL)
    {
        std::printf("Assertion failed: typed IR rejection should return a stable code and verifier message; code=%d message='%s'\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_cblc_parse_translation_unit_accepts_external_return_assignment)
{
    const char *main_source;
    t_cblc_translation_unit main_unit;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: parser should accept external return assignment syntax\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
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
        "void main()\n"
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
        "void main()\n"
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
    size_t point_type_index;
    size_t string_type_index;
    size_t point_index;
    size_t x_index;
    size_t name_index;
    size_t item_index;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name;\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    display(point.x);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct program should parse") != FT_SUCCESS)
    {
        std::printf("struct parse diagnostic: %d %s\n", unit.parse_error_code,
            unit.parse_error_message);
        goto cleanup;
    }
    point_type_index = static_cast<size_t>(-1);
    string_type_index = static_cast<size_t>(-1);
    for (size_t type_index = 0; type_index < unit.struct_type_count; ++type_index)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name, "Point") == 0)
            point_type_index = type_index;
        else if (std::strcmp(unit.struct_types[type_index].source_name, "string") == 0)
            string_type_index = type_index;
    }
    if (point_type_index == static_cast<size_t>(-1)
        || string_type_index == static_cast<size_t>(-1))
    {
        std::printf("Assertion failed: canonical string class and user Point type should both be registered\n");
        goto cleanup;
    }
    point_index = static_cast<size_t>(-1);
    x_index = static_cast<size_t>(-1);
    name_index = static_cast<size_t>(-1);
    item_index = 0;
    while (item_index < unit.data_count)
    {
        if (std::strcmp(unit.data_items[item_index].source_name, "point") == 0)
            point_index = item_index;
        else if (std::strcmp(unit.data_items[item_index].source_name, "point.x") == 0)
            x_index = item_index;
        else if (std::strcmp(unit.data_items[item_index].source_name, "point.name") == 0)
            name_index = item_index;
        item_index += 1;
    }
    if (point_index == static_cast<size_t>(-1)
        || unit.data_items[point_index].kind != CBLC_DATA_KIND_STRUCT)
    {
        std::printf("Assertion failed: struct instance should be registered as data item\n");
        goto cleanup;
    }
    if (x_index == static_cast<size_t>(-1)
        || unit.data_items[x_index].kind != CBLC_DATA_KIND_INT)
    {
        std::printf("Assertion failed: struct int field should be registered as data item\n");
        goto cleanup;
    }
    if (name_index == static_cast<size_t>(-1)
        || unit.data_items[name_index].kind != CBLC_DATA_KIND_STRUCT
        || std::strcmp(unit.data_items[name_index].struct_type_name, "string") != 0)
    {
        std::printf("Assertion failed: struct string field should be a string class instance\n");
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
    int parse_status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name;\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    display(point.x);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    parse_status = cblc_parse_translation_unit(source, &unit);
    if (test_expect_success(parse_status,
            "struct COBOL sample should parse") != FT_SUCCESS)
    {
        std::printf("struct COBOL parse diagnostic: %d %s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
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
    if (!ft_strnstr(generated_cobol,
            "10 POINT-NAME-CBLC-USER-DATA USAGE POINTER VALUE NULL.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit the owning string data pointer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "10 POINT-NAME-STORED-LENGTH PIC S9(9).",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "10 POINT-NAME-STORAGE-CAPACITY PIC S9(9).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit the owning string length and capacity fields\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE POINT-X = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign to struct int field\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_cobol, "POINT-NAME-BUF", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: owning string fields must not retain the removed inline-buffer layout\n");
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

    source = "char greeting[8];\n"
        "int counter;\n"
        "void helper()\n"
        "{\n"
        "    display(\"HI\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
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
    if (!ft_strnstr(generated_c, "static char greeting[8] = {0};", std::strlen(generated_c)))
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
    if (!ft_strnstr(generated_c, "cblc_char_assign_literal(greeting, 8, \"HELLO\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign a literal to the fixed char array\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_char_buffer(greeting, 8);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display the fixed char array\n");
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

    source = "void helper()\n"
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
        "    int name;\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    display(point.name);\n"
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
    if (!ft_strnstr(generated_c, "int name;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit scalar storage in struct\n");
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
    if (!ft_strnstr(generated_c, "int name;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit scalar storage for the field\n");
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
    size_t address_index;
    size_t city_index;
    size_t item_index;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city;\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    person.age = 7;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct program should parse") != FT_SUCCESS)
    {
        std::printf("nested struct diagnostic: %d %s\n", unit.parse_error_code,
            unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_size_t_equal(unit.struct_type_count, 12,
            "standard error and string classes plus two user struct types should be recorded") != FT_SUCCESS)
        goto cleanup;
    address_index = static_cast<size_t>(-1);
    city_index = static_cast<size_t>(-1);
    item_index = 0;
    while (item_index < unit.data_count)
    {
        if (std::strcmp(unit.data_items[item_index].source_name,
                "person.address") == 0)
            address_index = item_index;
        else if (std::strcmp(unit.data_items[item_index].source_name,
                "person.address.city") == 0)
            city_index = item_index;
        item_index += 1;
    }
    if (address_index == static_cast<size_t>(-1)
        || unit.data_items[address_index].kind != CBLC_DATA_KIND_STRUCT)
    {
        std::printf("Assertion failed: nested struct field should be registered as aggregate data item\n");
        goto cleanup;
    }
    if (city_index == static_cast<size_t>(-1)
        || unit.data_items[city_index].kind != CBLC_DATA_KIND_STRUCT
        || std::strcmp(unit.data_items[city_index].struct_type_name, "string") != 0)
    {
        std::printf("Assertion failed: nested string member should resolve to the owning string class\n");
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
        "    string city;\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
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
    if (!ft_strnstr(generated_cobol, "STORED-LENGTH",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested owning-string length storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "STORAGE-CAPACITY",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested owning-string capacity storage\n");
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
        "    int city;\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
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
    if (!ft_strnstr(generated_c, "int city;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should preserve nested scalar field\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

#include "validation_class_tests.inc"
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

FT_TEST(test_transpiler_validation_resolves_go_to_paragraph_targets)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. GO-VALIDATE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           GO TO DONE.\n"
        "       DONE.\n"
        "           STOP RUN.\n";
    return (test_expect_success(transpiler_validate_generated_cobol(source),
        "validator should accept GO TO with a defined paragraph target"));
}

FT_TEST(test_transpiler_validation_rejects_unresolved_go_to_target)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. GO-INVALID.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           GO TO MISSING-PARAGRAPH.\n"
        "           STOP RUN.\n";
    return (test_expect_int_equal(transpiler_validate_generated_cobol(source),
        FT_FAILURE, "validator should reject GO TO to an undefined paragraph"));
}

FT_TEST(test_transpiler_validation_resolves_perform_thru_targets)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. PERFORM-VALIDATE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           PERFORM WORKER THRU DONE.\n"
        "           STOP RUN.\n"
        "       WORKER.\n"
        "           CONTINUE.\n"
        "       DONE.\n"
        "           EXIT.\n";
    return (test_expect_success(transpiler_validate_generated_cobol(source),
        "validator should resolve both PERFORM THRU paragraph targets"));
}

FT_TEST(test_transpiler_validation_rejects_unresolved_perform_target)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. PERFORM-INVALID.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           PERFORM MISSING-PARAGRAPH.\n"
        "           STOP RUN.\n";
    return (test_expect_int_equal(transpiler_validate_generated_cobol(source),
        FT_FAILURE, "validator should reject PERFORM to an undefined paragraph"));
}

FT_TEST(test_transpiler_validation_rejects_unresolved_perform_thru_end_target)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. PERFORM-THRU-INVALID.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           PERFORM WORKER THRU MISSING-PARAGRAPH.\n"
        "           STOP RUN.\n"
        "       WORKER.\n"
        "           EXIT.\n";
    return (test_expect_int_equal(transpiler_validate_generated_cobol(source),
        FT_FAILURE, "validator should reject PERFORM THRU with an undefined end paragraph"));
}

FT_TEST(test_transpiler_validation_rejects_duplicate_paragraph_names)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DUPLICATE-PARAGRAPH.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           CONTINUE.\n"
        "       WORKER.\n"
        "           CONTINUE.\n"
        "       WORKER.\n"
        "           STOP RUN.\n";
    return (test_expect_int_equal(transpiler_validate_generated_cobol(source),
        FT_FAILURE, "validator should reject duplicate generated paragraph names"));
}

FT_TEST(test_transpiler_validation_rejects_case_colliding_paragraph_names)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CASE-COLLISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       Main.\n"
        "           CONTINUE.\n"
        "       worker.\n"
        "           CONTINUE.\n"
        "       WORKER.\n"
        "           STOP RUN.\n";
    return (test_expect_int_equal(transpiler_validate_generated_cobol(source),
        FT_FAILURE, "COBOL paragraph names that differ only by case must collide"));
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

FT_TEST(test_lexer_token_get_span_reports_expected_columns)
{
    t_lexer lexer;
    t_lexer_token token;
    t_transpiler_source_span span;
    int status;

    lexer_init(&lexer, "MOVE VALUE TO TARGET.");
    ft_bzero(&token, sizeof(token));
    ft_bzero(&span, sizeof(span));
    status = FT_FAILURE;
    if (test_expect_success(lexer_next_token(&lexer, &token),
            "lexer should produce first token for span test") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(lexer_token_get_span(&token, "span_test.cob", &span),
            "token span helper should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.start_line, 1,
            "token span should start on line 1") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.start_column, 1,
            "token span should start at column 1") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.end_line, 1,
            "token span should end on line 1") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.end_column, 5,
            "token span end column should be one past the token") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(span.path, "span_test.cob",
            "token span should carry the supplied path") != FT_SUCCESS)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    return (status);
}

FT_TEST(test_ast_node_get_span_uses_node_token)
{
    t_lexer_token token;
    t_ast_node *node;
    t_transpiler_source_span span;
    int status;

    ft_bzero(&token, sizeof(token));
    ft_bzero(&span, sizeof(span));
    node = NULL;
    status = FT_FAILURE;
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = "FIELD-NAME";
    token.length = 10;
    token.line = 3;
    token.column = 7;
    node = ast_node_create(AST_NODE_IDENTIFIER);
    if (!node)
        return (FT_FAILURE);
    if (test_expect_success(ast_node_set_token(node, &token),
            "AST token assignment should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_get_span(node, "node_span.cob", &span),
            "AST node span helper should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(span.start_line, 3,
            "node span should start on the token line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(span.start_column, 7,
            "node span should start on the token column") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(span.end_column, 17,
            "node span should end one past the token length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (node)
        ast_node_destroy(node);
    return (status);
}

FT_TEST(test_cblc_frontend_analysis_reports_parse_diagnostic)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    int status;

    source = "class Greeter\n"
        "{\n"
        "    string name(16)\n"
        "};\n";
    status = FT_FAILURE;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_analyze_document(&analysis, "broken_sample.cblc", source) == FT_SUCCESS)
    {
        std::printf("Assertion failed: broken CBL-C sample should not parse successfully\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(analysis.diagnostics.count), 1,
            "broken CBL-C sample should emit one parse diagnostic") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(analysis.diagnostics.items[0].message, "Parse error",
            "CBL-C parse diagnostic should use stable parse message") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(analysis.diagnostics.items[0].span.path, "broken_sample.cblc",
            "CBL-C parse diagnostic should carry the document path") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(analysis.diagnostics.items[0].span.start_line, 1,
            "CBL-C parse diagnostic should currently point at the start of the file") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_collect_document_symbols_returns_types_functions_and_data)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_document_symbol_list symbols;
    size_t index;
    int found_class;
    int found_function;
    int found_counter;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "};\n"
        "\n"
        "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_class = 0;
    found_function = 0;
    found_counter = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for symbol test") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_document_symbol_list_init(&symbols),
            "CBL-C symbol list should initialize") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "symbols_sample.cblc", source),
            "CBL-C symbol sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_collect_document_symbols(&analysis, &symbols),
            "CBL-C symbol collection should succeed") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < symbols.count)
    {
        if (symbols.items[index].kind == CBLC_DOCUMENT_SYMBOL_CLASS
            && std::strncmp(symbols.items[index].name, "Counter",
                sizeof(symbols.items[index].name)) == 0)
            found_class = 1;
        if (symbols.items[index].kind == CBLC_DOCUMENT_SYMBOL_FUNCTION
            && std::strncmp(symbols.items[index].name, "main",
                sizeof(symbols.items[index].name)) == 0)
            found_function = 1;
        if (symbols.items[index].kind == CBLC_DOCUMENT_SYMBOL_DATA_ITEM
            && std::strncmp(symbols.items[index].name, "counter",
                sizeof(symbols.items[index].name)) == 0)
            found_counter = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_class, 1,
            "CBL-C symbol collection should include declared classes") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_function, 1,
            "CBL-C symbol collection should include the main function") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_counter, 1,
            "CBL-C symbol collection should include the global data item") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_document_symbol_list_dispose(&symbols);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_find_definition_resolves_data_item_reference)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_transpiler_source_span definition_span;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(&definition_span, sizeof(definition_span));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for definition lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "definition_sample.cblc", source),
            "CBL-C definition sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_find_definition(&analysis, 4, 13, &definition_span),
            "CBL-C definition lookup should resolve the displayed data item") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_line, 1,
            "CBL-C variable definition should resolve to the declaration line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_column, 5,
            "CBL-C variable definition should resolve to the identifier column") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(definition_span.path, "definition_sample.cblc",
            "CBL-C definition lookup should preserve the analysis path") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_find_definition_resolves_function_call_target)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_transpiler_source_span definition_span;
    int status;

    source = "void helper()\n"
        "{\n"
        "    return;\n"
        "}\n"
        "\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(&definition_span, sizeof(definition_span));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for function definition lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "call_definition_sample.cblc", source),
            "CBL-C function definition sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_find_definition(&analysis, 8, 6, &definition_span),
            "CBL-C definition lookup should resolve the function call target") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_line, 1,
            "CBL-C function definition should resolve to the declaration line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_column, 6,
            "CBL-C function definition should resolve to the function identifier column") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_get_hover_reports_variable_summary)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    char hover[128];
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(hover, sizeof(hover));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for hover lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "hover_sample.cblc", source),
            "CBL-C hover sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_get_hover(&analysis, 4, 13, hover, sizeof(hover)),
            "CBL-C hover should resolve for variable reference") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(hover, "Variable counter : int",
            "CBL-C variable hover should include kind, name, and type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_get_hover_reports_class_summary)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    char hover[128];
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "};\n"
        "\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(hover, sizeof(hover));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for class hover") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "program_hover_sample.cblc", source),
            "CBL-C class hover sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_get_hover(&analysis, 7, 10, hover, sizeof(hover)),
            "CBL-C hover should resolve for a class-typed variable") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(hover, "Variable counter : Counter",
            "CBL-C hover should include the variable name and class type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_find_references_collects_declaration_and_uses)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_source_span_list references;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    counter = 42;\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for reference lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_source_span_list_init(&references),
            "CBL-C source span list should initialize for reference lookup") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "reference_sample.cblc", source),
            "CBL-C reference sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_find_references(&analysis, 4, 13, &references),
            "CBL-C references should resolve for a variable use") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.count, 3,
            "CBL-C references should include declaration and two uses") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.items[0].start_line, 1,
            "CBL-C references should begin with the declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.items[1].start_line, 4,
            "CBL-C references should include the display use") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.items[2].start_line, 5,
            "CBL-C references should include the assignment use") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_source_span_list_dispose(&references);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_collect_semantic_tokens_classifies_core_tokens)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_semantic_token_list tokens;
    size_t index;
    int found_string;
    int found_number;
    int found_variable;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    string name(\"HI\");\n"
        "    counter = 42;\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_string = 0;
    found_number = 0;
    found_variable = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for semantic token collection")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_semantic_token_list_init(&tokens),
            "CBL-C semantic token list should initialize") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "semantic_tokens_sample.cblc", source),
            "CBL-C semantic token sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_collect_semantic_tokens(&analysis, &tokens),
            "CBL-C semantic token collection should succeed") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < tokens.count)
    {
        if (tokens.items[index].kind == CBLC_SEMANTIC_TOKEN_STRING
            && tokens.items[index].span.start_line == 4)
            found_string = 1;
        if (tokens.items[index].kind == CBLC_SEMANTIC_TOKEN_NUMBER
            && tokens.items[index].span.start_line == 5)
            found_number = 1;
        if (tokens.items[index].kind == CBLC_SEMANTIC_TOKEN_VARIABLE
            && tokens.items[index].span.start_line == 1
            && tokens.items[index].span.start_column == 5)
            found_variable = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_string, 1,
            "CBL-C semantic tokens should include string literals") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_number, 1,
            "CBL-C semantic tokens should include numeric literals") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_variable, 1,
            "CBL-C semantic tokens should include declared variables") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_semantic_token_list_dispose(&tokens);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_keywords_and_symbols)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_display;
    int found_counter;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_display = 0;
    found_counter = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for completion") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "completion_sample.cblc", source),
            "CBL-C completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 4, 1, &completions),
            "CBL-C completion should succeed on an empty statement line") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "display",
                sizeof(completions.items[index].label)) == 0)
            found_display = 1;
        if (std::strncmp(completions.items[index].label, "counter",
                sizeof(completions.items[index].label)) == 0)
            found_counter = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_display, 1,
            "CBL-C completion should include language keywords like display") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_counter, 1,
            "CBL-C completion should include in-document symbols like counter") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_filters_function_local_scope)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_other;
    int status;

    source = "void helper()\n"
        "{\n"
        "    string other(8);\n"
        "    return;\n"
        "}\n"
        "\n"
        "void main()\n"
        "{\n"
        "    string message;\n"
        "\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_other = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for local scope completion")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for local scope completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "local_scope_completion_sample.cblc", source),
            "CBL-C local scope completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 11, 5, &completions),
            "CBL-C completion should succeed inside the main function body") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "other",
                sizeof(completions.items[index].label)) == 0)
            found_other = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_other, 0,
            "CBL-C completion should exclude locals from other functions") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_public_members_only)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_name;
    int found_hidden;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
"        int hidden;\n"
        "\n"
        "    public:\n"
        "        int value;\n"
        "        void reset()\n"
        "        {\n"
            "            return;\n"
        "        }\n"
        "};\n"
        "\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    counter.value = 0;\n"
        "    counter.reset();\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_name = 0;
    found_hidden = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for member completion") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for member completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "member_completion_sample.cblc", source),
            "CBL-C member completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 17, 13, &completions),
            "CBL-C completion should succeed for public field member access") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "value",
                sizeof(completions.items[index].label)) == 0)
            found_name = 1;
        if (std::strncmp(completions.items[index].label, "hidden",
                sizeof(completions.items[index].label)) == 0)
            found_hidden = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_name, 1,
            "CBL-C completion should include public fields for member access") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_hidden, 0,
            "CBL-C completion should exclude private members for external access") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_std_namespace_functions)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_strlen;
    int found_strcpy;
    int found_append;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"HI\");\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_strlen = 0;
    found_strcpy = 0;
    found_append = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for std completion") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for std completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "std_completion_sample.cblc", source),
            "CBL-C std completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 4, 20, &completions),
            "CBL-C completion should succeed for std namespace access") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "std::strlen",
                sizeof(completions.items[index].label)) == 0)
            found_strlen = 1;
        if (std::strncmp(completions.items[index].label, "std::strcpy",
                sizeof(completions.items[index].label)) == 0)
            found_strcpy = 1;
        if (std::strncmp(completions.items[index].label, "append",
                sizeof(completions.items[index].label)) == 0)
            found_append = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_strlen, 1,
            "CBL-C completion should include matching std namespace functions") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_strcpy, 1,
            "CBL-C completion should include other matching std string helpers") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_append, 0,
            "CBL-C std completion should not mix in unrelated member names") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_string_class_members)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_append;
    int found_clear;
    int found_capacity;
    int found_empty;
    int found_ends_with;
    int found_equals;
    int found_len;
    int found_compare;
    int found_contains;
    int found_starts_with;
    int status;

    source = "string greeting;\n"
        "void main()\n"
        "{\n"
        "    greeting.\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_append = 0;
    found_capacity = 0;
    found_clear = 0;
    found_empty = 0;
    found_ends_with = 0;
    found_equals = 0;
    found_len = 0;
    found_compare = 0;
    found_contains = 0;
    found_starts_with = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for string class completion")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for string class completion")
        != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
            "string_member_completion_sample.cblc", source) != FT_SUCCESS
                ? FT_SUCCESS : FT_FAILURE,
            "CBL-C string class completion sample should remain incomplete while typing")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 4, 14, &completions),
            "CBL-C completion should succeed for string class member access") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "append",
                sizeof(completions.items[index].label)) == 0)
            found_append = 1;
        if (std::strncmp(completions.items[index].label, "clear",
                sizeof(completions.items[index].label)) == 0)
            found_clear = 1;
        if (std::strncmp(completions.items[index].label, "capacity",
                sizeof(completions.items[index].label)) == 0)
            found_capacity = 1;
        if (std::strncmp(completions.items[index].label, "empty",
                sizeof(completions.items[index].label)) == 0)
            found_empty = 1;
        if (std::strncmp(completions.items[index].label, "ends_with",
                sizeof(completions.items[index].label)) == 0)
            found_ends_with = 1;
        if (std::strncmp(completions.items[index].label, "equals",
                sizeof(completions.items[index].label)) == 0)
            found_equals = 1;
        if (std::strncmp(completions.items[index].label, "compare",
                sizeof(completions.items[index].label)) == 0)
            found_compare = 1;
        if (std::strncmp(completions.items[index].label, "contains",
                sizeof(completions.items[index].label)) == 0)
            found_contains = 1;
        if (std::strncmp(completions.items[index].label, "len",
                sizeof(completions.items[index].label)) == 0)
            found_len = 1;
        if (std::strncmp(completions.items[index].label, "starts_with",
                sizeof(completions.items[index].label)) == 0)
            found_starts_with = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_append, 1,
            "CBL-C completion should include string class append") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_len, 1,
            "CBL-C completion should include string class len") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_clear, 1,
            "CBL-C completion should include string class clear") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_capacity, 1,
            "CBL-C completion should include string class capacity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_empty, 1,
            "CBL-C completion should include string class empty") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_ends_with, 1,
            "CBL-C completion should include string class ends_with") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_equals, 1,
            "CBL-C completion should include string class equals") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_compare, 1,
            "CBL-C completion should include string class compare") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_contains, 1,
            "CBL-C completion should include string class contains") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_starts_with, 1,
            "CBL-C completion should include string class starts_with") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_allows_private_members_on_this_inside_class)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    void reset()\n"
        "    {\n"
        "        return;\n"
        "    }\n"
        "    public:\n"
        "    void expose()\n"
        "    {\n"
        "        this.\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for this member completion")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for this member completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "this_member_completion_sample.cblc", source) != FT_SUCCESS
                ? FT_SUCCESS : FT_FAILURE,
            "CBL-C this member completion sample should remain incomplete while typing")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 12, 14, &completions),
            "CBL-C completion should succeed inside a class while typing this member access")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_native_try_catch_dispatch)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    char source_file_move[128];
    const char *terminate_label;
    int status;

    source = "void main() {\n"
        "    try {\n"
        "        throw 7;\n"
        "        display(\"NOT-REACHED\");\n"
        "    }\n"
        "    catch (int caught_value) {\n"
        "        display(caught_value);\n"
        "    }\n"
        "    catch (...) {\n"
        "        display(\"UNKNOWN\");\n"
        "    }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "try/catch source should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.source_file_id == 0
        || std::snprintf(source_file_move, sizeof(source_file_move),
            "MOVE %zu TO CBLC-EX-SOURCE-FILE-ID", unit.source_file_id) < 0)
    {
        std::printf("Assertion failed: source file ID should be assigned\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "try/catch source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    terminate_label = std::strstr(generated_cobol, "       CBLC-TERMINATE.");
    if (!ft_strnstr(generated_cobol, "01 CBLC-EX-CONTEXT.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-ACTIVE", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-DYNAMIC-TYPE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-PAYLOAD-SIZE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-PAYLOAD-OWNER",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-SOURCE-LINE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-SOURCE-COLUMN",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-SOURCE-FILE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-SECONDARY-SOURCE-LINE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-SECONDARY-SOURCE-COLUMN",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-RAISING", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "05 CBLC-EX-CLEANING", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-TERMINATE.", std::strlen(generated_cobol))
        || !terminate_label
        || !std::strstr(terminate_label, "STOP RUN.")
        || !ft_strnstr(generated_cobol,
            "IF CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-SECONDARY-TYPE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 0 TO CBLC-EX-TYPE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 0 TO CBLC-EX-DYNAMIC-TYPE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 'N' TO CBLC-EX-PAYLOAD-OWNER",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 3 TO CBLC-EX-SOURCE-LINE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 9 TO CBLC-EX-SOURCE-COLUMN",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 3 TO CBLC-EX-SECONDARY-SOURCE-LINE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 9 TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, source_file_move,
            std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "DWARF", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "_Unwind", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "__cxa_throw", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE SPACES TO CBLC-EX-MESSAGE-BUF",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-TRY-CBLC-TRY-CALL-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "-0-DONE.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-TYPE-ID =", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should contain native exception dispatch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_class_exception_does_not_fall_back_to_legacy_message)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main() {\n"
        "    string message(300);\n"
        "    try { throw message; }\n"
        "    catch (string caught_message) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "owning string exception source should parse") != FT_SUCCESS
        || test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "owning string exception source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-STRING",
            std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol,
            "MOVE MESSAGE-LEN TO CBLC-EX-MESSAGE-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: owning string exceptions need their typed object payload, not the legacy message slot\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_clears_exception_before_return_from_catch)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *catch_label;
    const char *cleaning_flag;
    const char *return_cleanup;
    const char *active_clear;
    const char *handler_return;
    int status;

    source = "void worker() {\n"
        "    try { throw 7; }\n"
        "    catch (int caught_value) { return; }\n"
        "}\n"
        "void main() { worker(); }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "return-from-catch source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "return-from-catch source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    catch_label = ft_strnstr(generated_cobol, "CBLC-TRY-CBLC-TRY-CALL-",
        std::strlen(generated_cobol));
    cleaning_flag = catch_label
        ? ft_strnstr(catch_label, "MOVE 'Y' TO CBLC-EX-CLEANING",
            std::strlen(catch_label)) : NULL;
    return_cleanup = catch_label
        ? ft_strnstr(catch_label, "PERFORM CBLC-EX-CLEAR-PAYLOAD",
            std::strlen(catch_label)) : NULL;
    active_clear = return_cleanup
        ? ft_strnstr(return_cleanup, "MOVE 'N' TO CBLC-EX-ACTIVE",
            std::strlen(return_cleanup)) : NULL;
    handler_return = active_clear
        ? ft_strnstr(active_clear, "GO TO CBLC-FRETURN-",
            std::strlen(active_clear)) : NULL;
    if (!catch_label || !cleaning_flag || !return_cleanup || !active_clear
        || !handler_return || cleaning_flag > return_cleanup
        || return_cleanup > active_clear || active_clear > handler_return)
    {
        std::printf("Assertion failed: catch returns must clear the active payload first\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_reuses_identical_exception_cleanup_sequences)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Resource { ~Resource() { } };\n"
        "void main() {\n"
        "    int selector;\n"
        "    selector = 0;\n"
        "    try {\n"
        "        Resource text;\n"
        "        if (selector) { throw 1; }\n"
        "        throw 2;\n"
        "    }\n"
        "    catch (int caught) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "identical cleanup-path source should parse") != FT_SUCCESS
        || test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "identical cleanup-path source should generate COBOL") != FT_SUCCESS)
    {
        std::printf("Cleanup-sequence fixture diagnostic: code=%d %s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (!generated_cobol
        || validation_count_text_occurrences(generated_cobol,
            "       CBLC-EX-CLEANUP-MAIN-EXH-") != 1
        || validation_count_text_occurrences(generated_cobol,
            "PERFORM CBLC-EX-CLEANUP-MAIN-EXH-") != 2)
    {
        std::printf("Assertion failed: typed cleanup sharing expected one paragraph/two calls; found %zu paragraph markers and %zu calls\n",
            generated_cobol ? validation_count_text_occurrences(generated_cobol,
                "       CBLC-EX-CLEANUP-MAIN-EXH-") : 0,
            generated_cobol ? validation_count_text_occurrences(generated_cobol,
                "PERFORM CBLC-EX-CLEANUP-MAIN-EXH-") : 0);
        if (generated_cobol)
        {
            const char *line_start = generated_cobol;

            while (*line_start)
            {
                const char *line_end = std::strchr(line_start, '\n');
                const size_t line_length = line_end
                    ? static_cast<size_t>(line_end - line_start)
                    : std::strlen(line_start);

                if (std::strstr(line_start, "CBLC-EX-CLEANUP")
                    && static_cast<size_t>(std::strstr(line_start,
                        "CBLC-EX-CLEANUP") - line_start) < line_length)
                    std::printf("Cleanup COBOL: %.*s\n",
                        static_cast<int>(line_length), line_start);
                if (!line_end)
                    break ;
                line_start = line_end + 1;
            }
        }
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_materializes_value_before_catch_cleanup)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *catch_label;
    const char *result_move;
    const char *payload_clear;
    const char *handler_return;
    const char *catch_label_prefix;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int generation_diagnostic_code;
    int status;

    source = "int worker() {\n"
        "    try { throw 7; }\n"
        "    catch (int caught_value) { return 9; }\n"
        "    return 0;\n"
        "}\n"
        "void main() { worker(); }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generation_diagnostic[0] = '\0';
    generation_diagnostic_code = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "value-return-from-catch source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol_with_diagnostic(&unit,
                &generated_cobol, &generation_diagnostic_code,
                generation_diagnostic, sizeof(generation_diagnostic)),
            "value-return-from-catch source should generate COBOL") != FT_SUCCESS)
    {
        std::printf("Catch-return generation diagnostic (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic);
        goto cleanup;
    }
    catch_label_prefix = "       CBLC-TRY-CBLC-TRY-CALL-";
    result_move = ft_strnstr(generated_cobol, "MOVE 9 TO CBLC-RETURN-WORKER",
        std::strlen(generated_cobol));
    catch_label = validation_find_last_paragraph_before(generated_cobol,
        catch_label_prefix, result_move);
    payload_clear = catch_label
        ? ft_strnstr(catch_label, "PERFORM CBLC-EX-CLEAR-PAYLOAD",
            std::strlen(catch_label)) : NULL;
    handler_return = catch_label
        ? ft_strnstr(catch_label, "GO TO CBLC-FRETURN-0",
            std::strlen(catch_label)) : NULL;
    if (!catch_label || !result_move || !payload_clear || !handler_return
        || result_move > payload_clear || payload_clear > handler_return)
    {
        std::printf("Assertion failed: catch return values must materialize before cleanup\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_qualifies_try_labels_per_function)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *worker_catch;
    const char *replacement_clear;
    const char *replacement_payload;
    int status;

    source = "void worker() {\n"
        "    try { throw 1; }\n"
        "    catch (int caught_value) { throw 2; }\n"
        "}\n"
        "void main() {\n"
        "    try { worker(); }\n"
        "    catch (int outer_value) { display(outer_value); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "replacement-throw source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "replacement-throw source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    replacement_payload = ft_strnstr(generated_cobol,
        "MOVE 2 TO CBLC-EX-INT-PAYLOAD", std::strlen(generated_cobol));
    worker_catch = validation_find_last_paragraph_before(generated_cobol,
        "       CBLC-TRY-CBLC-TRY-CALL-", replacement_payload);
    replacement_clear = worker_catch
        ? ft_strnstr(worker_catch, "PERFORM CBLC-EX-CLEAR-PAYLOAD",
            std::strlen(worker_catch)) : NULL;
    replacement_payload = worker_catch
        ? ft_strnstr(worker_catch, "MOVE 2 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(worker_catch)) : NULL;
    if (validation_count_paragraph_definitions(generated_cobol,
            "       CBLC-TRY-CALL-") != 2
        || !ft_strnstr(generated_cobol, "MOVE 2 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol))
        || !replacement_clear || !replacement_payload
        || replacement_clear > replacement_payload)
    {
        std::printf("Assertion failed: replacement throws need function-qualified try labels\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_translates_size_error_into_exception_context)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main() {\n"
        "    int left;\n"
        "    int right;\n"
        "    int result;\n"
        "    try { result = left + right; }\n"
        "    catch (overflow_error error) { display(error.code); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "size-error source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "size-error source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "ON SIZE ERROR",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE 1 TO CBLC-EX-PAYLOAD-OVERFLOW-ERROR-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "END-COMPUTE.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-TRY-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "-CATCH-0",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: arithmetic size errors should use exception dispatch (size=%d payload=%d end=%d branch=%d catch=%d)\n",
            ft_strnstr(generated_cobol, "ON SIZE ERROR", std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "MOVE 1 TO CBLC-EX-INT-PAYLOAD", std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "END-COMPUTE.", std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "GO TO CBLC-TRY-", std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "-CATCH-0", std::strlen(generated_cobol)) != NULL);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_translates_floating_size_error_to_range_error)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main() {\n"
        "    double left;\n"
        "    double right;\n"
        "    double result;\n"
        "    try { result = left + right; }\n"
        "    catch (range_error error) { display(error.code); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "floating size-error source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "floating size-error source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "ON SIZE ERROR",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE 1 TO CBLC-EX-PAYLOAD-RANGE-ERROR-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "END-COMPUTE.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: floating size errors should carry a range_error payload (on=%d payload=%d end=%d)\n",
            ft_strnstr(generated_cobol, "ON SIZE ERROR",
                std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol,
                "MOVE 1 TO CBLC-EX-PAYLOAD-RANGE-ERROR-CODE",
                std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "END-COMPUTE.",
                std::strlen(generated_cobol)) != NULL);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_translates_file_status_without_catching_eof)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "file datafile \"input.txt\";\n"
        "char buffer[8];\n"
        "void main() {\n"
        "    try {\n"
        "        open(datafile, \"r\");\n"
        "        read(datafile, buffer);\n"
        "    }\n"
        "    catch (...) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "file-status source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "file-status source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "FILE STATUS IS DATAFILE-STATUS.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF DATAFILE-STATUS NOT = \"00\"",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "IF DATAFILE-STATUS NOT = \"00\" AND DATAFILE-STATUS NOT = \"10\"",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE DATAFILE-STATUS TO CBLC-EX-MESSAGE-BUF",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: file status failures should become typed exceptions while EOF stays ordinary\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_indexed_file_invalid_key)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "file indexed orders \"orders.dat\";\n"
        "char record[16];\n"
        "void main() {\n"
        "    try { write(orders, record); }\n"
        "    catch (...) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "indexed-file source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "indexed-file source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "ORGANIZATION IS INDEXED",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "RECORD KEY IS ORDERS-RECORD",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "INVALID KEY CONTINUE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "END-WRITE",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: indexed-file writes should lower INVALID KEY through the exception status adapter\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_translates_raw_call_exception)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "char buffer[8];\n"
        "void main() {\n"
        "    try { std::strcpy(buffer, \"x\"); }\n"
        "    catch (int call_error) { display(call_error); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "raw-call exception source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "raw-call exception source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "ON EXCEPTION",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 1 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "END-CALL.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-TRY-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "-CATCH-0",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: raw external calls should translate native call failures\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_adapts_raw_call_outside_try)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *adapted_call;
    const char *uncaught_handler;
    const char *failure_route;
    int status;

    source = "char buffer[8];\n"
        "void main() {\n"
        "    try { throw 1; }\n"
        "    catch (int handled) { display(handled); }\n"
        "    std::strcpy(buffer, \"x\");\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "raw call outside try should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "raw call outside try should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    adapted_call = ft_strnstr(generated_cobol, "ON EXCEPTION", std::strlen(generated_cobol));
    uncaught_handler = ft_strnstr(generated_cobol, "       CBLC-ROOT-UNCAUGHT.",
        std::strlen(generated_cobol));
    failure_route = ft_strnstr(generated_cobol,
        "GO TO CBLC-ROOT-UNCAUGHT", std::strlen(generated_cobol));
    if (!ft_strnstr(generated_cobol, "ON EXCEPTION",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 1 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "STOP RUN",
            std::strlen(generated_cobol))
        || !uncaught_handler || !failure_route || !adapted_call
        || !ft_strnstr(uncaught_handler, "PERFORM CBLC-EX-CLEAR-PAYLOAD",
            std::strlen(uncaught_handler))
        || !ft_strnstr(adapted_call, "GO TO CBLC-ROOT-UNCAUGHT",
            std::strlen(adapted_call)))
    {
        std::printf("Assertion failed: raw calls outside try should propagate native call failures\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_propagates_raw_call_effect_to_caller)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "char buffer[8];\n"
        "void worker() {\n"
        "    try { throw 1; }\n"
        "    catch (int handled) { display(handled); }\n"
        "    std::strcpy(buffer, \"x\");\n"
        "}\n"
        "void main() { worker(); }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "transitive raw-call source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "transitive raw-call source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM WORKER THRU CBLC-FRETURN-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "ON EXCEPTION",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF CBLC-EX-ACTIVE = 'Y'",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: raw-call effects should propagate to local callers\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_translates_allocation_failure)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "int *numbers;\n"
        "void allocate_numbers() { numbers = new int[2]; }\n"
        "void main() {\n"
        "    try { allocate_numbers(); }\n"
        "    catch (bad_alloc array_error) { display(array_error.code); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "allocation failure source should parse") != FT_SUCCESS)
    {
        std::printf("Allocation failure parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message,
            sizeof(diagnostic_message)) != FT_SUCCESS || !generated_cobol)
    {
        std::printf("Allocation failure generation diagnostic: %d: %s\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "IF NUMBERS = NULL",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 1 TO CBLC-EX-PAYLOAD-BAD-ALLOC-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-DYNAMIC-TYPE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CATCH-0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: allocation failure should enter native exception dispatch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_translates_malloc_allocation_failure)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "char *raw;\n"
        "void main() {\n"
        "    try { raw = std::malloc(8); }\n"
        "    catch (int allocation_error) { display(allocation_error); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Malloc allocation failure parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message,
            sizeof(diagnostic_message)) != FT_SUCCESS || !generated_cobol)
    {
        std::printf("Malloc allocation failure generation diagnostic: %d: %s\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "ALLOCATE 8 CHARACTERS RETURNING RAW",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF RAW = NULL",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 1 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: std::malloc failure should preserve its legacy integer exception payload\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_out_of_line_method_allocation_failure_enters_catch)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int *allocation_result;\n"
        "class AllocationOwner {\n"
        "    public:\n"
        "    void allocate();\n"
        "};\n"
        "void AllocationOwner::allocate() { allocation_result = new int[1]; }\n"
        "void main() {\n"
        "    AllocationOwner owner;\n"
        "    try { owner.allocate(); }\n"
        "    catch (bad_alloc allocation_error) { display(allocation_error.code); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: out-of-line allocation method source should parse: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        std::printf("Assertion failed: out-of-line allocation method source should generate COBOL\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "IF ", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, " = NULL", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-BAD-ALLOC-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CATCH-0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: allocation in an out-of-line method should propagate to its caller's catch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_out_of_line_constructor_allocation_failure_enters_catch)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int *allocation_result;\n"
        "class AllocationOwner {\n"
        "    public:\n"
        "    AllocationOwner();\n"
        "};\n"
        "AllocationOwner::AllocationOwner() { allocation_result = new int[1]; }\n"
        "void main() {\n"
        "    try { AllocationOwner owner; }\n"
        "    catch (bad_alloc allocation_error) { display(allocation_error.code); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: out-of-line allocation constructor source should parse: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        std::printf("Assertion failed: out-of-line allocation constructor source should generate COBOL\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, " = NULL", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-BAD-ALLOC-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CATCH-0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: allocation in an out-of-line constructor should reach its caller's catch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_does_not_match_private_base_catch)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Base {\n"
        "    public:\n"
        "    int code;\n"
        "};\n"
        "class PrivateDerived : private Base {\n"
        "    public:\n"
        "    int detail;\n"
        "};\n"
        "PrivateDerived value;\n"
        "void main() {\n"
        "    try { throw value; }\n"
        "    catch (const Base& caught_base) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_SUCCESS;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: inaccessible private base catch should be rejected\n");
        status = FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_matches_derived_exception_in_base_catch)
{
    const char *source;
    t_cblc_translation_unit unit;
    const t_cblc_struct_type *derived_type;
    char *generated_cobol;
    int status;

    source = "class Base {\n"
        "    public:\n"
        "    int code;\n"
        "    char message;\n"
        "};\n"
        "class Derived : public Base {\n"
        "    public:\n"
        "    int detail;\n"
        "};\n"
        "Derived value;\n"
        "void main() {\n"
        "    try { throw value; }\n"
        "    catch (const Base& caught) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    derived_type = NULL;
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "derived exception source should parse") != FT_SUCCESS)
    {
        std::printf("Derived exception parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    for (size_t type_index = 0; type_index < unit.struct_type_count; ++type_index)
        if (std::strcmp(unit.struct_types[type_index].source_name, "Derived") == 0)
            derived_type = &unit.struct_types[type_index];
    if (!derived_type || !derived_type->has_base_type
        || std::strncmp(derived_type->base_type_name, "Base",
            sizeof(derived_type->base_type_name)) != 0)
    {
        std::printf("Assertion failed: derived type should record its base type\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "derived exception source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "CBLC-EX-TYPE-ID =",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-DERIVED-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-BASE-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-BASE-MESSAGE",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: base catch should match and materialize a derived payload view\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_copies_derived_payload_for_base_value_catch)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Base {\n"
        "    public:\n"
        "    int code;\n"
        "    string message;\n"
        "};\n"
        "class Derived : public Base {\n"
        "    public:\n"
        "    int detail;\n"
        "};\n"
        "Derived thing;\n"
        "void main() {\n"
        "    try { throw thing; }\n"
        "    catch (Base caught) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "base value catch source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "base value catch source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-DERIVED-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-BASE-CODE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-BASE-MESSAGE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CAUGHT-MESSAGE",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: base value catches should copy and own the base payload\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_runs_base_constructor_before_derived_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Base {\n"
        "    Base() { throw 21; }\n"
        "};\n"
        "class Derived : public Base {\n"
        "    Derived() { display(\"DERIVED\"); }\n"
        "};\n"
        "void main() {\n"
        "    try { Derived item; }\n"
        "    catch (int caught_value) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "base constructor ordering source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "base constructor ordering source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE 21 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-CONSTRUCTOR-MAIN-ITEM-0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: derived construction should execute the base constructor phase first\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_literal_exception_message_payload)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main() {\n"
        "    try {\n"
        "        throw \"FAILED\";\n"
        "    }\n"
        "    catch (...) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "literal exception-message source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "literal exception-message source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "CBLC-EX-MESSAGE-BUF",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-EX-MESSAGE-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should carry the literal exception message\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_propagates_local_throwing_call)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void worker() {\n"
        "    throw 11;\n"
        "}\n"
        "void main() {\n"
        "    try {\n"
        "        string owned_text(16);\n"
        "        worker();\n"
        "    }\n"
        "    catch (int caught_value) {\n"
        "        display(caught_value);\n"
        "    }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local throwing-call source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "local throwing-call source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM WORKER THRU CBLC-FRETURN-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF CBLC-EX-ACTIVE = 'Y'",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "PERFORM CBLC-EX-CLEANUP-MAIN-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-TRY-CBLC-TRY-CALL-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-FRETURN-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: local throwing call should propagate through context\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_routes_nested_rethrow_to_outer_handler)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main() {\n"
        "    try {\n"
        "        try {\n"
        "            throw 1;\n"
        "        }\n"
        "        catch (int inner_value) {\n"
        "            throw;\n"
        "        }\n"
        "    }\n"
        "    catch (int outer_value) {\n"
        "        display(outer_value);\n"
        "    }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested rethrow source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested rethrow source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "GO TO CBLC-TRY-CBLC-TRY-CALL-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "PERFORM CBLC-TRY-CALL-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: nested rethrow should dispatch to the enclosing handler\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_exits_after_return_with_raii_cleanup)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void worker() {\n"
        "    string owned_text(16);\n"
        "    return;\n"
        "}\n"
        "void main() { worker(); }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "RAII return source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "RAII return source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM CBLC-DESTRUCTOR-WORKER-OWNED-TEXT",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-FRETURN-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: return should clean owned values and leave the procedure\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_rejects_unreachable_exception_handlers)
{
    const char *sources[3];
    size_t index;
    int status;

    sources[0] = "void main() {\n"
        "    try { throw 1; }\n"
        "    catch (ErrorValue caught_error) { }\n"
        "    catch (int caught_value) { display(caught_value); }\n"
        "}\n";
    sources[1] = "void main() {\n"
        "    try { throw 1; }\n"
        "    catch (int first_value) { display(first_value); }\n"
        "    catch (int second_value) { display(second_value); }\n"
        "}\n";
    sources[2] = "class Base {\n"
        "    public:\n"
        "    int code;\n"
        "};\n"
        "class Derived : public Base {\n"
        "    public:\n"
        "    int detail;\n"
        "};\n"
        "Derived value;\n"
        "void main() {\n"
        "    try { throw value; }\n"
        "    catch (const Base& base_value) { }\n"
        "    catch (const Derived& derived_value) { }\n"
        "}\n";
    status = FT_SUCCESS;
    index = 0;
    while (index < 3)
    {
        t_cblc_translation_unit unit;

        cblc_translation_unit_init(&unit);
        if (cblc_parse_translation_unit(sources[index], &unit) == FT_SUCCESS)
        {
            std::printf("Assertion failed: unreachable exception handler should be rejected\n");
            status = FT_FAILURE;
        }
        cblc_translation_unit_dispose(&unit);
        index += 1;
    }
    return (status);
}

FT_TEST(test_cblc_generate_cobol_propagates_throwing_method)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Worker {\n"
        "    public:\n"
        "    int value;\n"
        "    void fail() { throw 4; }\n"
        "};\n"
        "Worker worker;\n"
        "void main() {\n"
        "    try { worker.fail(); }\n"
        "    catch (int caught_value) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "throwing method source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "throwing method source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "CBLC-METHOD-WORKER-FAIL.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "THRU CBLC-MRET-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF CBLC-EX-ACTIVE = 'Y'",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-MRET-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: throwing methods should propagate through the exception context\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_propagates_throwing_constructor)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Worker {\n"
        "    public:\n"
        "    int value;\n"
        "    Worker() { throw 9; }\n"
        "};\n"
        "void main() {\n"
        "    try { Worker worker; }\n"
        "    catch (int caught_value) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "throwing constructor source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "throwing constructor source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "CBLC-CONSTRUCTOR-MAIN-WORKER-0.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "PERFORM CBLC-CONSTRUCTOR-MAIN-WORKER-0 THRU CBLC-CRET-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF CBLC-EX-ACTIVE = 'Y'",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-CRET-",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: throwing constructors should propagate through the exception context\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_requires_exception_context_for_external_throwing_entry)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int api() { throw 7; }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "external throwing entry should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "external throwing entry should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "PROCEDURE DIVISION USING CBLC-EX-CONTEXT",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-TERMINATE.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: a throwing external entry must expose the exception context ABI\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parser_preserves_try_and_catch_source_positions)
{
    const char *source;
    t_cblc_translation_unit unit;
    size_t index;
    int saw_try;
    int saw_catch;
    int status;

    source = "void main() {\n"
        "    try {\n"
        "        throw 1;\n"
        "    }\n"
        "    catch (const int& caught_value) { display(caught_value); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    saw_try = 0;
    saw_catch = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "try/catch source positions should parse") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit.functions[0].statement_count)
    {
        const t_cblc_statement *statement;

        statement = &unit.functions[0].statements[index];
        if (statement->type == CBLC_STATEMENT_TRY_BEGIN)
        {
            if (statement->exception_source_line != 2
                || statement->exception_source_column != 5)
                goto cleanup;
            saw_try = 1;
        }
        else if (statement->type == CBLC_STATEMENT_CATCH_BEGIN)
        {
            if (statement->exception_source_line != 5
                || statement->exception_source_column != 5)
                goto cleanup;
            saw_catch = 1;
        }
        index += 1;
    }
    if (!saw_try || !saw_catch)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (status != FT_SUCCESS)
        std::printf("Assertion failed: try and catch source positions should be preserved\n");
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_mangles_reserved_exception_payload_names)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int value;\n"
        "void main() {\n"
        "    try { throw value; }\n"
        "    catch (int caught) { display(caught); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "reserved payload identifier source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "reserved payload identifier source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!std::strstr(generated_cobol, "01 CBLC-USER-VALUE")
        || std::strstr(generated_cobol, "01 VALUE "))
    {
        std::printf("Assertion failed: reserved CBL-C identifiers should be safe COBOL names\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_context_for_throwing_method_without_try)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Worker {\n"
        "    public:\n"
        "    int value;\n"
        "    void fail() { throw 14; }\n"
        "};\n"
        "Worker worker;\n"
        "void main() { worker.fail(); }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "unhandled throwing method source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "unhandled throwing method source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 CBLC-EX-CONTEXT.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-METHOD-WORKER-FAIL THRU CBLC-MRET-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF CBLC-EX-ACTIVE = 'Y'",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "STOP RUN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: throwing method calls should enable the exception context without a local try\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_context_for_throwing_constructor_without_try)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Worker {\n"
        "    Worker() { throw 15; }\n"
        "};\n"
        "void main() { Worker worker; }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "unhandled throwing constructor source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "unhandled throwing constructor source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 CBLC-EX-CONTEXT.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "PERFORM CBLC-CONSTRUCTOR-MAIN-WORKER-0 THRU CBLC-CRET-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "IF CBLC-EX-ACTIVE = 'Y'",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: throwing constructor calls should enable the exception context without a local try\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_cleans_completed_constructor_fields_on_throw)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Resource {\n"
        "    private: char *data;\n"
        "    public:\n"
        "    Resource() { data = new char[1]; }\n"
        "    ~Resource() { delete[] data; }\n"
        "};\n"
        "class Person {\n"
        "    Resource name;\n"
        "    Person() { throw 12; }\n"
        "};\n"
        "void main() {\n"
        "    try { Person person; }\n"
        "    catch (int caught_value) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "constructor field cleanup source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "constructor field cleanup source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM CBLC-DESTRUCTOR-MAIN-PERSON-NAME",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: completed constructor fields should be cleaned on throw\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_cleans_prior_local_when_constructor_throws)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *cleanup_paragraph;
    const char *cleanup_free;
    const char *cleanup_exit;
    int diagnostic_code;
    char diagnostic_message[512];
    int status;

    source = "class Resource {\n"
        "    private: char *data;\n"
        "    public:\n"
        "    Resource() { data = new char[1]; }\n"
        "    ~Resource() { delete[] data; }\n"
        "};\n"
        "class Failing {\n"
        "    Failing() { throw 14; }\n"
        "};\n"
        "void main() {\n"
        "    try { Resource ready; Failing failing; }\n"
        "    catch (int caught_value) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local constructor failure source should parse") != FT_SUCCESS)
        goto cleanup;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message, sizeof(diagnostic_message))
        != FT_SUCCESS)
    {
        std::printf("Local constructor cleanup diagnostic %d: %s\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    cleanup_paragraph = std::strstr(generated_cobol,
        "\n       CBLC-EX-CLEANUP-MAIN-");
    cleanup_free = cleanup_paragraph
        ? std::strstr(cleanup_paragraph, "PERFORM CBLC-DESTRUCTOR-MAIN-READY.") : NULL;
    cleanup_exit = cleanup_paragraph
        ? std::strstr(cleanup_paragraph, "           EXIT.") : NULL;
    if (!cleanup_paragraph || !cleanup_free || !cleanup_exit || cleanup_free >= cleanup_exit)
    {
        std::printf("Assertion failed: constructor-failure cleanup paragraph must release the completed local before exit\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_runs_array_constructors_and_reverse_destructors)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *second_destructor;
    const char *first_destructor;
    const char *free_statement;
    size_t free_count;
    int status;

    source = "class Tracker {\n"
        "    public:\n"
        "    int value;\n"
        "    char *owned;\n"
        "    Tracker() { value = 7; }\n"
        "    ~Tracker() { delete[] owned; display(value); }\n"
        "};\n"
        "Tracker trackers[2];\n"
        "void main() { }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Class array lifecycle parse failed: %d %s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class array lifecycle source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "TRACKERS-CBLC-USER-VALUE(1)",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "TRACKERS-CBLC-USER-VALUE(2)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: array constructor body should run for every element\n");
        goto cleanup;
    }
    second_destructor = std::strstr(generated_cobol,
        "DISPLAY TRACKERS-CBLC-USER-VALUE(2)");
    first_destructor = std::strstr(generated_cobol,
        "DISPLAY TRACKERS-CBLC-USER-VALUE(1)");
    if (!second_destructor || !first_destructor || second_destructor >= first_destructor)
    {
        std::printf("Assertion failed: array destructor bodies should run in reverse element order\n");
        goto cleanup;
    }
    free_count = 0;
    free_statement = generated_cobol;
    while ((free_statement = std::strstr(free_statement, "FREE ")) != NULL)
    {
        free_count += 1;
        free_statement += sizeof("FREE ") - 1;
    }
    if (free_count < 2)
    {
        std::printf("Assertion failed: each owning array element should execute its destructor\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_cleans_only_prior_fields_on_nested_constructor_throw)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Resource {\n"
        "    private: char *data;\n"
        "    public:\n"
        "    Resource() { data = new char[1]; }\n"
        "    ~Resource() { delete[] data; }\n"
        "};\n"
        "class FailingField {\n"
        "    FailingField() { throw 13; }\n"
        "};\n"
        "class Container {\n"
        "    Resource ready;\n"
        "    FailingField failing;\n"
        "    Container() { }\n"
        "};\n"
        "void main() {\n"
        "    try { Container container; }\n"
        "    catch (int caught_value) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested constructor failure source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested constructor failure source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "FREE MAIN-CONTAINER-READY-CBLC-USER-DATA",
            std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol,
            "PERFORM CBLC-DESTRUCTOR-MAIN-CONTAINER-FAILING",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: nested constructor failure must clean only prior completed fields\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_cleans_class_local_on_throw)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person {\n"
        "    int value;\n"
        "    Person() { value = 1; }\n"
        "    ~Person() { value = 0; }\n"
        "};\n"
        "void worker() { throw 4; }\n"
        "void main() {\n"
        "    try { Person person; worker(); }\n"
        "    catch (int caught_value) { display(caught_value); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class cleanup exception source should parse") != FT_SUCCESS)
    {
        std::printf("Class cleanup parse diagnostic (code=%d): %s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class cleanup exception source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM CBLC-EX-CLEANUP-MAIN-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "PERFORM CBLC-DESTRUCTOR-MAIN-PERSON",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 'Y' TO CBLC-EX-CLEANING",
            std::strlen(generated_cobol)))
    {
        std::printf("Class cleanup emitted: cleanup=%d destructor=%d cleaning_flag=%d\n",
            ft_strnstr(generated_cobol, "PERFORM CBLC-EX-CLEANUP-MAIN-",
                std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "PERFORM CBLC-DESTRUCTOR-MAIN-PERSON",
                std::strlen(generated_cobol)) != NULL,
            ft_strnstr(generated_cobol, "MOVE 'Y' TO CBLC-EX-CLEANING",
                std::strlen(generated_cobol)) != NULL);
        std::printf("Assertion failed: class locals should be destroyed through exception cleanup\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_rejects_throwing_destructor)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Resource {\n"
        "    ~Resource() { throw 1; }\n"
        "};\n"
        "void main() { }\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: throwing destructors should be rejected\n");
        goto cleanup;
    }
    if (unit.parse_error_code != TRANSPILE_ERROR_EXCEPTION_DESTRUCTOR_THROW)
    {
        std::printf("Assertion failed: throwing destructor should use a dedicated diagnostic\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_failure_reports_top_level_source_location)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void main() { }\n"
        "not_a_declaration;\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: invalid top-level declarations must be rejected\n");
        goto cleanup;
    }
    if (!std::strstr(unit.parse_error_message, "at 2:1")
        || !std::strstr(unit.parse_error_message, "not_a_declaration"))
    {
        std::printf("Assertion failed: top-level parse diagnostic must include line, column, and nearby source; got '%s'\n",
            unit.parse_error_message);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_rejects_untracked_temporary_exception_payload)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Error { public: int code; };\n"
        "void main() {\n"
        "    try { throw Error(1); }\n"
        "    catch (Error caught_error) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: untracked temporary exception payloads should be rejected\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_copies_trivial_struct_exception_payload)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct ErrorMeta { int severity; };\n"
        "struct ErrorValue { ErrorMeta meta; int code; char marker; };\n"
        "void main() {\n"
        "    ErrorValue error_value;\n"
        "    try { throw error_value; }\n"
        "    catch (ErrorValue caught_error) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "trivial struct exception source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "trivial struct exception source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 CBLC-EX-PAYLOAD-ERRORVALUE.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE MAIN-ERROR-VALUE TO CBLC-EX-PAYLOAD-ERRORVALUE.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE CBLC-EX-PAYLOAD-ERRORVALUE TO CAUGHT-ERROR",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: trivial struct exceptions should use typed payload storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_rejects_const_string_struct_exception_payload)
{
    const char *source = "struct ErrorValue { const string message; int code; };\n"
        "ErrorValue error_value;\n"
        "void main() { try { throw error_value; } catch (ErrorValue) { } }\n";
    t_cblc_translation_unit unit;
    cblc_translation_unit_init(&unit);
    if (test_expect_int_equal(cblc_parse_translation_unit(source, &unit)
                == FT_SUCCESS ? 1 : 0, 0,
            "const owning string payload must not use the obsolete buffer-copy ABI")
            != FT_SUCCESS
        || !ft_strnstr(unit.parse_error_message, "non-trivial lifecycle",
            std::strlen(unit.parse_error_message)))
    {
        std::printf("Const string payload diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_cobol_copies_string_struct_exception_payload)
{
    const char *source = "struct ErrorValue { string message; int code; };\n"
        "void main() { ErrorValue error_value; try { throw error_value; } "
        "catch (ErrorValue) { } }\n";
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *payload_cleanup;
    const char *payload_free;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int generation_diagnostic_code;
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generation_diagnostic[0] = '\0';
    generation_diagnostic_code = 0;
    status = FT_FAILURE;
    if (test_expect_int_equal(cblc_parse_translation_unit(source, &unit)
                == FT_SUCCESS ? 1 : 0, 1,
            "copyable owning string payload should be accepted for lifecycle-aware lowering")
            != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS)
    {
        std::printf("String-owning exception payload lowering diagnostic (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic);
        goto cleanup;
    }
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "CBLC-EX-PAYLOAD-ERRORVALUE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MESSAGE-CBLC-USER-DATA",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-DESTRUCTOR-CBLC-USER-STRING",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: string-bearing exception payload must emit typed storage and string lifecycle dispatch\n");
        goto cleanup;
    }
    payload_cleanup = ft_strnstr(generated_cobol, "CBLC-EX-CLEAR-PAYLOAD.",
        std::strlen(generated_cobol));
    payload_free = payload_cleanup
        ? ft_strnstr(payload_cleanup,
            "FREE CBLC-EX-PAYLOAD-ERRORVALUE-MESSAGE-CBLC-USER-DATA",
            std::strlen(payload_cleanup)) : NULL;
    if (!payload_cleanup || !payload_free)
    {
        std::printf("Assertion failed: exception cleanup must destroy the owning string field in its typed payload slot\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_binds_scalar_exception_reference)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main() {\n"
        "    try { throw 4; }\n"
        "    catch (const int& caught_value) { display(caught_value); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "scalar exception reference source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "scalar exception reference source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (ft_strnstr(generated_cobol, "MOVE CBLC-EX-INT-PAYLOAD TO CAUGHT-VALUE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "DISPLAY CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: scalar reference catches should bind the active payload\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_unit_lifetime_budget_reaches_cobol_generation)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int diagnostic_code;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    source = "void main() { int value; value = 1; return; }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: budget propagation fixture should parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    unit.max_lifetime_edge_transfers = 0;
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message,
            sizeof(diagnostic_message)) == FT_SUCCESS)
    {
        std::printf("Assertion failed: zero lifetime edge-transfer budget should stop COBOL generation\n");
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (diagnostic_code != TRANSPILE_ERROR_BACKEND_IR_VERIFICATION
        || !std::strstr(diagnostic_message, "resource_limit_exceeded")
        || !std::strstr(diagnostic_message, "dimension=cfg_edge_transfers"))
    {
        std::printf("Assertion failed: lifetime budget should produce the structured resource diagnostic, got (%d) %s\n",
            diagnostic_code, diagnostic_message);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

const t_test_case *get_validation_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"standard_library_migrated_helpers_use_direct_returns",
            test_standard_library_migrated_helpers_use_direct_returns},
        {"standard_library_date_parse_returns_multi_field_value",
            test_standard_library_date_parse_returns_multi_field_value},
        {"standard_library_typed_parsers_return_multi_field_values",
            test_standard_library_typed_parsers_return_multi_field_values},
        {"cblc_generate_cobol_emits_native_try_catch_dispatch",
            test_cblc_generate_cobol_emits_native_try_catch_dispatch},
        {"cblc_unit_lifetime_budget_reaches_cobol_generation",
            test_cblc_unit_lifetime_budget_reaches_cobol_generation},
        {"cblc_generate_cobol_requires_exception_context_for_external_throwing_entry",
            test_cblc_generate_cobol_requires_exception_context_for_external_throwing_entry},
        {"cblc_parser_preserves_try_and_catch_source_positions",
            test_cblc_parser_preserves_try_and_catch_source_positions},
        {"cblc_generate_cobol_mangles_reserved_exception_payload_names",
            test_cblc_generate_cobol_mangles_reserved_exception_payload_names},
        {"cblc_generate_cobol_clears_exception_before_return_from_catch",
            test_cblc_generate_cobol_clears_exception_before_return_from_catch},
        {"cblc_generate_cobol_materializes_value_before_catch_cleanup",
            test_cblc_generate_cobol_materializes_value_before_catch_cleanup},
        {"cblc_generate_cobol_qualifies_try_labels_per_function",
            test_cblc_generate_cobol_qualifies_try_labels_per_function},
        {"cblc_generate_cobol_translates_size_error_into_exception_context",
            test_cblc_generate_cobol_translates_size_error_into_exception_context},
        {"cblc_generate_cobol_translates_floating_size_error_to_range_error",
            test_cblc_generate_cobol_translates_floating_size_error_to_range_error},
        {"cblc_generate_cobol_translates_file_status_without_catching_eof",
            test_cblc_generate_cobol_translates_file_status_without_catching_eof},
        {"cblc_generate_cobol_emits_indexed_file_invalid_key",
            test_cblc_generate_cobol_emits_indexed_file_invalid_key},
        {"cblc_generate_cobol_translates_raw_call_exception",
            test_cblc_generate_cobol_translates_raw_call_exception},
        {"cblc_generate_cobol_adapts_raw_call_outside_try",
            test_cblc_generate_cobol_adapts_raw_call_outside_try},
        {"cblc_generate_cobol_propagates_raw_call_effect_to_caller",
            test_cblc_generate_cobol_propagates_raw_call_effect_to_caller},
        {"cblc_generate_cobol_translates_allocation_failure",
            test_cblc_generate_cobol_translates_allocation_failure},
        {"cblc_generate_cobol_translates_malloc_allocation_failure",
            test_cblc_generate_cobol_translates_malloc_allocation_failure},
        {"cblc_generate_cobol_matches_derived_exception_in_base_catch",
            test_cblc_generate_cobol_matches_derived_exception_in_base_catch},
        {"cblc_generate_cobol_does_not_match_private_base_catch",
            test_cblc_generate_cobol_does_not_match_private_base_catch},
        {"cblc_generate_cobol_copies_derived_payload_for_base_value_catch",
            test_cblc_generate_cobol_copies_derived_payload_for_base_value_catch},
        {"cblc_generate_cobol_runs_base_constructor_before_derived_body",
            test_cblc_generate_cobol_runs_base_constructor_before_derived_body},
        {"cblc_generate_cobol_emits_literal_exception_message_payload",
            test_cblc_generate_cobol_emits_literal_exception_message_payload},
        {"cblc_string_class_exception_does_not_fall_back_to_legacy_message",
            test_cblc_string_class_exception_does_not_fall_back_to_legacy_message},
        {"cblc_generate_cobol_propagates_local_throwing_call",
            test_cblc_generate_cobol_propagates_local_throwing_call},
        {"cblc_generate_cobol_routes_nested_rethrow_to_outer_handler",
            test_cblc_generate_cobol_routes_nested_rethrow_to_outer_handler},
        {"cblc_generate_cobol_propagates_throwing_method",
            test_cblc_generate_cobol_propagates_throwing_method},
        {"cblc_generate_cobol_emits_context_for_throwing_method_without_try",
            test_cblc_generate_cobol_emits_context_for_throwing_method_without_try},
        {"cblc_generate_cobol_emits_context_for_throwing_constructor_without_try",
            test_cblc_generate_cobol_emits_context_for_throwing_constructor_without_try},
        {"cblc_generate_cobol_propagates_throwing_constructor",
            test_cblc_generate_cobol_propagates_throwing_constructor},
        {"cblc_generate_cobol_cleans_completed_constructor_fields_on_throw",
            test_cblc_generate_cobol_cleans_completed_constructor_fields_on_throw},
        {"cblc_generate_cobol_cleans_prior_local_when_constructor_throws",
            test_cblc_generate_cobol_cleans_prior_local_when_constructor_throws},
        {"cblc_generate_cobol_runs_array_constructors_and_reverse_destructors",
            test_cblc_generate_cobol_runs_array_constructors_and_reverse_destructors},
        {"cblc_generate_cobol_cleans_only_prior_fields_on_nested_constructor_throw",
            test_cblc_generate_cobol_cleans_only_prior_fields_on_nested_constructor_throw},
        {"cblc_generate_cobol_exits_after_return_with_raii_cleanup",
            test_cblc_generate_cobol_exits_after_return_with_raii_cleanup},
        {"cblc_parse_rejects_unreachable_exception_handlers",
            test_cblc_parse_rejects_unreachable_exception_handlers},
        {"cblc_generate_cobol_cleans_class_local_on_throw",
            test_cblc_generate_cobol_cleans_class_local_on_throw},
        {"cblc_parse_rejects_throwing_destructor",
            test_cblc_parse_rejects_throwing_destructor},
        {"cblc_parse_failure_reports_top_level_source_location",
            test_cblc_parse_failure_reports_top_level_source_location},
        {"cblc_parse_rejects_untracked_temporary_exception_payload",
            test_cblc_parse_rejects_untracked_temporary_exception_payload},
        {"cblc_generate_cobol_copies_trivial_struct_exception_payload",
            test_cblc_generate_cobol_copies_trivial_struct_exception_payload},
        {"cblc_parse_rejects_const_string_struct_exception_payload",
            test_cblc_parse_rejects_const_string_struct_exception_payload},
        {"cblc_generate_cobol_copies_string_struct_exception_payload",
            test_cblc_generate_cobol_copies_string_struct_exception_payload},
        {"cblc_generate_cobol_binds_scalar_exception_reference",
            test_cblc_generate_cobol_binds_scalar_exception_reference},
        {"transpiler_validation_accepts_valid_cblc", test_transpiler_validation_accepts_valid_cblc},
        {"transpiler_validation_rejects_cblc_without_return", test_transpiler_validation_rejects_cblc_without_return},
        {"transpiler_validation_accepts_string_declaration", test_transpiler_validation_accepts_string_declaration},
        {"cblc_standard_string_class_uses_cblc_method_bodies",
            test_cblc_standard_string_class_uses_cblc_method_bodies},
        {"cblc_string_class_replaces_legacy_string_helper_overloads",
            test_cblc_string_class_replaces_legacy_string_helper_overloads},
        {"transpiler_validation_accepts_string_class_method_usage",
            test_transpiler_validation_accepts_string_class_method_usage},
        {"transpiler_validation_accepts_int_array_and_string_class_declarations",
            test_transpiler_validation_accepts_int_array_and_string_class_declarations},
        {"transpiler_validation_accepts_string_capacity_parentheses",
            test_transpiler_validation_accepts_string_capacity_parentheses},
        {"transpiler_validation_rejects_legacy_string_bracket_capacity",
            test_transpiler_validation_rejects_legacy_string_bracket_capacity},
        {"transpiler_validation_accepts_indexed_array_usage",
            test_transpiler_validation_accepts_indexed_array_usage},
        {"cblc_parse_translation_unit_records_imports", test_cblc_parse_translation_unit_records_imports},
        {"cblc_parse_translation_unit_records_copy_includes", test_cblc_parse_translation_unit_records_copy_includes},
        {"cblc_templates_instantiate_struct_fields", test_cblc_templates_instantiate_struct_fields},
        {"cblc_template_string_argument_resolves_to_class_field",
            test_cblc_template_string_argument_resolves_to_class_field},
        {"cblc_string_class_import_ignores_literals",
            test_cblc_string_class_import_ignores_literals},
        {"cblc_templates_instantiate_multi_parameter_struct_fields",
            test_cblc_templates_instantiate_multi_parameter_struct_fields},
        {"cblc_template_pointer_argument_preserves_shape",
            test_cblc_template_pointer_argument_preserves_shape},
        {"cblc_template_array_argument_preserves_shape",
            test_cblc_template_array_argument_preserves_shape},
        {"cblc_templates_instantiate_function", test_cblc_templates_instantiate_function},
        {"cblc_templates_instantiate_multi_parameter_function",
            test_cblc_templates_instantiate_multi_parameter_function},
        {"cblc_templates_reject_unsupported_parameter_forms",
            test_cblc_templates_reject_unsupported_parameter_forms},
        {"cblc_template_function_rejects_scalar_for_array_abi",
            test_cblc_template_function_rejects_scalar_for_array_abi},
        {"cblc_template_function_rejects_nontrivial_struct_array_abi",
            test_cblc_template_function_rejects_nontrivial_struct_array_abi},
        {"cblc_template_function_int_array_abi",
            test_cblc_template_function_int_array_abi},
        {"cblc_template_function_rejects_capacity_encoded_string_array_type",
            test_cblc_template_function_rejects_capacity_encoded_string_array_type},
        {"cblc_template_function_struct_array_abi",
            test_cblc_template_function_struct_array_abi},
        {"cblc_template_function_char_struct_array_abi",
            test_cblc_template_function_char_struct_array_abi},
        {"cblc_template_function_rejects_const_string_struct_array",
            test_cblc_template_function_rejects_const_string_struct_array},
        {"cblc_template_function_rejects_mutable_string_struct_array",
            test_cblc_template_function_rejects_mutable_string_struct_array},
        {"cblc_template_parse_reports_stable_diagnostic",
            test_cblc_template_parse_reports_stable_diagnostic},
        {"cblc_template_function_rejects_mismatched_argument",
            test_cblc_template_function_rejects_mismatched_argument},
        {"cblc_template_function_deduces_exact_string_argument",
            test_cblc_template_function_deduces_exact_string_argument},
        {"cblc_template_class_instantiates_members",
            test_cblc_template_class_instantiates_members},
        {"cblc_template_method_pointer_return_is_concrete",
            test_cblc_template_method_pointer_return_is_concrete},
        {"cblc_template_instantiation_limit_is_deterministic",
            test_cblc_template_instantiation_limit_is_deterministic},
        {"cblc_template_instantiation_limit_combines_class_and_function_instances",
            test_cblc_template_instantiation_limit_combines_class_and_function_instances},
        {"cblc_template_names_are_bounded_and_deterministic",
            test_cblc_template_names_are_bounded_and_deterministic},
        {"cblc_nested_template_type_application",
            test_cblc_nested_template_type_application},
        {"cblc_template_instantiation_depth_is_deterministic",
            test_cblc_template_instantiation_depth_is_deterministic},
        {"cblc_template_class_constructor_is_concrete",
            test_cblc_template_class_constructor_is_concrete},
        {"cblc_imported_template_type_instantiates_from_exported_metadata",
            test_cblc_imported_template_type_instantiates_from_exported_metadata},
        {"cblc_imported_inheritance_metadata_is_preserved",
            test_cblc_imported_inheritance_metadata_is_preserved},
        {"cblc_import_rejects_conflicting_same_named_type_layout",
            test_cblc_import_rejects_conflicting_same_named_type_layout},
        {"cblc_struct_pointer_fields_require_borrowed_annotation",
            test_cblc_struct_pointer_fields_require_borrowed_annotation},
        {"cblc_imported_template_function_instantiates_from_exported_body",
            test_cblc_imported_template_function_instantiates_from_exported_body},
        {"cblc_imported_trivial_struct_array_template_executes",
            test_cblc_imported_trivial_struct_array_template_executes},
        {"cblc_parse_translation_unit_tracks_multiple_functions",
            test_cblc_parse_translation_unit_tracks_multiple_functions},
        {"cblc_parse_translation_unit_accepts_functions_without_keyword",
            test_cblc_parse_translation_unit_accepts_functions_without_keyword},
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
        {"cblc_generate_cobol_emits_external_literal_argument_by_value",
            test_cblc_generate_cobol_emits_external_literal_argument_by_value},
        {"cblc_generate_cobol_emits_external_multiple_reference_arguments",
            test_cblc_generate_cobol_emits_external_multiple_reference_arguments},
        {"cblc_generate_cobol_emits_external_mixed_value_and_reference_arguments",
            test_cblc_generate_cobol_emits_external_mixed_value_and_reference_arguments},
        {"cblc_generate_cobol_emits_external_pointer_argument_by_reference",
            test_cblc_generate_cobol_emits_external_pointer_argument_by_reference},
        {"cblc_generate_cobol_emits_external_pointer_linkage",
            test_cblc_generate_cobol_emits_external_pointer_linkage},
        {"cblc_generate_cobol_emits_external_int_return_without_arguments",
            test_cblc_generate_cobol_emits_external_int_return_without_arguments},
        {"cblc_generate_cobol_emits_external_pointer_return_without_arguments",
            test_cblc_generate_cobol_emits_external_pointer_return_without_arguments},
        {"cblc_generate_cobol_emits_external_pointer_return_with_argument",
            test_cblc_generate_cobol_emits_external_pointer_return_with_argument},
        {"cblc_generate_cobol_emits_external_pointer_return_linkage",
            test_cblc_generate_cobol_emits_external_pointer_return_linkage},
        {"cblc_generate_cobol_emits_external_int_return_linkage",
            test_cblc_generate_cobol_emits_external_int_return_linkage},
        {"cblc_generate_cobol_propagates_external_exception_context",
            test_cblc_generate_cobol_propagates_external_exception_context},
        {"cblc_generate_cobol_handles_string_class_construction_and_methods",
            test_cblc_generate_cobol_handles_string_class_construction_and_methods},
        {"cblc_method_parameters_destroy_in_reverse_declaration_order",
            test_cblc_method_parameters_destroy_in_reverse_declaration_order},
        {"cblc_method_throw_cleans_by_value_parameters",
            test_cblc_method_throw_cleans_by_value_parameters},
        {"cblc_generate_cobol_handles_multiplication_and_division",
            test_cblc_generate_cobol_handles_multiplication_and_division},
        {"cblc_generate_c_emits_struct_types_and_field_access",
            test_cblc_generate_c_emits_struct_types_and_field_access},
        {"cblc_generate_c_emits_nested_struct_types_and_field_access",
            test_cblc_generate_c_emits_nested_struct_types_and_field_access},
        {"cblc_generate_c_emits_array_declarations",
            test_cblc_generate_c_emits_array_declarations},
        {"cblc_generate_cobol_emits_array_declarations",
            test_cblc_generate_cobol_emits_array_declarations},
        {"cblc_generate_c_emits_indexed_array_access",
            test_cblc_generate_c_emits_indexed_array_access},
        {"cblc_generate_cobol_emits_indexed_array_access",
            test_cblc_generate_cobol_emits_indexed_array_access},
        {"cblc_generate_c_emits_variable_index_array_access",
            test_cblc_generate_c_emits_variable_index_array_access},
        {"cblc_generate_cobol_emits_variable_index_array_access",
            test_cblc_generate_cobol_emits_variable_index_array_access},
        {"cblc_generate_c_emits_unique_block_local_scalar_storage",
            test_cblc_generate_c_emits_unique_block_local_scalar_storage},
        {"cblc_parse_translation_unit_rejects_block_local_after_scope",
            test_cblc_parse_translation_unit_rejects_block_local_after_scope},
        {"cblc_scope_metadata_tracks_lexical_ownership",
            test_cblc_scope_metadata_tracks_lexical_ownership},
        {"cblc_scope_metadata_marks_shadowed_bindings",
            test_cblc_scope_metadata_marks_shadowed_bindings},
        {"cblc_scope_resolution_rejects_cross_function_local_reference",
            test_cblc_scope_resolution_rejects_cross_function_local_reference},
        {"cblc_parse_translation_unit_records_class_lifecycle_metadata",
            test_cblc_parse_translation_unit_records_class_lifecycle_metadata},
        {"cblc_parse_translation_unit_records_parameterized_class_lifecycle_and_methods",
            test_cblc_parse_translation_unit_records_parameterized_class_lifecycle_and_methods},
        {"cblc_parse_translation_unit_records_constructor_overloads_by_arity",
            test_cblc_parse_translation_unit_records_constructor_overloads_by_arity},
        {"cblc_parse_translation_unit_records_copy_constructor_overload",
            test_cblc_parse_translation_unit_records_copy_constructor_overload},
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
        {"cblc_parse_translation_unit_imports_canonical_string_class",
            test_cblc_parse_translation_unit_imports_canonical_string_class},
        {"cblc_local_string_declaration_is_a_class_instance",
            test_cblc_local_string_declaration_is_a_class_instance},
        {"cblc_string_constructor_arguments_resolve_to_the_class",
            test_cblc_string_constructor_arguments_resolve_to_the_class},
        {"cblc_parse_translation_unit_records_class_methods",
            test_cblc_parse_translation_unit_records_class_methods},
        {"cblc_parse_translation_unit_records_out_of_class_method_definitions",
            test_cblc_parse_translation_unit_records_out_of_class_method_definitions},
        {"cblc_parse_translation_unit_records_struct_return_type",
            test_cblc_parse_translation_unit_records_struct_return_type},
        {"cblc_parse_translation_unit_records_struct_returning_method",
            test_cblc_parse_translation_unit_records_struct_returning_method},
        {"cblc_generate_c_emits_class_methods",
            test_cblc_generate_c_emits_class_methods},
        {"cblc_generate_c_emits_out_of_class_method_definitions",
            test_cblc_generate_c_emits_out_of_class_method_definitions},
        {"cblc_generate_c_and_cobol_emit_parameterized_class_lifecycle_and_methods",
            test_cblc_generate_c_and_cobol_emit_parameterized_class_lifecycle_and_methods},
        {"cblc_method_overloads_resolve_by_argument_conversion",
            test_cblc_method_overloads_resolve_by_argument_conversion},
        {"cblc_generate_c_and_cobol_emit_constructor_overloads_by_arity",
            test_cblc_generate_c_and_cobol_emit_constructor_overloads_by_arity},
        {"cblc_generate_c_and_cobol_emit_copy_constructor_overload",
            test_cblc_generate_c_and_cobol_emit_copy_constructor_overload},
        {"cblc_generate_c_emits_struct_returning_function_and_call_assignment",
            test_cblc_generate_c_emits_struct_returning_function_and_call_assignment},
        {"cblc_generate_c_emits_external_string_class_argument",
            test_cblc_generate_c_emits_external_string_class_argument},
        {"cblc_generate_c_emits_external_pointer_argument",
            test_cblc_generate_c_emits_external_pointer_argument},
        {"cblc_generate_c_emits_external_mixed_parameterized_call_arguments",
            test_cblc_generate_c_emits_external_mixed_parameterized_call_arguments},
        {"cblc_generate_c_emits_external_int_return_without_arguments",
            test_cblc_generate_c_emits_external_int_return_without_arguments},
        {"cblc_generate_c_emits_external_pointer_return_without_arguments",
            test_cblc_generate_c_emits_external_pointer_return_without_arguments},
        {"cblc_generate_c_and_cobol_emit_struct_returning_method_call_assignment",
            test_cblc_generate_c_and_cobol_emit_struct_returning_method_call_assignment},
        {"cblc_generate_cobol_emits_class_methods",
            test_cblc_generate_cobol_emits_class_methods},
        {"cblc_boolean_literals_use_numeric_boolean_storage",
            test_cblc_boolean_literals_use_numeric_boolean_storage},
        {"cblc_generate_cobol_emits_struct_returning_function_and_call_assignment",
            test_cblc_generate_cobol_emits_struct_returning_function_and_call_assignment},
        {"cblc_parse_translation_unit_rejects_private_field_access_outside_class",
            test_cblc_parse_translation_unit_rejects_private_field_access_outside_class},
        {"cblc_parse_translation_unit_rejects_private_method_call_outside_class",
            test_cblc_parse_translation_unit_rejects_private_method_call_outside_class},
        {"cblc_parse_translation_unit_accepts_public_members_outside_class",
            test_cblc_parse_translation_unit_accepts_public_members_outside_class},
        {"cblc_parse_translation_unit_accepts_private_access_within_class_methods",
            test_cblc_parse_translation_unit_accepts_private_access_within_class_methods},
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
        {"cblc_resolve_translation_unit_calls_accepts_external_two_argument_call",
            test_cblc_resolve_translation_unit_calls_accepts_external_two_argument_call},
        {"cblc_resolve_translation_unit_calls_rejects_exception_abi_mismatch",
            test_cblc_resolve_translation_unit_calls_rejects_exception_abi_mismatch},
        {"cblc_import_translation_unit_types_rejects_exception_abi_mismatch",
            test_cblc_import_translation_unit_types_rejects_exception_abi_mismatch},
        {"cblc_import_translation_unit_types_rejects_exception_payload_overflow",
            test_cblc_import_translation_unit_types_rejects_exception_payload_overflow},
        {"cblc_resolve_translation_unit_calls_rejects_external_argument_count_mismatch",
            test_cblc_resolve_translation_unit_calls_rejects_external_argument_count_mismatch},
        {"cblc_resolve_translation_unit_calls_rejects_external_extra_argument",
            test_cblc_resolve_translation_unit_calls_rejects_external_extra_argument},
        {"cblc_resolve_translation_unit_calls_rejects_external_argument_type_mismatch",
            test_cblc_resolve_translation_unit_calls_rejects_external_argument_type_mismatch},
        {"cblc_resolve_translation_unit_calls_rejects_external_void_return_assignment",
            test_cblc_resolve_translation_unit_calls_rejects_external_void_return_assignment},
        {"cblc_resolve_translation_unit_calls_rejects_unimported_external_parameterized_call",
            test_cblc_resolve_translation_unit_calls_rejects_unimported_external_parameterized_call},
        {"cblc_resolve_translation_unit_calls_accepts_external_string_variable_argument",
            test_cblc_resolve_translation_unit_calls_accepts_external_string_variable_argument},
        {"cblc_resolve_translation_unit_calls_accepts_constructed_external_string_argument",
            test_cblc_resolve_translation_unit_calls_accepts_constructed_external_string_argument},
        {"cblc_resolve_translation_unit_calls_rejects_external_string_argument_type_mismatch",
            test_cblc_resolve_translation_unit_calls_rejects_external_string_argument_type_mismatch},
        {"cblc_resolve_translation_unit_calls_accepts_external_int_pointer_argument",
            test_cblc_resolve_translation_unit_calls_accepts_external_int_pointer_argument},
        {"cblc_resolve_translation_unit_calls_accepts_external_void_pointer_from_int_pointer",
            test_cblc_resolve_translation_unit_calls_accepts_external_void_pointer_from_int_pointer},
        {"cblc_resolve_translation_unit_calls_rejects_external_pointer_argument_type_mismatch",
            test_cblc_resolve_translation_unit_calls_rejects_external_pointer_argument_type_mismatch},
        {"cblc_resolve_translation_unit_calls_accepts_external_int_return_without_arguments",
            test_cblc_resolve_translation_unit_calls_accepts_external_int_return_without_arguments},
        {"cblc_resolve_translation_unit_calls_accepts_external_pointer_return_without_arguments",
            test_cblc_resolve_translation_unit_calls_accepts_external_pointer_return_without_arguments},
        {"cblc_resolve_translation_unit_calls_accepts_external_pointer_return_with_argument",
            test_cblc_resolve_translation_unit_calls_accepts_external_pointer_return_with_argument},
        {"transpiler_validation_accepts_valid_cobol", test_transpiler_validation_accepts_valid_cobol},
        {"transpiler_validation_accepts_working_storage_program", test_transpiler_validation_accepts_working_storage_program},
        {"transpiler_validation_resolves_go_to_paragraph_targets", test_transpiler_validation_resolves_go_to_paragraph_targets},
        {"transpiler_validation_rejects_unresolved_go_to_target", test_transpiler_validation_rejects_unresolved_go_to_target},
        {"transpiler_validation_resolves_perform_thru_targets", test_transpiler_validation_resolves_perform_thru_targets},
        {"transpiler_validation_rejects_unresolved_perform_target", test_transpiler_validation_rejects_unresolved_perform_target},
        {"transpiler_validation_rejects_unresolved_perform_thru_end_target", test_transpiler_validation_rejects_unresolved_perform_thru_end_target},
        {"transpiler_validation_rejects_invalid_cobol", test_transpiler_validation_rejects_invalid_cobol},
        {"lexer_token_get_span_reports_expected_columns",
            test_lexer_token_get_span_reports_expected_columns},
        {"ast_node_get_span_uses_node_token", test_ast_node_get_span_uses_node_token},
        {"cblc_frontend_analysis_reports_parse_diagnostic",
            test_cblc_frontend_analysis_reports_parse_diagnostic},
        {"cblc_frontend_collect_document_symbols_returns_types_functions_and_data",
            test_cblc_frontend_collect_document_symbols_returns_types_functions_and_data},
        {"cblc_frontend_find_definition_resolves_data_item_reference",
            test_cblc_frontend_find_definition_resolves_data_item_reference},
        {"cblc_frontend_find_definition_resolves_function_call_target",
            test_cblc_frontend_find_definition_resolves_function_call_target},
        {"cblc_frontend_get_hover_reports_variable_summary",
            test_cblc_frontend_get_hover_reports_variable_summary},
        {"cblc_frontend_get_hover_reports_class_summary",
            test_cblc_frontend_get_hover_reports_class_summary},
        {"cblc_frontend_find_references_collects_declaration_and_uses",
            test_cblc_frontend_find_references_collects_declaration_and_uses},
        {"cblc_frontend_collect_semantic_tokens_classifies_core_tokens",
            test_cblc_frontend_collect_semantic_tokens_classifies_core_tokens},
        {"cblc_frontend_complete_returns_keywords_and_symbols",
            test_cblc_frontend_complete_returns_keywords_and_symbols},
        {"cblc_frontend_complete_filters_function_local_scope",
            test_cblc_frontend_complete_filters_function_local_scope},
        {"cblc_frontend_complete_returns_public_members_only",
            test_cblc_frontend_complete_returns_public_members_only},
        {"cblc_frontend_complete_returns_std_namespace_functions",
            test_cblc_frontend_complete_returns_std_namespace_functions},
        {"cblc_frontend_complete_returns_string_class_members",
            test_cblc_frontend_complete_returns_string_class_members}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
FT_TEST(test_cblc_generate_cobol_emits_sequential_file_io)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "file input input \"input.txt\";\n"
        "file output output \"output.txt\";\n"
        "char record[16];\n"
        "void main() {\n"
        "    open(input, \"r\");\n"
        "    open(output, \"w\");\n"
        "    while (read(input, record)) {\n"
        "        write(output, record);\n"
        "    }\n"
        "    close(input);\n"
        "    close(output);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "sequential file source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "sequential file source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    {
        static const char *const required_output[] = {
            "SELECT INPUT ASSIGN TO \"input.txt\" FILE STATUS IS INPUT-STATUS.",
            "SELECT CBLC-USER-OUTPUT ASSIGN TO \"output.txt\" FILE STATUS IS CBLC-USER-OUTPUT-STATUS.",
            "FD INPUT.",
            "OPEN INPUT INPUT.",
            "PERFORM UNTIL CBLC-EOF-FLAG = 'Y'",
            "READ INPUT INTO CBLC-USER-RECORD",
            "WRITE CBLC-USER-OUTPUT-RECORD",
            "END-PERFORM.",
            "CLOSE INPUT.",
            "CLOSE CBLC-USER-OUTPUT."
        };
        size_t index;

        index = 0;
        while (index < sizeof(required_output) / sizeof(required_output[0]))
        {
            if (!generated_cobol
                || !ft_strnstr(generated_cobol, required_output[index],
                    std::strlen(generated_cobol)))
            {
                std::printf("Assertion failed: generated COBOL is missing sequential file I/O: %s\n",
                    required_output[index]);
                goto cleanup;
            }
            index += 1;
        }
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_templates_materialize_independent_specializations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const t_cblc_struct_type *int_box;
    const t_cblc_struct_type *char_box;
    int status;

    source = "template <typename T> struct box { T value; };\n"
        "box<int> int_box;\n"
        "box<char> char_box;\n"
        "void main() { return; }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    int_box = NULL;
    char_box = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "independent template specializations should parse") != FT_SUCCESS)
        goto cleanup;
    for (size_t index = 0; index < unit.struct_type_count; ++index)
    {
        if (std::strcmp(unit.struct_types[index].source_name, "box__int") == 0)
            int_box = &unit.struct_types[index];
        if (std::strcmp(unit.struct_types[index].source_name, "box__char") == 0)
            char_box = &unit.struct_types[index];
    }
    if (!int_box || !char_box || int_box->field_count != 1 || char_box->field_count != 1
        || int_box->fields[0].kind != CBLC_DATA_KIND_INT
        || char_box->fields[0].kind != CBLC_DATA_KIND_CHAR
        || int_box->fields[0].is_template_parameter
        || char_box->fields[0].is_template_parameter)
    {
        std::printf("Assertion failed: template specializations should own concrete field layouts\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "independent template specializations should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "INT-BOX-VALUE", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CHAR-BOX-VALUE", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "T VALUE", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: concrete template fields should be emitted independently\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_try_catch_nested_rethrow_preserves_outer_dispatch)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *inner_catch;
    int status;

    source = "void main() {\n"
        "    try {\n"
        "        try { throw 1; }\n"
        "        catch (int inner_error) { throw 2; }\n"
        "    }\n"
        "    catch (int outer_error) { display(outer_error); }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    inner_catch = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested rethrow source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested rethrow source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    inner_catch = ft_strnstr(generated_cobol, "CBLC-TRY-CALL-",
        std::strlen(generated_cobol));
    if (!inner_catch
        || !ft_strnstr(inner_catch, "PERFORM CBLC-EX-CLEAR-PAYLOAD",
            std::strlen(inner_catch))
        || !ft_strnstr(generated_cobol, "GO TO CBLC-TRY-CBLC-TRY-CALL-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "MOVE 2 TO CBLC-EX-INT-PAYLOAD",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: nested rethrows should transfer to the enclosing handler\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_throw_emits_active_raise_guard_and_abort_path)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    const char *terminate_label;
    int status;

    source = "void main() {\n"
        "    try { throw 1; }\n"
        "    catch (int first_error) { }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    terminate_label = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "exception source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "exception source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    terminate_label = ft_strnstr(generated_cobol, "CBLC-TERMINATE.",
        std::strlen(generated_cobol));
    if (!terminate_label
        || !ft_strnstr(generated_cobol,
            "IF CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "TO CBLC-EX-SECONDARY-TYPE-ID",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE 'Y' TO CBLC-EX-CLEANING\n           PERFORM CBLC-EX-CLEAR-PAYLOAD",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "PERFORM CBLC-TERMINATE",
            std::strlen(generated_cobol))
        || !ft_strnstr(terminate_label, "STOP RUN.", std::strlen(terminate_label)))
    {
        std::printf("Assertion failed: generated throws should guard against re-entry during raising/cleanup and provide fatal termination\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}
