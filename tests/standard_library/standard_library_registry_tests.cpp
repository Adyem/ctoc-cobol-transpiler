#include "standard_library_test_support.hpp"

#include "../compiler/cobol/round_trip_pipeline_helpers.hpp"

FT_TEST(test_standard_library_lookup_enforces_std_prefix)
{
    const t_transpiler_standard_library_entry *entry;

    if (transpiler_standard_library_lookup("atoi") != NULL)
        return (FT_FAILURE);
    entry = transpiler_standard_library_lookup_with_buffer_kind(
        "std::atoi", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR);
    if (!entry || std::strcmp(entry->program_name, "CBLC-ATOI") != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_lookup_is_case_sensitive)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::STRLEN");
    if (entry)
    {
        std::printf("Assertion failed: std::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::strlen");
    if (entry)
    {
        std::printf("Assertion failed: STD::strlen should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::STRLEN");
    if (entry)
    {
        std::printf("Assertion failed: STD::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::MEMCMP");
    if (entry)
    {
        std::printf("Assertion failed: std::MEMCMP should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::memcmp");
    if (entry)
    {
        std::printf("Assertion failed: STD::memcmp should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_catalog_lists_all_entries)
{
    const t_transpiler_standard_library_entry *entries;
    size_t count;
    size_t expected_count;
    size_t index;
    static const struct
    {
        const char *qualified_name;
        t_transpiler_standard_library_buffer_kind buffer_kind;
    } expected[] = {
        {"std::abs", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::atoi", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::atol", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::atoll", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::fabs", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::floor", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::ceil", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::exp", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::log", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::sin", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::cos", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::tan", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::round", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"cblc::banker_round", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"cblc::date_parse_yyyymmdd", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"cblc::date_parse_yyyymmdd_legacy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"cblc::parse_int", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"cblc::parse_double", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"cblc::date_duration_days", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::strlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strnlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strcpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strncpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::memcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strcat", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strtod", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::pow", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::sqrt", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::fmin", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::fmax", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::toupper", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::tolower", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::isdigit", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::isalpha", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE}
    };

    entries = transpiler_standard_library_get_entries(&count);
    if (!entries)
    {
        std::printf("Assertion failed: catalog should return entry table\n");
        return (FT_FAILURE);
    }
    expected_count = sizeof(expected) / sizeof(expected[0]);
    if (count != expected_count)
    {
        std::printf("Assertion failed: catalog should report %u standard library entries but returned %u\n",
            static_cast<unsigned int>(expected_count),
            static_cast<unsigned int>(count));
        return (FT_FAILURE);
    }
    index = 0;
    while (index < expected_count)
    {
        if (std::strncmp(entries[index].qualified_name, expected[index].qualified_name,
                std::strlen(expected[index].qualified_name) + 1) != 0)
        {
            std::printf("Assertion failed: catalog entry %u should be %s\n",
                static_cast<unsigned int>(index + 1), expected[index].qualified_name);
            return (FT_FAILURE);
        }
        if (entries[index].buffer_kind != expected[index].buffer_kind)
        {
            std::printf("Assertion failed: catalog entry %u should target buffer kind %d but was %d\n",
                static_cast<unsigned int>(index + 1), expected[index].buffer_kind,
                entries[index].buffer_kind);
            return (FT_FAILURE);
        }
        if (!entries[index].public_return_type || !entries[index].public_parameters
            || !entries[index].hidden_abi_outputs || entries[index].interface_version == 0
            || entries[index].abi_version == 0)
        {
            std::printf("Assertion failed: catalog entry %u should have complete public and ABI metadata\n",
                static_cast<unsigned int>(index + 1));
            return (FT_FAILURE);
        }
        if (entries[index].failure_policy != TRANSPILE_STANDARD_LIBRARY_FAILURE_COMPATIBILITY
            && std::strstr(entries[index].public_parameters, "status") != NULL)
        {
            std::printf("Assertion failed: catalog entry %u should not expose a status parameter\n",
                static_cast<unsigned int>(index + 1));
            return (FT_FAILURE);
        }
        if (entries[index].return_kind == TRANSPILE_STANDARD_LIBRARY_RETURN_RESULT_OBJECT
            && std::strcmp(entries[index].public_return_type, "void") == 0)
        {
            std::printf("Assertion failed: result-object entry %u should expose a value type\n",
                static_cast<unsigned int>(index + 1));
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_generators_validate_out_parameter)
{
    if (transpiler_standard_library_generate_atoi(NULL) != FT_FAILURE
        || transpiler_standard_library_generate_strlen(NULL) != FT_FAILURE
        || transpiler_standard_library_generate_strcmp(NULL) != FT_FAILURE
        || transpiler_standard_library_generate_strcpy(NULL) != FT_FAILURE
        || transpiler_standard_library_generate_toupper(NULL) != FT_FAILURE)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_versioned_lookup_pins_interface_and_abi)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup_versioned("std::strlen",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR,
        TRANSPILE_STANDARD_LIBRARY_INTERFACE_VERSION_CURRENT,
        TRANSPILE_STANDARD_LIBRARY_ABI_VERSION_CURRENT);
    if (!entry || entry->buffer_kind != TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR)
        return (FT_FAILURE);
    if (transpiler_standard_library_lookup_versioned("std::strlen",
            TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, 0u,
            TRANSPILE_STANDARD_LIBRARY_ABI_VERSION_CURRENT) != NULL)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_catalog_entries_have_native_cblc_sources)
{
    const t_transpiler_standard_library_entry *entries;
    size_t count;
    size_t index;

    entries = transpiler_standard_library_get_entries(&count);
    if (!entries || count == 0)
    {
        std::printf("Assertion failed: standard-library catalog should not be empty\n");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < count)
    {
        const char *source;
        const char *source_path;
        char *generated;
        char program_marker[128];

        source = transpiler_standard_library_get_native_source(entries[index].program_name);
        source_path = transpiler_standard_library_get_native_source_path(entries[index].program_name);
        if (!source || !std::strstr(source, " F(") || !source_path
            || std::strlen(source_path) < std::strlen(".cblc")
            || std::strcmp(source_path + std::strlen(source_path) - std::strlen(".cblc"), ".cblc") != 0)
        {
            std::printf("Assertion failed: %s should map to a canonical native CBL-C source path\n",
                entries[index].program_name);
            return (FT_FAILURE);
        }
        generated = NULL;
        if (transpiler_standard_library_generate_native(entries[index].program_name,
                &generated) != FT_SUCCESS || !generated)
        {
            if (generated)
                cma_free(generated);
            std::printf("Assertion failed: %s native CBL-C source should regenerate\n",
                entries[index].program_name);
            return (FT_FAILURE);
        }
        if (std::snprintf(program_marker, sizeof(program_marker),
                "PROGRAM-ID. %s.", entries[index].program_name) < 0
            || !std::strstr(generated, program_marker)
            || std::strstr(generated, "DWARF")
            || std::strstr(generated, "__cxa_throw"))
        {
            cma_free(generated);
            std::printf("Assertion failed: %s should emit native COBOL without C++ exception ABI text\n",
                entries[index].program_name);
            return (FT_FAILURE);
        }
        cma_free(generated);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_string_class_generates_from_cblc)
{
    static const char *native_string_methods[] = {
        "int strlen()",
        "int strnlen(int request)",
        "int strcmp(string source)",
        "void strcpy(string source)",
        "void strncpy(string source, int request)",
        "int memcmp(string source, int count)",
        "void strcat(string left, string right)",
        "int capacity()",
        "void reserve(int requested_capacity)",
        "void remove_prefix(int byte_count)",
        "void remove_suffix(int byte_count)",
        "char *to_c_string()",
        "int parse_int()",
        "double parse_double()",
        "void toupper()",
        "void tolower()",
        "void to_upper_ascii()",
        "void to_lower_ascii()"
    };
    const char *string_source;
    const char *errors_source;
    const char *strncpy_body;
    const char *move_constructor;
    const char *move_assignment;
    const char *capacity_constructor;
    const char *length_constructor;
    const char *nul_terminated_constructor;
    const char *parse_int_body;
    const char *is_valid_utf8_body;
    const char *codepoint_length_body;
    const char *codepoint_at_body;
    const char *encode_codepoint_body;
    const char *push_codepoint_body;
    const char *set_codepoint_body;
    const char *set_byte_body;
    const char *resize_default_body;
    const char *resize_fill_body;
    const char *push_back_body;
    const char *pop_back_body;
    const char *resize_fill_reserve;
    const char *resize_fill_commit;
    const char *atoi_body;
    const char *atol_body;
    const char *atoll_body;
    const char *parse_double_body;
    const char *strncpy_reserve;
    const char *strncpy_clear;
    const char *strcat_body;
    const char *strcat_reserve;
    const char *strcat_clear;
    const char *capacity_growth_guard;
    const char *reserve_terminator;
    const char *reserve_release_old;
    char *generated;
    char *legacy_generated;
    size_t method_index;
    int status;

    legacy_generated = NULL;
    if (transpiler_standard_library_get_native_source("CBLC-STRLEN-STRING")
        || transpiler_standard_library_generate_native("CBLC-STRLEN-STRING",
            &legacy_generated) != FT_FAILURE
        || legacy_generated)
    {
        if (legacy_generated)
            cma_free(legacy_generated);
        std::printf("Assertion failed: legacy fixed-buffer string helpers must not be exposed\n");
        return (FT_FAILURE);
    }
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!string_source)
    {
        std::printf("Assertion failed: native string class source should be registered\n");
        return (FT_FAILURE);
    }
    method_index = 0;
    while (method_index < sizeof(native_string_methods) / sizeof(native_string_methods[0]))
    {
        if (!std::strstr(string_source, native_string_methods[method_index]))
        {
            std::printf("Assertion failed: native string class is missing method %s\n",
                native_string_methods[method_index]);
            return (FT_FAILURE);
        }
        method_index += 1;
    }
    strncpy_body = string_source
        ? std::strstr(string_source, "void strncpy(string source, int request)") : NULL;
    move_constructor = string_source
        ? std::strstr(string_source, "string(string *source)") : NULL;
    move_assignment = string_source
        ? std::strstr(string_source, "void move_assign(string *source)") : NULL;
    capacity_constructor = string_source
        ? std::strstr(string_source, "string(int requested_capacity)") : NULL;
    length_constructor = string_source
        ? std::strstr(string_source, "string(char *source, int source_length)") : NULL;
    nul_terminated_constructor = string_source
        ? std::strstr(string_source, "string(char *source)") : NULL;
    parse_int_body = string_source ? std::strstr(string_source, "int parse_int()") : NULL;
    is_valid_utf8_body = string_source
        ? std::strstr(string_source, "int is_valid_utf8()") : NULL;
    codepoint_length_body = string_source
        ? std::strstr(string_source, "int codepoint_length()") : NULL;
    codepoint_at_body = string_source
        ? std::strstr(string_source, "int codepoint_at(int character_index)") : NULL;
    encode_codepoint_body = string_source
        ? std::strstr(string_source, "int encode_codepoint(int scalar, char *encoded)") : NULL;
    push_codepoint_body = string_source
        ? std::strstr(string_source, "void push_codepoint(int scalar)") : NULL;
    set_codepoint_body = string_source
        ? std::strstr(string_source, "void set_codepoint(int character_index, int scalar)") : NULL;
    set_byte_body = string_source
        ? std::strstr(string_source, "void set_byte(int byte_index, char replacement_byte)") : NULL;
    resize_default_body = string_source
        ? std::strstr(string_source, "void resize(int requested)") : NULL;
    resize_fill_body = string_source
        ? std::strstr(string_source, "void resize(int requested, char fill)") : NULL;
    push_back_body = string_source
        ? std::strstr(string_source, "void push_back(char value)") : NULL;
    pop_back_body = string_source
        ? std::strstr(string_source, "void pop_back()") : NULL;
    resize_fill_reserve = resize_fill_body
        ? std::strstr(resize_fill_body, "reserve_storage(requested + 1);") : NULL;
    resize_fill_commit = resize_fill_reserve
        ? std::strstr(resize_fill_reserve, "data[index] = fill;") : NULL;
    atoi_body = string_source ? std::strstr(string_source, "int atoi()") : NULL;
    atol_body = string_source ? std::strstr(string_source, "int atol()") : NULL;
    atoll_body = string_source ? std::strstr(string_source, "int atoll()") : NULL;
    parse_double_body = string_source
        ? std::strstr(string_source, "double parse_double()") : NULL;
    strcat_body = string_source ? std::strstr(string_source, "void strcat(") : NULL;
    strncpy_reserve = strncpy_body ? std::strstr(strncpy_body,
        "reserve_storage(copy_limit + 1);") : NULL;
    strncpy_clear = strncpy_body ? std::strstr(strncpy_body, "clear();") : NULL;
    strcat_reserve = strcat_body ? std::strstr(strcat_body,
        "reserve_storage(left_length + right_length + 1);") : NULL;
    strcat_clear = strcat_body ? std::strstr(strcat_body, "clear();") : NULL;
    capacity_growth_guard = string_source
        ? std::strstr(string_source, "if (new_capacity > 499999999)") : NULL;
    reserve_terminator = string_source
        ? std::strstr(string_source, "replacement[stored_length] = '\\0';") : NULL;
    reserve_release_old = reserve_terminator
        ? std::strstr(reserve_terminator, "delete[] data;") : NULL;
    if (!move_constructor || !move_assignment || !capacity_constructor || !length_constructor
        || !nul_terminated_constructor || !strncpy_body || !strcat_body
        || !parse_int_body || !is_valid_utf8_body
        || !std::strstr(is_valid_utf8_body, "stored_length < 0")
        || !std::strstr(is_valid_utf8_body, "storage_capacity <= stored_length")
        || !std::strstr(is_valid_utf8_body, "data[stored_length] != '\\0'")
        || !std::strstr(is_valid_utf8_body,
            "valid_encoding = is_valid_utf8_bytes(data, stored_length);")
        || !codepoint_length_body || !codepoint_at_body || !encode_codepoint_body
        || !push_codepoint_body || !set_codepoint_body || !set_byte_body
        || !resize_default_body || !resize_fill_body
        || !push_back_body || !pop_back_body
        || !atoi_body || !atol_body
        || !atoll_body || !parse_double_body
        || std::strstr(string_source, "double strtod()")
        || !std::strstr(string_source, "int is_valid_utf8_bytes(char *source, int source_length)")
        || !std::strstr(string_source, "int is_valid_utf8()")
        || std::strstr(codepoint_length_body, "valid_encoding = is_valid_utf8();")
        || !std::strstr(codepoint_length_body, "while (index < stored_length)")
        || !std::strstr(codepoint_at_body, "if (character_index < 0)")
        || std::strstr(codepoint_at_body, "valid_encoding = is_valid_utf8();")
        || !std::strstr(codepoint_at_body, "out_of_range error;")
        || !std::strstr(codepoint_at_body, "result = first - 240;")
        || !std::strstr(codepoint_at_body, "result = result + fourth;")
        || !std::strstr(encode_codepoint_body, "scalar > 1114111")
        || !std::strstr(encode_codepoint_body, "scalar >= 55296")
        || !std::strstr(encode_codepoint_body, "scalar <= 57343")
        || !std::strstr(encode_codepoint_body, "encoded[0] = std::chr(scalar);")
        || !std::strstr(encode_codepoint_body, "encoded[3] = std::chr(byte_value);")
        || !std::strstr(push_codepoint_body, "encoded_length = encode_codepoint(scalar, encoded);")
        || !std::strstr(set_codepoint_body, "replacement[replacement_length] = '\\0';")
        || !std::strstr(set_codepoint_body, "delete[] data;")
        || !std::strstr(set_codepoint_body, "data = replacement;")
        || !std::strstr(set_byte_body, "if (byte_index < 0 || byte_index >= stored_length)")
        || !std::strstr(set_byte_body, "replacement[index] = replacement_byte;")
        || !std::strstr(set_byte_body,
            "valid_encoding = is_valid_utf8_bytes(replacement, stored_length);")
        || !std::strstr(set_byte_body, "if (valid_encoding == 0)")
        || !std::strstr(set_byte_body, "invalid_encoding error;")
        || !std::strstr(set_byte_body, "delete[] replacement;")
        || !std::strstr(set_byte_body, "delete[] data;")
        || !std::strstr(set_byte_body, "data = replacement;")
        || !std::strstr(resize_default_body, "invalid_argument error;")
        || !std::strstr(resize_default_body, "length_error error;")
        || !std::strstr(resize_default_body, "std::ord(boundary) - 1 <= 191")
        || !std::strstr(resize_fill_body, "fill_value > 127")
        || !std::strstr(resize_fill_body, "if (fill_value <= 191)")
        || !resize_fill_reserve || !resize_fill_commit
        || resize_fill_commit < resize_fill_reserve
        || !std::strstr(push_back_body, "if (byte_value > 127)")
        || !std::strstr(push_back_body, "invalid_encoding error;")
        || !std::strstr(push_back_body, "reserve_storage(stored_length + 2);")
        || !std::strstr(pop_back_body, "if (stored_length <= 0)")
        || !std::strstr(pop_back_body, "while (index > 0 && byte_value >= 128 && byte_value <= 191)")
        || !errors_source || !std::strstr(errors_source, "class invalid_encoding")
        || !std::strstr(string_source, "first == 224")
        || !std::strstr(string_source, "second_minimum = 160")
        || !std::strstr(string_source, "first == 237")
        || !std::strstr(string_source, "second_maximum = 159")
        || !std::strstr(string_source, "first == 240")
        || !std::strstr(string_source, "second_minimum = 144")
        || !std::strstr(string_source, "first == 244")
        || !std::strstr(string_source, "second_maximum = 143")
        || !std::strstr(nul_terminated_constructor,
            "valid_encoding = is_valid_utf8_bytes(source, source_length);")
        || !std::strstr(nul_terminated_constructor, "if (valid_encoding == 0)")
        || !std::strstr(length_constructor,
            "valid_encoding = is_valid_utf8_bytes(source, source_length);")
        || !std::strstr(length_constructor, "if (valid_encoding == 0)")
        || !std::strstr(length_constructor, "invalid_encoding error;")
        || !std::strstr(nul_terminated_constructor, "invalid_encoding error;")
        || !std::strstr(errors_source, "class parse_error")
        || !std::strstr(parse_int_body, "parse_error error;")
        || !std::strstr(parse_int_body, "range_error error;")
        || !std::strstr(parse_int_body, "error.code = 2;")
        || !std::strstr(parse_int_body,
            "digit = std::ord(current) - std::ord('0');")
        || !std::strstr(atoi_body, "result = parse_int();")
        || !std::strstr(atol_body, "result = parse_int();")
        || !std::strstr(atoll_body, "result = parse_int();")
        || !std::strstr(parse_double_body, "parse_error error;")
        || !std::strstr(parse_double_body,
            "digit = std::ord(current) - std::ord('0');")
        || !std::strstr(parse_double_body, "throw error;")
        || !std::strstr(strncpy_body, "error.code = 2;")
        || !std::strstr(move_constructor, "if (source == NULL)")
        || !std::strstr(move_assignment,
            "if (source == NULL || source->data == data)")
        || !std::strstr(move_assignment, "delete[] data;")
        || !std::strstr(move_assignment, "data = source->data;")
        || !std::strstr(move_assignment, "source->data = NULL;")
        || !std::strstr(move_assignment, "source->stored_length = 0;")
        || !std::strstr(move_assignment, "source->storage_capacity = 0;")
        || !std::strstr(capacity_constructor, "if (requested_capacity < 0)")
        || !std::strstr(capacity_constructor, "requested_capacity > 999999998")
        || !std::strstr(capacity_constructor,
            "storage_capacity = requested_capacity + 1;")
        || !std::strstr(string_source, "return storage_capacity - 1;")
        || !std::strstr(string_source,
            "reserve_storage(requested_capacity + 1);")
        || !std::strstr(length_constructor, "if (source_length < 0)")
        || !std::strstr(length_constructor, "source == NULL && source_length > 0")
        || !std::strstr(length_constructor, "source_length > 999999998")
        || !std::strstr(string_source, "requested_capacity > 999999999")
        || !strncpy_reserve || !strncpy_clear
        || strncpy_clear < strncpy_reserve || !strcat_reserve
        || !strcat_clear || strcat_clear < strcat_reserve || !capacity_growth_guard
        || !std::strstr(string_source,
            "source_length > 999999998 - old_length")
        || !std::strstr(strcat_body,
            "right_length > 999999998 - left_length")
        || !reserve_terminator || !reserve_release_old)
    {
        std::printf("Assertion failed: string growth must preserve its terminator and reserve before clearing existing contents\n");
        return (FT_FAILURE);
    }
    generated = NULL;
    status = transpiler_standard_library_generate_native("CBLC-STRING", &generated);
    if (status != FT_SUCCESS || !generated)
    {
        if (generated)
            cma_free(generated);
        std::printf("Assertion failed: owning string class should generate native COBOL from CBL-C\n");
        return (FT_FAILURE);
    }
    if (!std::strstr(generated, "CBLC-STRING")
        || !std::strstr(generated, "IS-VALID-UTF8")
        || !std::strstr(generated, "CODEPOINT-LENGTH")
        || !std::strstr(generated, "CODEPOINT-AT")
        || !std::strstr(generated, "REMOVE-PREFIX")
        || !std::strstr(generated, "REMOVE-SUFFIX")
        || std::strstr(generated, "STRING-VALUE") )
    {
        cma_free(generated);
        std::printf("Assertion failed: generated string class should use its native class layout\n");
        return (FT_FAILURE);
    }
    cma_free(generated);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_string_codepoint_at_uses_checked_native_method)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "int point;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    point = text.codepoint_at(0);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string codepoint access source should parse") != FT_SUCCESS)
    {
        std::printf("String codepoint access parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string codepoint access should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "PERFORM CBLC-METHOD-MAIN-TEXT-CODEPOINT-AT")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: codepoint_at must lower to the owning string method and typed bounds exception path\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_assign_raw_bytes_is_transactional)
{
    const char *source;
    const char *method;
    const char *validate_encoding;
    const char *allocate_replacement;
    const char *release_previous;
    const char *errors_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = transpiler_standard_library_get_native_source("CBLC-STRING");
    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    method = source ? std::strstr(source,
        "void assign(char *source, int source_length)") : NULL;
    validate_encoding = method ? std::strstr(method,
        "valid_encoding = is_valid_utf8_bytes(source, source_length);") : NULL;
    allocate_replacement = method ? std::strstr(method,
        "replacement = new char[source_length + 1];") : NULL;
    release_previous = method ? std::strstr(method, "delete[] data;") : NULL;
    if (!method || !validate_encoding || !allocate_replacement
        || !release_previous || validate_encoding > allocate_replacement
        || allocate_replacement > release_previous
        || !std::strstr(method, "if (source_length < 0)")
        || !std::strstr(method, "if (source == NULL && source_length > 0)")
        || !std::strstr(method, "if (source_length > 999999998)"))
    {
        std::printf("Assertion failed: raw-byte string assignment must validate length/UTF-8 and stage replacement storage before releasing the old value\n");
        return (FT_FAILURE);
    }
    if (!errors_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + source
        + "\nvoid main() { char *bytes; int byte_count; string text; "
        "text.assign(bytes, byte_count); return; }\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(
                combined_source.c_str(), &unit),
            "raw-byte string assignment should parse as a class method")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "raw-byte string assignment should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated,
            "PERFORM CBLC-METHOD-MAIN-TEXT-ASSIGN")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: raw-byte assignment must lower to the owning class method and its typed exception path\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_ascii_case_insensitive_compare)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "int matches;\n"
        "void main() {\n"
        "    string left(3);\n"
        "    string right(3);\n"
        "    matches = left.equals_ignore_case_ascii(right);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "ASCII case-insensitive comparison source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "ASCII case-insensitive comparison should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "CBLC-METHOD-MAIN-LEFT-EQUALS-IGNORE-CASE-ASCII")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: equals_ignore_case_ascii should lower as a native byte-wise string method\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_ascii_named_mutators_resolve)
{
    const char *source =
        "void main() {\n"
        "    string original(\"MiXeD\");\n"
        "    original.to_upper_ascii();\n"
        "    original.to_lower_ascii();\n"
        "}\n";
    t_cblc_translation_unit unit;
    char *generated = NULL;
    const char *string_native =
        transpiler_standard_library_get_native_source("CBLC-STRING");
    const char *upper_mutator = string_native
        ? std::strstr(string_native, "void to_upper_ascii()") : NULL;
    const char *lower_mutator = string_native
        ? std::strstr(string_native, "void to_lower_ascii()") : NULL;
    int status = FT_FAILURE;
    int diagnostic_code = 0;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX] = {};

    if (!upper_mutator || !lower_mutator
        || !std::strstr(upper_mutator, "toupper();")
        || !std::strstr(lower_mutator, "tolower();"))
    {
        std::printf("ASCII-named mutators must delegate to the established ASCII implementation\n");
        return (FT_FAILURE);
    }
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("ASCII case-mutator source parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated,
            &diagnostic_code, diagnostic_message,
            sizeof(diagnostic_message)) != FT_SUCCESS || !generated)
    {
        std::printf("ASCII case-mutator COBOL generation failed (%d): %s\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    if (!std::strstr(generated, "TO-UPPER-ASCII")
        || !std::strstr(generated, "TO-LOWER-ASCII"))
    {
        std::printf("ASCII case mutators were not emitted as resolved native class calls\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_compare_uses_numeric_utf8_byte_order)
{
    const char *string_source;
    const char *compare_body;
    const char *contains_body;
    const char *left_ordinal;
    const char *right_ordinal;

    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    compare_body = string_source ? std::strstr(string_source,
        "int compare(string source)") : NULL;
    contains_body = compare_body ? std::strstr(compare_body,
        "int contains(string source)") : NULL;
    left_ordinal = compare_body ? std::strstr(compare_body,
        "std::ord(data[index]) - 1") : NULL;
    right_ordinal = left_ordinal ? std::strstr(left_ordinal + 1,
        "std::ord(source.data[index]) - 1") : NULL;
    if (!compare_body || !contains_body || !left_ordinal || !right_ordinal
        || left_ordinal >= contains_body || right_ordinal >= contains_body
        || (std::strstr(compare_body, "data[index] < current")
            && std::strstr(compare_body, "data[index] < current") < contains_body)
        || (std::strstr(compare_body, "data[index] > current")
            && std::strstr(compare_body, "data[index] > current") < contains_body))
    {
        std::printf("Assertion failed: string::compare must order UTF-8 bytes by numeric ordinal, not the host COBOL collating sequence\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_string_ascii_trim_methods_generate)
{
    const char *source;
    const char *string_source;
    const char *trim_end_body;
    const char *null_guard;
    const char *terminator_write;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "void main() {\n"
        "    string text(9);\n"
        "    text.trim_ascii();\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "ASCII trim source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "ASCII trim methods should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    trim_end_body = string_source ? std::strstr(string_source,
        "void trim_end_ascii()") : NULL;
    null_guard = trim_end_body ? std::strstr(trim_end_body,
        "if (data == NULL)") : NULL;
    terminator_write = trim_end_body ? std::strstr(trim_end_body,
        "data[stored_length] = '\\0';") : NULL;
    if (!generated
        || !trim_end_body || !null_guard || !terminator_write
        || null_guard >= terminator_write
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-TRIM-ASCII")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-TRIM-START-ASCII")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-TRIM-END-ASCII"))
    {
        std::printf("Assertion failed: ASCII trim must lower through the native String class methods\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_replace_all_is_transactional)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    const char *string_source;
    const char *replace_all_body;
    const char *allocation;
    const char *commit;
    int status;

    source = "void main() {\n"
        "    string text(7);\n"
        "    string old_text(3);\n"
        "    string new_text(3);\n"
        "    text.replace_all(old_text, new_text);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "replace_all source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "replace_all should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    replace_all_body = string_source ? std::strstr(string_source,
        "void replace_all(string old_text, string new_text)") : NULL;
    allocation = replace_all_body
        ? std::strstr(replace_all_body, "replacement = new char[result_length + 1]") : NULL;
    commit = allocation ? std::strstr(allocation, "delete[] data") : NULL;
    if (!generated || !replace_all_body || !allocation || !commit
        || !std::strstr(replace_all_body, "if (old_length == 0)")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-REPLACE-ALL")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: replace_all must reject an empty pattern, stage output, and publish only after construction\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_at_returns_checked_char_value)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "char value;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    value = text.at(0);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string byte access source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string char-returning at() should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-AT")
        || !std::strstr(generated, "PIC X VALUE SPACE")
        || !std::strstr(generated, "CBLC-EX-RAISING")
        || !std::strstr(generated, "MOVE CBLC-PTR-CHAR TO CBLC-RESULT-MAIN-TEXT-AT"))
    {
        std::printf("Assertion failed: string::at must return a checked char by value through native COBOL storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_shrink_to_fit_uses_native_lifecycle)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "int initial_capacity;\n"
        "int compacted_capacity;\n"
        "void main() {\n"
        "    string text(64);\n"
        "    initial_capacity = text.capacity();\n"
        "    text.shrink_to_fit();\n"
        "    compacted_capacity = text.capacity();\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string shrink_to_fit source should parse") != FT_SUCCESS)
    {
        std::printf("String shrink_to_fit parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string shrink_to_fit should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated,
            "CBLC-METHOD-MAIN-TEXT-SHRINK-TO-FIT")
        || !std::strstr(generated, "ALLOCATE")
        || !std::strstr(generated, "FREE")
        || !std::strstr(generated, "STORAGE-CAPACITY"))
    {
        std::printf("Assertion failed: string::shrink_to_fit must lower to native allocation/copy/release behavior\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_copy_to_is_bounded_and_typed)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "char *bytes;\n"
        "char *null_bytes;\n"
        "int copied;\n"
        "void main() {\n"
        "    string text(\"abc\");\n"
        "    bytes = new char[4];\n"
        "    null_bytes = NULL;\n"
        "    bytes[0] = '!';\n"
        "    try { copied = text.copy_to(bytes, 2); }\n"
        "    catch (const length_error& error) { copied = 0; }\n"
        "    try { copied = text.copy_to(null_bytes, 3); }\n"
        "    catch (const invalid_argument& error) { copied = 0; }\n"
        "    try { copied = text.copy_to(bytes, -1); }\n"
        "    catch (const invalid_argument& error) { copied = 0; }\n"
        "    copied = text.copy_to(bytes, 3);\n"
        "    delete[] bytes;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "bounded string copy source should parse") != FT_SUCCESS)
    {
        std::printf("String copy_to parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "bounded string copy should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-COPY-TO")
        || !std::strstr(generated, "CBLC-EX-PAYLOAD-LENGTH-ERROR")
        || !std::strstr(generated, "CBLC-EX-PAYLOAD-INVALID-ARGUMENT")
        || !std::strstr(generated, "MOVE CBLC-USER-STRING-COPY-TO-"))
    {
        std::printf("Assertion failed: string::copy_to must return its byte count and lower both typed failure paths\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_substr_returns_owned_string_value)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "int matched;\n"
        "int rejected;\n"
        "void main() {\n"
        "    string text(\"hello\");\n"
        "    string multibyte(\"éx\");\n"
        "    string expected(\"ell\");\n"
        "    string slice;\n"
        "    slice = text.substr(1, 3);\n"
        "    matched = slice.equals(expected);\n"
        "    try { slice = text.substr(-1, 1); }\n"
        "    catch (const out_of_range& error) { rejected = 1; }\n"
        "    try { slice = text.substr(1, 5); }\n"
        "    catch (const out_of_range& error) { rejected = rejected + 1; }\n"
        "    try { slice = multibyte.substr(1, 1); }\n"
        "    catch (const invalid_argument& error) { rejected = rejected + 1; }\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string substr source should parse") != FT_SUCCESS)
    {
        std::printf("String substr parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string substr should return a class value through native COBOL")
        != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-SUBSTR")
        || !std::strstr(generated, "CBLC-RESULT-MAIN-TEXT-SUBSTR")
        || !std::strstr(generated, "CBLC-EX-PAYLOAD-OUT-OF-RANGE")
        || !std::strstr(generated, "CBLC-EX-PAYLOAD-INVALID-ARGUMENT"))
    {
        std::printf("Assertion failed: string::substr must return an owned string and lower range failures\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_operator_plus_assign_dispatches_natively)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "int matched;\n"
        "void main() {\n"
        "    string text(\"alpha\");\n"
        "    string suffix(\"beta\");\n"
        "    string expected(\"alphabeta\");\n"
        "    text += suffix;\n"
        "    matched = text.equals(expected);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string operator+= source should parse") != FT_SUCCESS)
    {
        std::printf("String operator+= parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("String operator+= generation diagnostic: code=%d message=%s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated
        || !std::strstr(generated,
            "CBLC-METHOD-MAIN-TEXT-OPERATOR-PLUS-ASSIGN")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-APPEND")
        || !std::strstr(generated,
            "CBLC-USER-STRING-OPERATOR-PLUS-ASSIGN-SOURCE")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: string += must emit operator+= and nested append methods with the source parameter and exception path\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_operator_plus_returns_owned_value)
{
    const char *errors_source;
    const char *string_source;
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int diagnostic_code;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    source = "void main() {\n"
        "    string left(\"alpha\");\n"
        "    string right(\"beta\");\n"
        "    string suffix(\"gamma\");\n"
        "    string delta(\"delta\");\n"
        "    string tail(\"epsilon\");\n"
        "    string joined;\n"
        "    joined = left + right + suffix + delta + tail;\n"
        "}\n";
    if (!errors_source || !string_source)
        return (FT_FAILURE);
    std::string combined_source = std::string(errors_source) + "\n"
        + string_source + "\n" + source;
    cblc_translation_unit_init(&unit);
    generated = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(
                combined_source.c_str(), &unit),
            "string operator+ source should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.entry_function_index >= unit.function_count)
    {
        std::printf("Assertion failed: string operator+ fixture should resolve its entry function\n");
        goto cleanup;
    }
    {
        const t_cblc_function *entry = &unit.functions[unit.entry_function_index];
        size_t operator_plus_call_count = 0;
        size_t operator_plus_call_indices[4] = {0, 0, 0, 0};
        char operator_plus_result_targets[4][TRANSPILE_IDENTIFIER_MAX] = {{0}};
        size_t final_assignment_index = static_cast<size_t>(-1);
        size_t reverse_cleanup_index = static_cast<size_t>(-1);

        for (size_t index = 0; index < entry->statement_count; ++index)
        {
            if (entry->statements[index].type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(entry->statements[index].call_identifier,
                    "operator+") == 0)
            {
                if (operator_plus_call_count < 4)
                {
                    operator_plus_call_indices[operator_plus_call_count] = index;
                    ft_strlcpy(operator_plus_result_targets[operator_plus_call_count],
                        entry->statements[index].target,
                        sizeof(operator_plus_result_targets[operator_plus_call_count]));
                }
                operator_plus_call_count += 1;
            }
        }
        if (operator_plus_call_count != 4)
        {
            std::printf("Assertion failed: a + b + c + d + e should lower to four ordered owned method-call results (found %zu)\n",
                operator_plus_call_count);
            goto cleanup;
        }
        for (size_t index = operator_plus_call_indices[3] + 1;
            index < entry->statement_count; ++index)
            if (entry->statements[index].type == CBLC_STATEMENT_ASSIGNMENT
                && std::strcmp(entry->statements[index].source,
                    operator_plus_result_targets[3]) == 0)
            {
                final_assignment_index = index;
                break ;
            }
        if (operator_plus_call_indices[0] >= operator_plus_call_indices[1]
            || operator_plus_call_indices[1] >= operator_plus_call_indices[2]
            || operator_plus_call_indices[2] >= operator_plus_call_indices[3]
            || final_assignment_index == static_cast<size_t>(-1))
        {
            std::printf("Assertion failed: chained addition should preserve left-to-right evaluation and assign only after the final result exists\n");
            goto cleanup;
        }
        for (size_t cleanup_slot = 4; cleanup_slot > 0; --cleanup_slot)
        {
            size_t found_cleanup_index = static_cast<size_t>(-1);

            for (size_t index = reverse_cleanup_index == static_cast<size_t>(-1)
                    ? final_assignment_index + 1 : reverse_cleanup_index + 1;
                index < entry->statement_count; ++index)
                if (entry->statements[index].type == CBLC_STATEMENT_DESTRUCT
                    && std::strcmp(entry->statements[index].target,
                        operator_plus_result_targets[cleanup_slot - 1]) == 0)
                {
                    found_cleanup_index = index;
                    break ;
                }
            if (found_cleanup_index == static_cast<size_t>(-1))
            {
                std::printf("Assertion failed: chained addition temporary %zu must be destroyed after assignment in reverse order\n",
                    cleanup_slot - 1);
                goto cleanup;
            }
            reverse_cleanup_index = found_cleanup_index;
        }
        if (reverse_cleanup_index <= final_assignment_index)
        {
            std::printf("Assertion failed: chained addition temporaries must be destroyed in reverse construction order\n");
            goto cleanup;
        }
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("String operator+ generation diagnostic: code=%d message=%s\n",
            diagnostic_code, diagnostic);
        goto cleanup;
    }
    if (!generated
        || !std::strstr(generated, "CBLC-USER-STRING-OPERATOR-PLUS")
        || !std::strstr(generated, "CBLC-USER-STRING-OPERATOR-PLUS-SOURCE")
        || !std::strstr(generated, "CBLC-USER-STRING-OPERATOR-PLUS-RESULT"))
    {
        std::printf("Assertion failed: string operator+ must emit its CBL-C body, owned result storage, and parameter layout\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_operator_plus_reuses_owned_result_storage)
{
    const char *string_source;
    const char *operator_plus;
    const char *next_method;
    const char *reserve_result;
    const char *allocate_result;

    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    operator_plus = string_source
        ? std::strstr(string_source, "string operator+(string source)") : NULL;
    next_method = operator_plus
        ? std::strstr(operator_plus, "void assign(string source)") : NULL;
    reserve_result = operator_plus
        ? std::strstr(operator_plus, "result.reserve_storage(result_length + 1);") : NULL;
    allocate_result = operator_plus
        ? std::strstr(operator_plus, "result.data = new char[") : NULL;

    if (!string_source || !operator_plus || !next_method || !reserve_result
        || reserve_result >= next_method
        || (allocate_result && allocate_result < next_method))
    {
        std::printf("Assertion failed: string operator+ must reuse the default-constructed result buffer instead of overwriting and leaking its owned allocation\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_string_to_c_string_returns_owned_copy)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    char validation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    source = "void main() {\n"
        "    char *legacy_text;\n"
        "    string text(\"portable\");\n"
        "    legacy_text = text.to_c_string();\n"
        "    delete[] legacy_text;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string::to_c_string source should parse") != FT_SUCCESS)
    {
        std::printf("string::to_c_string parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string::to_c_string should lower through native COBOL") != FT_SUCCESS)
        goto cleanup;
    validation_diagnostic[0] = '\0';
    if (!generated
        || transpiler_validate_generated_cobol_with_diagnostic(generated,
            validation_diagnostic, sizeof(validation_diagnostic)) != FT_SUCCESS
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-TO-C-STRING")
        || !std::strstr(generated, "SET MAIN-LEGACY-TEXT")
        || !std::strstr(generated, "FREE MAIN-LEGACY-TEXT"))
    {
        std::printf("Assertion failed: to_c_string must generate valid COBOL and return an independently owned char[] copy (%s)\n",
            validation_diagnostic);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_front_back_return_checked_char_values)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "char first;\n"
        "char last;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    first = text.front();\n"
        "    last = text.back();\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string front/back source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string front/back should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-FRONT")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-BACK")
        || !std::strstr(generated, "CBLC-EX-RAISING")
        || !std::strstr(generated, "MOVE CBLC-RESULT-MAIN-TEXT-FRONT TO FIRST")
        || !std::strstr(generated, "MOVE CBLC-RESULT-MAIN-TEXT-BACK TO LAST"))
    {
        std::printf("Assertion failed: string front/back must lower as checked char-by-value methods\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_insert_erase_replace_use_utf8_boundaries)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    const char *string_source;
    const char *assign_body;
    const char *insert_body;
    const char *erase_body;
    const char *zero_count_guard;
    const char *erase_storage_access;
    const char *replace_body;
    const char *remove_prefix_body;
    const char *remove_suffix_body;
    int status;

    source = "void main() {\n"
        "    string text(2);\n"
        "    string replacement(1);\n"
        "    text.assign(replacement);\n"
        "    text.assign(text);\n"
        "    text.insert(1, replacement);\n"
        "    text.erase(0, 1);\n"
        "    text.replace(0, 1, replacement);\n"
        "    text.insert(0, text);\n"
        "    text.replace(0, 1, text);\n"
        "    text.remove_prefix(1);\n"
        "    text.remove_suffix(1);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string insertion and replacement source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string insertion and replacement should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    assign_body = string_source ? std::strstr(string_source,
        "void assign(string source)") : NULL;
    insert_body = string_source ? std::strstr(string_source,
        "void insert(int byte_position, string source)") : NULL;
    erase_body = string_source ? std::strstr(string_source,
        "void erase(int byte_position, int byte_count)") : NULL;
    zero_count_guard = erase_body ? std::strstr(erase_body,
        "if (byte_count == 0)") : NULL;
    erase_storage_access = erase_body ? std::strstr(erase_body,
        "source_index = end_position;") : NULL;
    replace_body = string_source ? std::strstr(string_source,
        "void replace(int byte_position, int byte_count, string source)") : NULL;
    remove_prefix_body = string_source ? std::strstr(string_source,
        "void remove_prefix(int byte_count)") : NULL;
    remove_suffix_body = string_source ? std::strstr(string_source,
        "void remove_suffix(int byte_count)") : NULL;
    if (!generated || !assign_body || !insert_body || !erase_body
        || !zero_count_guard || !erase_storage_access
        || zero_count_guard >= erase_storage_access || !replace_body
        || !remove_prefix_body || !remove_suffix_body
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-INSERT")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-ASSIGN")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-ERASE")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-REPLACE")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-REMOVE-PREFIX")
        || !std::strstr(generated, "CBLC-METHOD-MAIN-TEXT-REMOVE-SUFFIX")
        || !std::strstr(generated, "CBLC-EX-RAISING")
        || !std::strstr(assign_body, "replacement = new char[source_length + 1]")
        || !std::strstr(assign_body, "delete[] data")
        || !std::strstr(insert_body, "byte_value >= 128")
        || !std::strstr(erase_body, "byte_value <= 191")
        || !std::strstr(replace_body, "byte_value >= 128")
        || !std::strstr(remove_prefix_body, "byte_value <= 191")
        || !std::strstr(remove_suffix_body, "byte_value >= 128"))
    {
        std::printf("Assertion failed: string mutation must validate UTF-8 boundaries and emit typed failure paths\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_c_backend_preserves_char_return_type)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "class CharacterValue {\n"
        "private:\n"
        "    char stored;\n"
        "public:\n"
        "    char get() { return stored; }\n"
        "};\n"
        "CharacterValue object;\n"
        "char result;\n"
        "void main() { result = object.get(); }\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "char-returning method source should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated),
            "C backend should generate char-returning methods") != FT_SUCCESS)
        goto cleanup;
    if (!generated || !std::strstr(generated, "char cblc_method_CharacterValue_get"))
    {
        std::printf("Assertion failed: C backend must preserve char method return types\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_set_byte_uses_checked_native_method)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "char replacement;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    replacement = 'B';\n"
        "    text.set_byte(0, replacement);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string byte mutation source should parse") != FT_SUCCESS)
    {
        std::printf("String byte mutation parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string byte mutation should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "PERFORM CBLC-METHOD-MAIN-TEXT-SET-BYTE")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: set_byte must lower to the owning string method and typed error path\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_resize_uses_checked_native_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "char fill;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    fill = 'x';\n"
        "    text.resize(3, fill);\n"
        "    text.resize(1);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string resize source should parse") != FT_SUCCESS)
    {
        std::printf("String resize parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string resize overloads should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "RESIZE")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: string resize overloads must lower to native methods with exception dispatch\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_push_pop_back_use_checked_native_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "char value;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    value = 'B';\n"
        "    text.push_back(value);\n"
        "    text.pop_back();\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string push/pop source should parse") != FT_SUCCESS)
    {
        std::printf("String push/pop parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string push/pop should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "PUSH-BACK")
        || !std::strstr(generated, "POP-BACK")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: string push/pop must lower to native methods with checked exception paths\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_std_chr_converts_character_ordinal_to_char_storage)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "int ordinal;\n"
        "char value;\n"
        "char *bytes;\n"
        "void main() {\n"
        "    ordinal = 66;\n"
        "    value = std::chr(ordinal);\n"
        "    bytes = new char[1];\n"
        "    bytes[0] = std::chr(ordinal);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "std::chr assignments should parse") != FT_SUCCESS)
    {
        std::printf("std::chr parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "std::chr should generate portable COBOL character conversion")
        != FT_SUCCESS)
        goto cleanup;
    if (!generated || !std::strstr(generated, "FUNCTION CHAR(")
        || !std::strstr(generated, "+ 1)") )
    {
        std::printf("Assertion failed: std::chr must lower as the one-based inverse of FUNCTION ORD for scalar and indexed-char destinations\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_codepoint_mutation_uses_native_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated;
    int status;

    source = "int scalar;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    scalar = 128512;\n"
        "    text.push_codepoint(scalar);\n"
        "    text.set_codepoint(0, scalar);\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string codepoint mutation source should parse") != FT_SUCCESS)
    {
        std::printf("String codepoint mutation parse diagnostic: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated),
            "string codepoint mutation should generate native COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated
        || !std::strstr(generated, "PUSH-CODEPOINT")
        || !std::strstr(generated, "SET-CODEPOINT")
        || !std::strstr(generated, "CBLC-EX-RAISING"))
    {
        std::printf("Assertion failed: UTF-8 codepoint mutation markers missing (generated=%d push=%d set=%d exception=%d)\n",
            generated != NULL,
            generated && std::strstr(generated, "PUSH-CODEPOINT") != NULL,
            generated && std::strstr(generated, "SET-CODEPOINT") != NULL,
            generated && std::strstr(generated, "CBLC-EX-RAISING") != NULL);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_standard_library_string_push_codepoint_compiles_and_executes)
{
    t_cblc_translation_unit unit;
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char output[256];
    char *generated;
    const char *source;
    size_t output_offset;
    int status;

    FT_REQUIRE_COBC();
    source = "int scalar;\nint byte_count;\nint codepoint_value;\n"
        "void main() {\n"
        "    string text(1);\n"
        "    scalar = 233;\n"
        "    text.push_codepoint(scalar);\n"
        "    byte_count = text.byte_length();\n"
        "    codepoint_value = text.codepoint_at(0);\n"
        "    display(byte_count);\n"
        "    display(codepoint_value);\n"
        "    display(\"STRING CODEPOINT PASS\");\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("string push_codepoint parse failed: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (cblc_generate_cobol(&unit, &generated) != FT_SUCCESS)
    {
        std::printf("string push_codepoint COBOL emission failed\n");
        goto cleanup;
    }
    if (transpiler_validate_generated_cobol(generated) != FT_SUCCESS)
    {
        std::printf("string push_codepoint generated COBOL failed structural validation\n");
        goto cleanup;
    }
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS
        || test_join_path(directory, "string_codepoint.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "string_codepoint.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, "string_codepoint.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS
        || test_join_path(directory, "string_codepoint_compile.txt",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS
        || test_write_text_file(source_path, generated) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "GnuCOBOL should compile string push_codepoint lowering")
        != FT_SUCCESS
        || execute_binary(directory, "string_codepoint.bin",
            "string_codepoint.txt",
            "string push_codepoint program should execute") != FT_SUCCESS
        || test_read_text_file(output_path, output, sizeof(output)) != FT_SUCCESS)
        goto cleanup;
    output_offset = 0;
    if (test_expect_transcript_double_line(output, &output_offset, 2,
            g_default_float_tolerance,
            "UTF-8 push_codepoint should append two bytes") != FT_SUCCESS
        || test_expect_transcript_double_line(output, &output_offset, 233,
            g_default_float_tolerance,
            "UTF-8 push_codepoint should preserve its scalar") != FT_SUCCESS
        || test_expect_transcript_status_line(output, &output_offset,
            "STRING CODEPOINT PASS",
            "string codepoint runtime marker") != FT_SUCCESS
        || test_expect_transcript_complete(output, output_offset,
            "string codepoint runtime should have no trailing output")
            != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated)
        cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    if (directory[0] != '\0')
        test_remove_directory(directory);
    return (status);
}

FT_TEST(test_standard_library_vector_template_is_embedded_as_cblc)
{
    const char *source;

    source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    if (!source || !std::strstr(source, "template <typename T>")
        || !std::strstr(source, "class vector")
        || !std::strstr(source, "T *elements")
        || !std::strstr(source, "T *relocation")
        || !std::strstr(source, "int count")
        || !std::strstr(source, "int reserved")
        || !std::strstr(source, "int destroy_index")
        || !std::strstr(source, "void reallocate(int requested)\n    {\n        T *relocation;\n        int relocation_index;")
        || std::strstr(source, "int reserved;\n    int relocation_index;")
        || !std::strstr(source, "vector(vector source)")
        || !std::strstr(source, "void operator=(vector source)")
        || !std::strstr(source, "T *operator[](int index)")
        || !std::strstr(source, "T *operator->()")
        || !std::strstr(source, "~vector()")
        || !std::strstr(source, "int size()")
        || !std::strstr(source, "int capacity()")
        || !std::strstr(source, "int max_size()")
        || !std::strstr(source, "int empty()")
        || !std::strstr(source, "void reserve(int requested)")
        || !std::strstr(source, "void shrink_to_fit()")
        || !std::strstr(source, "void resize(int requested)")
        || !std::strstr(source, "void resize(int requested, T value)")
        || !std::strstr(source, "void push_back(T value)")
        || !std::strstr(source, "void pop_back()")
        || !std::strstr(source, "void clear()")
        || !std::strstr(source, "void assign(int requested, T value)")
        || !std::strstr(source, "T *at(int index)")
        || !std::strstr(source, "T *front()")
        || !std::strstr(source, "T *back()")
        || !std::strstr(source, "T at_value(int index)")
        || !std::strstr(source, "T front_value()")
        || !std::strstr(source, "T back_value()")
        || !std::strstr(source, "void emplace_back(T value)")
        || !std::strstr(source, "void insert(int index, T value)")
        || !std::strstr(source, "void erase(int index)")
        || !std::strstr(source, "out_of_range error")
        || !std::strstr(source, "error.code = 1")
        || !std::strstr(source, "throw error")
        || !std::strstr(source, "delete[] elements")
        || std::strstr(source, "malloc")
        || std::strstr(source, "free"))
    {
        std::printf("Assertion failed: CBLC-VECTOR should expose the native CBL-C vector template\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

