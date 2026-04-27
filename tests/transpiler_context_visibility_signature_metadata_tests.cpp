#include "test_suites.hpp"

FT_TEST(test_cblc_import_type_stubs_preserve_const_field_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.fields = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(1, sizeof(*signature.fields)));
    if (!signature.fields)
        goto cleanup;
    ft_strlcpy(signature.name, "Ledger", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 1;
    ft_strlcpy(signature.fields[0].name, "limit", sizeof(signature.fields[0].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].is_const = 1;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod",
                "ledger_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "ledger_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[0].is_const, 1,
            "imported field should preserve const metadata") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.fields)
        cma_free(signature.fields);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_array_field_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.fields = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(1, sizeof(*signature.fields)));
    if (!signature.fields)
        goto cleanup;
    ft_strlcpy(signature.name, "Buffer", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 1;
    ft_strlcpy(signature.fields[0].name, "values", sizeof(signature.fields[0].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].array_count = 4;
    signature.fields[0].length = 16;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "buffer_mod",
                "buffer_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "buffer_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "buffer_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].fields[0].array_count, 4,
            "imported field should preserve array count metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].fields[0].length, 16,
            "imported field should preserve declared length metadata") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.fields)
        cma_free(signature.fields);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_struct_field_type_names_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.fields = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(1, sizeof(*signature.fields)));
    if (!signature.fields)
        goto cleanup;
    ft_strlcpy(signature.name, "Holder", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 1;
    ft_strlcpy(signature.fields[0].name, "point", sizeof(signature.fields[0].name));
    ft_strlcpy(signature.fields[0].declared_type_name, "Point",
        sizeof(signature.fields[0].declared_type_name));
    ft_strlcpy(signature.fields[0].struct_type_name, "Point",
        sizeof(signature.fields[0].struct_type_name));
    signature.fields[0].kind = CBLC_DATA_KIND_STRUCT;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "holder_mod",
                "holder_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "holder_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "holder_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[0].declared_type_name,
            "Point", "imported field should preserve declared type name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[0].struct_type_name,
            "Point", "imported field should preserve struct type name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[0].kind,
            CBLC_DATA_KIND_STRUCT,
            "imported field should preserve struct kind metadata") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.fields)
        cma_free(signature.fields);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_field_visibility_order_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.fields = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(2, sizeof(*signature.fields)));
    if (!signature.fields)
        goto cleanup;
    ft_strlcpy(signature.name, "Ledger", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 2;
    ft_strlcpy(signature.fields[0].name, "hidden", sizeof(signature.fields[0].name));
    ft_strlcpy(signature.fields[1].name, "visible", sizeof(signature.fields[1].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[1].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PRIVATE;
    signature.fields[1].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod",
                "ledger_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "ledger_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[0].visibility,
            CBLC_MEMBER_VISIBILITY_PRIVATE,
            "first imported field should preserve private visibility") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[1].visibility,
            CBLC_MEMBER_VISIBILITY_PUBLIC,
            "second imported field should preserve public visibility") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[1].source_name,
            "visible", "imported field order should be preserved") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.fields)
        cma_free(signature.fields);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_method_parameter_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "add", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "delta",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Counter__add__delta",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[0], "COUNTER-ADD-DELTA",
        sizeof(signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].parameters[0].source_name,
            "delta", "imported method should preserve parameter source name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[0].actual_source_name,
                "Counter__add__delta",
                "imported method should preserve parameter actual source name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].parameters[0].type_name,
            "int", "imported method should preserve parameter type name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_method_return_struct_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Builder", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "current", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_STRUCT;
    ft_strlcpy(signature.methods[0].return_type_name, "Point",
        sizeof(signature.methods[0].return_type_name));
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "builder_mod",
                "builder_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "builder_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "builder_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_STRUCT,
            "imported method should preserve struct return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].return_type_name,
            "Point", "imported method should preserve struct return type name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_private_method_visibility_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "secret", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PRIVATE;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].visibility,
            CBLC_MEMBER_VISIBILITY_PRIVATE,
            "imported method should preserve private visibility") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_method_definition_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    t_cblc_statement statement;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    std::memset(&statement, 0, sizeof(statement));
    signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "current", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_INT;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.methods[0].has_definition = 1;
    signature.methods[0].statements = &statement;
    signature.methods[0].statement_count = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].has_definition, 1,
            "imported method should preserve definition flag") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].methods[0].statement_count, 1,
            "imported method should preserve statement count") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_definition_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    t_cblc_statement statement;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    std::memset(&statement, 0, sizeof(statement));
    signature.constructors = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.constructors = static_cast<t_transpiler_type_constructor_signature *>(
        cma_calloc(1, sizeof(*signature.constructors)));
    if (!signature.constructors)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructor_count = 1;
    signature.constructors[0].has_definition = 1;
    signature.constructors[0].statements = &statement;
    signature.constructors[0].statement_count = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].constructors[0].has_definition, 1,
            "imported constructor should preserve definition flag") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].constructors[0].statement_count, 1,
            "imported constructor should preserve statement count") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.constructors)
        cma_free(signature.constructors);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_register_method_parameter_storage_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    size_t index;
    t_cblc_data_item *parameter_item;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.methods = NULL;
    parameter_item = NULL;
    index = 0;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "add", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "delta",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Counter__add__delta",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[0], "COUNTER-ADD-DELTA",
        sizeof(signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    while (index < consumer_unit.data_count)
    {
        if (consumer_unit.data_items[index].is_active
            && std::strncmp(consumer_unit.data_items[index].source_name,
                "Counter__add__delta",
                sizeof(consumer_unit.data_items[index].source_name)) == 0)
        {
            parameter_item = &consumer_unit.data_items[index];
            break ;
        }
        index += 1;
    }
    if (!parameter_item)
    {
        std::printf("Assertion failed: imported method parameter storage should be registered as a data item\n");
        goto cleanup;
    }
    if (test_expect_int_equal(parameter_item->kind, CBLC_DATA_KIND_INT,
            "imported method parameter storage should preserve its kind") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_struct_kind_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(signature.name, "Point", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_STRUCT;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "point_mod",
                "point_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "point_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "point_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].is_class, 0,
            "imported struct should preserve non-class kind") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_mark_imported_types_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].is_imported, 1,
            "imported type stubs should be marked imported") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_default_constructor_flag_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.has_default_constructor = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].has_default_constructor, 1,
            "imported type should preserve default constructor flag") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_destructor_definition_flag_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.has_destructor = 1;
    signature.has_destructor_definition = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].has_destructor_definition, 1,
            "imported type should preserve destructor definition flag") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_destructor_statement_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    t_cblc_statement statement;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    std::memset(&statement, 0, sizeof(statement));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.has_destructor = 1;
    signature.destructor_statements = &statement;
    signature.destructor_statement_count = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].destructor_statement_count, 1,
            "imported type should preserve destructor statement count") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_parameter_metadata_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.constructors = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.constructors = static_cast<t_transpiler_type_constructor_signature *>(
        cma_calloc(1, sizeof(*signature.constructors)));
    if (!signature.constructors)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructor_count = 1;
    signature.constructors[0].parameter_count = 1;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_source_names[0], "start",
        sizeof(signature.constructors[0].parameter_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[0], "Counter__ctor__start",
        sizeof(signature.constructors[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[0], "COUNTER-CTOR-START",
        sizeof(signature.constructors[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].constructors[0].parameters[0].source_name,
                "start", "imported constructor should preserve parameter source name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].constructors[0].parameters[0].actual_source_name,
                "Counter__ctor__start",
                "imported constructor should preserve parameter actual source name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].constructors[0].parameters[0].type_name,
                "int", "imported constructor should preserve parameter type name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.constructors)
        cma_free(signature.constructors);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_register_constructor_parameter_storage_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    size_t index;
    t_cblc_data_item *parameter_item;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.constructors = NULL;
    index = 0;
    parameter_item = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.constructors = static_cast<t_transpiler_type_constructor_signature *>(
        cma_calloc(1, sizeof(*signature.constructors)));
    if (!signature.constructors)
        goto cleanup;
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructor_count = 1;
    signature.constructors[0].parameter_count = 1;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_source_names[0], "start",
        sizeof(signature.constructors[0].parameter_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[0], "Counter__ctor__start",
        sizeof(signature.constructors[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[0], "COUNTER-CTOR-START",
        sizeof(signature.constructors[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    while (index < consumer_unit.data_count)
    {
        if (consumer_unit.data_items[index].is_active
            && std::strncmp(consumer_unit.data_items[index].source_name,
                "Counter__ctor__start",
                sizeof(consumer_unit.data_items[index].source_name)) == 0)
        {
            parameter_item = &consumer_unit.data_items[index];
            break ;
        }
        index += 1;
    }
    if (!parameter_item)
    {
        std::printf("Assertion failed: imported constructor parameter storage should be registered as a data item\n");
        goto cleanup;
    }
    if (test_expect_int_equal(parameter_item->kind, CBLC_DATA_KIND_INT,
            "imported constructor parameter storage should preserve its kind") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.constructors)
        cma_free(signature.constructors);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_char_pointer_method_parameter_kind_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Writer", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "write", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "text",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Writer__write__text",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "char",
        sizeof(signature.methods[0].parameter_type_names[0]));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "writer_mod",
                "writer_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "writer_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "writer_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].methods[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER,
            "imported method should preserve char pointer parameter kind") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_struct_pointer_method_parameter_type_name_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&signature, 0, sizeof(signature));
    signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    if (!signature.methods)
        goto cleanup;
    ft_strlcpy(signature.name, "Walker", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "visit", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "node",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Walker__visit__node",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "Node",
        sizeof(signature.methods[0].parameter_type_names[0]));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "walker_mod",
                "walker_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "walker_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "walker_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].parameters[0].type_name,
            "Node", "imported struct pointer parameter should preserve pointee type name")
            != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (signature.methods)
        cma_free(signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

