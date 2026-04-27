#include "test_suites.hpp"


FT_TEST(test_cblc_import_type_stubs_preserve_method_parameter_count_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "mix", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 2;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "left",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_source_names[1], "right",
        sizeof(signature.methods[0].parameter_source_names[1]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Counter__mix__left",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[1], "Counter__mix__right",
        sizeof(signature.methods[0].parameter_actual_source_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
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
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].methods[0].parameter_count, 2,
            "imported method should preserve parameter count") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_struct_pointer_field_type_names_from_signature)
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
    ft_strlcpy(signature.name, "Walker", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 1;
    ft_strlcpy(signature.fields[0].name, "current", sizeof(signature.fields[0].name));
    ft_strlcpy(signature.fields[0].declared_type_name, "Node",
        sizeof(signature.fields[0].declared_type_name));
    ft_strlcpy(signature.fields[0].struct_type_name, "Node",
        sizeof(signature.fields[0].struct_type_name));
    signature.fields[0].kind = CBLC_DATA_KIND_STRUCT_POINTER;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
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
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[0].kind,
            CBLC_DATA_KIND_STRUCT_POINTER,
            "imported field should preserve struct pointer kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[0].declared_type_name,
            "Node", "imported field should preserve struct pointer declared type name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[0].struct_type_name,
            "Node", "imported field should preserve struct pointer pointee type name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_second_parameter_source_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "mix", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 2;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "left",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_source_names[1], "right",
        sizeof(signature.methods[0].parameter_source_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[1].source_name,
                "right", "imported method should preserve second parameter source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_second_parameter_actual_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "mix", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 2;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Counter__mix__left",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[1], "Counter__mix__right",
        sizeof(signature.methods[0].parameter_actual_source_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[1].actual_source_name,
                "Counter__mix__right",
                "imported method should preserve second parameter actual source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_second_parameter_cobol_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "mix", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 2;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_cobol_names[0], "COUNTER-MIX-LEFT",
        sizeof(signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[1], "COUNTER-MIX-RIGHT",
        sizeof(signature.methods[0].parameter_cobol_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[1].cobol_name,
                "COUNTER-MIX-RIGHT",
                "imported method should preserve second parameter COBOL name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_second_parameter_type_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "mix", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 2;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT_POINTER;
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[1].type_name,
                "int", "imported method should preserve second parameter type name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_string_constructor_parameter_kind_from_signature)
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
    ft_strlcpy(signature.name, "Writer", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructor_count = 1;
    signature.constructors[0].parameter_count = 1;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "string",
        sizeof(signature.constructors[0].parameter_type_names[0]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_STRING,
            "imported constructor should preserve string parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_char_pointer_constructor_parameter_kind_from_signature)
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
    ft_strlcpy(signature.name, "Writer", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructor_count = 1;
    signature.constructors[0].parameter_count = 1;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "char",
        sizeof(signature.constructors[0].parameter_type_names[0]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER,
            "imported constructor should preserve char pointer parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_struct_pointer_constructor_parameter_type_name_from_signature)
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
    ft_strlcpy(signature.name, "Walker", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructor_count = 1;
    signature.constructors[0].parameter_count = 1;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "Node",
        sizeof(signature.constructors[0].parameter_type_names[0]));
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].constructors[0].parameters[0].type_name,
                "Node", "imported constructor should preserve struct pointer pointee type name")
            != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_second_parameter_cobol_name_from_signature)
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
    signature.constructors[0].parameter_count = 2;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[0], "COUNTER-CTOR-LEFT",
        sizeof(signature.constructors[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[1], "COUNTER-CTOR-RIGHT",
        sizeof(signature.constructors[0].parameter_cobol_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[1].cobol_name,
                "COUNTER-CTOR-RIGHT",
                "imported constructor should preserve second parameter COBOL name")
            != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_parameter_count_from_signature)
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
    signature.constructors[0].parameter_count = 2;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
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
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].constructors[0].parameter_count, 2,
            "imported constructor should preserve parameter count") != FT_SUCCESS)
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

