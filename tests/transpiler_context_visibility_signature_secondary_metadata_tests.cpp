#include "test_suites.hpp"

FT_TEST(test_cblc_import_type_stubs_preserve_field_second_declared_type_name_from_signature)
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
    ft_strlcpy(signature.name, "Holder", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 2;
    ft_strlcpy(signature.fields[0].name, "left", sizeof(signature.fields[0].name));
    ft_strlcpy(signature.fields[1].name, "right", sizeof(signature.fields[1].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.fields[1].kind = CBLC_DATA_KIND_STRUCT;
    signature.fields[1].visibility = TRANSPILE_SYMBOL_PUBLIC;
    ft_strlcpy(signature.fields[1].declared_type_name, "Point",
        sizeof(signature.fields[1].declared_type_name));
    ft_strlcpy(signature.fields[1].struct_type_name, "Point",
        sizeof(signature.fields[1].struct_type_name));
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
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[1].declared_type_name,
            "Point", "imported second field should preserve declared type name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_field_second_array_count_from_signature)
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
    ft_strlcpy(signature.name, "Buffer", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 2;
    ft_strlcpy(signature.fields[0].name, "single", sizeof(signature.fields[0].name));
    ft_strlcpy(signature.fields[1].name, "many", sizeof(signature.fields[1].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[1].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.fields[1].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.fields[1].array_count = 8;
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
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].fields[1].array_count, 8,
            "imported second field should preserve array count") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_field_second_const_flag_from_signature)
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
    ft_strlcpy(signature.fields[0].name, "mutable_value", sizeof(signature.fields[0].name));
    ft_strlcpy(signature.fields[1].name, "constant_value", sizeof(signature.fields[1].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[1].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.fields[1].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.fields[1].is_const = 1;
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
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[1].is_const, 1,
            "imported second field should preserve const flag") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_second_parameter_kind_from_signature)
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
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].methods[0].parameters[1].kind),
            TRANSPILE_FUNCTION_PARAMETER_INT_POINTER,
            "imported method should preserve second parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_second_parameter_struct_pointer_type_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "pair", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 2;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "Node",
        sizeof(signature.methods[0].parameter_type_names[1]));
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[1].type_name,
                "Node", "imported method should preserve second struct pointer type name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_second_parameter_source_name_from_signature)
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
    ft_strlcpy(signature.constructors[0].parameter_source_names[0], "left",
        sizeof(signature.constructors[0].parameter_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_source_names[1], "right",
        sizeof(signature.constructors[0].parameter_source_names[1]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[1].source_name,
                "right", "imported constructor should preserve second parameter source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_second_parameter_actual_name_from_signature)
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
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[0], "Counter__ctor__left",
        sizeof(signature.constructors[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[1], "Counter__ctor__right",
        sizeof(signature.constructors[0].parameter_actual_source_names[1]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[1].actual_source_name,
                "Counter__ctor__right",
                "imported constructor should preserve second parameter actual source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_second_parameter_type_name_from_signature)
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
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "Node",
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
                consumer_unit.struct_types[1].constructors[0].parameters[1].type_name,
                "Node", "imported constructor should preserve second parameter type name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_second_parameter_kind_from_signature)
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
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "Node",
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
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].constructors[0].parameters[1].kind),
            TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER,
            "imported constructor should preserve second parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_third_parameter_source_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "blend", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 3;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "left",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_source_names[1], "middle",
        sizeof(signature.methods[0].parameter_source_names[1]));
    ft_strlcpy(signature.methods[0].parameter_source_names[2], "right",
        sizeof(signature.methods[0].parameter_source_names[2]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[2], "int",
        sizeof(signature.methods[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].methods[0].parameters[2].source_name,
                "right", "imported method should preserve third parameter source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_third_parameter_actual_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "blend", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 3;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Counter__blend__left",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[1], "Counter__blend__middle",
        sizeof(signature.methods[0].parameter_actual_source_names[1]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[2], "Counter__blend__right",
        sizeof(signature.methods[0].parameter_actual_source_names[2]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[2], "int",
        sizeof(signature.methods[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].methods[0].parameters[2].actual_source_name,
                "Counter__blend__right",
                "imported method should preserve third parameter actual source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_third_parameter_cobol_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "blend", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 3;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_cobol_names[0], "COUNTER-BLEND-LEFT",
        sizeof(signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[1], "COUNTER-BLEND-MIDDLE",
        sizeof(signature.methods[0].parameter_cobol_names[1]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[2], "COUNTER-BLEND-RIGHT",
        sizeof(signature.methods[0].parameter_cobol_names[2]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[2], "int",
        sizeof(signature.methods[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].methods[0].parameters[2].cobol_name,
                "COUNTER-BLEND-RIGHT",
                "imported method should preserve third parameter COBOL name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_third_parameter_type_name_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "blend", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 3;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[2], "Node",
        sizeof(signature.methods[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].methods[0].parameters[2].type_name,
                "Node", "imported method should preserve third parameter type name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_third_parameter_kind_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "blend", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 3;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.methods[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[1], "int",
        sizeof(signature.methods[0].parameter_type_names[1]));
    ft_strlcpy(signature.methods[0].parameter_type_names[2], "Node",
        sizeof(signature.methods[0].parameter_type_names[2]));
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
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].methods[0].parameters[2].kind),
            TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER,
            "imported method should preserve third parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_third_parameter_source_name_from_signature)
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
    signature.constructors[0].parameter_count = 3;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_source_names[0], "left",
        sizeof(signature.constructors[0].parameter_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_source_names[1], "middle",
        sizeof(signature.constructors[0].parameter_source_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_source_names[2], "right",
        sizeof(signature.constructors[0].parameter_source_names[2]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[2], "int",
        sizeof(signature.constructors[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[2].source_name,
                "right", "imported constructor should preserve third parameter source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_third_parameter_actual_name_from_signature)
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
    signature.constructors[0].parameter_count = 3;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[0], "Counter__ctor__left",
        sizeof(signature.constructors[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[1], "Counter__ctor__middle",
        sizeof(signature.constructors[0].parameter_actual_source_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_actual_source_names[2], "Counter__ctor__right",
        sizeof(signature.constructors[0].parameter_actual_source_names[2]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[2], "int",
        sizeof(signature.constructors[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[2].actual_source_name,
                "Counter__ctor__right",
                "imported constructor should preserve third parameter actual source name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_third_parameter_cobol_name_from_signature)
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
    signature.constructors[0].parameter_count = 3;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[0], "COUNTER-CTOR-LEFT",
        sizeof(signature.constructors[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[1], "COUNTER-CTOR-MIDDLE",
        sizeof(signature.constructors[0].parameter_cobol_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_cobol_names[2], "COUNTER-CTOR-RIGHT",
        sizeof(signature.constructors[0].parameter_cobol_names[2]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[2], "int",
        sizeof(signature.constructors[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[2].cobol_name,
                "COUNTER-CTOR-RIGHT",
                "imported constructor should preserve third parameter COBOL name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_third_parameter_type_name_from_signature)
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
    signature.constructors[0].parameter_count = 3;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[2], "Node",
        sizeof(signature.constructors[0].parameter_type_names[2]));
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
                consumer_unit.struct_types[1].constructors[0].parameters[2].type_name,
                "Node", "imported constructor should preserve third parameter type name")
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_third_parameter_kind_from_signature)
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
    signature.constructors[0].parameter_count = 3;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[1] = TRANSPILE_FUNCTION_PARAMETER_INT;
    signature.constructors[0].parameter_kinds[2] = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[1], "int",
        sizeof(signature.constructors[0].parameter_type_names[1]));
    ft_strlcpy(signature.constructors[0].parameter_type_names[2], "Node",
        sizeof(signature.constructors[0].parameter_type_names[2]));
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
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].constructors[0].parameters[2].kind),
            TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER,
            "imported constructor should preserve third parameter kind") != FT_SUCCESS)
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
