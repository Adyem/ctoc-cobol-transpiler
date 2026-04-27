#include "test_suites.hpp"

FT_TEST(test_cblc_import_type_stubs_skip_private_types_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    size_t index;
    int found_hidden;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    index = 0;
    found_hidden = 0;
    std::memset(&signature, 0, sizeof(signature));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(signature.name, "Hidden", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PRIVATE;
    if (test_expect_success(transpiler_context_register_module(&context, "hidden_mod",
                "hidden_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "hidden_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "hidden_mod",
                &signature), "private type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    while (index < consumer_unit.struct_type_count)
    {
        if (std::strncmp(consumer_unit.struct_types[index].source_name, "Hidden",
                sizeof(consumer_unit.struct_types[index].source_name)) == 0)
        {
            found_hidden = 1;
            break ;
        }
        index += 1;
    }
    if (test_expect_int_equal(found_hidden, 0,
            "private imported types should not be materialized as stubs") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_transpiler_context_allows_duplicate_private_type_names_across_modules)
{
    t_transpiler_context context;
    t_transpiler_type_signature signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&signature, 0, sizeof(signature));
    ft_strlcpy(signature.name, "Ledger", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PRIVATE;
    if (test_expect_success(transpiler_context_register_module(&context, "left_mod", NULL),
            "left module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module(&context, "right_mod", NULL),
            "right module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "left_mod",
                &signature), "left private type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "right_mod",
                &signature), "right private type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_size_t_equal(context.type_count, 2,
            "duplicate private type names should coexist across modules") != FT_SUCCESS)
        goto failure;
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

FT_TEST(test_transpiler_context_allows_public_and_private_type_names_across_modules)
{
    t_transpiler_context context;
    t_transpiler_type_signature public_signature;
    t_transpiler_type_signature private_signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&public_signature, 0, sizeof(public_signature));
    std::memset(&private_signature, 0, sizeof(private_signature));
    ft_strlcpy(public_signature.name, "Ledger", sizeof(public_signature.name));
    ft_strlcpy(private_signature.name, "Ledger", sizeof(private_signature.name));
    public_signature.kind = TRANSPILE_TYPE_CLASS;
    private_signature.kind = TRANSPILE_TYPE_CLASS;
    public_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    private_signature.visibility = TRANSPILE_SYMBOL_PRIVATE;
    if (test_expect_success(transpiler_context_register_module(&context, "public_mod", NULL),
            "public module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module(&context, "private_mod", NULL),
            "private module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "public_mod",
                &public_signature), "public type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "private_mod",
                &private_signature), "private type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_size_t_equal(context.type_count, 2,
            "public and private type names should coexist across modules") != FT_SUCCESS)
        goto failure;
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

FT_TEST(test_transpiler_context_rejects_duplicate_type_names_within_module)
{
    t_transpiler_context context;
    t_transpiler_type_signature signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&signature, 0, sizeof(signature));
    ft_strlcpy(signature.name, "Ledger", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "ledger_mod",
                &signature), "initial type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (transpiler_context_register_type_signature(&context, "ledger_mod",
            &signature) != FT_FAILURE)
    {
        std::printf("Assertion failed: duplicate type names within one module should be rejected\n");
        goto failure;
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_TYPE_DUPLICATE_NAME,
            "duplicate same-module types should report duplicate-name error") != FT_SUCCESS)
        goto failure;
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

FT_TEST(test_cblc_import_type_stubs_preserve_type_cobol_name_from_signature)
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
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].cobol_name,
            "COUNTER", "imported type should preserve derived COBOL name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_field_cobol_name_from_signature)
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
    ft_strlcpy(signature.fields[0].name, "amount", sizeof(signature.fields[0].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
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
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].fields[0].cobol_name,
            "AMOUNT", "imported field should preserve derived COBOL name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_cobol_name_from_signature)
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
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].cobol_name,
            "WRITE", "imported method should preserve derived COBOL name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_method_parameter_cobol_name_from_signature)
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
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "text",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Writer__write__text",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[0], "WRITER-WRITE-TEXT",
        sizeof(signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "string",
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
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].methods[0].parameters[0].cobol_name,
                "WRITER-WRITE-TEXT",
                "imported method parameter should preserve COBOL name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_constructor_parameter_cobol_name_from_signature)
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
                consumer_unit.struct_types[1].constructors[0].parameters[0].cobol_name,
                "COUNTER-CTOR-START",
                "imported constructor parameter should preserve COBOL name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_void_pointer_method_parameter_kind_from_signature)
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
    ft_strlcpy(signature.name, "Allocator", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "release", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "memory",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Allocator__release__memory",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "void",
        sizeof(signature.methods[0].parameter_type_names[0]));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "allocator_mod",
                "allocator_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "allocator_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "allocator_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].methods[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER,
            "imported method should preserve void pointer parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_skip_when_module_has_no_imports)
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
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_type_count, 1,
            "consumer without imports should retain only builtin string type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_skip_same_module_exports)
{
    t_cblc_translation_unit unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&unit);
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
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "main_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 1,
            "same-module exports should not be imported as extra stubs") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_import_multiple_direct_modules)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature left_signature;
    t_transpiler_type_signature right_signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    std::memset(&left_signature, 0, sizeof(left_signature));
    std::memset(&right_signature, 0, sizeof(right_signature));
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    ft_strlcpy(left_signature.name, "Counter", sizeof(left_signature.name));
    left_signature.kind = TRANSPILE_TYPE_CLASS;
    left_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    ft_strlcpy(right_signature.name, "Ledger", sizeof(right_signature.name));
    right_signature.kind = TRANSPILE_TYPE_CLASS;
    right_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "first provider registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod",
                "ledger_mod"), "second provider registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer should import first provider") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer should import second provider") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_mod",
                &left_signature), "first type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "ledger_mod",
                &right_signature), "second type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_type_count, 3,
            "consumer should receive builtin string plus both directly imported types")
            != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_string_method_parameter_kind_from_signature)
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
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "text",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Writer__write__text",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "string",
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
            TRANSPILE_FUNCTION_PARAMETER_STRING,
            "imported method should preserve string parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_int_pointer_method_parameter_kind_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "store", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT_POINTER;
    ft_strlcpy(signature.methods[0].parameter_source_names[0], "target",
        sizeof(signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_actual_source_names[0], "Writer__store__target",
        sizeof(signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(signature.methods[0].parameter_cobol_names[0], "WRITER-STORE-TARGET",
        sizeof(signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
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
            TRANSPILE_FUNCTION_PARAMETER_INT_POINTER,
            "imported method should preserve int pointer parameter kind") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_void_pointer_method_return_metadata_from_signature)
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
    ft_strlcpy(signature.name, "Allocator", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "allocate", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID_POINTER;
    ft_strlcpy(signature.methods[0].return_type_name, "void",
        sizeof(signature.methods[0].return_type_name));
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "allocator_mod",
                "allocator_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "allocator_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "allocator_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_VOID_POINTER,
            "imported method should preserve void pointer return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].return_type_name,
            "void", "imported method should preserve void pointer return type name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_char_pointer_method_return_metadata_from_signature)
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
    ft_strlcpy(signature.name, "Reader", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "buffer", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_CHAR_POINTER;
    ft_strlcpy(signature.methods[0].return_type_name, "char",
        sizeof(signature.methods[0].return_type_name));
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "reader_mod",
                "reader_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "reader_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "reader_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_CHAR_POINTER,
            "imported method should preserve char pointer return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].return_type_name,
            "char", "imported method should preserve char pointer return type name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_int_pointer_method_return_metadata_from_signature)
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
    ft_strlcpy(signature.name, "Reader", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.method_count = 1;
    ft_strlcpy(signature.methods[0].name, "slot", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_INT_POINTER;
    ft_strlcpy(signature.methods[0].return_type_name, "int",
        sizeof(signature.methods[0].return_type_name));
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "reader_mod",
                "reader_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "reader_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "reader_mod",
                &signature), "type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_INT_POINTER,
            "imported method should preserve int pointer return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].return_type_name,
            "int", "imported method should preserve int pointer return type name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_preserve_struct_pointer_method_return_metadata_from_signature)
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
    ft_strlcpy(signature.methods[0].name, "current", sizeof(signature.methods[0].name));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_STRUCT_POINTER;
    ft_strlcpy(signature.methods[0].return_type_name, "Node",
        sizeof(signature.methods[0].return_type_name));
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
    if (test_expect_int_equal(consumer_unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_STRUCT_POINTER,
            "imported method should preserve struct pointer return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(consumer_unit.struct_types[1].methods[0].return_type_name,
            "Node", "imported method should preserve struct pointer return type name") != FT_SUCCESS)
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

FT_TEST(test_cblc_import_type_stubs_register_struct_method_parameter_storage_from_signature)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature point_signature;
    t_transpiler_type_signature builder_signature;
    size_t index;
    t_cblc_data_item *parameter_item;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    index = 0;
    parameter_item = NULL;
    std::memset(&point_signature, 0, sizeof(point_signature));
    std::memset(&builder_signature, 0, sizeof(builder_signature));
    builder_signature.methods = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    builder_signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*builder_signature.methods)));
    if (!builder_signature.methods)
        goto cleanup;
    ft_strlcpy(point_signature.name, "Point", sizeof(point_signature.name));
    point_signature.kind = TRANSPILE_TYPE_STRUCT;
    point_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    ft_strlcpy(builder_signature.name, "Builder", sizeof(builder_signature.name));
    builder_signature.kind = TRANSPILE_TYPE_CLASS;
    builder_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    builder_signature.method_count = 1;
    ft_strlcpy(builder_signature.methods[0].name, "set", sizeof(builder_signature.methods[0].name));
    builder_signature.methods[0].parameter_count = 1;
    builder_signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_STRUCT;
    ft_strlcpy(builder_signature.methods[0].parameter_source_names[0], "point",
        sizeof(builder_signature.methods[0].parameter_source_names[0]));
    ft_strlcpy(builder_signature.methods[0].parameter_actual_source_names[0], "Builder__set__point",
        sizeof(builder_signature.methods[0].parameter_actual_source_names[0]));
    ft_strlcpy(builder_signature.methods[0].parameter_cobol_names[0], "BUILDER-SET-POINT",
        sizeof(builder_signature.methods[0].parameter_cobol_names[0]));
    ft_strlcpy(builder_signature.methods[0].parameter_type_names[0], "Point",
        sizeof(builder_signature.methods[0].parameter_type_names[0]));
    builder_signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    builder_signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "shape_mod",
                "shape_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "shape_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "shape_mod",
                &point_signature), "point type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "shape_mod",
                &builder_signature), "builder type signature registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    while (index < consumer_unit.data_count)
    {
        if (consumer_unit.data_items[index].is_active
            && std::strncmp(consumer_unit.data_items[index].source_name,
                "Builder__set__point",
                sizeof(consumer_unit.data_items[index].source_name)) == 0)
        {
            parameter_item = &consumer_unit.data_items[index];
            break ;
        }
        index += 1;
    }
    if (!parameter_item)
    {
        std::printf("Assertion failed: imported struct method parameter storage should be registered as a data item\n");
        goto cleanup;
    }
    if (test_expect_int_equal(parameter_item->kind, CBLC_DATA_KIND_STRUCT,
            "imported struct parameter storage should preserve struct kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(parameter_item->struct_type_name, "Point",
            "imported struct parameter storage should preserve struct type name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (builder_signature.methods)
        cma_free(builder_signature.methods);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_do_not_follow_transitive_imports)
{
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    t_transpiler_type_signature signature;
    int context_initialized;
    int status;

    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    std::memset(&signature, 0, sizeof(signature));
    ft_strlcpy(signature.name, "Ledger", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "type_mod", "type_mod"),
            "type provider registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "middle_mod", "middle_mod"),
            "middle module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", "main_mod"),
            "main module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "middle_mod",
                "type_mod"), "middle should import provider") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "middle_mod"), "main should import only middle") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "type_mod",
                &signature), "provider type signature should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "direct import type stub pass should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_type_count, 1,
            "consumer should only retain builtin string without transitive type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}
