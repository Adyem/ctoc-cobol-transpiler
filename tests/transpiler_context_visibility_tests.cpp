#include "test_suites.hpp"

FT_TEST(test_transpiler_context_allows_private_duplicates)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "first private function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "beta_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "second private function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.function_count), 2,
            "private duplicates should register in both modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "private duplicates should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_public_duplicate_exports)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC),
            "first public function registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_function(&context, "beta_mod", "shared",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        std::printf("Assertion failed: expected duplicate public export to be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "duplicate export should emit a diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_FUNCTION_EXPORT_CONFLICT,
            "duplicate export diagnostic should use export conflict code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_allows_private_access_within_module)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "private helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "alpha_mod", "alpha_mod", "helper");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        std::printf("Assertion failed: private helper should be accessible inside its module\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "private self-access should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "private self-access should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_private_access_across_modules)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "beta_mod", "alpha_mod"),
            "dependent module should record import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module ordering should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PRIVATE),
            "private helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "beta_mod", "alpha_mod", "helper");
    if (signature)
    {
        transpiler_context_dispose(&context);
        std::printf("Assertion failed: cross-module private access should be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "private access rejection should emit one diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_FUNCTION_PRIVATE_ACCESS,
            "private access rejection should use dedicated error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "private access rejection should record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_allows_public_access_across_modules)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "beta_mod", "alpha_mod"),
            "dependent module should record import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module ordering should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC),
            "public helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "beta_mod", "alpha_mod", "helper");
    if (!signature)
    {
        transpiler_context_dispose(&context);
        std::printf("Assertion failed: cross-module public access should be permitted\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "public access should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "public access should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_requires_import_for_cross_module_access)
{
    t_transpiler_context context;
    const t_transpiler_function_signature *signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "first module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "beta_mod", NULL),
            "second module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_function(&context, "alpha_mod", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC),
            "public helper registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    signature = transpiler_context_resolve_function_access(&context, "beta_mod", "alpha_mod", "helper");
    if (signature)
    {
        transpiler_context_dispose(&context);
        std::printf("Assertion failed: cross-module access should require imports\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "missing import should emit diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_MODULE_IMPORT_REQUIRED,
            "missing import diagnostic should use dedicated code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "missing import should mark context as having errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_resolves_imported_public_type)
{
    t_transpiler_context context;
    t_transpiler_type_signature source_signature;
    const t_transpiler_type_signature *resolved;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&source_signature, 0, sizeof(source_signature));
    ft_strlcpy(source_signature.name, "Ledger", sizeof(source_signature.name));
    source_signature.kind = TRANSPILE_TYPE_CLASS;
    source_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    source_signature.field_count = 2;
    source_signature.method_count = 1;
    source_signature.constructor_count = 1;
    source_signature.has_default_constructor = 1;
    source_signature.has_destructor = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod", NULL),
            "provider module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "consumer module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer should import provider") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module ordering should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "ledger_mod",
                &source_signature), "type signature registration should succeed") != FT_SUCCESS)
        goto failure;
    resolved = transpiler_context_resolve_type_access(&context, "main_mod", "ledger_mod", "Ledger");
    if (!resolved)
    {
        std::printf("Assertion failed: imported public class type should resolve\n");
        goto failure;
    }
    if (test_expect_int_equal(resolved->kind, TRANSPILE_TYPE_CLASS,
            "resolved type should preserve class kind") != FT_SUCCESS)
        goto failure;
    if (test_expect_size_t_equal(resolved->field_count, 2,
            "resolved type should preserve field count") != FT_SUCCESS)
        goto failure;
    if (test_expect_size_t_equal(resolved->method_count, 1,
            "resolved type should preserve method count") != FT_SUCCESS)
        goto failure;
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

FT_TEST(test_transpiler_context_requires_import_for_cross_module_type_access)
{
    t_transpiler_context context;
    t_transpiler_type_signature source_signature;
    const t_transpiler_type_signature *resolved;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&source_signature, 0, sizeof(source_signature));
    ft_strlcpy(source_signature.name, "Ledger", sizeof(source_signature.name));
    source_signature.kind = TRANSPILE_TYPE_CLASS;
    source_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod", NULL),
            "provider module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "consumer module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "ledger_mod",
                &source_signature), "type signature registration should succeed") != FT_SUCCESS)
        goto failure;
    resolved = transpiler_context_resolve_type_access(&context, "main_mod", "ledger_mod", "Ledger");
    if (resolved)
    {
        std::printf("Assertion failed: cross-module type access should require imports\n");
        goto failure;
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "missing type import should emit one diagnostic") != FT_SUCCESS)
        goto failure;
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_MODULE_IMPORT_REQUIRED,
            "missing type import should use import-required diagnostic") != FT_SUCCESS)
        goto failure;
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

FT_TEST(test_cblc_register_translation_unit_exports_records_class_type_metadata)
{
    const char *source;
    t_cblc_translation_unit unit;
    t_transpiler_context context;
    const t_transpiler_type_signature *types;
    size_t type_count;
    int context_initialized;
    int status;

    source = "class Ledger\n"
        "{\n"
        "    private:\n"
        "        int amount;\n"
        "\n"
        "    public:\n"
        "        void reset() {\n"
        "            amount = 0;\n"
        "            return;\n"
        "        }\n"
        "};\n";
    cblc_translation_unit_init(&unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class-only module should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "ledger_mod",
                &unit), "class metadata export should succeed") != FT_SUCCESS)
        goto cleanup;
    types = transpiler_context_get_types(&context, &type_count);
    if (test_expect_size_t_equal(type_count, 1,
            "only the user-defined class should be exported") != FT_SUCCESS)
        goto cleanup;
    if (!types || std::strncmp(types[0].name, "Ledger", sizeof(types[0].name)) != 0)
    {
        std::printf("Assertion failed: exported type should be Ledger\n");
        goto cleanup;
    }
    if (test_expect_int_equal(types[0].kind, TRANSPILE_TYPE_CLASS,
            "exported type should be a class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(types[0].field_count, 1,
            "exported class should preserve field count") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(types[0].method_count, 1,
            "exported class should preserve method count") != FT_SUCCESS)
        goto cleanup;
    if (!types[0].fields || std::strncmp(types[0].fields[0].name, "amount",
            sizeof(types[0].fields[0].name)) != 0)
    {
        std::printf("Assertion failed: exported class should preserve field names\n");
        goto cleanup;
    }
    if (test_expect_int_equal(types[0].fields[0].visibility, TRANSPILE_SYMBOL_PRIVATE,
            "exported class should preserve private field visibility") != FT_SUCCESS)
        goto cleanup;
    if (!types[0].methods || std::strncmp(types[0].methods[0].name, "reset",
            sizeof(types[0].methods[0].name)) != 0)
    {
        std::printf("Assertion failed: exported class should preserve method names\n");
        goto cleanup;
    }
    if (test_expect_int_equal(types[0].methods[0].visibility, TRANSPILE_SYMBOL_PUBLIC,
            "exported class should preserve public method visibility") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_allow_direct_imported_class_declaration)
{
    const char *provider_source;
    const char *consumer_source;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    provider_source = "class Ledger\n"
        "{\n"
        "    private:\n"
        "        int hidden;\n"
        "\n"
        "    public:\n"
        "        int amount;\n"
        "        void reset() {\n"
        "            amount = 0;\n"
        "            return;\n"
        "        }\n"
        "};\n";
    consumer_source = "import \"ledger_mod\";\n"
        "Ledger book;\n"
        "void main()\n"
        "{\n"
        "    book.reset();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider class module should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod", "ledger_mod"),
            "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", "main_mod"),
            "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "ledger_mod",
                &provider_unit), "provider type export should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer should import type stubs") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(consumer_source, &consumer_unit),
            "consumer should parse imported class declarations") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_type_count, 2,
            "consumer should contain builtin string and imported class stub") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].is_imported, 1,
            "imported class should be marked as imported") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].field_count, 2,
            "imported class should preserve field metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].method_count, 1,
            "imported class should preserve public method metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[0].visibility,
            CBLC_MEMBER_VISIBILITY_PRIVATE,
            "imported class should preserve private field visibility") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].fields[1].visibility,
            CBLC_MEMBER_VISIBILITY_PUBLIC,
            "imported class should preserve public field visibility") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.data_count, 3,
            "consumer should declare object plus imported class field aliases") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(consumer_unit.data_items[0].struct_type_name, "Ledger",
            sizeof(consumer_unit.data_items[0].struct_type_name)) != 0)
    {
        std::printf("Assertion failed: imported class object should preserve type name\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_reject_private_imported_methods)
{
    const char *provider_source;
    const char *consumer_source;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    provider_source = "class Ledger\n"
        "{\n"
        "    private:\n"
        "        void seal() {\n"
        "            return;\n"
        "        }\n"
        "\n"
        "    public:\n"
        "        void reset() {\n"
        "            return;\n"
        "        }\n"
        "};\n";
    consumer_source = "import \"ledger_mod\";\n"
        "Ledger book;\n"
        "void main()\n"
        "{\n"
        "    book.seal();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider class with private method should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod", "ledger_mod"),
            "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", "main_mod"),
            "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "ledger_mod",
                &provider_unit), "provider type export should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer should import type stubs") != FT_SUCCESS)
        goto cleanup;
    if (cblc_parse_translation_unit(consumer_source, &consumer_unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: private imported class methods should not be callable outside the class\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_merge_direct_definition_exports_for_method_calls)
{
    const char *declaration_source;
    const char *implementation_source;
    const char *consumer_source;
    t_cblc_translation_unit declaration_unit;
    t_cblc_translation_unit implementation_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    declaration_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "        void add(int delta);\n"
        "};\n";
    implementation_source = "import \"counter_decl\";\n"
        "void Counter::add(int delta) {\n"
        "    value = value + delta;\n"
        "    return;\n"
        "}\n";
    consumer_source = "import \"counter_decl\";\n"
        "import \"counter_impl\";\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    counter.add(2);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&declaration_unit);
    cblc_translation_unit_init(&implementation_unit);
    cblc_translation_unit_init(&consumer_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(declaration_source, &declaration_unit),
            "declaration unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl", "counter_decl"),
            "declaration module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl", "counter_impl"),
            "implementation module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", "main_mod"),
            "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "counter_impl",
                "counter_decl"), "implementation should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_decl"), "consumer should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_impl"), "consumer should import implementation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_decl",
                &declaration_unit), "declaration exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "counter_impl",
                &implementation_unit), "implementation should import declaration type stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(implementation_source,
                &implementation_unit), "implementation unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_impl",
                &implementation_unit), "implementation exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer should import merged declaration and implementation stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(consumer_source, &consumer_unit),
            "consumer unit should parse merged class metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&consumer_unit, &generated_c),
            "consumer unit should generate C with imported method body") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c
        || !ft_strnstr(generated_c, "Counter__add__delta = 2;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "counter.value = counter.value + Counter__add__delta;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: direct declaration and implementation imports should merge imported method bodies\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&declaration_unit);
    cblc_translation_unit_dispose(&implementation_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_imported_constructor_metadata)
{
    const char *provider_source;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    provider_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        Counter();\n"
        "        Counter(int start);\n"
        "};\n";
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider constructor declarations should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_mod",
                &provider_unit), "provider type export should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].constructor_count, 2,
            "imported class should preserve constructor overload count") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].has_default_constructor, 1,
            "imported class should preserve default constructor flag") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(consumer_unit.struct_types[1].constructors[1].parameter_count, 1,
            "imported class should preserve parameterized constructor arity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(
                consumer_unit.struct_types[1].constructors[1].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_INT,
            "imported class should preserve constructor parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(
                consumer_unit.struct_types[1].constructors[1].parameters[0].actual_source_name,
                "Counter__ctor_1__start",
                "imported class should preserve constructor parameter storage name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_preserve_imported_destructor_metadata)
{
    const char *provider_source;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    provider_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        ~Counter();\n"
        "};\n";
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider destructor declaration should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_mod",
                "counter_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_mod",
                &provider_unit), "provider type export should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].has_destructor, 1,
            "imported class should preserve destructor presence") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(consumer_unit.struct_types[1].has_destructor_definition, 0,
            "imported class should preserve lack of destructor definition") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_allow_public_imported_field_assignment)
{
    const char *provider_source;
    const char *consumer_source;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    provider_source = "class Ledger\n"
        "{\n"
        "    public:\n"
        "        int amount;\n"
        "};\n";
    consumer_source = "import \"ledger_mod\";\n"
        "Ledger book;\n"
        "void main()\n"
        "{\n"
        "    book.amount = 7;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider field declaration should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod",
                "ledger_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "ledger_mod",
                &provider_unit), "provider type export should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(consumer_source, &consumer_unit),
            "consumer should parse public imported field assignment") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_reject_private_imported_field_assignment)
{
    const char *provider_source;
    const char *consumer_source;
    t_cblc_translation_unit provider_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    provider_source = "class Ledger\n"
        "{\n"
        "    private:\n"
        "        int hidden;\n"
        "\n"
        "    public:\n"
        "        int amount;\n"
        "};\n";
    consumer_source = "import \"ledger_mod\";\n"
        "Ledger book;\n"
        "void main()\n"
        "{\n"
        "    book.hidden = 7;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&provider_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(provider_source, &provider_unit),
            "provider private field declaration should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "ledger_mod",
                "ledger_mod"), "provider module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "ledger_mod"), "consumer import registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "ledger_mod",
                &provider_unit), "provider type export should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (cblc_parse_translation_unit(consumer_source, &consumer_unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: private imported fields should not be assignable outside the class\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&provider_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_merge_direct_definition_exports_for_method_return_assignments)
{
    const char *declaration_source;
    const char *implementation_source;
    const char *consumer_source;
    t_cblc_translation_unit declaration_unit;
    t_cblc_translation_unit implementation_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    declaration_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "        int current();\n"
        "};\n";
    implementation_source = "import \"counter_decl\";\n"
        "int Counter::current() {\n"
        "    return value;\n"
        "}\n";
    consumer_source = "import \"counter_decl\";\n"
        "import \"counter_impl\";\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = counter.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&declaration_unit);
    cblc_translation_unit_init(&implementation_unit);
    cblc_translation_unit_init(&consumer_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(declaration_source, &declaration_unit),
            "declaration unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl",
                "counter_decl"), "declaration module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl",
                "counter_impl"), "implementation module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "counter_impl",
                "counter_decl"), "implementation should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_decl"), "consumer should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_impl"), "consumer should import implementation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_decl",
                &declaration_unit), "declaration exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "counter_impl",
                &implementation_unit), "implementation should import declaration type stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(implementation_source,
                &implementation_unit), "implementation unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_impl",
                &implementation_unit), "implementation exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer should import merged declaration and implementation stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(consumer_source, &consumer_unit),
            "consumer unit should parse merged method return metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&consumer_unit, &generated_c),
            "consumer unit should generate C with imported return method body") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c
        || !ft_strnstr(generated_c, "total = counter.value;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: direct declaration and implementation imports should merge imported return method bodies\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&declaration_unit);
    cblc_translation_unit_dispose(&implementation_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_merge_direct_definition_exports_for_constructor_bodies)
{
    const char *declaration_source;
    const char *implementation_source;
    const char *consumer_source;
    t_cblc_translation_unit declaration_unit;
    t_cblc_translation_unit implementation_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    char *generated_c;
    int context_initialized;
    int status;

    declaration_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "        Counter(int start);\n"
        "};\n";
    implementation_source = "import \"counter_decl\";\n"
        "Counter::Counter(int start) {\n"
        "    value = start;\n"
        "}\n";
    consumer_source = "import \"counter_decl\";\n"
        "import \"counter_impl\";\n"
        "Counter counter(7);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&declaration_unit);
    cblc_translation_unit_init(&implementation_unit);
    cblc_translation_unit_init(&consumer_unit);
    generated_c = NULL;
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(declaration_source, &declaration_unit),
            "declaration unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl",
                "counter_decl"), "declaration module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl",
                "counter_impl"), "implementation module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "counter_impl",
                "counter_decl"), "implementation should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_decl"), "consumer should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_impl"), "consumer should import implementation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_decl",
                &declaration_unit), "declaration exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "counter_impl",
                &implementation_unit), "implementation should import declaration type stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(implementation_source,
                &implementation_unit), "implementation unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_impl",
                &implementation_unit), "implementation exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer should import merged declaration and implementation stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(consumer_source, &consumer_unit),
            "consumer unit should parse merged constructor metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&consumer_unit, &generated_c),
            "consumer unit should generate C with imported constructor body") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c
        || !ft_strnstr(generated_c, "Counter__ctor__start = 7;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "counter.value = Counter__ctor__start;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: direct declaration and implementation imports should merge imported constructor bodies\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&declaration_unit);
    cblc_translation_unit_dispose(&implementation_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_reject_qualified_destructor_definitions)
{
    const char *declaration_source;
    const char *implementation_source;
    t_cblc_translation_unit declaration_unit;
    t_cblc_translation_unit implementation_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    declaration_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        ~Counter();\n"
        "};\n";
    implementation_source = "import \"counter_decl\";\n"
        "Counter::~Counter() {\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&declaration_unit);
    cblc_translation_unit_init(&implementation_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(declaration_source, &declaration_unit),
            "declaration unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl",
                "counter_decl"), "declaration module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl",
                "counter_impl"), "implementation module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "counter_impl",
                "counter_decl"), "implementation should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_decl"), "consumer should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_impl"), "consumer should import implementation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_decl",
                &declaration_unit), "declaration exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "counter_impl",
                &implementation_unit), "implementation should import declaration type stubs")
            != FT_SUCCESS)
        goto cleanup;
    if (cblc_parse_translation_unit(implementation_source, &implementation_unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: qualified destructor definitions should be rejected until lifecycle parsing supports them\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&declaration_unit);
    cblc_translation_unit_dispose(&implementation_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_import_type_stubs_require_direct_type_import_for_implementation_only_module)
{
    const char *declaration_source;
    const char *consumer_source;
    t_cblc_translation_unit declaration_unit;
    t_cblc_translation_unit consumer_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    declaration_source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "        void add(int delta);\n"
        "};\n";
    consumer_source = "import \"counter_impl\";\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&declaration_unit);
    cblc_translation_unit_init(&consumer_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(declaration_source, &declaration_unit),
            "declaration unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl",
                "counter_decl"), "declaration module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl",
                "counter_impl"), "implementation module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod",
                "main_mod"), "consumer module registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "counter_impl",
                "counter_decl"), "implementation should import declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod",
                "counter_impl"), "consumer should import only implementation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_register_translation_unit_exports(&context, "counter_decl",
                &declaration_unit), "declaration exports should register") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_import_translation_unit_type_stubs(&context, "main_mod",
                &consumer_unit), "consumer type stub import should succeed") != FT_SUCCESS)
        goto cleanup;
    if (cblc_parse_translation_unit(consumer_source, &consumer_unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: importing only the implementation module should not expose the declaration type\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&declaration_unit);
    cblc_translation_unit_dispose(&consumer_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_transpiler_context_allows_compatible_public_type_exports_across_modules)
{
    t_transpiler_context context;
    t_transpiler_type_signature signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&signature, 0, sizeof(signature));
    ft_strlcpy(signature.name, "Counter", sizeof(signature.name));
    signature.kind = TRANSPILE_TYPE_CLASS;
    signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.field_count = 1;
    signature.method_count = 1;
    signature.constructor_count = 1;
    signature.has_default_constructor = 0;
    signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(1, sizeof(*signature.fields)));
    signature.methods = static_cast<t_transpiler_type_method_signature *>(
        cma_calloc(1, sizeof(*signature.methods)));
    signature.constructors = static_cast<t_transpiler_type_constructor_signature *>(
        cma_calloc(1, sizeof(*signature.constructors)));
    if (!signature.fields || !signature.methods || !signature.constructors)
        goto failure;
    ft_strlcpy(signature.fields[0].name, "value", sizeof(signature.fields[0].name));
    signature.fields[0].kind = CBLC_DATA_KIND_INT;
    signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    ft_strlcpy(signature.methods[0].name, "add", sizeof(signature.methods[0].name));
    signature.methods[0].parameter_count = 1;
    signature.methods[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.methods[0].parameter_type_names[0], "int",
        sizeof(signature.methods[0].parameter_type_names[0]));
    signature.methods[0].return_kind = CBLC_FUNCTION_RETURN_VOID;
    signature.methods[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    signature.constructors[0].parameter_count = 1;
    signature.constructors[0].parameter_kinds[0] = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(signature.constructors[0].parameter_type_names[0], "int",
        sizeof(signature.constructors[0].parameter_type_names[0]));
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl", NULL),
            "declaration module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl", NULL),
            "implementation module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_decl",
                &signature), "declaration type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_impl",
                &signature), "compatible implementation type registration should succeed")
            != FT_SUCCESS)
        goto failure;
    if (test_expect_size_t_equal(context.type_count, 2,
            "compatible public type exports should coexist across modules") != FT_SUCCESS)
        goto failure;
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "compatible public type exports should not record errors") != FT_SUCCESS)
        goto failure;
    cma_free(signature.fields);
    cma_free(signature.methods);
    cma_free(signature.constructors);
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    if (signature.fields)
        cma_free(signature.fields);
    if (signature.methods)
        cma_free(signature.methods);
    if (signature.constructors)
        cma_free(signature.constructors);
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

FT_TEST(test_transpiler_context_rejects_conflicting_public_type_exports_across_modules)
{
    t_transpiler_context context;
    t_transpiler_type_signature first_signature;
    t_transpiler_type_signature second_signature;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&first_signature, 0, sizeof(first_signature));
    std::memset(&second_signature, 0, sizeof(second_signature));
    ft_strlcpy(first_signature.name, "Counter", sizeof(first_signature.name));
    ft_strlcpy(second_signature.name, "Counter", sizeof(second_signature.name));
    first_signature.kind = TRANSPILE_TYPE_CLASS;
    second_signature.kind = TRANSPILE_TYPE_CLASS;
    first_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    second_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
    first_signature.field_count = 1;
    second_signature.field_count = 2;
    first_signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(1, sizeof(*first_signature.fields)));
    second_signature.fields = static_cast<t_transpiler_type_field_signature *>(
        cma_calloc(2, sizeof(*second_signature.fields)));
    if (!first_signature.fields || !second_signature.fields)
        goto failure;
    ft_strlcpy(first_signature.fields[0].name, "value", sizeof(first_signature.fields[0].name));
    ft_strlcpy(second_signature.fields[0].name, "value", sizeof(second_signature.fields[0].name));
    ft_strlcpy(second_signature.fields[1].name, "other", sizeof(second_signature.fields[1].name));
    first_signature.fields[0].kind = CBLC_DATA_KIND_INT;
    second_signature.fields[0].kind = CBLC_DATA_KIND_INT;
    second_signature.fields[1].kind = CBLC_DATA_KIND_INT;
    first_signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    second_signature.fields[0].visibility = TRANSPILE_SYMBOL_PUBLIC;
    second_signature.fields[1].visibility = TRANSPILE_SYMBOL_PUBLIC;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_decl", NULL),
            "declaration module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_module(&context, "counter_impl", NULL),
            "implementation module registration should succeed") != FT_SUCCESS)
        goto failure;
    if (test_expect_success(transpiler_context_register_type_signature(&context, "counter_decl",
                &first_signature), "declaration type registration should succeed") != FT_SUCCESS)
        goto failure;
    if (transpiler_context_register_type_signature(&context, "counter_impl",
            &second_signature) != FT_FAILURE)
    {
        std::printf("Assertion failed: conflicting public type exports should be rejected\n");
        goto failure;
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
            "conflicting public type exports should emit one diagnostic") != FT_SUCCESS)
        goto failure;
    if (test_expect_int_equal(context.diagnostics.items[0].code,
            TRANSPILE_ERROR_TYPE_EXPORT_CONFLICT,
            "conflicting public type exports should use export conflict code") != FT_SUCCESS)
        goto failure;
    cma_free(first_signature.fields);
    cma_free(second_signature.fields);
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
failure:
    if (first_signature.fields)
        cma_free(first_signature.fields);
    if (second_signature.fields)
        cma_free(second_signature.fields);
    transpiler_context_dispose(&context);
    return (FT_FAILURE);
}

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
