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
