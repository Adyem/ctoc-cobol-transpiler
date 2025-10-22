#include <filesystem>
#include <string>

#include "test_suites.hpp"

FT_TEST(test_transpiler_context_scans_imports_and_orders_modules)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    const size_t *order;
    size_t order_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", NULL),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_scan_imports_for_module(&context, "main_mod",
            "import \"worker_mod\";\n"),
            "import scan should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to query registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "two modules should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[0].import_count), 0,
            "worker module should not record imports") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[1].import_count), 1,
            "main module should record one import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 2,
            "module order should include both modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[0]].name, "worker_mod",
            "worker module should initialize before main") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[1]].name, "main_mod",
            "main module should initialize after dependencies") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_orders_modules_deterministically)
{
    t_transpiler_context context;
    const size_t *order;
    size_t order_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "delta_mod", NULL),
            "delta module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "alpha module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "charlie_mod", NULL),
            "charlie module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 3,
            "module order should contain three modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[0]].name, "alpha_mod",
            "alpha should appear first when no dependencies exist") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[1]].name, "charlie_mod",
            "charlie should appear second in lexical order") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[2]].name, "delta_mod",
            "delta should appear last in lexical order") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_resolves_imports_by_file_name)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    const size_t *order;
    size_t module_count;
    size_t order_count;
    size_t worker_index;
    size_t main_index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "metrics_main.cblc",
                "samples/cblc/metrics_main.cblc"),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "metrics_worker.cblc",
                "samples/cblc/workers/metrics_worker.cblc"),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "metrics_main.cblc",
                "metrics_worker.cblc"),
            "import by base file name should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "metrics_main.cblc",
                "samples/cblc/workers/metrics_worker.cblc"),
            "import by relative path should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to query registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "two modules should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ft_strncmp(modules[0].name, "metrics_worker.cblc", ft_strlen("metrics_worker.cblc") + 1) == 0)
    {
        worker_index = 0;
        main_index = 1;
    }
    else
    {
        worker_index = 1;
        main_index = 0;
    }
    if (test_expect_cstring_equal(modules[worker_index].name, "metrics_worker.cblc",
            "worker module should retain its base file name") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].name, "metrics_main.cblc",
            "main module should retain its base file name") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].import_count), 1,
            "main module should record a single normalized import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].imports[0].resolved_index),
            static_cast<int>(worker_index),
            "normalized import should resolve to worker module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    {
        std::filesystem::path expected_path;
        std::string expected_string;

        expected_path = std::filesystem::path("samples/cblc/workers/metrics_worker.cblc");
        expected_path = std::filesystem::absolute(expected_path);
        expected_path = expected_path.lexically_normal();
        expected_string = expected_path.string();
        if (test_expect_cstring_equal(modules[main_index].imports[0].path, expected_string.c_str(),
                "import path should normalize to worker module path") != FT_SUCCESS)
        {
            transpiler_context_dispose(&context);
            return (FT_FAILURE);
        }
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 2,
            "module order should include importer and dependency") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order[0]), static_cast<int>(worker_index),
            "worker should initialize before main module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order[1]), static_cast<int>(main_index),
            "main module should initialize after worker") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_empty_module_name)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_register_module(&context, "", NULL) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected empty module name to be rejected\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.module_count), 0,
            "empty name should not register a module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "blank module registration should not record diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_empty_import_path)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_module_import(&context, "main_mod", "") != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected empty import path to be rejected\n");
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to inspect registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 1,
            "only one module should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[0].import_count), 0,
            "empty import should not add dependencies") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_deduplicates_module_imports)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    size_t main_index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", NULL),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "worker_mod"),
            "first import registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "worker_mod"),
            "duplicate import registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to inspect registered modules\n");
        return (FT_FAILURE);
    }
    main_index = 0;
    while (main_index < module_count
        && ft_strncmp(modules[main_index].name, "main_mod", TRANSPILE_MODULE_NAME_MAX) != 0)
        main_index += 1;
    if (main_index >= module_count)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected main module to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].import_count), 1,
            "duplicate imports should not increase import count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].imports[0].path, "worker_mod",
            "import path should be retained after deduplication") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "duplicate imports should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "duplicate imports should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_sorts_module_imports)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    size_t main_index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "alpha_mod", NULL),
            "alpha module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "omega_mod", NULL),
            "omega module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", NULL),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "omega_mod"),
            "first import registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module_import(&context, "main_mod", "alpha_mod"),
            "second import registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to inspect registered modules\n");
        return (FT_FAILURE);
    }
    main_index = 0;
    while (main_index < module_count
        && ft_strncmp(modules[main_index].name, "main_mod", TRANSPILE_MODULE_NAME_MAX) != 0)
        main_index += 1;
    if (main_index >= module_count)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected main module to be registered\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[main_index].import_count), 2,
            "two imports should be tracked for main module") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].imports[0].path, "alpha_mod",
            "imports should be sorted lexicographically") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[main_index].imports[1].path, "omega_mod",
            "imports should retain relative ordering after sort") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
            "sorted imports should not emit diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "sorted imports should not record errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_resolves_imports_by_path)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;
    const size_t *order;
    size_t order_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_register_module(&context, "worker_mod", "modules/worker_mod.cblc"),
            "worker module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_register_module(&context, "main_mod", "modules/main_mod.cblc"),
            "main module registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_scan_imports_for_module(&context, "main_mod",
            "import \"modules/worker_mod.cblc\";\n"),
            "import scan should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to inspect registered modules\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "two modules should be registered") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[1].import_count), 1,
            "main module should record a single path import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    {
        std::filesystem::path expected_path;
        std::string expected_string;

        expected_path = std::filesystem::path("modules/worker_mod.cblc");
        expected_path = std::filesystem::absolute(expected_path);
        expected_path = expected_path.lexically_normal();
        expected_string = expected_path.string();
        if (test_expect_cstring_equal(modules[1].imports[0].path, expected_string.c_str(),
                "path import should normalize to absolute worker path") != FT_SUCCESS)
        {
            transpiler_context_dispose(&context);
            return (FT_FAILURE);
        }
    }
    if (test_expect_success(transpiler_context_compute_module_initialization_order(&context),
            "module order computation should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    order = transpiler_context_get_module_initialization_order(&context, &order_count);
    if (!order)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module order to be available\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(order_count), 2,
            "module order should contain two entries") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.modules[order[0]].name, "worker_mod",
            "worker module should run before main via path import") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(modules[1].imports[0].resolved_index), 0,
            "path import should resolve to worker module index") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_unit_reset_preserves_registered_modules)
{
    t_transpiler_context context;
    const t_transpiler_module *modules;
    size_t module_count;

    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
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
    transpiler_context_reset_unit_state(&context);
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module registry to remain accessible\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 2,
            "unit reset should preserve registered modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[0].name, "alpha_mod",
            "first module should remain registered after unit reset") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(modules[1].name, "beta_mod",
            "second module should remain registered after unit reset") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_reset_module_registry(&context);
    modules = transpiler_context_get_modules(&context, &module_count);
    if (!modules)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected module registry pointer to remain valid\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(module_count), 0,
            "module registry reset should clear registered modules") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

