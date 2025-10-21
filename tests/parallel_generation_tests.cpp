#include <cstdlib>

#include "test_suites.hpp"

static int test_set_environment(const char *name, const char *value)
{
#if defined(_WIN32)
    if (_putenv_s(name, value) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
#else
    if (setenv(name, value, 1) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
#endif
}

static void test_unset_environment(const char *name)
{
#if defined(_WIN32)
    _putenv_s(name, "");
#else
    unsetenv(name);
#endif
}

FT_TEST(test_parallel_generate_cobol_handles_empty_jobs)
{
    t_transpiler_parallel_result *results;

    results = NULL;
    if (test_expect_success(transpiler_parallel_generate_cobol(NULL, NULL, 0, &results),
            "empty job list should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (results)
        transpiler_parallel_results_dispose(results, 0);
    return (FT_SUCCESS);
}

FT_TEST(test_parallel_generate_cobol_emits_outputs_for_multiple_units)
{
    t_cblc_translation_unit unit_one;
    t_cblc_translation_unit unit_two;
    const t_cblc_translation_unit *units[2];
    const char *paths[2];
    t_transpiler_parallel_result *results;
    const char *unit_one_source;
    const char *unit_two_source;
    int environment_set;
    int status;

    cblc_translation_unit_init(&unit_one);
    cblc_translation_unit_init(&unit_two);
    results = NULL;
    environment_set = 0;
    status = FT_FAILURE;
    unit_one_source = "int counter;\n"
        "function void main()\n"
        "{\n"
        "    counter = 5;\n"
        "    return;\n"
        "}\n";
    unit_two_source = "int shared;\n"
        "function void helper()\n"
        "{\n"
        "    shared = 7;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(cblc_parse_translation_unit(unit_one_source, &unit_one),
            "first unit should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(unit_two_source, &unit_two),
            "second unit should parse") != FT_SUCCESS)
        goto cleanup;
    units[0] = &unit_one;
    units[1] = &unit_two;
    paths[0] = "module_one.cblc";
    paths[1] = "module_two.cblc";
    if (test_set_environment("CTOC_PARALLELISM", "2") != FT_SUCCESS)
        goto cleanup;
    environment_set = 1;
    if (test_expect_success(transpiler_parallel_generate_cobol(units, paths, 2, &results),
            "parallel generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (!results)
        goto cleanup;
    if (test_expect_int_equal(results[0].status, FT_SUCCESS,
            "first result should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(results[1].status, FT_SUCCESS,
            "second result should succeed") != FT_SUCCESS)
        goto cleanup;
    if (!results[0].text || !results[1].text)
        goto cleanup;
    if (!ft_strnstr(results[0].text, "PROGRAM-ID.", ft_strlen(results[0].text)))
        goto cleanup;
    if (!ft_strnstr(results[1].text, "PROGRAM-ID.", ft_strlen(results[1].text)))
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (environment_set)
        test_unset_environment("CTOC_PARALLELISM");
    if (results)
    {
        transpiler_parallel_results_dispose(results, 2);
        results = NULL;
    }
    cblc_translation_unit_dispose(&unit_one);
    cblc_translation_unit_dispose(&unit_two);
    return (status);
}
