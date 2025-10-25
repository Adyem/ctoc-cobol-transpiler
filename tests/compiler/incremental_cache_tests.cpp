#include "../test_suites.hpp"

#include "compiler_test_support.hpp"
#include "../test_support.hpp"
#include "cblc_transpiler.hpp"

const t_test_case *get_compiler_incremental_cache_tests(size_t *count);

FT_TEST(test_compiler_incremental_cache_records_and_skips)
{
    t_transpiler_incremental_cache cache;
    char directory[256];
    char input_path[256];
    char output_path[256];
    char manifest_path[256];
    int directory_created;
    int status;
    int should_skip;

    directory_created = 0;
    status = FT_FAILURE;
    if (transpiler_incremental_cache_init(&cache) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "input.cob", input_path, sizeof(input_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "output.cblc", output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "cache.manifest", manifest_path, sizeof(manifest_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, "IDENTIFICATION DIVISION.\n") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(output_path, "program-id. sample.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_set_manifest(&cache, manifest_path) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_load(&cache) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_record(&cache, input_path, output_path, NULL, 0) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_save(&cache) != FT_SUCCESS)
        goto cleanup;
    transpiler_incremental_cache_dispose(&cache);
    if (transpiler_incremental_cache_init(&cache) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_set_manifest(&cache, manifest_path) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_load(&cache) != FT_SUCCESS)
        goto cleanup;
    should_skip = 0;
    if (transpiler_incremental_cache_should_skip(&cache, input_path, output_path, 0, &should_skip) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(should_skip, 1, "cache should skip unchanged inputs") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, "IDENTIFICATION DIVISION.\nCHANGED.\n") != FT_SUCCESS)
        goto cleanup;
    should_skip = 1;
    if (transpiler_incremental_cache_should_skip(&cache, input_path, output_path, 0, &should_skip) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(should_skip, 0, "cache should rebuild when inputs change") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_incremental_cache_dispose(&cache);
    if (directory_created)
        test_remove_directory(directory);
    return (status);
}

FT_TEST(test_compiler_incremental_cache_detects_copybook_changes)
{
    t_transpiler_incremental_cache cache;
    char directory[256];
    char input_path[256];
    char output_path[256];
    char manifest_path[256];
    int directory_created;
    int status;
    int should_skip;

    directory_created = 0;
    status = FT_FAILURE;
    if (transpiler_incremental_cache_init(&cache) != FT_SUCCESS)
        goto cleanup;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "input.cob", input_path, sizeof(input_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "output.cblc", output_path, sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "cache.manifest", manifest_path, sizeof(manifest_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(input_path, "IDENTIFICATION DIVISION.\n") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(output_path, "program-id. sample.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_set_manifest(&cache, manifest_path) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_load(&cache) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_record(&cache, input_path, output_path, NULL, 123ULL) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_save(&cache) != FT_SUCCESS)
        goto cleanup;
    transpiler_incremental_cache_dispose(&cache);
    if (transpiler_incremental_cache_init(&cache) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_set_manifest(&cache, manifest_path) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_incremental_cache_load(&cache) != FT_SUCCESS)
        goto cleanup;
    should_skip = 0;
    if (transpiler_incremental_cache_should_skip(&cache, input_path, output_path, 123ULL, &should_skip) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(should_skip, 1, "cache should skip when copybook signature matches") != FT_SUCCESS)
        goto cleanup;
    should_skip = 1;
    if (transpiler_incremental_cache_should_skip(&cache, input_path, output_path, 456ULL, &should_skip) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(should_skip, 0, "cache should rebuild when copybook signature differs") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_incremental_cache_dispose(&cache);
    if (directory_created)
        test_remove_directory(directory);
    return (status);
}

static const t_test_case g_compiler_incremental_cache_tests[] = {
    {"compiler_incremental_cache_records_and_skips", test_compiler_incremental_cache_records_and_skips},
    {"compiler_incremental_cache_detects_copybook_changes", test_compiler_incremental_cache_detects_copybook_changes},
};

const t_test_case *get_compiler_incremental_cache_tests(size_t *count)
{
    if (count)
        *count = sizeof(g_compiler_incremental_cache_tests) / sizeof(t_test_case);
    return (g_compiler_incremental_cache_tests);
}
