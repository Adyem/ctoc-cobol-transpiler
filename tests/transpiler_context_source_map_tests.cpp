#include "test_suites.hpp"

FT_TEST(test_transpiler_context_records_source_map_entry)
{
    t_transpiler_context context;
    t_transpiler_source_span cblc_span;
    t_transpiler_source_span cobol_span;
    const t_transpiler_source_map_entry *entry;
    const t_transpiler_source_map_entry *reverse_entry;
    size_t map_count;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(&cblc_span, sizeof(cblc_span));
    ft_bzero(&cobol_span, sizeof(cobol_span));
    ft_strlcpy(cblc_span.path, "modern.cblc", TRANSPILE_FILE_PATH_MAX);
    cblc_span.start_line = 7;
    cblc_span.start_column = 3;
    cblc_span.end_line = 9;
    cblc_span.end_column = 14;
    ft_strlcpy(cobol_span.path, "legacy.cob", TRANSPILE_FILE_PATH_MAX);
    cobol_span.start_line = 42;
    cobol_span.start_column = 11;
    cobol_span.end_line = 44;
    cobol_span.end_column = 27;
    if (test_expect_success(transpiler_context_record_source_map_entry(&context, &cblc_span, &cobol_span),
            "source map registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    entry = transpiler_context_map_cblc_to_cobol(&context, "modern.cblc", 8, 5);
    if (!entry)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate cblc to cobol mapping\n");
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(entry->cobol_span.path, "legacy.cob",
            "mapped cobol span should expose registered path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    reverse_entry = transpiler_context_map_cobol_to_cblc(&context, "legacy.cob", 43, 13);
    if (!reverse_entry)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate cobol to cblc mapping\n");
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(reverse_entry->cblc_span.path, "modern.cblc",
            "mapped cblc span should expose registered path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_get_source_maps(&context, &map_count);
    if (test_expect_int_equal(static_cast<int>(map_count), 1,
            "context should track a single source map entry") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_invalid_source_map_span)
{
    t_transpiler_context context;
    t_transpiler_source_span cblc_span;
    t_transpiler_source_span cobol_span;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(&cblc_span, sizeof(cblc_span));
    ft_bzero(&cobol_span, sizeof(cobol_span));
    ft_strlcpy(cblc_span.path, "broken.cblc", TRANSPILE_FILE_PATH_MAX);
    cblc_span.start_line = 5;
    cblc_span.start_column = 2;
    cblc_span.end_line = 4;
    cblc_span.end_column = 20;
    ft_strlcpy(cobol_span.path, "broken.cob", TRANSPILE_FILE_PATH_MAX);
    cobol_span.start_line = 8;
    cobol_span.start_column = 1;
    cobol_span.end_line = 9;
    cobol_span.end_column = 30;
    if (transpiler_context_record_source_map_entry(&context, &cblc_span, &cobol_span) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected invalid span registration to fail\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.source_map_count), 0,
            "context should not retain rejected source map entry") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

