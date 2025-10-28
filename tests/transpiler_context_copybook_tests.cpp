#include "test_suites.hpp"

FT_TEST(test_transpiler_context_registers_copybook_metadata)
{
    t_transpiler_context context;
    t_transpiler_copybook_item items[2];
    const t_transpiler_copybook *copybook;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(items, sizeof(items));
    ft_strlcpy(items[0].name, "COPY-ALPHA", sizeof(items[0].name));
    items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[0].declared_length = 10;
    ft_strlcpy(items[1].name, "COPY-NUMERIC", sizeof(items[1].name));
    items[1].kind = TRANSPILE_DATA_ITEM_NUMERIC;
    items[1].declared_length = 0;
    if (test_expect_success(transpiler_context_register_copybook(&context, "MASTER-COPY", items, 2),
            "copybook registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    copybook = transpiler_context_find_copybook(&context, "MASTER-COPY");
    if (!copybook)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected to locate registered copybook metadata\n");
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(copybook->item_count, 2,
            "copybook should persist item count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(copybook->items[0].name, "COPY-ALPHA",
            "first copybook item name should match") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(copybook->items[0].kind, TRANSPILE_DATA_ITEM_ALPHANUMERIC,
            "first copybook item should be alphanumeric") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(copybook->items[0].declared_length, 10,
            "first copybook item should record declared length") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 0,
            "copybook registration should not raise errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_rejects_duplicate_copybook)
{
    t_transpiler_context context;
    t_transpiler_copybook_item items[1];
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(items, sizeof(items));
    ft_strlcpy(items[0].name, "ONLY-FIELD", sizeof(items[0].name));
    items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[0].declared_length = 5;
    if (transpiler_context_register_copybook(&context, "DUP-COPY", items, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_copybook(&context, "DUP-COPY", items, 1) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1
            && context.diagnostics.count >= 1
            && context.diagnostics.items[0].code == TRANSPILE_ERROR_COPYBOOK_DUPLICATE)
            status = FT_SUCCESS;
    }
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_transpiler_context_records_copybook_replacements)
{
    t_transpiler_context context;
    t_transpiler_copybook_replacement_view views[2];
    const t_transpiler_copybook *copybook;

    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_register_copybook(&context, "SALES-BOOK", NULL, 0) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    ft_bzero(views, sizeof(views));
    views[0].pair_flags = AST_NODE_FLAG_COPYBOOK_PAIR_LEADING;
    views[0].source.text = "CUSTOMER";
    views[0].source.length = ft_strlen("CUSTOMER");
    views[0].source.flags = AST_NODE_FLAG_COPYBOOK_TEXT_WORD | AST_NODE_FLAG_COPYBOOK_TEXT_HAS_OF;
    views[0].source.qualifier = "TABLE";
    views[0].source.qualifier_length = ft_strlen("TABLE");
    views[0].target.text = "CLIENT";
    views[0].target.length = ft_strlen("CLIENT");
    views[0].target.flags = AST_NODE_FLAG_COPYBOOK_TEXT_DELIMITED;
    views[1].pair_flags = AST_NODE_FLAG_COPYBOOK_PAIR_TRAILING;
    views[1].source.text = "OLD";
    views[1].source.length = ft_strlen("OLD");
    views[1].source.flags = 0;
    views[1].target.text = "NEW";
    views[1].target.length = ft_strlen("NEW");
    views[1].target.flags = AST_NODE_FLAG_COPYBOOK_TEXT_WORD;
    if (transpiler_context_register_copybook_replacements(&context, "SALES-BOOK",
            views, 2) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    copybook = transpiler_context_find_copybook(&context, "SALES-BOOK");
    if (!copybook)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected registered copybook\n");
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(copybook->replacement_count, 2,
            "copybook should retain replacement count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(copybook->replacements[0].pair_flags,
            AST_NODE_FLAG_COPYBOOK_PAIR_LEADING, "first replacement should record leading flag") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(copybook->replacements[0].source.text, "CUSTOMER",
            "first replacement should preserve source text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(copybook->replacements[0].source.qualifier, "TABLE",
            "first replacement should copy qualifier") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(copybook->replacements[0].target.flags,
            AST_NODE_FLAG_COPYBOOK_TEXT_DELIMITED, "first replacement target should mark delimited text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(copybook->replacements[1].pair_flags,
            AST_NODE_FLAG_COPYBOOK_PAIR_TRAILING, "second replacement should record trailing flag") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(copybook->replacements[1].target.flags,
            AST_NODE_FLAG_COPYBOOK_TEXT_WORD, "second replacement target should mark word flag") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_copybook_replacements(&context, "SALES-BOOK", NULL, 0) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(copybook->replacement_count, 0,
            "copybook replacements should clear when no views are provided") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

