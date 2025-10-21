#include "standard_library_test_support.hpp"

FT_TEST(test_runtime_helper_registry_lists_entries)
{
    const t_transpiler_runtime_helper_entry *entries;
    size_t count;

    entries = transpiler_runtime_helpers_get_entries(&count);
    if (!entries || count == 0)
    {
        pf_printf("Assertion failed: runtime helper registry should expose at least one entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[0].identifier, "cblc_min_size", ft_strlen("cblc_min_size") + 1) != 0)
    {
        pf_printf("Assertion failed: first runtime helper identifier should be 'cblc_min_size'\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_helper_registry_renders_source)
{
    char *source;

    source = NULL;
    if (transpiler_runtime_helpers_render_c_source(&source) != FT_SUCCESS || !source)
    {
        pf_printf("Assertion failed: runtime helper renderer should produce aggregated source text\n");
        if (source)
            cma_free(source);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(source, "cblc_display_literal", ft_strlen(source)))
    {
        pf_printf("Assertion failed: aggregated runtime helper source should contain cblc_display_literal implementation\n");
        cma_free(source);
        return (FT_FAILURE);
    }
    cma_free(source);
    return (FT_SUCCESS);
}
