#include "test_suites.hpp"

#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

static int cblc_inventory_doc_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/cblc_sample_inventory.md", buffer, buffer_size) != FT_SUCCESS)
    {
        std::printf("Assertion failed: expected docs/cblc_sample_inventory.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_inventory_expect_contains(const char *buffer, const char *snippet)
{
    size_t length;

    if (!buffer)
        return (FT_FAILURE);
    if (!snippet)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length == 0)
        return (FT_FAILURE);
    if (!ft_strnstr(buffer, snippet, length))
    {
        std::printf("Assertion failed: cblc sample inventory should contain snippet:\n%s\n", snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_inventory_expect_sample_block(const char *buffer, const char *sample_path)
{
    char sample_buffer[4096];
    char snippet[8192];
    size_t sample_length;

    if (!buffer)
        return (FT_FAILURE);
    if (!sample_path)
        return (FT_FAILURE);
    if (test_read_text_file(sample_path, sample_buffer, sizeof(sample_buffer)) != FT_SUCCESS)
    {
        std::printf("Assertion failed: expected sample '%s' to be readable\n", sample_path);
        return (FT_FAILURE);
    }
    sample_length = std::strlen(sample_buffer);
    if (ft_strlcpy(snippet, "```cblc\n", sizeof(snippet)) >= sizeof(snippet))
        return (FT_FAILURE);
    if (ft_strlcat(snippet, sample_buffer, sizeof(snippet)) >= sizeof(snippet))
        return (FT_FAILURE);
    if (sample_length == 0 || sample_buffer[sample_length - 1] != '\n')
    {
        if (ft_strlcat(snippet, "\n", sizeof(snippet)) >= sizeof(snippet))
            return (FT_FAILURE);
    }
    if (ft_strlcat(snippet, "```\n", sizeof(snippet)) >= sizeof(snippet))
        return (FT_FAILURE);
    if (cblc_inventory_expect_contains(buffer, snippet) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_inventory_doc_exists)
{
    char buffer[65536];

    if (cblc_inventory_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strlen(buffer) == 0)
    {
        std::printf("Assertion failed: docs/cblc_sample_inventory.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (cblc_inventory_expect_contains(buffer, "CBL-C Sample Inventory") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_inventory_lists_full_samples)
{
    char buffer[65536];

    if (cblc_inventory_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/copy_file.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/filter_prefix.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/record_writer.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/record_summary.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/reverse_constructs.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/reverse_normalization.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_inventory_expect_sample_block(buffer, "samples/cblc/reverse_control_flow.cblc") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_cblc_doc_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cblc_inventory_doc_exists", test_cblc_inventory_doc_exists},
        {"cblc_inventory_lists_full_samples", test_cblc_inventory_lists_full_samples}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
