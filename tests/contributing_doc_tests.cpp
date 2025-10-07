#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int contributing_doc_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/contributing.md", buffer, buffer_size) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected docs/contributing.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int contributing_doc_expect_contains(const char *buffer, const char *snippet)
{
    size_t length;

    if (!buffer)
        return (FT_FAILURE);
    if (!snippet)
        return (FT_FAILURE);
    length = ft_strlen(buffer);
    if (length == 0)
        return (FT_FAILURE);
    if (!ft_strnstr(buffer, snippet, length))
    {
        pf_printf("Assertion failed: contributing guide should mention '%s'\n", snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_contributing_doc_exists)
{
    char buffer[65536];

    if (contributing_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strlen(buffer) == 0)
    {
        pf_printf("Assertion failed: docs/contributing.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (contributing_doc_expect_contains(buffer, "Contributor Guide") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_contributing_doc_spells_out_coding_standards)
{
    char buffer[65536];

    if (contributing_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (contributing_doc_expect_contains(buffer, "Coding Standards") != FT_SUCCESS)
        return (FT_FAILURE);
    if (contributing_doc_expect_contains(buffer, "Allman-style braces") != FT_SUCCESS)
        return (FT_FAILURE);
    if (contributing_doc_expect_contains(buffer, "snake_case") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_contributing_doc_mentions_testing_policy)
{
    char buffer[65536];

    if (contributing_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (contributing_doc_expect_contains(buffer, "Testing Expectations") != FT_SUCCESS)
        return (FT_FAILURE);
    if (contributing_doc_expect_contains(buffer, "Every feature must include tests") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_design_doc_references_contributing_doc)
{
    FILE *file;
    char line[1024];

    file = ft_fopen("design_doc.txt", "r");
    if (!file)
    {
        pf_printf("Assertion failed: design_doc.txt should be readable\n");
        return (FT_FAILURE);
    }
    while (ft_fgets(line, sizeof(line), file))
    {
        if (ft_strnstr(line, "docs/contributing.md", ft_strlen(line)))
        {
            ft_fclose(file);
            return (FT_SUCCESS);
        }
    }
    ft_fclose(file);
    pf_printf("Assertion failed: design_doc.txt should reference docs/contributing.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_contributing_doc_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"contributing_doc_exists", test_contributing_doc_exists},
        {"contributing_doc_spells_out_coding_standards", test_contributing_doc_spells_out_coding_standards},
        {"contributing_doc_mentions_testing_policy", test_contributing_doc_mentions_testing_policy},
        {"design_doc_references_contributing_doc", test_design_doc_references_contributing_doc}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
