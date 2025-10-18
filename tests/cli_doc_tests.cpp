#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int cli_usage_document_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/cli_usage_examples.md", buffer, buffer_size) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected docs/cli_usage_examples.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cli_usage_document_expect_contains(const char *buffer, const char *snippet)
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
        pf_printf("Assertion failed: CLI usage document should mention '%s'\n", snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cli_usage_document_exists)
{
    char buffer[32768];

    if (cli_usage_document_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strlen(buffer) == 0)
    {
        pf_printf("Assertion failed: docs/cli_usage_examples.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (cli_usage_document_expect_contains(buffer, "ctoc_cobol_transpiler --direction") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_usage_lists_supported_flags)
{
    static const char *snippets[] = {
        "--direction",
        "--input",
        "--output",
        "--output-dir",
        "--format",
        "--layout",
        "--diagnostics",
        "--warnings-as-errors",
        "--help"
    };
    char buffer[32768];
    size_t index;
    size_t count;

    if (cli_usage_document_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(snippets) / sizeof(snippets[0]);
    while (index < count)
    {
        if (cli_usage_document_expect_contains(buffer, snippets[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cli_usage_mentions_environment_variable)
{
    char buffer[32768];

    if (cli_usage_document_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cli_usage_document_expect_contains(buffer, "CTOC_DEFAULT_DIRECTION") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cli_usage_document_expect_contains(buffer, "export CTOC_DEFAULT_DIRECTION") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_design_doc_references_cli_usage_document)
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
        if (ft_strnstr(line, "docs/cli_usage_examples.md", ft_strlen(line)))
        {
            ft_fclose(file);
            return (FT_SUCCESS);
        }
    }
    ft_fclose(file);
    pf_printf("Assertion failed: design_doc.txt should reference docs/cli_usage_examples.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_cli_doc_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cli_usage_document_exists", test_cli_usage_document_exists},
        {"cli_usage_lists_supported_flags", test_cli_usage_lists_supported_flags},
        {"cli_usage_mentions_environment_variable", test_cli_usage_mentions_environment_variable},
        {"design_doc_references_cli_usage_document", test_design_doc_references_cli_usage_document}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
