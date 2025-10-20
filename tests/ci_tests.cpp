#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int load_file(const char *path, char *buffer, size_t buffer_size)
{
    if (!path)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, buffer_size) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected %s to be readable\n", path);
        return (FT_FAILURE);
    }
    if (ft_strlen(buffer) == 0)
    {
        pf_printf("Assertion failed: expected %s to be non-empty\n", path);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int expect_contains(const char *buffer, const char *snippet, const char *label)
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
        if (label)
            pf_printf("Assertion failed: %s should mention '%s'\n", label, snippet);
        else
            pf_printf("Assertion failed: expected to find '%s' in buffer\n", snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_makefile_defines_ci_targets)
{
    static const char *snippets[] = {
        "\nci:",
        "\nci-build:",
        "\nci-test:",
        "\nci-lint:",
        "\nlint:"
    };
    char buffer[65536];
    size_t index;
    size_t count;

    if (load_file("Makefile", buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(snippets) / sizeof(snippets[0]);
    while (index < count)
    {
        if (expect_contains(buffer, snippets[index], "Makefile") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_makefile_enables_reproducible_builds)
{
    static const char *snippets[] = {
        "REPRODUCIBLE ?= 1",
        "SOURCE_DATE_EPOCH ?=",
        "-ffile-prefix-map=$(CURDIR)=.",
        "-fmacro-prefix-map=$(CURDIR)=.",
        "-fdebug-prefix-map=$(CURDIR)=.",
        "-frandom-seed=ctoc"
    };
    char buffer[65536];
    size_t index;
    size_t count;

    if (load_file("Makefile", buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(snippets) / sizeof(snippets[0]);
    while (index < count)
    {
        if (expect_contains(buffer, snippets[index], "Makefile") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_ci_document_lists_targets)
{
    static const char *commands[] = {
        "make ci", "make ci-build", "make ci-test", "make ci-lint"
    };
    char buffer[65536];
    size_t index;
    size_t count;

    if (load_file("docs/ci_pipeline.md", buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(commands) / sizeof(commands[0]);
    while (index < count)
    {
        if (expect_contains(buffer, commands[index], "docs/ci_pipeline.md") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_ci_document_mentions_lint_script)
{
    char buffer[65536];

    if (load_file("docs/ci_pipeline.md", buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (expect_contains(buffer, "scripts/lint_sources.py", "docs/ci_pipeline.md") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_ci_document_mentions_reproducible_flag)
{
    char buffer[65536];

    if (load_file("docs/ci_pipeline.md", buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (expect_contains(buffer, "REPRODUCIBLE=0", "docs/ci_pipeline.md") != FT_SUCCESS)
        return (FT_FAILURE);
    if (expect_contains(buffer, "SOURCE_DATE_EPOCH", "docs/ci_pipeline.md") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_ci_document_referenced_by_design_doc)
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
        if (ft_strnstr(line, "docs/ci_pipeline.md", ft_strlen(line)))
        {
            ft_fclose(file);
            return (FT_SUCCESS);
        }
    }
    ft_fclose(file);
    pf_printf("Assertion failed: design_doc.txt should reference docs/ci_pipeline.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_ci_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"makefile_defines_ci_targets", test_makefile_defines_ci_targets},
        {"makefile_enables_reproducible_builds", test_makefile_enables_reproducible_builds},
        {"ci_document_lists_targets", test_ci_document_lists_targets},
        {"ci_document_mentions_lint_script", test_ci_document_mentions_lint_script},
        {"ci_document_mentions_reproducible_flag", test_ci_document_mentions_reproducible_flag},
        {"ci_document_referenced_by_design_doc", test_ci_document_referenced_by_design_doc}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
