#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int onboarding_doc_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/onboarding_checklist.md", buffer, buffer_size) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected docs/onboarding_checklist.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int onboarding_doc_expect_contains(const char *buffer, const char *snippet)
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
        pf_printf("Assertion failed: onboarding checklist should mention '%s'\n", snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_onboarding_doc_exists(void)
{
    char buffer[65536];

    if (onboarding_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strlen(buffer) == 0)
    {
        pf_printf("Assertion failed: docs/onboarding_checklist.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (onboarding_doc_expect_contains(buffer, "Developer Onboarding Checklist") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_onboarding_doc_covers_repository_setup(void)
{
    char buffer[65536];

    if (onboarding_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (onboarding_doc_expect_contains(buffer, "Repository Setup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (onboarding_doc_expect_contains(buffer, "git clone --recurse-submodules") != FT_SUCCESS)
        return (FT_FAILURE);
    if (onboarding_doc_expect_contains(buffer, "make initialize") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_onboarding_doc_highlights_testing_steps(void)
{
    char buffer[65536];

    if (onboarding_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (onboarding_doc_expect_contains(buffer, "Test Execution") != FT_SUCCESS)
        return (FT_FAILURE);
    if (onboarding_doc_expect_contains(buffer, "make tests") != FT_SUCCESS)
        return (FT_FAILURE);
    if (onboarding_doc_expect_contains(buffer, "make test") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_design_doc_links_onboarding_checklist(void)
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
        if (ft_strnstr(line, "docs/onboarding_checklist.md", ft_strlen(line)))
        {
            ft_fclose(file);
            return (FT_SUCCESS);
        }
    }
    ft_fclose(file);
    pf_printf("Assertion failed: design_doc.txt should reference docs/onboarding_checklist.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_onboarding_doc_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"onboarding_doc_exists", test_onboarding_doc_exists},
        {"onboarding_doc_covers_repository_setup", test_onboarding_doc_covers_repository_setup},
        {"onboarding_doc_highlights_testing_steps", test_onboarding_doc_highlights_testing_steps},
        {"design_doc_links_onboarding_checklist", test_design_doc_links_onboarding_checklist}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
