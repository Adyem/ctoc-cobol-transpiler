#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int cobol_dialect_doc_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/cobol_dialect_requirements.md", buffer, buffer_size) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected docs/cobol_dialect_requirements.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cobol_dialect_doc_expect_contains(const char *buffer, const char *needle)
{
    size_t length;

    if (!buffer)
        return (FT_FAILURE);
    if (!needle)
        return (FT_FAILURE);
    length = ft_strlen(buffer);
    if (length == 0)
        return (FT_FAILURE);
    if (!ft_strnstr(buffer, needle, length))
    {
        pf_printf("Assertion failed: dialect document should contain '%s'\n", needle);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_cobol_dialect_doc_exists(void)
{
    char buffer[4096];

    if (cobol_dialect_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strlen(buffer) == 0)
    {
        pf_printf("Assertion failed: docs/cobol_dialect_requirements.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (cobol_dialect_doc_expect_contains(buffer, "COBOL Dialect Requirements") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_cobol_dialect_doc_lists_constraints(void)
{
    static const char *needles[] = {
        "ANSI-85 compatible COBOL",
        "Unsupported Features",
        "`STRING` / `UNSTRING`",
        "OCCURS DEPENDING ON",
        "void-only function signatures",
        "`void main()` entrypoint"
    };
    char buffer[8192];
    size_t index;
    size_t count;

    if (cobol_dialect_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(needles) / sizeof(needles[0]);
    while (index < count)
    {
        if (cobol_dialect_doc_expect_contains(buffer, needles[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int test_design_doc_references_cobol_dialect_doc(void)
{
    char line[1024];
    FILE *file;

    file = ft_fopen("design_doc.txt", "r");
    if (!file)
    {
        pf_printf("Assertion failed: design_doc.txt should be readable\n");
        return (FT_FAILURE);
    }
    while (ft_fgets(line, sizeof(line), file))
    {
        if (ft_strnstr(line, "docs/cobol_dialect_requirements.md", ft_strlen(line)))
        {
            ft_fclose(file);
            return (FT_SUCCESS);
        }
    }
    ft_fclose(file);
    pf_printf("Assertion failed: design_doc.txt should reference docs/cobol_dialect_requirements.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_cobol_doc_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_dialect_doc_exists", test_cobol_dialect_doc_exists},
        {"cobol_dialect_doc_lists_constraints", test_cobol_dialect_doc_lists_constraints},
        {"design_doc_references_cobol_dialect_doc", test_design_doc_references_cobol_dialect_doc}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
