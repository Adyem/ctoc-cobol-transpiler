#include "test_suites.hpp"

#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

static int language_standard_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/cblc_language_standard.md", buffer, buffer_size) != FT_SUCCESS)
    {
        std::printf("Assertion failed: expected docs/cblc_language_standard.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_language_standard_exists)
{
    char buffer[65536];

    if (language_standard_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strlen(buffer) == 0)
    {
        std::printf("Assertion failed: docs/cblc_language_standard.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "## 1. Scope and conformance", std::strlen(buffer)))
    {
        std::printf("Assertion failed: language standard should define conformance\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int language_standard_expect_rule(const char *buffer, const char *rule)
{
    size_t length;

    if (!buffer)
        return (FT_FAILURE);
    if (!rule)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length == 0)
        return (FT_FAILURE);
    if (!ft_strnstr(buffer, rule, length))
    {
        std::printf("Assertion failed: language standard should include '%s'\n", rule);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_language_standard_lists_core_rules)
{
    static const char *rules[] = {
        "## 2. Compiler pipeline",
        "## 6. Type system",
        "## 8. Functions, methods, and calls",
        "## 9. Expressions",
        "## 10. Statements and control flow",
        "## 13. Diagnostics and rejection behavior",
        "## 14. Target behavior",
        "## 16. Extension framework",
        "## 17. Conformance evidence and change procedure"
    };
    char buffer[65536];
    size_t index;
    size_t count;

    if (language_standard_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(rules) / sizeof(rules[0]);
    while (index < count)
    {
        if (language_standard_expect_rule(buffer, rules[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_language_standard_is_authoritative)
{
    char line[1024];
    FILE *file;

    file = std::fopen("docs/cblc_language_standard.md", "r");
    if (!file)
    {
        std::printf("Assertion failed: docs/cblc_language_standard.md should be readable\n");
        return (FT_FAILURE);
    }
    while (std::fgets(line, sizeof(line), file))
    {
        if (ft_strnstr(line, "authoritative repository specification", std::strlen(line)))
        {
            std::fclose(file);
            return (FT_SUCCESS);
        }
    }
    std::fclose(file);
    std::printf("Assertion failed: language standard should identify itself as authoritative\n");
    return (FT_FAILURE);
}

const t_test_case *get_grammar_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"language_standard_exists", test_language_standard_exists},
        {"language_standard_lists_core_rules", test_language_standard_lists_core_rules},
        {"language_standard_is_authoritative", test_language_standard_is_authoritative}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
