#include "test_suites.hpp"

#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

static int runtime_doc_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/runtime_api_reference.md", buffer, buffer_size) != FT_SUCCESS)
    {
        std::printf("Assertion failed: expected docs/runtime_api_reference.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int runtime_doc_expect_contains(const char *buffer, const char *snippet)
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
        std::printf("Assertion failed: runtime API document should mention '%s'\n", snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_doc_exists)
{
    char buffer[65536];

    if (runtime_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strlen(buffer) == 0)
    {
        std::printf("Assertion failed: docs/runtime_api_reference.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (runtime_doc_expect_contains(buffer, "Runtime Helper API Reference") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_doc_lists_modules)
{
    static const char *snippets[] = {
        "runtime_scalar",
        "runtime_string",
        "runtime_memory",
        "runtime_sort",
        "runtime_csv",
        "runtime_collation",
        "runtime_encoding",
        "runtime_record",
        "runtime_file"
    };
    char buffer[65536];
    size_t index;
    size_t count;

    if (runtime_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(snippets) / sizeof(snippets[0]);
    while (index < count)
    {
        if (runtime_doc_expect_contains(buffer, snippets[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_doc_mentions_cma)
{
    char buffer[65536];

    if (runtime_doc_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_doc_expect_contains(buffer, "cma_calloc") != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_doc_expect_contains(buffer, "cma_free") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_design_doc_references_runtime_doc)
{
    FILE *file;
    char line[1024];

    file = std::fopen("design_doc.txt", "r");
    if (!file)
    {
        std::printf("Assertion failed: design_doc.txt should be readable\n");
        return (FT_FAILURE);
    }
    while (std::fgets(line, sizeof(line), file))
    {
        if (ft_strnstr(line, "docs/runtime_api_reference.md", std::strlen(line)))
        {
            std::fclose(file);
            return (FT_SUCCESS);
        }
    }
    std::fclose(file);
    std::printf("Assertion failed: design_doc.txt should reference docs/runtime_api_reference.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_runtime_doc_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_doc_exists", test_runtime_doc_exists},
        {"runtime_doc_lists_modules", test_runtime_doc_lists_modules},
        {"runtime_doc_mentions_cma", test_runtime_doc_mentions_cma},
        {"design_doc_references_runtime_doc", test_design_doc_references_runtime_doc}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
