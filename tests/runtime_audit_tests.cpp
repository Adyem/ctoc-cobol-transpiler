#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int runtime_audit_expect_no_forbidden_allocator(const char *path)
{
    static const char *forbidden_tokens[] = {
        "malloc(",
        " malloc",
        "\tmalloc",
        " realloc",
        "\trealloc",
        " free(",
        " free ",
        " new ",
        " new(",
        " delete",
        "operator new",
        "operator delete"
    };
    char buffer[65536];
    size_t token_index;
    size_t token_count;
    size_t length;

    if (!path)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected %s to be readable\n", path);
        return (FT_FAILURE);
    }
    length = ft_strlen(buffer);
    if (length == 0)
    {
        pf_printf("Assertion failed: expected %s to contain source code\n", path);
        return (FT_FAILURE);
    }
    token_index = 0;
    token_count = sizeof(forbidden_tokens) / sizeof(forbidden_tokens[0]);
    while (token_index < token_count)
    {
        const char *token;

        token = forbidden_tokens[token_index];
        if (ft_strnstr(buffer, token, length))
        {
            pf_printf("Assertion failed: %s should not use allocator token '%s'\n", path, token);
            return (FT_FAILURE);
        }
        token_index += 1;
    }
    return (FT_SUCCESS);
}

static int test_runtime_helpers_use_cma_allocators(void)
{
    static const char *runtime_files[] = {
        "runtime_record.cpp",
        "runtime_string.cpp"
    };
    size_t index;
    size_t count;

    index = 0;
    count = sizeof(runtime_files) / sizeof(runtime_files[0]);
    while (index < count)
    {
        if (runtime_audit_expect_no_forbidden_allocator(runtime_files[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int runtime_audit_expect_cma_include(const char *path)
{
    char buffer[65536];
    size_t length;

    if (!path)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected %s to be readable\n", path);
        return (FT_FAILURE);
    }
    length = ft_strlen(buffer);
    if (!ft_strnstr(buffer, "\"libft/CMA/CMA.hpp\"", length))
    {
        pf_printf("Assertion failed: %s should include libft/CMA/CMA.hpp to use CMA allocators\n", path);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_runtime_helpers_include_cma_headers(void)
{
    static const char *runtime_files[] = {
        "runtime_record.cpp",
        "runtime_string.cpp"
    };
    size_t index;
    size_t count;

    index = 0;
    count = sizeof(runtime_files) / sizeof(runtime_files[0]);
    while (index < count)
    {
        if (runtime_audit_expect_cma_include(runtime_files[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_audit_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_helpers_use_cma_allocators", test_runtime_helpers_use_cma_allocators},
        {"runtime_helpers_include_cma_headers", test_runtime_helpers_include_cma_headers}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
