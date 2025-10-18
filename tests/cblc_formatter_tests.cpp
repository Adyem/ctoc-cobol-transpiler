#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

#include "libft/CMA/CMA.hpp"

FT_TEST(test_cblc_formatter_formats_function_body)
{
    const char *input;
    const char *expected;
    char *output;

    input = "function void sample(){char value[4];if(value==NULL){return;}else{value[0]='x';}}";
    expected = "function void sample()\n"
        "{\n"
        "    char value[4];\n"
        "    if (value == NULL)\n"
        "    {\n"
        "        return;\n"
        "    }\n"
        "    else\n"
        "    {\n"
        "        value[0] = 'x';\n"
        "    }\n"
        "}\n";
    if (test_expect_success(cblc_formatter_format(input, TRANSPILE_FORMAT_PRETTY, &output),
            "formatter should succeed for structured function") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(output, expected,
            "formatter should emit canonical Allman layout") != FT_SUCCESS)
    {
        cma_free(output);
        return (FT_FAILURE);
    }
    cma_free(output);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_formatter_preserves_strings_and_comments)
{
    const char *input;
    const char *expected;
    char *output;

    input = "function void log(){//comment\nwrite(out,\"hello\\n\");}";
    expected = "function void log()\n"
        "{\n"
        "    //comment\n"
        "    write(out, \"hello\\n\");\n"
        "}\n";
    if (test_expect_success(cblc_formatter_format(input, TRANSPILE_FORMAT_PRETTY, &output),
            "formatter should succeed for strings and comments") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(output, expected,
            "formatter should retain escape sequences and comments") != FT_SUCCESS)
    {
        cma_free(output);
        return (FT_FAILURE);
    }
    cma_free(output);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_formatter_passthrough_minimal_mode)
{
    const char *input;
    char *output;

    input = "function void noop(){}";
    if (test_expect_success(cblc_formatter_format(input, TRANSPILE_FORMAT_MINIMAL, &output),
            "minimal mode should copy text without formatting") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(output, input,
            "minimal mode should preserve original layout") != FT_SUCCESS)
    {
        cma_free(output);
        return (FT_FAILURE);
    }
    cma_free(output);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cblc_apply_layout_normalize)
{
    const char *input;
    const char *expected;
    char *output;

    input = "function void sample(){return;}";
    expected = "function void sample()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_cblc_apply_layout(input, TRANSPILE_LAYOUT_NORMALIZE,
                TRANSPILE_FORMAT_PRETTY, &output),
            "normalize layout should format output") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(output, expected,
            "normalize layout should produce formatted text") != FT_SUCCESS)
    {
        cma_free(output);
        return (FT_FAILURE);
    }
    cma_free(output);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cblc_apply_layout_preserve)
{
    const char *input;
    char *output;

    input = "function void sample(){return;}";
    if (test_expect_success(transpiler_cblc_apply_layout(input, TRANSPILE_LAYOUT_PRESERVE,
                TRANSPILE_FORMAT_PRETTY, &output),
            "preserve layout should copy input") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(output, input,
            "preserve layout should keep original spacing") != FT_SUCCESS)
    {
        cma_free(output);
        return (FT_FAILURE);
    }
    cma_free(output);
    return (FT_SUCCESS);
}

const t_test_case *get_cblc_formatter_tests(size_t *count)
{
    static t_test_case tests[] = {
        {"test_cblc_formatter_formats_function_body", test_cblc_formatter_formats_function_body},
        {"test_cblc_formatter_preserves_strings_and_comments", test_cblc_formatter_preserves_strings_and_comments},
        {"test_cblc_formatter_passthrough_minimal_mode", test_cblc_formatter_passthrough_minimal_mode},
        {"test_transpiler_cblc_apply_layout_normalize", test_transpiler_cblc_apply_layout_normalize},
        {"test_transpiler_cblc_apply_layout_preserve", test_transpiler_cblc_apply_layout_preserve}
    };
    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
