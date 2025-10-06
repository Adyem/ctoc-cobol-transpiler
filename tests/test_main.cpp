#include "test_suites.hpp"

int main(void)
{
    int status;
    size_t count;
    const t_test_case *tests;

    status = FT_SUCCESS;
    tests = get_ast_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_lexer_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_int_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_char_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_string_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_file_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_pipeline_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_compiler_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
