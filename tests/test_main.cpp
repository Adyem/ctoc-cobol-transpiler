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
    tests = get_parser_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_semantics_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_codegen_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_round_trip_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_golden_file_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cobol_type_tests(&count);
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
    tests = get_standard_library_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_audit_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_record_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_file_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_pipeline_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_logging_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_transpiler_context_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_compiler_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_sample_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_grammar_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cobol_doc_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cobol_execution_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cli_doc_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cblc_doc_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_runtime_doc_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_contributing_doc_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_onboarding_doc_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cli_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_ci_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    tests = get_cobol_reverse_tests(&count);
    if (run_test_suite(tests, count) != FT_SUCCESS)
        status = FT_FAILURE;
    test_report_summary();
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
