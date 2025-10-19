#ifndef TEST_SUITES_HPP
#define TEST_SUITES_HPP

#include <cstddef>

#include "test_support.hpp"

const t_test_case *get_ast_tests(size_t *count);
const t_test_case *get_lexer_tests(size_t *count);
const t_test_case *get_parser_tests(size_t *count);
const t_test_case *get_semantics_tests(size_t *count);
const t_test_case *get_codegen_tests(size_t *count);
const t_test_case *get_round_trip_tests(size_t *count);
const t_test_case *get_property_tests(size_t *count);
const t_test_case *get_cobol_type_tests(size_t *count);
const t_test_case *get_golden_file_tests(size_t *count);
const t_test_case *get_runtime_int_tests(size_t *count);
const t_test_case *get_runtime_char_tests(size_t *count);
const t_test_case *get_runtime_string_tests(size_t *count);
const t_test_case *get_runtime_csv_tests(size_t *count);
const t_test_case *get_runtime_collation_tests(size_t *count);
const t_test_case *get_runtime_encoding_tests(size_t *count);
const t_test_case *get_standard_library_tests(size_t *count);
const t_test_case *get_runtime_audit_tests(size_t *count);
const t_test_case *get_runtime_record_tests(size_t *count);
const t_test_case *get_runtime_sort_tests(size_t *count);
const t_test_case *get_runtime_file_tests(size_t *count);
const t_test_case *get_pipeline_tests(size_t *count);
const t_test_case *get_compiler_tests(size_t *count);
const t_test_case *get_cli_tests(size_t *count);
const t_test_case *get_sample_tests(size_t *count);
const t_test_case *get_grammar_tests(size_t *count);
const t_test_case *get_cobol_doc_tests(size_t *count);
const t_test_case *get_cobol_execution_tests(size_t *count);
const t_test_case *get_cli_doc_tests(size_t *count);
const t_test_case *get_runtime_doc_tests(size_t *count);
const t_test_case *get_contributing_doc_tests(size_t *count);
const t_test_case *get_onboarding_doc_tests(size_t *count);
const t_test_case *get_cblc_doc_tests(size_t *count);
const t_test_case *get_transpiler_context_tests(size_t *count);
const t_test_case *get_logging_tests(size_t *count);
const t_test_case *get_cobol_reverse_tests(size_t *count);
const t_test_case *get_ci_tests(size_t *count);
const t_test_case *get_cblc_formatter_tests(size_t *count);
const t_test_case *get_validation_tests(size_t *count);

#endif
