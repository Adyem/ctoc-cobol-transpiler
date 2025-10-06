#ifndef TEST_SUITES_HPP
#define TEST_SUITES_HPP

#include <cstddef>

#include "test_support.hpp"

const t_test_case *get_ast_tests(size_t *count);
const t_test_case *get_lexer_tests(size_t *count);
const t_test_case *get_runtime_int_tests(size_t *count);
const t_test_case *get_runtime_char_tests(size_t *count);
const t_test_case *get_runtime_string_tests(size_t *count);
const t_test_case *get_runtime_file_tests(size_t *count);
const t_test_case *get_pipeline_tests(size_t *count);
const t_test_case *get_compiler_tests(size_t *count);
const t_test_case *get_cli_tests(size_t *count);
const t_test_case *get_sample_tests(size_t *count);
const t_test_case *get_grammar_tests(size_t *count);
const t_test_case *get_cobol_doc_tests(size_t *count);
const t_test_case *get_transpiler_context_tests(size_t *count);

#endif
