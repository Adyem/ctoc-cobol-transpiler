#ifndef SEMANTICS_TEST_GROUPS_HPP
#define SEMANTICS_TEST_GROUPS_HPP

#include <cstddef>

#include "test_suites.hpp"

const t_test_case *get_semantics_assignment_tests(size_t *count);
const t_test_case *get_semantics_arithmetic_tests(size_t *count);
const t_test_case *get_semantics_condition_tests(size_t *count);
const t_test_case *get_semantics_control_flow_tests(size_t *count);
const t_test_case *get_semantics_usage_tests(size_t *count);

#endif
