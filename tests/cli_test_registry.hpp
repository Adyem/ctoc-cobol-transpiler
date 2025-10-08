#ifndef CLI_TEST_REGISTRY_HPP
#define CLI_TEST_REGISTRY_HPP

#include <cstddef>

#include "test_support.hpp"

const t_test_case *get_cli_parse_success_tests(size_t *count);
const t_test_case *get_cli_parse_failure_tests(size_t *count);

#endif
