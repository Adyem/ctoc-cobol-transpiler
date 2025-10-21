#include "test_suites.hpp"
#include "cli_test_registry.hpp"

const t_test_case *get_cli_tests(size_t *count)
{
    static t_test_case tests[64];
    static size_t total_count = 0;
    static int initialized = 0;

    if (!initialized)
    {
        size_t subset_count;
        const t_test_case *subset;
        size_t index = 0;

        subset = get_cli_parse_success_tests(&subset_count);
        while (index < subset_count)
        {
            tests[total_count] = subset[index];
            total_count++;
            index++;
        }
        subset = get_cli_parse_failure_tests(&subset_count);
        index = 0;
        while (index < subset_count)
        {
            tests[total_count] = subset[index];
            total_count++;
            index++;
        }
        subset = get_cli_standard_library_tests(&subset_count);
        index = 0;
        while (index < subset_count)
        {
            tests[total_count] = subset[index];
            total_count++;
            index++;
        }
        initialized = 1;
    }
    if (count)
        *count = total_count;
    return (tests);
}
