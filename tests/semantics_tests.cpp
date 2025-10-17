#include "semantics_test_groups.hpp"

const t_test_case *get_semantics_tests(size_t *count)
{
    static t_test_case tests[80];
    static size_t total_count = 0;
    static int initialized = 0;
    size_t offset;
    size_t subset_count;
    const t_test_case *subset;
    size_t index;

    if (!initialized)
    {
        offset = 0;
        subset = get_semantics_assignment_tests(&subset_count);
        index = 0;
        while (index < subset_count)
        {
            tests[offset + index] = subset[index];
            index++;
        }
        offset += subset_count;
        subset = get_semantics_arithmetic_tests(&subset_count);
        index = 0;
        while (index < subset_count)
        {
            tests[offset + index] = subset[index];
            index++;
        }
        offset += subset_count;
        subset = get_semantics_condition_tests(&subset_count);
        index = 0;
        while (index < subset_count)
        {
            tests[offset + index] = subset[index];
            index++;
        }
        offset += subset_count;
        total_count = offset;
        initialized = 1;
    }
    if (count)
        *count = total_count;
    return (tests);
}

