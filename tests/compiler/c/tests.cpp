#include "../../test_suites.hpp"

#include <cstdlib>

const t_test_case *get_compiler_c_tests(size_t *count);
const t_test_case *get_compiler_c_build_tests(size_t *count);
const t_test_case *get_compiler_c_exit_tests(size_t *count);

const t_test_case *get_compiler_c_tests(size_t *count)
{
    static t_test_case *combined = NULL;
    static size_t combined_count = 0;
    static int initialized = 0;
    const t_test_case *build_tests;
    const t_test_case *exit_tests;
    size_t build_count;
    size_t exit_count;
    size_t index;
    size_t offset;

    if (!initialized)
    {
        build_tests = get_compiler_c_build_tests(&build_count);
        exit_tests = get_compiler_c_exit_tests(&exit_count);
        combined_count = build_count + exit_count;
        if (combined_count > 0)
        {
            combined = static_cast<t_test_case *>(calloc(combined_count, sizeof(t_test_case)));
            if (!combined)
            {
                combined_count = 0;
                initialized = 1;
                if (count)
                    *count = 0;
                return (NULL);
            }
            index = 0;
            while (index < build_count)
            {
                combined[index] = build_tests[index];
                index += 1;
            }
            offset = index;
            index = 0;
            while (index < exit_count)
            {
                combined[offset + index] = exit_tests[index];
                index += 1;
            }
        }
        initialized = 1;
    }
    if (count)
        *count = combined_count;
    return (combined);
}

