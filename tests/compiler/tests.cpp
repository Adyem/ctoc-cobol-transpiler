#include "../test_suites.hpp"

#include <cstdlib>

const t_test_case *get_compiler_c_tests(size_t *count);
const t_test_case *get_compiler_cobol_tests(size_t *count);

const t_test_case *get_compiler_tests(size_t *count)
{
    static t_test_case *combined = NULL;
    static size_t combined_count = 0;
    static int initialized = 0;
    const t_test_case *c_tests;
    const t_test_case *cobol_tests;
    size_t c_count;
    size_t cobol_count;
    size_t index;
    size_t offset;

    if (!initialized)
    {
        c_tests = get_compiler_c_tests(&c_count);
        cobol_tests = get_compiler_cobol_tests(&cobol_count);
        combined_count = c_count + cobol_count;
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
            while (index < c_count)
            {
                combined[index] = c_tests[index];
                index += 1;
            }
            offset = index;
            index = 0;
            while (index < cobol_count)
            {
                combined[offset + index] = cobol_tests[index];
                index += 1;
            }
        }
        initialized = 1;
    }
    if (count)
        *count = combined_count;
    return (combined);
}
