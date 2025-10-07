#include "../../test_suites.hpp"

#include <cstdlib>

const t_test_case *get_compiler_cobol_tests(size_t *count);
const t_test_case *get_compiler_cobol_return_numeric_tests(size_t *count);
const t_test_case *get_compiler_cobol_multi_module_tests(size_t *count);
const t_test_case *get_compiler_cobol_return_boolean_tests(size_t *count);
const t_test_case *get_compiler_cobol_return_character_tests(size_t *count);

const t_test_case *get_compiler_cobol_tests(size_t *count)
{
    static t_test_case *combined = NULL;
    static size_t combined_count = 0;
    static int initialized = 0;
    const t_test_case *numeric_tests;
    const t_test_case *multi_module_tests;
    const t_test_case *boolean_tests;
    const t_test_case *character_tests;
    size_t numeric_count;
    size_t multi_module_count;
    size_t boolean_count;
    size_t character_count;
    size_t index;
    size_t offset;

    if (!initialized)
    {
        numeric_tests = get_compiler_cobol_return_numeric_tests(&numeric_count);
        multi_module_tests = get_compiler_cobol_multi_module_tests(&multi_module_count);
        boolean_tests = get_compiler_cobol_return_boolean_tests(&boolean_count);
        character_tests = get_compiler_cobol_return_character_tests(&character_count);
        combined_count = numeric_count + multi_module_count + boolean_count + character_count;
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
            while (index < numeric_count)
            {
                combined[index] = numeric_tests[index];
                index += 1;
            }
            offset = index;
            index = 0;
            while (index < multi_module_count)
            {
                combined[offset + index] = multi_module_tests[index];
                index += 1;
            }
            offset += multi_module_count;
            index = 0;
            while (index < boolean_count)
            {
                combined[offset + index] = boolean_tests[index];
                index += 1;
            }
            offset += boolean_count;
            index = 0;
            while (index < character_count)
            {
                combined[offset + index] = character_tests[index];
                index += 1;
            }
        }
        initialized = 1;
    }
    if (count)
        *count = combined_count;
    return (combined);
}

