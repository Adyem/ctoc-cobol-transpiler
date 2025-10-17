#include "test_suites.hpp"

#define STANDARD_LIBRARY_TEST(name) \
    int test_##name(void);
#include "standard_library/standard_library_test_list.hpp"
#undef STANDARD_LIBRARY_TEST

static const t_test_case g_standard_library_tests[] = {
#define STANDARD_LIBRARY_TEST(name) \
    {#name, test_##name},
#include "standard_library/standard_library_test_list.hpp"
#undef STANDARD_LIBRARY_TEST
};

const t_test_case *get_standard_library_tests(size_t *count)
{
    if (count)
        *count = sizeof(g_standard_library_tests) / sizeof(g_standard_library_tests[0]);
    return (g_standard_library_tests);
}
