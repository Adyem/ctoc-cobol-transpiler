#ifndef IMPORTED_TEST_RUNNER_HPP
#define IMPORTED_TEST_RUNNER_HPP

#include <cstddef>

typedef int (*t_imported_test_func)(void);

#ifndef TEST_MODULE
#define TEST_MODULE "Libft"
#endif

int imported_test_register(t_imported_test_func func, const char *description, const char *module);
void imported_test_fail(const char *expression, const char *file, int line);
int imported_test_run_registered(void);

#define IMPORTED_FT_TEST(name, description) \
    static int name(void); \
    static void register_##name(void) __attribute__((constructor)); \
    static void register_##name(void) \
    { \
        imported_test_register(name, description, TEST_MODULE); \
        return ; \
    } \
    static int name(void)

#define IMPORTED_FT_ASSERT(expression) \
    do \
    { \
        if (!(expression)) \
        { \
            imported_test_fail(#expression, __FILE__, __LINE__); \
            return (0); \
        } \
    } while (0)

#define IMPORTED_FT_ASSERT_EQ(expected, actual) \
    do \
    { \
        if ((expected) != (actual)) \
        { \
            imported_test_fail(#expected " == " #actual, __FILE__, __LINE__); \
            return (0); \
        } \
    } while (0)

#endif
