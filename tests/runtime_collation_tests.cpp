#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

static unsigned char runtime_collation_ascii_upper(unsigned char value)
{
    if (value >= 'a' && value <= 'z')
        return (value - ('a' - 'A'));
    return (value);
}

static int runtime_collation_case_insensitive_compare(const char *left, size_t left_length,
    const char *right, size_t right_length, int *result, void *user_data)
{
    size_t index;
    unsigned char left_char;
    unsigned char right_char;

    (void)user_data;
    if (!result)
        return (FT_FAILURE);
    *result = 0;
    index = 0;
    while (index < left_length && index < right_length)
    {
        left_char = static_cast<unsigned char>(left[index]);
        right_char = static_cast<unsigned char>(right[index]);
        left_char = runtime_collation_ascii_upper(left_char);
        right_char = runtime_collation_ascii_upper(right_char);
        if (left_char < right_char)
        {
            *result = -1;
            return (FT_SUCCESS);
        }
        if (left_char > right_char)
        {
            *result = 1;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    if (left_length < right_length)
    {
        *result = -1;
        return (FT_SUCCESS);
    }
    if (left_length > right_length)
    {
        *result = 1;
        return (FT_SUCCESS);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_collation_compare_defaults_to_ascii)
{
    int result;

    runtime_collation_clear_bridge();
    if (test_expect_success(runtime_collation_compare("APPLE", 5, "APRICOT", 7, &result),
            "runtime_collation_compare should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result, -1,
            "runtime_collation_compare should order ASCII lexicographically") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_collation_compare("BETA", 4, "ALPHA", 5, &result),
            "runtime_collation_compare should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result, 1,
            "runtime_collation_compare should compare ASCII buffers") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_collation_compare("SAME", 4, "SAME", 4, &result),
            "runtime_collation_compare should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result, 0,
            "runtime_collation_compare should report equality") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_collation_bridge_registers_case_insensitive)
{
    t_runtime_collation_bridge bridge;
    int result;

    runtime_collation_clear_bridge();
    bridge.compare = runtime_collation_case_insensitive_compare;
    bridge.user_data = NULL;
    if (test_expect_success(runtime_collation_set_bridge(&bridge),
            "runtime_collation_set_bridge should accept callbacks") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_collation_compare("hello", 5, "HELLO", 5, &result),
            "runtime_collation_compare should succeed") != FT_SUCCESS)
    {
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(result, 0,
            "runtime_collation_compare should defer to registered bridge") != FT_SUCCESS)
    {
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    runtime_collation_clear_bridge();
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_compare_uses_collation_bridge)
{
    t_runtime_string left;
    t_runtime_string right;
    t_runtime_collation_bridge bridge;
    int comparison;

    runtime_collation_clear_bridge();
    bridge.compare = runtime_collation_case_insensitive_compare;
    bridge.user_data = NULL;
    if (runtime_collation_set_bridge(&bridge) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_init(&left, 0),
            "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_init(&right, 0),
            "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&left, "gamma"),
            "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "GAMMA"),
            "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&left, &right);
    if (test_expect_int_equal(comparison, 0,
            "runtime_string_compare should use registered collation bridge") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_collation_clear_bridge();
        return (FT_FAILURE);
    }
    runtime_string_dispose(&left);
    runtime_string_dispose(&right);
    runtime_collation_clear_bridge();
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_collation_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_collation_compare_defaults_to_ascii", test_runtime_collation_compare_defaults_to_ascii},
        {"runtime_collation_bridge_registers_case_insensitive", test_runtime_collation_bridge_registers_case_insensitive},
        {"runtime_string_compare_uses_collation_bridge", test_runtime_string_compare_uses_collation_bridge}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
