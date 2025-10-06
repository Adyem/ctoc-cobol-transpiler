#include "runtime_scalar.hpp"

#include "test_suites.hpp"

static int test_runtime_int_add_and_subtract(void)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, 72);
    runtime_int_set(&right, 30);
    if (test_expect_success(runtime_int_add(left, right, &result), "runtime_int_add should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result.value, 102, "runtime_int_add should add values") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_int_subtract(result, left, &result), "runtime_int_subtract should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result.value, 30, "runtime_int_subtract should subtract values") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_int_add_detects_overflow(void)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, FT_INT_MAX);
    runtime_int_set(&right, 1);
    runtime_int_set(&result, 73);
    if (runtime_int_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_add should detect overflow\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(result.value, 73, "runtime_int_add should not update on overflow") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_int_to_string(void)
{
    t_runtime_int value;
    char buffer[32];

    runtime_int_set(&value, 512);
    if (test_expect_success(runtime_int_to_string(value, buffer, sizeof(buffer)), "runtime_int_to_string should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strncmp(buffer, "512", 4) != 0)
    {
        pf_printf("Assertion failed: runtime_int_to_string should write textual representation\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_runtime_int_to_string_rejects_small_buffer(void)
{
    t_runtime_int value;
    char buffer[2];

    runtime_int_set(&value, 1024);
    if (runtime_int_to_string(value, buffer, sizeof(buffer)) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_to_string should reject insufficient buffer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_runtime_int_from_string(void)
{
    t_runtime_int value;

    runtime_int_set(&value, 48);
    if (test_expect_success(runtime_int_from_string("-96", &value), "runtime_int_from_string should parse text") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(value.value, -96, "runtime_int_from_string should update value") != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_int_from_string("12a", &value) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_from_string should reject invalid input\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, -96, "runtime_int_from_string should preserve previous value on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_int_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_int_add_and_subtract", test_runtime_int_add_and_subtract},
        {"runtime_int_add_detects_overflow", test_runtime_int_add_detects_overflow},
        {"runtime_int_to_string", test_runtime_int_to_string},
        {"runtime_int_to_string_rejects_small_buffer", test_runtime_int_to_string_rejects_small_buffer},
        {"runtime_int_from_string", test_runtime_int_from_string}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
