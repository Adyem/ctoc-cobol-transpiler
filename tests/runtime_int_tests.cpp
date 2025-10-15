#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

FT_TEST(test_runtime_int_add_and_subtract)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, 72);
    runtime_int_set(&right, 30);
    FT_ASSERT_SUCCESS(runtime_int_add(left, right, &result), "runtime_int_add should succeed");
    FT_ASSERT_INT_EQUAL(result.value, 102, "runtime_int_add should add values");
    FT_ASSERT_SUCCESS(runtime_int_subtract(result, left, &result), "runtime_int_subtract should succeed");
    FT_ASSERT_INT_EQUAL(result.value, 30, "runtime_int_subtract should subtract values");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_multiply_and_divide)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, 12);
    runtime_int_set(&right, 6);
    FT_ASSERT_SUCCESS(runtime_int_multiply(left, right, &result), "runtime_int_multiply should succeed");
    FT_ASSERT_INT_EQUAL(result.value, 72, "runtime_int_multiply should multiply values");
    FT_ASSERT_SUCCESS(runtime_int_divide(result, right, &result), "runtime_int_divide should succeed");
    FT_ASSERT_INT_EQUAL(result.value, 12, "runtime_int_divide should divide values");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_divide_rejects_zero)
{
    t_runtime_int dividend;
    t_runtime_int divisor;
    t_runtime_int result;

    runtime_int_set(&dividend, 88);
    runtime_int_set(&divisor, 0);
    runtime_int_set(&result, 41);
    if (runtime_int_divide(dividend, divisor, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_divide should reject division by zero\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(result.value, 41, "runtime_int_divide should preserve destination on failure");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_add_detects_overflow)
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
    FT_ASSERT_INT_EQUAL(result.value, 73, "runtime_int_add should not update on overflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_to_string)
{
    t_runtime_int value;
    char buffer[32];

    runtime_int_set(&value, 512);
    FT_ASSERT_SUCCESS(runtime_int_to_string(value, buffer, sizeof(buffer)), "runtime_int_to_string should succeed");
    if (ft_strncmp(buffer, "512", 4) != 0)
    {
        pf_printf("Assertion failed: runtime_int_to_string should write textual representation\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_to_string_rejects_small_buffer)
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

FT_TEST(test_runtime_int_from_string)
{
    t_runtime_int value;

    runtime_int_set(&value, 48);
    FT_ASSERT_SUCCESS(runtime_int_from_string(&value, "-96"), "runtime_int_from_string should parse text");
    FT_ASSERT_INT_EQUAL(value.value, -96, "runtime_int_from_string should update value");
    if (runtime_int_from_string(&value, "12a") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_from_string should reject invalid input\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(value.value, -96, "runtime_int_from_string should preserve previous value on failure");
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_int_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_int_add_and_subtract", test_runtime_int_add_and_subtract},
        {"runtime_int_multiply_and_divide", test_runtime_int_multiply_and_divide},
        {"runtime_int_divide_rejects_zero", test_runtime_int_divide_rejects_zero},
        {"runtime_int_add_detects_overflow", test_runtime_int_add_detects_overflow},
        {"runtime_int_to_string", test_runtime_int_to_string},
        {"runtime_int_to_string_rejects_small_buffer", test_runtime_int_to_string_rejects_small_buffer},
        {"runtime_int_from_string", test_runtime_int_from_string}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
