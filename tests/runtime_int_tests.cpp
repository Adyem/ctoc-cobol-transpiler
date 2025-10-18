#include "cblc_transpiler.hpp"

#include "test_suites.hpp"
#include <climits>

static int test_expect_long_equal(long actual, long expected, const char *message)
{
    if (actual != expected)
    {
        pf_printf("Assertion failed: %s (expected %ld, actual %ld)\n", message, expected, actual);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

#define FT_ASSERT_LONG_EQUAL(actual, expected, message) \
    if (test_expect_long_equal((actual), (expected), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

static int test_expect_long_long_equal(long long actual, long long expected, const char *message)
{
    if (actual != expected)
    {
        pf_printf("Assertion failed: %s (expected %lld, actual %lld)\n", message, expected, actual);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

#define FT_ASSERT_LLONG_EQUAL(actual, expected, message) \
    if (test_expect_long_long_equal((actual), (expected), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

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

FT_TEST(test_runtime_int_add_handles_negative_operands)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, -40);
    runtime_int_set(&right, 15);
    FT_ASSERT_SUCCESS(runtime_int_add(left, right, &result),
        "runtime_int_add should handle mixed signs");
    FT_ASSERT_INT_EQUAL(result.value, -25,
        "runtime_int_add should subtract when adding negative to positive");
    runtime_int_set(&left, -40);
    runtime_int_set(&right, -15);
    FT_ASSERT_SUCCESS(runtime_int_add(left, right, &result),
        "runtime_int_add should add two negative values");
    FT_ASSERT_INT_EQUAL(result.value, -55,
        "runtime_int_add should preserve sign when both operands are negative");
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

FT_TEST(test_runtime_int_multiply_detects_overflow)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, FT_INT_MAX);
    runtime_int_set(&right, 2);
    runtime_int_set(&result, 31);
    if (runtime_int_multiply(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_multiply should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(result.value, 31, "runtime_int_multiply should preserve destination on overflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_multiply_handles_negative_and_zero)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, -8);
    runtime_int_set(&right, 4);
    FT_ASSERT_SUCCESS(runtime_int_multiply(left, right, &result),
        "runtime_int_multiply should support negative values");
    FT_ASSERT_INT_EQUAL(result.value, -32,
        "runtime_int_multiply should preserve sign when multiplying");
    runtime_int_set(&right, 0);
    FT_ASSERT_SUCCESS(runtime_int_multiply(left, right, &result),
        "runtime_int_multiply should support zero operands");
    FT_ASSERT_INT_EQUAL(result.value, 0,
        "runtime_int_multiply should produce zero when one operand is zero");
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

FT_TEST(test_runtime_int_divide_detects_overflow)
{
    t_runtime_int dividend;
    t_runtime_int divisor;
    t_runtime_int result;

    runtime_int_set(&dividend, FT_INT_MIN);
    runtime_int_set(&divisor, -1);
    runtime_int_set(&result, 17);
    if (runtime_int_divide(dividend, divisor, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_divide should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(result.value, 17, "runtime_int_divide should preserve destination on overflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_divide_handles_negative_operands)
{
    t_runtime_int dividend;
    t_runtime_int divisor;
    t_runtime_int result;

    runtime_int_set(&dividend, -81);
    runtime_int_set(&divisor, 9);
    FT_ASSERT_SUCCESS(runtime_int_divide(dividend, divisor, &result),
        "runtime_int_divide should divide negative numbers by positive");
    FT_ASSERT_INT_EQUAL(result.value, -9,
        "runtime_int_divide should retain the correct sign");
    runtime_int_set(&dividend, 81);
    runtime_int_set(&divisor, -9);
    FT_ASSERT_SUCCESS(runtime_int_divide(dividend, divisor, &result),
        "runtime_int_divide should divide positive numbers by negative");
    FT_ASSERT_INT_EQUAL(result.value, -9,
        "runtime_int_divide should retain the correct sign for mixed operands");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_unary_plus_returns_input)
{
    t_runtime_int value;
    t_runtime_int result;

    runtime_int_set(&value, -71);
    runtime_int_set(&result, 24);
    FT_ASSERT_SUCCESS(runtime_int_unary_plus(value, &result),
        "runtime_int_unary_plus should copy operand value");
    FT_ASSERT_INT_EQUAL(result.value, -71,
        "runtime_int_unary_plus should preserve operand sign");
    runtime_int_set(&value, 0);
    FT_ASSERT_SUCCESS(runtime_int_unary_plus(value, &result),
        "runtime_int_unary_plus should accept zero operand");
    FT_ASSERT_INT_EQUAL(result.value, 0,
        "runtime_int_unary_plus should return zero for zero operand");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_unary_minus_negates_values)
{
    t_runtime_int value;
    t_runtime_int result;

    runtime_int_set(&value, 29);
    runtime_int_set(&result, 13);
    FT_ASSERT_SUCCESS(runtime_int_unary_minus(value, &result),
        "runtime_int_unary_minus should negate positive values");
    FT_ASSERT_INT_EQUAL(result.value, -29,
        "runtime_int_unary_minus should produce negative value for positive input");
    runtime_int_set(&value, -46);
    FT_ASSERT_SUCCESS(runtime_int_unary_minus(value, &result),
        "runtime_int_unary_minus should negate negative values");
    FT_ASSERT_INT_EQUAL(result.value, 46,
        "runtime_int_unary_minus should flip sign for negative input");
    runtime_int_set(&value, 0);
    FT_ASSERT_SUCCESS(runtime_int_unary_minus(value, &result),
        "runtime_int_unary_minus should accept zero operand");
    FT_ASSERT_INT_EQUAL(result.value, 0,
        "runtime_int_unary_minus should return zero for zero operand");
    runtime_int_set(&value, FT_INT_MIN);
    runtime_int_set(&result, 77);
    if (runtime_int_unary_minus(value, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_unary_minus should detect overflow for minimum value\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(result.value, 77,
        "runtime_int_unary_minus should preserve destination on overflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_absolute_returns_magnitude)
{
    t_runtime_int value;
    t_runtime_int result;

    runtime_int_set(&value, -93);
    runtime_int_set(&result, 51);
    FT_ASSERT_SUCCESS(runtime_int_absolute(value, &result),
        "runtime_int_absolute should return magnitude for negative operand");
    FT_ASSERT_INT_EQUAL(result.value, 93,
        "runtime_int_absolute should convert negative operand to positive magnitude");
    runtime_int_set(&value, 84);
    FT_ASSERT_SUCCESS(runtime_int_absolute(value, &result),
        "runtime_int_absolute should accept positive operand");
    FT_ASSERT_INT_EQUAL(result.value, 84,
        "runtime_int_absolute should preserve positive operand");
    runtime_int_set(&value, 0);
    FT_ASSERT_SUCCESS(runtime_int_absolute(value, &result),
        "runtime_int_absolute should accept zero operand");
    FT_ASSERT_INT_EQUAL(result.value, 0,
        "runtime_int_absolute should return zero for zero operand");
    runtime_int_set(&value, FT_INT_MIN);
    runtime_int_set(&result, 32);
    if (runtime_int_absolute(value, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_absolute should detect overflow for minimum value\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(result.value, 32,
        "runtime_int_absolute should preserve destination on overflow");
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

FT_TEST(test_runtime_int_subtract_detects_underflow)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, FT_INT_MIN);
    runtime_int_set(&right, 1);
    runtime_int_set(&result, -51);
    if (runtime_int_subtract(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_subtract should detect underflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_INT_EQUAL(result.value, -51,
        "runtime_int_subtract should not update destination on underflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_subtract_handles_negative_operands)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, -20);
    runtime_int_set(&right, -5);
    FT_ASSERT_SUCCESS(runtime_int_subtract(left, right, &result),
        "runtime_int_subtract should subtract negative operands");
    FT_ASSERT_INT_EQUAL(result.value, -15,
        "runtime_int_subtract should compute difference of negative operands");
    runtime_int_set(&left, 20);
    runtime_int_set(&right, -5);
    FT_ASSERT_SUCCESS(runtime_int_subtract(left, right, &result),
        "runtime_int_subtract should subtract a negative from positive");
    FT_ASSERT_INT_EQUAL(result.value, 25,
        "runtime_int_subtract should produce addition when subtracting negative");
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

FT_TEST(test_runtime_int_compare_orders_values)
{
    t_runtime_int left;
    t_runtime_int right;

    runtime_int_set(&left, -4);
    runtime_int_set(&right, 9);
    FT_ASSERT_INT_EQUAL(runtime_int_compare(left, right), -1,
        "runtime_int_compare should report left smaller than right");
    runtime_int_set(&left, 12);
    FT_ASSERT_INT_EQUAL(runtime_int_compare(left, right), 1,
        "runtime_int_compare should report left larger than right");
    runtime_int_set(&right, 12);
    FT_ASSERT_INT_EQUAL(runtime_int_compare(left, right), 0,
        "runtime_int_compare should report equality when values match");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_long_unary_operations)
{
    t_runtime_long value;
    t_runtime_long result;

    runtime_long_set(&value, 123456789L);
    runtime_long_set(&result, 0L);
    FT_ASSERT_SUCCESS(runtime_long_unary_plus(value, &result),
        "runtime_long_unary_plus should copy operand value");
    FT_ASSERT_LONG_EQUAL(result.value, 123456789L,
        "runtime_long_unary_plus should not adjust magnitude");
    runtime_long_set(&value, -987654321L);
    FT_ASSERT_SUCCESS(runtime_long_unary_minus(value, &result),
        "runtime_long_unary_minus should negate negative values");
    FT_ASSERT_LONG_EQUAL(result.value, 987654321L,
        "runtime_long_unary_minus should flip operand sign");
    FT_ASSERT_SUCCESS(runtime_long_absolute(value, &result),
        "runtime_long_absolute should return magnitude");
    FT_ASSERT_LONG_EQUAL(result.value, 987654321L,
        "runtime_long_absolute should discard operand sign");
    runtime_long_set(&value, 0L);
    FT_ASSERT_SUCCESS(runtime_long_unary_minus(value, &result),
        "runtime_long_unary_minus should accept zero operands");
    FT_ASSERT_LONG_EQUAL(result.value, 0L,
        "runtime_long_unary_minus should return zero for zero operand");
    runtime_long_set(&value, LONG_MIN);
    runtime_long_set(&result, 314L);
    if (runtime_long_unary_minus(value, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_unary_minus should detect overflow for minimum value\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, 314L,
        "runtime_long_unary_minus should preserve destination on overflow");
    if (runtime_long_absolute(value, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_absolute should detect overflow for minimum value\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, 314L,
        "runtime_long_absolute should preserve destination on overflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_long_arithmetic_operations)
{
    t_runtime_long left;
    t_runtime_long right;
    t_runtime_long result;

    runtime_long_set(&left, 120000L);
    runtime_long_set(&right, 345000L);
    runtime_long_set(&result, 0L);
    FT_ASSERT_SUCCESS(runtime_long_add(left, right, &result),
        "runtime_long_add should add operands within range");
    FT_ASSERT_LONG_EQUAL(result.value, 465000L,
        "runtime_long_add should produce expected sum");
    FT_ASSERT_SUCCESS(runtime_long_subtract(result, right, &result),
        "runtime_long_subtract should subtract operands within range");
    FT_ASSERT_LONG_EQUAL(result.value, 120000L,
        "runtime_long_subtract should restore left operand");
    runtime_long_set(&left, -64000L);
    runtime_long_set(&right, 3L);
    FT_ASSERT_SUCCESS(runtime_long_multiply(left, right, &result),
        "runtime_long_multiply should multiply operands within range");
    FT_ASSERT_LONG_EQUAL(result.value, -192000L,
        "runtime_long_multiply should compute product");
    runtime_long_set(&left, -192000L);
    runtime_long_set(&right, -6L);
    FT_ASSERT_SUCCESS(runtime_long_divide(left, right, &result),
        "runtime_long_divide should divide operands within range");
    FT_ASSERT_LONG_EQUAL(result.value, 32000L,
        "runtime_long_divide should compute quotient with sign");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_long_arithmetic_detects_overflow)
{
    t_runtime_long left;
    t_runtime_long right;
    t_runtime_long result;

    runtime_long_set(&left, LONG_MAX);
    runtime_long_set(&right, 1L);
    runtime_long_set(&result, -9L);
    if (runtime_long_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_add should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, -9L,
        "runtime_long_add should preserve destination on overflow");
    runtime_long_set(&left, LONG_MIN);
    runtime_long_set(&right, -1L);
    runtime_long_set(&result, 103L);
    if (runtime_long_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_add should detect underflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, 103L,
        "runtime_long_add should preserve destination on underflow");
    runtime_long_set(&left, LONG_MIN);
    runtime_long_set(&right, 1L);
    runtime_long_set(&result, 27L);
    if (runtime_long_subtract(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_subtract should detect underflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, 27L,
        "runtime_long_subtract should preserve destination on underflow");
    runtime_long_set(&left, LONG_MAX);
    runtime_long_set(&right, -1L);
    runtime_long_set(&result, -63L);
    if (runtime_long_subtract(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_subtract should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, -63L,
        "runtime_long_subtract should preserve destination on overflow");
    runtime_long_set(&left, LONG_MAX);
    runtime_long_set(&right, 2L);
    runtime_long_set(&result, 19L);
    if (runtime_long_multiply(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_multiply should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, 19L,
        "runtime_long_multiply should preserve destination on overflow");
    runtime_long_set(&left, LONG_MIN);
    runtime_long_set(&right, -1L);
    runtime_long_set(&result, 45L);
    if (runtime_long_divide(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_divide should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, 45L,
        "runtime_long_divide should preserve destination on overflow");
    runtime_long_set(&left, 100L);
    runtime_long_set(&right, 0L);
    runtime_long_set(&result, -73L);
    if (runtime_long_divide(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_divide should reject division by zero\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LONG_EQUAL(result.value, -73L,
        "runtime_long_divide should preserve destination on division by zero");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_long_long_unary_operations)
{
    t_runtime_long_long value;
    t_runtime_long_long result;

    runtime_long_long_set(&value, 9223372036854770000LL);
    runtime_long_long_set(&result, -1LL);
    FT_ASSERT_SUCCESS(runtime_long_long_unary_plus(value, &result),
        "runtime_long_long_unary_plus should copy operand value");
    FT_ASSERT_LLONG_EQUAL(result.value, 9223372036854770000LL,
        "runtime_long_long_unary_plus should not adjust magnitude");
    runtime_long_long_set(&value, -4444444444444444444LL);
    FT_ASSERT_SUCCESS(runtime_long_long_unary_minus(value, &result),
        "runtime_long_long_unary_minus should negate negative operands");
    FT_ASSERT_LLONG_EQUAL(result.value, 4444444444444444444LL,
        "runtime_long_long_unary_minus should flip operand sign");
    FT_ASSERT_SUCCESS(runtime_long_long_absolute(value, &result),
        "runtime_long_long_absolute should return magnitude");
    FT_ASSERT_LLONG_EQUAL(result.value, 4444444444444444444LL,
        "runtime_long_long_absolute should discard operand sign");
    runtime_long_long_set(&value, 0LL);
    FT_ASSERT_SUCCESS(runtime_long_long_unary_minus(value, &result),
        "runtime_long_long_unary_minus should accept zero operands");
    FT_ASSERT_LLONG_EQUAL(result.value, 0LL,
        "runtime_long_long_unary_minus should return zero for zero operand");
    runtime_long_long_set(&value, LLONG_MIN);
    runtime_long_long_set(&result, -21LL);
    if (runtime_long_long_unary_minus(value, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_unary_minus should detect overflow for minimum value\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, -21LL,
        "runtime_long_long_unary_minus should preserve destination on overflow");
    if (runtime_long_long_absolute(value, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_absolute should detect overflow for minimum value\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, -21LL,
        "runtime_long_long_absolute should preserve destination on overflow");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_long_long_arithmetic_operations)
{
    t_runtime_long_long left;
    t_runtime_long_long right;
    t_runtime_long_long result;

    runtime_long_long_set(&left, 4000000000000LL);
    runtime_long_long_set(&right, 6000000000000LL);
    runtime_long_long_set(&result, 0LL);
    FT_ASSERT_SUCCESS(runtime_long_long_add(left, right, &result),
        "runtime_long_long_add should add operands within range");
    FT_ASSERT_LLONG_EQUAL(result.value, 10000000000000LL,
        "runtime_long_long_add should compute sum");
    FT_ASSERT_SUCCESS(runtime_long_long_subtract(result, right, &result),
        "runtime_long_long_subtract should subtract operands within range");
    FT_ASSERT_LLONG_EQUAL(result.value, 4000000000000LL,
        "runtime_long_long_subtract should restore left operand");
    runtime_long_long_set(&left, -3500000000LL);
    runtime_long_long_set(&right, 2000000LL);
    FT_ASSERT_SUCCESS(runtime_long_long_multiply(left, right, &result),
        "runtime_long_long_multiply should multiply operands within range");
    FT_ASSERT_LLONG_EQUAL(result.value, -7000000000000000LL,
        "runtime_long_long_multiply should compute product");
    runtime_long_long_set(&left, -7000000000000000LL);
    runtime_long_long_set(&right, -3500LL);
    FT_ASSERT_SUCCESS(runtime_long_long_divide(left, right, &result),
        "runtime_long_long_divide should divide operands within range");
    FT_ASSERT_LLONG_EQUAL(result.value, 2000000000000LL,
        "runtime_long_long_divide should compute quotient with sign");
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_long_long_arithmetic_detects_overflow)
{
    t_runtime_long_long left;
    t_runtime_long_long right;
    t_runtime_long_long result;

    runtime_long_long_set(&left, LLONG_MAX);
    runtime_long_long_set(&right, 1LL);
    runtime_long_long_set(&result, -41LL);
    if (runtime_long_long_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_add should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, -41LL,
        "runtime_long_long_add should preserve destination on overflow");
    runtime_long_long_set(&left, LLONG_MIN);
    runtime_long_long_set(&right, -1LL);
    runtime_long_long_set(&result, 207LL);
    if (runtime_long_long_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_add should detect underflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, 207LL,
        "runtime_long_long_add should preserve destination on underflow");
    runtime_long_long_set(&left, LLONG_MIN);
    runtime_long_long_set(&right, 1LL);
    runtime_long_long_set(&result, 93LL);
    if (runtime_long_long_subtract(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_subtract should detect underflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, 93LL,
        "runtime_long_long_subtract should preserve destination on underflow");
    runtime_long_long_set(&left, LLONG_MAX);
    runtime_long_long_set(&right, -1LL);
    runtime_long_long_set(&result, -187LL);
    if (runtime_long_long_subtract(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_subtract should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, -187LL,
        "runtime_long_long_subtract should preserve destination on overflow");
    runtime_long_long_set(&left, LLONG_MAX);
    runtime_long_long_set(&right, 2LL);
    runtime_long_long_set(&result, -57LL);
    if (runtime_long_long_multiply(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_multiply should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, -57LL,
        "runtime_long_long_multiply should preserve destination on overflow");
    runtime_long_long_set(&left, LLONG_MIN);
    runtime_long_long_set(&right, -1LL);
    runtime_long_long_set(&result, 121LL);
    if (runtime_long_long_divide(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_divide should detect overflow\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, 121LL,
        "runtime_long_long_divide should preserve destination on overflow");
    runtime_long_long_set(&left, 42LL);
    runtime_long_long_set(&right, 0LL);
    runtime_long_long_set(&result, 512LL);
    if (runtime_long_long_divide(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_long_long_divide should reject division by zero\n");
        return (FT_FAILURE);
    }
    FT_ASSERT_LLONG_EQUAL(result.value, 512LL,
        "runtime_long_long_divide should preserve destination on division by zero");
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_int_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_int_add_and_subtract", test_runtime_int_add_and_subtract},
        {"runtime_int_add_handles_negative_operands", test_runtime_int_add_handles_negative_operands},
        {"runtime_int_multiply_and_divide", test_runtime_int_multiply_and_divide},
        {"runtime_int_multiply_detects_overflow", test_runtime_int_multiply_detects_overflow},
        {"runtime_int_multiply_handles_negative_and_zero", test_runtime_int_multiply_handles_negative_and_zero},
        {"runtime_int_divide_rejects_zero", test_runtime_int_divide_rejects_zero},
        {"runtime_int_divide_detects_overflow", test_runtime_int_divide_detects_overflow},
        {"runtime_int_divide_handles_negative_operands", test_runtime_int_divide_handles_negative_operands},
        {"runtime_int_unary_plus_returns_input", test_runtime_int_unary_plus_returns_input},
        {"runtime_int_unary_minus_negates_values", test_runtime_int_unary_minus_negates_values},
        {"runtime_int_absolute_returns_magnitude", test_runtime_int_absolute_returns_magnitude},
        {"runtime_int_add_detects_overflow", test_runtime_int_add_detects_overflow},
        {"runtime_int_subtract_detects_underflow", test_runtime_int_subtract_detects_underflow},
        {"runtime_int_subtract_handles_negative_operands", test_runtime_int_subtract_handles_negative_operands},
        {"runtime_int_to_string", test_runtime_int_to_string},
        {"runtime_int_to_string_rejects_small_buffer", test_runtime_int_to_string_rejects_small_buffer},
        {"runtime_int_from_string", test_runtime_int_from_string},
        {"runtime_int_compare_orders_values", test_runtime_int_compare_orders_values},
        {"runtime_long_unary_operations", test_runtime_long_unary_operations},
        {"runtime_long_arithmetic_operations", test_runtime_long_arithmetic_operations},
        {"runtime_long_arithmetic_detects_overflow", test_runtime_long_arithmetic_detects_overflow},
        {"runtime_long_long_unary_operations", test_runtime_long_long_unary_operations},
        {"runtime_long_long_arithmetic_operations", test_runtime_long_long_arithmetic_operations},
        {"runtime_long_long_arithmetic_detects_overflow", test_runtime_long_long_arithmetic_detects_overflow}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
