#include "runtime_scalar.hpp"
#include "runtime_string.hpp"

#include "test_suites.hpp"

FT_TEST(test_runtime_string_assign_and_trim)
{
    t_runtime_string value;

    FT_ASSERT_SUCCESS(runtime_string_init(&value, 0), "runtime_string_init should succeed");
    if (test_expect_success(runtime_string_assign(&value, "   hello world  "), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_trim(&value), "runtime_string_trim should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(value.data, "hello world", "runtime_string_trim should remove leading and trailing whitespace") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(value.length), 11, "runtime_string_trim should update length") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&value);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_compare_orders_text)
{
    t_runtime_string left;
    t_runtime_string right;
    int comparison;

    if (test_expect_success(runtime_string_init(&left, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_init(&right, 0), "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&left, "APPLE"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "APRICOT"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&left, &right);
    if (test_expect_int_equal(comparison, -1, "runtime_string_compare should order lexicographically") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&right, &left);
    if (test_expect_int_equal(comparison, 1, "runtime_string_compare should reverse ordering") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "APPLE"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&left, &right);
    if (test_expect_int_equal(comparison, 0, "runtime_string_compare should report equality") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&left);
    runtime_string_dispose(&right);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_to_int_parses_numbers)
{
    t_runtime_string text;
    t_runtime_int value;

    if (test_expect_success(runtime_string_init(&text, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_assign(&text, "   -256  "), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    runtime_int_set(&value, 99);
    if (test_expect_success(runtime_string_to_int(&text, &value), "runtime_string_to_int should parse trimmed numbers") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, -256, "runtime_string_to_int should store parsed integers") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    runtime_int_set(&value, 73);
    if (test_expect_success(runtime_string_assign(&text, "12a"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    if (runtime_string_to_int(&text, &value) != FT_FAILURE)
    {
        runtime_string_dispose(&text);
        pf_printf("Assertion failed: runtime_string_to_int should reject invalid numbers\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, 73, "runtime_string_to_int should leave destination unchanged on failure") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&text);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_equals_detects_matches)
{
    t_runtime_string left;
    t_runtime_string right;

    if (test_expect_success(runtime_string_init(&left, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_init(&right, 0), "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&left, "alpha"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "alpha"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(runtime_string_equals(&left, &right), 1,
            "runtime_string_equals should report identical strings") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "beta"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(runtime_string_equals(&left, &right), 0,
            "runtime_string_equals should detect differences") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&left);
    runtime_string_dispose(&right);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_concat_appends_text)
{
    t_runtime_string left;
    t_runtime_string right;
    t_runtime_string result;

    if (test_expect_success(runtime_string_init(&left, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_init(&right, 0), "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_init(&result, 0), "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&left, "hello"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_string_dispose(&result);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, " world"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_string_dispose(&result);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_concat(&result, &left, &right),
            "runtime_string_concat should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_string_dispose(&result);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(result.data, "hello world",
            "runtime_string_concat should join inputs") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_string_dispose(&result);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(result.length), 11,
            "runtime_string_concat should update length") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        runtime_string_dispose(&result);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&left);
    runtime_string_dispose(&right);
    runtime_string_dispose(&result);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_string_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_string_assign_and_trim", test_runtime_string_assign_and_trim},
        {"runtime_string_compare_orders_text", test_runtime_string_compare_orders_text},
        {"runtime_string_to_int_parses_numbers", test_runtime_string_to_int_parses_numbers},
        {"runtime_string_equals_detects_matches", test_runtime_string_equals_detects_matches},
        {"runtime_string_concat_appends_text", test_runtime_string_concat_appends_text}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
