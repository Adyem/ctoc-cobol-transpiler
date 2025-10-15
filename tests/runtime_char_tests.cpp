#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

FT_TEST(test_runtime_char_transforms)
{
    t_runtime_char character;

    runtime_char_set(&character, 'a');
    runtime_char_to_upper(&character);
    if (test_expect_char_equal(character.value, 'A', "runtime_char_to_upper should uppercase characters") != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_char_to_lower(&character);
    if (test_expect_char_equal(character.value, 'a', "runtime_char_to_lower should lowercase characters") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_char_from_string_rejects_empty_input)
{
    t_runtime_char character;

    runtime_char_set(&character, 'X');
    if (runtime_char_from_string(&character, "") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_char_from_string should reject empty text\n");
        return (FT_FAILURE);
    }
    if (test_expect_char_equal(character.value, 'X', "runtime_char_from_string should preserve previous value") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_char_to_string_and_compare)
{
    t_runtime_char left;
    t_runtime_char right;
    char buffer[8];

    runtime_char_set(&left, 'X');
    runtime_char_set(&right, 'Z');
    if (test_expect_success(runtime_char_to_string(left, buffer, sizeof(buffer)), "runtime_char_to_string should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strncmp(buffer, "X", 2) != 0)
    {
        pf_printf("Assertion failed: runtime_char_to_string should copy character to buffer\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(runtime_char_compare(left, right), -1, "runtime_char_compare should order characters") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(runtime_char_compare(right, left), 1, "runtime_char_compare should reverse ordering") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(runtime_char_compare(left, left), 0, "runtime_char_compare should detect equality") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_char_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_char_transforms", test_runtime_char_transforms},
        {"runtime_char_from_string_rejects_empty_input", test_runtime_char_from_string_rejects_empty_input},
        {"runtime_char_to_string_and_compare", test_runtime_char_to_string_and_compare}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
