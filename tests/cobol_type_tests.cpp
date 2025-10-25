#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

static t_transpiler_cobol_usage cobol_usage_make_invalid(void)
{
    int value;

    value = static_cast<int>(TRANSPILE_COBOL_USAGE_COMP_5);
    value += 1;
    return (static_cast<t_transpiler_cobol_usage>(value));
}

FT_TEST(test_transpiler_cobol_formats_signed_integer)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_int(10, 1, &element),
            "signed integer descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    element.scale = 2;
    if (test_expect_success(transpiler_cobol_format_elementary("ACCOUNT-BAL", 5, &element, 2, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "        05 ACCOUNT-BAL PIC 9(10)V9(2) SIGN IS LEADING SEPARATE USAGE COMP-5.\n",
            "formatted PIC should include decimals and sign clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_signed_long_range)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_long(1, &element),
            "long descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("HIGH-LIMIT", 5, &element, 1, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "    05 HIGH-LIMIT PIC 9(18) SIGN IS LEADING SEPARATE USAGE COMP-5.\n",
            "formatted PIC should cover 18 digits with sign clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_unsigned_long_long_range)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_long_long(0, &element),
            "long long descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("AGGREGATE-TOTAL", 5, &element, 3, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "            05 AGGREGATE-TOTAL PIC 9(36) USAGE COMP-5.\n",
            "formatted PIC should cover 36 digits without sign clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_float_field)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_float(&element),
            "float descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("INTEREST-RATE", 5, &element, 2, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "        05 INTEREST-RATE PIC 9(9)V9(9) SIGN IS LEADING SEPARATE.\n",
            "formatted PIC should include fractional digits and sign clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_double_field)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_double(&element),
            "double descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("AMORTIZATION-RATE", 5, &element, 0, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "05 AMORTIZATION-RATE PIC 9(18)V9(18) SIGN IS LEADING SEPARATE.\n",
            "formatted PIC should widen to double precision with sign clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_fraction_only_field)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_fixed_point(0, 6, 0, TRANSPILE_COBOL_USAGE_DISPLAY, &element),
            "fixed-point descriptor should allow fraction-only") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("FRACTION-ONLY", 5, &element, 1, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "    05 FRACTION-ONLY PIC V9(6).\n",
            "formatted PIC should allow fractional-only definitions") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_character_array)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_char_array(40, &element),
            "char array descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("CUSTOMER-NAME", 5, &element, 1, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "    05 CUSTOMER-NAME PIC X(40).\n",
            "formatted PIC should size alpha item") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_describe_numeric_defaults_digits)
{
    t_transpiler_cobol_elementary element;

    if (test_expect_success(transpiler_cobol_describe_numeric(0, 1, TRANSPILE_COBOL_USAGE_COMP_5, &element),
            "numeric descriptor should succeed when digits are omitted") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.kind), static_cast<int>(TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED),
            "numeric descriptor should mark signed kind") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.length, 1,
            "numeric descriptor should default to one digit") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.scale, 0,
            "numeric descriptor should default scale to zero") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.usage), static_cast<int>(TRANSPILE_COBOL_USAGE_COMP_5),
            "numeric descriptor should preserve requested usage") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cobol_describe_numeric_signed_kind)
{
    t_transpiler_cobol_elementary element;

    if (test_expect_success(transpiler_cobol_describe_numeric(4, 1, TRANSPILE_COBOL_USAGE_COMP_5, &element),
            "numeric descriptor should accept explicit digit count") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.kind), static_cast<int>(TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED),
            "numeric descriptor should mark signed kind when requested") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.length, 4,
            "numeric descriptor should store the provided digit count") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.scale, 0,
            "numeric descriptor should keep scale at zero for integers") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.usage), static_cast<int>(TRANSPILE_COBOL_USAGE_COMP_5),
            "numeric descriptor should remember requested usage") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cobol_describe_numeric_rejects_unknown_usage)
{
    t_transpiler_cobol_elementary element;
    t_transpiler_cobol_usage invalid_usage;

    element.kind = TRANSPILE_COBOL_ELEMENTARY_ALPHANUMERIC;
    invalid_usage = cobol_usage_make_invalid();
    if (test_expect_int_equal(transpiler_cobol_describe_numeric(3, 0, invalid_usage, &element), FT_FAILURE,
            "numeric descriptor should reject unsupported usage") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.kind), static_cast<int>(TRANSPILE_COBOL_ELEMENTARY_ALPHANUMERIC),
            "numeric descriptor should leave output untouched on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cobol_formats_custom_numeric_display)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_numeric(7, 0, TRANSPILE_COBOL_USAGE_DISPLAY, &element),
            "numeric descriptor should accept custom digits") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("UNITS-ON-HAND", 5, &element, 2, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "        05 UNITS-ON-HAND PIC 9(7).\n",
            "formatted PIC should size unsigned numeric without usage clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_custom_numeric_comp_5_unsigned)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_numeric(8, 0, TRANSPILE_COBOL_USAGE_COMP_5, &element),
            "numeric descriptor should support unsigned COMP-5") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("BATCH-COUNT", 5, &element, 3, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "            05 BATCH-COUNT PIC 9(8) USAGE COMP-5.\n",
            "formatted PIC should omit sign clause for unsigned COMP-5") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_fixed_point_comp_5)
{
    t_transpiler_cobol_elementary element;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_fixed_point(5, 3, 1, TRANSPILE_COBOL_USAGE_COMP_5, &element),
            "fixed-point descriptor should accept integral and fractional digits") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_format_elementary("AMOUNT-DUE", 5, &element, 3, NULL, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "            05 AMOUNT-DUE PIC 9(5)V9(3) SIGN IS LEADING SEPARATE USAGE COMP-5.\n",
            "formatted PIC should emit sign and COMP-5 clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_describe_fixed_point_defaults_integral_digit)
{
    t_transpiler_cobol_elementary element;

    if (test_expect_success(transpiler_cobol_describe_fixed_point(0, 0, 0, TRANSPILE_COBOL_USAGE_DISPLAY, &element),
            "fixed-point descriptor should succeed with no digits specified") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.kind), static_cast<int>(TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED),
            "fixed-point descriptor should select unsigned kind") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.length, 1,
            "fixed-point descriptor should default integral digits to one") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.scale, 0,
            "fixed-point descriptor should keep fractional digits at zero") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.usage), static_cast<int>(TRANSPILE_COBOL_USAGE_DISPLAY),
            "fixed-point descriptor should capture requested usage") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cobol_describe_fixed_point_signed_scale)
{
    t_transpiler_cobol_elementary element;

    if (test_expect_success(transpiler_cobol_describe_fixed_point(6, 2, 1, TRANSPILE_COBOL_USAGE_COMP_5, &element),
            "fixed-point descriptor should accept explicit digits and sign") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.kind), static_cast<int>(TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED),
            "fixed-point descriptor should report signed kind when requested") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.length, 6,
            "fixed-point descriptor should store provided integral digits") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.scale, 2,
            "fixed-point descriptor should store provided fractional digits") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(element.usage), static_cast<int>(TRANSPILE_COBOL_USAGE_COMP_5),
            "fixed-point descriptor should capture usage selection") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cobol_describe_fixed_point_rejects_unknown_usage)
{
    t_transpiler_cobol_elementary element;
    t_transpiler_cobol_usage invalid_usage;

    element.length = 7;
    element.scale = 3;
    invalid_usage = cobol_usage_make_invalid();
    if (test_expect_int_equal(transpiler_cobol_describe_fixed_point(4, 2, 1, invalid_usage, &element), FT_FAILURE,
            "fixed-point descriptor should reject unsupported usage") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.length, 7,
            "fixed-point descriptor should leave integral digits untouched on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(element.scale, 3,
            "fixed-point descriptor should leave fractional digits untouched on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_cobol_formats_group_with_boolean)
{
    t_transpiler_cobol_elementary flag_element;
    t_transpiler_cobol_elementary name_element;
    t_transpiler_cobol_group_field fields[2];
    t_transpiler_cobol_group group;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_c_bool(&flag_element),
            "boolean descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_cobol_describe_c_char_array(16, &name_element),
            "char array descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    fields[0].name = "IS-ACTIVE";
    fields[0].level = 5;
    fields[0].element = flag_element;
    fields[0].value_text = NULL;
    fields[1].name = "SHORT-NAME";
    fields[1].level = 5;
    fields[1].element = name_element;
    fields[1].value_text = NULL;
    group.name = "CUSTOMER-REC";
    group.level = 1;
    group.fields = fields;
    group.field_count = 2;
    if (test_expect_success(transpiler_cobol_format_group(&group, 1, &formatted),
            "group formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "    01 CUSTOMER-REC.\n"
            "        05 IS-ACTIVE PIC X VALUE 'N'.\n"
            "        05 SHORT-NAME PIC X(16).\n",
            "group formatting should nest elementary clauses") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

FT_TEST(test_transpiler_cobol_formats_group_with_numeric_value)
{
    t_transpiler_cobol_elementary amount_element;
    t_transpiler_cobol_group_field fields[1];
    t_transpiler_cobol_group group;
    char *formatted;
    int status;

    formatted = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_cobol_describe_numeric(4, 0,
                TRANSPILE_COBOL_USAGE_DISPLAY, &amount_element),
            "numeric descriptor should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    fields[0].name = "TOTAL-AMOUNT";
    fields[0].level = 5;
    fields[0].element = amount_element;
    fields[0].value_text = "ZERO";
    group.name = "SUMMARY-REC";
    group.level = 1;
    group.fields = fields;
    group.field_count = 1;
    if (test_expect_success(transpiler_cobol_format_group(&group, 0, &formatted),
            "group formatting should succeed with VALUE clause") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "01 SUMMARY-REC.\n"
            "    05 TOTAL-AMOUNT PIC 9(4) VALUE ZERO.\n",
            "group formatting should preserve numeric VALUE clause") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (formatted)
        cma_free(formatted);
    return (status);
}

const t_test_case *get_cobol_type_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_cobol_formats_signed_integer", test_transpiler_cobol_formats_signed_integer},
        {"transpiler_cobol_formats_signed_long_range", test_transpiler_cobol_formats_signed_long_range},
        {"transpiler_cobol_formats_unsigned_long_long_range", test_transpiler_cobol_formats_unsigned_long_long_range},
        {"transpiler_cobol_formats_float_field", test_transpiler_cobol_formats_float_field},
        {"transpiler_cobol_formats_double_field", test_transpiler_cobol_formats_double_field},
        {"transpiler_cobol_formats_fraction_only_field", test_transpiler_cobol_formats_fraction_only_field},
        {"transpiler_cobol_formats_character_array", test_transpiler_cobol_formats_character_array},
        {"transpiler_cobol_describe_numeric_defaults_digits", test_transpiler_cobol_describe_numeric_defaults_digits},
        {"transpiler_cobol_describe_numeric_rejects_unknown_usage", test_transpiler_cobol_describe_numeric_rejects_unknown_usage},
        {"transpiler_cobol_formats_custom_numeric_display", test_transpiler_cobol_formats_custom_numeric_display},
        {"transpiler_cobol_formats_fixed_point_comp_5", test_transpiler_cobol_formats_fixed_point_comp_5},
        {"transpiler_cobol_describe_fixed_point_defaults_integral_digit", test_transpiler_cobol_describe_fixed_point_defaults_integral_digit},
        {"transpiler_cobol_describe_fixed_point_rejects_unknown_usage", test_transpiler_cobol_describe_fixed_point_rejects_unknown_usage},
        {"transpiler_cobol_formats_group_with_boolean", test_transpiler_cobol_formats_group_with_boolean},
        {"transpiler_cobol_formats_group_with_numeric_value", test_transpiler_cobol_formats_group_with_numeric_value}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
