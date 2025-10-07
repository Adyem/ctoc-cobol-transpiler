#include "transpiler_cobol_types.hpp"

#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

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
    if (test_expect_success(transpiler_cobol_format_elementary("ACCOUNT-BAL", 5, &element, 2, &formatted),
            "elementary formatting should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(formatted,
            "        05 ACCOUNT-BAL PIC 9(10)V9(2) SIGN IS LEADING SEPARATE.\n",
            "formatted PIC should include decimals and sign clause") != FT_SUCCESS)
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
    if (test_expect_success(transpiler_cobol_format_elementary("CUSTOMER-NAME", 5, &element, 1, &formatted),
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
    fields[1].name = "SHORT-NAME";
    fields[1].level = 5;
    fields[1].element = name_element;
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

const t_test_case *get_cobol_type_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_cobol_formats_signed_integer", test_transpiler_cobol_formats_signed_integer},
        {"transpiler_cobol_formats_character_array", test_transpiler_cobol_formats_character_array},
        {"transpiler_cobol_formats_group_with_boolean", test_transpiler_cobol_formats_group_with_boolean}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
