#include "semantics_test_groups.hpp"

int test_semantics_accepts_numeric_condition_equality(void);
int test_semantics_accepts_mixed_numeric_condition_equality(void);
int test_semantics_accepts_floating_condition_equality(void);
int test_semantics_accepts_mixed_floating_alphanumeric_condition_equality(void);
int test_semantics_accepts_alphanumeric_condition_equality(void);
int test_semantics_accepts_numeric_condition_not_equal(void);
int test_semantics_accepts_floating_condition_not_equal(void);
int test_semantics_rejects_mixed_condition_equality(void);
int test_semantics_accepts_mixed_numeric_condition_not_equal(void);
int test_semantics_accepts_numeric_condition_less_than(void);
int test_semantics_accepts_mixed_numeric_condition_less_than(void);
int test_semantics_accepts_mixed_numeric_condition_less_or_equal(void);
int test_semantics_does_not_report_mixed_numeric_condition_less_than(void);
int test_semantics_accepts_mixed_numeric_condition_greater_than(void);
int test_semantics_accepts_mixed_numeric_condition_greater_or_equal(void);
int test_semantics_does_not_report_mixed_numeric_condition_greater_or_equal(void);
int test_semantics_accepts_alphanumeric_condition_not_equal(void);
int test_semantics_rejects_mixed_alphanumeric_numeric_condition_not_equal(void);
int test_semantics_rejects_alphanumeric_condition_less_than(void);

const t_test_case *get_semantics_condition_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_accepts_numeric_condition_equality", test_semantics_accepts_numeric_condition_equality},
        {"semantics_accepts_mixed_numeric_condition_equality", test_semantics_accepts_mixed_numeric_condition_equality},
        {"semantics_accepts_floating_condition_equality", test_semantics_accepts_floating_condition_equality},
        {"semantics_accepts_mixed_floating_alphanumeric_condition_equality", test_semantics_accepts_mixed_floating_alphanumeric_condition_equality},
        {"semantics_accepts_alphanumeric_condition_equality", test_semantics_accepts_alphanumeric_condition_equality},
        {"semantics_accepts_numeric_condition_not_equal", test_semantics_accepts_numeric_condition_not_equal},
        {"semantics_accepts_floating_condition_not_equal", test_semantics_accepts_floating_condition_not_equal},
        {"semantics_rejects_mixed_condition_equality", test_semantics_rejects_mixed_condition_equality},
        {"semantics_accepts_mixed_numeric_condition_not_equal", test_semantics_accepts_mixed_numeric_condition_not_equal},
        {"semantics_accepts_numeric_condition_less_than", test_semantics_accepts_numeric_condition_less_than},
        {"semantics_accepts_mixed_numeric_condition_less_than", test_semantics_accepts_mixed_numeric_condition_less_than},
        {"semantics_accepts_mixed_numeric_condition_less_or_equal", test_semantics_accepts_mixed_numeric_condition_less_or_equal},
        {"semantics_does_not_report_mixed_numeric_condition_less_than", test_semantics_does_not_report_mixed_numeric_condition_less_than},
        {"semantics_accepts_mixed_numeric_condition_greater_than", test_semantics_accepts_mixed_numeric_condition_greater_than},
        {"semantics_accepts_mixed_numeric_condition_greater_or_equal", test_semantics_accepts_mixed_numeric_condition_greater_or_equal},
        {"semantics_does_not_report_mixed_numeric_condition_greater_or_equal", test_semantics_does_not_report_mixed_numeric_condition_greater_or_equal},
        {"semantics_accepts_alphanumeric_condition_not_equal", test_semantics_accepts_alphanumeric_condition_not_equal},
        {"semantics_rejects_mixed_alphanumeric_numeric_condition_not_equal", test_semantics_rejects_mixed_alphanumeric_numeric_condition_not_equal},
        {"semantics_rejects_alphanumeric_condition_less_than", test_semantics_rejects_alphanumeric_condition_less_than}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
