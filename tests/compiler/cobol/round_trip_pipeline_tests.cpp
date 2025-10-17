#include "../../test_suites.hpp"

const t_test_case *get_compiler_cobol_round_trip_pipeline_tests(size_t *count);

int test_cblc_copy_file_translates_to_cobol_and_executes(void);
int test_cblc_filter_prefix_translates_to_cobol_and_executes(void);
int test_cblc_reverse_control_flow_translates_to_cobol_and_executes(void);
int test_cblc_integration_showcase_translates_to_cobol_and_executes(void);
int test_cblc_message_showcase_translates_to_cobol_and_executes(void);
int test_message_showcase_sample_make_pipeline(void);
int test_cblc_multi_module_translates_to_cobol_and_executes(void);
int test_cblc_numeric_precision_translates_to_cobol_and_executes(void);
int test_cblc_floating_point_mix_translates_to_cobol_and_executes(void);
int test_cblc_mixed_numeric_types_translates_to_cobol_and_executes(void);
int test_cblc_textual_priority_mix_translates_to_cobol_and_executes(void);
int test_cblc_reverse_group_items_translates_to_cobol_and_executes(void);
int test_cblc_return_boolean_translates_to_cobol_and_executes(void);
int test_cblc_return_character_translates_to_cobol_and_executes(void);
int test_cblc_return_numeric_translates_to_cobol_and_executes(void);
int test_cobol_reverse_control_flow_round_trips_and_executes(void);
int test_cobol_copy_file_round_trips_and_executes(void);
int test_cobol_filter_prefix_round_trips_and_executes(void);
int test_cobol_integration_showcase_round_trips_and_executes(void);
int test_cobol_multi_module_round_trips_and_executes(void);
int test_cobol_numeric_precision_round_trips_and_executes(void);
int test_cobol_floating_point_mix_round_trips_and_executes(void);
int test_cobol_mixed_numeric_types_round_trips_and_executes(void);
int test_cobol_textual_priority_mix_round_trips_and_executes(void);
int test_cobol_reverse_group_items_round_trips_and_executes(void);
int test_cobol_return_boolean_round_trips_and_executes(void);
int test_cobol_return_character_round_trips_and_executes(void);
int test_cobol_return_numeric_round_trips_and_executes(void);

const t_test_case *get_compiler_cobol_round_trip_pipeline_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cblc_copy_file_translates_to_cobol_and_executes",
            test_cblc_copy_file_translates_to_cobol_and_executes},
        {"cblc_filter_prefix_translates_to_cobol_and_executes",
            test_cblc_filter_prefix_translates_to_cobol_and_executes},
        {"cblc_reverse_control_flow_translates_to_cobol_and_executes",
            test_cblc_reverse_control_flow_translates_to_cobol_and_executes},
        {"cblc_integration_showcase_translates_to_cobol_and_executes",
            test_cblc_integration_showcase_translates_to_cobol_and_executes},
        {"cblc_message_showcase_translates_to_cobol_and_executes",
            test_cblc_message_showcase_translates_to_cobol_and_executes},
        {"message_showcase_sample_make_pipeline", test_message_showcase_sample_make_pipeline},
        {"cblc_multi_module_translates_to_cobol_and_executes",
            test_cblc_multi_module_translates_to_cobol_and_executes},
        {"cblc_numeric_precision_translates_to_cobol_and_executes",
            test_cblc_numeric_precision_translates_to_cobol_and_executes},
        {"cblc_floating_point_mix_translates_to_cobol_and_executes",
            test_cblc_floating_point_mix_translates_to_cobol_and_executes},
        {"cblc_mixed_numeric_types_translates_to_cobol_and_executes",
            test_cblc_mixed_numeric_types_translates_to_cobol_and_executes},
        {"cblc_textual_priority_mix_translates_to_cobol_and_executes",
            test_cblc_textual_priority_mix_translates_to_cobol_and_executes},
        {"cblc_reverse_group_items_translates_to_cobol_and_executes",
            test_cblc_reverse_group_items_translates_to_cobol_and_executes},
        {"cblc_return_boolean_translates_to_cobol_and_executes",
            test_cblc_return_boolean_translates_to_cobol_and_executes},
        {"cblc_return_character_translates_to_cobol_and_executes",
            test_cblc_return_character_translates_to_cobol_and_executes},
        {"cblc_return_numeric_translates_to_cobol_and_executes",
            test_cblc_return_numeric_translates_to_cobol_and_executes},
        {"cobol_reverse_control_flow_round_trips_and_executes",
            test_cobol_reverse_control_flow_round_trips_and_executes},
        {"cobol_copy_file_round_trips_and_executes",
            test_cobol_copy_file_round_trips_and_executes},
        {"cobol_filter_prefix_round_trips_and_executes",
            test_cobol_filter_prefix_round_trips_and_executes},
        {"cobol_integration_showcase_round_trips_and_executes",
            test_cobol_integration_showcase_round_trips_and_executes},
        {"cobol_multi_module_round_trips_and_executes",
            test_cobol_multi_module_round_trips_and_executes},
        {"cobol_numeric_precision_round_trips_and_executes",
            test_cobol_numeric_precision_round_trips_and_executes},
        {"cobol_floating_point_mix_round_trips_and_executes",
            test_cobol_floating_point_mix_round_trips_and_executes},
        {"cobol_mixed_numeric_types_round_trips_and_executes",
            test_cobol_mixed_numeric_types_round_trips_and_executes},
        {"cobol_textual_priority_mix_round_trips_and_executes",
            test_cobol_textual_priority_mix_round_trips_and_executes},
        {"cobol_reverse_group_items_round_trips_and_executes",
            test_cobol_reverse_group_items_round_trips_and_executes},
        {"cobol_return_boolean_round_trips_and_executes",
            test_cobol_return_boolean_round_trips_and_executes},
        {"cobol_return_character_round_trips_and_executes",
            test_cobol_return_character_round_trips_and_executes},
        {"cobol_return_numeric_round_trips_and_executes",
            test_cobol_return_numeric_round_trips_and_executes}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
