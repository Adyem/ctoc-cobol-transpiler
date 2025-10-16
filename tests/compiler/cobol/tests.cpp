#include "../../test_suites.hpp"

#include <cstdlib>

const t_test_case *get_compiler_cobol_tests(size_t *count);
const t_test_case *get_compiler_cobol_return_numeric_tests(size_t *count);
const t_test_case *get_compiler_cobol_multi_module_tests(size_t *count);
const t_test_case *get_compiler_cobol_return_boolean_tests(size_t *count);
const t_test_case *get_compiler_cobol_return_character_tests(size_t *count);
const t_test_case *get_compiler_cobol_filter_prefix_tests(size_t *count);
const t_test_case *get_compiler_cobol_copy_file_tests(size_t *count);
const t_test_case *get_compiler_cobol_record_writer_tests(size_t *count);
const t_test_case *get_compiler_cobol_record_summary_tests(size_t *count);
const t_test_case *get_compiler_cobol_integration_showcase_tests(size_t *count);
const t_test_case *get_compiler_cobol_reverse_control_flow_tests(size_t *count);
const t_test_case *get_compiler_cobol_reverse_normalization_tests(size_t *count);
const t_test_case *get_compiler_cobol_reverse_cli_tests(size_t *count);
const t_test_case *get_compiler_cobol_round_trip_pipeline_tests(size_t *count);

const t_test_case *get_compiler_cobol_tests(size_t *count)
{
    static t_test_case *combined = NULL;
    static size_t combined_count = 0;
    static int initialized = 0;
    const t_test_case *numeric_tests;
    const t_test_case *multi_module_tests;
    const t_test_case *boolean_tests;
    const t_test_case *character_tests;
    const t_test_case *filter_prefix_tests;
    const t_test_case *copy_file_tests;
    const t_test_case *record_writer_tests;
    const t_test_case *integration_showcase_tests;
    const t_test_case *reverse_control_flow_tests;
    const t_test_case *reverse_normalization_tests;
    const t_test_case *reverse_cli_tests;
    const t_test_case *round_trip_pipeline_tests;
    const t_test_case *record_summary_tests;
    size_t numeric_count;
    size_t multi_module_count;
    size_t boolean_count;
    size_t character_count;
    size_t filter_prefix_count;
    size_t copy_file_count;
    size_t record_writer_count;
    size_t integration_showcase_count;
    size_t reverse_control_flow_count;
    size_t reverse_normalization_count;
    size_t reverse_cli_count;
    size_t round_trip_pipeline_count;
    size_t record_summary_count;
    size_t index;
    size_t offset;

    if (!initialized)
    {
        numeric_tests = get_compiler_cobol_return_numeric_tests(&numeric_count);
        multi_module_tests = get_compiler_cobol_multi_module_tests(&multi_module_count);
        boolean_tests = get_compiler_cobol_return_boolean_tests(&boolean_count);
        character_tests = get_compiler_cobol_return_character_tests(&character_count);
        filter_prefix_tests = get_compiler_cobol_filter_prefix_tests(&filter_prefix_count);
        copy_file_tests = get_compiler_cobol_copy_file_tests(&copy_file_count);
        record_writer_tests = get_compiler_cobol_record_writer_tests(&record_writer_count);
        integration_showcase_tests = get_compiler_cobol_integration_showcase_tests(&integration_showcase_count);
        reverse_control_flow_tests = get_compiler_cobol_reverse_control_flow_tests(&reverse_control_flow_count);
        reverse_normalization_tests = get_compiler_cobol_reverse_normalization_tests(&reverse_normalization_count);
        reverse_cli_tests = get_compiler_cobol_reverse_cli_tests(&reverse_cli_count);
        round_trip_pipeline_tests = get_compiler_cobol_round_trip_pipeline_tests(&round_trip_pipeline_count);
        record_summary_tests = get_compiler_cobol_record_summary_tests(&record_summary_count);
        combined_count = numeric_count + multi_module_count + boolean_count + character_count
            + filter_prefix_count + copy_file_count + record_writer_count + integration_showcase_count
            + reverse_control_flow_count + reverse_normalization_count + reverse_cli_count
            + round_trip_pipeline_count + record_summary_count;
        if (combined_count > 0)
        {
            combined = static_cast<t_test_case *>(calloc(combined_count, sizeof(t_test_case)));
            if (!combined)
            {
                combined_count = 0;
                initialized = 1;
                if (count)
                    *count = 0;
                return (NULL);
            }
            index = 0;
            while (index < numeric_count)
            {
                combined[index] = numeric_tests[index];
                index += 1;
            }
            offset = index;
            index = 0;
            while (index < multi_module_count)
            {
                combined[offset + index] = multi_module_tests[index];
                index += 1;
            }
            offset += multi_module_count;
            index = 0;
            while (index < boolean_count)
            {
                combined[offset + index] = boolean_tests[index];
                index += 1;
            }
            offset += boolean_count;
            index = 0;
            while (index < character_count)
            {
                combined[offset + index] = character_tests[index];
                index += 1;
            }
            offset += character_count;
            index = 0;
            while (index < filter_prefix_count)
            {
                combined[offset + index] = filter_prefix_tests[index];
                index += 1;
            }
            offset += filter_prefix_count;
            index = 0;
            while (index < copy_file_count)
            {
                combined[offset + index] = copy_file_tests[index];
                index += 1;
            }
            offset += copy_file_count;
            index = 0;
            while (index < record_writer_count)
            {
                combined[offset + index] = record_writer_tests[index];
                index += 1;
            }
            offset += record_writer_count;
            index = 0;
            while (index < integration_showcase_count)
            {
                combined[offset + index] = integration_showcase_tests[index];
                index += 1;
            }
            offset += integration_showcase_count;
            index = 0;
            while (index < reverse_control_flow_count)
            {
                combined[offset + index] = reverse_control_flow_tests[index];
                index += 1;
            }
            offset += reverse_control_flow_count;
            index = 0;
            while (index < reverse_normalization_count)
            {
                combined[offset + index] = reverse_normalization_tests[index];
                index += 1;
            }
            offset += reverse_normalization_count;
            index = 0;
            while (index < reverse_cli_count)
            {
                combined[offset + index] = reverse_cli_tests[index];
                index += 1;
            }
            offset += reverse_cli_count;
            index = 0;
            while (index < round_trip_pipeline_count)
            {
                combined[offset + index] = round_trip_pipeline_tests[index];
                index += 1;
            }
            offset += round_trip_pipeline_count;
            index = 0;
            while (index < record_summary_count)
            {
                combined[offset + index] = record_summary_tests[index];
                index += 1;
            }
        }
        initialized = 1;
    }
    if (count)
        *count = combined_count;
    return (combined);
}

