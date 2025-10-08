#include "test_suites.hpp"

int test_parser_parses_minimal_program(void);
int test_parser_parses_control_flow_statements(void);
int test_parser_accepts_boolean_character_literals(void);
int test_parser_rejects_unknown_statement(void);
int test_parser_rejects_paragraph_without_terminator(void);
int test_parser_rejects_incomplete_if_statement(void);
int test_parser_rejects_move_without_target(void);
int test_parser_rejects_move_without_source(void);
int test_parser_rejects_display_without_operand(void);
int test_parser_rejects_perform_until_without_end(void);
int test_parser_rejects_perform_varying_without_by_clause(void);
int test_parser_rejects_stop_without_run_keyword(void);
int test_parser_rejects_missing_program_id_name(void);
int test_parser_rejects_missing_identification_division(void);
int test_parser_rejects_missing_environment_division(void);
int test_parser_rejects_missing_data_division(void);
int test_parser_rejects_missing_procedure_division(void);
int test_parser_rejects_procedure_division_without_period(void);
int test_parser_rejects_data_item_without_level(void);
int test_parser_rejects_data_items_without_section(void);

const t_test_case *get_parser_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"parser_parses_minimal_program", test_parser_parses_minimal_program},
        {"parser_parses_control_flow_statements", test_parser_parses_control_flow_statements},
        {"parser_accepts_boolean_character_literals", test_parser_accepts_boolean_character_literals},
        {"parser_rejects_unknown_statement", test_parser_rejects_unknown_statement},
        {"parser_rejects_paragraph_without_terminator", test_parser_rejects_paragraph_without_terminator},
        {"parser_rejects_incomplete_if_statement", test_parser_rejects_incomplete_if_statement},
        {"parser_rejects_move_without_target", test_parser_rejects_move_without_target},
        {"parser_rejects_move_without_source", test_parser_rejects_move_without_source},
        {"parser_rejects_display_without_operand", test_parser_rejects_display_without_operand},
        {"parser_rejects_perform_until_without_end", test_parser_rejects_perform_until_without_end},
        {"parser_rejects_perform_varying_without_by_clause", test_parser_rejects_perform_varying_without_by_clause},
        {"parser_rejects_stop_without_run_keyword", test_parser_rejects_stop_without_run_keyword},
        {"parser_rejects_missing_program_id_name", test_parser_rejects_missing_program_id_name},
        {"parser_rejects_missing_identification_division", test_parser_rejects_missing_identification_division},
        {"parser_rejects_missing_environment_division", test_parser_rejects_missing_environment_division},
        {"parser_rejects_missing_data_division", test_parser_rejects_missing_data_division},
        {"parser_rejects_missing_procedure_division", test_parser_rejects_missing_procedure_division},
        {"parser_rejects_procedure_division_without_period", test_parser_rejects_procedure_division_without_period},
        {"parser_rejects_data_item_without_level", test_parser_rejects_data_item_without_level},
        {"parser_rejects_data_items_without_section", test_parser_rejects_data_items_without_section}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
