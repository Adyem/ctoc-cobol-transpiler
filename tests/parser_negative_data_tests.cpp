#include "parser.hpp"

#include "test_suites.hpp"

FT_TEST(test_parser_rejects_data_item_without_level)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "RESULT PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    MOVE \"42\" TO RESULT\n";
    parser_init(&parser, source);
    program = NULL;
    status = parser_parse_program(&parser, &program);
    parser_dispose(&parser);
    if (program)
        ast_node_destroy(program);
    if (status == FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_parser_rejects_data_items_without_section)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "01 RESULT PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    MOVE 1 TO RESULT.\n";
    parser_init(&parser, source);
    program = NULL;
    status = parser_parse_program(&parser, &program);
    parser_dispose(&parser);
    if (program)
        ast_node_destroy(program);
    if (status == FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
