#include "parser.hpp"

#include "test_suites.hpp"

FT_TEST(test_parser_rejects_missing_program_id_name)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "PROCEDURE DIVISION.\n";
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

FT_TEST(test_parser_rejects_missing_identification_division)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
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

FT_TEST(test_parser_rejects_missing_environment_division)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
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

FT_TEST(test_parser_rejects_missing_data_division)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 RESULT PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    DISPLAY RESULT.\n";
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

FT_TEST(test_parser_rejects_missing_procedure_division)
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
        "01 RESULT PIC 9(4).\n";
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

FT_TEST(test_parser_rejects_procedure_division_without_period)
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
        "01 RESULT PIC 9(4).\n"
        "PROCEDURE DIVISION\n"
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
