#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

FT_TEST(test_parser_rejects_unknown_statement)
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
        "PROCEDURE DIVISION.\n"
        "    OPEN FILE.\n";
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

FT_TEST(test_parser_rejects_paragraph_without_terminator)
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
        "PROCEDURE DIVISION.\n"
        "MAIN\n"
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

FT_TEST(test_parser_rejects_incomplete_if_statement)
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
        "01 FLAG PIC X.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    IF FLAG == 'Y'\n"
        "        MOVE 'N' TO FLAG\n";
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

FT_TEST(test_parser_accepts_single_equals_condition)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *procedure_division;
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_ast_node *if_statement;
    t_ast_node *condition;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 FLAG PIC X.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    IF FLAG = 'Y'\n"
        "        DISPLAY FLAG\n"
        "    END-IF.\n";
    parser_init(&parser, source);
    program = NULL;
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        return (FT_FAILURE);
    }
    parser_dispose(&parser);
    if (!program)
        return (FT_FAILURE);
    procedure_division = ast_node_get_child(program, 3);
    if (!procedure_division || procedure_division->kind != AST_NODE_PROCEDURE_DIVISION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    paragraph = ast_node_get_child(procedure_division, 0);
    if (!paragraph || paragraph->kind != AST_NODE_PARAGRAPH)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    sequence = ast_node_get_child(paragraph, 0);
    if (!sequence || sequence->kind != AST_NODE_STATEMENT_SEQUENCE)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if_statement = ast_node_get_child(sequence, 0);
    if (!if_statement || if_statement->kind != AST_NODE_IF_STATEMENT)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    condition = ast_node_get_child(if_statement, 0);
    if (!condition || condition->kind != AST_NODE_CONDITION)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_child_count(condition) != 3)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (!ast_node_get_child(condition, 1)
        || ast_node_get_child(condition, 1)->token.kind != LEXER_TOKEN_EQUALS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}

FT_TEST(test_parser_rejects_assignment_without_expression)
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
        "01 TOTAL PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    TOTAL = ;\n";
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

FT_TEST(test_parser_rejects_move_without_target)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    MOVE 1 TO.\n";
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

FT_TEST(test_parser_rejects_display_without_operand)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    DISPLAY.\n";
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

FT_TEST(test_parser_rejects_move_without_source)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    MOVE TO RESULT.\n";
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

FT_TEST(test_parser_rejects_perform_until_without_end)
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
        "01 FLAG PIC X.\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    PERFORM UNTIL FLAG == 'Y'\n"
        "        MOVE 'Y' TO FLAG.\n";
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

FT_TEST(test_parser_rejects_perform_varying_without_by_clause)
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
        "01 INDEX PIC 9(4).\n"
        "01 LIMIT PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    PERFORM VARYING INDEX FROM 0 UNTIL INDEX == LIMIT\n"
        "        MOVE 1 TO INDEX\n"
        "    END-PERFORM.\n";
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

FT_TEST(test_parser_rejects_stop_without_run_keyword)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    STOP.\n";
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

FT_TEST(test_parser_rejects_open_without_mode)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    OPEN.\n";
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

FT_TEST(test_parser_rejects_open_without_file)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    OPEN INPUT.\n";
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

FT_TEST(test_parser_rejects_close_without_file)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    CLOSE.\n";
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

FT_TEST(test_parser_rejects_read_without_file)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    READ.\n";
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

FT_TEST(test_parser_rejects_read_into_without_target)
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
        "01 RECORD-AREA PIC X(10).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    READ INVENTORY INTO.\n";
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

FT_TEST(test_parser_rejects_write_without_file)
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
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    WRITE.\n";
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

FT_TEST(test_parser_rejects_write_from_without_source)
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
        "01 REPORT-RECORD PIC X(20).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    WRITE REPORT-FILE FROM.\n";
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

FT_TEST(test_parser_reports_multiple_statement_errors)
{
    const char *source;
    t_parser parser;
    t_ast_node *program;
    int status;
    size_t error_count;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "ENVIRONMENT DIVISION.\n"
        "DATA DIVISION.\n"
        "WORKING-STORAGE SECTION.\n"
        "01 RESULT PIC 9(4).\n"
        "PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "    OPEN FILE.\n"
        "    OPEN FILE.\n"
        "    STOP RUN.\n";
    parser_init(&parser, source);
    program = NULL;
    status = parser_parse_program(&parser, &program);
    error_count = parser.error_count;
    parser_dispose(&parser);
    if (program)
        ast_node_destroy(program);
    if (status != FT_FAILURE)
        return (FT_FAILURE);
    if (error_count != 2)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
