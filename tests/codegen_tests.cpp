#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

FT_TEST(test_transpiler_codegen_emits_text_file_sections)
{
    t_transpiler_context context;
    t_transpiler_cobol_file_sections sections;
    int status;

    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_codegen_file_sections_init(&sections);
    if (test_expect_success(transpiler_context_register_file(&context, "input_file", TRANSPILE_FILE_ROLE_INPUT,
            "input.txt", 0), "file registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_record_file_length_hint(&context, "input_file", 80),
            "record length hint should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_codegen_build_file_sections(&context, &sections),
            "codegen should build file sections") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(sections.environment_division,
            "       ENVIRONMENT DIVISION.\n"
            "       INPUT-OUTPUT SECTION.\n"
            "       FILE-CONTROL.\n"
            "           SELECT INPUT_FILE ASSIGN TO \"input.txt\"\n"
            "               ORGANIZATION IS LINE SEQUENTIAL.\n",
            "environment division should describe input file") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(sections.data_division,
            "       DATA DIVISION.\n"
            "       FILE SECTION.\n"
            "       FD INPUT_FILE.\n"
            "       01 INPUT_FILE-REC PIC X(80).\n",
            "data division should size record from hint") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_codegen_file_sections_dispose(&sections);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_transpiler_codegen_respects_explicit_record_length)
{
    t_transpiler_context context;
    t_transpiler_cobol_file_sections sections;
    int status;

    status = FT_FAILURE;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_codegen_file_sections_init(&sections);
    if (test_expect_success(transpiler_context_register_file(&context, "data_store", TRANSPILE_FILE_ROLE_DATA,
            "store.bin", 128), "data file registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_record_file_length_hint(&context, "data_store", 256),
            "hints should be accepted without overriding explicit length") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_codegen_build_file_sections(&context, &sections),
            "codegen should build data file sections") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(sections.environment_division,
            "       ENVIRONMENT DIVISION.\n"
            "       INPUT-OUTPUT SECTION.\n"
            "       FILE-CONTROL.\n"
            "           SELECT DATA_STORE ASSIGN TO \"store.bin\"\n"
            "               ORGANIZATION IS SEQUENTIAL.\n",
            "environment division should emit sequential organization for data file") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(sections.data_division,
            "       DATA DIVISION.\n"
            "       FILE SECTION.\n"
            "       FD DATA_STORE.\n"
            "       01 DATA_STORE-REC PIC X(128).\n",
            "data division should preserve explicit record length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_codegen_file_sections_dispose(&sections);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_transpiler_codegen_formats_if_else)
{
    t_transpiler_cobol_procedure procedure;
    t_transpiler_cobol_paragraph *paragraph;
    t_transpiler_cobol_statement_block *block;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement *if_statement;
    t_transpiler_cobol_statement_block *branch;
    t_transpiler_cobol_statement *move_statement;
    char *procedure_text;
    int status;

    transpiler_cobol_procedure_init(&procedure);
    paragraph = transpiler_cobol_paragraph_create("main");
    if (!paragraph)
        return (FT_FAILURE);
    block = transpiler_cobol_paragraph_get_statements(paragraph);
    if (!block)
    {
        transpiler_cobol_paragraph_destroy(paragraph);
        return (FT_FAILURE);
    }
    procedure_text = NULL;
    if_statement = NULL;
    status = FT_FAILURE;
    condition.left = "FLAG";
    condition.right = "'Y'";
    condition.op = TRANSPILE_COBOL_COMPARISON_EQUALS;
    condition.negated = 0;
    if_statement = transpiler_cobol_statement_create_if(&condition);
    if (!if_statement)
        goto cleanup;
    branch = transpiler_cobol_if_get_then_branch(if_statement);
    if (!branch)
        goto cleanup;
    move_statement = transpiler_cobol_statement_create_move("INPUT-REC", "OUTPUT-REC");
    if (!move_statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(branch, move_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(move_statement);
        goto cleanup;
    }
    branch = transpiler_cobol_if_get_else_branch(if_statement);
    if (!branch)
        goto cleanup;
    move_statement = transpiler_cobol_statement_create_move("ZERO", "OUTPUT-REC");
    if (!move_statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(branch, move_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(move_statement);
        goto cleanup;
    }
    if (transpiler_cobol_statement_block_append(block, if_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(if_statement);
        if_statement = NULL;
        goto cleanup;
    }
    if_statement = NULL;
    if (transpiler_cobol_procedure_append(&procedure, paragraph) != FT_SUCCESS)
        goto cleanup;
    paragraph = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&procedure, &procedure_text),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure_text,
            "       PROCEDURE DIVISION.\n"
            "       MAIN.\n"
            "           IF FLAG = 'Y'\n"
            "               MOVE INPUT-REC TO OUTPUT-REC\n"
            "           ELSE\n"
            "               MOVE ZERO TO OUTPUT-REC\n"
            "           END-IF\n",
            "procedure division should format IF/ELSE branches") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure_text)
        cma_free(procedure_text);
    if (if_statement)
        transpiler_cobol_statement_destroy(if_statement);
    if (paragraph)
        transpiler_cobol_paragraph_destroy(paragraph);
    transpiler_cobol_procedure_dispose(&procedure);
    return (status);
}

FT_TEST(test_transpiler_codegen_formats_perform_until)
{
    t_transpiler_cobol_procedure procedure;
    t_transpiler_cobol_paragraph *paragraph;
    t_transpiler_cobol_statement_block *block;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement *perform_statement;
    t_transpiler_cobol_statement_block *body;
    t_transpiler_cobol_statement *move_statement;
    char *procedure_text;
    int status;

    transpiler_cobol_procedure_init(&procedure);
    paragraph = transpiler_cobol_paragraph_create("main");
    if (!paragraph)
        return (FT_FAILURE);
    block = transpiler_cobol_paragraph_get_statements(paragraph);
    if (!block)
    {
        transpiler_cobol_paragraph_destroy(paragraph);
        return (FT_FAILURE);
    }
    procedure_text = NULL;
    perform_statement = NULL;
    status = FT_FAILURE;
    condition.left = "EOF-FLAG";
    condition.right = "'Y'";
    condition.op = TRANSPILE_COBOL_COMPARISON_EQUALS;
    condition.negated = 1;
    perform_statement = transpiler_cobol_statement_create_perform_until(&condition);
    if (!perform_statement)
        goto cleanup;
    body = transpiler_cobol_perform_until_get_body(perform_statement);
    if (!body)
        goto cleanup;
    move_statement = transpiler_cobol_statement_create_move("BUFFER", "OUTPUT-REC");
    if (!move_statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(body, move_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(move_statement);
        goto cleanup;
    }
    if (transpiler_cobol_statement_block_append(block, perform_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(perform_statement);
        perform_statement = NULL;
        goto cleanup;
    }
    perform_statement = NULL;
    if (transpiler_cobol_procedure_append(&procedure, paragraph) != FT_SUCCESS)
        goto cleanup;
    paragraph = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&procedure, &procedure_text),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure_text,
            "       PROCEDURE DIVISION.\n"
            "       MAIN.\n"
            "           PERFORM UNTIL NOT EOF-FLAG = 'Y'\n"
            "               MOVE BUFFER TO OUTPUT-REC\n"
            "           END-PERFORM\n",
            "procedure division should format PERFORM UNTIL loops") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure_text)
        cma_free(procedure_text);
    if (perform_statement)
        transpiler_cobol_statement_destroy(perform_statement);
    if (paragraph)
        transpiler_cobol_paragraph_destroy(paragraph);
    transpiler_cobol_procedure_dispose(&procedure);
    return (status);
}

FT_TEST(test_transpiler_codegen_formats_perform_varying)
{
    t_transpiler_cobol_procedure procedure;
    t_transpiler_cobol_paragraph *paragraph;
    t_transpiler_cobol_statement_block *block;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement *perform_statement;
    t_transpiler_cobol_statement_block *body;
    t_transpiler_cobol_statement *move_statement;
    char *procedure_text;
    int status;

    transpiler_cobol_procedure_init(&procedure);
    paragraph = transpiler_cobol_paragraph_create("main");
    if (!paragraph)
        return (FT_FAILURE);
    block = transpiler_cobol_paragraph_get_statements(paragraph);
    if (!block)
    {
        transpiler_cobol_paragraph_destroy(paragraph);
        return (FT_FAILURE);
    }
    procedure_text = NULL;
    perform_statement = NULL;
    status = FT_FAILURE;
    condition.left = "INDEX";
    condition.right = "10";
    condition.op = TRANSPILE_COBOL_COMPARISON_GREATER_THAN;
    condition.negated = 0;
    perform_statement = transpiler_cobol_statement_create_perform_varying("INDEX", "0", "1", &condition);
    if (!perform_statement)
        goto cleanup;
    body = transpiler_cobol_perform_varying_get_body(perform_statement);
    if (!body)
        goto cleanup;
    move_statement = transpiler_cobol_statement_create_move("INDEX", "SUM");
    if (!move_statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(body, move_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(move_statement);
        goto cleanup;
    }
    if (transpiler_cobol_statement_block_append(block, perform_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(perform_statement);
        perform_statement = NULL;
        goto cleanup;
    }
    perform_statement = NULL;
    if (transpiler_cobol_procedure_append(&procedure, paragraph) != FT_SUCCESS)
        goto cleanup;
    paragraph = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&procedure, &procedure_text),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure_text,
            "       PROCEDURE DIVISION.\n"
            "       MAIN.\n"
            "           PERFORM VARYING INDEX FROM 0 BY 1 UNTIL INDEX > 10\n"
            "               MOVE INDEX TO SUM\n"
            "           END-PERFORM\n",
            "procedure division should format PERFORM VARYING loops") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure_text)
        cma_free(procedure_text);
    if (perform_statement)
        transpiler_cobol_statement_destroy(perform_statement);
    if (paragraph)
        transpiler_cobol_paragraph_destroy(paragraph);
    transpiler_cobol_procedure_dispose(&procedure);
    return (status);
}

FT_TEST(test_transpiler_codegen_emits_call_with_return_slot)
{
    t_transpiler_cobol_procedure procedure;
    t_transpiler_cobol_paragraph *paragraph;
    t_transpiler_cobol_statement_block *block;
    t_transpiler_cobol_statement *call_statement;
    char *procedure_text;
    int status;

    transpiler_cobol_procedure_init(&procedure);
    paragraph = transpiler_cobol_paragraph_create("main");
    if (!paragraph)
        return (FT_FAILURE);
    block = transpiler_cobol_paragraph_get_statements(paragraph);
    if (!block)
    {
        transpiler_cobol_paragraph_destroy(paragraph);
        return (FT_FAILURE);
    }
    procedure_text = NULL;
    call_statement = NULL;
    status = FT_FAILURE;
    call_statement = transpiler_cobol_statement_create_call("worker");
    if (!call_statement)
        goto cleanup;
    if (transpiler_cobol_call_append_argument(call_statement, "ARG-ONE") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_cobol_call_append_argument(call_statement, "ARG-TWO") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_cobol_call_set_return_slot(call_statement, "RESULT-SLOT") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(block, call_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(call_statement);
        call_statement = NULL;
        goto cleanup;
    }
    call_statement = NULL;
    if (transpiler_cobol_procedure_append(&procedure, paragraph) != FT_SUCCESS)
        goto cleanup;
    paragraph = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&procedure, &procedure_text),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure_text,
            "       PROCEDURE DIVISION.\n"
            "       MAIN.\n"
            "           CALL 'WORKER' USING BY REFERENCE ARG-ONE BY REFERENCE ARG-TWO BY REFERENCE RESULT-SLOT\n",
            "procedure division should append return slot as final call argument") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure_text)
        cma_free(procedure_text);
    if (call_statement)
        transpiler_cobol_statement_destroy(call_statement);
    if (paragraph)
        transpiler_cobol_paragraph_destroy(paragraph);
    transpiler_cobol_procedure_dispose(&procedure);
    return (status);
}

FT_TEST(test_transpiler_codegen_emits_multiple_paragraphs)
{
    t_transpiler_cobol_procedure procedure;
    t_transpiler_cobol_paragraph *main_paragraph;
    t_transpiler_cobol_paragraph *helper_paragraph;
    t_transpiler_cobol_statement *statement;
    char *procedure_text;
    int status;

    transpiler_cobol_procedure_init(&procedure);
    main_paragraph = transpiler_cobol_paragraph_create("main");
    if (!main_paragraph)
        return (FT_FAILURE);
    helper_paragraph = transpiler_cobol_paragraph_create("helper");
    if (!helper_paragraph)
    {
        transpiler_cobol_paragraph_destroy(main_paragraph);
        return (FT_FAILURE);
    }
    procedure_text = NULL;
    statement = transpiler_cobol_statement_create_move("INPUT-REC", "OUTPUT-REC");
    if (!statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(transpiler_cobol_paragraph_get_statements(main_paragraph), statement)
        != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(statement);
        goto cleanup;
    }
    statement = transpiler_cobol_statement_create_move("ZERO", "OUTPUT-REC");
    if (!statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(transpiler_cobol_paragraph_get_statements(helper_paragraph), statement)
        != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(statement);
        goto cleanup;
    }
    statement = NULL;
    if (transpiler_cobol_procedure_append(&procedure, main_paragraph) != FT_SUCCESS)
        goto cleanup;
    main_paragraph = NULL;
    if (transpiler_cobol_procedure_append(&procedure, helper_paragraph) != FT_SUCCESS)
        goto cleanup;
    helper_paragraph = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&procedure, &procedure_text),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure_text,
            "       PROCEDURE DIVISION.\n"
            "       MAIN.\n"
            "           MOVE INPUT-REC TO OUTPUT-REC\n"
            "\n"
            "       HELPER.\n"
            "           MOVE ZERO TO OUTPUT-REC\n",
            "procedure division should emit multiple paragraphs with spacing") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure_text)
        cma_free(procedure_text);
    if (statement)
        transpiler_cobol_statement_destroy(statement);
    if (main_paragraph)
        transpiler_cobol_paragraph_destroy(main_paragraph);
    if (helper_paragraph)
        transpiler_cobol_paragraph_destroy(helper_paragraph);
    transpiler_cobol_procedure_dispose(&procedure);
    return (status);
}

const t_test_case *get_codegen_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_codegen_emits_text_file_sections", test_transpiler_codegen_emits_text_file_sections},
        {"transpiler_codegen_respects_explicit_record_length", test_transpiler_codegen_respects_explicit_record_length},
        {"transpiler_codegen_formats_if_else", test_transpiler_codegen_formats_if_else},
        {"transpiler_codegen_formats_perform_until", test_transpiler_codegen_formats_perform_until},
        {"transpiler_codegen_formats_perform_varying", test_transpiler_codegen_formats_perform_varying},
        {"transpiler_codegen_emits_call_with_return_slot", test_transpiler_codegen_emits_call_with_return_slot},
        {"transpiler_codegen_emits_multiple_paragraphs", test_transpiler_codegen_emits_multiple_paragraphs}
};

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
