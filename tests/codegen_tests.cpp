#include "transpiler_codegen.hpp"

#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

static int test_transpiler_codegen_emits_text_file_sections(void)
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
            "ENVIRONMENT DIVISION.\n"
            "    INPUT-OUTPUT SECTION.\n"
            "    FILE-CONTROL.\n"
            "        SELECT INPUT_FILE ASSIGN TO \"input.txt\"\n"
            "            ORGANIZATION IS LINE SEQUENTIAL.\n",
            "environment division should describe input file") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(sections.data_division,
            "DATA DIVISION.\n"
            "    FILE SECTION.\n"
            "    FD INPUT_FILE.\n"
            "        01 INPUT_FILE-REC PIC X(80).\n",
            "data division should size record from hint") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_codegen_file_sections_dispose(&sections);
    transpiler_context_dispose(&context);
    return (status);
}

static int test_transpiler_codegen_respects_explicit_record_length(void)
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
            "ENVIRONMENT DIVISION.\n"
            "    INPUT-OUTPUT SECTION.\n"
            "    FILE-CONTROL.\n"
            "        SELECT DATA_STORE ASSIGN TO \"store.bin\"\n"
            "            ORGANIZATION IS SEQUENTIAL.\n",
            "environment division should emit sequential organization for data file") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(sections.data_division,
            "DATA DIVISION.\n"
            "    FILE SECTION.\n"
            "    FD DATA_STORE.\n"
            "        01 DATA_STORE-REC PIC X(128).\n",
            "data division should preserve explicit record length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_codegen_file_sections_dispose(&sections);
    transpiler_context_dispose(&context);
    return (status);
}

static int test_transpiler_codegen_formats_if_else(void)
{
    t_transpiler_cobol_statement_block block;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement *if_statement;
    t_transpiler_cobol_statement_block *branch;
    t_transpiler_cobol_statement *move_statement;
    char *procedure;
    int status;

    transpiler_cobol_statement_block_init(&block);
    procedure = NULL;
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
    if (transpiler_cobol_statement_block_append(&block, if_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(if_statement);
        if_statement = NULL;
        goto cleanup;
    }
    if_statement = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&block, &procedure),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure,
            "PROCEDURE DIVISION.\n"
            "    IF FLAG = 'Y'\n"
            "        MOVE INPUT-REC TO OUTPUT-REC\n"
            "    ELSE\n"
            "        MOVE ZERO TO OUTPUT-REC\n"
            "    END-IF\n",
            "procedure division should format IF/ELSE branches") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure)
        cma_free(procedure);
    if (if_statement)
        transpiler_cobol_statement_destroy(if_statement);
    transpiler_cobol_statement_block_dispose(&block);
    return (status);
}

static int test_transpiler_codegen_formats_perform_until(void)
{
    t_transpiler_cobol_statement_block block;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement *perform_statement;
    t_transpiler_cobol_statement_block *body;
    t_transpiler_cobol_statement *move_statement;
    char *procedure;
    int status;

    transpiler_cobol_statement_block_init(&block);
    procedure = NULL;
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
    if (transpiler_cobol_statement_block_append(&block, perform_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(perform_statement);
        perform_statement = NULL;
        goto cleanup;
    }
    perform_statement = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&block, &procedure),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure,
            "PROCEDURE DIVISION.\n"
            "    PERFORM UNTIL NOT EOF-FLAG = 'Y'\n"
            "        MOVE BUFFER TO OUTPUT-REC\n"
            "    END-PERFORM\n",
            "procedure division should format PERFORM UNTIL loops") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure)
        cma_free(procedure);
    if (perform_statement)
        transpiler_cobol_statement_destroy(perform_statement);
    transpiler_cobol_statement_block_dispose(&block);
    return (status);
}

static int test_transpiler_codegen_formats_perform_varying(void)
{
    t_transpiler_cobol_statement_block block;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement *perform_statement;
    t_transpiler_cobol_statement_block *body;
    t_transpiler_cobol_statement *move_statement;
    char *procedure;
    int status;

    transpiler_cobol_statement_block_init(&block);
    procedure = NULL;
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
    if (transpiler_cobol_statement_block_append(&block, perform_statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(perform_statement);
        perform_statement = NULL;
        goto cleanup;
    }
    perform_statement = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&block, &procedure),
            "procedure generation should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(procedure,
            "PROCEDURE DIVISION.\n"
            "    PERFORM VARYING INDEX FROM 0 BY 1 UNTIL INDEX > 10\n"
            "        MOVE INDEX TO SUM\n"
            "    END-PERFORM\n",
            "procedure division should format PERFORM VARYING loops") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (procedure)
        cma_free(procedure);
    if (perform_statement)
        transpiler_cobol_statement_destroy(perform_statement);
    transpiler_cobol_statement_block_dispose(&block);
    return (status);
}

const t_test_case *get_codegen_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_codegen_emits_text_file_sections", test_transpiler_codegen_emits_text_file_sections},
        {"transpiler_codegen_respects_explicit_record_length", test_transpiler_codegen_respects_explicit_record_length},
        {"transpiler_codegen_formats_if_else", test_transpiler_codegen_formats_if_else},
        {"transpiler_codegen_formats_perform_until", test_transpiler_codegen_formats_perform_until},
        {"transpiler_codegen_formats_perform_varying", test_transpiler_codegen_formats_perform_varying}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
