#include "parser.hpp"
#include "transpiler_codegen.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

static char *test_round_trip_join_fragments(const char *a, const char *b,
    const char *c, const char *d, const char *e)
{
    size_t total_length;
    char *result;

    total_length = 0;
    if (a)
        total_length += ft_strlen(a);
    if (b)
        total_length += ft_strlen(b);
    if (c)
        total_length += ft_strlen(c);
    if (d)
        total_length += ft_strlen(d);
    if (e)
        total_length += ft_strlen(e);
    result = static_cast<char *>(cma_calloc(total_length + 1, sizeof(char)));
    if (!result)
        return (NULL);
    if (a)
        ft_strlcpy(result, a, total_length + 1);
    else
        result[0] = '\0';
    if (b)
        ft_strlcat(result, b, total_length + 1);
    if (c)
        ft_strlcat(result, c, total_length + 1);
    if (d)
        ft_strlcat(result, d, total_length + 1);
    if (e)
        ft_strlcat(result, e, total_length + 1);
    return (result);
}

static int test_codegen_round_trip_parses_generated_cobol(void)
{
    t_transpiler_context context;
    t_transpiler_cobol_file_sections sections;
    t_transpiler_cobol_procedure procedure;
    t_transpiler_cobol_paragraph *paragraph;
    t_transpiler_cobol_statement_block *block;
    t_transpiler_cobol_statement *statement;
    t_transpiler_cobol_statement_block *body;
    t_transpiler_cobol_statement *body_move;
    t_transpiler_cobol_condition condition;
    char *procedure_text;
    char *program_text;
    const char *identification;
    const char *working_storage;
    t_parser parser;
    t_ast_node *program;
    t_ast_node *division;
    t_ast_node *procedure_division;
    t_ast_node *paragraph_node;
    t_ast_node *sequence;
    t_ast_node *move_statement;
    t_ast_node *perform_until;
    t_ast_node *condition_node;
    t_ast_node *body_sequence;
    int status;

    status = FT_FAILURE;
    program = NULL;
    paragraph = NULL;
    statement = NULL;
    body_move = NULL;
    procedure_text = NULL;
    program_text = NULL;
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_CBL_C,
        TRANSPILE_LANGUAGE_COBOL);
    transpiler_codegen_file_sections_init(&sections);
    if (test_expect_success(transpiler_context_register_file(&context,
                "input_file", TRANSPILE_FILE_ROLE_INPUT, "records.txt", 0),
            "file registration should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_context_record_file_length_hint(&context,
                "input_file", 64),
            "record length hint should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_codegen_build_file_sections(&context,
                &sections), "file sections should be generated") != FT_SUCCESS)
        goto cleanup;
    transpiler_cobol_procedure_init(&procedure);
    paragraph = transpiler_cobol_paragraph_create("main");
    if (!paragraph)
        goto cleanup;
    block = transpiler_cobol_paragraph_get_statements(paragraph);
    if (!block)
        goto cleanup;
    statement = transpiler_cobol_statement_create_move("'N'", "EOF-FLAG");
    if (!statement)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(block, statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(statement);
        statement = NULL;
        goto cleanup;
    }
    statement = NULL;
    condition.left = "EOF-FLAG";
    condition.right = "'Y'";
    condition.op = TRANSPILE_COBOL_COMPARISON_EQUALS;
    condition.negated = 0;
    statement = transpiler_cobol_statement_create_perform_until(&condition);
    if (!statement)
        goto cleanup;
    body = transpiler_cobol_perform_until_get_body(statement);
    if (!body)
        goto cleanup;
    body_move = transpiler_cobol_statement_create_move("INPUT-REC",
        "OUTPUT-REC");
    if (!body_move)
        goto cleanup;
    if (transpiler_cobol_statement_block_append(body, body_move) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(body_move);
        goto cleanup;
    }
    body_move = NULL;
    if (transpiler_cobol_statement_block_append(block, statement) != FT_SUCCESS)
    {
        transpiler_cobol_statement_destroy(statement);
        statement = NULL;
        goto cleanup;
    }
    statement = NULL;
    if (transpiler_cobol_procedure_append(&procedure, paragraph) != FT_SUCCESS)
        goto cleanup;
    paragraph = NULL;
    if (test_expect_success(transpiler_codegen_build_procedure_division(&procedure,
                &procedure_text),
            "procedure division should be generated") != FT_SUCCESS)
        goto cleanup;
    identification = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ROUND-TRIP.\n";
    working_storage = "       WORKING-STORAGE SECTION.\n"
        "       01 EOF-FLAG PIC X VALUE 'N'.\n";
    program_text = test_round_trip_join_fragments(identification,
        sections.environment_division, sections.data_division,
        working_storage, procedure_text);
    if (!program_text)
        goto cleanup;
    parser_init(&parser, program_text);
    if (test_expect_success(parser_parse_program(&parser, &program),
            "parser should accept generated program") != FT_SUCCESS)
    {
        parser_dispose(&parser);
        goto cleanup;
    }
    parser_dispose(&parser);
    if (test_expect_success(program ? FT_SUCCESS : FT_FAILURE,
            "parser should produce an AST") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(program) == 4 ? FT_SUCCESS : FT_FAILURE,
            "program should include four divisions") != FT_SUCCESS)
        goto cleanup;
    division = ast_node_get_child(program, 0);
    if (test_expect_success(division && division->kind == AST_NODE_IDENTIFICATION_DIVISION ? FT_SUCCESS : FT_FAILURE,
            "first division should be IDENTIFICATION") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(division) == 1 ? FT_SUCCESS : FT_FAILURE,
            "identification division should have one child") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_get_child(division, 0)->kind == AST_NODE_PROGRAM_ID ? FT_SUCCESS : FT_FAILURE,
            "program id node should be present") != FT_SUCCESS)
        goto cleanup;
    procedure_division = ast_node_get_child(program, 3);
    if (test_expect_success(procedure_division && procedure_division->kind == AST_NODE_PROCEDURE_DIVISION ? FT_SUCCESS : FT_FAILURE,
            "procedure division should be present") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(procedure_division) == 1 ? FT_SUCCESS : FT_FAILURE,
            "procedure division should contain one paragraph") != FT_SUCCESS)
        goto cleanup;
    paragraph_node = ast_node_get_child(procedure_division, 0);
    if (test_expect_success(paragraph_node && paragraph_node->kind == AST_NODE_PARAGRAPH ? FT_SUCCESS : FT_FAILURE,
            "paragraph node should be parsed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(paragraph_node->token.lexeme ? FT_SUCCESS : FT_FAILURE,
            "paragraph should have a name") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ft_strncmp(paragraph_node->token.lexeme, "MAIN",
                paragraph_node->token.length) == 0 ? FT_SUCCESS : FT_FAILURE,
            "paragraph name should match generated identifier") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(paragraph_node) == 1 ? FT_SUCCESS : FT_FAILURE,
            "paragraph should contain statements") != FT_SUCCESS)
        goto cleanup;
    sequence = ast_node_get_child(paragraph_node, 0);
    if (test_expect_success(sequence && sequence->kind == AST_NODE_STATEMENT_SEQUENCE ? FT_SUCCESS : FT_FAILURE,
            "statement sequence should be parsed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(sequence) == 2 ? FT_SUCCESS : FT_FAILURE,
            "sequence should include move and perform statements") != FT_SUCCESS)
        goto cleanup;
    move_statement = ast_node_get_child(sequence, 0);
    if (test_expect_success(move_statement && move_statement->kind == AST_NODE_MOVE_STATEMENT ? FT_SUCCESS : FT_FAILURE,
            "first statement should be MOVE") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(move_statement) == 2 ? FT_SUCCESS : FT_FAILURE,
            "move statement should include value and target") != FT_SUCCESS)
        goto cleanup;
    perform_until = ast_node_get_child(sequence, 1);
    if (test_expect_success(perform_until && perform_until->kind == AST_NODE_PERFORM_UNTIL_STATEMENT ? FT_SUCCESS : FT_FAILURE,
            "second statement should be PERFORM UNTIL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(perform_until) == 2 ? FT_SUCCESS : FT_FAILURE,
            "perform statement should include condition and body") != FT_SUCCESS)
        goto cleanup;
    condition_node = ast_node_get_child(perform_until, 0);
    if (test_expect_success(condition_node && condition_node->kind == AST_NODE_CONDITION ? FT_SUCCESS : FT_FAILURE,
            "perform statement should expose condition") != FT_SUCCESS)
        goto cleanup;
    body_sequence = ast_node_get_child(perform_until, 1);
    if (test_expect_success(body_sequence && body_sequence->kind == AST_NODE_STATEMENT_SEQUENCE ? FT_SUCCESS : FT_FAILURE,
            "perform body should be a statement sequence") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_child_count(body_sequence) == 1 ? FT_SUCCESS : FT_FAILURE,
            "perform body should contain generated move") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_get_child(body_sequence, 0)->kind == AST_NODE_MOVE_STATEMENT ? FT_SUCCESS : FT_FAILURE,
            "perform body should include move statement") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (program)
        ast_node_destroy(program);
    if (program_text)
        cma_free(program_text);
    if (procedure_text)
        cma_free(procedure_text);
    if (paragraph)
        transpiler_cobol_paragraph_destroy(paragraph);
    if (statement)
        transpiler_cobol_statement_destroy(statement);
    transpiler_cobol_procedure_dispose(&procedure);
    transpiler_codegen_file_sections_dispose(&sections);
    transpiler_context_dispose(&context);
    return (status);
}

const t_test_case *get_round_trip_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"codegen_round_trip_parses_generated_cobol",
            test_codegen_round_trip_parses_generated_cobol}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
