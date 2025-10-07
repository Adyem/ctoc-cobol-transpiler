#include "parser.hpp"
#include "transpiler_cobol_reverse.hpp"

#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

static int cobol_reverse_run_fixture(const char *cobol_path, const char *expected_path, const char *label)
{
    t_parser parser;
    t_ast_node *program;
    t_transpiler_context context;
    char *output;
    char cobol_buffer[4096];
    char expected_buffer[4096];
    int status;
    int context_initialized;

    program = NULL;
    output = NULL;
    context_initialized = 0;
    if (test_expect_success(test_read_text_file(cobol_path,
                    cobol_buffer, sizeof(cobol_buffer)),
            label) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(test_read_text_file(expected_path,
                    expected_buffer, sizeof(expected_buffer)),
            label) != FT_SUCCESS)
        return (FT_FAILURE);
    parser_init(&parser, cobol_buffer);
    status = parser_parse_program(&parser, &program);
    if (status != FT_SUCCESS && parser.has_current && parser.current.lexeme)
        pf_printf("Parser stopped at token kind %d text '%.*s'\n",
            parser.current.kind, static_cast<int>(parser.current.length), parser.current.lexeme);
    if (test_expect_success(status,
            label) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        if (program)
            ast_node_destroy(program);
        return (FT_FAILURE);
    }
    parser_dispose(&parser);
    if (test_expect_success(program ? FT_SUCCESS : FT_FAILURE,
            label) != FT_SUCCESS)
    {
        if (program)
            ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_init(&context),
            label) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    context_initialized = 1;
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_COBOL,
        TRANSPILE_LANGUAGE_CBL_C);
    if (test_expect_success(transpiler_cobol_program_to_cblc(&context, program, &output),
            label) != FT_SUCCESS)
    {
        if (context_initialized)
            transpiler_context_dispose(&context);
        ast_node_destroy(program);
        if (output)
            cma_free(output);
        return (FT_FAILURE);
    }
    if (test_expect_success(output ? FT_SUCCESS : FT_FAILURE,
            label) != FT_SUCCESS)
    {
        if (context_initialized)
            transpiler_context_dispose(&context);
        ast_node_destroy(program);
        if (output)
            cma_free(output);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(output, expected_buffer,
            label) != FT_SUCCESS)
    {
        if (context_initialized)
            transpiler_context_dispose(&context);
        ast_node_destroy(program);
        if (output)
            cma_free(output);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_has_errors(&context) ? FT_FAILURE : FT_SUCCESS,
            label) != FT_SUCCESS)
    {
        if (context_initialized)
            transpiler_context_dispose(&context);
        ast_node_destroy(program);
        if (output)
            cma_free(output);
        return (FT_FAILURE);
    }
    if (context_initialized)
        transpiler_context_dispose(&context);
    ast_node_destroy(program);
    if (output)
        cma_free(output);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_reverse_fixtures)
{
    if (cobol_reverse_run_fixture("samples/cobol/reverse_constructs.cob",
            "samples/cblc/reverse_constructs.cblc", "reverse_constructs") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_normalization.cob",
            "samples/cblc/reverse_normalization.cblc", "reverse_normalization") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_control_flow.cob",
            "samples/cblc/reverse_control_flow.cblc", "reverse_control_flow") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_cobol_reverse_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_reverse_fixtures", test_cobol_reverse_fixtures}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
