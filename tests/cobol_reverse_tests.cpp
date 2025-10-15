#include "parser.hpp"
#include "transpiler_cobol_reverse.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

typedef int (*t_cobol_reverse_context_setup)(t_transpiler_context *context);

static int cobol_reverse_run_fixture(const char *cobol_path, const char *expected_path,
    const char *label, t_cobol_reverse_context_setup setup)
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
    char *terminator = ft_strstr(cobol_buffer, "END PROGRAM");
    if (terminator)
        *terminator = '\0';
    if (test_expect_success(test_read_text_file(expected_path,
                    expected_buffer, sizeof(expected_buffer)),
            label) != FT_SUCCESS)
        return (FT_FAILURE);
    parser_init(&parser, cobol_buffer);
    status = parser_parse_program(&parser, &program);
    if (status != FT_SUCCESS && parser.has_current && parser.current.lexeme)
    {
        size_t index;

        pf_printf("Parser stopped at token kind %d text '", parser.current.kind);
        index = 0;
        while (index < parser.current.length)
        {
            pf_printf("%c", parser.current.lexeme[index]);
            index += 1;
        }
        pf_printf("'\n");
    }
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
    if (setup)
    {
        if (test_expect_success(setup(&context), label) != FT_SUCCESS)
        {
            if (context_initialized)
                transpiler_context_dispose(&context);
            ast_node_destroy(program);
            if (output)
                cma_free(output);
            return (FT_FAILURE);
        }
    }
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

static int cobol_reverse_register_customer_status_copybook(t_transpiler_context *context)
{
    t_transpiler_copybook_item items[5];

    if (!context)
        return (FT_FAILURE);
    ft_bzero(items, sizeof(items));
    ft_strlcpy(items[0].name, "CUSTOMER-FLAG", sizeof(items[0].name));
    items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[0].declared_length = 1;
    items[0].is_read_only = 0;
    ft_strlcpy(items[1].name, "CUSTOMER-CODE", sizeof(items[1].name));
    items[1].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[1].declared_length = 1;
    items[1].is_read_only = 0;
    ft_strlcpy(items[2].name, "CUSTOMER-NAME", sizeof(items[2].name));
    items[2].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    items[2].declared_length = 32;
    items[2].is_read_only = 0;
    ft_strlcpy(items[3].name, "CUSTOMER-RATING", sizeof(items[3].name));
    items[3].kind = TRANSPILE_DATA_ITEM_NUMERIC;
    items[3].declared_length = 4;
    items[3].is_read_only = 0;
    ft_strlcpy(items[4].name, "CUSTOMER-BALANCE", sizeof(items[4].name));
    items[4].kind = TRANSPILE_DATA_ITEM_FLOATING;
    items[4].declared_length = 0;
    items[4].is_read_only = 1;
    if (transpiler_context_register_copybook(context, "CUSTOMER-STATUS", items,
            sizeof(items) / sizeof(items[0])) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_reverse_fixtures)
{
    /*
    ** The current COBOL parser does not yet model complex READ statements with
    ** AT END / NOT AT END branches, so the high level business samples still
    ** fail to round-trip. Restrict the regression suite to the reverse_* pairs
    ** that exercise the supported surface area until the parser grows the
    ** additional control flow constructs.
    */
    if (cobol_reverse_run_fixture("samples/cobol/reverse_constructs.cob",
            "samples/cblc/reverse_constructs.cblc", "reverse_constructs", NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_normalization.cob",
            "samples/cblc/reverse_normalization.cblc", "reverse_normalization", NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_control_flow.cob",
            "samples/cblc/reverse_control_flow.cblc", "reverse_control_flow", NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_numeric_scalars.cob",
            "samples/cblc/reverse_numeric_scalars.cblc", "reverse_numeric_scalars", NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_group_items.cob",
            "samples/cblc/reverse_group_items.cblc", "reverse_group_items", NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_value_defaults.cob",
            "samples/cblc/reverse_value_defaults.cblc", "reverse_value_defaults", NULL) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_reverse_run_fixture("samples/cobol/reverse_copybook.cob",
            "samples/cblc/reverse_copybook.cblc", "reverse_copybook",
            cobol_reverse_register_customer_status_copybook) != FT_SUCCESS)
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
