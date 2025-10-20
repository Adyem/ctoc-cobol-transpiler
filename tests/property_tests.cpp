#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

#include "test_suites.hpp"

typedef struct s_round_trip_fixture
{
    const char *cobol_path;
    const char *label;
    int requires_copybook;
}   t_round_trip_fixture;

static int register_customer_status_copybook(t_transpiler_context *context)
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

static int run_round_trip_fixture(const t_round_trip_fixture *fixture)
{
    t_parser parser;
    t_parser regenerated_parser;
    t_ast_node *program;
    t_ast_node *regenerated_program;
    t_transpiler_context context;
    t_cblc_translation_unit unit;
    char cobol_buffer[32768];
    char *first_pass_cblc;
    char *second_pass_cblc;
    char *normalized_first;
    char *normalized_second;
    char *generated_cobol;
    char *generated_copy;
    char *terminator;
    size_t generated_length;
    int context_initialized;
    int unit_initialized;
    int status;
    int message_status;

    program = NULL;
    regenerated_program = NULL;
    first_pass_cblc = NULL;
    second_pass_cblc = NULL;
    normalized_first = NULL;
    normalized_second = NULL;
    generated_cobol = NULL;
    generated_copy = NULL;
    context_initialized = 0;
    unit_initialized = 0;
    status = FT_FAILURE;
    if (!fixture)
        return (FT_FAILURE);
    if (test_expect_success(test_read_text_file(fixture->cobol_path,
                cobol_buffer, sizeof(cobol_buffer)),
            fixture->label) != FT_SUCCESS)
        return (FT_FAILURE);
    terminator = ft_strstr(cobol_buffer, "END PROGRAM");
    if (terminator)
        *terminator = '\0';
    if (test_expect_success(transpiler_context_init(&context), fixture->label)
        != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_COBOL,
        TRANSPILE_LANGUAGE_CBL_C);
    context.active_source_text = cobol_buffer;
    context.active_source_length = ft_strlen(cobol_buffer);
    transpiler_context_clear_comments(&context);
    parser_init_with_context(&parser, cobol_buffer, &context);
    status = parser_parse_program(&parser, &program);
    parser_dispose(&parser);
    if (test_expect_success(status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (program)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (fixture->requires_copybook)
    {
        if (test_expect_success(register_customer_status_copybook(&context),
                fixture->label) != FT_SUCCESS)
            goto cleanup;
    }
    if (test_expect_success(transpiler_cobol_program_to_cblc(&context, program,
                &first_pass_cblc), fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (first_pass_cblc)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cblc(first_pass_cblc),
            fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_has_errors(&context))
        message_status = FT_FAILURE;
    else
        message_status = FT_SUCCESS;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    cblc_translation_unit_init(&unit);
    unit_initialized = 1;
    if (test_expect_success(cblc_parse_translation_unit(first_pass_cblc, &unit),
            fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (generated_cobol)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            fixture->label) != FT_SUCCESS)
        goto cleanup;
    generated_length = ft_strlen(generated_cobol);
    generated_copy = static_cast<char *>(cma_calloc(generated_length + 1,
            sizeof(char)));
    if (!generated_copy)
        goto cleanup;
    ft_strlcpy(generated_copy, generated_cobol, generated_length + 1);
    terminator = ft_strstr(generated_copy, "END PROGRAM");
    if (terminator)
        *terminator = '\0';
    context.active_source_text = generated_copy;
    context.active_source_length = ft_strlen(generated_copy);
    transpiler_context_clear_comments(&context);
    parser_init_with_context(&regenerated_parser, generated_copy, &context);
    status = parser_parse_program(&regenerated_parser, &regenerated_program);
    parser_dispose(&regenerated_parser);
    if (test_expect_success(status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (regenerated_program)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    transpiler_context_reset_unit_state(&context);
    context.active_source_text = generated_copy;
    context.active_source_length = ft_strlen(generated_copy);
    if (test_expect_success(transpiler_cobol_program_to_cblc(&context,
                regenerated_program, &second_pass_cblc), fixture->label)
        != FT_SUCCESS)
        goto cleanup;
    if (second_pass_cblc)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cblc(second_pass_cblc),
            fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_has_errors(&context))
        message_status = FT_FAILURE;
    else
        message_status = FT_SUCCESS;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_cblc_apply_layout(first_pass_cblc,
                TRANSPILE_LAYOUT_NORMALIZE, TRANSPILE_FORMAT_PRETTY,
                &normalized_first), fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (normalized_first)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_cblc_apply_layout(second_pass_cblc,
                TRANSPILE_LAYOUT_NORMALIZE, TRANSPILE_FORMAT_PRETTY,
                &normalized_second), fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (normalized_second)
        message_status = FT_SUCCESS;
    else
        message_status = FT_FAILURE;
    if (test_expect_success(message_status, fixture->label) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(normalized_first, normalized_second,
            "normalized round-trip should be stable") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (normalized_second)
        cma_free(normalized_second);
    if (normalized_first)
        cma_free(normalized_first);
    if (second_pass_cblc)
        cma_free(second_pass_cblc);
    if (first_pass_cblc)
        cma_free(first_pass_cblc);
    if (generated_copy)
        cma_free(generated_copy);
    if (generated_cobol)
        cma_free(generated_cobol);
    if (unit_initialized)
        cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    if (regenerated_program)
        ast_node_destroy(regenerated_program);
    if (program)
        ast_node_destroy(program);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_normalization_is_idempotent)
{
    static const char *samples[] = {
        "samples/cblc/reverse_constructs.cblc",
        "samples/cblc/reverse_control_flow.cblc",
        "samples/cblc/reverse_numeric_scalars.cblc",
        "samples/cblc/reverse_group_items.cblc",
        "samples/cblc/reverse_value_defaults.cblc",
        "samples/cblc/reverse_copybook.cblc"
    };
    size_t count;
    size_t index;
    char buffer[32768];
    char *first_pass;
    char *second_pass;

    count = sizeof(samples) / sizeof(samples[0]);
    index = 0;
    while (index < count)
    {
        first_pass = NULL;
        second_pass = NULL;
        if (test_expect_success(test_read_text_file(samples[index], buffer,
                    sizeof(buffer)), samples[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        if (test_expect_success(transpiler_cblc_apply_layout(buffer,
                    TRANSPILE_LAYOUT_NORMALIZE, TRANSPILE_FORMAT_PRETTY,
                    &first_pass), samples[index]) != FT_SUCCESS)
        {
            if (first_pass)
                cma_free(first_pass);
            return (FT_FAILURE);
        }
        if (!first_pass)
        {
            return (FT_FAILURE);
        }
        if (test_expect_success(transpiler_cblc_apply_layout(first_pass,
                    TRANSPILE_LAYOUT_NORMALIZE, TRANSPILE_FORMAT_PRETTY,
                    &second_pass), samples[index]) != FT_SUCCESS)
        {
            if (second_pass)
                cma_free(second_pass);
            cma_free(first_pass);
            return (FT_FAILURE);
        }
        if (!second_pass)
        {
            cma_free(first_pass);
            return (FT_FAILURE);
        }
        if (test_expect_cstring_equal(first_pass, second_pass,
                "normalized layout should remain stable") != FT_SUCCESS)
        {
            cma_free(second_pass);
            cma_free(first_pass);
            return (FT_FAILURE);
        }
        cma_free(second_pass);
        cma_free(first_pass);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_round_trip_is_idempotent)
{
    static const t_round_trip_fixture fixtures[] = {
        {"samples/cobol/reverse_constructs.cob", "reverse_constructs", 0},
        {"samples/cobol/reverse_normalization.cob", "reverse_normalization", 0},
        {"samples/cobol/reverse_control_flow.cob", "reverse_control_flow", 0},
        {"samples/cobol/reverse_numeric_scalars.cob", "reverse_numeric_scalars", 0},
        {"samples/cobol/reverse_group_items.cob", "reverse_group_items", 0},
        {"samples/cobol/reverse_value_defaults.cob", "reverse_value_defaults", 0},
        {"samples/cobol/reverse_copybook.cob", "reverse_copybook", 1}
    };
    size_t count;
    size_t index;

    count = sizeof(fixtures) / sizeof(fixtures[0]);
    index = 0;
    while (index < count)
    {
        if (run_round_trip_fixture(&fixtures[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

const t_test_case *get_property_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cblc_normalization_is_idempotent", test_cblc_normalization_is_idempotent},
        {"cobol_round_trip_is_idempotent", test_cobol_round_trip_is_idempotent}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
