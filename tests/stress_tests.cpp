#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "test_suites.hpp"

static int test_stress_append_fragment(char *buffer, size_t buffer_size, size_t *used,
    const char *fragment)
{
    size_t fragment_length;
    size_t index;

    if (!buffer || buffer_size == 0 || !used || !fragment)
        return (FT_FAILURE);
    fragment_length = ft_strlen(fragment);
    if (*used + fragment_length >= buffer_size)
        return (FT_FAILURE);
    index = 0;
    while (index < fragment_length)
    {
        buffer[*used + index] = fragment[index];
        index += 1;
    }
    *used += fragment_length;
    buffer[*used] = '\0';
    return (FT_SUCCESS);
}

static int test_stress_append_indent(char *buffer, size_t buffer_size, size_t *used,
    size_t depth)
{
    char indentation[256];
    size_t indent_length;
    size_t index;

    if (!buffer || buffer_size == 0 || !used)
        return (FT_FAILURE);
    indent_length = 7 + depth * 4;
    if (indent_length >= sizeof(indentation))
        indent_length = sizeof(indentation) - 1;
    index = 0;
    while (index < indent_length)
    {
        indentation[index] = ' ';
        index += 1;
    }
    indentation[indent_length] = '\0';
    return (test_stress_append_fragment(buffer, buffer_size, used, indentation));
}

static int test_stress_append_long_move(char *buffer, size_t buffer_size, size_t *used,
    size_t literal_length)
{
    char *literal;
    size_t index;
    int status;

    if (!buffer || buffer_size == 0 || !used)
        return (FT_FAILURE);
    literal = static_cast<char *>(cma_calloc(literal_length + 1, sizeof(char)));
    if (!literal)
        return (FT_FAILURE);
    index = 0;
    while (index < literal_length)
    {
        literal[index] = static_cast<char>('A' + (index % 26));
        index += 1;
    }
    literal[literal_length] = '\0';
    if (test_stress_append_fragment(buffer, buffer_size, used, "       MOVE \"") != FT_SUCCESS)
    {
        cma_free(literal);
        return (FT_FAILURE);
    }
    if (test_stress_append_fragment(buffer, buffer_size, used, literal) != FT_SUCCESS)
    {
        cma_free(literal);
        return (FT_FAILURE);
    }
    status = test_stress_append_fragment(buffer, buffer_size, used, "\" TO HUGE-FIELD.\n");
    cma_free(literal);
    return (status);
}

static int test_stress_build_program(size_t depth, size_t literal_length, char **out_text)
{
    const char *header;
    const char *footer;
    const char *wide_limit;
    char *program_text;
    size_t buffer_size;
    size_t used;
    size_t level;

    if (!out_text)
        return (FT_FAILURE);
    header = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRESS.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 HUGE-RECORD.\n"
        "          05 HUGE-FIELD PIC X(1024).\n"
        "       01 WIDE-NUMERIC PIC 9(36).\n"
        "       01 DEPTH-COUNTER PIC 9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "       MOVE ZERO TO DEPTH-COUNTER.\n"
        "       MOVE ZERO TO WIDE-NUMERIC.\n";
    footer = "       STOP RUN.\n"
        "       END PROGRAM STRESS.\n";
    wide_limit = "900000000000000000";
    buffer_size = ft_strlen(header) + ft_strlen(footer) + depth * 256 + literal_length + 1024;
    program_text = static_cast<char *>(cma_calloc(buffer_size + 1, sizeof(char)));
    if (!program_text)
        return (FT_FAILURE);
    used = 0;
    if (test_stress_append_fragment(program_text, buffer_size + 1, &used, header) != FT_SUCCESS)
    {
        cma_free(program_text);
        return (FT_FAILURE);
    }
    if (test_stress_append_long_move(program_text, buffer_size + 1, &used, literal_length) != FT_SUCCESS)
    {
        cma_free(program_text);
        return (FT_FAILURE);
    }
    level = 0;
    while (level < depth)
    {
        if (test_stress_append_indent(program_text, buffer_size + 1, &used, level) != FT_SUCCESS)
        {
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (test_stress_append_fragment(program_text, buffer_size + 1, &used, "IF WIDE-NUMERIC < ") != FT_SUCCESS)
        {
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (test_stress_append_fragment(program_text, buffer_size + 1, &used, wide_limit) != FT_SUCCESS)
        {
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (test_stress_append_fragment(program_text, buffer_size + 1, &used, " THEN\n") != FT_SUCCESS)
        {
            cma_free(program_text);
            return (FT_FAILURE);
        }
        level += 1;
    }
    if (test_stress_append_indent(program_text, buffer_size + 1, &used, depth) != FT_SUCCESS)
    {
        cma_free(program_text);
        return (FT_FAILURE);
    }
    if (test_stress_append_fragment(program_text, buffer_size + 1, &used,
            "COMPUTE WIDE-NUMERIC = 123456789012345678.\n") != FT_SUCCESS)
    {
        cma_free(program_text);
        return (FT_FAILURE);
    }
    while (depth > 0)
    {
        depth -= 1;
        if (test_stress_append_indent(program_text, buffer_size + 1, &used, depth) != FT_SUCCESS)
        {
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (test_stress_append_fragment(program_text, buffer_size + 1, &used, "END-IF.\n") != FT_SUCCESS)
        {
            cma_free(program_text);
            return (FT_FAILURE);
        }
    }
    if (test_stress_append_fragment(program_text, buffer_size + 1, &used, footer) != FT_SUCCESS)
    {
        cma_free(program_text);
        return (FT_FAILURE);
    }
    *out_text = program_text;
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_handles_large_length)
{
    t_runtime_record record;
    size_t length;
    char *buffer;
    size_t index;
    char *copy;
    size_t written;

    length = 16384;
    if (test_expect_success(runtime_record_init(&record, length),
            "record init should support large lengths") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_record_set_length(&record, length),
            "record length should expand to large size") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    buffer = static_cast<char *>(cma_calloc(length, sizeof(char)));
    if (!buffer)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < length)
    {
        buffer[index] = static_cast<char>('A' + (index % 26));
        index += 1;
    }
    if (test_expect_success(runtime_record_copy_from_buffer(&record, buffer, length),
            "record should copy large payloads") != FT_SUCCESS)
    {
        cma_free(buffer);
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    copy = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!copy)
    {
        cma_free(buffer);
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_record_copy_to_buffer(&record, copy, length + 1, &written),
            "record should round-trip large payloads") != FT_SUCCESS)
    {
        cma_free(copy);
        cma_free(buffer);
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(written, length,
            "record copy should report full length") != FT_SUCCESS)
    {
        cma_free(copy);
        cma_free(buffer);
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_success(ft_memcmp(copy, buffer, length) == 0 ? FT_SUCCESS : FT_FAILURE,
            "record data should match original buffer") != FT_SUCCESS)
    {
        cma_free(copy);
        cma_free(buffer);
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    cma_free(copy);
    cma_free(buffer);
    runtime_record_dispose(&record);
    return (FT_SUCCESS);
}

FT_TEST(test_parser_handles_stress_program)
{
    const size_t depth = 8;
    const size_t literal_length = 256;
    char *program_text;
    t_lexer lexer;
    t_lexer_token token;
    size_t token_count;
    size_t if_count;
    const char *wide_literal;
    const char *literal_start;
    const char *literal_end;
    size_t long_literal_length;
    const char *failure_stage;
    int status;

    program_text = NULL;
    status = FT_FAILURE;
    failure_stage = "build program";
    if (test_stress_build_program(depth, literal_length, &program_text) != FT_SUCCESS)
        goto cleanup;
    lexer_init(&lexer, program_text);
    token_count = 0;
    if_count = 0;
    while (lexer_next_token(&lexer, &token) == FT_SUCCESS)
    {
        token_count += 1;
        if (token.kind == LEXER_TOKEN_KEYWORD_IF)
            if_count += 1;
        if (token.kind == LEXER_TOKEN_END_OF_FILE)
            break ;
    }
    if (token.kind != LEXER_TOKEN_END_OF_FILE)
    {
        failure_stage = "lexing";
        goto cleanup;
    }
    if (if_count < depth)
    {
        failure_stage = "if count";
        goto cleanup;
    }
    if (!ft_strnstr(program_text, "X(1024)", ft_strlen(program_text)))
    {
        failure_stage = "picture literal";
        goto cleanup;
    }
    literal_start = ft_strnstr(program_text, "MOVE \"", ft_strlen(program_text));
    if (!literal_start)
    {
        failure_stage = "long move";
        goto cleanup;
    }
    literal_start += ft_strlen("MOVE \"");
    literal_end = ft_strnstr(literal_start, "\" TO HUGE-FIELD.", ft_strlen(literal_start));
    if (!literal_end)
    {
        failure_stage = "move terminator";
        goto cleanup;
    }
    long_literal_length = static_cast<size_t>(literal_end - literal_start);
    if (long_literal_length != literal_length)
    {
        failure_stage = "literal length";
        goto cleanup;
    }
    wide_literal = "COMPUTE WIDE-NUMERIC = 123456789012345678.";
    if (!ft_strnstr(program_text, wide_literal, ft_strlen(program_text)))
    {
        failure_stage = "compute literal";
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
    {
        if (failure_stage)
            pf_printf("Stress program failure: %s\n", failure_stage);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

const t_test_case *get_stress_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_record_handles_large_length", test_runtime_record_handles_large_length},
        {"parser_handles_stress_program", test_parser_handles_stress_program}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
