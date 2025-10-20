#include "test_suites.hpp"

FT_TEST(test_transpiler_context_record_comment_allocates_minimum_capacity)
{
    t_transpiler_context context;
    const char *text;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    text = "minimum capacity remark";
    if (test_expect_success(transpiler_context_record_comment(&context, 3, 2, text,
                ft_strlen(text)), "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.comment_capacity < 4)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: recording first comment should allocate minimum capacity\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_expands_capacity_for_additional_entries)
{
    t_transpiler_context context;
    const char *texts[5];
    size_t index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    texts[0] = "alpha";
    texts[1] = "bravo";
    texts[2] = "charlie";
    texts[3] = "delta";
    texts[4] = "echo";
    index = 0;
    while (index < 5)
    {
        if (test_expect_success(transpiler_context_record_comment(&context, index + 1, index + 2,
                    texts[index], ft_strlen(texts[index])), "recording comment should succeed") != FT_SUCCESS)
        {
            transpiler_context_dispose(&context);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (test_expect_size_t_equal(context.comment_count, 5,
            "context should retain all recorded comments") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.comment_capacity < 5)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: recording additional comments should expand capacity\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_retains_entries_after_growth)
{
    t_transpiler_context context;
    const char *first_text;
    const t_transpiler_comment *first_comment;
    size_t index;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    first_text = "initial remark";
    if (test_expect_success(transpiler_context_record_comment(&context, 7, 4, first_text,
                ft_strlen(first_text)), "recording first comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    first_comment = &context.comments[0];
    index = 0;
    while (index < 4)
    {
        const char *text;

        if (index == 0)
            text = "follow up one";
        else if (index == 1)
            text = "follow up two";
        else if (index == 2)
            text = "follow up three";
        else
            text = "follow up four";
        if (test_expect_success(transpiler_context_record_comment(&context, 8 + index, 5 + index,
                    text, ft_strlen(text)), "recording subsequent comment should succeed") != FT_SUCCESS)
        {
            transpiler_context_dispose(&context);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (test_expect_cstring_equal(first_comment->text, first_text,
            "first comment text should remain after growth") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(first_comment->line, 7,
            "first comment line should remain after growth") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(first_comment->column, 4,
            "first comment column should remain after growth") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_ignores_null_text)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 5, 1, NULL, 12),
            "recording null comment should succeed without changes") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 0,
            "recording null comment should not modify count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_dispose_clears_comment_storage)
{
    t_transpiler_context context;
    const char *text;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    text = "dispose remark";
    if (test_expect_success(transpiler_context_record_comment(&context, 11, 6, text,
                ft_strlen(text)), "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    if (context.comments)
    {
        pf_printf("Assertion failed: disposing context should release comment storage\n");
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 0,
            "disposing context should reset comment count") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(context.comment_capacity, 0,
            "disposing context should reset comment capacity") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(context.comment_emit_index, 0,
            "disposing context should reset emit index") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_accepts_zero_position)
{
    t_transpiler_context context;
    const t_transpiler_comment *comment;
    const char *text;

    text = "zero origin remark";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 0, 0, text,
                ft_strlen(text)), "recording comment at zero position should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 1,
            "context should track comment recorded at zero position") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    comment = &context.comments[0];
    if (test_expect_size_t_equal(comment->line, 0,
            "comment should retain zero line value") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(comment->column, 0,
            "comment should retain zero column value") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(comment->text, text,
            "comment should capture zero position text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_clear_comments_allows_reuse)
{
    t_transpiler_context context;
    const char *first_text;
    const char *second_text;
    t_transpiler_comment *allocated_comments;
    size_t allocated_capacity;

    first_text = "first reusable remark";
    second_text = "second reusable remark";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 4, 2, first_text,
                ft_strlen(first_text)), "recording first comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    allocated_comments = context.comments;
    allocated_capacity = context.comment_capacity;
    transpiler_context_clear_comments(&context);
    if (test_expect_size_t_equal(context.comment_count, 0,
            "clearing comments should reset recorded count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_record_comment(&context, 5, 3, second_text,
                ft_strlen(second_text)), "recording comment after clear should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.comments != allocated_comments)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: recording after clear should reuse allocation\n");
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_capacity, allocated_capacity,
            "recording after clear should retain capacity") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 1,
            "context should track new comment after clear") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.comments[0].text, second_text,
            "recorded comment after clear should store new text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_does_not_reallocate_within_capacity)
{
    t_transpiler_context context;
    const char *first_text;
    const char *second_text;
    t_transpiler_comment *allocated_comments;
    size_t allocated_capacity;

    first_text = "initial capacity remark";
    second_text = "follow up within capacity";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 6, 1, first_text,
                ft_strlen(first_text)), "recording first comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    allocated_comments = context.comments;
    allocated_capacity = context.comment_capacity;
    if (test_expect_success(transpiler_context_record_comment(&context, 7, 4, second_text,
                ft_strlen(second_text)), "recording second comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.comments != allocated_comments)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: comment storage should remain allocated within capacity\n");
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_capacity, allocated_capacity,
            "recording within capacity should not change capacity") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 2,
            "context should track both comments within capacity") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_retains_append_order_with_decreasing_lines)
{
    t_transpiler_context context;
    const t_transpiler_comment *first_comment;
    const t_transpiler_comment *second_comment;
    const char *first_text;
    const char *second_text;

    first_text = "later line remark";
    second_text = "earlier line remark";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 20, 5, first_text,
                ft_strlen(first_text)), "recording first comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_record_comment(&context, 10, 3, second_text,
                ft_strlen(second_text)), "recording second comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 2,
            "context should retain both comments regardless of ordering") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    first_comment = &context.comments[0];
    second_comment = &context.comments[1];
    if (test_expect_cstring_equal(first_comment->text, first_text,
            "first comment should remain in append position") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(first_comment->line, 20,
            "first comment should retain original line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(second_comment->text, second_text,
            "second comment should remain in append position") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(second_comment->line, 10,
            "second comment should retain decreasing line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_allows_duplicate_positions)
{
    t_transpiler_context context;
    const t_transpiler_comment *first_comment;
    const t_transpiler_comment *second_comment;
    const char *first_text;
    const char *second_text;

    first_text = "duplicate position first";
    second_text = "duplicate position second";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 14, 2, first_text,
                ft_strlen(first_text)), "recording first comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_record_comment(&context, 14, 2, second_text,
                ft_strlen(second_text)), "recording duplicate position comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 2,
            "context should retain both comments at duplicate position") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    first_comment = &context.comments[0];
    second_comment = &context.comments[1];
    if (test_expect_cstring_equal(first_comment->text, first_text,
            "first duplicate comment should retain text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(second_comment->text, second_text,
            "second duplicate comment should retain text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(first_comment->line, 14,
            "first duplicate comment should retain line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(second_comment->line, 14,
            "second duplicate comment should retain line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

