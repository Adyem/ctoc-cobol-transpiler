#include "test_suites.hpp"

FT_TEST(test_transpiler_context_record_comment_stores_metadata)
{
    t_transpiler_context context;
    const t_transpiler_comment *comment;
    const char *text;

    text = "* remark before statement";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 27, 3, text,
                ft_strlen(text)), "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 1,
            "context should track recorded comment") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    comment = &context.comments[0];
    if (test_expect_cstring_equal(comment->text, text,
            "comment should reference original text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(comment->length, ft_strlen(text),
            "comment should record text length") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(comment->line, 27,
            "comment should capture source line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(comment->column, 3,
            "comment should capture source column") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_clear_comments_resets_state)
{
    t_transpiler_context context;
    const char *text;

    text = "* paragraph remark";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 12, 6, text,
                ft_strlen(text)), "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    context.comment_emit_index = 5;
    transpiler_context_clear_comments(&context);
    if (test_expect_size_t_equal(context.comment_count, 0,
            "clear should reset recorded comment count") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_emit_index, 0,
            "clear should reset emit index") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_reset_comment_iteration_rewinds_index)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    context.comment_emit_index = 9;
    transpiler_context_reset_comment_iteration(&context);
    if (test_expect_size_t_equal(context.comment_emit_index, 0,
            "reset should rewind emit index") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_ignores_empty_text)
{
    t_transpiler_context context;
    const char *text;

    text = "*";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 4, 1, text, 0),
            "empty comment should be ignored") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 0,
            "context should not retain empty comment") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_appends_in_order)
{
    t_transpiler_context context;
    const t_transpiler_comment *first_comment;
    const t_transpiler_comment *second_comment;
    const char *first_text;
    const char *second_text;

    first_text = "* first remark";
    second_text = "* second remark";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 10, 2, first_text,
                ft_strlen(first_text)), "recording first comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_record_comment(&context, 12, 5, second_text,
                ft_strlen(second_text)), "recording second comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_count, 2,
            "context should retain both recorded comments") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    first_comment = &context.comments[0];
    second_comment = &context.comments[1];
    if (test_expect_cstring_equal(first_comment->text, first_text,
            "first comment text should match input") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(first_comment->line, 10,
            "first comment should track source line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(first_comment->column, 2,
            "first comment should track source column") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(second_comment->text, second_text,
            "second comment text should match input") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(second_comment->line, 12,
            "second comment should track source line") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(second_comment->column, 5,
            "second comment should track source column") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_preserves_emit_index)
{
    t_transpiler_context context;
    const char *text;

    text = "* remark during iteration";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    context.comment_emit_index = 4;
    if (test_expect_success(transpiler_context_record_comment(&context, 18, 7, text,
                ft_strlen(text)), "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(context.comment_emit_index, 4,
            "recording comment should not advance emit index") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_respects_provided_length)
{
    t_transpiler_context context;
    const t_transpiler_comment *comment;
    const char *text;
    size_t reported_length;

    text = "* remark with trailing detail";
    reported_length = 9;
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 22, 1, text, reported_length),
            "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    comment = &context.comments[0];
    if (test_expect_size_t_equal(comment->length, reported_length,
            "comment should store provided length") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ft_strncmp(comment->text, text, reported_length) != 0)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: comment should preserve prefix text\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_clear_comments_preserves_allocation)
{
    t_transpiler_context context;
    const char *text;
    t_transpiler_comment *allocated_comments;
    size_t allocated_capacity;

    text = "* remark before reuse";
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_record_comment(&context, 6, 3, text,
                ft_strlen(text)), "recording comment should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.comment_capacity == 0 || !context.comments)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: expected comment storage to be allocated\n");
        return (FT_FAILURE);
    }
    allocated_comments = context.comments;
    allocated_capacity = context.comment_capacity;
    transpiler_context_clear_comments(&context);
    if (test_expect_size_t_equal(context.comment_capacity, allocated_capacity,
            "clearing comments should not release capacity") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (context.comments != allocated_comments)
    {
        transpiler_context_dispose(&context);
        pf_printf("Assertion failed: clearing comments should retain allocation pointer\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_comment_helpers_accept_null_context)
{
    transpiler_context_clear_comments(NULL);
    transpiler_context_reset_comment_iteration(NULL);
    if (test_expect_int_equal(transpiler_context_record_comment(NULL, 3, 1, "* remark", 1), FT_FAILURE,
            "record comment should still reject null context") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_context_record_comment_rejects_null_context)
{
    if (test_expect_int_equal(transpiler_context_record_comment(NULL, 1, 1,
                "* remark", sizeof("* remark") - 1), FT_FAILURE,
            "record comment should reject null context") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

