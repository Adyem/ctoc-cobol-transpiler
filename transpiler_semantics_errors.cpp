#include "transpiler_semantics_internal.hpp"

#include "libft/Libft/libft.hpp"

static void transpiler_semantics_build_span_from_node(const t_transpiler_context *context, const t_ast_node *node,
    t_transpiler_source_span *span)
{
    size_t token_length;

    if (!span)
        return ;
    ft_bzero(span, sizeof(*span));
    if (!context)
        return ;
    if (!node)
        return ;
    if (context->source_path)
        ft_strlcpy(span->path, context->source_path, TRANSPILE_FILE_PATH_MAX);
    span->start_line = node->token.line;
    span->start_column = node->token.column;
    span->end_line = node->token.line;
    token_length = node->token.length;
    if (token_length > 0 && node->token.column > 0)
        span->end_column = node->token.column + token_length - 1;
    else
        span->end_column = node->token.column;
}

static t_transpiler_warning_group transpiler_semantics_warning_group_from_code(int code)
{
    if (code >= TRANSPILE_WARNING_SEMANTIC_FLOAT_TO_DOUBLE
        && code <= TRANSPILE_WARNING_SEMANTIC_ALPHANUMERIC_TO_BOOLEAN)
        return (TRANSPILE_WARNING_GROUP_CONVERSION);
    if (code == TRANSPILE_WARNING_SEMANTIC_UNREACHABLE_CODE
        || code == TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM
        || code == TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM
        || code == TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE
        || code == TRANSPILE_WARNING_SEMANTIC_DUPLICATE_COPYBOOK_INCLUDE)
        return (TRANSPILE_WARNING_GROUP_UNUSED);
    return (TRANSPILE_WARNING_GROUP_CONVERSION);
}

int transpiler_semantics_emit_error(t_transpiler_context *context, int code,
    const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, code, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, code);
    return (FT_FAILURE);
}

int transpiler_semantics_emit_error_at(t_transpiler_context *context, const t_ast_node *node,
    int code, const char *message, const char *suggestion)
{
    t_transpiler_source_span span;
    char snippet[TRANSPILE_DIAGNOSTIC_SNIPPET_MAX];

    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (!node)
        return (transpiler_semantics_emit_error(context, code, message));
    transpiler_semantics_build_span_from_node(context, node, &span);
    snippet[0] = '\0';
    if (span.start_line > 0)
        (void)transpiler_context_get_line_snippet(context, span.start_line, snippet, sizeof(snippet));
    if (transpiler_logging_emit_with_details(context, TRANSPILE_SEVERITY_ERROR, code, message,
            &span, snippet, suggestion) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, code);
    return (FT_FAILURE);
}

int transpiler_semantics_emit_warning(t_transpiler_context *context, int code,
    const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (!transpiler_context_warning_group_enabled(context,
            transpiler_semantics_warning_group_from_code(code)))
        return (FT_SUCCESS);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_WARNING, code, message) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_semantics_emit_warning_at(t_transpiler_context *context, const t_ast_node *node,
    int code, const char *message, const char *suggestion)
{
    t_transpiler_source_span span;
    char snippet[TRANSPILE_DIAGNOSTIC_SNIPPET_MAX];

    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (!transpiler_context_warning_group_enabled(context,
            transpiler_semantics_warning_group_from_code(code)))
        return (FT_SUCCESS);
    if (!node)
        return (transpiler_semantics_emit_warning(context, code, message));
    transpiler_semantics_build_span_from_node(context, node, &span);
    snippet[0] = '\0';
    if (span.start_line > 0)
        (void)transpiler_context_get_line_snippet(context, span.start_line, snippet, sizeof(snippet));
    if (transpiler_logging_emit_with_details(context, TRANSPILE_SEVERITY_WARNING, code, message,
            &span, snippet, suggestion) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_semantics_emit_invalid_expression(t_transpiler_context *context,
    const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    return (transpiler_semantics_emit_error(context,
        TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION, message));
}
