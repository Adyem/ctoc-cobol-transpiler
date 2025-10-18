#include <cstdlib>

#include "libft/CMA/CMA.hpp"
#include "cblc_transpiler.hpp"

static int transpiler_diagnostics_reserve(t_transpiler_diagnostic_list *list, size_t desired_capacity)
{
    t_transpiler_diagnostic *new_items;

    if (!list)
        return (FT_FAILURE);
    if (list->capacity >= desired_capacity)
        return (FT_SUCCESS);
    new_items = static_cast<t_transpiler_diagnostic *>(cma_calloc(desired_capacity, sizeof(t_transpiler_diagnostic)));
    if (!new_items)
        return (FT_FAILURE);
    if (list->items)
    {
        ft_memcpy(new_items, list->items, list->count * sizeof(t_transpiler_diagnostic));
        cma_free(list->items);
    }
    list->items = new_items;
    list->capacity = desired_capacity;
    return (FT_SUCCESS);
}

int transpiler_diagnostics_init(t_transpiler_diagnostic_list *list)
{
    if (!list)
        return (FT_FAILURE);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
    if (transpiler_diagnostics_reserve(list, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

void transpiler_diagnostics_dispose(t_transpiler_diagnostic_list *list)
{
    if (!list)
        return ;
    if (list->items)
        cma_free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

int transpiler_diagnostics_push_with_details(t_transpiler_diagnostic_list *list, t_transpiler_severity severity, int code,
    const char *message, const t_transpiler_source_span *span, const char *snippet, const char *suggestion)
{
    t_transpiler_diagnostic *destination;

    if (!list)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (list->count >= list->capacity)
    {
        if (transpiler_diagnostics_reserve(list, list->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    destination = &list->items[list->count];
    destination->severity = severity;
    destination->code = code;
    ft_strlcpy(destination->message, message, TRANSPILE_DIAGNOSTIC_MESSAGE_MAX);
    ft_bzero(&destination->span, sizeof(destination->span));
    if (span)
    {
        ft_strlcpy(destination->span.path, span->path, TRANSPILE_FILE_PATH_MAX);
        destination->span.start_line = span->start_line;
        destination->span.start_column = span->start_column;
        destination->span.end_line = span->end_line;
        destination->span.end_column = span->end_column;
    }
    if (snippet)
        ft_strlcpy(destination->snippet, snippet, TRANSPILE_DIAGNOSTIC_SNIPPET_MAX);
    else
        destination->snippet[0] = '\0';
    if (suggestion)
        ft_strlcpy(destination->suggestion, suggestion, TRANSPILE_DIAGNOSTIC_MESSAGE_MAX);
    else
        destination->suggestion[0] = '\0';
    list->count += 1;
    return (FT_SUCCESS);
}

int transpiler_diagnostics_push(t_transpiler_diagnostic_list *list, t_transpiler_severity severity, int code, const char *message)
{
    return (transpiler_diagnostics_push_with_details(list, severity, code, message, NULL, NULL, NULL));
}
