#include "transpiler_semantics_internal.hpp"

static t_transpiler_warning_group transpiler_semantics_warning_group_from_code(int code)
{
    if (code >= TRANSPILE_WARNING_SEMANTIC_FLOAT_TO_DOUBLE
        && code <= TRANSPILE_WARNING_SEMANTIC_ALPHANUMERIC_TO_BOOLEAN)
        return (TRANSPILE_WARNING_GROUP_CONVERSION);
    if (code == TRANSPILE_WARNING_SEMANTIC_UNREACHABLE_CODE
        || code == TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM
        || code == TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM
        || code == TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE)
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
