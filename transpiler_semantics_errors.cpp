#include "transpiler_semantics_internal.hpp"

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
