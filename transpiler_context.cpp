#include "transpiler_context.hpp"

int transpiler_context_init(t_transpiler_context *context)
{
    if (!context)
        return (FT_FAILURE);
    context->source_language = TRANSPILE_LANGUAGE_NONE;
    context->target_language = TRANSPILE_LANGUAGE_NONE;
    context->source_path = NULL;
    context->target_path = NULL;
    context->last_error_code = FT_SUCCESS;
    if (transpiler_diagnostics_init(&context->diagnostics) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

void transpiler_context_dispose(t_transpiler_context *context)
{
    if (!context)
        return ;
    transpiler_diagnostics_dispose(&context->diagnostics);
    context->source_language = TRANSPILE_LANGUAGE_NONE;
    context->target_language = TRANSPILE_LANGUAGE_NONE;
    context->source_path = NULL;
    context->target_path = NULL;
    context->last_error_code = FT_SUCCESS;
}

void transpiler_context_set_languages(t_transpiler_context *context, t_transpiler_language source, t_transpiler_language target)
{
    if (!context)
        return ;
    context->source_language = source;
    context->target_language = target;
}

void transpiler_context_set_io_paths(t_transpiler_context *context, const char *source_path, const char *target_path)
{
    if (!context)
        return ;
    context->source_path = source_path;
    context->target_path = target_path;
}

void transpiler_context_record_error(t_transpiler_context *context, int error_code)
{
    if (!context)
        return ;
    context->last_error_code = error_code;
}

int transpiler_context_has_errors(const t_transpiler_context *context)
{
    if (!context)
        return (0);
    if (context->last_error_code != FT_SUCCESS)
        return (1);
    if (context->diagnostics.count > 0)
        return (1);
    return (0);
}
