#include "cblc_transpiler.hpp"

#include "libft/Printf/printf.hpp"

static int transpiler_logging_should_store(const t_transpiler_context *context, t_transpiler_severity severity)
{
    if (!context)
        return (0);
    if (severity == TRANSPILE_SEVERITY_ERROR)
        return (1);
    if (severity == TRANSPILE_SEVERITY_WARNING)
    {
        if (context->diagnostic_level == TRANSPILE_DIAGNOSTIC_SILENT)
            return (0);
        return (1);
    }
    if (severity == TRANSPILE_SEVERITY_INFO)
    {
        if (context->diagnostic_level == TRANSPILE_DIAGNOSTIC_VERBOSE)
            return (1);
        return (0);
    }
    return (0);
}

static const char *transpiler_logging_get_stage_name(const char *stage_name)
{
    if (stage_name && stage_name[0] != '\0')
        return (stage_name);
    return ("(anonymous)");
}

static const char *transpiler_logging_get_severity_label(t_transpiler_severity severity)
{
    if (severity == TRANSPILE_SEVERITY_INFO)
        return ("info");
    if (severity == TRANSPILE_SEVERITY_WARNING)
        return ("warning");
    if (severity == TRANSPILE_SEVERITY_ERROR)
        return ("error");
    return ("unknown");
}

static t_transpiler_severity transpiler_logging_resolve_severity(const t_transpiler_context *context,
    t_transpiler_severity severity)
{
    if (!context)
        return (severity);
    if (severity == TRANSPILE_SEVERITY_WARNING && context->warnings_as_errors)
        return (TRANSPILE_SEVERITY_ERROR);
    return (severity);
}

int transpiler_logging_emit(t_transpiler_context *context, t_transpiler_severity severity, int code, const char *message)
{
    t_transpiler_severity effective_severity;

    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    effective_severity = transpiler_logging_resolve_severity(context, severity);
    if (transpiler_logging_should_store(context, effective_severity) == 0)
        return (FT_SUCCESS);
    if (transpiler_diagnostics_push(&context->diagnostics, effective_severity, code, message) != FT_SUCCESS)
        return (FT_FAILURE);
    if (effective_severity == TRANSPILE_SEVERITY_ERROR)
    {
        if (code != 0)
            transpiler_context_record_error(context, code);
        else
            transpiler_context_record_error(context, FT_FAILURE);
    }
    return (FT_SUCCESS);
}

void transpiler_logging_stage_start(t_transpiler_context *context, const char *stage_name)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return ;
    pf_snprintf(message, sizeof(message), "Starting stage '%s'", transpiler_logging_get_stage_name(stage_name));
    transpiler_logging_emit(context, TRANSPILE_SEVERITY_INFO, 0, message);
}

void transpiler_logging_stage_success(t_transpiler_context *context, const char *stage_name)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return ;
    pf_snprintf(message, sizeof(message), "Completed stage '%s'", transpiler_logging_get_stage_name(stage_name));
    transpiler_logging_emit(context, TRANSPILE_SEVERITY_INFO, 0, message);
}

void transpiler_logging_stage_failure(t_transpiler_context *context, const char *stage_name, int error_code)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return ;
    pf_snprintf(message, sizeof(message), "Stage '%s' failed with code %d", transpiler_logging_get_stage_name(stage_name),
        error_code);
    transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, error_code, message);
    transpiler_context_record_error(context, error_code);
}

void transpiler_logging_flush(const t_transpiler_context *context)
{
    size_t index;
    const t_transpiler_diagnostic *diagnostic;
    const char *label;

    if (!context)
        return ;
    index = 0;
    while (index < context->diagnostics.count)
    {
        diagnostic = &context->diagnostics.items[index];
        label = transpiler_logging_get_severity_label(diagnostic->severity);
        if (diagnostic->severity == TRANSPILE_SEVERITY_ERROR)
        {
            pf_printf_fd(2, "[%s] (%d) %s\n", label, diagnostic->code, diagnostic->message);
        }
        else if (diagnostic->code != 0)
        {
            pf_printf("[%s] (%d) %s\n", label, diagnostic->code, diagnostic->message);
        }
        else
        {
            pf_printf("[%s] %s\n", label, diagnostic->message);
        }
        index += 1;
    }
}
