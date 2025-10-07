#ifndef TRANSPILER_LOGGING_HPP
#define TRANSPILER_LOGGING_HPP

#include "transpiler_context.hpp"

int transpiler_logging_emit(t_transpiler_context *context, t_transpiler_severity severity, int code, const char *message);
void transpiler_logging_stage_start(t_transpiler_context *context, const char *stage_name);
void transpiler_logging_stage_success(t_transpiler_context *context, const char *stage_name);
void transpiler_logging_stage_failure(t_transpiler_context *context, const char *stage_name, int error_code);
void transpiler_logging_flush(const t_transpiler_context *context);

#endif
