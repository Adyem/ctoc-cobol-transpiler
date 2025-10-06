#ifndef TRANSPILER_CONTEXT_HPP
#define TRANSPILER_CONTEXT_HPP

#include "libft/Libft/libft.hpp"
#include "transpiler_diagnostics.hpp"

typedef enum e_transpiler_language
{
    TRANSPILE_LANGUAGE_NONE = 0,
    TRANSPILE_LANGUAGE_CBL_C,
    TRANSPILE_LANGUAGE_COBOL
}   t_transpiler_language;

typedef struct s_transpiler_context
{
    t_transpiler_language source_language;
    t_transpiler_language target_language;
    const char *source_path;
    const char *target_path;
    t_transpiler_diagnostic_list diagnostics;
    int last_error_code;
}   t_transpiler_context;

int transpiler_context_init(t_transpiler_context *context);
void transpiler_context_dispose(t_transpiler_context *context);
void transpiler_context_set_languages(t_transpiler_context *context, t_transpiler_language source, t_transpiler_language target);
void transpiler_context_set_io_paths(t_transpiler_context *context, const char *source_path, const char *target_path);
void transpiler_context_record_error(t_transpiler_context *context, int error_code);
int transpiler_context_has_errors(const t_transpiler_context *context);

#endif
