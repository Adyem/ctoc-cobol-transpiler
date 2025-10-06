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

typedef enum e_transpiler_format_mode
{
    TRANSPILE_FORMAT_DEFAULT = 0,
    TRANSPILE_FORMAT_MINIMAL,
    TRANSPILE_FORMAT_PRETTY
}   t_transpiler_format_mode;

typedef enum e_transpiler_diagnostic_level
{
    TRANSPILE_DIAGNOSTIC_SILENT = 0,
    TRANSPILE_DIAGNOSTIC_NORMAL,
    TRANSPILE_DIAGNOSTIC_VERBOSE
}   t_transpiler_diagnostic_level;

typedef struct s_transpiler_context
{
    t_transpiler_language source_language;
    t_transpiler_language target_language;
    const char *source_path;
    const char *target_path;
    const char *output_directory;
    t_transpiler_format_mode format_mode;
    t_transpiler_diagnostic_level diagnostic_level;
    t_transpiler_diagnostic_list diagnostics;
    int last_error_code;
}   t_transpiler_context;

int transpiler_context_init(t_transpiler_context *context);
void transpiler_context_dispose(t_transpiler_context *context);
void transpiler_context_set_languages(t_transpiler_context *context, t_transpiler_language source, t_transpiler_language target);
void transpiler_context_set_io_paths(t_transpiler_context *context, const char *source_path, const char *target_path);
void transpiler_context_set_output_directory(t_transpiler_context *context, const char *output_directory);
void transpiler_context_set_format_mode(t_transpiler_context *context, t_transpiler_format_mode mode);
void transpiler_context_set_diagnostic_level(t_transpiler_context *context, t_transpiler_diagnostic_level level);
void transpiler_context_record_error(t_transpiler_context *context, int error_code);
int transpiler_context_has_errors(const t_transpiler_context *context);

#endif
