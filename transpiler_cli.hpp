#ifndef TRANSPILER_CLI_HPP
#define TRANSPILER_CLI_HPP

#include "transpiler_context.hpp"

typedef struct s_transpiler_cli_options
{
    const char **input_paths;
    size_t input_count;
    size_t input_capacity;
    const char **output_paths;
    size_t output_count;
    size_t output_capacity;
    const char *output_directory;
    t_transpiler_language source_language;
    t_transpiler_language target_language;
    t_transpiler_format_mode format_mode;
    t_transpiler_diagnostic_level diagnostic_level;
    int warnings_as_errors;
    int show_help;
}   t_transpiler_cli_options;

int transpiler_cli_options_init(t_transpiler_cli_options *options);
void transpiler_cli_options_dispose(t_transpiler_cli_options *options);
int transpiler_cli_parse(t_transpiler_cli_options *options, int argc, const char **argv);
int transpiler_cli_apply(const t_transpiler_cli_options *options, t_transpiler_context *context);
void transpiler_cli_print_usage(void);

#endif
