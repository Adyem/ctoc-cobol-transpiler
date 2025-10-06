#include "transpiler_cli.hpp"

#include <cstdlib>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int transpiler_cli_parse_direction_value(const char *value, t_transpiler_cli_options *options)
{
    size_t length;

    if (!value || !options)
        return (FT_FAILURE);
    length = ft_strlen("cblc-to-cobol");
    if (ft_strncmp(value, "cblc-to-cobol", length + 1) == 0)
    {
        options->source_language = TRANSPILE_LANGUAGE_CBL_C;
        options->target_language = TRANSPILE_LANGUAGE_COBOL;
        return (FT_SUCCESS);
    }
    length = ft_strlen("cobol-to-cblc");
    if (ft_strncmp(value, "cobol-to-cblc", length + 1) == 0)
    {
        options->source_language = TRANSPILE_LANGUAGE_COBOL;
        options->target_language = TRANSPILE_LANGUAGE_CBL_C;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown direction '%s'. Expected 'cblc-to-cobol' or 'cobol-to-cblc'.\n", value);
    return (FT_FAILURE);
}

static int transpiler_cli_apply_direction_from_env(t_transpiler_cli_options *options)
{
    const char *value;

    if (!options)
        return (FT_FAILURE);
    if (options->source_language != TRANSPILE_LANGUAGE_NONE)
        return (FT_SUCCESS);
    value = std::getenv("CTOC_DEFAULT_DIRECTION");
    if (!value)
    {
        pf_printf("Missing required --direction option or CTOC_DEFAULT_DIRECTION environment variable.\n");
        return (FT_FAILURE);
    }
    return (transpiler_cli_parse_direction_value(value, options));
}

static int transpiler_cli_require_paths(const t_transpiler_cli_options *options)
{
    if (!options)
        return (FT_FAILURE);
    if (options->show_help)
        return (FT_SUCCESS);
    if (!options->input_path)
    {
        pf_printf("Missing required --input option.\n");
        return (FT_FAILURE);
    }
    if (!options->output_path)
    {
        pf_printf("Missing required --output option.\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int transpiler_cli_parse_format_value(const char *value, t_transpiler_cli_options *options)
{
    size_t length;

    if (!value || !options)
        return (FT_FAILURE);
    length = ft_strlen("default");
    if (ft_strncmp(value, "default", length + 1) == 0)
    {
        options->format_mode = TRANSPILE_FORMAT_DEFAULT;
        return (FT_SUCCESS);
    }
    length = ft_strlen("minimal");
    if (ft_strncmp(value, "minimal", length + 1) == 0)
    {
        options->format_mode = TRANSPILE_FORMAT_MINIMAL;
        return (FT_SUCCESS);
    }
    length = ft_strlen("pretty");
    if (ft_strncmp(value, "pretty", length + 1) == 0)
    {
        options->format_mode = TRANSPILE_FORMAT_PRETTY;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown format '%s'. Expected 'default', 'minimal', or 'pretty'.\n", value);
    return (FT_FAILURE);
}

static int transpiler_cli_parse_diagnostics_value(const char *value, t_transpiler_cli_options *options)
{
    size_t length;

    if (!value || !options)
        return (FT_FAILURE);
    length = ft_strlen("silent");
    if (ft_strncmp(value, "silent", length + 1) == 0)
    {
        options->diagnostic_level = TRANSPILE_DIAGNOSTIC_SILENT;
        return (FT_SUCCESS);
    }
    length = ft_strlen("normal");
    if (ft_strncmp(value, "normal", length + 1) == 0)
    {
        options->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
        return (FT_SUCCESS);
    }
    length = ft_strlen("verbose");
    if (ft_strncmp(value, "verbose", length + 1) == 0)
    {
        options->diagnostic_level = TRANSPILE_DIAGNOSTIC_VERBOSE;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown diagnostics level '%s'. Expected 'silent', 'normal', or 'verbose'.\n", value);
    return (FT_FAILURE);
}

int transpiler_cli_options_init(t_transpiler_cli_options *options)
{
    if (!options)
        return (FT_FAILURE);
    options->input_path = NULL;
    options->output_path = NULL;
    options->output_directory = NULL;
    options->source_language = TRANSPILE_LANGUAGE_NONE;
    options->target_language = TRANSPILE_LANGUAGE_NONE;
    options->format_mode = TRANSPILE_FORMAT_DEFAULT;
    options->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
    options->show_help = 0;
    return (FT_SUCCESS);
}

static int transpiler_cli_parse_long_option(t_transpiler_cli_options *options, const char **argv, int argc, int *index)
{
    const char *argument;

    if (!options || !argv || !index)
        return (FT_FAILURE);
    argument = argv[*index];
    if (ft_strncmp(argument, "--help", 7) == 0 && ft_strlen(argument) == 6)
    {
        options->show_help = 1;
        return (FT_SUCCESS);
    }
    if (ft_strncmp(argument, "--direction", 12) == 0 && ft_strlen(argument) == 11)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --direction option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_direction_value(argv[*index], options));
    }
    if (ft_strncmp(argument, "--input", 8) == 0 && ft_strlen(argument) == 7)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --input option.\n");
            return (FT_FAILURE);
        }
        options->input_path = argv[*index];
        return (FT_SUCCESS);
    }
    if (ft_strncmp(argument, "--output", 9) == 0 && ft_strlen(argument) == 8)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --output option.\n");
            return (FT_FAILURE);
        }
        options->output_path = argv[*index];
        return (FT_SUCCESS);
    }
    if (ft_strncmp(argument, "--output-dir", 13) == 0 && ft_strlen(argument) == 12)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --output-dir option.\n");
            return (FT_FAILURE);
        }
        options->output_directory = argv[*index];
        return (FT_SUCCESS);
    }
    if (ft_strncmp(argument, "--format", 9) == 0 && ft_strlen(argument) == 8)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --format option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_format_value(argv[*index], options));
    }
    if (ft_strncmp(argument, "--diagnostics", 14) == 0 && ft_strlen(argument) == 13)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --diagnostics option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_diagnostics_value(argv[*index], options));
    }
    pf_printf("Unknown option '%s'.\n", argument);
    return (FT_FAILURE);
}

int transpiler_cli_parse(t_transpiler_cli_options *options, int argc, const char **argv)
{
    int index;

    if (!options || !argv)
        return (FT_FAILURE);
    if (transpiler_cli_options_init(options) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 1;
    while (index < argc)
    {
        if (transpiler_cli_parse_long_option(options, argv, argc, &index) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    if (options->show_help)
        return (FT_SUCCESS);
    if (transpiler_cli_apply_direction_from_env(options) != FT_SUCCESS)
        return (FT_FAILURE);
    if (options->source_language == TRANSPILE_LANGUAGE_NONE || options->target_language == TRANSPILE_LANGUAGE_NONE)
    {
        pf_printf("Unable to determine translation direction.\n");
        return (FT_FAILURE);
    }
    if (transpiler_cli_require_paths(options) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_cli_apply(const t_transpiler_cli_options *options, t_transpiler_context *context)
{
    if (!options || !context)
        return (FT_FAILURE);
    transpiler_context_set_languages(context, options->source_language, options->target_language);
    transpiler_context_set_io_paths(context, options->input_path, options->output_path);
    transpiler_context_set_output_directory(context, options->output_directory);
    transpiler_context_set_format_mode(context, options->format_mode);
    transpiler_context_set_diagnostic_level(context, options->diagnostic_level);
    return (FT_SUCCESS);
}

void transpiler_cli_print_usage(void)
{
    pf_printf("Usage: ctoc_cobol_transpiler --direction <dir> --input <path> --output <path>\n");
    pf_printf("       Direction: cblc-to-cobol | cobol-to-cblc\n");
    pf_printf("       Environment: CTOC_DEFAULT_DIRECTION can supply the direction.\n");
    pf_printf("       Optional: --output-dir <directory> to override emission path base.\n");
    pf_printf("                 --format <default|minimal|pretty> to control COBOL layout.\n");
    pf_printf("                 --diagnostics <silent|normal|verbose> to tune logging.\n");
}
