#include "cblc_transpiler.hpp"

#include <cstdlib>

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int transpiler_cli_options_reserve_inputs(t_transpiler_cli_options *options, size_t desired_capacity)
{
    const char **paths;

    if (!options)
        return (FT_FAILURE);
    if (options->input_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    paths = static_cast<const char **>(cma_calloc(desired_capacity, sizeof(const char *)));
    if (!paths)
        return (FT_FAILURE);
    if (options->input_paths)
    {
        ft_memcpy(paths, options->input_paths, options->input_count * sizeof(const char *));
        cma_free(options->input_paths);
    }
    options->input_paths = paths;
    options->input_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_cli_options_reserve_outputs(t_transpiler_cli_options *options, size_t desired_capacity)
{
    const char **paths;

    if (!options)
        return (FT_FAILURE);
    if (options->output_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    paths = static_cast<const char **>(cma_calloc(desired_capacity, sizeof(const char *)));
    if (!paths)
        return (FT_FAILURE);
    if (options->output_paths)
    {
        ft_memcpy(paths, options->output_paths, options->output_count * sizeof(const char *));
        cma_free(options->output_paths);
    }
    options->output_paths = paths;
    options->output_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static void transpiler_cli_warning_settings_enable_all(t_transpiler_warning_settings *settings)
{
    if (!settings)
        return ;
    settings->conversion = 1;
    settings->overflow = 1;
    settings->string_truncation = 1;
    settings->shadow = 1;
    settings->unused = 1;
}

static int transpiler_cli_options_append_input(t_transpiler_cli_options *options, const char *path)
{
    if (!options || !path)
        return (FT_FAILURE);
    if (options->input_capacity == 0)
    {
        if (transpiler_cli_options_reserve_inputs(options, 4) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (options->input_count >= options->input_capacity)
    {
        if (transpiler_cli_options_reserve_inputs(options, options->input_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    options->input_paths[options->input_count] = path;
    options->input_count += 1;
    return (FT_SUCCESS);
}

static int transpiler_cli_options_append_output(t_transpiler_cli_options *options, const char *path)
{
    if (!options || !path)
        return (FT_FAILURE);
    if (options->output_capacity == 0)
    {
        if (transpiler_cli_options_reserve_outputs(options, 4) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (options->output_count >= options->output_capacity)
    {
        if (transpiler_cli_options_reserve_outputs(options, options->output_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    options->output_paths[options->output_count] = path;
    options->output_count += 1;
    return (FT_SUCCESS);
}

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
        options->emit_standard_library = 0;
        return (FT_SUCCESS);
    }
    length = ft_strlen("cobol-to-cblc");
    if (ft_strncmp(value, "cobol-to-cblc", length + 1) == 0)
    {
        options->source_language = TRANSPILE_LANGUAGE_COBOL;
        options->target_language = TRANSPILE_LANGUAGE_CBL_C;
        options->emit_standard_library = 0;
        return (FT_SUCCESS);
    }
    length = ft_strlen("standard-library");
    if (ft_strncmp(value, "standard-library", length + 1) == 0)
    {
        options->emit_standard_library = 1;
        options->source_language = TRANSPILE_LANGUAGE_NONE;
        options->target_language = TRANSPILE_LANGUAGE_NONE;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown direction '%s'. Expected 'cblc-to-cobol', 'cobol-to-cblc', or 'standard-library'.\n", value);
    return (FT_FAILURE);
}

static int transpiler_cli_apply_direction_from_env(t_transpiler_cli_options *options)
{
    const char *value;

    if (!options)
        return (FT_FAILURE);
    if (options->emit_standard_library)
        return (FT_SUCCESS);
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
    if (options->emit_standard_library)
        return (FT_SUCCESS);
    if (options->input_count == 0)
    {
        pf_printf("Missing required --input option.\n");
        return (FT_FAILURE);
    }
    if (options->output_count == 0)
    {
        pf_printf("Missing required --output option.\n");
        return (FT_FAILURE);
    }
    if (options->input_count != options->output_count)
    {
        pf_printf("Input and output file counts must match (got %lu inputs and %lu outputs).\n",
            static_cast<unsigned long>(options->input_count),
            static_cast<unsigned long>(options->output_count));
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

static int transpiler_cli_parse_layout_value(const char *value, t_transpiler_cli_options *options)
{
    size_t length;

    if (!value || !options)
        return (FT_FAILURE);
    length = ft_strlen("normalize");
    if (ft_strncmp(value, "normalize", length + 1) == 0)
    {
        options->layout_mode = TRANSPILE_LAYOUT_NORMALIZE;
        return (FT_SUCCESS);
    }
    length = ft_strlen("preserve");
    if (ft_strncmp(value, "preserve", length + 1) == 0)
    {
        options->layout_mode = TRANSPILE_LAYOUT_PRESERVE;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown layout '%s'. Expected 'normalize' or 'preserve'.\n", value);
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

static int transpiler_cli_parse_warning_option(t_transpiler_cli_options *options, const char *argument)
{
    const char *name;
    size_t length;
    size_t name_length;
    int enable;

    if (!options || !argument)
        return (FT_FAILURE);
    if (ft_strlen(argument) < 3)
    {
        pf_printf("Unknown option '%s'.\n", argument);
        return (FT_FAILURE);
    }
    name = argument + 2;
    name_length = static_cast<size_t>(ft_strlen(name));
    if (name_length == 0)
    {
        pf_printf("Missing warning group for -W option.\n");
        return (FT_FAILURE);
    }
    length = ft_strlen("error");
    if (ft_strncmp(name, "error", length + 1) == 0)
    {
        options->warnings_as_errors = 1;
        return (FT_SUCCESS);
    }
    enable = 1;
    length = ft_strlen("no-");
    if (ft_strncmp(name, "no-", length) == 0 && name_length > length)
    {
        enable = 0;
        name += length;
        name_length = static_cast<size_t>(ft_strlen(name));
    }
    length = ft_strlen("conversion");
    if (ft_strncmp(name, "conversion", length + 1) == 0)
    {
        options->warning_settings.conversion = enable;
        return (FT_SUCCESS);
    }
    length = ft_strlen("overflow");
    if (ft_strncmp(name, "overflow", length + 1) == 0)
    {
        options->warning_settings.overflow = enable;
        return (FT_SUCCESS);
    }
    length = ft_strlen("string-trunc");
    if (ft_strncmp(name, "string-trunc", length + 1) == 0)
    {
        options->warning_settings.string_truncation = enable;
        return (FT_SUCCESS);
    }
    length = ft_strlen("shadow");
    if (ft_strncmp(name, "shadow", length + 1) == 0)
    {
        options->warning_settings.shadow = enable;
        return (FT_SUCCESS);
    }
    length = ft_strlen("unused");
    if (ft_strncmp(name, "unused", length + 1) == 0)
    {
        options->warning_settings.unused = enable;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown warning option '%s'.\n", argument);
    return (FT_FAILURE);
}

int transpiler_cli_options_init(t_transpiler_cli_options *options)
{
    if (!options)
        return (FT_FAILURE);
    options->input_paths = NULL;
    options->input_count = 0;
    options->input_capacity = 0;
    if (transpiler_cli_options_reserve_inputs(options, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    options->output_paths = NULL;
    options->output_count = 0;
    options->output_capacity = 0;
    if (transpiler_cli_options_reserve_outputs(options, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    options->output_directory = NULL;
    options->source_language = TRANSPILE_LANGUAGE_NONE;
    options->target_language = TRANSPILE_LANGUAGE_NONE;
    options->format_mode = TRANSPILE_FORMAT_DEFAULT;
    options->layout_mode = TRANSPILE_LAYOUT_NORMALIZE;
    options->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
    options->warnings_as_errors = 0;
    transpiler_cli_warning_settings_enable_all(&options->warning_settings);
    options->show_help = 0;
    options->emit_standard_library = 0;
    return (FT_SUCCESS);
}

void transpiler_cli_options_dispose(t_transpiler_cli_options *options)
{
    if (!options)
        return ;
    if (options->input_paths)
        cma_free(options->input_paths);
    options->input_paths = NULL;
    options->input_count = 0;
    options->input_capacity = 0;
    if (options->output_paths)
        cma_free(options->output_paths);
    options->output_paths = NULL;
    options->output_count = 0;
    options->output_capacity = 0;
    options->output_directory = NULL;
    options->source_language = TRANSPILE_LANGUAGE_NONE;
    options->target_language = TRANSPILE_LANGUAGE_NONE;
    options->format_mode = TRANSPILE_FORMAT_DEFAULT;
    options->layout_mode = TRANSPILE_LAYOUT_NORMALIZE;
    options->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
    options->warnings_as_errors = 0;
    transpiler_cli_warning_settings_enable_all(&options->warning_settings);
    options->show_help = 0;
    options->emit_standard_library = 0;
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
        if (transpiler_cli_options_append_input(options, argv[*index]) != FT_SUCCESS)
        {
            pf_printf("Unable to record input path.\n");
            return (FT_FAILURE);
        }
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
        if (transpiler_cli_options_append_output(options, argv[*index]) != FT_SUCCESS)
        {
            pf_printf("Unable to record output path.\n");
            return (FT_FAILURE);
        }
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
    if (ft_strncmp(argument, "--layout", 9) == 0 && ft_strlen(argument) == 8)
    {
        *index += 1;
        if (*index >= argc)
        {
            pf_printf("Missing value for --layout option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_layout_value(argv[*index], options));
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
    if (ft_strncmp(argument, "--warnings-as-errors", 21) == 0 && ft_strlen(argument) == 20)
    {
        options->warnings_as_errors = 1;
        return (FT_SUCCESS);
    }
    pf_printf("Unknown option '%s'.\n", argument);
    return (FT_FAILURE);
}

int transpiler_cli_parse(t_transpiler_cli_options *options, int argc, const char **argv)
{
    int index;
    const char *argument;

    if (!options || !argv)
        return (FT_FAILURE);
    if (transpiler_cli_options_init(options) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 1;
    while (index < argc)
    {
        argument = argv[index];
        if (ft_strncmp(argument, "--", 2) == 0 && ft_strlen(argument) > 2)
        {
            if (transpiler_cli_parse_long_option(options, argv, argc, &index) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (ft_strncmp(argument, "-W", 2) == 0 && ft_strlen(argument) > 2)
        {
            if (transpiler_cli_parse_warning_option(options, argument) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            pf_printf("Unknown option '%s'.\n", argument);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (options->show_help)
        return (FT_SUCCESS);
    if (transpiler_cli_apply_direction_from_env(options) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!options->emit_standard_library
        && (options->source_language == TRANSPILE_LANGUAGE_NONE
            || options->target_language == TRANSPILE_LANGUAGE_NONE))
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
    if (transpiler_context_set_io_paths(context, options->input_paths, options->input_count,
            options->output_paths, options->output_count) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_output_directory(context, options->output_directory);
    transpiler_context_set_emit_standard_library(context, options->emit_standard_library);
    transpiler_context_set_format_mode(context, options->format_mode);
    transpiler_context_set_layout_mode(context, options->layout_mode);
    transpiler_context_set_diagnostic_level(context, options->diagnostic_level);
    transpiler_context_set_warnings_as_errors(context, options->warnings_as_errors);
    transpiler_context_set_warning_settings(context, &options->warning_settings);
    return (FT_SUCCESS);
}

void transpiler_cli_print_usage(void)
{
    pf_printf("Usage: ctoc_cobol_transpiler --direction <dir> --input <path> [--input <path> ...]\n");
    pf_printf("       --output <path> [--output <path> ...]\n");
    pf_printf("       Direction: cblc-to-cobol | cobol-to-cblc | standard-library\n");
    pf_printf("       Environment: CTOC_DEFAULT_DIRECTION can supply the direction.\n");
    pf_printf("       Standard-library builds emit all cataloged programs to the selected directory.\n");
    pf_printf("       Optional: --output-dir <directory> to override emission path base.\n");
    pf_printf("                 --format <default|minimal|pretty> to control COBOL layout.\n");
    pf_printf("                 --layout <normalize|preserve> to control regenerated CBL-C layout.\n");
    pf_printf("                 --diagnostics <silent|normal|verbose> to tune logging.\n");
    pf_printf("                 --warnings-as-errors to treat warnings as build errors.\n");
    pf_printf("                 -Werror to escalate warnings.\n");
    pf_printf("                 -Wconversion / -Wno-conversion to toggle conversion warnings.\n");
    pf_printf("                 -Woverflow / -Wno-overflow to toggle overflow warnings.\n");
    pf_printf("                 -Wstring-trunc / -Wno-string-trunc to toggle truncation warnings.\n");
    pf_printf("                 -Wshadow / -Wno-shadow to toggle shadowing warnings.\n");
    pf_printf("                 -Wunused / -Wno-unused to toggle unused-code warnings.\n");
}
