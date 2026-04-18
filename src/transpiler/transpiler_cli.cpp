#include "cblc_transpiler.hpp"

#include <cstdlib>

#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

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
        std::memcpy(paths, options->input_paths, options->input_count * sizeof(const char *));
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
        std::memcpy(paths, options->output_paths, options->output_count * sizeof(const char *));
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
    length = std::strlen("cblc-to-cobol");
    if (std::strncmp(value, "cblc-to-cobol", length + 1) == 0)
    {
        options->source_language = TRANSPILE_LANGUAGE_CBL_C;
        options->target_language = TRANSPILE_LANGUAGE_COBOL;
        options->emit_standard_library = 0;
        return (FT_SUCCESS);
    }
    length = std::strlen("cblc-to-c");
    if (std::strncmp(value, "cblc-to-c", length + 1) == 0)
    {
        options->source_language = TRANSPILE_LANGUAGE_CBL_C;
        options->target_language = TRANSPILE_LANGUAGE_C;
        options->emit_standard_library = 0;
        return (FT_SUCCESS);
    }
    length = std::strlen("cobol-to-cblc");
    if (std::strncmp(value, "cobol-to-cblc", length + 1) == 0)
    {
        options->source_language = TRANSPILE_LANGUAGE_COBOL;
        options->target_language = TRANSPILE_LANGUAGE_CBL_C;
        options->emit_standard_library = 0;
        return (FT_SUCCESS);
    }
    length = std::strlen("standard-library");
    if (std::strncmp(value, "standard-library", length + 1) == 0)
    {
        options->emit_standard_library = 1;
        options->source_language = TRANSPILE_LANGUAGE_NONE;
        options->target_language = TRANSPILE_LANGUAGE_NONE;
        return (FT_SUCCESS);
    }
    std::printf("Unknown direction '%s'. Expected 'cblc-to-cobol', 'cblc-to-c', 'cobol-to-cblc', or 'standard-library'.\n", value);
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
        std::printf("Missing required --direction option or CTOC_DEFAULT_DIRECTION environment variable.\n");
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
        std::printf("Missing required --input option.\n");
        return (FT_FAILURE);
    }
    if (options->output_count == 0)
    {
        std::printf("Missing required --output option.\n");
        return (FT_FAILURE);
    }
    if (options->input_count != options->output_count)
    {
        std::printf("Input and output file counts must match (got %lu inputs and %lu outputs).\n",
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
    length = std::strlen("default");
    if (std::strncmp(value, "default", length + 1) == 0)
    {
        options->format_mode = TRANSPILE_FORMAT_DEFAULT;
        return (FT_SUCCESS);
    }
    length = std::strlen("minimal");
    if (std::strncmp(value, "minimal", length + 1) == 0)
    {
        options->format_mode = TRANSPILE_FORMAT_MINIMAL;
        return (FT_SUCCESS);
    }
    length = std::strlen("pretty");
    if (std::strncmp(value, "pretty", length + 1) == 0)
    {
        options->format_mode = TRANSPILE_FORMAT_PRETTY;
        return (FT_SUCCESS);
    }
    std::printf("Unknown format '%s'. Expected 'default', 'minimal', or 'pretty'.\n", value);
    return (FT_FAILURE);
}

static int transpiler_cli_parse_layout_value(const char *value, t_transpiler_cli_options *options)
{
    size_t length;

    if (!value || !options)
        return (FT_FAILURE);
    length = std::strlen("normalize");
    if (std::strncmp(value, "normalize", length + 1) == 0)
    {
        options->layout_mode = TRANSPILE_LAYOUT_NORMALIZE;
        return (FT_SUCCESS);
    }
    length = std::strlen("preserve");
    if (std::strncmp(value, "preserve", length + 1) == 0)
    {
        options->layout_mode = TRANSPILE_LAYOUT_PRESERVE;
        return (FT_SUCCESS);
    }
    std::printf("Unknown layout '%s'. Expected 'normalize' or 'preserve'.\n", value);
    return (FT_FAILURE);
}

static int transpiler_cli_parse_diagnostics_value(const char *value, t_transpiler_cli_options *options)
{
    size_t length;

    if (!value || !options)
        return (FT_FAILURE);
    length = std::strlen("silent");
    if (std::strncmp(value, "silent", length + 1) == 0)
    {
        options->diagnostic_level = TRANSPILE_DIAGNOSTIC_SILENT;
        return (FT_SUCCESS);
    }
    length = std::strlen("normal");
    if (std::strncmp(value, "normal", length + 1) == 0)
    {
        options->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
        return (FT_SUCCESS);
    }
    length = std::strlen("verbose");
    if (std::strncmp(value, "verbose", length + 1) == 0)
    {
        options->diagnostic_level = TRANSPILE_DIAGNOSTIC_VERBOSE;
        return (FT_SUCCESS);
    }
    std::printf("Unknown diagnostics level '%s'. Expected 'silent', 'normal', or 'verbose'.\n", value);
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
    if (std::strlen(argument) < 3)
    {
        std::printf("Unknown option '%s'.\n", argument);
        return (FT_FAILURE);
    }
    name = argument + 2;
    name_length = std::strlen(name);
    if (name_length == 0)
    {
        std::printf("Missing warning group for -W option.\n");
        return (FT_FAILURE);
    }
    length = std::strlen("error");
    if (std::strncmp(name, "error", length + 1) == 0)
    {
        options->warnings_as_errors = 1;
        return (FT_SUCCESS);
    }
    enable = 1;
    length = std::strlen("no-");
    if (std::strncmp(name, "no-", length) == 0 && name_length > length)
    {
        enable = 0;
        name += length;
        name_length = std::strlen(name);
    }
    length = std::strlen("conversion");
    if (std::strncmp(name, "conversion", length + 1) == 0)
    {
        options->warning_settings.conversion = enable;
        return (FT_SUCCESS);
    }
    length = std::strlen("overflow");
    if (std::strncmp(name, "overflow", length + 1) == 0)
    {
        options->warning_settings.overflow = enable;
        return (FT_SUCCESS);
    }
    length = std::strlen("string-trunc");
    if (std::strncmp(name, "string-trunc", length + 1) == 0)
    {
        options->warning_settings.string_truncation = enable;
        return (FT_SUCCESS);
    }
    length = std::strlen("shadow");
    if (std::strncmp(name, "shadow", length + 1) == 0)
    {
        options->warning_settings.shadow = enable;
        return (FT_SUCCESS);
    }
    length = std::strlen("unused");
    if (std::strncmp(name, "unused", length + 1) == 0)
    {
        options->warning_settings.unused = enable;
        return (FT_SUCCESS);
    }
    std::printf("Unknown warning option '%s'.\n", argument);
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
    options->dump_ast = 0;
    options->dump_ast_directory = NULL;
    options->dump_copybook_graph = 0;
    options->dump_copybook_graph_directory = NULL;
    options->dump_semantic_ir = 0;
    options->dump_semantic_ir_directory = NULL;
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
    options->dump_ast = 0;
    options->dump_ast_directory = NULL;
    options->dump_copybook_graph = 0;
    options->dump_copybook_graph_directory = NULL;
    options->dump_semantic_ir = 0;
    options->dump_semantic_ir_directory = NULL;
}

static int transpiler_cli_parse_long_option(t_transpiler_cli_options *options, const char **argv, int argc, int *index)
{
    const char *argument;

    if (!options || !argv || !index)
        return (FT_FAILURE);
    argument = argv[*index];
    if (std::strncmp(argument, "--help", 7) == 0 && std::strlen(argument) == 6)
    {
        options->show_help = 1;
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--direction", 12) == 0 && std::strlen(argument) == 11)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --direction option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_direction_value(argv[*index], options));
    }
    if (std::strncmp(argument, "--input", 8) == 0 && std::strlen(argument) == 7)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --input option.\n");
            return (FT_FAILURE);
        }
        if (transpiler_cli_options_append_input(options, argv[*index]) != FT_SUCCESS)
        {
            std::printf("Unable to record input path.\n");
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--output", 9) == 0 && std::strlen(argument) == 8)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --output option.\n");
            return (FT_FAILURE);
        }
        if (transpiler_cli_options_append_output(options, argv[*index]) != FT_SUCCESS)
        {
            std::printf("Unable to record output path.\n");
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--output-dir", 13) == 0 && std::strlen(argument) == 12)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --output-dir option.\n");
            return (FT_FAILURE);
        }
        options->output_directory = argv[*index];
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--format", 9) == 0 && std::strlen(argument) == 8)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --format option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_format_value(argv[*index], options));
    }
    if (std::strncmp(argument, "--layout", 9) == 0 && std::strlen(argument) == 8)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --layout option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_layout_value(argv[*index], options));
    }
    if (std::strncmp(argument, "--diagnostics", 14) == 0 && std::strlen(argument) == 13)
    {
        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --diagnostics option.\n");
            return (FT_FAILURE);
        }
        return (transpiler_cli_parse_diagnostics_value(argv[*index], options));
    }
    if (std::strncmp(argument, "--warnings-as-errors", 21) == 0 && std::strlen(argument) == 20)
    {
        options->warnings_as_errors = 1;
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--dump-ast", 11) == 0 && std::strlen(argument) == 10)
    {
        size_t auto_length;

        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --dump-ast option.\n");
            return (FT_FAILURE);
        }
        options->dump_ast = 1;
        auto_length = std::strlen("auto");
        if (std::strncmp(argv[*index], "auto", auto_length + 1) == 0)
            options->dump_ast_directory = NULL;
        else
            options->dump_ast_directory = argv[*index];
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--dump-copybook-graph", 22) == 0 && std::strlen(argument) == 21)
    {
        size_t auto_length;

        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --dump-copybook-graph option.\n");
            return (FT_FAILURE);
        }
        options->dump_copybook_graph = 1;
        auto_length = std::strlen("auto");
        if (std::strncmp(argv[*index], "auto", auto_length + 1) == 0)
            options->dump_copybook_graph_directory = NULL;
        else
            options->dump_copybook_graph_directory = argv[*index];
        return (FT_SUCCESS);
    }
    if (std::strncmp(argument, "--dump-semantic-ir", 20) == 0 && std::strlen(argument) == 18)
    {
        size_t auto_length;

        *index += 1;
        if (*index >= argc)
        {
            std::printf("Missing value for --dump-semantic-ir option.\n");
            return (FT_FAILURE);
        }
        options->dump_semantic_ir = 1;
        auto_length = std::strlen("auto");
        if (std::strncmp(argv[*index], "auto", auto_length + 1) == 0)
            options->dump_semantic_ir_directory = NULL;
        else
            options->dump_semantic_ir_directory = argv[*index];
        return (FT_SUCCESS);
    }
    std::printf("Unknown option '%s'.\n", argument);
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
        if (std::strncmp(argument, "--", 2) == 0 && std::strlen(argument) > 2)
        {
            if (transpiler_cli_parse_long_option(options, argv, argc, &index) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (std::strncmp(argument, "-W", 2) == 0 && std::strlen(argument) > 2)
        {
            if (transpiler_cli_parse_warning_option(options, argument) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            std::printf("Unknown option '%s'.\n", argument);
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
        std::printf("Unable to determine translation direction.\n");
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
    transpiler_context_set_ast_dump_enabled(context, options->dump_ast);
    transpiler_context_set_ast_dump_directory(context, options->dump_ast_directory);
    transpiler_context_set_copybook_graph_enabled(context, options->dump_copybook_graph);
    transpiler_context_set_copybook_graph_directory(context, options->dump_copybook_graph_directory);
    transpiler_context_set_semantic_diff_enabled(context, options->dump_semantic_ir);
    transpiler_context_set_semantic_diff_directory(context, options->dump_semantic_ir_directory);
    transpiler_context_set_format_mode(context, options->format_mode);
    transpiler_context_set_layout_mode(context, options->layout_mode);
    transpiler_context_set_diagnostic_level(context, options->diagnostic_level);
    transpiler_context_set_warnings_as_errors(context, options->warnings_as_errors);
    transpiler_context_set_warning_settings(context, &options->warning_settings);
    return (FT_SUCCESS);
}

void transpiler_cli_print_usage(void)
{
    std::printf("Usage: ctoc_cobol_transpiler --direction <dir> --input <path> [--input <path> ...]\n");
    std::printf("       --output <path> [--output <path> ...]\n");
    std::printf("       Direction: cblc-to-cobol | cblc-to-c | cobol-to-cblc | standard-library\n");
    std::printf("       Environment: CTOC_DEFAULT_DIRECTION can supply the direction.\n");
    std::printf("       Standard-library builds emit all cataloged programs to the selected directory.\n");
    std::printf("       Optional: --output-dir <directory> to override emission path base.\n");
    std::printf("                 --format <default|minimal|pretty> to control COBOL layout.\n");
    std::printf("                 --layout <normalize|preserve> to control regenerated CBL-C layout.\n");
    std::printf("                 --diagnostics <silent|normal|verbose> to tune logging.\n");
    std::printf("                 --dump-ast <auto|directory> to emit Graphviz AST visualizations.\n");
    std::printf("                 --dump-copybook-graph <auto|directory> to emit copybook dependency graphs.\n");
    std::printf("                 --dump-semantic-ir <auto|directory> to record pre/post-normalization semantic IR.\n");
    std::printf("                 --warnings-as-errors to treat warnings as build errors.\n");
    std::printf("                 -Werror to escalate warnings.\n");
    std::printf("                 -Wconversion / -Wno-conversion to toggle conversion warnings.\n");
    std::printf("                 -Woverflow / -Wno-overflow to toggle overflow warnings.\n");
    std::printf("                 -Wstring-trunc / -Wno-string-trunc to toggle truncation warnings.\n");
    std::printf("                 -Wshadow / -Wno-shadow to toggle shadowing warnings.\n");
    std::printf("                 -Wunused / -Wno-unused to toggle unused-code warnings.\n");
}
