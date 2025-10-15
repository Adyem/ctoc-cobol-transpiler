#include <climits>
#include <filesystem>
#include <system_error>

#include "cblc_formatter.hpp"
#include "parser.hpp"
#include "runtime_file.hpp"
#include "transpiler_cli.hpp"
#include "transpiler_cblc.hpp"
#include "transpiler_cobol_reverse.hpp"
#include "transpiler_logging.hpp"
#include "transpiler_pipeline.hpp"
#include "transpiler_semantics.hpp"
#include "transpiler_validation.hpp"
#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int pipeline_emit_error(t_transpiler_context *context, const char *message)
{
    if (!context || !message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, FT_FAILURE, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int pipeline_read_file(const char *path, char **out_text)
{
    t_runtime_file file;
    char stack_buffer[1024];
    char *buffer;
    size_t capacity;
    size_t length;
    size_t bytes_read;
    int status;

    if (!path || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    runtime_file_init(&file);
    if (runtime_file_open_read(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    capacity = 1024;
    buffer = static_cast<char *>(cma_calloc(capacity, sizeof(char)));
    if (!buffer)
    {
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    length = 0;
    status = FT_FAILURE;
    while (1)
    {
        if (runtime_file_read(&file, stack_buffer, sizeof(stack_buffer), &bytes_read) != FT_SUCCESS)
            break ;
        if (bytes_read == 0)
        {
            status = FT_SUCCESS;
            break ;
        }
        while (length + bytes_read + 1 > capacity)
        {
            size_t new_capacity;
            char *new_buffer;

            if (capacity >= SIZE_MAX / 2)
                goto cleanup;
            new_capacity = capacity * 2;
            new_buffer = static_cast<char *>(cma_calloc(new_capacity, sizeof(char)));
            if (!new_buffer)
                goto cleanup;
            if (length > 0)
                ft_memcpy(new_buffer, buffer, length);
            cma_free(buffer);
            buffer = new_buffer;
            capacity = new_capacity;
        }
        if (bytes_read > 0)
        {
            ft_memcpy(buffer + length, stack_buffer, bytes_read);
            length += bytes_read;
            buffer[length] = '\0';
        }
    }
    if (status == FT_SUCCESS)
    {
        *out_text = buffer;
        buffer = NULL;
    }
cleanup:
    runtime_file_close(&file);
    if (buffer)
        cma_free(buffer);
    return (status);
}

static int pipeline_prepare_output_directory(const char *path)
{
    std::error_code error;
    std::filesystem::path output_path;
    std::filesystem::path parent_directory;

    if (!path)
        return (FT_FAILURE);
    output_path = std::filesystem::path(path);
    parent_directory = output_path.parent_path();
    if (parent_directory.empty())
        return (FT_SUCCESS);
    std::filesystem::create_directories(parent_directory, error);
    if (error)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_write_file(const char *path, const char *text)
{
    t_runtime_file file;
    size_t length;

    if (!path || !text)
        return (FT_FAILURE);
    if (pipeline_prepare_output_directory(path) != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_file_init(&file);
    if (runtime_file_open_write(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    length = ft_strlen(text);
    if (runtime_file_write(&file, text, length) != FT_SUCCESS)
    {
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    if (runtime_file_close(&file) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_resolve_output_path(const t_transpiler_context *context, const char *target_path,
    char *buffer, size_t buffer_size)
{
    const char *directory;
    const char *filename;
    const char *separator;
    size_t length;

    if (!context || !target_path || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    directory = context->output_directory;
    if (directory && directory[0] != '\0')
    {
        filename = target_path;
        separator = ft_strrchr(target_path, '/');
        if (!separator)
            separator = ft_strrchr(target_path, '\\');
        if (separator && separator[1] != '\0')
            filename = separator + 1;
        if (pf_snprintf(buffer, buffer_size, "%s/%s", directory, filename) < 0)
            return (FT_FAILURE);
        length = ft_strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    length = ft_strlen(target_path);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    ft_strlcpy(buffer, target_path, buffer_size);
    return (FT_SUCCESS);
}

static int pipeline_format_cblc(const char *input, t_transpiler_format_mode mode, char **out_text)
{
    if (!input || !out_text)
        return (FT_FAILURE);
    return (cblc_formatter_format(input, mode, out_text));
}

static int pipeline_convert_cobol_to_cblc(t_transpiler_context *context, const char *input_path, const char *output_path)
{
    char resolved_path[TRANSPILE_FILE_PATH_MAX];
    char *source_text;
    char *cblc_text;
    char *formatted_text;
    t_parser parser;
    t_ast_node *program;
    int status;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context || !input_path || !output_path)
        return (FT_FAILURE);
    source_text = NULL;
    cblc_text = NULL;
    formatted_text = NULL;
    program = NULL;
    status = FT_FAILURE;
    if (pipeline_read_file(input_path, &source_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    parser_init(&parser, source_text);
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        if (pf_snprintf(message, sizeof(message), "Failed to parse COBOL source '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    parser_dispose(&parser);
    transpiler_context_reset_unit_state(context);
    if (transpiler_semantics_analyze_program(context, program) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Semantic analysis failed for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (transpiler_cobol_program_to_cblc(context, program, &cblc_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to generate CBL-C for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_format_cblc(cblc_text, context->format_mode, &formatted_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Failed to format generated CBL-C for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (transpiler_validate_generated_cblc(formatted_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Generated CBL-C failed validation for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_resolve_output_path(context, output_path, resolved_path, sizeof(resolved_path)) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to resolve output path for '%s'", output_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_write_file(resolved_path, formatted_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'", resolved_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (program)
        ast_node_destroy(program);
    if (formatted_text)
        cma_free(formatted_text);
    if (cblc_text)
        cma_free(cblc_text);
    if (source_text)
        cma_free(source_text);
    return (status);
}

static int pipeline_stage_cobol_to_cblc(t_transpiler_context *context, void *user_data)
{
    size_t index;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    index = 0;
    while (index < context->source_count)
    {
        if (pipeline_convert_cobol_to_cblc(context, context->source_paths[index], context->target_paths[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int pipeline_stage_cblc_to_cobol(t_transpiler_context *context, void *user_data)
{
    size_t index;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    index = 0;
    while (index < context->source_count)
    {
        char resolved_path[TRANSPILE_FILE_PATH_MAX];
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        char *source_text;
        char *cobol_text;
        t_cblc_translation_unit unit;

        source_text = NULL;
        cobol_text = NULL;
        cblc_translation_unit_init(&unit);
        if (pipeline_read_file(context->source_paths[index], &source_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        if (cblc_parse_translation_unit(source_text, &unit) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to parse CBL-C source '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (!unit.saw_return)
        {
            if (pf_snprintf(message, sizeof(message),
                    "CBL-C source '%s' is missing a terminating return;", context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (cblc_generate_cobol(&unit, &cobol_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to generate COBOL for '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (transpiler_validate_generated_cobol(cobol_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Generated COBOL failed validation for '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            if (cobol_text)
                cma_free(cobol_text);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (pipeline_resolve_output_path(context, context->target_paths[index], resolved_path,
                sizeof(resolved_path)) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to resolve output path for '%s'",
                    context->target_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            if (cobol_text)
                cma_free(cobol_text);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (pipeline_write_file(resolved_path, cobol_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'",
                    resolved_path) >= 0)
                (void)pipeline_emit_error(context, message);
            if (cobol_text)
                cma_free(cobol_text);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (cobol_text)
            cma_free(cobol_text);
        cblc_translation_unit_dispose(&unit);
        if (source_text)
            cma_free(source_text);
        index += 1;
    }
    return (FT_SUCCESS);
}

int main(int argc, const char **argv)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    t_transpiler_cli_options options;
    int status;

    if (transpiler_cli_parse(&options, argc, argv) != FT_SUCCESS)
    {
        transpiler_cli_print_usage();
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (options.show_help)
    {
        transpiler_cli_print_usage();
        transpiler_cli_options_dispose(&options);
        return (0);
    }
    if (transpiler_pipeline_init(&pipeline) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    status = FT_FAILURE;
    if (context.source_language == TRANSPILE_LANGUAGE_COBOL
        && context.target_language == TRANSPILE_LANGUAGE_CBL_C)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "cobol-to-cblc",
                pipeline_stage_cobol_to_cblc, NULL) != FT_SUCCESS)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
    }
    else if (context.source_language == TRANSPILE_LANGUAGE_CBL_C
        && context.target_language == TRANSPILE_LANGUAGE_COBOL)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "cblc-to-cobol",
                pipeline_stage_cblc_to_cobol, NULL) != FT_SUCCESS)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
    }
    else
    {
        (void)pipeline_emit_error(&context, "Unsupported translation direction");
        status = FT_FAILURE;
        goto cleanup;
    }
    status = transpiler_pipeline_execute(&pipeline, &context);
cleanup:
    transpiler_logging_flush(&context);
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    transpiler_cli_options_dispose(&options);
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
