#include <climits>
#include <filesystem>
#include <system_error>

#include "cblc_transpiler.hpp"
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

static int pipeline_build_ast_output_path(const t_transpiler_context *context, const char *input_path,
    const char *resolved_output_path, char *buffer, size_t buffer_size)
{
    const char *directory;
    const char *filename;
    const char *cursor;
    size_t length;

    if (!context || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    directory = transpiler_context_get_ast_dump_directory(context);
    if (directory && directory[0] != '\0')
    {
        char base[TRANSPILE_FILE_PATH_MAX];

        filename = input_path;
        if (input_path)
        {
            cursor = input_path;
            while (*cursor != '\0')
            {
                if (*cursor == '/' || *cursor == '\\')
                    filename = cursor + 1;
                cursor += 1;
            }
        }
        if (!filename || filename[0] == '\0')
            filename = "program";
        ft_strlcpy(base, filename, sizeof(base));
        length = ft_strlen(base);
        while (length > 0)
        {
            if (base[length - 1] == '.')
            {
                base[length - 1] = '\0';
                break ;
            }
            length -= 1;
        }
        if (base[0] == '\0')
            ft_strlcpy(base, "program", sizeof(base));
        if (pf_snprintf(buffer, buffer_size, "%s/%s.dot", directory, base) < 0)
            return (FT_FAILURE);
        length = ft_strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!resolved_output_path)
        return (FT_FAILURE);
    length = ft_strlen(resolved_output_path);
    if (length + 5 > buffer_size)
        return (FT_FAILURE);
    ft_strlcpy(buffer, resolved_output_path, buffer_size);
    while (length > 0)
    {
        if (buffer[length - 1] == '.')
        {
            buffer[length - 1] = '\0';
            break ;
        }
        if (buffer[length - 1] == '/' || buffer[length - 1] == '\\')
            break ;
        length -= 1;
    }
    if (ft_strlcat(buffer, ".dot", buffer_size) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_apply_cblc_layout(const char *input, t_transpiler_layout_mode layout_mode,
    t_transpiler_format_mode format_mode, char **out_text)
{
    if (!input || !out_text)
        return (FT_FAILURE);
    return (transpiler_cblc_apply_layout(input, layout_mode, format_mode, out_text));
}

static int pipeline_convert_cobol_to_cblc(t_transpiler_context *context, const char *input_path, const char *output_path)
{
    char resolved_path[TRANSPILE_FILE_PATH_MAX];
    char ast_path[TRANSPILE_FILE_PATH_MAX];
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
    ast_path[0] = '\0';
    if (pipeline_read_file(input_path, &source_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    context->source_path = input_path;
    context->target_path = output_path;
    context->active_source_text = source_text;
    context->active_source_length = ft_strlen(source_text);
    transpiler_context_clear_comments(context);
    parser_init_with_context(&parser, source_text, context);
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        if (pf_snprintf(message, sizeof(message), "Failed to parse COBOL source '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    parser_dispose(&parser);
    transpiler_context_reset_unit_state(context);
    context->active_source_text = source_text;
    context->active_source_length = ft_strlen(source_text);
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
    if (pipeline_apply_cblc_layout(cblc_text, context->layout_mode, context->format_mode,
            &formatted_text) != FT_SUCCESS)
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
    if (transpiler_context_get_ast_dump_enabled(context))
    {
        if (pipeline_build_ast_output_path(context, input_path, resolved_path, ast_path,
                sizeof(ast_path)) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Unable to select AST visualization path for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
        if (pipeline_prepare_output_directory(ast_path) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Unable to prepare AST visualization path '%s'", ast_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
        if (transpiler_ast_visualize_program(program, ast_path) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Unable to emit AST visualization for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
    }
    if (pipeline_write_file(resolved_path, formatted_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'", resolved_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (context)
    {
        context->active_source_text = NULL;
        context->active_source_length = 0;
    }
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

static int pipeline_stage_emit_standard_library(t_transpiler_context *context, void *user_data)
{
    const t_transpiler_standard_library_entry *entries;
    size_t entry_count;
    size_t index;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    entries = transpiler_standard_library_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        char filename[TRANSPILE_FILE_PATH_MAX];
        char resolved_path[TRANSPILE_FILE_PATH_MAX];
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        char *program_text;
        int status;

        if (pf_snprintf(filename, sizeof(filename), "%s.cob", entries[index].program_name) < 0)
            return (FT_FAILURE);
        program_text = NULL;
        status = entries[index].generator(&program_text);
        if (status != FT_SUCCESS || !program_text)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Unable to generate standard library program '%s'", entries[index].program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            if (program_text)
                cma_free(program_text);
            return (FT_FAILURE);
        }
        const char *skip_validation_env;

        skip_validation_env = std::getenv("CTOC_SKIP_STANDARD_LIBRARY_VALIDATION");
        if ((!skip_validation_env || skip_validation_env[0] == '\0'
                || skip_validation_env[0] == '0')
            && transpiler_validate_generated_cobol(program_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Generated standard library program '%s' failed validation",
                    entries[index].program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (pipeline_resolve_output_path(context, filename, resolved_path, sizeof(resolved_path)) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Unable to resolve output path for standard library program '%s'",
                    entries[index].program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (pipeline_write_file(resolved_path, program_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Unable to write standard library program '%s' to '%s'",
                    entries[index].program_name, resolved_path) >= 0)
                (void)pipeline_emit_error(context, message);
            cma_free(program_text);
            return (FT_FAILURE);
        }
        cma_free(program_text);
        index += 1;
    }
    return (FT_SUCCESS);
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

static void pipeline_extract_module_name(const char *path, char *buffer, size_t buffer_size)
{
    const char *cursor;

    if (!buffer || buffer_size == 0)
        return ;
    buffer[0] = '\0';
    if (!path)
        return ;
    cursor = path;
    while (*cursor != '\0')
    {
        if (*cursor == '/' || *cursor == '\\')
            path = cursor + 1;
        cursor += 1;
    }
    if (!*path)
        return ;
    ft_strlcpy(buffer, path, buffer_size);
}

static void pipeline_choose_module_name(const char *path, const t_cblc_translation_unit *unit,
    char *buffer, size_t buffer_size)
{
    if (!buffer || buffer_size == 0)
        return ;
    pipeline_extract_module_name(path, buffer, buffer_size);
    if (buffer[0] == '\0' && unit && unit->program_name[0] != '\0')
        ft_strlcpy(buffer, unit->program_name, buffer_size);
    if (buffer[0] == '\0')
        ft_strlcpy(buffer, "MODULE", buffer_size);
}

static int pipeline_stage_cblc_to_c(t_transpiler_context *context, void *user_data)
{
    t_cblc_translation_unit *units;
    char **sources;
    size_t *module_indices;
    char (*module_names)[TRANSPILE_MODULE_NAME_MAX];
    const size_t *order;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    const t_cblc_translation_unit **ordered_units;
    const char **ordered_source_paths;
    size_t *ordered_source_indices;
    size_t order_count;
    size_t file_count;
    size_t index;
    int status;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    file_count = context->source_count;
    if (file_count == 0)
        return (FT_SUCCESS);
    transpiler_context_reset_module_registry(context);
    transpiler_context_reset_unit_state(context);
    units = static_cast<t_cblc_translation_unit *>(cma_calloc(file_count,
        sizeof(t_cblc_translation_unit)));
    sources = static_cast<char **>(cma_calloc(file_count, sizeof(char *)));
    module_indices = static_cast<size_t *>(cma_calloc(file_count, sizeof(size_t)));
    module_names = static_cast<char (*)[TRANSPILE_MODULE_NAME_MAX]>(cma_calloc(file_count,
        sizeof(*module_names)));
    if (!units || !sources || !module_indices || !module_names)
    {
        (void)pipeline_emit_error(context, "Unable to allocate module tracking for CBL-C inputs");
        status = FT_FAILURE;
        goto cleanup;
    }
    ordered_units = NULL;
    ordered_source_paths = NULL;
    ordered_source_indices = NULL;
    order_count = 0;
    index = 0;
    while (index < file_count)
    {
        t_cblc_translation_unit *unit;
        char module_name[TRANSPILE_MODULE_NAME_MAX];
        size_t import_index;

        module_indices[index] = static_cast<size_t>(-1);
        unit = &units[index];
        cblc_translation_unit_init(unit);
        if (pipeline_read_file(context->source_paths[index], &sources[index]) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (cblc_parse_translation_unit(sources[index], unit) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to parse CBL-C source '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (unit->function_count == 0)
        {
            if (pf_snprintf(message, sizeof(message),
                    "CBL-C source '%s' does not declare any functions;", context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        else
        {
            size_t entry_index;

            entry_index = unit->entry_function_index;
            if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
                entry_index = 0;
            if (!unit->functions[entry_index].saw_return)
            {
                if (pf_snprintf(message, sizeof(message),
                        "CBL-C source '%s' is missing a terminating return;", context->source_paths[index]) >= 0)
                    (void)pipeline_emit_error(context, message);
                status = FT_FAILURE;
                goto cleanup;
            }
        }
        pipeline_choose_module_name(context->source_paths[index], unit, module_name,
            sizeof(module_name));
        ft_strlcpy(module_names[index], module_name, TRANSPILE_MODULE_NAME_MAX);
        if (transpiler_context_register_module(context, module_name, context->source_paths[index])
            != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to register module for '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (context->module_count == 0)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        module_indices[index] = context->module_count - 1;
        import_index = 0;
        while (import_index < unit->import_count)
        {
            if (transpiler_context_register_module_import(context, module_name,
                    unit->imports[import_index].path) != FT_SUCCESS)
            {
                if (pf_snprintf(message, sizeof(message),
                        "Failed to register import '%s' for module '%s'", unit->imports[import_index].path,
                        module_name) >= 0)
                    (void)pipeline_emit_error(context, message);
                status = FT_FAILURE;
                goto cleanup;
            }
            import_index += 1;
        }
        if (cblc_register_translation_unit_exports(context, module_name, unit) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Failed to register exports for module '%s'", module_name) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    index = 0;
    while (index < file_count)
    {
        if (cblc_resolve_translation_unit_calls(context, module_names[index], &units[index]) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Failed to resolve function calls for module '%s'", module_names[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    if (transpiler_context_compute_module_initialization_order(context) != FT_SUCCESS)
    {
        (void)pipeline_emit_error(context, "Unable to compute module initialization order");
        status = FT_FAILURE;
        goto cleanup;
    }
    order = transpiler_context_get_module_initialization_order(context, &order_count);
    if (!order || order_count == 0)
    {
        status = FT_FAILURE;
        goto cleanup;
    }
    ordered_units = static_cast<const t_cblc_translation_unit **>(cma_calloc(order_count,
        sizeof(*ordered_units)));
    ordered_source_paths = static_cast<const char **>(cma_calloc(order_count,
        sizeof(*ordered_source_paths)));
    ordered_source_indices = static_cast<size_t *>(cma_calloc(order_count,
        sizeof(*ordered_source_indices)));
    if (!ordered_units || !ordered_source_paths || !ordered_source_indices)
    {
        (void)pipeline_emit_error(context, "Unable to allocate generation buffers");
        status = FT_FAILURE;
        goto cleanup;
    }
    index = 0;
    while (index < order_count)
    {
        size_t module_index;
        size_t source_index;

        module_index = order[index];
        source_index = 0;
        while (source_index < file_count)
        {
            if (module_indices[source_index] == module_index)
                break ;
            source_index += 1;
        }
        if (source_index >= file_count)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        ordered_units[index] = &units[source_index];
        ordered_source_paths[index] = context->source_paths[source_index];
        ordered_source_indices[index] = source_index;
        index += 1;
    }
    index = 0;
    while (index < order_count)
    {
        size_t source_index;
        char resolved_path[TRANSPILE_FILE_PATH_MAX];
        char *generated_text;

        source_index = ordered_source_indices[index];
        generated_text = NULL;
        if (cblc_generate_c(ordered_units[index], &generated_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to generate C output for '%s'",
                    ordered_source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (pipeline_resolve_output_path(context, context->target_paths[source_index], resolved_path,
                sizeof(resolved_path)) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to resolve output path for '%s'",
                    context->target_paths[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            if (generated_text)
                cma_free(generated_text);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (pipeline_write_file(resolved_path, generated_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'",
                    resolved_path) >= 0)
                (void)pipeline_emit_error(context, message);
            if (generated_text)
                cma_free(generated_text);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (generated_text)
            cma_free(generated_text);
        module_indices[source_index] = static_cast<size_t>(-1);
        index += 1;
    }
    status = FT_SUCCESS;
cleanup:
    if (ordered_source_indices)
        cma_free(ordered_source_indices);
    if (ordered_source_paths)
        cma_free(ordered_source_paths);
    if (ordered_units)
        cma_free(ordered_units);
    if (module_indices)
        cma_free(module_indices);
    if (module_names)
        cma_free(module_names);
    if (units)
    {
        index = 0;
        while (index < file_count)
        {
            cblc_translation_unit_dispose(&units[index]);
            index += 1;
        }
        cma_free(units);
    }
    if (sources)
    {
        index = 0;
        while (index < file_count)
        {
            if (sources[index])
                cma_free(sources[index]);
            index += 1;
        }
        cma_free(sources);
    }
    return (status);
}

static int pipeline_stage_cblc_to_cobol(t_transpiler_context *context, void *user_data)
{
    t_cblc_translation_unit *units;
    char **sources;
    size_t *module_indices;
    char (*module_names)[TRANSPILE_MODULE_NAME_MAX];
    const size_t *order;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    const t_cblc_translation_unit **ordered_units;
    const char **ordered_source_paths;
    size_t *ordered_source_indices;
    t_transpiler_parallel_result *parallel_results;
    int generation_status;
    size_t order_count;
    size_t file_count;
    size_t index;
    int status;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    file_count = context->source_count;
    if (file_count == 0)
        return (FT_SUCCESS);
    transpiler_context_reset_module_registry(context);
    transpiler_context_reset_unit_state(context);
    units = static_cast<t_cblc_translation_unit *>(cma_calloc(file_count,
        sizeof(t_cblc_translation_unit)));
    sources = static_cast<char **>(cma_calloc(file_count, sizeof(char *)));
    module_indices = static_cast<size_t *>(cma_calloc(file_count, sizeof(size_t)));
    module_names = NULL;
    module_names = static_cast<char (*)[TRANSPILE_MODULE_NAME_MAX]>(cma_calloc(file_count,
        sizeof(*module_names)));
    if (!units || !sources || !module_indices || !module_names)
    {
        (void)pipeline_emit_error(context, "Unable to allocate module tracking for CBL-C inputs");
        status = FT_FAILURE;
        goto cleanup;
    }
    ordered_units = NULL;
    ordered_source_paths = NULL;
    ordered_source_indices = NULL;
    parallel_results = NULL;
    generation_status = FT_SUCCESS;
    order_count = 0;
    index = 0;
    while (index < file_count)
    {
        t_cblc_translation_unit *unit;
        char module_name[TRANSPILE_MODULE_NAME_MAX];
        size_t import_index;

        module_indices[index] = static_cast<size_t>(-1);
        unit = &units[index];
        cblc_translation_unit_init(unit);
        if (pipeline_read_file(context->source_paths[index], &sources[index]) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (cblc_parse_translation_unit(sources[index], unit) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to parse CBL-C source '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (unit->function_count == 0)
        {
            if (pf_snprintf(message, sizeof(message),
                    "CBL-C source '%s' does not declare any functions;", context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        else
        {
            size_t entry_index;

            entry_index = unit->entry_function_index;
            if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
                entry_index = 0;
            if (!unit->functions[entry_index].saw_return)
            {
                if (pf_snprintf(message, sizeof(message),
                        "CBL-C source '%s' is missing a terminating return;", context->source_paths[index]) >= 0)
                    (void)pipeline_emit_error(context, message);
                status = FT_FAILURE;
                goto cleanup;
            }
        }
        pipeline_choose_module_name(context->source_paths[index], unit, module_name,
            sizeof(module_name));
        ft_strlcpy(module_names[index], module_name, TRANSPILE_MODULE_NAME_MAX);
        if (transpiler_context_register_module(context, module_name, context->source_paths[index])
            != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to register module for '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (context->module_count == 0)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        module_indices[index] = context->module_count - 1;
        import_index = 0;
        while (import_index < unit->import_count)
        {
            if (transpiler_context_register_module_import(context, module_name,
                    unit->imports[import_index].path) != FT_SUCCESS)
            {
                if (pf_snprintf(message, sizeof(message),
                        "Failed to register import '%s' for module '%s'", unit->imports[import_index].path,
                        module_name) >= 0)
                    (void)pipeline_emit_error(context, message);
                status = FT_FAILURE;
                goto cleanup;
            }
            import_index += 1;
        }
        if (cblc_register_translation_unit_exports(context, module_name, unit) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Failed to register exports for module '%s'", module_name) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    index = 0;
    while (index < file_count)
    {
        if (cblc_resolve_translation_unit_calls(context, module_names[index], &units[index]) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message),
                    "Failed to resolve function calls for module '%s'", module_names[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    if (transpiler_context_compute_module_initialization_order(context) != FT_SUCCESS)
    {
        (void)pipeline_emit_error(context, "Unable to compute module initialization order");
        status = FT_FAILURE;
        goto cleanup;
    }
    order = transpiler_context_get_module_initialization_order(context, &order_count);
    if (!order || order_count == 0)
    {
        status = FT_FAILURE;
        goto cleanup;
    }
    ordered_units = static_cast<const t_cblc_translation_unit **>(cma_calloc(order_count,
        sizeof(*ordered_units)));
    ordered_source_paths = static_cast<const char **>(cma_calloc(order_count,
        sizeof(*ordered_source_paths)));
    ordered_source_indices = static_cast<size_t *>(cma_calloc(order_count,
        sizeof(*ordered_source_indices)));
    if (!ordered_units || !ordered_source_paths || !ordered_source_indices)
    {
        (void)pipeline_emit_error(context, "Unable to allocate parallel generation buffers");
        status = FT_FAILURE;
        goto cleanup;
    }
    index = 0;
    while (index < order_count)
    {
        size_t module_index;
        size_t source_index;

        module_index = order[index];
        source_index = 0;
        while (source_index < file_count)
        {
            if (module_indices[source_index] == module_index)
                break ;
            source_index += 1;
        }
        if (source_index >= file_count)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        ordered_units[index] = &units[source_index];
        ordered_source_paths[index] = context->source_paths[source_index];
        ordered_source_indices[index] = source_index;
        index += 1;
    }
    generation_status = transpiler_parallel_generate_cobol(ordered_units,
        ordered_source_paths, order_count, &parallel_results);
    if (!parallel_results && order_count > 0)
    {
        status = FT_FAILURE;
        goto cleanup;
    }
    index = 0;
    while (index < order_count)
    {
        size_t source_index;
        char resolved_path[TRANSPILE_FILE_PATH_MAX];

        source_index = ordered_source_indices[index];
        if (!parallel_results || parallel_results[index].status != FT_SUCCESS)
        {
            if (parallel_results && parallel_results[index].error_message[0] != '\0')
                (void)pipeline_emit_error(context, parallel_results[index].error_message);
            else
                (void)pipeline_emit_error(context, "Parallel COBOL generation failed");
            status = FT_FAILURE;
            goto cleanup;
        }
        if (pipeline_resolve_output_path(context, context->target_paths[source_index], resolved_path,
                sizeof(resolved_path)) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to resolve output path for '%s'",
                    context->target_paths[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (pipeline_write_file(resolved_path, parallel_results[index].text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'",
                    resolved_path) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (parallel_results[index].text)
        {
            cma_free(parallel_results[index].text);
            parallel_results[index].text = NULL;
        }
        module_indices[source_index] = static_cast<size_t>(-1);
        index += 1;
    }
    if (generation_status != FT_SUCCESS)
    {
        status = FT_FAILURE;
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (parallel_results)
        transpiler_parallel_results_dispose(parallel_results, order_count);
    if (ordered_source_indices)
        cma_free(ordered_source_indices);
    if (ordered_source_paths)
        cma_free(ordered_source_paths);
    if (ordered_units)
        cma_free(ordered_units);
    if (module_indices)
        cma_free(module_indices);
    if (module_names)
        cma_free(module_names);
    if (units)
    {
        index = 0;
        while (index < file_count)
        {
            cblc_translation_unit_dispose(&units[index]);
            index += 1;
        }
        cma_free(units);
    }
    if (sources)
    {
        index = 0;
        while (index < file_count)
        {
            if (sources[index])
                cma_free(sources[index]);
            index += 1;
        }
        cma_free(sources);
    }
    return (status);
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
    if (context.emit_standard_library)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "emit-standard-library",
                pipeline_stage_emit_standard_library, NULL) != FT_SUCCESS)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
    }
    else if (context.source_language == TRANSPILE_LANGUAGE_COBOL
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
    else if (context.source_language == TRANSPILE_LANGUAGE_CBL_C
        && context.target_language == TRANSPILE_LANGUAGE_C)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "cblc-to-c",
                pipeline_stage_cblc_to_c, NULL) != FT_SUCCESS)
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
