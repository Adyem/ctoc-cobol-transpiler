#include <cstdlib>

#include "libft/CMA/CMA.hpp"
#include "libft/Printf/printf.hpp"
#include "transpiler_context.hpp"

static int transpiler_context_functions_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    t_transpiler_function_signature *new_functions;

    if (!context)
        return (FT_FAILURE);
    if (context->function_capacity >= desired_capacity)
        return (FT_SUCCESS);
    new_functions = static_cast<t_transpiler_function_signature *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_function_signature)));
    if (!new_functions)
        return (FT_FAILURE);
    if (context->functions)
    {
        ft_memcpy(new_functions, context->functions,
            context->function_count * sizeof(t_transpiler_function_signature));
        cma_free(context->functions);
    }
    context->functions = new_functions;
    context->function_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_context_files_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    t_transpiler_file_declaration *new_files;

    if (!context)
        return (FT_FAILURE);
    if (context->file_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_files = static_cast<t_transpiler_file_declaration *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_file_declaration)));
    if (!new_files)
        return (FT_FAILURE);
    if (context->files)
    {
        ft_memcpy(new_files, context->files,
            context->file_count * sizeof(t_transpiler_file_declaration));
        cma_free(context->files);
    }
    context->files = new_files;
    context->file_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_context_assign_paths(const char ***storage, size_t *count,
    size_t *capacity, const char **paths, size_t path_count)
{
    const char **new_paths;

    if (!storage || !count || !capacity)
        return (FT_FAILURE);
    if (path_count == 0)
    {
        if (*storage && *capacity > 0)
            ft_bzero(*storage, *capacity * sizeof(const char *));
        *count = 0;
        return (FT_SUCCESS);
    }
    if (!paths)
        return (FT_FAILURE);
    if (*capacity < path_count)
    {
        new_paths = static_cast<const char **>(cma_calloc(path_count, sizeof(const char *)));
        if (!new_paths)
            return (FT_FAILURE);
        if (*storage)
            cma_free(*storage);
        *storage = new_paths;
        *capacity = path_count;
    }
    ft_memcpy(*storage, paths, path_count * sizeof(const char *));
    *count = path_count;
    return (FT_SUCCESS);
}

int transpiler_context_init(t_transpiler_context *context)
{
    if (!context)
        return (FT_FAILURE);
    context->source_language = TRANSPILE_LANGUAGE_NONE;
    context->target_language = TRANSPILE_LANGUAGE_NONE;
    context->source_path = NULL;
    context->target_path = NULL;
    context->source_paths = NULL;
    context->source_count = 0;
    context->source_capacity = 0;
    context->target_paths = NULL;
    context->target_count = 0;
    context->target_capacity = 0;
    context->output_directory = NULL;
    context->format_mode = TRANSPILE_FORMAT_DEFAULT;
    context->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
    context->last_error_code = FT_SUCCESS;
    context->functions = NULL;
    context->function_count = 0;
    context->function_capacity = 0;
    context->files = NULL;
    context->file_count = 0;
    context->file_capacity = 0;
    ft_bzero(&context->entrypoint, sizeof(context->entrypoint));
    if (transpiler_diagnostics_init(&context->diagnostics) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_functions_reserve(context, 4) != FT_SUCCESS)
    {
        transpiler_diagnostics_dispose(&context->diagnostics);
        return (FT_FAILURE);
    }
    if (transpiler_context_files_reserve(context, 4) != FT_SUCCESS)
    {
        if (context->functions)
            cma_free(context->functions);
        context->functions = NULL;
        context->function_count = 0;
        context->function_capacity = 0;
        transpiler_diagnostics_dispose(&context->diagnostics);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

void transpiler_context_dispose(t_transpiler_context *context)
{
    if (!context)
        return ;
    transpiler_diagnostics_dispose(&context->diagnostics);
    context->source_language = TRANSPILE_LANGUAGE_NONE;
    context->target_language = TRANSPILE_LANGUAGE_NONE;
    context->source_path = NULL;
    context->target_path = NULL;
    context->output_directory = NULL;
    context->format_mode = TRANSPILE_FORMAT_DEFAULT;
    context->diagnostic_level = TRANSPILE_DIAGNOSTIC_NORMAL;
    context->last_error_code = FT_SUCCESS;
    if (context->functions)
        cma_free(context->functions);
    context->functions = NULL;
    context->function_count = 0;
    context->function_capacity = 0;
    if (context->files)
        cma_free(context->files);
    context->files = NULL;
    context->file_count = 0;
    context->file_capacity = 0;
    if (context->source_paths)
        cma_free(context->source_paths);
    context->source_paths = NULL;
    context->source_count = 0;
    context->source_capacity = 0;
    if (context->target_paths)
        cma_free(context->target_paths);
    context->target_paths = NULL;
    context->target_count = 0;
    context->target_capacity = 0;
    ft_bzero(&context->entrypoint, sizeof(context->entrypoint));
}

void transpiler_context_set_languages(t_transpiler_context *context, t_transpiler_language source, t_transpiler_language target)
{
    if (!context)
        return ;
    context->source_language = source;
    context->target_language = target;
}

int transpiler_context_set_io_paths(t_transpiler_context *context, const char **source_paths, size_t source_count,
    const char **target_paths, size_t target_count)
{
    if (!context)
        return (FT_FAILURE);
    if (source_count != target_count)
        return (FT_FAILURE);
    if (transpiler_context_assign_paths(&context->source_paths, &context->source_count,
            &context->source_capacity, source_paths, source_count) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_assign_paths(&context->target_paths, &context->target_count,
            &context->target_capacity, target_paths, target_count) != FT_SUCCESS)
    {
        (void)transpiler_context_assign_paths(&context->source_paths, &context->source_count,
            &context->source_capacity, NULL, 0);
        context->source_path = NULL;
        context->target_path = NULL;
        return (FT_FAILURE);
    }
    context->source_path = NULL;
    context->target_path = NULL;
    if (context->source_count > 0)
        context->source_path = context->source_paths[0];
    if (context->target_count > 0)
        context->target_path = context->target_paths[0];
    return (FT_SUCCESS);
}

void transpiler_context_set_output_directory(t_transpiler_context *context, const char *output_directory)
{
    if (!context)
        return ;
    context->output_directory = output_directory;
}

void transpiler_context_set_format_mode(t_transpiler_context *context, t_transpiler_format_mode mode)
{
    if (!context)
        return ;
    context->format_mode = mode;
}

void transpiler_context_set_diagnostic_level(t_transpiler_context *context, t_transpiler_diagnostic_level level)
{
    if (!context)
        return ;
    context->diagnostic_level = level;
}

void transpiler_context_record_error(t_transpiler_context *context, int error_code)
{
    if (!context)
        return ;
    context->last_error_code = error_code;
}

int transpiler_context_has_errors(const t_transpiler_context *context)
{
    if (!context)
        return (0);
    if (context->last_error_code != FT_SUCCESS)
        return (1);
    if (context->diagnostics.count > 0)
        return (1);
    return (0);
}

int transpiler_context_register_function(t_transpiler_context *context, const char *name,
    t_transpiler_function_return_mode return_mode)
{
    t_transpiler_function_signature *signature;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (!name)
        return (FT_FAILURE);
    index = 0;
    while (index < context->function_count)
    {
        if (ft_strncmp(context->functions[index].name, name, TRANSPILE_FUNCTION_NAME_MAX) == 0)
        {
            pf_snprintf(message, sizeof(message),
                "function '%s' already declared; choose a unique name", name);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (context->function_count >= context->function_capacity)
    {
        if (transpiler_context_functions_reserve(context, context->function_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    signature = &context->functions[context->function_count];
    ft_strlcpy(signature->name, name, TRANSPILE_FUNCTION_NAME_MAX);
    signature->return_mode = return_mode;
    context->function_count += 1;
    return (FT_SUCCESS);
}

const t_transpiler_function_signature *transpiler_context_find_function(const t_transpiler_context *context,
    const char *name)
{
    size_t index;

    if (!context)
        return (NULL);
    if (!name)
        return (NULL);
    index = 0;
    while (index < context->function_count)
    {
        if (ft_strncmp(context->functions[index].name, name, TRANSPILE_FUNCTION_NAME_MAX) == 0)
            return (&context->functions[index]);
        index += 1;
    }
    return (NULL);
}

int transpiler_context_register_entrypoint(t_transpiler_context *context, const char *name,
    t_transpiler_function_return_mode return_mode, const char *argc_identifier, const char *argv_identifier)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return (FT_FAILURE);
    if (!name)
        return (FT_FAILURE);
    if (context->entrypoint.present)
    {
        pf_snprintf(message, sizeof(message),
            "entrypoint '%s' already registered", context->entrypoint.name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_ENTRYPOINT_DUPLICATE, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_ENTRYPOINT_DUPLICATE);
        return (FT_FAILURE);
    }
    if (ft_strncmp(name, "main", TRANSPILE_FUNCTION_NAME_MAX) != 0)
    {
        pf_snprintf(message, sizeof(message),
            "entrypoint '%s' must be declared as 'main'", name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_ENTRYPOINT_INVALID_NAME, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_ENTRYPOINT_INVALID_NAME);
        return (FT_FAILURE);
    }
    if (return_mode != TRANSPILE_FUNCTION_RETURN_VOID)
    {
        pf_snprintf(message, sizeof(message),
            "entrypoint '%s' must use void return semantics; pass outputs by reference", name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_FUNCTION_RETURNS_VALUE, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_FUNCTION_RETURNS_VALUE);
        return (FT_FAILURE);
    }
    if ((argc_identifier && !argv_identifier) || (!argc_identifier && argv_identifier))
    {
        pf_snprintf(message, sizeof(message),
            "entrypoint 'main' must provide both argc and argv when supplying arguments");
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_ENTRYPOINT_ARGUMENT_MISMATCH, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_ENTRYPOINT_ARGUMENT_MISMATCH);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_function(context, name, return_mode) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(&context->entrypoint, sizeof(context->entrypoint));
    context->entrypoint.present = 1;
    ft_strlcpy(context->entrypoint.name, name, sizeof(context->entrypoint.name));
    context->entrypoint.has_argument_vectors = 0;
    context->entrypoint.needs_argument_copy = 0;
    if (argc_identifier && argv_identifier)
    {
        context->entrypoint.has_argument_vectors = 1;
        context->entrypoint.needs_argument_copy = 1;
        ft_strlcpy(context->entrypoint.argc_identifier, argc_identifier,
            sizeof(context->entrypoint.argc_identifier));
        ft_strlcpy(context->entrypoint.argv_identifier, argv_identifier,
            sizeof(context->entrypoint.argv_identifier));
    }
    return (FT_SUCCESS);
}

const t_transpiler_entrypoint *transpiler_context_get_entrypoint(const t_transpiler_context *context)
{
    if (!context)
        return (NULL);
    if (!context->entrypoint.present)
        return (NULL);
    return (&context->entrypoint);
}

int transpiler_context_register_file(t_transpiler_context *context, const char *name, t_transpiler_file_role role,
    const char *path, size_t explicit_record_length)
{
    t_transpiler_file_declaration *file;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (!name || !path)
        return (FT_FAILURE);
    index = 0;
    while (index < context->file_count)
    {
        if (ft_strncmp(context->files[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
        {
            pf_snprintf(message, sizeof(message),
                "file '%s' already declared; choose a unique identifier", name);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_FILE_DUPLICATE_NAME, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_FILE_DUPLICATE_NAME);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (context->file_count >= context->file_capacity)
    {
        if (transpiler_context_files_reserve(context, context->file_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    file = &context->files[context->file_count];
    ft_bzero(file, sizeof(*file));
    ft_strlcpy(file->name, name, sizeof(file->name));
    ft_strlcpy(file->path, path, sizeof(file->path));
    file->role = role;
    file->explicit_record_length = explicit_record_length;
    file->inferred_record_length = explicit_record_length;
    context->file_count += 1;
    return (FT_SUCCESS);
}

int transpiler_context_record_file_length_hint(t_transpiler_context *context, const char *name, size_t record_length)
{
    t_transpiler_file_declaration *file;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (!name)
        return (FT_FAILURE);
    if (record_length == 0)
        return (FT_SUCCESS);
    index = 0;
    while (index < context->file_count)
    {
        if (ft_strncmp(context->files[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
            break ;
        index += 1;
    }
    if (index >= context->file_count)
    {
        pf_snprintf(message, sizeof(message),
            "file '%s' not declared before recording record length", name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_FILE_UNKNOWN, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_FILE_UNKNOWN);
        return (FT_FAILURE);
    }
    file = &context->files[index];
    if (file->explicit_record_length > 0)
        return (FT_SUCCESS);
    if (record_length > file->inferred_record_length)
        file->inferred_record_length = record_length;
    return (FT_SUCCESS);
}

const t_transpiler_file_declaration *transpiler_context_get_files(const t_transpiler_context *context, size_t *count)
{
    if (!context)
        return (NULL);
    if (count)
        *count = context->file_count;
    return (context->files);
}
