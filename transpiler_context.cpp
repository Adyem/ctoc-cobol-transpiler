#include <cstdlib>

#include "libft/CMA/CMA.hpp"
#include "libft/Printf/printf.hpp"
#include "transpiler_context.hpp"

static void transpiler_context_module_clear(t_transpiler_module *module)
{
    if (!module)
        return ;
    if (module->imports)
        cma_free(module->imports);
    module->imports = NULL;
    module->import_count = 0;
    module->import_capacity = 0;
    module->name[0] = '\0';
    module->path[0] = '\0';
    module->initialization_rank = 0;
}

static int transpiler_context_string_is_blank(const char *value)
{
    if (!value)
        return (1);
    while (*value != '\0')
    {
        if (!ft_isspace(*value))
            return (0);
        value += 1;
    }
    return (1);
}

static int transpiler_context_module_imports_reserve(t_transpiler_module *module, size_t desired_capacity)
{
    t_transpiler_module_import *imports;

    if (!module)
        return (FT_FAILURE);
    if (module->import_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    imports = static_cast<t_transpiler_module_import *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_module_import)));
    if (!imports)
        return (FT_FAILURE);
    if (module->imports)
    {
        ft_memcpy(imports, module->imports,
            module->import_count * sizeof(t_transpiler_module_import));
        cma_free(module->imports);
    }
    module->imports = imports;
    module->import_capacity = desired_capacity;
    return (FT_SUCCESS);
}

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

static int transpiler_context_modules_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    t_transpiler_module *modules;
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (context->module_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    modules = static_cast<t_transpiler_module *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_module)));
    if (!modules)
        return (FT_FAILURE);
    if (context->modules)
    {
        index = 0;
        while (index < context->module_count)
        {
            modules[index] = context->modules[index];
            index += 1;
        }
        cma_free(context->modules);
    }
    context->modules = modules;
    context->module_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_context_module_order_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    size_t *order;

    if (!context)
        return (FT_FAILURE);
    if (context->module_order_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    order = static_cast<size_t *>(cma_calloc(desired_capacity, sizeof(size_t)));
    if (!order)
        return (FT_FAILURE);
    if (context->module_order)
    {
        ft_memcpy(order, context->module_order,
            context->module_order_count * sizeof(size_t));
        cma_free(context->module_order);
    }
    context->module_order = order;
    context->module_order_capacity = desired_capacity;
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

static int transpiler_context_data_items_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    t_transpiler_data_item *items;

    if (!context)
        return (FT_FAILURE);
    if (context->data_item_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    items = static_cast<t_transpiler_data_item *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_data_item)));
    if (!items)
        return (FT_FAILURE);
    if (context->data_items)
    {
        ft_memcpy(items, context->data_items,
            context->data_item_count * sizeof(t_transpiler_data_item));
        cma_free(context->data_items);
    }
    context->data_items = items;
    context->data_item_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_context_source_maps_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    t_transpiler_source_map_entry *maps;

    if (!context)
        return (FT_FAILURE);
    if (context->source_map_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 8)
        desired_capacity = 8;
    maps = static_cast<t_transpiler_source_map_entry *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_source_map_entry)));
    if (!maps)
        return (FT_FAILURE);
    if (context->source_maps)
    {
        ft_memcpy(maps, context->source_maps,
            context->source_map_count * sizeof(t_transpiler_source_map_entry));
        cma_free(context->source_maps);
    }
    context->source_maps = maps;
    context->source_map_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_context_copybooks_reserve(t_transpiler_context *context, size_t desired_capacity)
{
    t_transpiler_copybook *copybooks;

    if (!context)
        return (FT_FAILURE);
    if (context->copybook_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    copybooks = static_cast<t_transpiler_copybook *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_copybook)));
    if (!copybooks)
        return (FT_FAILURE);
    if (context->copybooks)
    {
        ft_memcpy(copybooks, context->copybooks,
            context->copybook_count * sizeof(t_transpiler_copybook));
        cma_free(context->copybooks);
    }
    context->copybooks = copybooks;
    context->copybook_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_context_span_is_valid(const t_transpiler_source_span *span)
{
    if (!span)
        return (0);
    if (transpiler_context_string_is_blank(span->path))
        return (0);
    if (span->end_line < span->start_line)
        return (0);
    if (span->end_line == span->start_line && span->end_column < span->start_column)
        return (0);
    return (1);
}

static int transpiler_context_span_contains(const t_transpiler_source_span *span, size_t line, size_t column)
{
    if (!span)
        return (0);
    if (line < span->start_line)
        return (0);
    if (line > span->end_line)
        return (0);
    if (line == span->start_line && column < span->start_column)
        return (0);
    if (line == span->end_line && column > span->end_column)
        return (0);
    return (1);
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
    context->warnings_as_errors = 0;
    context->last_error_code = FT_SUCCESS;
    context->functions = NULL;
    context->function_count = 0;
    context->function_capacity = 0;
    context->files = NULL;
    context->file_count = 0;
    context->file_capacity = 0;
    context->modules = NULL;
    context->module_count = 0;
    context->module_capacity = 0;
    context->module_order = NULL;
    context->module_order_count = 0;
    context->module_order_capacity = 0;
    ft_bzero(&context->entrypoint, sizeof(context->entrypoint));
    context->data_items = NULL;
    context->data_item_count = 0;
    context->data_item_capacity = 0;
    context->source_maps = NULL;
    context->source_map_count = 0;
    context->source_map_capacity = 0;
    context->copybooks = NULL;
    context->copybook_count = 0;
    context->copybook_capacity = 0;
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
    if (transpiler_context_modules_reserve(context, 4) != FT_SUCCESS)
    {
        if (context->files)
            cma_free(context->files);
        context->files = NULL;
        context->file_count = 0;
        context->file_capacity = 0;
        if (context->functions)
            cma_free(context->functions);
        context->functions = NULL;
        context->function_count = 0;
        context->function_capacity = 0;
        transpiler_diagnostics_dispose(&context->diagnostics);
        return (FT_FAILURE);
    }
    if (transpiler_context_module_order_reserve(context, 4) != FT_SUCCESS)
    {
        if (context->modules)
        {
            size_t index;

            index = 0;
            while (index < context->module_count)
            {
                transpiler_context_module_clear(&context->modules[index]);
                index += 1;
            }
            cma_free(context->modules);
        }
        context->modules = NULL;
        context->module_count = 0;
        context->module_capacity = 0;
        if (context->files)
            cma_free(context->files);
        context->files = NULL;
        context->file_count = 0;
        context->file_capacity = 0;
        if (context->functions)
            cma_free(context->functions);
        context->functions = NULL;
        context->function_count = 0;
        context->function_capacity = 0;
        transpiler_diagnostics_dispose(&context->diagnostics);
        return (FT_FAILURE);
    }
    if (transpiler_context_data_items_reserve(context, 4) != FT_SUCCESS)
    {
        if (context->module_order)
            cma_free(context->module_order);
        context->module_order = NULL;
        context->module_order_count = 0;
        context->module_order_capacity = 0;
        if (context->modules)
        {
            size_t index;

            index = 0;
            while (index < context->module_count)
            {
                transpiler_context_module_clear(&context->modules[index]);
                index += 1;
            }
            cma_free(context->modules);
        }
        context->modules = NULL;
        context->module_count = 0;
        context->module_capacity = 0;
        if (context->files)
            cma_free(context->files);
        context->files = NULL;
        context->file_count = 0;
        context->file_capacity = 0;
        if (context->functions)
            cma_free(context->functions);
        context->functions = NULL;
        context->function_count = 0;
        context->function_capacity = 0;
        transpiler_diagnostics_dispose(&context->diagnostics);
        return (FT_FAILURE);
    }
    if (transpiler_context_source_maps_reserve(context, 8) != FT_SUCCESS)
    {
        if (context->data_items)
            cma_free(context->data_items);
        context->data_items = NULL;
        context->data_item_count = 0;
        context->data_item_capacity = 0;
        if (context->module_order)
            cma_free(context->module_order);
        context->module_order = NULL;
        context->module_order_count = 0;
        context->module_order_capacity = 0;
        if (context->modules)
        {
            size_t index;

            index = 0;
            while (index < context->module_count)
            {
                transpiler_context_module_clear(&context->modules[index]);
                index += 1;
            }
            cma_free(context->modules);
        }
        context->modules = NULL;
        context->module_count = 0;
        context->module_capacity = 0;
        if (context->files)
            cma_free(context->files);
        context->files = NULL;
        context->file_count = 0;
        context->file_capacity = 0;
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
    context->warnings_as_errors = 0;
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
    if (context->modules)
    {
        size_t index;

        index = 0;
        while (index < context->module_count)
        {
            transpiler_context_module_clear(&context->modules[index]);
            index += 1;
        }
        cma_free(context->modules);
    }
    context->modules = NULL;
    context->module_count = 0;
    context->module_capacity = 0;
    if (context->module_order)
        cma_free(context->module_order);
    context->module_order = NULL;
    context->module_order_count = 0;
    context->module_order_capacity = 0;
    if (context->data_items)
        cma_free(context->data_items);
    context->data_items = NULL;
    context->data_item_count = 0;
    context->data_item_capacity = 0;
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
    if (context->source_maps)
        cma_free(context->source_maps);
    context->source_maps = NULL;
    context->source_map_count = 0;
    context->source_map_capacity = 0;
    if (context->copybooks)
    {
        size_t index;

        index = 0;
        while (index < context->copybook_count)
        {
            if (context->copybooks[index].items)
                cma_free(context->copybooks[index].items);
            context->copybooks[index].items = NULL;
            context->copybooks[index].item_count = 0;
            index += 1;
        }
        cma_free(context->copybooks);
    }
    context->copybooks = NULL;
    context->copybook_count = 0;
    context->copybook_capacity = 0;
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

void transpiler_context_set_warnings_as_errors(t_transpiler_context *context, int warnings_as_errors)
{
    if (!context)
        return ;
    context->warnings_as_errors = warnings_as_errors;
}

void transpiler_context_record_error(t_transpiler_context *context, int error_code)
{
    if (!context)
        return ;
    context->last_error_code = error_code;
}

int transpiler_context_has_errors(const t_transpiler_context *context)
{
    size_t index;

    if (!context)
        return (0);
    if (context->last_error_code != FT_SUCCESS)
        return (1);
    index = 0;
    while (index < context->diagnostics.count)
    {
        if (context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_ERROR)
            return (1);
        if (context->warnings_as_errors
            && context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_WARNING)
            return (1);
        index += 1;
    }
    return (0);
}

static int transpiler_context_is_identifier_char(char value)
{
    if (value >= 'a' && value <= 'z')
        return (1);
    if (value >= 'A' && value <= 'Z')
        return (1);
    if (value >= '0' && value <= '9')
        return (1);
    if (value == '_')
        return (1);
    return (0);
}

static int transpiler_context_find_module_index_by_name(const t_transpiler_context *context, const char *name)
{
    size_t index;

    if (!context)
        return (-1);
    if (!name)
        return (-1);
    index = 0;
    while (index < context->module_count)
    {
        if (ft_strncmp(context->modules[index].name, name, TRANSPILE_MODULE_NAME_MAX) == 0)
            return (static_cast<int>(index));
        index += 1;
    }
    return (-1);
}

static int transpiler_context_find_module_index_by_path(const t_transpiler_context *context, const char *path)
{
    size_t index;

    if (!context)
        return (-1);
    if (!path || path[0] == '\0')
        return (-1);
    index = 0;
    while (index < context->module_count)
    {
        if (ft_strncmp(context->modules[index].path, path, TRANSPILE_FILE_PATH_MAX) == 0)
            return (static_cast<int>(index));
        index += 1;
    }
    return (-1);
}

static int transpiler_context_find_module_index(const t_transpiler_context *context, const char *identifier)
{
    int index;

    index = transpiler_context_find_module_index_by_name(context, identifier);
    if (index >= 0)
        return (index);
    return (transpiler_context_find_module_index_by_path(context, identifier));
}

int transpiler_context_register_module(t_transpiler_context *context, const char *name, const char *path)
{
    t_transpiler_module *module;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (transpiler_context_string_is_blank(name))
        return (FT_FAILURE);
    if (path && transpiler_context_string_is_blank(path))
        return (FT_FAILURE);
    index = 0;
    while (index < context->module_count)
    {
        if (ft_strncmp(context->modules[index].name, name, TRANSPILE_MODULE_NAME_MAX) == 0)
        {
            pf_snprintf(message, sizeof(message),
                "module '%s' already registered", name);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_MODULE_DUPLICATE_NAME, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_MODULE_DUPLICATE_NAME);
            return (FT_FAILURE);
        }
        if (path && path[0] != '\0'
            && ft_strncmp(context->modules[index].path, path, TRANSPILE_FILE_PATH_MAX) == 0)
        {
            pf_snprintf(message, sizeof(message),
                "module path '%s' already registered", path);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_MODULE_DUPLICATE_NAME, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_MODULE_DUPLICATE_NAME);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (context->module_count >= context->module_capacity)
    {
        if (transpiler_context_modules_reserve(context, context->module_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    module = &context->modules[context->module_count];
    ft_bzero(module, sizeof(*module));
    ft_strlcpy(module->name, name, sizeof(module->name));
    if (path)
        ft_strlcpy(module->path, path, sizeof(module->path));
    module->imports = NULL;
    module->import_count = 0;
    module->import_capacity = 0;
    module->initialization_rank = 0;
    context->module_count += 1;
    return (FT_SUCCESS);
}

const t_transpiler_module *transpiler_context_get_modules(const t_transpiler_context *context, size_t *count)
{
    if (!context)
        return (NULL);
    if (count)
        *count = context->module_count;
    return (context->modules);
}

int transpiler_context_register_module_import(t_transpiler_context *context, const char *module_name,
    const char *import_path)
{
    t_transpiler_module *module;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int module_index;
    size_t index;
    size_t insert_index;

    if (!context)
        return (FT_FAILURE);
    if (transpiler_context_string_is_blank(module_name)
        || transpiler_context_string_is_blank(import_path))
        return (FT_FAILURE);
    module_index = transpiler_context_find_module_index_by_name(context, module_name);
    if (module_index < 0)
    {
        pf_snprintf(message, sizeof(message),
            "module '%s' not registered", module_name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_MODULE_UNKNOWN, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_MODULE_UNKNOWN);
        return (FT_FAILURE);
    }
    module = &context->modules[static_cast<size_t>(module_index)];
    index = 0;
    while (index < module->import_count)
    {
        if (ft_strncmp(module->imports[index].path, import_path, TRANSPILE_FILE_PATH_MAX) == 0)
            return (FT_SUCCESS);
        index += 1;
    }
    if (module->import_count >= module->import_capacity)
    {
        size_t desired;

        desired = 4;
        if (module->import_capacity > 0)
            desired = module->import_capacity * 2;
        if (transpiler_context_module_imports_reserve(module, desired) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    insert_index = module->import_count;
    while (insert_index > 0
        && ft_strncmp(module->imports[insert_index - 1].path, import_path, TRANSPILE_FILE_PATH_MAX) > 0)
    {
        module->imports[insert_index] = module->imports[insert_index - 1];
        insert_index -= 1;
    }
    ft_strlcpy(module->imports[insert_index].path, import_path, TRANSPILE_FILE_PATH_MAX);
    module->imports[insert_index].resolved_index = static_cast<size_t>(-1);
    module->import_count += 1;
    return (FT_SUCCESS);
}

int transpiler_context_scan_imports_for_module(t_transpiler_context *context, const char *module_name,
    const char *source_text)
{
    const char *cursor;
    char path[TRANSPILE_FILE_PATH_MAX];
    size_t length;
    int status;

    if (!context)
        return (FT_FAILURE);
    if (!module_name)
        return (FT_FAILURE);
    if (!source_text)
        return (FT_SUCCESS);
    cursor = source_text;
    status = FT_SUCCESS;
    while (cursor && *cursor != '\0')
    {
        if (*cursor == '/' && *(cursor + 1) == '/')
        {
            cursor += 2;
            while (*cursor != '\0' && *cursor != '\n')
                cursor += 1;
            continue ;
        }
        if (ft_isspace(*cursor))
        {
            cursor += 1;
            continue ;
        }
        if (ft_strncmp(cursor, "import", 6) == 0 && !transpiler_context_is_identifier_char(*(cursor + 6)))
        {
            const char *scan;

            scan = cursor + 6;
            while (*scan != '\0' && ft_isspace(*scan))
                scan += 1;
            if (*scan == '"')
            {
                scan += 1;
                length = 0;
                while (scan[length] != '\0' && scan[length] != '"')
                {
                    if (length + 1 >= sizeof(path))
                        break ;
                    path[length] = scan[length];
                    length += 1;
                }
                if (scan[length] == '"' && length < sizeof(path))
                {
                    path[length] = '\0';
                    scan += length + 1;
                    while (*scan != '\0' && ft_isspace(*scan))
                        scan += 1;
                    if (*scan == ';')
                        scan += 1;
                    if (transpiler_context_register_module_import(context, module_name, path) != FT_SUCCESS)
                        status = FT_FAILURE;
                    cursor = scan;
                    continue ;
                }
            }
        }
        while (*cursor != '\0' && *cursor != '\n')
            cursor += 1;
        if (*cursor == '\n')
            cursor += 1;
    }
    return (status);
}

static int transpiler_context_module_visit(t_transpiler_context *context, size_t module_index,
    int *states, size_t *order_position)
{
    t_transpiler_module *module;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t import_index;
    int dependency_index;

    if (!context || !states || !order_position)
        return (FT_FAILURE);
    if (states[module_index] == 1)
    {
        module = &context->modules[module_index];
        pf_snprintf(message, sizeof(message),
            "module '%s' has an import cycle", module->name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_MODULE_IMPORT_CYCLE, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_MODULE_IMPORT_CYCLE);
        return (FT_FAILURE);
    }
    if (states[module_index] == 2)
        return (FT_SUCCESS);
    states[module_index] = 1;
    module = &context->modules[module_index];
    import_index = 0;
    while (import_index < module->import_count)
    {
        dependency_index = transpiler_context_find_module_index(context, module->imports[import_index].path);
        if (dependency_index < 0)
        {
            pf_snprintf(message, sizeof(message),
                "module '%s' imports unknown module '%s'", module->name,
                module->imports[import_index].path);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_MODULE_IMPORT_UNKNOWN, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_MODULE_IMPORT_UNKNOWN);
            return (FT_FAILURE);
        }
        module->imports[import_index].resolved_index = static_cast<size_t>(dependency_index);
        if (transpiler_context_module_visit(context, static_cast<size_t>(dependency_index), states, order_position)
            != FT_SUCCESS)
            return (FT_FAILURE);
        import_index += 1;
    }
    states[module_index] = 2;
    if (*order_position >= context->module_order_capacity)
    {
        if (transpiler_context_module_order_reserve(context, context->module_order_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    context->module_order[*order_position] = module_index;
    context->modules[module_index].initialization_rank = *order_position;
    *order_position += 1;
    return (FT_SUCCESS);
}

int transpiler_context_compute_module_initialization_order(t_transpiler_context *context)
{
    size_t *sorted;
    int *states;
    size_t index;
    size_t position;
    size_t inner;
    int status;

    if (!context)
        return (FT_FAILURE);
    context->module_order_count = 0;
    if (context->module_count == 0)
        return (FT_SUCCESS);
    if (transpiler_context_module_order_reserve(context, context->module_count) != FT_SUCCESS)
        return (FT_FAILURE);
    sorted = static_cast<size_t *>(cma_calloc(context->module_count, sizeof(size_t)));
    if (!sorted)
        return (FT_FAILURE);
    states = static_cast<int *>(cma_calloc(context->module_count, sizeof(int)));
    if (!states)
    {
        cma_free(sorted);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < context->module_count)
    {
        sorted[index] = index;
        states[index] = 0;
        context->modules[index].initialization_rank = 0;
        index += 1;
    }
    index = 0;
    while (index < context->module_count)
    {
        inner = index;
        while (inner > 0)
        {
            size_t current_index;
            size_t previous_index;
            int compare;

            current_index = sorted[inner];
            previous_index = sorted[inner - 1];
            compare = ft_strncmp(context->modules[current_index].name,
                context->modules[previous_index].name, TRANSPILE_MODULE_NAME_MAX);
            if (compare >= 0)
                break ;
            sorted[inner] = previous_index;
            sorted[inner - 1] = current_index;
            inner -= 1;
        }
        index += 1;
    }
    position = 0;
    status = FT_SUCCESS;
    index = 0;
    while (index < context->module_count && status == FT_SUCCESS)
    {
        if (transpiler_context_module_visit(context, sorted[index], states, &position) != FT_SUCCESS)
            status = FT_FAILURE;
        index += 1;
    }
    if (status == FT_SUCCESS)
        context->module_order_count = position;
    cma_free(states);
    cma_free(sorted);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const size_t *transpiler_context_get_module_initialization_order(const t_transpiler_context *context, size_t *count)
{
    if (!context)
        return (NULL);
    if (count)
        *count = context->module_order_count;
    return (context->module_order);
}

int transpiler_context_register_function(t_transpiler_context *context, const char *module_name, const char *name,
    t_transpiler_function_return_mode return_mode, t_transpiler_symbol_visibility visibility)
{
    t_transpiler_function_signature *signature;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t index;
    int module_index;

    if (!context)
        return (FT_FAILURE);
    if (transpiler_context_string_is_blank(module_name)
        || transpiler_context_string_is_blank(name))
        return (FT_FAILURE);
    module_index = transpiler_context_find_module_index_by_name(context, module_name);
    if (module_index < 0)
    {
        pf_snprintf(message, sizeof(message),
            "module '%s' not registered", module_name);
        transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
            TRANSPILE_ERROR_MODULE_UNKNOWN, message);
        transpiler_context_record_error(context, TRANSPILE_ERROR_MODULE_UNKNOWN);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < context->function_count)
    {
        if (ft_strncmp(context->functions[index].module, module_name, TRANSPILE_MODULE_NAME_MAX) == 0
            && ft_strncmp(context->functions[index].name, name, TRANSPILE_FUNCTION_NAME_MAX) == 0)
        {
            pf_snprintf(message, sizeof(message),
                "function '%s' already declared in module '%s'", name, module_name);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME);
            return (FT_FAILURE);
        }
        if (visibility == TRANSPILE_SYMBOL_PUBLIC
            && context->functions[index].visibility == TRANSPILE_SYMBOL_PUBLIC
            && ft_strncmp(context->functions[index].name, name, TRANSPILE_FUNCTION_NAME_MAX) == 0
            && ft_strncmp(context->functions[index].module, module_name, TRANSPILE_MODULE_NAME_MAX) != 0)
        {
            pf_snprintf(message, sizeof(message),
                "public function '%s' in module '%s' conflicts with export from module '%s'",
                name, module_name, context->functions[index].module);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_FUNCTION_EXPORT_CONFLICT, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_FUNCTION_EXPORT_CONFLICT);
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
    ft_strlcpy(signature->name, name, sizeof(signature->name));
    ft_strlcpy(signature->module, module_name, sizeof(signature->module));
    signature->return_mode = return_mode;
    signature->visibility = visibility;
    context->function_count += 1;
    (void)module_index;
    return (FT_SUCCESS);
}

const t_transpiler_function_signature *transpiler_context_find_function(const t_transpiler_context *context,
    const char *module_name, const char *name)
{
    size_t index;

    if (!context)
        return (NULL);
    if (!module_name || !name)
        return (NULL);
    index = 0;
    while (index < context->function_count)
    {
        if (ft_strncmp(context->functions[index].module, module_name, TRANSPILE_MODULE_NAME_MAX) == 0
            && ft_strncmp(context->functions[index].name, name, TRANSPILE_FUNCTION_NAME_MAX) == 0)
            return (&context->functions[index]);
        index += 1;
    }
    return (NULL);
}

const t_transpiler_function_signature *transpiler_context_resolve_function_access(t_transpiler_context *context,
    const char *requesting_module, const char *module_name, const char *name)
{
    const t_transpiler_function_signature *signature;
    const char *requester_label;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return (NULL);
    if (transpiler_context_string_is_blank(module_name)
        || transpiler_context_string_is_blank(name))
        return (NULL);
    signature = transpiler_context_find_function(context, module_name, name);
    if (!signature)
        return (NULL);
    if (transpiler_context_string_is_blank(requesting_module))
        return (signature);
    if (ft_strncmp(requesting_module, module_name, TRANSPILE_MODULE_NAME_MAX) == 0)
        return (signature);
    if (signature->visibility == TRANSPILE_SYMBOL_PUBLIC)
        return (signature);
    requester_label = requesting_module;
    pf_snprintf(message, sizeof(message),
        "module '%s' cannot access private function '%s' exported by module '%s'",
        requester_label, name, module_name);
    transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
        TRANSPILE_ERROR_FUNCTION_PRIVATE_ACCESS, message);
    transpiler_context_record_error(context, TRANSPILE_ERROR_FUNCTION_PRIVATE_ACCESS);
    return (NULL);
}

int transpiler_context_register_entrypoint(t_transpiler_context *context, const char *module_name, const char *name,
    t_transpiler_function_return_mode return_mode, const char *argc_identifier, const char *argv_identifier)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return (FT_FAILURE);
    if (transpiler_context_string_is_blank(module_name)
        || transpiler_context_string_is_blank(name))
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
    if (transpiler_context_register_function(context, module_name, name, return_mode,
            TRANSPILE_SYMBOL_PUBLIC) != FT_SUCCESS)
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

int transpiler_context_register_data_item(t_transpiler_context *context, const char *name,
    t_transpiler_data_item_kind kind, size_t declared_length, int is_read_only)
{
    t_transpiler_data_item *item;
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (!name)
        return (FT_FAILURE);
    index = 0;
    while (index < context->data_item_count)
    {
        if (ft_strncmp(context->data_items[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
        {
            size_t existing_length;
            t_transpiler_data_item_kind existing_kind;

            existing_length = context->data_items[index].declared_length;
            existing_kind = context->data_items[index].kind;
            context->data_items[index].kind = kind;
            if (is_read_only)
                context->data_items[index].is_read_only = 1;
            if (existing_kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && existing_length > 0
                && declared_length > 0
                && declared_length > existing_length)
            {
                if (!context->data_items[index].has_caller_length)
                {
                    context->data_items[index].declared_length = declared_length;
                    context->data_items[index].has_caller_length = 1;
                }
                return (FT_SUCCESS);
            }
            if (existing_kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && existing_length > 0
                && declared_length > 0
                && declared_length < existing_length)
            {
                if (context->data_items[index].has_caller_length)
                {
                    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    pf_snprintf(message, sizeof(message),
                        "subprogram data item '%s' (%lu characters) is smaller than caller buffer (%lu characters)",
                        name,
                        static_cast<unsigned long>(declared_length),
                        static_cast<unsigned long>(existing_length));
                    transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                        TRANSPILE_ERROR_DATA_ITEM_PARAMETER_TRUNCATION, message);
                    transpiler_context_record_error(context, TRANSPILE_ERROR_DATA_ITEM_PARAMETER_TRUNCATION);
                    return (FT_FAILURE);
                }
                context->data_items[index].declared_length = declared_length;
                context->data_items[index].has_caller_length = 1;
                return (FT_SUCCESS);
            }
            if (declared_length > 0 && existing_length == 0)
            {
                context->data_items[index].declared_length = declared_length;
                return (FT_SUCCESS);
            }
            if (existing_length > 0)
            {
                if (declared_length == 0 || declared_length > existing_length)
                    declared_length = existing_length;
            }
            context->data_items[index].declared_length = declared_length;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    if (context->data_item_count >= context->data_item_capacity)
    {
        size_t desired_capacity;

        desired_capacity = context->data_item_capacity;
        if (desired_capacity == 0)
            desired_capacity = 4;
        else
            desired_capacity *= 2;
        if (transpiler_context_data_items_reserve(context, desired_capacity) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &context->data_items[context->data_item_count];
    ft_bzero(item, sizeof(*item));
    ft_strlcpy(item->name, name, sizeof(item->name));
    item->kind = kind;
    item->declared_length = declared_length;
    item->is_read_only = is_read_only ? 1 : 0;
    context->data_item_count += 1;
    return (FT_SUCCESS);
}

const t_transpiler_data_item *transpiler_context_find_data_item(const t_transpiler_context *context, const char *name)
{
    size_t index;

    if (!context)
        return (NULL);
    if (!name)
        return (NULL);
    index = 0;
    while (index < context->data_item_count)
    {
        if (ft_strncmp(context->data_items[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (&context->data_items[index]);
        index += 1;
    }
    return (NULL);
}

int transpiler_context_register_copybook(t_transpiler_context *context, const char *name,
    const t_transpiler_copybook_item *items, size_t item_count)
{
    t_transpiler_copybook *copybook;
    size_t index;

    if (!context)
        return (FT_FAILURE);
    if (transpiler_context_string_is_blank(name))
        return (FT_FAILURE);
    if (item_count > 0 && !items)
        return (FT_FAILURE);
    index = 0;
    while (index < context->copybook_count)
    {
        if (ft_strncmp(context->copybooks[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            pf_snprintf(message, sizeof(message),
                "copybook '%s' registered multiple times", name);
            transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR,
                TRANSPILE_ERROR_COPYBOOK_DUPLICATE, message);
            transpiler_context_record_error(context, TRANSPILE_ERROR_COPYBOOK_DUPLICATE);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (context->copybook_count >= context->copybook_capacity)
    {
        size_t desired_capacity;

        desired_capacity = context->copybook_capacity == 0 ? 4 : context->copybook_capacity * 2;
        if (transpiler_context_copybooks_reserve(context, desired_capacity) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    copybook = &context->copybooks[context->copybook_count];
    ft_bzero(copybook, sizeof(*copybook));
    ft_strlcpy(copybook->name, name, sizeof(copybook->name));
    if (item_count > 0)
    {
        copybook->items = static_cast<t_transpiler_copybook_item *>(cma_calloc(item_count,
            sizeof(t_transpiler_copybook_item)));
        if (!copybook->items)
        {
            copybook->name[0] = '\0';
            return (FT_FAILURE);
        }
        index = 0;
        while (index < item_count)
        {
            copybook->items[index] = items[index];
            index += 1;
        }
    }
    copybook->item_count = item_count;
    context->copybook_count += 1;
    return (FT_SUCCESS);
}

const t_transpiler_copybook *transpiler_context_find_copybook(const t_transpiler_context *context, const char *name)
{
    size_t index;

    if (!context)
        return (NULL);
    if (transpiler_context_string_is_blank(name))
        return (NULL);
    index = 0;
    while (index < context->copybook_count)
    {
        if (ft_strncmp(context->copybooks[index].name, name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (&context->copybooks[index]);
        index += 1;
    }
    return (NULL);
}

const t_transpiler_data_item *transpiler_context_get_data_items(const t_transpiler_context *context, size_t *count)
{
    if (!context)
        return (NULL);
    if (count)
        *count = context->data_item_count;
    return (context->data_items);
}

int transpiler_context_record_source_map_entry(t_transpiler_context *context,
    const t_transpiler_source_span *cblc_span, const t_transpiler_source_span *cobol_span)
{
    t_transpiler_source_map_entry *entry;

    if (!context)
        return (FT_FAILURE);
    if (!transpiler_context_span_is_valid(cblc_span))
        return (FT_FAILURE);
    if (!transpiler_context_span_is_valid(cobol_span))
        return (FT_FAILURE);
    if (transpiler_context_source_maps_reserve(context, context->source_map_count + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    entry = &context->source_maps[context->source_map_count];
    ft_bzero(entry, sizeof(*entry));
    ft_strlcpy(entry->cblc_span.path, cblc_span->path, TRANSPILE_FILE_PATH_MAX);
    entry->cblc_span.start_line = cblc_span->start_line;
    entry->cblc_span.start_column = cblc_span->start_column;
    entry->cblc_span.end_line = cblc_span->end_line;
    entry->cblc_span.end_column = cblc_span->end_column;
    ft_strlcpy(entry->cobol_span.path, cobol_span->path, TRANSPILE_FILE_PATH_MAX);
    entry->cobol_span.start_line = cobol_span->start_line;
    entry->cobol_span.start_column = cobol_span->start_column;
    entry->cobol_span.end_line = cobol_span->end_line;
    entry->cobol_span.end_column = cobol_span->end_column;
    context->source_map_count += 1;
    return (FT_SUCCESS);
}

const t_transpiler_source_map_entry *transpiler_context_get_source_maps(const t_transpiler_context *context,
    size_t *count)
{
    if (!context)
        return (NULL);
    if (count)
        *count = context->source_map_count;
    return (context->source_maps);
}

const t_transpiler_source_map_entry *transpiler_context_map_cblc_to_cobol(const t_transpiler_context *context,
    const char *path, size_t line, size_t column)
{
    size_t index;

    if (!context)
        return (NULL);
    if (transpiler_context_string_is_blank(path))
        return (NULL);
    index = 0;
    while (index < context->source_map_count)
    {
        if (ft_strncmp(context->source_maps[index].cblc_span.path, path, TRANSPILE_FILE_PATH_MAX) == 0
            && transpiler_context_span_contains(&context->source_maps[index].cblc_span, line, column))
        {
            return (&context->source_maps[index]);
        }
        index += 1;
    }
    return (NULL);
}

const t_transpiler_source_map_entry *transpiler_context_map_cobol_to_cblc(const t_transpiler_context *context,
    const char *path, size_t line, size_t column)
{
    size_t index;

    if (!context)
        return (NULL);
    if (transpiler_context_string_is_blank(path))
        return (NULL);
    index = 0;
    while (index < context->source_map_count)
    {
        if (ft_strncmp(context->source_maps[index].cobol_span.path, path, TRANSPILE_FILE_PATH_MAX) == 0
            && transpiler_context_span_contains(&context->source_maps[index].cobol_span, line, column))
        {
            return (&context->source_maps[index]);
        }
        index += 1;
    }
    return (NULL);
}
