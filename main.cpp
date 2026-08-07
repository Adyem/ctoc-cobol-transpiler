#include <cctype>
#include <climits>
#include <filesystem>
#include <string>
#include <system_error>

#include "cblc_transpiler.hpp"
#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"
#include "transpiler_semantic_dump.hpp"

static t_transpiler_incremental_cache g_incremental_cache;
static int g_incremental_cache_ready = 0;

static int pipeline_emit_error(t_transpiler_context *context, const char *message)
{
    if (!context || !message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, FT_FAILURE, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static std::filesystem::path pipeline_normalize_path(const char *path)
{
    std::error_code error;
    std::filesystem::path fs_path;
    std::filesystem::path absolute_path;

    if (!path)
        return (std::filesystem::path());
    fs_path = std::filesystem::path(path);
    absolute_path = std::filesystem::absolute(fs_path, error);
    if (!error)
        return (absolute_path.lexically_normal());
    return (fs_path.lexically_normal());
}

static int pipeline_paths_equal(const char *lhs, const char *rhs)
{
    std::filesystem::path lhs_path;
    std::filesystem::path rhs_path;

    if (!lhs || !rhs)
        return (0);
    lhs_path = pipeline_normalize_path(lhs);
    rhs_path = pipeline_normalize_path(rhs);
    if (lhs_path == rhs_path)
        return (1);
    return (0);
}

static int pipeline_detect_source_conflict(const t_transpiler_context *context, const char *resolved_path,
    size_t *conflict_index)
{
    size_t index;

    if (conflict_index)
        *conflict_index = static_cast<size_t>(-1);
    if (!context || !resolved_path)
        return (0);
    index = 0;
    while (index < context->source_count)
    {
        if (pipeline_paths_equal(resolved_path, context->source_paths[index]))
        {
            if (conflict_index)
                *conflict_index = index;
            return (1);
        }
        index += 1;
    }
    return (0);
}

static const t_transpiler_standard_library_entry *pipeline_detect_standard_library_conflict(const char *resolved_path)
{
    const t_transpiler_standard_library_entry *entries;
    const char *filename;
    const char *cursor;
    size_t entry_count;
    size_t index;
    char candidate[TRANSPILE_FILE_PATH_MAX];

    if (!resolved_path)
        return (NULL);
    filename = resolved_path;
    cursor = resolved_path;
    while (*cursor != '\0')
    {
        if (*cursor == '/' || *cursor == '\\')
            filename = cursor + 1;
        cursor += 1;
    }
    if (filename[0] == '\0')
        return (NULL);
    entries = transpiler_standard_library_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        if (std::snprintf(candidate, sizeof(candidate), "%s.cob", entries[index].program_name) < 0)
            return (NULL);
        if (std::strncmp(filename, candidate, std::strlen(candidate) + 1) == 0)
            return (&entries[index]);
        index += 1;
    }
    return (NULL);
}

static int pipeline_select_cache_manifest(const t_transpiler_context *context, char *buffer, size_t buffer_size)
{
    const char *directory;
    size_t length;

    if (!buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    directory = NULL;
    if (context)
        directory = context->output_directory;
    if (directory && directory[0] != '\0')
    {
        if (std::snprintf(buffer, buffer_size, "%s/.ctoc-cache", directory) < 0)
            return (FT_FAILURE);
        length = std::strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (std::snprintf(buffer, buffer_size, ".ctoc-cache") < 0)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_initialize_incremental_cache(t_transpiler_context *context)
{
    char manifest_path[TRANSPILE_FILE_PATH_MAX];

    if (!context)
        return (FT_FAILURE);
    if (transpiler_incremental_cache_init(&g_incremental_cache) != FT_SUCCESS)
        return (FT_FAILURE);
    if (pipeline_select_cache_manifest(context, manifest_path, sizeof(manifest_path)) != FT_SUCCESS)
    {
        transpiler_incremental_cache_dispose(&g_incremental_cache);
        return (FT_FAILURE);
    }
    if (transpiler_incremental_cache_set_manifest(&g_incremental_cache, manifest_path) != FT_SUCCESS)
    {
        transpiler_incremental_cache_dispose(&g_incremental_cache);
        return (FT_FAILURE);
    }
    if (transpiler_incremental_cache_load(&g_incremental_cache) != FT_SUCCESS)
    {
        transpiler_incremental_cache_dispose(&g_incremental_cache);
        return (FT_FAILURE);
    }
    g_incremental_cache_ready = 1;
    return (FT_SUCCESS);
}

static void pipeline_finalize_incremental_cache(t_transpiler_context *context)
{
    if (!g_incremental_cache_ready)
        return ;
    if (transpiler_incremental_cache_save(&g_incremental_cache) != FT_SUCCESS && context)
        (void)transpiler_logging_emit(context, TRANSPILE_SEVERITY_WARNING, 0,
            "Unable to persist incremental cache manifest");
    transpiler_incremental_cache_dispose(&g_incremental_cache);
    g_incremental_cache_ready = 0;
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
                std::memcpy(new_buffer, buffer, length);
            cma_free(buffer);
            buffer = new_buffer;
            capacity = new_capacity;
        }
        if (bytes_read > 0)
        {
            std::memcpy(buffer + length, stack_buffer, bytes_read);
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
    length = std::strlen(text);
    if (runtime_file_write(&file, text, length) != FT_SUCCESS)
    {
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    if (runtime_file_close(&file) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static std::string pipeline_json_escape(const char *text)
{
    std::string escaped;

    if (!text)
        return (escaped);
    while (*text != '\0')
    {
        unsigned char value;

        value = static_cast<unsigned char>(*text);
        if (value == '\\')
            escaped += "\\\\";
        else if (value == '"')
            escaped += "\\\"";
        else if (value == '\n')
            escaped += "\\n";
        else if (value == '\r')
            escaped += "\\r";
        else if (value == '\t')
            escaped += "\\t";
        else if (value < 0x20)
            escaped += '?';
        else
            escaped += static_cast<char>(value);
        text += 1;
    }
    return (escaped);
}

static unsigned long long pipeline_hash_text(const char *text)
{
    unsigned long long hash;

    hash = 1469598103934665603ULL;
    if (!text)
        return (hash);
    while (*text != '\0')
    {
        hash ^= static_cast<unsigned char>(*text);
        hash *= 1099511628211ULL;
        text += 1;
    }
    return (hash);
}

static int pipeline_resolve_output_path(const t_transpiler_context *context,
    const char *target_path, char *buffer, size_t buffer_size);

static int pipeline_write_translation_manifest(const t_transpiler_context *context,
    const char *mode, const std::string &manifest)
{
    char manifest_path[TRANSPILE_FILE_PATH_MAX];
    const char *first_target;
    const char *separator;
    size_t directory_length;

    if (!context || !mode)
        return (FT_FAILURE);
    if (context->output_directory && context->output_directory[0] != '\0')
    {
        if (pipeline_resolve_output_path(context, "cblc.manifest.json", manifest_path,
                sizeof(manifest_path)) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        /* With explicit output paths, keep the manifest beside the outputs.
         * Falling back to the process working directory made test and build
         * invocations leave an unrelated root-level artifact behind. */
        if (!context->target_paths || context->target_count == 0
            || !context->target_paths[0])
            return (FT_FAILURE);
        first_target = context->target_paths[0];
        separator = std::strrchr(first_target, '/');
        {
            const char *backslash;

            backslash = std::strrchr(first_target, '\\');
            if (!separator || (backslash && backslash > separator))
                separator = backslash;
        }
        if (!separator)
            ft_strlcpy(manifest_path, "cblc.manifest.json", sizeof(manifest_path));
        else
        {
            directory_length = static_cast<size_t>(separator - first_target);
            if (directory_length == 0)
                directory_length = 1;
            if (directory_length + std::strlen("/cblc.manifest.json") + 1
                > sizeof(manifest_path))
                return (FT_FAILURE);
            std::memcpy(manifest_path, first_target, directory_length);
            manifest_path[directory_length] = '\0';
            if (directory_length == 1 && (manifest_path[0] == '/' || manifest_path[0] == '\\'))
                ft_strlcpy(manifest_path + directory_length, "cblc.manifest.json",
                    sizeof(manifest_path) - directory_length);
            else
                std::strncat(manifest_path, "/cblc.manifest.json",
                    sizeof(manifest_path) - std::strlen(manifest_path) - 1);
        }
    }
    return (pipeline_write_file(manifest_path, manifest.c_str()));
}

static int pipeline_manifest_contains_id(const std::string &manifest, const char *identifier)
{
    std::string needle;

    if (!identifier)
        return (0);
    needle = "\"id\": \"";
    needle += pipeline_json_escape(identifier);
    needle += "\"";
    return (manifest.find(needle) != std::string::npos);
}

static std::string pipeline_manifest_dependencies(const char *generated_text,
    const char *self_id)
{
    const t_transpiler_standard_library_entry *entries;
    size_t entry_count;
    size_t index;
    int has_dependency;
    std::string dependencies;

    dependencies = "[";
    has_dependency = 0;
    if (!generated_text)
        return (dependencies + "]");
    entries = transpiler_standard_library_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        if ((!self_id || std::strcmp(entries[index].program_name, self_id) != 0)
            && std::strstr(generated_text, entries[index].program_name))
        {
            if (has_dependency)
                dependencies += ", ";
            dependencies += "\"";
            dependencies += pipeline_json_escape(entries[index].program_name);
            dependencies += "\"";
            has_dependency = 1;
        }
        index += 1;
    }
    dependencies += "]";
    return (dependencies);
}

/* Standard-library programs are deployment artifacts, so an existing file
 * must never be replaced merely because a second compilation generated it.
 * Generated character-buffer variants are compatible when the existing
 * declaration is at least as wide as the new one.  Keeping the wider file
 * makes a shared deployment directory safe to populate from several builds.
 */
static size_t pipeline_standard_library_max_char_width(const char *text)
{
    size_t maximum;

    maximum = 0;
    if (!text)
        return (maximum);
    while (*text != '\0')
    {
        const char *marker;
        const char *cursor;
        size_t value;

        marker = std::strstr(text, "PIC X(");
        if (!marker)
            break ;
        cursor = marker + std::strlen("PIC X(");
        value = 0;
        while (std::isdigit(static_cast<unsigned char>(*cursor)))
        {
            value = value * 10 + static_cast<size_t>(*cursor - '0');
            cursor += 1;
        }
        if (*cursor == ')' && value > maximum)
            maximum = value;
        text = marker + 1;
    }
    return (maximum);
}

static int pipeline_write_standard_library_artifact(t_transpiler_context *context,
    const t_transpiler_standard_library_entry *entry, const char *resolved_path,
    char *generated_text, char **selected_text)
{
    std::error_code error;
    char *existing_text;
    size_t existing_width;
    size_t generated_width;
    char expected_program[TRANSPILE_IDENTIFIER_MAX];

    if (!context || !entry || !resolved_path || !generated_text || !selected_text)
        return (FT_FAILURE);
    *selected_text = generated_text;
    existing_text = NULL;
    if (std::filesystem::exists(std::filesystem::path(resolved_path), error))
    {
        if (error || pipeline_read_file(resolved_path, &existing_text) != FT_SUCCESS
            || !existing_text)
            return (FT_FAILURE);
        if (std::snprintf(expected_program, sizeof(expected_program),
                "PROGRAM-ID. %s.", entry->program_name) < 0
            || !std::strstr(existing_text, expected_program))
        {
            cma_free(existing_text);
            (void)pipeline_emit_error(context,
                "Refusing to overwrite a non-standard-library file with a standard-library artifact");
            return (FT_FAILURE);
        }
        existing_width = pipeline_standard_library_max_char_width(existing_text);
        generated_width = pipeline_standard_library_max_char_width(generated_text);
        if (existing_width >= generated_width)
        {
            *selected_text = existing_text;
            return (FT_SUCCESS);
        }
        cma_free(existing_text);
    }
    if (pipeline_write_file(resolved_path, generated_text) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_emit_required_standard_library(t_transpiler_context *context,
    const char *generated_text, std::string &manifest, int *manifest_has_artifact)
{
    const t_transpiler_standard_library_entry *entries;
    std::string dependency_text;
    size_t entry_count;
    size_t index;

    if (!context || !generated_text || !manifest_has_artifact)
        return (FT_FAILURE);
    dependency_text = generated_text;
    entries = transpiler_standard_library_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        char filename[TRANSPILE_FILE_PATH_MAX];
        char resolved_path[TRANSPILE_FILE_PATH_MAX];
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        char *program_text;

        if ((!context->emit_all_standard_library
                && dependency_text.find(entries[index].program_name) == std::string::npos)
            || pipeline_manifest_contains_id(manifest, entries[index].program_name))
        {
            index += 1;
            continue ;
        }
        if (std::snprintf(filename, sizeof(filename), "%s.cob", entries[index].program_name) < 0)
            return (FT_FAILURE);
        program_text = NULL;
        if (entries[index].generator(&program_text) != FT_SUCCESS || !program_text)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to generate required standard library program '%s'",
                    entries[index].program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            if (program_text)
                cma_free(program_text);
            return (FT_FAILURE);
        }
        /* A generated standard-library program may call another generated
         * program. Keep its source in the dependency scan so the same pass
         * computes the transitive closure instead of only the target's direct
         * references. */
        dependency_text += "\n";
        dependency_text += program_text;
        {
            char *selected_text;

            selected_text = NULL;
            if (pipeline_resolve_output_path(context, filename, resolved_path,
                    sizeof(resolved_path)) != FT_SUCCESS
                || pipeline_write_standard_library_artifact(context, &entries[index],
                    resolved_path, program_text, &selected_text) != FT_SUCCESS)
            {
                if (selected_text && selected_text != program_text)
                    cma_free(selected_text);
                if (std::snprintf(message, sizeof(message),
                        "Unable to write required standard library program '%s'",
                        entries[index].program_name) >= 0)
                    (void)pipeline_emit_error(context, message);
                if (selected_text != program_text && program_text)
                    cma_free(program_text);
                return (FT_FAILURE);
            }
            if (selected_text != program_text)
            {
                cma_free(program_text);
                program_text = selected_text;
            }
        }
        if (*manifest_has_artifact)
            manifest += ",\n";
        manifest += "    {\n      \"kind\": \"standard-library\",\n      \"id\": \"";
        manifest += pipeline_json_escape(entries[index].program_name);
        manifest += "\",\n      \"qualified_name\": \"";
        manifest += pipeline_json_escape(entries[index].qualified_name);
        manifest += "\",\n      \"path\": \"";
        manifest += pipeline_json_escape(filename);
        manifest += "\",\n      \"hash\": \"fnv1a64:";
        {
            char hash_text[32];

            if (std::snprintf(hash_text, sizeof(hash_text), "%016llx",
                    pipeline_hash_text(program_text)) < 0)
            {
                cma_free(program_text);
                return (FT_FAILURE);
            }
            manifest += hash_text;
        }
        manifest += "\",\n      \"dependencies\": ";
        manifest += pipeline_manifest_dependencies(program_text, entries[index].program_name);
        manifest += "\n    }";
        *manifest_has_artifact = 1;
        cma_free(program_text);
        /* Restart the catalog so dependencies that appear before this entry
         * are discovered as well. Manifest membership guarantees termination. */
        index = 0;
    }
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
        separator = std::strrchr(target_path, '/');
        if (!separator)
            separator = std::strrchr(target_path, '\\');
        if (separator && separator[1] != '\0')
            filename = separator + 1;
        if (std::snprintf(buffer, buffer_size, "%s/%s", directory, filename) < 0)
            return (FT_FAILURE);
        length = std::strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    length = std::strlen(target_path);
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
        length = std::strlen(base);
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
        if (std::snprintf(buffer, buffer_size, "%s/%s.dot", directory, base) < 0)
            return (FT_FAILURE);
        length = std::strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!resolved_output_path)
        return (FT_FAILURE);
    length = std::strlen(resolved_output_path);
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

static int pipeline_build_copybook_graph_output_path(const t_transpiler_context *context,
    const char *input_path, const char *resolved_output_path, char *buffer, size_t buffer_size)
{
    const char *directory;
    const char *filename;
    const char *cursor;
    size_t length;

    if (!context || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    directory = transpiler_context_get_copybook_graph_directory(context);
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
        length = std::strlen(base);
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
        if (std::snprintf(buffer, buffer_size, "%s/%s.copybooks.dot", directory, base) < 0)
            return (FT_FAILURE);
        length = std::strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!resolved_output_path)
        return (FT_FAILURE);
    length = std::strlen(resolved_output_path);
    if (length + 15 > buffer_size)
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
    if (ft_strlcat(buffer, ".copybooks.dot", buffer_size) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_emit_semantic_ir_snapshots(t_transpiler_context *context, const char *input_path,
    const char *resolved_output_path)
{
    return (transpiler_semantic_dump_emit(context, input_path, resolved_output_path));
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
    char copybook_graph_path[TRANSPILE_FILE_PATH_MAX];
    char *source_text;
    char *cblc_text;
    char *formatted_text;
    t_parser parser;
    t_ast_node *program;
    int status;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    unsigned long long copybook_signature;
    const t_transpiler_standard_library_entry *stdlib_entry;

    if (!context || !input_path || !output_path)
        return (FT_FAILURE);
    source_text = NULL;
    cblc_text = NULL;
    formatted_text = NULL;
    program = NULL;
    status = FT_FAILURE;
    ast_path[0] = '\0';
    copybook_graph_path[0] = '\0';
    context->source_path = input_path;
    context->target_path = output_path;
    context->active_source_text = NULL;
    context->active_source_length = 0;
    if (pipeline_resolve_output_path(context, output_path, resolved_path, sizeof(resolved_path)) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Unable to resolve output path for '%s'", output_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    size_t conflict_index;

    conflict_index = static_cast<size_t>(-1);
    if (pipeline_detect_source_conflict(context, resolved_path, &conflict_index))
    {
        if (conflict_index != static_cast<size_t>(-1)
            && conflict_index < context->source_count)
        {
            if (std::snprintf(message, sizeof(message),
                    "Output path '%s' for source '%s' matches input source '%s'; refusing to overwrite",
                    resolved_path, input_path, context->source_paths[conflict_index]) >= 0)
                (void)pipeline_emit_error(context, message);
        }
        else if (std::snprintf(message, sizeof(message),
                     "Output path '%s' for source '%s' matches an input source; refusing to overwrite",
                     resolved_path, input_path)
            >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    copybook_signature = transpiler_context_compute_copybook_signature(context);
    if (g_incremental_cache_ready)
    {
        int should_skip;

        should_skip = 0;
        if (transpiler_incremental_cache_should_skip(&g_incremental_cache, input_path, resolved_path,
                copybook_signature,
                pipeline_hash_text("CBLC-TEMPLATE-TYPE-SUBSTITUTION@6"), &should_skip)
            != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message), "Unable to query incremental cache for '%s'", input_path) >= 0)
                (void)transpiler_logging_emit(context, TRANSPILE_SEVERITY_WARNING, 0, message);
        }
        else if (should_skip)
        {
            if (std::snprintf(message, sizeof(message), "Skipping '%s'; cached output is current", input_path) >= 0)
                (void)transpiler_logging_emit(context, TRANSPILE_SEVERITY_INFO, 0, message);
            status = FT_SUCCESS;
            goto cleanup;
        }
    }
    if (pipeline_read_file(input_path, &source_text) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Unable to read input file '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    context->active_source_text = source_text;
    context->active_source_length = std::strlen(source_text);
    transpiler_context_clear_comments(context);
    parser_init_with_context(&parser, source_text, context);
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        if (std::snprintf(message, sizeof(message), "Failed to parse COBOL source '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    parser_dispose(&parser);
    transpiler_context_reset_unit_state(context);
    context->active_source_text = source_text;
    context->active_source_length = std::strlen(source_text);
    if (transpiler_semantics_analyze_program(context, program) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Semantic analysis failed for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (transpiler_context_get_semantic_diff_enabled(context))
    {
        if (pipeline_emit_semantic_ir_snapshots(context, input_path, resolved_path) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to emit semantic IR snapshots for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
    }
    if (transpiler_cobol_program_to_cblc(context, program, &cblc_text) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Unable to generate CBL-C for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_apply_cblc_layout(cblc_text, context->layout_mode, context->format_mode,
            &formatted_text) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Failed to format generated CBL-C for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (transpiler_validate_generated_cblc(formatted_text) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Generated CBL-C failed validation for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (transpiler_context_get_ast_dump_enabled(context))
    {
        if (pipeline_build_ast_output_path(context, input_path, resolved_path, ast_path,
                sizeof(ast_path)) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to select AST visualization path for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
        if (pipeline_prepare_output_directory(ast_path) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to prepare AST visualization path '%s'", ast_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
        if (transpiler_ast_visualize_program(program, ast_path) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to emit AST visualization for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
    }
    if (transpiler_context_get_copybook_graph_enabled(context))
    {
        if (pipeline_build_copybook_graph_output_path(context, input_path, resolved_path,
                copybook_graph_path, sizeof(copybook_graph_path)) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to select copybook graph path for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
        if (pipeline_prepare_output_directory(copybook_graph_path) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to prepare copybook graph path '%s'", copybook_graph_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
        if (transpiler_copybook_graph_emit(context, program, input_path, copybook_graph_path) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to emit copybook graph for '%s'", input_path) >= 0)
                (void)pipeline_emit_error(context, message);
            goto cleanup;
        }
    }
    stdlib_entry = pipeline_detect_standard_library_conflict(resolved_path);
    if (stdlib_entry)
    {
        if (std::snprintf(message, sizeof(message),
                "Output file '%s' for source '%s' matches standard library program '%s'; refusing to emit",
                resolved_path, input_path, stdlib_entry->program_name) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_write_file(resolved_path, formatted_text) != FT_SUCCESS)
    {
        if (std::snprintf(message, sizeof(message), "Failed to write output file '%s'", resolved_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (g_incremental_cache_ready)
    {
        const char *record_ast_path;

        record_ast_path = NULL;
        if (ast_path[0] != '\0')
            record_ast_path = ast_path;
        copybook_signature = transpiler_context_compute_copybook_signature(context);
        if (transpiler_incremental_cache_record(&g_incremental_cache, input_path, resolved_path, record_ast_path,
                copybook_signature,
            pipeline_hash_text("CBLC-TEMPLATE-TYPE-SUBSTITUTION@6"))
            != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message), "Failed to update incremental cache for '%s'", input_path) >= 0)
                (void)transpiler_logging_emit(context, TRANSPILE_SEVERITY_WARNING, 0, message);
        }
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
    std::string manifest;
    size_t entry_count;
    size_t index;
    int manifest_has_artifact;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    entries = transpiler_standard_library_get_entries(&entry_count);
    manifest = "{\n  \"schema_version\": 1,\n  \"mode\": \"standard-library\",\n  \"template_contract\": \"CBLC-TEMPLATE-TYPE-SUBSTITUTION@6\",\n  \"artifacts\": [\n";
    manifest_has_artifact = 0;
    index = 0;
    while (index < entry_count)
    {
        char filename[TRANSPILE_FILE_PATH_MAX];
        char resolved_path[TRANSPILE_FILE_PATH_MAX];
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        char *program_text;
        int status;

        if (std::snprintf(filename, sizeof(filename), "%s.cob", entries[index].program_name) < 0)
            return (FT_FAILURE);
        program_text = NULL;
        status = entries[index].generator(&program_text);
        if (status != FT_SUCCESS || !program_text)
        {
            if (std::snprintf(message, sizeof(message),
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
            if (std::snprintf(message, sizeof(message),
                    "Generated standard library program '%s' failed validation",
                    entries[index].program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            cma_free(program_text);
            return (FT_FAILURE);
        }
        if (pipeline_resolve_output_path(context, filename, resolved_path, sizeof(resolved_path)) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to resolve output path for standard library program '%s'",
                    entries[index].program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            cma_free(program_text);
            return (FT_FAILURE);
        }
        {
            char *selected_text;

            selected_text = NULL;
            if (pipeline_write_standard_library_artifact(context, &entries[index],
                    resolved_path, program_text, &selected_text) != FT_SUCCESS)
            {
                if (std::snprintf(message, sizeof(message),
                        "Unable to write standard library program '%s' to '%s'",
                        entries[index].program_name, resolved_path) >= 0)
                    (void)pipeline_emit_error(context, message);
                if (selected_text && selected_text != program_text)
                    cma_free(selected_text);
                if (program_text)
                    cma_free(program_text);
                return (FT_FAILURE);
            }
            if (selected_text != program_text)
            {
                cma_free(program_text);
                program_text = selected_text;
            }
        }
        if (manifest_has_artifact)
            manifest += ",\n";
        manifest += "    {\n      \"kind\": \"standard-library\",\n      \"id\": \"";
        manifest += pipeline_json_escape(entries[index].program_name);
        manifest += "\",\n      \"qualified_name\": \"";
        manifest += pipeline_json_escape(entries[index].qualified_name);
        manifest += "\",\n      \"path\": \"";
        manifest += pipeline_json_escape(filename);
        manifest += "\",\n      \"hash\": \"fnv1a64:";
        {
            char hash_text[32];

            if (std::snprintf(hash_text, sizeof(hash_text), "%016llx",
                    pipeline_hash_text(program_text)) < 0)
            {
                cma_free(program_text);
                return (FT_FAILURE);
            }
            manifest += hash_text;
        }
        {
            char width_text[32];

            if (std::snprintf(width_text, sizeof(width_text), "%zu",
                    pipeline_standard_library_max_char_width(program_text)) < 0)
            {
                cma_free(program_text);
                return (FT_FAILURE);
            }
            manifest += "\",\n      \"char_width\": ";
            manifest += width_text;
            manifest += ",\n      \"dependencies\": ";
        }
        manifest += pipeline_manifest_dependencies(program_text, entries[index].program_name);
        manifest += "\n    }";
        manifest_has_artifact = 1;
        cma_free(program_text);
        index += 1;
    }
    manifest += "\n  ]\n}\n";
    {
        char manifest_path[TRANSPILE_FILE_PATH_MAX];
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        if (pipeline_resolve_output_path(context, "cblc.manifest.json", manifest_path,
                sizeof(manifest_path)) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to resolve standard-library manifest output path") >= 0)
                (void)pipeline_emit_error(context, message);
            return (FT_FAILURE);
        }
        if (pipeline_write_file(manifest_path, manifest.c_str()) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Unable to write standard-library manifest to '%s'", manifest_path) >= 0)
                (void)pipeline_emit_error(context, message);
            return (FT_FAILURE);
        }
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
    const char *extension;
    size_t length;

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
    extension = NULL;
    cursor = path;
    while (*cursor != '\0')
    {
        if (*cursor == '.')
            extension = cursor;
        cursor += 1;
    }
    length = std::strlen(path);
    if (extension && extension != path)
        length = static_cast<size_t>(extension - path);
    if (length >= buffer_size)
        length = buffer_size - 1;
    std::memcpy(buffer, path, length);
    buffer[length] = '\0';
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

static int pipeline_unit_has_declarations(const t_cblc_translation_unit *unit)
{
    if (!unit)
        return (0);
    if (unit->function_count > 0)
        return (1);
    if (unit->struct_type_count > 0)
        return (1);
    if (unit->data_count > 0)
        return (1);
    if (unit->copy_include_count > 0)
        return (1);
    return (0);
}

static void pipeline_assign_unit_program_name(t_cblc_translation_unit *unit, const char *module_name)
{
    size_t index;

    if (!unit || !module_name || module_name[0] == '\0')
        return ;
    if (unit->function_count > 0 && unit->program_name[0] != '\0')
        return ;
    ft_strlcpy(unit->program_name, module_name, sizeof(unit->program_name));
    index = 0;
    while (unit->program_name[index] != '\0')
    {
        if (unit->program_name[index] >= 'a' && unit->program_name[index] <= 'z')
            unit->program_name[index] = static_cast<char>(unit->program_name[index] - 'a' + 'A');
        else if (!std::isalnum(static_cast<unsigned char>(unit->program_name[index])))
            unit->program_name[index] = '-';
        index += 1;
    }
}

static std::string pipeline_module_dependencies_json(const t_transpiler_context *context,
    const t_cblc_translation_unit *unit, char **sources,
    char (*module_names)[TRANSPILE_MODULE_NAME_MAX])
{
    std::string dependencies;
    size_t import_index;

    dependencies = "[";
    if (!context || !unit || !sources || !module_names)
        return (dependencies + "]");
    import_index = 0;
    while (import_index < unit->import_count)
    {
        size_t source_index;
        const char *dependency_name;
        const char *dependency_source;
        char hash_text[32];

        source_index = 0;
        while (source_index < context->source_count)
        {
            if (pipeline_paths_equal(unit->imports[import_index].path,
                    context->source_paths[source_index])
                || std::strncmp(unit->imports[import_index].path,
                    module_names[source_index], TRANSPILE_MODULE_NAME_MAX) == 0)
                break;
            source_index += 1;
        }
        dependency_name = unit->imports[import_index].path;
        dependency_source = NULL;
        if (source_index < context->source_count)
        {
            dependency_name = module_names[source_index];
            dependency_source = sources[source_index];
        }
        if (import_index > 0)
            dependencies += ",";
        dependencies += "{\"module\":\"";
        dependencies += pipeline_json_escape(dependency_name);
        dependencies += "\",\"source_hash\":\"fnv1a64:";
        if (std::snprintf(hash_text, sizeof(hash_text), "%016llx",
                pipeline_hash_text(dependency_source)) >= 0)
            dependencies += hash_text;
        dependencies += "\"}";
        import_index += 1;
    }
    dependencies += "]";
    return (dependencies);
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
    std::string manifest;
    int manifest_has_artifact;
    int status;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    file_count = context->source_count;
    if (file_count == 0)
        return (FT_SUCCESS);
    manifest = "{\n  \"schema_version\": 1,\n  \"mode\": \"cblc-to-cobol\",\n  \"runtime\": \"external-standard-library\",\n  \"template_contract\": \"CBLC-TEMPLATE-TYPE-SUBSTITUTION@6\",\n  \"artifacts\": [\n";
    manifest_has_artifact = 0;
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

        module_indices[index] = static_cast<size_t>(-1);
        unit = &units[index];
        cblc_translation_unit_init(unit);
        if (pipeline_read_file(context->source_paths[index], &sources[index]) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message), "Unable to read input file '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        pipeline_choose_module_name(context->source_paths[index], unit, module_name,
            sizeof(module_name));
        ft_strlcpy(module_names[index], module_name, TRANSPILE_MODULE_NAME_MAX);
        if (transpiler_context_register_module(context, module_name, context->source_paths[index])
            != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message), "Failed to register module for '%s'",
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
        if (transpiler_context_scan_imports_for_module(context, module_name, sources[index])
            != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Failed to scan imports for module '%s'", module_name) >= 0)
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
    index = 0;
    while (index < order_count)
    {
        size_t module_index;
        size_t source_index;
        t_cblc_translation_unit *unit;

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
        unit = &units[source_index];
        if (cblc_import_translation_unit_type_stubs(context, module_names[source_index],
                unit) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Failed to import type stubs for module '%s'", module_names[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (cblc_import_translation_unit_function_stubs(context,
                module_names[source_index], unit) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Failed to import function template stubs for module '%s'",
                    module_names[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (cblc_parse_translation_unit(sources[source_index], unit) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message), "Failed to parse CBL-C source '%s'",
                    context->source_paths[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        pipeline_assign_unit_program_name(unit, module_names[source_index]);
        if (!pipeline_unit_has_declarations(unit))
        {
            if (std::snprintf(message, sizeof(message),
                    "CBL-C source '%s' does not declare any transpilable content;",
                    context->source_paths[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        else if (unit->function_count > 0)
        {
            size_t entry_index;

            entry_index = unit->entry_function_index;
            if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
                entry_index = 0;
            if (unit->functions[entry_index].return_kind != CBLC_FUNCTION_RETURN_VOID
                && !unit->functions[entry_index].saw_return)
            {
                if (std::snprintf(message, sizeof(message),
                        "CBL-C source '%s' is missing a terminating return;",
                        context->source_paths[source_index]) >= 0)
                    (void)pipeline_emit_error(context, message);
                status = FT_FAILURE;
                goto cleanup;
            }
        }
        if (cblc_register_translation_unit_exports(context, module_names[source_index],
                unit) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message),
                    "Failed to register exports for module '%s'", module_names[source_index]) >= 0)
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
            if (std::snprintf(message, sizeof(message),
                    "Failed to resolve function calls for module '%s'", module_names[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
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
            if (std::snprintf(message, sizeof(message), "Unable to resolve output path for '%s'",
                    context->target_paths[source_index]) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        size_t conflict_index;
        const t_transpiler_standard_library_entry *stdlib_entry;

        conflict_index = static_cast<size_t>(-1);
        if (pipeline_detect_source_conflict(context, resolved_path, &conflict_index))
        {
            if (conflict_index != static_cast<size_t>(-1)
                && conflict_index < context->source_count)
            {
                if (std::snprintf(message, sizeof(message),
                        "Output path '%s' for source '%s' matches input source '%s'; refusing to overwrite",
                        resolved_path, ordered_source_paths[index], context->source_paths[conflict_index]) >= 0)
                    (void)pipeline_emit_error(context, message);
            }
            else if (std::snprintf(message, sizeof(message),
                         "Output path '%s' for source '%s' matches an input source; refusing to overwrite",
                         resolved_path, ordered_source_paths[index])
                >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        stdlib_entry = pipeline_detect_standard_library_conflict(resolved_path);
        if (stdlib_entry)
        {
            if (std::snprintf(message, sizeof(message),
                    "Output file '%s' for source '%s' matches standard library program '%s'; refusing to emit",
                    resolved_path, ordered_source_paths[index], stdlib_entry->program_name) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (pipeline_write_file(resolved_path, parallel_results[index].text) != FT_SUCCESS)
        {
            if (std::snprintf(message, sizeof(message), "Failed to write output file '%s'",
                    resolved_path) >= 0)
                (void)pipeline_emit_error(context, message);
            status = FT_FAILURE;
            goto cleanup;
        }
        if (pipeline_emit_required_standard_library(context, parallel_results[index].text,
                manifest, &manifest_has_artifact) != FT_SUCCESS)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        if (manifest_has_artifact)
            manifest += ",\n";
        manifest += "    {\n      \"kind\": \"target\",\n      \"id\": \"";
        manifest += pipeline_json_escape(ordered_units[index]->program_name);
        manifest += "\",\n      \"path\": \"";
        manifest += pipeline_json_escape(context->target_paths[source_index]);
        manifest += "\",\n      \"source_hash\": \"fnv1a64:";
        {
            char hash_text[32];

            if (std::snprintf(hash_text, sizeof(hash_text), "%016llx",
                    pipeline_hash_text(sources[source_index])) < 0)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            manifest += hash_text;
        }
        manifest += "\",\n      \"module_dependencies\": ";
        manifest += pipeline_module_dependencies_json(context, ordered_units[index],
            sources, module_names);
        manifest += ",\n      \"hash\": \"fnv1a64:";
        {
            char hash_text[32];

            if (std::snprintf(hash_text, sizeof(hash_text), "%016llx",
                    pipeline_hash_text(parallel_results[index].text)) < 0)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            manifest += hash_text;
        }
        manifest += "\",\n      \"dependencies\": ";
        manifest += pipeline_manifest_dependencies(parallel_results[index].text,
            ordered_units[index]->program_name);
        manifest += "\n    }";
        manifest_has_artifact = 1;
        if (parallel_results[index].text)
        {
            cma_free(parallel_results[index].text);
            parallel_results[index].text = NULL;
        }
        module_indices[source_index] = static_cast<size_t>(-1);
        index += 1;
    }
    manifest += "\n  ]\n}\n";
    if (pipeline_write_translation_manifest(context, "cblc-to-cobol", manifest) != FT_SUCCESS)
    {
        (void)pipeline_emit_error(context, "Failed to write COBOL translation manifest");
        status = FT_FAILURE;
        goto cleanup;
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
    if (pipeline_initialize_incremental_cache(&context) != FT_SUCCESS)
    {
        transpiler_incremental_cache_dispose(&g_incremental_cache);
        g_incremental_cache_ready = 0;
        (void)transpiler_logging_emit(&context, TRANSPILE_SEVERITY_WARNING, 0,
            "Incremental cache disabled; manifest load failed");
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
    else
    {
        (void)pipeline_emit_error(&context, "Unsupported translation direction");
        status = FT_FAILURE;
        goto cleanup;
    }
    status = transpiler_pipeline_execute(&pipeline, &context);
cleanup:
    pipeline_finalize_incremental_cache(&context);
    transpiler_logging_flush(&context);
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    transpiler_cli_options_dispose(&options);
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
