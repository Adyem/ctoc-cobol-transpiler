#include "transpiler_semantic_dump.hpp"

#include <filesystem>
#include <system_error>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int transpiler_semantic_dump_prepare_directory(const char *path)
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

static int transpiler_semantic_dump_write_file(const char *path, const char *text)
{
    t_runtime_file file;
    size_t length;

    if (!path || !text)
        return (FT_FAILURE);
    if (transpiler_semantic_dump_prepare_directory(path) != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_file_init(&file);
    if (runtime_file_open_write(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    length = ft_strlen_size_t(text);
    if (runtime_file_write(&file, text, length) != FT_SUCCESS)
    {
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    if (runtime_file_close(&file) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static void transpiler_semantic_dump_strip_extension(char *buffer)
{
    size_t length;

    if (!buffer)
        return ;
    length = ft_strlen_size_t(buffer);
    while (length > 0)
    {
        if (buffer[length - 1] == '.')
        {
            buffer[length - 1] = '\0';
            return ;
        }
        if (buffer[length - 1] == '/' || buffer[length - 1] == '\\')
            return ;
        length -= 1;
    }
}

int transpiler_semantic_dump_build_output_path(const t_transpiler_context *context,
    const char *input_path, const char *resolved_output_path, const char *suffix,
    char *buffer, size_t buffer_size)
{
    const char *directory;
    const char *filename;
    const char *cursor;

    if (!context || !suffix || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    directory = transpiler_context_get_semantic_diff_directory(context);
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
        transpiler_semantic_dump_strip_extension(base);
        if (base[0] == '\0')
            ft_strlcpy(base, "program", sizeof(base));
        if (pf_snprintf(buffer, buffer_size, "%s/%s.%s.txt", directory, base, suffix) < 0)
            return (FT_FAILURE);
        if (ft_strlen_size_t(buffer) + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!resolved_output_path)
        return (FT_FAILURE);
    if (ft_strlcpy(buffer, resolved_output_path, buffer_size) >= buffer_size)
        return (FT_FAILURE);
    transpiler_semantic_dump_strip_extension(buffer);
    if (ft_strlcat(buffer, ".", buffer_size) >= buffer_size)
        return (FT_FAILURE);
    if (ft_strlcat(buffer, suffix, buffer_size) >= buffer_size)
        return (FT_FAILURE);
    if (ft_strlcat(buffer, ".txt", buffer_size) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_semantic_dump_emit(t_transpiler_context *context, const char *input_path,
    const char *resolved_output_path)
{
    char before_path[TRANSPILE_FILE_PATH_MAX];
    char after_path[TRANSPILE_FILE_PATH_MAX];
    const char *before;
    const char *after;

    if (!context)
        return (FT_FAILURE);
    before = transpiler_context_get_semantic_snapshot_before(context);
    after = transpiler_context_get_semantic_snapshot_after(context);
    if (!before && !after)
        return (FT_SUCCESS);
    before_path[0] = '\0';
    after_path[0] = '\0';
    if (before)
    {
        if (transpiler_semantic_dump_build_output_path(context, input_path,
                resolved_output_path, "semantic.before", before_path,
                sizeof(before_path)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_semantic_dump_write_file(before_path, before) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (after)
    {
        if (transpiler_semantic_dump_build_output_path(context, input_path,
                resolved_output_path, "semantic.after", after_path,
                sizeof(after_path)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_semantic_dump_write_file(after_path, after) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}
