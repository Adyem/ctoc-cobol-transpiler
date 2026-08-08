#include "cblc_transpiler.hpp"

static int runtime_file_validate(t_runtime_file *file)
{
    if (!file)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_file_has_stream(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (file->stream)
        return (FT_SUCCESS);
    return (FT_FAILURE);
}

static int runtime_file_prepare_open(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_file_has_stream(file) == FT_SUCCESS)
    {
        if (runtime_file_close(file) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

void runtime_file_init(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return ;
    file->stream = NULL;
}

int runtime_file_open_read(t_runtime_file *file, const char *path)
{
    if (runtime_file_prepare_open(file) != FT_SUCCESS || !path)
        return (FT_FAILURE);
    file->stream = std::fopen(path, "rb");
    return (file->stream ? FT_SUCCESS : FT_FAILURE);
}

int runtime_file_open_write(t_runtime_file *file, const char *path)
{
    if (runtime_file_prepare_open(file) != FT_SUCCESS || !path)
        return (FT_FAILURE);
    file->stream = std::fopen(path, "wb");
    return (file->stream ? FT_SUCCESS : FT_FAILURE);
}

int runtime_file_read(t_runtime_file *file, char *buffer, size_t buffer_size, size_t *bytes_read)
{
    size_t result;

    if (runtime_file_has_stream(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    result = std::fread(buffer, sizeof(char), buffer_size - 1, file->stream);
    if (result == 0 && std::ferror(file->stream))
        return (FT_FAILURE);
    buffer[result] = '\0';
    if (bytes_read)
        *bytes_read = result;
    return (FT_SUCCESS);
}

int runtime_file_write(t_runtime_file *file, const char *buffer, size_t length)
{
    size_t offset;
    size_t result;

    if (runtime_file_has_stream(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    offset = 0;
    while (offset < length)
    {
        result = std::fwrite(buffer + offset, sizeof(char), length - offset, file->stream);
        if (result == 0)
            return (FT_FAILURE);
        offset += static_cast<size_t>(result);
    }
    return (FT_SUCCESS);
}

int runtime_file_close(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!file->stream)
        return (FT_SUCCESS);
    if (std::fclose(file->stream) != 0)
        return (FT_FAILURE);
    file->stream = NULL;
    return (FT_SUCCESS);
}
