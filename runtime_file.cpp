#include "runtime_file.hpp"

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

static int runtime_file_validate(t_runtime_file *file)
{
    if (!file)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_file_has_descriptor(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (file->descriptor >= 0)
        return (FT_SUCCESS);
    return (FT_FAILURE);
}

static int runtime_file_prepare_open(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_file_has_descriptor(file) == FT_SUCCESS)
    {
        if (runtime_file_close(file) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int runtime_file_open_internal(t_runtime_file *file, const char *path, int flags, mode_t mode, int use_mode)
{
    int descriptor;

    if (runtime_file_prepare_open(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!path)
        return (FT_FAILURE);
    if (use_mode)
        descriptor = open(path, flags, mode);
    else
        descriptor = open(path, flags);
    if (descriptor < 0)
        return (FT_FAILURE);
    file->descriptor = descriptor;
    return (FT_SUCCESS);
}

void runtime_file_init(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return ;
    file->descriptor = -1;
}

int runtime_file_open_read(t_runtime_file *file, const char *path)
{
    return (runtime_file_open_internal(file, path, O_RDONLY, 0, 0));
}

int runtime_file_open_write(t_runtime_file *file, const char *path)
{
    return (runtime_file_open_internal(file, path, O_WRONLY | O_CREAT | O_TRUNC, 0644, 1));
}

int runtime_file_read(t_runtime_file *file, char *buffer, size_t buffer_size, size_t *bytes_read)
{
    ssize_t result;

    if (runtime_file_has_descriptor(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    result = read(file->descriptor, buffer, buffer_size - 1);
    if (result < 0)
        return (FT_FAILURE);
    buffer[result] = '\0';
    if (bytes_read)
        *bytes_read = static_cast<size_t>(result);
    return (FT_SUCCESS);
}

int runtime_file_write(t_runtime_file *file, const char *buffer, size_t length)
{
    size_t offset;
    ssize_t result;

    if (runtime_file_has_descriptor(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    offset = 0;
    while (offset < length)
    {
        result = write(file->descriptor, buffer + offset, length - offset);
        if (result < 0)
            return (FT_FAILURE);
        offset += static_cast<size_t>(result);
    }
    return (FT_SUCCESS);
}

int runtime_file_close(t_runtime_file *file)
{
    if (runtime_file_validate(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (file->descriptor < 0)
        return (FT_SUCCESS);
    if (close(file->descriptor) != 0)
        return (FT_FAILURE);
    file->descriptor = -1;
    return (FT_SUCCESS);
}
