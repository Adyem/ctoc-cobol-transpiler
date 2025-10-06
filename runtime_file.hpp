#pragma once

#include <cstddef>

#include "libft/Libft/libft.hpp"

typedef struct s_runtime_file
{
    int descriptor;
}   t_runtime_file;

void runtime_file_init(t_runtime_file *file);
int runtime_file_open_read(t_runtime_file *file, const char *path);
int runtime_file_open_write(t_runtime_file *file, const char *path);
int runtime_file_read(t_runtime_file *file, char *buffer, size_t buffer_size, size_t *bytes_read);
int runtime_file_write(t_runtime_file *file, const char *buffer, size_t length);
int runtime_file_close(t_runtime_file *file);
