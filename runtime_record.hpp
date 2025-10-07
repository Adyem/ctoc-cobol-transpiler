#ifndef RUNTIME_RECORD_HPP
#define RUNTIME_RECORD_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"

typedef struct s_runtime_record
{
    char *data;
    size_t length;
    size_t capacity;
}   t_runtime_record;

int runtime_record_init(t_runtime_record *record, size_t initial_capacity);
void runtime_record_dispose(t_runtime_record *record);
int runtime_record_set_length(t_runtime_record *record, size_t length);
int runtime_record_fill(t_runtime_record *record, char value);
int runtime_record_copy_from_buffer(t_runtime_record *record, const char *buffer, size_t length);
int runtime_record_copy_to_buffer(const t_runtime_record *record, char *buffer, size_t buffer_size, size_t *written);

#endif
