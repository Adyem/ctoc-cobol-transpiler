#ifndef RUNTIME_STRING_HPP
#define RUNTIME_STRING_HPP

#include <cstddef>

#include "runtime_scalar.hpp"

typedef struct s_runtime_string
{
    char *data;
    size_t length;
    size_t capacity;
}   t_runtime_string;

int runtime_string_init(t_runtime_string *value, size_t initial_capacity);
void runtime_string_dispose(t_runtime_string *value);
int runtime_string_assign(t_runtime_string *value, const char *text);
int runtime_string_trim(t_runtime_string *value);
int runtime_string_compare(const t_runtime_string *left, const t_runtime_string *right);
int runtime_string_to_int(const t_runtime_string *value, t_runtime_int *destination);
int runtime_string_equals(const t_runtime_string *left, const t_runtime_string *right);
int runtime_string_concat(t_runtime_string *destination, const t_runtime_string *left,
    const t_runtime_string *right);

#endif
