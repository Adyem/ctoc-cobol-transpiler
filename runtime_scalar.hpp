#ifndef RUNTIME_SCALAR_HPP
#define RUNTIME_SCALAR_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"

typedef struct s_runtime_int
{
    int value;
}   t_runtime_int;

typedef struct s_runtime_char
{
    char value;
}   t_runtime_char;

void runtime_int_set(t_runtime_int *destination, int value);
int runtime_int_from_string(t_runtime_int *destination, const char *text);
int runtime_int_add(t_runtime_int left, t_runtime_int right, t_runtime_int *result);
int runtime_int_subtract(t_runtime_int left, t_runtime_int right, t_runtime_int *result);
int runtime_int_multiply(t_runtime_int left, t_runtime_int right, t_runtime_int *result);
int runtime_int_divide(t_runtime_int dividend, t_runtime_int divisor, t_runtime_int *result);
int runtime_int_compare(t_runtime_int left, t_runtime_int right);
int runtime_int_to_string(t_runtime_int value, char *buffer, size_t buffer_size);

void runtime_char_set(t_runtime_char *destination, char value);
int runtime_char_from_string(t_runtime_char *destination, const char *text);
void runtime_char_to_upper(t_runtime_char *value);
void runtime_char_to_lower(t_runtime_char *value);
int runtime_char_compare(t_runtime_char left, t_runtime_char right);
int runtime_char_to_string(t_runtime_char value, char *buffer, size_t buffer_size);

void runtime_demo(void);

#endif
