#ifndef TRANSPILER_STANDARD_LIBRARY_HPP
#define TRANSPILER_STANDARD_LIBRARY_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"

typedef struct s_transpiler_standard_library_entry
{
    const char *qualified_name;
    const char *program_name;
    int (*generator)(char **out_text);
}   t_transpiler_standard_library_entry;

int transpiler_standard_library_generate_strlen(char **out_text);
int transpiler_standard_library_generate_strnlen(char **out_text);
int transpiler_standard_library_generate_strcmp(char **out_text);
int transpiler_standard_library_generate_strcpy(char **out_text);
int transpiler_standard_library_generate_strncpy(char **out_text);
int transpiler_standard_library_generate_memcmp(char **out_text);
int transpiler_standard_library_generate_strcat(char **out_text);
int transpiler_standard_library_generate_atoi(char **out_text);
int transpiler_standard_library_generate_atol(char **out_text);
int transpiler_standard_library_generate_atoll(char **out_text);
int transpiler_standard_library_generate_powerof(char **out_text);
int transpiler_standard_library_generate_sqrt(char **out_text);
int transpiler_standard_library_generate_toupper(char **out_text);
int transpiler_standard_library_generate_tolower(char **out_text);
int transpiler_standard_library_generate_isdigit(char **out_text);
int transpiler_standard_library_generate_isalpha(char **out_text);

const t_transpiler_standard_library_entry *transpiler_standard_library_get_entries(size_t *count);
const t_transpiler_standard_library_entry *transpiler_standard_library_lookup(const char *qualified_name);

#endif
