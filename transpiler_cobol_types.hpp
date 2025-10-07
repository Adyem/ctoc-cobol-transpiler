#ifndef TRANSPILER_COBOL_TYPES_HPP
#define TRANSPILER_COBOL_TYPES_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"
#include "transpiler_context.hpp"

typedef enum e_transpiler_cobol_elementary_kind
{
    TRANSPILE_COBOL_ELEMENTARY_ALPHABETIC = 0,
    TRANSPILE_COBOL_ELEMENTARY_ALPHANUMERIC,
    TRANSPILE_COBOL_ELEMENTARY_BOOLEAN,
    TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED,
    TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED
}   t_transpiler_cobol_elementary_kind;

typedef struct s_transpiler_cobol_elementary
{
    t_transpiler_cobol_elementary_kind kind;
    size_t length;
    size_t scale;
}   t_transpiler_cobol_elementary;

typedef struct s_transpiler_cobol_group_field
{
    const char *name;
    size_t level;
    t_transpiler_cobol_elementary element;
}   t_transpiler_cobol_group_field;

typedef struct s_transpiler_cobol_group
{
    const char *name;
    size_t level;
    const t_transpiler_cobol_group_field *fields;
    size_t field_count;
}   t_transpiler_cobol_group;

int transpiler_cobol_describe_c_int(size_t digits, int is_signed, t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_char_array(size_t length, t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_bool(t_transpiler_cobol_elementary *out);

int transpiler_cobol_format_elementary(const char *name, size_t level,
    const t_transpiler_cobol_elementary *element, size_t indentation, char **out);
int transpiler_cobol_format_group(const t_transpiler_cobol_group *group,
    size_t indentation, char **out);

#endif
