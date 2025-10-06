#ifndef TRANSPILER_DIAGNOSTICS_HPP
#define TRANSPILER_DIAGNOSTICS_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"

#define TRANSPILE_DIAGNOSTIC_MESSAGE_MAX 256

typedef enum e_transpiler_severity
{
    TRANSPILE_SEVERITY_INFO = 0,
    TRANSPILE_SEVERITY_WARNING,
    TRANSPILE_SEVERITY_ERROR
}   t_transpiler_severity;

typedef struct s_transpiler_diagnostic
{
    t_transpiler_severity severity;
    int code;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
}   t_transpiler_diagnostic;

typedef struct s_transpiler_diagnostic_list
{
    t_transpiler_diagnostic *items;
    size_t count;
    size_t capacity;
}   t_transpiler_diagnostic_list;

int transpiler_diagnostics_init(t_transpiler_diagnostic_list *list);
void transpiler_diagnostics_dispose(t_transpiler_diagnostic_list *list);
int transpiler_diagnostics_push(t_transpiler_diagnostic_list *list, t_transpiler_severity severity, int code, const char *message)
;

#endif
