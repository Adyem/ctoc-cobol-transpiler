#include "cblc_transpiler.hpp"

static const size_t g_standard_library_default_alphanumeric_limit = 255;
static size_t g_standard_library_strlen_limit = 0;

void transpiler_standard_library_reset_usage(void)
{
    g_standard_library_strlen_limit = 0;
}

void transpiler_standard_library_note_strlen_usage(size_t declared_length)
{
    if (declared_length == 0)
        return ;
    if (declared_length > g_standard_library_strlen_limit)
        g_standard_library_strlen_limit = declared_length;
}

size_t transpiler_standard_library_get_strlen_limit(void)
{
    if (g_standard_library_strlen_limit == 0)
        return (g_standard_library_default_alphanumeric_limit);
    return (g_standard_library_strlen_limit);
}

