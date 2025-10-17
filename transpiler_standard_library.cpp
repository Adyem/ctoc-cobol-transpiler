#include "cblc_transpiler.hpp"

static const t_transpiler_standard_library_entry g_transpiler_standard_library_entries[] = {
    {"std::abs", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-ABS",
        transpiler_standard_library_generate_abs},
    {"std::atoi", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-ATOI",
        transpiler_standard_library_generate_atoi},
    {"std::atol", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-ATOL",
        transpiler_standard_library_generate_atol},
    {"std::atoll", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-ATOLL",
        transpiler_standard_library_generate_atoll},
    {"std::fabs", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-FABS",
        transpiler_standard_library_generate_fabs},
    {"std::floor", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-FLOOR",
        transpiler_standard_library_generate_floor},
    {"std::ceil", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-CEIL",
        transpiler_standard_library_generate_ceil},
    {"std::exp", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-EXP",
        transpiler_standard_library_generate_exp},
    {"std::log", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-LOG",
        transpiler_standard_library_generate_log},
    {"std::sin", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-SIN",
        transpiler_standard_library_generate_sin},
    {"std::cos", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-COS",
        transpiler_standard_library_generate_cos},
    {"std::tan", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-TAN",
        transpiler_standard_library_generate_tan},
    {"std::strlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRLEN",
        transpiler_standard_library_generate_strlen},
    {"std::strlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING, "CBLC-STRLEN-STRING",
        transpiler_standard_library_generate_strlen_string},
    {"std::strnlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRNLEN",
        transpiler_standard_library_generate_strnlen},
    {"std::strcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRCMP",
        transpiler_standard_library_generate_strcmp},
    {"std::strcpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRCPY",
        transpiler_standard_library_generate_strcpy},
    {"std::strncpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRNCPY",
        transpiler_standard_library_generate_strncpy},
    {"std::memcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-MEMCMP",
        transpiler_standard_library_generate_memcmp},
    {"std::strcat", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRCAT",
        transpiler_standard_library_generate_strcat},
    {"std::strtod", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-STRTOD",
        transpiler_standard_library_generate_strtod},
    {"std::pow", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-POWEROF",
        transpiler_standard_library_generate_powerof},
    {"std::sqrt", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-SQRT",
        transpiler_standard_library_generate_sqrt},
    {"std::toupper", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-TOUPPER",
        transpiler_standard_library_generate_toupper},
    {"std::tolower", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR, "CBLC-TOLOWER",
        transpiler_standard_library_generate_tolower},
    {"std::isdigit", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-ISDIGIT",
        transpiler_standard_library_generate_isdigit},
    {"std::isalpha", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE, "CBLC-ISALPHA",
        transpiler_standard_library_generate_isalpha}
};

const t_transpiler_standard_library_entry *transpiler_standard_library_get_entries(size_t *count)
{
    if (count)
        *count = sizeof(g_transpiler_standard_library_entries) / sizeof(g_transpiler_standard_library_entries[0]);
    return (g_transpiler_standard_library_entries);
}

const t_transpiler_standard_library_entry *transpiler_standard_library_lookup(const char *qualified_name)
{
    return (transpiler_standard_library_lookup_with_buffer_kind(qualified_name,
        TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE));
}

const t_transpiler_standard_library_entry *transpiler_standard_library_lookup_with_buffer_kind(
    const char *qualified_name, t_transpiler_standard_library_buffer_kind buffer_kind)
{
    const t_transpiler_standard_library_entry *entries;
    size_t entry_count;
    size_t index;

    if (!qualified_name)
        return (NULL);
    entries = transpiler_standard_library_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        if (ft_strncmp(entries[index].qualified_name, qualified_name,
                ft_strlen(entries[index].qualified_name) + 1) == 0)
        {
            if (buffer_kind == TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE
                || entries[index].buffer_kind == buffer_kind)
                return (&entries[index]);
        }
        index += 1;
    }
    return (NULL);
}
