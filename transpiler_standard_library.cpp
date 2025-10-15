#include "cblc_transpiler.hpp"

static const t_transpiler_standard_library_entry g_transpiler_standard_library_entries[] = {
    {"std::abs", "CBLC-ABS", transpiler_standard_library_generate_abs},
    {"std::atoi", "CBLC-ATOI", transpiler_standard_library_generate_atoi},
    {"std::atol", "CBLC-ATOL", transpiler_standard_library_generate_atol},
    {"std::atoll", "CBLC-ATOLL", transpiler_standard_library_generate_atoll},
    {"std::fabs", "CBLC-FABS", transpiler_standard_library_generate_fabs},
    {"std::floor", "CBLC-FLOOR", transpiler_standard_library_generate_floor},
    {"std::ceil", "CBLC-CEIL", transpiler_standard_library_generate_ceil},
    {"std::exp", "CBLC-EXP", transpiler_standard_library_generate_exp},
    {"std::log", "CBLC-LOG", transpiler_standard_library_generate_log},
    {"std::sin", "CBLC-SIN", transpiler_standard_library_generate_sin},
    {"std::cos", "CBLC-COS", transpiler_standard_library_generate_cos},
    {"std::tan", "CBLC-TAN", transpiler_standard_library_generate_tan},
    {"std::strlen", "CBLC-STRLEN", transpiler_standard_library_generate_strlen},
    {"std::strnlen", "CBLC-STRNLEN", transpiler_standard_library_generate_strnlen},
    {"std::strcmp", "CBLC-STRCMP", transpiler_standard_library_generate_strcmp},
    {"std::strcpy", "CBLC-STRCPY", transpiler_standard_library_generate_strcpy},
    {"std::strncpy", "CBLC-STRNCPY", transpiler_standard_library_generate_strncpy},
    {"std::memcmp", "CBLC-MEMCMP", transpiler_standard_library_generate_memcmp},
    {"std::strcat", "CBLC-STRCAT", transpiler_standard_library_generate_strcat},
    {"std::strtod", "CBLC-STRTOD", transpiler_standard_library_generate_strtod},
    {"std::pow", "CBLC-POWEROF", transpiler_standard_library_generate_powerof},
    {"std::sqrt", "CBLC-SQRT", transpiler_standard_library_generate_sqrt},
    {"std::toupper", "CBLC-TOUPPER", transpiler_standard_library_generate_toupper},
    {"std::tolower", "CBLC-TOLOWER", transpiler_standard_library_generate_tolower},
    {"std::isdigit", "CBLC-ISDIGIT", transpiler_standard_library_generate_isdigit},
    {"std::isalpha", "CBLC-ISALPHA", transpiler_standard_library_generate_isalpha}
};

const t_transpiler_standard_library_entry *transpiler_standard_library_get_entries(size_t *count)
{
    if (count)
        *count = sizeof(g_transpiler_standard_library_entries) / sizeof(g_transpiler_standard_library_entries[0]);
    return (g_transpiler_standard_library_entries);
}

const t_transpiler_standard_library_entry *transpiler_standard_library_lookup(const char *qualified_name)
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
            return (&entries[index]);
        index += 1;
    }
    return (NULL);
}
