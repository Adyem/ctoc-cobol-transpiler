#include "standard_library_test_support.hpp"

FT_TEST(test_standard_library_lookup_enforces_std_prefix)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::abs");
    if (!entry)
    {
        pf_printf("Assertion failed: std::abs should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ABS", ft_strlen("CBLC-ABS") + 1) != 0)
    {
        pf_printf("Assertion failed: std::abs should map to CBLC-ABS program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::atoi");
    if (!entry)
    {
        pf_printf("Assertion failed: std::atoi should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOI", ft_strlen("CBLC-ATOI") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atoi should map to CBLC-ATOI program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::atoi",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::atoi overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOI-STRING",
            ft_strlen("CBLC-ATOI-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atoi overload for string should map to CBLC-ATOI-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::atol");
    if (!entry)
    {
        pf_printf("Assertion failed: std::atol should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOL", ft_strlen("CBLC-ATOL") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atol should map to CBLC-ATOL program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::atol",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::atol overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOL-STRING",
            ft_strlen("CBLC-ATOL-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atol overload for string should map to CBLC-ATOL-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::atoll");
    if (!entry)
    {
        pf_printf("Assertion failed: std::atoll should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOLL", ft_strlen("CBLC-ATOLL") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atoll should map to CBLC-ATOLL program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::atoll",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::atoll overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOLL-STRING",
            ft_strlen("CBLC-ATOLL-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atoll overload for string should map to CBLC-ATOLL-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::fabs");
    if (!entry)
    {
        pf_printf("Assertion failed: std::fabs should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-FABS", ft_strlen("CBLC-FABS") + 1) != 0)
    {
        pf_printf("Assertion failed: std::fabs should map to CBLC-FABS program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::floor");
    if (!entry)
    {
        pf_printf("Assertion failed: std::floor should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-FLOOR", ft_strlen("CBLC-FLOOR") + 1) != 0)
    {
        pf_printf("Assertion failed: std::floor should map to CBLC-FLOOR program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::ceil");
    if (!entry)
    {
        pf_printf("Assertion failed: std::ceil should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-CEIL", ft_strlen("CBLC-CEIL") + 1) != 0)
    {
        pf_printf("Assertion failed: std::ceil should map to CBLC-CEIL program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::exp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::exp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-EXP", ft_strlen("CBLC-EXP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::exp should map to CBLC-EXP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::log");
    if (!entry)
    {
        pf_printf("Assertion failed: std::log should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-LOG", ft_strlen("CBLC-LOG") + 1) != 0)
    {
        pf_printf("Assertion failed: std::log should map to CBLC-LOG program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::sin");
    if (!entry)
    {
        pf_printf("Assertion failed: std::sin should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-SIN", ft_strlen("CBLC-SIN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::sin should map to CBLC-SIN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::cos");
    if (!entry)
    {
        pf_printf("Assertion failed: std::cos should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-COS", ft_strlen("CBLC-COS") + 1) != 0)
    {
        pf_printf("Assertion failed: std::cos should map to CBLC-COS program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::tan");
    if (!entry)
    {
        pf_printf("Assertion failed: std::tan should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TAN", ft_strlen("CBLC-TAN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::tan should map to CBLC-TAN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strlen");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strlen should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRLEN", ft_strlen("CBLC-STRLEN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strlen should map to CBLC-STRLEN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strlen",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strlen overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRLEN-STRING",
            ft_strlen("CBLC-STRLEN-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strlen overload for string should map to CBLC-STRLEN-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strnlen");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strnlen should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNLEN", ft_strlen("CBLC-STRNLEN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strnlen should map to CBLC-STRNLEN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strnlen",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strnlen overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNLEN-STRING",
            ft_strlen("CBLC-STRNLEN-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strnlen overload for string should map to CBLC-STRNLEN-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcmp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcmp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCMP", ft_strlen("CBLC-STRCMP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcmp should map to CBLC-STRCMP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strcmp",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcmp overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCMP-STRING",
            ft_strlen("CBLC-STRCMP-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcmp overload for string should map to CBLC-STRCMP-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcpy");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcpy should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCPY", ft_strlen("CBLC-STRCPY") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcpy should map to CBLC-STRCPY program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strcpy",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcpy overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCPY-STRING",
            ft_strlen("CBLC-STRCPY-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcpy overload for string should map to CBLC-STRCPY-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strncpy");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strncpy should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNCPY", ft_strlen("CBLC-STRNCPY") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strncpy should map to CBLC-STRNCPY program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strncpy",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strncpy overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNCPY-STRING",
            ft_strlen("CBLC-STRNCPY-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strncpy overload for string should map to CBLC-STRNCPY-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::memcmp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::memcmp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-MEMCMP", ft_strlen("CBLC-MEMCMP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::memcmp should map to CBLC-MEMCMP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::memcmp",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::memcmp overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-MEMCMP-STRING",
            ft_strlen("CBLC-MEMCMP-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::memcmp overload for string should map to CBLC-MEMCMP-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcat");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcat should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCAT", ft_strlen("CBLC-STRCAT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcat should map to CBLC-STRCAT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strcat",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcat overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCAT-STRING",
            ft_strlen("CBLC-STRCAT-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcat overload for string should map to CBLC-STRCAT-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strtod");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strtod should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRTOD", ft_strlen("CBLC-STRTOD") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strtod should map to CBLC-STRTOD program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strtod",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strtod overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRTOD-STRING",
            ft_strlen("CBLC-STRTOD-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strtod overload for string should map to CBLC-STRTOD-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::pow");
    if (!entry)
    {
        pf_printf("Assertion failed: std::pow should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-POWEROF", ft_strlen("CBLC-POWEROF") + 1) != 0)
    {
        pf_printf("Assertion failed: std::pow should map to CBLC-POWEROF program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::sqrt");
    if (!entry)
    {
        pf_printf("Assertion failed: std::sqrt should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-SQRT", ft_strlen("CBLC-SQRT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::sqrt should map to CBLC-SQRT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::toupper");
    if (!entry)
    {
        pf_printf("Assertion failed: std::toupper should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TOUPPER", ft_strlen("CBLC-TOUPPER") + 1) != 0)
    {
        pf_printf("Assertion failed: std::toupper should map to CBLC-TOUPPER program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::toupper",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::toupper overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TOUPPER-STRING",
            ft_strlen("CBLC-TOUPPER-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::toupper overload for string should map to CBLC-TOUPPER-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::tolower");
    if (!entry)
    {
        pf_printf("Assertion failed: std::tolower should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TOLOWER", ft_strlen("CBLC-TOLOWER") + 1) != 0)
    {
        pf_printf("Assertion failed: std::tolower should map to CBLC-TOLOWER program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::tolower",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::tolower overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TOLOWER-STRING",
            ft_strlen("CBLC-TOLOWER-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::tolower overload for string should map to CBLC-TOLOWER-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::isdigit");
    if (!entry)
    {
        pf_printf("Assertion failed: std::isdigit should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ISDIGIT", ft_strlen("CBLC-ISDIGIT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::isdigit should map to CBLC-ISDIGIT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::isalpha");
    if (!entry)
    {
        pf_printf("Assertion failed: std::isalpha should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ISALPHA", ft_strlen("CBLC-ISALPHA") + 1) != 0)
    {
        pf_printf("Assertion failed: std::isalpha should map to CBLC-ISALPHA program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strlen");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strlen should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strnlen");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strnlen should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcmp");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcmp should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcpy");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcpy should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strncpy");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strncpy should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("memcmp");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified memcmp should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcat");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcat should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("fabs");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified fabs should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("floor");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified floor should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("ceil");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified ceil should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strtod");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strtod should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("sqrt");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified sqrt should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("toupper");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified toupper should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("tolower");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified tolower should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("abs");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified abs should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_lookup_is_case_sensitive)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::STRLEN");
    if (entry)
    {
        pf_printf("Assertion failed: std::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::strlen");
    if (entry)
    {
        pf_printf("Assertion failed: STD::strlen should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::STRLEN");
    if (entry)
    {
        pf_printf("Assertion failed: STD::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::MEMCMP");
    if (entry)
    {
        pf_printf("Assertion failed: std::MEMCMP should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::memcmp");
    if (entry)
    {
        pf_printf("Assertion failed: STD::memcmp should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_catalog_lists_all_entries)
{
    const t_transpiler_standard_library_entry *entries;
    size_t count;
    size_t expected_count;
    size_t index;
    static const struct
    {
        const char *qualified_name;
        t_transpiler_standard_library_buffer_kind buffer_kind;
    } expected[] = {
        {"std::abs", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::atoi", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::atoi", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::atol", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::atol", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::atoll", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::atoll", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::fabs", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::floor", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::ceil", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::exp", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::log", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::sin", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::cos", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::tan", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::strlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::strnlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strnlen", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::strcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::strcpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strcpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::strncpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strncpy", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::memcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::memcmp", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::strcat", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strcat", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::strtod", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::strtod", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::pow", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::sqrt", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::toupper", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::toupper", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::tolower", TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR},
        {"std::tolower", TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING},
        {"std::isdigit", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE},
        {"std::isalpha", TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE}
    };

    entries = transpiler_standard_library_get_entries(&count);
    if (!entries)
    {
        pf_printf("Assertion failed: catalog should return entry table\n");
        return (FT_FAILURE);
    }
    expected_count = sizeof(expected) / sizeof(expected[0]);
    if (count != expected_count)
    {
        pf_printf("Assertion failed: catalog should report %u standard library entries but returned %u\n",
            static_cast<unsigned int>(expected_count),
            static_cast<unsigned int>(count));
        return (FT_FAILURE);
    }
    index = 0;
    while (index < expected_count)
    {
        if (ft_strncmp(entries[index].qualified_name, expected[index].qualified_name,
                ft_strlen(expected[index].qualified_name) + 1) != 0)
        {
            pf_printf("Assertion failed: catalog entry %u should be %s\n",
                static_cast<unsigned int>(index + 1), expected[index].qualified_name);
            return (FT_FAILURE);
        }
        if (entries[index].buffer_kind != expected[index].buffer_kind)
        {
            pf_printf("Assertion failed: catalog entry %u should target buffer kind %d but was %d\n",
                static_cast<unsigned int>(index + 1), expected[index].buffer_kind,
                entries[index].buffer_kind);
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_generators_validate_out_parameter)
{
    if (transpiler_standard_library_generate_abs(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: abs generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_fabs(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: fabs generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strlen(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strlen generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strnlen(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strnlen generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strnlen_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strnlen string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcmp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcmp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcmp_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcmp string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcpy(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcpy generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcpy_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcpy string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strncpy(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strncpy generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strncpy_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strncpy string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_memcmp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: memcmp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_memcmp_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: memcmp string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcat(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcat generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcat_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcat string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strtod(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strtod generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strtod_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strtod string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_exp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: exp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_log(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: log generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_sin(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: sin generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_cos(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: cos generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_tan(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: tan generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_powerof(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: powerof generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_sqrt(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: sqrt generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_toupper(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: toupper generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_toupper_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: toupper string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_tolower(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: tolower generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_tolower_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: tolower string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atoi(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atoi generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atoi_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atoi string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atol(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atol generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atol_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atol string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atoll(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atoll generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atoll_string(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atoll string generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

