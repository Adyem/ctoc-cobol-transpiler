#include "cblc_transpiler.hpp"

/*
 * These symbols remain temporarily for source compatibility with older
 * callers and tests. They deliberately contain no standard-library
 * implementation: the implementation is the embedded CBL-C source.
 */
static int transpiler_standard_library_generate_native_compat(const char *program_name,
    char **out_text)
{
    return (transpiler_standard_library_generate_native(program_name, out_text));
}

int transpiler_standard_library_generate_abs(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ABS", out_text));
}

int transpiler_standard_library_generate_isalpha(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ISALPHA", out_text));
}

int transpiler_standard_library_generate_isdigit(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ISDIGIT", out_text));
}

int transpiler_standard_library_generate_strlen(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRLEN", out_text));
}

int transpiler_standard_library_generate_strlen_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRLEN-STRING", out_text));
}

int transpiler_standard_library_generate_strnlen(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRNLEN", out_text));
}

int transpiler_standard_library_generate_strnlen_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRNLEN-STRING", out_text));
}

int transpiler_standard_library_generate_strcmp_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRCMP-STRING", out_text));
}

int transpiler_standard_library_generate_toupper(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-TOUPPER", out_text));
}

int transpiler_standard_library_generate_tolower(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-TOLOWER", out_text));
}

int transpiler_standard_library_generate_strcmp(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRCMP", out_text));
}

int transpiler_standard_library_generate_strcpy(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRCPY", out_text));
}

int transpiler_standard_library_generate_strcpy_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRCPY-STRING", out_text));
}

int transpiler_standard_library_generate_strncpy(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRNCPY", out_text));
}

int transpiler_standard_library_generate_strncpy_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRNCPY-STRING", out_text));
}

int transpiler_standard_library_generate_memcmp(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-MEMCMP", out_text));
}

int transpiler_standard_library_generate_memcmp_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-MEMCMP-STRING", out_text));
}

int transpiler_standard_library_generate_strcat(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRCAT", out_text));
}

int transpiler_standard_library_generate_strcat_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRCAT-STRING", out_text));
}

int transpiler_standard_library_generate_toupper_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-TOUPPER-STRING", out_text));
}

int transpiler_standard_library_generate_tolower_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-TOLOWER-STRING", out_text));
}

int transpiler_standard_library_generate_atoi(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ATOI", out_text));
}

int transpiler_standard_library_generate_atoi_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ATOI-STRING", out_text));
}

int transpiler_standard_library_generate_atol(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ATOL", out_text));
}

int transpiler_standard_library_generate_atol_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ATOL-STRING", out_text));
}

int transpiler_standard_library_generate_atoll(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ATOLL", out_text));
}

int transpiler_standard_library_generate_atoll_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ATOLL-STRING", out_text));
}

int transpiler_standard_library_generate_fabs(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-FABS", out_text));
}

int transpiler_standard_library_generate_floor(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-FLOOR", out_text));
}

int transpiler_standard_library_generate_ceil(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-CEIL", out_text));
}

int transpiler_standard_library_generate_exp(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-EXP", out_text));
}

int transpiler_standard_library_generate_log(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-LOG", out_text));
}

int transpiler_standard_library_generate_sin(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-SIN", out_text));
}

int transpiler_standard_library_generate_cos(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-COS", out_text));
}

int transpiler_standard_library_generate_tan(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-TAN", out_text));
}

int transpiler_standard_library_generate_powerof(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-POWEROF", out_text));
}

int transpiler_standard_library_generate_sqrt(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-SQRT", out_text));
}

int transpiler_standard_library_generate_min(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-MIN", out_text));
}

int transpiler_standard_library_generate_max(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-MAX", out_text));
}

int transpiler_standard_library_generate_rounded(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-ROUNDED", out_text));
}

int transpiler_standard_library_generate_banker_round(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-BANKER-ROUND", out_text));
}

int transpiler_standard_library_generate_date_yyyymmdd(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-DATE-YYYYMMDD", out_text));
}

int transpiler_standard_library_generate_date_parse_result(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-DATE-PARSE-RESULT", out_text));
}

int transpiler_standard_library_generate_parse_int_result(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-PARSE-INT-RESULT", out_text));
}

int transpiler_standard_library_generate_parse_double_result(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-PARSE-DOUBLE-RESULT", out_text));
}

int transpiler_standard_library_generate_date_duration(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-DATE-DURATION", out_text));
}

int transpiler_standard_library_generate_strtod(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRTOD", out_text));
}

int transpiler_standard_library_generate_strtod_string(char **out_text)
{
    return (transpiler_standard_library_generate_native_compat("CBLC-STRTOD-STRING", out_text));
}
