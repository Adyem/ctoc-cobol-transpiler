#include "cblc_transpiler.hpp"

#include "transpiler_standard_library_embedded.hpp"
#include "compatibility/memory_compat.hpp"

#include <cstring>

const char *transpiler_standard_library_get_native_source(const char *program_name)
{
    size_t index;

    if (!program_name)
        return (NULL);
    index = 0;
    while (index < g_embedded_standard_library_source_count)
    {
        if (std::strcmp(program_name, g_embedded_standard_library_sources[index].program_name) == 0)
            return (g_embedded_standard_library_sources[index].source);
        index += 1;
    }
    return (NULL);
}

int transpiler_standard_library_generate_native(const char *program_name, char **out_text)
{
    t_cblc_translation_unit unit;
    const char *source;
    const char *error_source;
    const char *string_class_source;
    char *combined_source;
    size_t combined_length;
    int status;

    if (!program_name || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    source = transpiler_standard_library_get_native_source(program_name);
    if (!source)
        return (FT_FAILURE);
    combined_source = NULL;
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!error_source)
        return (FT_FAILURE);
    string_class_source = NULL;
    if (std::strcmp(program_name, "CBLC-STRING") != 0
        && std::strcmp(program_name, "CBLC-ERRORS") != 0
        && std::strstr(program_name, "-STRING") != NULL)
    {
        string_class_source = transpiler_standard_library_get_native_source("CBLC-STRING");
        if (!string_class_source)
            return (FT_FAILURE);
    }
    if (std::strcmp(program_name, "CBLC-ERRORS") != 0)
    {
        combined_length = std::strlen(error_source) + std::strlen(source) + 2;
        if (string_class_source)
            combined_length += std::strlen(string_class_source) + 1;
        combined_source = static_cast<char *>(cma_calloc(combined_length, sizeof(char)));
        if (!combined_source)
            return (FT_FAILURE);
        ft_strlcpy(combined_source, error_source, combined_length);
        ft_strlcat(combined_source, "\n", combined_length);
        if (string_class_source)
        {
            ft_strlcat(combined_source, string_class_source, combined_length);
            ft_strlcat(combined_source, "\n", combined_length);
        }
        ft_strlcat(combined_source, source, combined_length);
        source = combined_source;
    }
    cblc_translation_unit_init(&unit);
    status = cblc_parse_translation_unit(source, &unit);
    if (status == FT_SUCCESS)
    {
        size_t index;

        unit.is_native_standard_library = 1;
        ft_strlcpy(unit.program_name, program_name, sizeof(unit.program_name));
        index = 0;
        while (index < unit.function_count)
        {
            if (std::strcmp(unit.functions[index].source_name, "F") == 0)
            {
                unit.entry_function_index = index;
                break ;
            }
            index += 1;
        }
        cblc_set_native_standard_library_expression_mode(1);
        status = cblc_generate_cobol(&unit, out_text);
        cblc_set_native_standard_library_expression_mode(0);
    }
    cblc_translation_unit_dispose(&unit);
    if (combined_source)
        cma_free(combined_source);
    return (status);
}
