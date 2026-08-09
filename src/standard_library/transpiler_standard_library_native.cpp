#include "cblc_transpiler.hpp"

#include "transpiler_standard_library_embedded.hpp"

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
    int status;

    if (!program_name || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    source = transpiler_standard_library_get_native_source(program_name);
    if (!source)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    status = cblc_parse_translation_unit(source, &unit);
    if (status == FT_SUCCESS)
    {
        unit.is_native_standard_library = 1;
        ft_strlcpy(unit.program_name, program_name, sizeof(unit.program_name));
        cblc_set_native_standard_library_expression_mode(1);
        status = cblc_generate_cobol(&unit, out_text);
        cblc_set_native_standard_library_expression_mode(0);
    }
    cblc_translation_unit_dispose(&unit);
    return (status);
}
