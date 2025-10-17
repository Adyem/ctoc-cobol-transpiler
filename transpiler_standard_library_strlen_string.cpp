#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Printf/printf.hpp"

#define CBLC_STRLEN_STRING_TEMPLATE \
    "       IDENTIFICATION DIVISION.\n" \
    "       PROGRAM-ID. CBLC-STRLEN-STRING.\n" \
    "       DATA DIVISION.\n" \
    "       WORKING-STORAGE SECTION.\n" \
    "       LINKAGE SECTION.\n" \
    "       01 LNK-SOURCE.\n" \
    "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n" \
    "          05 LNK-SOURCE-BUF PIC X(%zu).\n" \
    "       01 LNK-RESULT PIC 9(9).\n" \
    "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n" \
    "           BY REFERENCE LNK-RESULT.\n" \
    "       MAIN.\n" \
    "           MOVE LNK-SOURCE-LEN TO LNK-RESULT.\n" \
    "           GOBACK.\n" \
    "       END PROGRAM CBLC-STRLEN-STRING.\n"

static size_t transpiler_standard_library_count_digits(size_t value)
{
    size_t digits;

    digits = 1;
    while (value >= 10)
    {
        value /= 10;
        digits += 1;
    }
    return (digits);
}

int transpiler_standard_library_generate_strlen_string(char **out_text)
{
    char *buffer;
    size_t template_length;
    size_t digits;
    size_t limit;
    size_t buffer_length;
    int written;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    limit = transpiler_standard_library_get_strlen_string_limit();
    template_length = ft_strlen(CBLC_STRLEN_STRING_TEMPLATE);
    digits = transpiler_standard_library_count_digits(limit);
    buffer_length = template_length - 3 + digits;
    buffer = static_cast<char *>(cma_calloc(buffer_length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    written = pf_snprintf(buffer, buffer_length + 1, CBLC_STRLEN_STRING_TEMPLATE, limit);
    if (written < 0 || static_cast<size_t>(written) != buffer_length)
    {
        cma_free(buffer);
        return (FT_FAILURE);
    }
    *out_text = buffer;
    return (FT_SUCCESS);
}

