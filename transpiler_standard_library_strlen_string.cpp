#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strlen_string(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRLEN-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE LNK-SOURCE-LEN TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRLEN-STRING.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

