#include "transpiler_standard_library.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_ceil(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-CEIL.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION INTEGER-PART(LNK-OPERAND).\n"
        "           IF LNK-OPERAND NOT = LNK-RESULT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               IF LNK-OPERAND > 0\n"
        "                   COMPUTE LNK-RESULT = LNK-RESULT + 1\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-CEIL.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

