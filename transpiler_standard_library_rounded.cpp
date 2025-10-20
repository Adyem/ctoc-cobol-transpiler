#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_rounded(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ROUNDED.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 WS-ABS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-REMAINDER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-HALF USAGE COMP-2 VALUE 0.5.\n"
        "       01 WS-TWO USAGE COMP-2 VALUE 2.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS " TO LNK-STATUS.\n"
        "           COMPUTE WS-INTEGER = FUNCTION INTEGER-PART(LNK-OPERAND).\n"
        "           COMPUTE WS-FRACTION = FUNCTION ABS(LNK-OPERAND - WS-INTEGER).\n"
        "           MOVE WS-INTEGER TO LNK-RESULT.\n"
        "           IF WS-FRACTION > 0\n"
        "               MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_INVALID_ARGUMENT " TO LNK-STATUS\n"
        "           END-IF\n"
        "           IF WS-FRACTION > WS-HALF\n"
        "               IF LNK-OPERAND >= 0\n"
        "                   COMPUTE LNK-RESULT = WS-INTEGER + 1\n"
        "               ELSE\n"
        "                   COMPUTE LNK-RESULT = WS-INTEGER - 1\n"
        "               END-IF\n"
        "           ELSE\n"
        "               IF WS-FRACTION = WS-HALF\n"
        "                   COMPUTE WS-ABS-INTEGER = FUNCTION ABS(WS-INTEGER).\n"
        "                   COMPUTE WS-REMAINDER = FUNCTION MOD(WS-ABS-INTEGER, WS-TWO).\n"
        "                   IF WS-REMAINDER NOT = 0\n"
        "                       IF LNK-OPERAND >= 0\n"
        "                           COMPUTE LNK-RESULT = WS-INTEGER + 1\n"
        "                       ELSE\n"
        "                           COMPUTE LNK-RESULT = WS-INTEGER - 1\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ROUNDED.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

