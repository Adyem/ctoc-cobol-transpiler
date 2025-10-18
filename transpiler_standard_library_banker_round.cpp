#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_banker_round(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-BANKER-ROUND.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-SCALE-POWER USAGE COMP-2 VALUE 1.\n"
        "       01 WS-SCALED USAGE COMP-2 VALUE 0.\n"
        "       01 WS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 WS-ABS-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-REMAINDER USAGE COMP-2 VALUE 0.\n"
        "       01 WS-HALF USAGE COMP-2 VALUE 0.5.\n"
        "       01 WS-TWO USAGE COMP-2 VALUE 2.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-SCALE PIC S9(4) COMP-5.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-SCALE BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-SCALE < 0 OR LNK-SCALE > 18\n"
        "               MOVE 2 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE WS-SCALE-POWER = 10 ** LNK-SCALE\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 2 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           COMPUTE WS-SCALED = LNK-OPERAND * WS-SCALE-POWER\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 2 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           COMPUTE WS-INTEGER = FUNCTION INTEGER-PART(WS-SCALED).\n"
        "           COMPUTE WS-FRACTION = FUNCTION ABS(WS-SCALED - WS-INTEGER).\n"
        "           MOVE WS-INTEGER TO WS-SCALED.\n"
        "           IF WS-FRACTION > 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF\n"
        "           IF WS-FRACTION > WS-HALF\n"
        "               IF LNK-OPERAND >= 0\n"
        "                   COMPUTE WS-SCALED = WS-INTEGER + 1\n"
        "               ELSE\n"
        "                   COMPUTE WS-SCALED = WS-INTEGER - 1\n"
        "               END-IF\n"
        "           ELSE\n"
        "               IF WS-FRACTION = WS-HALF\n"
        "                   COMPUTE WS-ABS-INTEGER = FUNCTION ABS(WS-INTEGER).\n"
        "                   COMPUTE WS-REMAINDER = FUNCTION MOD(WS-ABS-INTEGER, WS-TWO).\n"
        "                   IF WS-REMAINDER NOT = 0\n"
        "                       IF LNK-OPERAND >= 0\n"
        "                           COMPUTE WS-SCALED = WS-INTEGER + 1\n"
        "                       ELSE\n"
        "                           COMPUTE WS-SCALED = WS-INTEGER - 1\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = WS-SCALED / WS-SCALE-POWER\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 2 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-BANKER-ROUND.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

