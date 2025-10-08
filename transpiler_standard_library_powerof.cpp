#include "transpiler_standard_library.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_powerof(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-POWEROF.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 EXP-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 EXP-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 FRACTION-TOLERANCE USAGE COMP-2 VALUE 0.0000000000001.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-BASE USAGE COMP-2.\n"
        "       01 LNK-EXPONENT USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-BASE\n"
        "           BY REFERENCE LNK-EXPONENT BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-BASE = 0 AND LNK-EXPONENT <= 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE FUNCTION INTEGER(LNK-EXPONENT) TO EXP-INTEGER.\n"
        "           COMPUTE EXP-FRACTION = FUNCTION ABS(LNK-EXPONENT - EXP-INTEGER).\n"
        "           IF LNK-BASE < 0 AND EXP-FRACTION > FRACTION-TOLERANCE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = LNK-BASE ** LNK-EXPONENT\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-POWEROF.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

