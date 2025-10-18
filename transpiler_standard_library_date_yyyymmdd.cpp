#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_date_yyyymmdd(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-DATE-YYYYMMDD.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9 VALUE 1.\n"
        "       01 WS-CHAR PIC X.\n"
        "       01 WS-DATE-DISPLAY PIC 9(8).\n"
        "       01 WS-REMAINDER PIC 9(8).\n"
        "       01 WS-YEAR PIC 9(4).\n"
        "       01 WS-MONTH PIC 9(2).\n"
        "       01 WS-DAY PIC 9(2).\n"
        "       01 WS-MAX-DAY PIC 9(2).\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-INPUT PIC X(8).\n"
        "       01 LNK-YEAR PIC 9(4).\n"
        "       01 LNK-MONTH PIC 9(2).\n"
        "       01 LNK-DAY PIC 9(2).\n"
        "       01 LNK-PACKED PIC 9(8) COMP-3.\n"
        "       01 LNK-SERIAL PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-INPUT\n"
        "           BY REFERENCE LNK-YEAR BY REFERENCE LNK-MONTH\n"
        "           BY REFERENCE LNK-DAY BY REFERENCE LNK-PACKED\n"
        "           BY REFERENCE LNK-SERIAL BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-YEAR.\n"
        "           MOVE 0 TO LNK-MONTH.\n"
        "           MOVE 0 TO LNK-DAY.\n"
        "           MOVE 0 TO LNK-PACKED.\n"
        "           MOVE 0 TO LNK-SERIAL.\n"
        "           MOVE 1 TO IDX.\n"
        "           PERFORM UNTIL IDX > 8\n"
        "               MOVE LNK-INPUT(IDX:1) TO WS-CHAR\n"
        "               IF WS-CHAR < \"0\" OR WS-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   GO TO VALIDATION-EXIT\n"
        "               END-IF\n"
        "               ADD 1 TO IDX\n"
        "           END-PERFORM.\n"
        "           MOVE LNK-INPUT TO WS-DATE-DISPLAY.\n"
        "           DIVIDE WS-DATE-DISPLAY BY 10000 GIVING WS-YEAR\n"
        "               REMAINDER WS-REMAINDER.\n"
        "           DIVIDE WS-REMAINDER BY 100 GIVING WS-MONTH\n"
        "               REMAINDER WS-DAY.\n"
        "           IF WS-MONTH < 1 OR WS-MONTH > 12\n"
        "               MOVE 2 TO LNK-STATUS\n"
        "               GO TO VALIDATION-EXIT\n"
        "           END-IF\n"
        "           MOVE 31 TO WS-MAX-DAY.\n"
        "           IF WS-MONTH = 4 OR WS-MONTH = 6 OR WS-MONTH = 9\n"
        "               OR WS-MONTH = 11\n"
        "               MOVE 30 TO WS-MAX-DAY\n"
        "           END-IF\n"
        "           IF WS-MONTH = 2\n"
        "               MOVE 28 TO WS-MAX-DAY\n"
        "               IF FUNCTION MOD(WS-YEAR, 4) = 0\n"
        "                   MOVE 29 TO WS-MAX-DAY\n"
        "                   IF FUNCTION MOD(WS-YEAR, 100) = 0\n"
        "                       IF FUNCTION MOD(WS-YEAR, 400) NOT = 0\n"
        "                           MOVE 28 TO WS-MAX-DAY\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF\n"
        "           IF WS-DAY < 1 OR WS-DAY > WS-MAX-DAY\n"
        "               MOVE 3 TO LNK-STATUS\n"
        "               GO TO VALIDATION-EXIT\n"
        "           END-IF\n"
        "           MOVE WS-YEAR TO LNK-YEAR.\n"
        "           MOVE WS-MONTH TO LNK-MONTH.\n"
        "           MOVE WS-DAY TO LNK-DAY.\n"
        "           MOVE WS-DATE-DISPLAY TO LNK-PACKED.\n"
        "           COMPUTE LNK-SERIAL = FUNCTION INTEGER-OF-DATE(WS-DATE-DISPLAY).\n"
        "       VALIDATION-EXIT.\n"
        "           IF LNK-STATUS NOT = 0\n"
        "               MOVE 0 TO LNK-YEAR\n"
        "               MOVE 0 TO LNK-MONTH\n"
        "               MOVE 0 TO LNK-DAY\n"
        "               MOVE 0 TO LNK-PACKED\n"
        "               MOVE 0 TO LNK-SERIAL\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-DATE-YYYYMMDD.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}
