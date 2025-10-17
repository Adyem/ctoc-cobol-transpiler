#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strcmp_string(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCMP-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 FIRST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 SECOND-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COMPARE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-FIRST.\n"
        "          05 LNK-FIRST-LEN PIC 9(4) COMP.\n"
        "          05 LNK-FIRST-BUF PIC X(255).\n"
        "       01 LNK-SECOND.\n"
        "          05 LNK-SECOND-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SECOND-BUF PIC X(255).\n"
        "       01 LNK-RESULT PIC S9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-FIRST\n"
        "           BY REFERENCE LNK-SECOND BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-FIRST-LEN TO FIRST-LIMIT.\n"
        "           IF FIRST-LIMIT > 255\n"
        "               MOVE 255 TO FIRST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SECOND-LEN TO SECOND-LIMIT.\n"
        "           IF SECOND-LIMIT > 255\n"
        "               MOVE 255 TO SECOND-LIMIT\n"
        "           END-IF.\n"
        "           MOVE FIRST-LIMIT TO COMPARE-LIMIT.\n"
        "           IF SECOND-LIMIT < COMPARE-LIMIT\n"
        "               MOVE SECOND-LIMIT TO COMPARE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COMPARE-LIMIT\n"
        "               IF LNK-FIRST-BUF(IDX:1) < LNK-SECOND-BUF(IDX:1)\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-FIRST-BUF(IDX:1) > LNK-SECOND-BUF(IDX:1)\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF FIRST-LIMIT < SECOND-LIMIT\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF FIRST-LIMIT > SECOND-LIMIT\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCMP-STRING.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

