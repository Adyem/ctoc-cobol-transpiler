#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strcat_string(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCAT-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 LEFT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 RIGHT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION.\n"
        "          05 LNK-DESTINATION-LEN PIC 9(4) COMP.\n"
        "          05 LNK-DESTINATION-BUF PIC X(255).\n"
        "       01 LNK-LEFT.\n"
        "          05 LNK-LEFT-LEN PIC 9(4) COMP.\n"
        "          05 LNK-LEFT-BUF PIC X(255).\n"
        "       01 LNK-RIGHT.\n"
        "          05 LNK-RIGHT-LEN PIC 9(4) COMP.\n"
        "          05 LNK-RIGHT-BUF PIC X(255).\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY REFERENCE LNK-LEFT BY REFERENCE LNK-RIGHT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 255 TO DEST-LIMIT.\n"
        "           MOVE LNK-DESTINATION-LEN TO RESULT-LENGTH.\n"
        "           IF RESULT-LENGTH < 0\n"
        "               MOVE 0 TO RESULT-LENGTH\n"
        "           END-IF.\n"
        "           IF RESULT-LENGTH > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO RESULT-LENGTH\n"
        "           END-IF.\n"
        "           MOVE LNK-LEFT-LEN TO LEFT-LIMIT.\n"
        "           IF LEFT-LIMIT < 0\n"
        "               MOVE 0 TO LEFT-LIMIT\n"
        "           END-IF.\n"
        "           IF LEFT-LIMIT > 255\n"
        "               MOVE 255 TO LEFT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-RIGHT-LEN TO RIGHT-LIMIT.\n"
        "           IF RIGHT-LIMIT < 0\n"
        "               MOVE 0 TO RIGHT-LIMIT\n"
        "           END-IF.\n"
        "           IF RIGHT-LIMIT > 255\n"
        "               MOVE 255 TO RIGHT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEFT-LIMIT\n"
        "               IF RESULT-LENGTH >= DEST-LIMIT\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               ADD 1 TO RESULT-LENGTH\n"
        "               MOVE LNK-LEFT-BUF(IDX:1) TO\n"
        "                   LNK-DESTINATION-BUF(RESULT-LENGTH:1)\n"
        "           END-PERFORM.\n"
        "           IF LNK-STATUS = 0\n"
        "               MOVE 0 TO IDX\n"
        "               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > RIGHT-LIMIT\n"
        "                   IF RESULT-LENGTH >= DEST-LIMIT\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       EXIT PERFORM\n"
        "                   END-IF\n"
        "                   ADD 1 TO RESULT-LENGTH\n"
        "                   MOVE LNK-RIGHT-BUF(IDX:1) TO\n"
        "                       LNK-DESTINATION-BUF(RESULT-LENGTH:1)\n"
        "               END-PERFORM\n"
        "           END-IF.\n"
        "           IF RESULT-LENGTH > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO RESULT-LENGTH\n"
        "           END-IF.\n"
        "           MOVE RESULT-LENGTH TO LNK-DESTINATION-LEN.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCAT-STRING.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

