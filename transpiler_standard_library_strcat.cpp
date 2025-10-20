#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strcat(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCAT.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 LEFT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 RIGHT-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-OFFSET PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION PIC X(255).\n"
        "       01 LNK-DESTINATION-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-LEFT PIC X(255).\n"
        "       01 LNK-LEFT-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RIGHT PIC X(255).\n"
        "       01 LNK-RIGHT-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       01 LNK-RESULT-LENGTH PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY VALUE LNK-DESTINATION-LENGTH BY REFERENCE LNK-LEFT\n"
        "           BY VALUE LNK-LEFT-LENGTH BY REFERENCE LNK-RIGHT\n"
        "           BY VALUE LNK-RIGHT-LENGTH BY REFERENCE LNK-STATUS\n"
        "           BY REFERENCE LNK-RESULT-LENGTH.\n"
        "       MAIN.\n"
        "           MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS " TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT-LENGTH.\n"
        "           MOVE LNK-DESTINATION-LENGTH TO DEST-LIMIT.\n"
        "           IF DEST-LIMIT > 255\n"
        "               MOVE 255 TO DEST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-LEFT-LENGTH TO LEFT-LIMIT.\n"
        "           IF LEFT-LIMIT > 255\n"
        "               MOVE 255 TO LEFT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-RIGHT-LENGTH TO RIGHT-LIMIT.\n"
        "           IF RIGHT-LIMIT > 255\n"
        "               MOVE 255 TO RIGHT-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO DEST-OFFSET.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DEST-LIMIT\n"
        "               IF LNK-DESTINATION(IDX:1) NOT = SPACE\n"
        "                   MOVE IDX TO DEST-OFFSET\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           MOVE DEST-OFFSET TO LNK-RESULT-LENGTH.\n"
        "           ADD 1 TO DEST-OFFSET.\n"
        "           IF DEST-OFFSET > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO DEST-OFFSET\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEFT-LIMIT\n"
        "               IF LNK-RESULT-LENGTH >= DEST-LIMIT\n"
        "                   MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_INVALID_ARGUMENT " TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               ADD 1 TO LNK-RESULT-LENGTH\n"
        "               MOVE LNK-LEFT(IDX:1) TO LNK-DESTINATION(LNK-RESULT-LENGTH:1)\n"
        "           END-PERFORM.\n"
        "           IF LNK-STATUS = " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS "\n"
        "               MOVE 0 TO IDX\n"
        "               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > RIGHT-LIMIT\n"
        "                   IF LNK-RESULT-LENGTH >= DEST-LIMIT\n"
        "                       MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_INVALID_ARGUMENT " TO LNK-STATUS\n"
        "                       EXIT PERFORM\n"
        "                   END-IF\n"
        "                   ADD 1 TO LNK-RESULT-LENGTH\n"
        "                   MOVE LNK-RIGHT(IDX:1) TO LNK-DESTINATION(LNK-RESULT-LENGTH:1)\n"
        "               END-PERFORM\n"
        "           END-IF.\n"
        "           IF LNK-RESULT-LENGTH > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO LNK-RESULT-LENGTH\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCAT.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

