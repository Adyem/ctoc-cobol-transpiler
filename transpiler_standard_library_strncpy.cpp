#include "transpiler_standard_library.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strncpy(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNCPY.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COPY-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION PIC X(255).\n"
        "       01 LNK-DESTINATION-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-REQUEST-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY VALUE LNK-DESTINATION-LENGTH BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY VALUE LNK-REQUEST-LENGTH\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-DESTINATION-LENGTH TO DEST-LIMIT.\n"
        "           IF DEST-LIMIT > 255\n"
        "               MOVE 255 TO DEST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SOURCE-LIMIT.\n"
        "           IF SOURCE-LIMIT > 255\n"
        "               MOVE 255 TO SOURCE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-REQUEST-LENGTH TO COPY-LIMIT.\n"
        "           IF COPY-LIMIT > 255\n"
        "               MOVE 255 TO COPY-LIMIT\n"
        "           END-IF.\n"
        "           IF LNK-REQUEST-LENGTH > DEST-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           IF LNK-REQUEST-LENGTH > SOURCE-LIMIT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "           END-IF.\n"
        "           IF COPY-LIMIT > DEST-LIMIT\n"
        "               MOVE DEST-LIMIT TO COPY-LIMIT\n"
        "           END-IF.\n"
        "           IF COPY-LIMIT > SOURCE-LIMIT\n"
        "               MOVE SOURCE-LIMIT TO COPY-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DEST-LIMIT\n"
        "               MOVE SPACE TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COPY-LIMIT\n"
        "               IF IDX > SOURCE-LIMIT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-SOURCE(IDX:1) TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNCPY.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

