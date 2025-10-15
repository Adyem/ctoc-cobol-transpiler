#include "transpiler_standard_library.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strlen(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRLEN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-DECLARED-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-DECLARED-LENGTH BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-DECLARED-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE IDX TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRLEN.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

