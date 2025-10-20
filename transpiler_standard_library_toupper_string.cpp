#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_toupper_string(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TOUPPER-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 TARGET-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-TARGET.\n"
        "          05 LNK-TARGET-LEN PIC 9(4) COMP.\n"
        "          05 LNK-TARGET-BUF PIC X(255).\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-TARGET\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS " TO LNK-STATUS.\n"
        "           MOVE LNK-TARGET-LEN TO TARGET-LIMIT.\n"
        "           IF TARGET-LIMIT > 255\n"
        "               MOVE 255 TO TARGET-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TARGET-LIMIT\n"
        "               IF LNK-TARGET-BUF(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-TARGET-BUF(IDX:1) TO CURRENT-CHAR\n"
        "               INSPECT CURRENT-CHAR CONVERTING \"abcdefghijklmnopqrstuvwxyz\"\n"
        "                   TO \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\n"
        "               MOVE CURRENT-CHAR TO LNK-TARGET-BUF(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TOUPPER-STRING.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

