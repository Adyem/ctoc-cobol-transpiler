#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strcpy_string(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCPY-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COPY-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION.\n"
        "          05 LNK-DESTINATION-LEN PIC 9(4) COMP.\n"
        "          05 LNK-DESTINATION-BUF PIC X(255).\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY REFERENCE LNK-SOURCE BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS " TO LNK-STATUS.\n"
        "           MOVE LNK-SOURCE-LEN TO SOURCE-LIMIT.\n"
        "           IF SOURCE-LIMIT > 255\n"
        "               MOVE 255 TO SOURCE-LIMIT\n"
        "               MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_INVALID_ARGUMENT " TO LNK-STATUS\n"
        "           END-IF.\n"
        "           MOVE SOURCE-LIMIT TO COPY-LIMIT.\n"
        "           MOVE 0 TO LNK-DESTINATION-LEN.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 255\n"
        "               MOVE SPACE TO LNK-DESTINATION-BUF(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COPY-LIMIT\n"
        "               MOVE LNK-SOURCE-BUF(IDX:1) TO LNK-DESTINATION-BUF(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE COPY-LIMIT TO LNK-DESTINATION-LEN.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCPY-STRING.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

