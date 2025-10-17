#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_strnlen_string(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNLEN-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 REQUEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-REQUEST PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-REQUEST BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE LNK-SOURCE-LEN TO ACTUAL-LENGTH.\n"
        "           IF ACTUAL-LENGTH > 255\n"
        "               MOVE 255 TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE LNK-REQUEST TO REQUEST-LIMIT.\n"
        "           IF REQUEST-LIMIT < 0\n"
        "               MOVE 0 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT > 255\n"
        "               MOVE 255 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT < ACTUAL-LENGTH\n"
        "               MOVE REQUEST-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE ACTUAL-LENGTH TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNLEN-STRING.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}

