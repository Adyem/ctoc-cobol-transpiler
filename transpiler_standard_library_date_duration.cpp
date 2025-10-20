#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

int transpiler_standard_library_generate_date_duration(char **out_text)
{
    const char *template_text;
    char *buffer;
    size_t length;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    template_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-DATE-DURATION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WS-DIFF PIC S9(9) COMP-5.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-START-SERIAL PIC S9(9) COMP-5.\n"
        "       01 LNK-END-SERIAL PIC S9(9) COMP-5.\n"
        "       01 LNK-DURATION PIC S9(9) COMP-5.\n"
        "       01 LNK-COMPARISON PIC S9 COMP-5.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-START-SERIAL\n"
        "           BY REFERENCE LNK-END-SERIAL BY REFERENCE LNK-DURATION\n"
        "           BY REFERENCE LNK-COMPARISON BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE " TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS " TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-DURATION.\n"
        "           MOVE 0 TO LNK-COMPARISON.\n"
        "           MOVE LNK-END-SERIAL TO WS-DIFF.\n"
        "           SUBTRACT LNK-START-SERIAL FROM WS-DIFF.\n"
        "           IF WS-DIFF < 0\n"
        "               MOVE -1 TO LNK-COMPARISON\n"
        "               MULTIPLY -1 BY WS-DIFF GIVING LNK-DURATION\n"
        "           ELSE\n"
        "               IF WS-DIFF > 0\n"
        "                   MOVE 1 TO LNK-COMPARISON\n"
        "                   MOVE WS-DIFF TO LNK-DURATION\n"
        "               ELSE\n"
        "                   MOVE 0 TO LNK-COMPARISON\n"
        "                   MOVE 0 TO LNK-DURATION\n"
        "               END-IF\n"
        "           END-IF\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-DATE-DURATION.\n";
    length = ft_strlen(template_text);
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    ft_memcpy(buffer, template_text, length);
    buffer[length] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}
