#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Printf/printf.hpp"

#define CBLC_STRLEN_TEMPLATE \
    "       IDENTIFICATION DIVISION.\n" \
    "       PROGRAM-ID. CBLC-STRLEN.\n" \
    "       DATA DIVISION.\n" \
    "       WORKING-STORAGE SECTION.\n" \
    "       01 IDX PIC 9(9) VALUE 000000000.\n" \
    "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n" \
    "       LINKAGE SECTION.\n" \
    "       01 LNK-SOURCE PIC X(%zu).\n" \
    "       01 LNK-DECLARED-LENGTH PIC S9(9) COMP-5.\n" \
    "       01 LNK-RESULT PIC 9(9).\n" \
    "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n" \
    "           BY VALUE LNK-DECLARED-LENGTH BY REFERENCE LNK-RESULT.\n" \
    "       MAIN.\n" \
    "           MOVE 0 TO LNK-RESULT.\n" \
    "           MOVE LNK-DECLARED-LENGTH TO SCAN-LIMIT.\n" \
    "           IF SCAN-LIMIT > %zu\n" \
    "               MOVE %zu TO SCAN-LIMIT\n" \
    "           END-IF.\n" \
    "           MOVE 0 TO IDX.\n" \
    "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n" \
    "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n" \
    "                   EXIT PERFORM\n" \
    "               END-IF\n" \
    "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n" \
    "                   MOVE IDX TO LNK-RESULT\n" \
    "               END-IF\n" \
    "           END-PERFORM.\n" \
    "           GOBACK.\n" \
    "       END PROGRAM CBLC-STRLEN.\n"

static size_t transpiler_standard_library_count_digits(size_t value)
{
    size_t digits;

    digits = 1;
    while (value >= 10)
    {
        value /= 10;
        digits += 1;
    }
    return (digits);
}

int transpiler_standard_library_generate_strlen(char **out_text)
{
    char *buffer;
    size_t digits;
    size_t limit;
    size_t buffer_length;
    int written;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    limit = transpiler_standard_library_get_strlen_limit();
    digits = transpiler_standard_library_count_digits(limit);
    buffer_length = ft_strlen(CBLC_STRLEN_TEMPLATE) - (3 * 3) + (3 * digits);
    buffer = static_cast<char *>(cma_calloc(buffer_length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    written = pf_snprintf(buffer, buffer_length + 1, CBLC_STRLEN_TEMPLATE, limit, limit, limit);
    if (written < 0 || static_cast<size_t>(written) != buffer_length)
    {
        cma_free(buffer);
        return (FT_FAILURE);
    }
    *out_text = buffer;
    return (FT_SUCCESS);
}

