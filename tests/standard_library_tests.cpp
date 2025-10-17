#include "compiler/compiler_test_support.hpp"
#include "libft/CMA/CMA.hpp"
#include "test_suites.hpp"

#include <cerrno>
#include <cmath>
#include <cstdlib>

static const double g_default_float_tolerance = 0.0001;

static int test_read_transcript_line(const char *buffer, size_t *offset, char *line_buffer, size_t buffer_size)
{
    size_t write_index;

    if (!buffer || !offset || !line_buffer || buffer_size == 0)
        return (FT_FAILURE);
    write_index = 0;
    while (buffer[*offset] != '\0' && buffer[*offset] != '\n')
    {
        if (write_index + 1 >= buffer_size)
            return (FT_FAILURE);
        line_buffer[write_index] = buffer[*offset];
        write_index += 1;
        *offset += 1;
    }
    if (write_index >= buffer_size)
        return (FT_FAILURE);
    line_buffer[write_index] = '\0';
    if (buffer[*offset] == '\n')
        *offset += 1;
    return (FT_SUCCESS);
}

static int test_expect_transcript_double(const char *line, double expected, double tolerance, const char *message)
{
    const char *cursor;
    double actual;
    char *end;

    if (!line || !message)
        return (FT_FAILURE);
    errno = 0;
    actual = std::strtod(line, &end);
    cursor = end;
    if (line == cursor || errno == ERANGE)
    {
        pf_printf("Assertion failed: %s (expected floating-point line, received '%s')\n",
            message, line);
        return (FT_FAILURE);
    }
    while (*cursor == ' ')
        cursor += 1;
    if (*cursor != '\0')
    {
        pf_printf("Assertion failed: %s (unexpected trailing characters '%s')\n", message, cursor);
        return (FT_FAILURE);
    }
    if (std::fabs(actual - expected) > tolerance)
    {
        pf_printf("Assertion failed: %s (expected %.6f ± %.6f, observed %.6f)\n",
            message, expected, tolerance, actual);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_expect_transcript_double_line(const char *buffer, size_t *offset, double expected, double tolerance,
    const char *message)
{
    char line_buffer[64];

    if (test_read_transcript_line(buffer, offset, line_buffer, sizeof(line_buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: %s (missing floating-point transcript line)\n", message);
        return (FT_FAILURE);
    }
    return (test_expect_transcript_double(line_buffer, expected, tolerance, message));
}

static int test_expect_transcript_status_line(const char *buffer, size_t *offset, const char *expected,
    const char *message)
{
    char line_buffer[32];

    if (test_read_transcript_line(buffer, offset, line_buffer, sizeof(line_buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: %s (missing status transcript line)\n", message);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line_buffer, expected, message) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_expect_transcript_complete(const char *buffer, size_t offset, const char *message)
{
    if (!buffer || !message)
        return (FT_FAILURE);
    if (buffer[offset] != '\0')
    {
        pf_printf("Assertion failed: %s (unexpected trailing transcript data '%s')\n", message, buffer + offset);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atoi_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ATOI.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 REMAINING-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 DIGIT-COUNT PIC 9(9) VALUE 000000000.\n"
        "       01 NEGATIVE-FLAG PIC 9 VALUE 0.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 DIGIT-VALUE PIC 9 VALUE 0.\n"
        "       01 OVERFLOW-FLAG PIC 9 VALUE 0.\n"
        "       01 ACCUMULATOR PIC S9(36) COMP-3 VALUE 0.\n"
        "       01 MAX-VALUE PIC S9(36) COMP-3 VALUE 999999999.\n"
        "       01 MIN-VALUE PIC S9(36) COMP-3 VALUE -999999999.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(9).\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO ACTUAL-LENGTH.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE IDX TO ACTUAL-LENGTH\n"
        "           END-PERFORM.\n"
        "           IF ACTUAL-LENGTH = 0\n"
        "               MOVE SCAN-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE 1 TO START-INDEX.\n"
        "           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(START-INDEX:1) NOT = SPACE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE(START-INDEX:1) TO CURRENT-CHAR.\n"
        "           MOVE 0 TO NEGATIVE-FLAG.\n"
        "           IF CURRENT-CHAR = \"-\"\n"
        "               MOVE 1 TO NEGATIVE-FLAG\n"
        "               ADD 1 TO START-INDEX\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR = \"+\"\n"
        "                   ADD 1 TO START-INDEX\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO DIGIT-COUNT.\n"
        "           MOVE 0 TO OVERFLOW-FLAG.\n"
        "           MOVE 0 TO ACCUMULATOR.\n"
        "           MOVE 0 TO END-INDEX.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR = LOW-VALUE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR < \"0\" OR CURRENT-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               ADD 1 TO DIGIT-COUNT\n"
        "               MOVE LNK-SOURCE(IDX:1) TO DIGIT-VALUE\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR * 10\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR + DIGIT-VALUE\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               MOVE IDX TO END-INDEX\n"
        "           END-PERFORM.\n"
        "           IF DIGIT-COUNT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF END-INDEX = 0\n"
        "               MOVE ACTUAL-LENGTH TO END-INDEX\n"
        "           END-IF.\n"
        "           COMPUTE REMAINING-INDEX = END-INDEX + 1.\n"
        "           IF REMAINING-INDEX < START-INDEX\n"
        "               MOVE START-INDEX TO REMAINING-INDEX\n"
        "           END-IF.\n"
        "           PERFORM VARYING IDX FROM REMAINING-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF NEGATIVE-FLAG = 1\n"
        "               COMPUTE ACCUMULATOR = 0 - ACCUMULATOR\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR > MAX-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR < MIN-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACCUMULATOR TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ATOI.\n";
    if (test_expect_success(transpiler_standard_library_generate_atoi(&program_text),
            "atoi generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "atoi generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atol_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ATOL.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 REMAINING-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 DIGIT-COUNT PIC 9(9) VALUE 000000000.\n"
        "       01 NEGATIVE-FLAG PIC 9 VALUE 0.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 DIGIT-VALUE PIC 9 VALUE 0.\n"
        "       01 OVERFLOW-FLAG PIC 9 VALUE 0.\n"
        "       01 ACCUMULATOR PIC S9(36) COMP-3 VALUE 0.\n"
        "       01 MAX-VALUE PIC S9(36) COMP-3 VALUE 999999999999999999.\n"
        "       01 MIN-VALUE PIC S9(36) COMP-3 VALUE -999999999999999999.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(18).\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO ACTUAL-LENGTH.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE IDX TO ACTUAL-LENGTH\n"
        "           END-PERFORM.\n"
        "           IF ACTUAL-LENGTH = 0\n"
        "               MOVE SCAN-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE 1 TO START-INDEX.\n"
        "           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(START-INDEX:1) NOT = SPACE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE(START-INDEX:1) TO CURRENT-CHAR.\n"
        "           MOVE 0 TO NEGATIVE-FLAG.\n"
        "           IF CURRENT-CHAR = \"-\"\n"
        "               MOVE 1 TO NEGATIVE-FLAG\n"
        "               ADD 1 TO START-INDEX\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR = \"+\"\n"
        "                   ADD 1 TO START-INDEX\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO DIGIT-COUNT.\n"
        "           MOVE 0 TO OVERFLOW-FLAG.\n"
        "           MOVE 0 TO ACCUMULATOR.\n"
        "           MOVE 0 TO END-INDEX.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR = LOW-VALUE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR < \"0\" OR CURRENT-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               ADD 1 TO DIGIT-COUNT\n"
        "               MOVE LNK-SOURCE(IDX:1) TO DIGIT-VALUE\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR * 10\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR + DIGIT-VALUE\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               MOVE IDX TO END-INDEX\n"
        "           END-PERFORM.\n"
        "           IF DIGIT-COUNT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF END-INDEX = 0\n"
        "               MOVE ACTUAL-LENGTH TO END-INDEX\n"
        "           END-IF.\n"
        "           COMPUTE REMAINING-INDEX = END-INDEX + 1.\n"
        "           IF REMAINING-INDEX < START-INDEX\n"
        "               MOVE START-INDEX TO REMAINING-INDEX\n"
        "           END-IF.\n"
        "           PERFORM VARYING IDX FROM REMAINING-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF NEGATIVE-FLAG = 1\n"
        "               COMPUTE ACCUMULATOR = 0 - ACCUMULATOR\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR > MAX-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR < MIN-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACCUMULATOR TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ATOL.\n";
    if (test_expect_success(transpiler_standard_library_generate_atol(&program_text),
            "atol generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "atol generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atoll_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ATOLL.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 REMAINING-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 DIGIT-COUNT PIC 9(9) VALUE 000000000.\n"
        "       01 NEGATIVE-FLAG PIC 9 VALUE 0.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 DIGIT-VALUE PIC 9 VALUE 0.\n"
        "       01 OVERFLOW-FLAG PIC 9 VALUE 0.\n"
        "       01 ACCUMULATOR PIC S9(36) COMP-3 VALUE 0.\n"
        "       01 MAX-VALUE PIC S9(36) COMP-3 VALUE 999999999999999999999999999999999999.\n"
        "       01 MIN-VALUE PIC S9(36) COMP-3 VALUE -999999999999999999999999999999999999.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(36).\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO ACTUAL-LENGTH.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE IDX TO ACTUAL-LENGTH\n"
        "           END-PERFORM.\n"
        "           IF ACTUAL-LENGTH = 0\n"
        "               MOVE SCAN-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE 1 TO START-INDEX.\n"
        "           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(START-INDEX:1) NOT = SPACE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE LNK-SOURCE(START-INDEX:1) TO CURRENT-CHAR.\n"
        "           MOVE 0 TO NEGATIVE-FLAG.\n"
        "           IF CURRENT-CHAR = \"-\"\n"
        "               MOVE 1 TO NEGATIVE-FLAG\n"
        "               ADD 1 TO START-INDEX\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR = \"+\"\n"
        "                   ADD 1 TO START-INDEX\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO DIGIT-COUNT.\n"
        "           MOVE 0 TO OVERFLOW-FLAG.\n"
        "           MOVE 0 TO ACCUMULATOR.\n"
        "           MOVE 0 TO END-INDEX.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR = LOW-VALUE\n"
        "                   COMPUTE END-INDEX = IDX - 1\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF CURRENT-CHAR < \"0\" OR CURRENT-CHAR > \"9\"\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               ADD 1 TO DIGIT-COUNT\n"
        "               MOVE LNK-SOURCE(IDX:1) TO DIGIT-VALUE\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR * 10\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               COMPUTE ACCUMULATOR = ACCUMULATOR + DIGIT-VALUE\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               MOVE IDX TO END-INDEX\n"
        "           END-PERFORM.\n"
        "           IF DIGIT-COUNT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF END-INDEX = 0\n"
        "               MOVE ACTUAL-LENGTH TO END-INDEX\n"
        "           END-IF.\n"
        "           COMPUTE REMAINING-INDEX = END-INDEX + 1.\n"
        "           IF REMAINING-INDEX < START-INDEX\n"
        "               MOVE START-INDEX TO REMAINING-INDEX\n"
        "           END-IF.\n"
        "           PERFORM VARYING IDX FROM REMAINING-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF NEGATIVE-FLAG = 1\n"
        "               COMPUTE ACCUMULATOR = 0 - ACCUMULATOR\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO OVERFLOW-FLAG\n"
        "               END-COMPUTE\n"
        "               IF OVERFLOW-FLAG = 1\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR > MAX-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF ACCUMULATOR < MIN-VALUE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACCUMULATOR TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ATOLL.\n";
    if (test_expect_success(transpiler_standard_library_generate_atoll(&program_text),
            "atoll generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "atoll generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atoi_converts_signed_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atoi_lib.cob";
    driver_path = "stdlib_atoi_drv.cob";
    binary_path = "stdlib_atoi.bin";
    output_path = "stdlib_atoi.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOI-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(16) VALUE \"   -12345   \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +12.\n"
        "       01 RESULT PIC S9(9) VALUE 000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(9).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOI' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOI-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atoi(&library_text),
            "atoi generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "-000012345\n0\n",
            "atoi helper should convert signed input and succeed") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atoi_rejects_invalid_input)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atoi_invalid_lib.cob";
    driver_path = "stdlib_atoi_invalid_drv.cob";
    binary_path = "stdlib_atoi_invalid.bin";
    output_path = "stdlib_atoi_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOI-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(8) VALUE \"123A5\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT PIC S9(9) VALUE 000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(9).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOI' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOI-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atoi(&library_text),
            "atoi generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, " 000000000\n1\n",
            "atoi helper should reject non-digit characters") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atol_converts_large_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[160];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atol_lib.cob";
    driver_path = "stdlib_atol_drv.cob";
    binary_path = "stdlib_atol.bin";
    output_path = "stdlib_atol.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(32) VALUE \"123456789012345678\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +18.\n"
        "       01 RESULT PIC S9(18) VALUE 000000000000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(18).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOL' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atol(&library_text),
            "atol generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, " 123456789012345678\n0\n",
            "atol helper should convert large positive input") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atol_detects_overflow)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[160];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atol_overflow_lib.cob";
    driver_path = "stdlib_atol_overflow_drv.cob";
    binary_path = "stdlib_atol_overflow.bin";
    output_path = "stdlib_atol_overflow.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOL-OVERFLOW-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(32) VALUE \"1234567890123456789\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +19.\n"
        "       01 RESULT PIC S9(18) VALUE 000000000000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(18).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOL' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOL-OVERFLOW-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atol(&library_text),
            "atol generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, " 000000000000000000\n1\n",
            "atol helper should report overflow for wide input") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atoll_converts_extended_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[256];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atoll_lib.cob";
    driver_path = "stdlib_atoll_drv.cob";
    binary_path = "stdlib_atoll.bin";
    output_path = "stdlib_atoll.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOLL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(64) VALUE \"-123456789012345678901234567890\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +31.\n"
        "       01 RESULT PIC S9(36) VALUE 000000000000000000000000000000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(36).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOLL' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOLL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atoll(&library_text),
            "atoll generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "-000000123456789012345678901234567890\n0\n",
            "atoll helper should convert extended negative input") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_atoll_rejects_trailing_garbage)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[256];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_atoll_invalid_lib.cob";
    driver_path = "stdlib_atoll_invalid_drv.cob";
    binary_path = "stdlib_atoll_invalid.bin";
    output_path = "stdlib_atoll_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ATOLL-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE PIC X(64) VALUE \"123456XYZ\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +9.\n"
        "       01 RESULT PIC S9(36) VALUE 000000000000000000000000000000000000.\n"
        "       01 STATUS PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(36).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ATOLL' USING BY REFERENCE SOURCE\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE RESULT\n"
        "               BY REFERENCE STATUS.\n"
        "           MOVE RESULT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ATOLL-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_atoll(&library_text),
            "atoll generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer,
            " 000000000000000000000000000000000000\n1\n",
            "atoll helper should flag trailing garbage") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
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
    if (test_expect_success(transpiler_standard_library_generate_strlen(&program_text),
            "strlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_string_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRLEN-STRING.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE.\n"
        "          05 LNK-SOURCE-LEN PIC 9(4) COMP.\n"
        "          05 LNK-SOURCE-BUF PIC X(255).\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE LNK-SOURCE-LEN TO LNK-RESULT.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRLEN-STRING.\n";
    if (test_expect_success(transpiler_standard_library_generate_strlen_string(&program_text),
            "string strlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "string strlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_lookup_enforces_std_prefix)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::abs");
    if (!entry)
    {
        pf_printf("Assertion failed: std::abs should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ABS", ft_strlen("CBLC-ABS") + 1) != 0)
    {
        pf_printf("Assertion failed: std::abs should map to CBLC-ABS program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::atoi");
    if (!entry)
    {
        pf_printf("Assertion failed: std::atoi should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOI", ft_strlen("CBLC-ATOI") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atoi should map to CBLC-ATOI program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::atol");
    if (!entry)
    {
        pf_printf("Assertion failed: std::atol should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOL", ft_strlen("CBLC-ATOL") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atol should map to CBLC-ATOL program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::atoll");
    if (!entry)
    {
        pf_printf("Assertion failed: std::atoll should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ATOLL", ft_strlen("CBLC-ATOLL") + 1) != 0)
    {
        pf_printf("Assertion failed: std::atoll should map to CBLC-ATOLL program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::fabs");
    if (!entry)
    {
        pf_printf("Assertion failed: std::fabs should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-FABS", ft_strlen("CBLC-FABS") + 1) != 0)
    {
        pf_printf("Assertion failed: std::fabs should map to CBLC-FABS program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::floor");
    if (!entry)
    {
        pf_printf("Assertion failed: std::floor should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-FLOOR", ft_strlen("CBLC-FLOOR") + 1) != 0)
    {
        pf_printf("Assertion failed: std::floor should map to CBLC-FLOOR program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::ceil");
    if (!entry)
    {
        pf_printf("Assertion failed: std::ceil should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-CEIL", ft_strlen("CBLC-CEIL") + 1) != 0)
    {
        pf_printf("Assertion failed: std::ceil should map to CBLC-CEIL program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::exp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::exp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-EXP", ft_strlen("CBLC-EXP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::exp should map to CBLC-EXP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::log");
    if (!entry)
    {
        pf_printf("Assertion failed: std::log should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-LOG", ft_strlen("CBLC-LOG") + 1) != 0)
    {
        pf_printf("Assertion failed: std::log should map to CBLC-LOG program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::sin");
    if (!entry)
    {
        pf_printf("Assertion failed: std::sin should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-SIN", ft_strlen("CBLC-SIN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::sin should map to CBLC-SIN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::cos");
    if (!entry)
    {
        pf_printf("Assertion failed: std::cos should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-COS", ft_strlen("CBLC-COS") + 1) != 0)
    {
        pf_printf("Assertion failed: std::cos should map to CBLC-COS program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::tan");
    if (!entry)
    {
        pf_printf("Assertion failed: std::tan should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TAN", ft_strlen("CBLC-TAN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::tan should map to CBLC-TAN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strlen");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strlen should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRLEN", ft_strlen("CBLC-STRLEN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strlen should map to CBLC-STRLEN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup_with_buffer_kind("std::strlen",
        TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING);
    if (!entry)
    {
        pf_printf("Assertion failed: std::strlen overload for string should resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRLEN-STRING",
            ft_strlen("CBLC-STRLEN-STRING") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strlen overload for string should map to CBLC-STRLEN-STRING program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strnlen");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strnlen should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNLEN", ft_strlen("CBLC-STRNLEN") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strnlen should map to CBLC-STRNLEN program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcmp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcmp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCMP", ft_strlen("CBLC-STRCMP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcmp should map to CBLC-STRCMP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcpy");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcpy should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCPY", ft_strlen("CBLC-STRCPY") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcpy should map to CBLC-STRCPY program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strncpy");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strncpy should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRNCPY", ft_strlen("CBLC-STRNCPY") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strncpy should map to CBLC-STRNCPY program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::memcmp");
    if (!entry)
    {
        pf_printf("Assertion failed: std::memcmp should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-MEMCMP", ft_strlen("CBLC-MEMCMP") + 1) != 0)
    {
        pf_printf("Assertion failed: std::memcmp should map to CBLC-MEMCMP program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strcat");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strcat should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRCAT", ft_strlen("CBLC-STRCAT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strcat should map to CBLC-STRCAT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::strtod");
    if (!entry)
    {
        pf_printf("Assertion failed: std::strtod should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-STRTOD", ft_strlen("CBLC-STRTOD") + 1) != 0)
    {
        pf_printf("Assertion failed: std::strtod should map to CBLC-STRTOD program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::pow");
    if (!entry)
    {
        pf_printf("Assertion failed: std::pow should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-POWEROF", ft_strlen("CBLC-POWEROF") + 1) != 0)
    {
        pf_printf("Assertion failed: std::pow should map to CBLC-POWEROF program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::sqrt");
    if (!entry)
    {
        pf_printf("Assertion failed: std::sqrt should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-SQRT", ft_strlen("CBLC-SQRT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::sqrt should map to CBLC-SQRT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::toupper");
    if (!entry)
    {
        pf_printf("Assertion failed: std::toupper should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TOUPPER", ft_strlen("CBLC-TOUPPER") + 1) != 0)
    {
        pf_printf("Assertion failed: std::toupper should map to CBLC-TOUPPER program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::tolower");
    if (!entry)
    {
        pf_printf("Assertion failed: std::tolower should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-TOLOWER", ft_strlen("CBLC-TOLOWER") + 1) != 0)
    {
        pf_printf("Assertion failed: std::tolower should map to CBLC-TOLOWER program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::isdigit");
    if (!entry)
    {
        pf_printf("Assertion failed: std::isdigit should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ISDIGIT", ft_strlen("CBLC-ISDIGIT") + 1) != 0)
    {
        pf_printf("Assertion failed: std::isdigit should map to CBLC-ISDIGIT program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::isalpha");
    if (!entry)
    {
        pf_printf("Assertion failed: std::isalpha should resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entry->program_name, "CBLC-ISALPHA", ft_strlen("CBLC-ISALPHA") + 1) != 0)
    {
        pf_printf("Assertion failed: std::isalpha should map to CBLC-ISALPHA program\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strlen");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strlen should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strnlen");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strnlen should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcmp");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcmp should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcpy");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcpy should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strncpy");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strncpy should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("memcmp");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified memcmp should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strcat");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strcat should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("fabs");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified fabs should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("floor");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified floor should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("ceil");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified ceil should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("strtod");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified strtod should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("sqrt");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified sqrt should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("toupper");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified toupper should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("tolower");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified tolower should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("abs");
    if (entry)
    {
        pf_printf("Assertion failed: unqualified abs should not resolve to standard library entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_lookup_is_case_sensitive)
{
    const t_transpiler_standard_library_entry *entry;

    entry = transpiler_standard_library_lookup("std::STRLEN");
    if (entry)
    {
        pf_printf("Assertion failed: std::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::strlen");
    if (entry)
    {
        pf_printf("Assertion failed: STD::strlen should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::STRLEN");
    if (entry)
    {
        pf_printf("Assertion failed: STD::STRLEN should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("std::MEMCMP");
    if (entry)
    {
        pf_printf("Assertion failed: std::MEMCMP should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    entry = transpiler_standard_library_lookup("STD::memcmp");
    if (entry)
    {
        pf_printf("Assertion failed: STD::memcmp should not resolve to catalog entry\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_catalog_lists_all_entries)
{
    const t_transpiler_standard_library_entry *entries;
    size_t count;

    entries = transpiler_standard_library_get_entries(&count);
    if (!entries)
    {
        pf_printf("Assertion failed: catalog should return entry table\n");
        return (FT_FAILURE);
    }
    if (count != 27)
    {
        pf_printf("Assertion failed: catalog should report twenty-seven standard library entries but returned %u\n",
            static_cast<unsigned int>(count));
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[0].qualified_name, "std::abs", ft_strlen("std::abs") + 1) != 0)
    {
        pf_printf("Assertion failed: first catalog entry should be std::abs\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[1].qualified_name, "std::atoi", ft_strlen("std::atoi") + 1) != 0)
    {
        pf_printf("Assertion failed: second catalog entry should be std::atoi\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[2].qualified_name, "std::atol", ft_strlen("std::atol") + 1) != 0)
    {
        pf_printf("Assertion failed: third catalog entry should be std::atol\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[3].qualified_name, "std::atoll", ft_strlen("std::atoll") + 1) != 0)
    {
        pf_printf("Assertion failed: fourth catalog entry should be std::atoll\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[4].qualified_name, "std::fabs", ft_strlen("std::fabs") + 1) != 0)
    {
        pf_printf("Assertion failed: fifth catalog entry should be std::fabs\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[5].qualified_name, "std::floor", ft_strlen("std::floor") + 1) != 0)
    {
        pf_printf("Assertion failed: sixth catalog entry should be std::floor\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[6].qualified_name, "std::ceil", ft_strlen("std::ceil") + 1) != 0)
    {
        pf_printf("Assertion failed: seventh catalog entry should be std::ceil\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[7].qualified_name, "std::exp", ft_strlen("std::exp") + 1) != 0)
    {
        pf_printf("Assertion failed: eighth catalog entry should be std::exp\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[8].qualified_name, "std::log", ft_strlen("std::log") + 1) != 0)
    {
        pf_printf("Assertion failed: ninth catalog entry should be std::log\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[9].qualified_name, "std::sin", ft_strlen("std::sin") + 1) != 0)
    {
        pf_printf("Assertion failed: tenth catalog entry should be std::sin\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[10].qualified_name, "std::cos", ft_strlen("std::cos") + 1) != 0)
    {
        pf_printf("Assertion failed: eleventh catalog entry should be std::cos\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[11].qualified_name, "std::tan", ft_strlen("std::tan") + 1) != 0)
    {
        pf_printf("Assertion failed: twelfth catalog entry should be std::tan\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[12].qualified_name, "std::strlen", ft_strlen("std::strlen") + 1) != 0)
    {
        pf_printf("Assertion failed: thirteenth catalog entry should be std::strlen\n");
        return (FT_FAILURE);
    }
    if (entries[12].buffer_kind != TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR)
    {
        pf_printf("Assertion failed: thirteenth catalog entry should target char buffers\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[13].qualified_name, "std::strlen", ft_strlen("std::strlen") + 1) != 0)
    {
        pf_printf("Assertion failed: fourteenth catalog entry should be std::strlen overload\n");
        return (FT_FAILURE);
    }
    if (entries[13].buffer_kind != TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING)
    {
        pf_printf("Assertion failed: fourteenth catalog entry should target string buffers\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[14].qualified_name, "std::strnlen", ft_strlen("std::strnlen") + 1) != 0)
    {
        pf_printf("Assertion failed: fifteenth catalog entry should be std::strnlen\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[15].qualified_name, "std::strcmp", ft_strlen("std::strcmp") + 1) != 0)
    {
        pf_printf("Assertion failed: sixteenth catalog entry should be std::strcmp\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[16].qualified_name, "std::strcpy", ft_strlen("std::strcpy") + 1) != 0)
    {
        pf_printf("Assertion failed: seventeenth catalog entry should be std::strcpy\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[17].qualified_name, "std::strncpy", ft_strlen("std::strncpy") + 1) != 0)
    {
        pf_printf("Assertion failed: eighteenth catalog entry should be std::strncpy\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[18].qualified_name, "std::memcmp", ft_strlen("std::memcmp") + 1) != 0)
    {
        pf_printf("Assertion failed: nineteenth catalog entry should be std::memcmp\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[19].qualified_name, "std::strcat", ft_strlen("std::strcat") + 1) != 0)
    {
        pf_printf("Assertion failed: twentieth catalog entry should be std::strcat\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[20].qualified_name, "std::strtod", ft_strlen("std::strtod") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-first catalog entry should be std::strtod\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[21].qualified_name, "std::pow", ft_strlen("std::pow") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-second catalog entry should be std::pow\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[22].qualified_name, "std::sqrt", ft_strlen("std::sqrt") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-third catalog entry should be std::sqrt\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[23].qualified_name, "std::toupper", ft_strlen("std::toupper") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-fourth catalog entry should be std::toupper\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[24].qualified_name, "std::tolower", ft_strlen("std::tolower") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-fifth catalog entry should be std::tolower\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[25].qualified_name, "std::isdigit", ft_strlen("std::isdigit") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-sixth catalog entry should be std::isdigit\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(entries[26].qualified_name, "std::isalpha", ft_strlen("std::isalpha") + 1) != 0)
    {
        pf_printf("Assertion failed: twenty-seventh catalog entry should be std::isalpha\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_generators_validate_out_parameter)
{
    if (transpiler_standard_library_generate_abs(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: abs generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_fabs(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: fabs generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strlen(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strlen generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strnlen(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strnlen generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcmp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcmp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcpy(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcpy generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strncpy(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strncpy generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_memcmp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: memcmp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strcat(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strcat generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_strtod(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: strtod generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_exp(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: exp generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_log(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: log generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_sin(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: sin generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_cos(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: cos generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_tan(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: tan generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_powerof(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: powerof generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_sqrt(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: sqrt generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_toupper(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: toupper generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_tolower(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: tolower generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atoi(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atoi generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atol(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atol generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    if (transpiler_standard_library_generate_atoll(NULL) != FT_FAILURE)
    {
        pf_printf("Assertion failed: atoll generator should reject NULL output pointer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_executes)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strlen_lib.cob";
    driver_path = "stdlib_strlen_drv.cob";
    binary_path = "stdlib_strlen.bin";
    output_path = "stdlib_strlen.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRLEN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLO     \".\n"
        "       01 DECLARED-LENGTH PIC 9(9) VALUE 000000010.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRLEN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&library_text),
            "strlen generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000005\n",
            "strlen helper should report trimmed length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strlen_handles_all_spaces)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strlen_spc_lib.cob";
    driver_path = "stdlib_strlen_spc_drv.cob";
    binary_path = "stdlib_strlen_spc.bin";
    output_path = "stdlib_strlen_spc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRLEN-SPACES-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DECLARED-LENGTH PIC 9(9) VALUE 000000010.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000999.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRLEN-SPACES-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strlen(&library_text),
            "strlen generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000000\n",
            "strlen helper should report zero for all-space buffer") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRNLEN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 DECLARED-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 REQUEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-DECLARED-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-REQUEST-LIMIT PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-DECLARED-LENGTH BY VALUE LNK-REQUEST-LIMIT\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE 0 TO DECLARED-LIMIT.\n"
        "           IF LNK-DECLARED-LENGTH > 0\n"
        "               MOVE LNK-DECLARED-LENGTH TO DECLARED-LIMIT\n"
        "           END-IF.\n"
        "           IF DECLARED-LIMIT > 255\n"
        "               MOVE 255 TO DECLARED-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO REQUEST-LIMIT.\n"
        "           IF LNK-REQUEST-LIMIT > 0\n"
        "               MOVE LNK-REQUEST-LIMIT TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT > 255\n"
        "               MOVE 255 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT < DECLARED-LIMIT\n"
        "               MOVE REQUEST-LIMIT TO DECLARED-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DECLARED-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-SOURCE(IDX:1) NOT = SPACE\n"
        "                   MOVE IDX TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRNLEN.\n";
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&program_text),
            "strnlen generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strnlen generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_respects_request_limit)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_lim_lib.cob";
    driver_path = "stdlib_strnlen_lim_drv.cob";
    binary_path = "stdlib_strnlen_lim.bin";
    output_path = "stdlib_strnlen_lim.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-LIMIT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(12) VALUE \"HELLOWORLD  \".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +12.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-LIMIT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000005\n",
            "strnlen helper should clamp result to requested limit") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_honors_declared_length)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_decl_lib.cob";
    driver_path = "stdlib_strnlen_decl_drv.cob";
    binary_path = "stdlib_strnlen_decl.bin";
    output_path = "stdlib_strnlen_decl.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-DECL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"ABCD      \".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +4.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +9.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000123.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-DECL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000004\n",
            "strnlen helper should respect caller-declared length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_trims_trailing_spaces)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_spc_lib.cob";
    driver_path = "stdlib_strnlen_spc_drv.cob";
    binary_path = "stdlib_strnlen_spc.bin";
    output_path = "stdlib_strnlen_spc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-SPACES-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(5) VALUE \"AB   \".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000321.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-SPACES-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000002\n",
            "strnlen helper should trim trailing spaces within the scan window") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strnlen_stops_at_low_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[64];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strnlen_nul_lib.cob";
    driver_path = "stdlib_strnlen_nul_drv.cob";
    binary_path = "stdlib_strnlen_nul.bin";
    output_path = "stdlib_strnlen_nul.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNLEN-NUL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(6) VALUE \"ABCDEF\".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 REQUEST-LIMIT PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000111.\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE LOW-VALUE TO SOURCE-BUFFER(3:1).\n"
        "           CALL 'CBLC-STRNLEN' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY VALUE REQUEST-LIMIT\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNLEN-NUL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strnlen(&library_text),
            "strnlen generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000002\n",
            "strnlen helper should stop scanning at LOW-VALUE bytes") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcmp_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCMP.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 FIRST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 SECOND-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COMPARE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-FIRST PIC X(255).\n"
        "       01 LNK-FIRST-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SECOND PIC X(255).\n"
        "       01 LNK-SECOND-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-FIRST\n"
        "           BY VALUE LNK-FIRST-LENGTH BY REFERENCE LNK-SECOND\n"
        "           BY VALUE LNK-SECOND-LENGTH BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-FIRST-LENGTH TO FIRST-LIMIT.\n"
        "           IF FIRST-LIMIT > 255\n"
        "               MOVE 255 TO FIRST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SECOND-LENGTH TO SECOND-LIMIT.\n"
        "           IF SECOND-LIMIT > 255\n"
        "               MOVE 255 TO SECOND-LIMIT\n"
        "           END-IF.\n"
        "           MOVE FIRST-LIMIT TO COMPARE-LIMIT.\n"
        "           IF SECOND-LIMIT < COMPARE-LIMIT\n"
        "               MOVE SECOND-LIMIT TO COMPARE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COMPARE-LIMIT\n"
        "               IF LNK-FIRST(IDX:1) < LNK-SECOND(IDX:1)\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-FIRST(IDX:1) > LNK-SECOND(IDX:1)\n"
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
        "       END PROGRAM CBLC-STRCMP.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcmp(&program_text),
            "strcmp generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcmp generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcmp_executes)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strcmp_lib.cob";
    driver_path = "stdlib_strcmp_drv.cob";
    binary_path = "stdlib_strcmp.bin";
    output_path = "stdlib_strcmp.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCMP-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 FIRST-BUFFER PIC X(10) VALUE \"APPLE     \".\n"
        "       01 SECOND-BUFFER PIC X(10) VALUE \"APPLE     \".\n"
        "       01 FIRST-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 SECOND-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 RESULT PIC S9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"APRIC     \" TO SECOND-BUFFER.\n"
        "           MOVE +5 TO SECOND-LENGTH.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-STRCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"APPL      \" TO SECOND-BUFFER.\n"
        "           MOVE +4 TO SECOND-LENGTH.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-STRCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCMP-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcmp(&library_text),
            "strcmp generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "EQUAL\nLESS\nGREATER\n",
            "strcmp helper should report lexicographic ordering") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_memcmp_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-MEMCMP.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 FIRST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 SECOND-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 REQUEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 COMPARE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-FIRST PIC X(255).\n"
        "       01 LNK-FIRST-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SECOND PIC X(255).\n"
        "       01 LNK-SECOND-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-COUNT PIC 9(9).\n"
        "       01 LNK-RESULT PIC S9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-FIRST\n"
        "           BY VALUE LNK-FIRST-LENGTH BY REFERENCE LNK-SECOND\n"
        "           BY VALUE LNK-SECOND-LENGTH BY VALUE LNK-COUNT\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-FIRST-LENGTH TO FIRST-LIMIT.\n"
        "           IF FIRST-LIMIT > 255\n"
        "               MOVE 255 TO FIRST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-SECOND-LENGTH TO SECOND-LIMIT.\n"
        "           IF SECOND-LIMIT > 255\n"
        "               MOVE 255 TO SECOND-LIMIT\n"
        "           END-IF.\n"
        "           MOVE LNK-COUNT TO REQUEST-LIMIT.\n"
        "           IF REQUEST-LIMIT > 255\n"
        "               MOVE 255 TO REQUEST-LIMIT\n"
        "           END-IF.\n"
        "           MOVE FIRST-LIMIT TO COMPARE-LIMIT.\n"
        "           IF SECOND-LIMIT < COMPARE-LIMIT\n"
        "               MOVE SECOND-LIMIT TO COMPARE-LIMIT\n"
        "           END-IF.\n"
        "           IF REQUEST-LIMIT < COMPARE-LIMIT\n"
        "               MOVE REQUEST-LIMIT TO COMPARE-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > COMPARE-LIMIT\n"
        "               IF LNK-FIRST(IDX:1) < LNK-SECOND(IDX:1)\n"
        "                   MOVE -1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               IF LNK-FIRST(IDX:1) > LNK-SECOND(IDX:1)\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF REQUEST-LIMIT > COMPARE-LIMIT\n"
        "                   IF FIRST-LIMIT < SECOND-LIMIT\n"
        "                       IF FIRST-LIMIT < REQUEST-LIMIT\n"
        "                           MOVE -1 TO LNK-RESULT\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           IF LNK-RESULT = 0\n"
        "               IF REQUEST-LIMIT > COMPARE-LIMIT\n"
        "                   IF SECOND-LIMIT < FIRST-LIMIT\n"
        "                       IF SECOND-LIMIT < REQUEST-LIMIT\n"
        "                           MOVE 1 TO LNK-RESULT\n"
        "                       END-IF\n"
        "                   END-IF\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-MEMCMP.\n";
    if (test_expect_success(transpiler_standard_library_generate_memcmp(&program_text),
            "memcmp generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "memcmp generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_memcmp_executes)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_memcmp_lib.cob";
    driver_path = "stdlib_memcmp_drv.cob";
    binary_path = "stdlib_memcmp.bin";
    output_path = "stdlib_memcmp.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MEMCMP-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 FIRST-BUFFER PIC X(8) VALUE \"ABCDEF  \".\n"
        "       01 SECOND-BUFFER PIC X(8) VALUE \"ABCDEF  \".\n"
        "       01 FIRST-LENGTH PIC S9(9) COMP-5 VALUE +8.\n"
        "       01 SECOND-LENGTH PIC S9(9) COMP-5 VALUE +8.\n"
        "       01 REQUEST-COUNT PIC 9(9) VALUE 000000006.\n"
        "       01 RESULT PIC S9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-MEMCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY VALUE REQUEST-COUNT\n"
        "               BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"ABDDEF  \" TO SECOND-BUFFER.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-MEMCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY VALUE REQUEST-COUNT\n"
        "               BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"ABBDEF  \" TO SECOND-BUFFER.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-MEMCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY VALUE REQUEST-COUNT\n"
        "               BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           MOVE \"ABCD    \" TO FIRST-BUFFER.\n"
        "           MOVE \"ABCDXY  \" TO SECOND-BUFFER.\n"
        "           MOVE +4 TO FIRST-LENGTH.\n"
        "           MOVE +8 TO SECOND-LENGTH.\n"
        "           MOVE 000000006 TO REQUEST-COUNT.\n"
        "           MOVE 0 TO RESULT.\n"
        "           CALL 'CBLC-MEMCMP' USING BY REFERENCE FIRST-BUFFER\n"
        "               BY VALUE FIRST-LENGTH BY REFERENCE SECOND-BUFFER\n"
        "               BY VALUE SECOND-LENGTH BY VALUE REQUEST-COUNT\n"
        "               BY REFERENCE RESULT.\n"
        "           IF RESULT < 0\n"
        "               DISPLAY \"LESS\"\n"
        "           END-IF.\n"
        "           IF RESULT = 0\n"
        "               DISPLAY \"EQUAL\"\n"
        "           END-IF.\n"
        "           IF RESULT > 0\n"
        "               DISPLAY \"GREATER\"\n"
        "           END-IF.\n"
        "           STOP RUN.\n"
        "       END PROGRAM MEMCMP-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_memcmp(&library_text),
            "memcmp generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "EQUAL\nLESS\nGREATER\nLESS\n",
            "memcmp helper should report byte-wise ordering without overruns") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcpy_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRCPY.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SOURCE-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 DEST-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-DESTINATION PIC X(255).\n"
        "       01 LNK-DESTINATION-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-DESTINATION\n"
        "           BY VALUE LNK-DESTINATION-LENGTH BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-STATUS.\n"
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
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > DEST-LIMIT\n"
        "               MOVE SPACE TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SOURCE-LIMIT\n"
        "               IF IDX > DEST-LIMIT\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-SOURCE(IDX:1) TO LNK-DESTINATION(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRCPY.\n";
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&program_text),
            "strcpy generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcpy generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcpy_executes_without_truncation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strcpy_lib.cob";
    driver_path = "stdlib_strcpy_drv.cob";
    binary_path = "stdlib_strcpy.bin";
    output_path = "stdlib_strcpy.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCPY-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLO     \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCPY-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&library_text),
            "strcpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HELLO     <\n000000000\n",
            "strcpy helper should copy source into destination without truncation") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcpy_blanks_destination)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strcpy_blk_lib.cob";
    driver_path = "stdlib_strcpy_blk_drv.cob";
    binary_path = "stdlib_strcpy_blk.bin";
    output_path = "stdlib_strcpy_blk.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCPY-BLANK-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"XXXXXXXXXX\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"TEST      \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +4.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \"<\" DEST-BUFFER \">\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCPY-BLANK-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&library_text),
            "strcpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "<TEST      >\n000000000\n",
            "strcpy helper should blank destination before copying") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcpy_reports_truncation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strcpy_trc_lib.cob";
    driver_path = "stdlib_strcpy_trc_drv.cob";
    binary_path = "stdlib_strcpy_trc.bin";
    output_path = "stdlib_strcpy_trc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCPY-TRUNC-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +3.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLOWORLD\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCPY-TRUNC-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcpy(&library_text),
            "strcpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HEL       <\n000000001\n",
            "strcpy helper should flag truncation and preserve destination limit") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
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
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&program_text),
            "strncpy generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strncpy generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_sqrt_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-SQRT.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-OPERAND < 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION SQRT(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-SQRT.\n";
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&program_text),
            "sqrt generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "sqrt generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_executes_without_truncation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strncpy_lib.cob";
    driver_path = "stdlib_strncpy_drv.cob";
    binary_path = "stdlib_strncpy.bin";
    output_path = "stdlib_strncpy.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLOWORLD\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +4.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HELL      <\n000000000\n",
            "strncpy helper should copy requested subset without truncation") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_reports_truncation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strncpy_trc_lib.cob";
    driver_path = "stdlib_strncpy_trc_drv.cob";
    binary_path = "stdlib_strncpy_trc.bin";
    output_path = "stdlib_strncpy_trc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-TRUNC-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"          \".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +3.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HELLOWORLD\".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-TRUNC-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HEL       <\n000000001\n",
            "strncpy helper should flag truncation when destination is smaller than request") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_reports_short_source)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strncpy_shr_lib.cob";
    driver_path = "stdlib_strncpy_shr_drv.cob";
    binary_path = "stdlib_strncpy_shr.bin";
    output_path = "stdlib_strncpy_shr.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-SHORT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"XXXXXXXXXX\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"HI        \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +2.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-SHORT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HI        <\n000000001\n",
            "strncpy helper should pad spaces and flag short source requests") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strncpy_honors_zero_request)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strncpy_zro_lib.cob";
    driver_path = "stdlib_strncpy_zro_drv.cob";
    binary_path = "stdlib_strncpy_zro.bin";
    output_path = "stdlib_strncpy_zro.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRNCPY-ZERO-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(10) VALUE \"XXXXXXXXXX\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +10.\n"
        "       01 SOURCE-BUFFER PIC X(10) VALUE \"SAMPLE    \".\n"
        "       01 SOURCE-LENGTH PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 REQUEST-LEN PIC S9(9) COMP-5 VALUE +0.\n"
        "       01 COPY-STATUS PIC 9(9) VALUE 000000123.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRNCPY' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE SOURCE-LENGTH BY VALUE REQUEST-LEN\n"
        "               BY REFERENCE COPY-STATUS.\n"
        "           DISPLAY \"<\" DEST-BUFFER \">\".\n"
        "           DISPLAY COPY-STATUS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRNCPY-ZERO-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strncpy(&library_text),
            "strncpy generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "<          >\n000000000\n",
            "strncpy helper should blank destination when zero bytes requested") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcat_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
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
        "           MOVE 0 TO LNK-STATUS.\n"
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
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               ADD 1 TO LNK-RESULT-LENGTH\n"
        "               MOVE LNK-LEFT(IDX:1) TO LNK-DESTINATION(LNK-RESULT-LENGTH:1)\n"
        "           END-PERFORM.\n"
        "           IF LNK-STATUS = 0\n"
        "               MOVE 0 TO IDX\n"
        "               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > RIGHT-LIMIT\n"
        "                   IF LNK-RESULT-LENGTH >= DEST-LIMIT\n"
        "                       MOVE 1 TO LNK-STATUS\n"
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
    if (test_expect_success(transpiler_standard_library_generate_strcat(&program_text),
            "strcat generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strcat generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_abs_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ABS.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND PIC S9(18) COMP-5.\n"
        "       01 LNK-RESULT PIC S9(18) COMP-5.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-OPERAND TO LNK-RESULT.\n"
        "           IF LNK-RESULT < 0\n"
        "               COMPUTE LNK-RESULT = 0 - LNK-RESULT\n"
        "                   ON SIZE ERROR\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       MOVE 0 TO LNK-RESULT\n"
        "               END-COMPUTE\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ABS.\n";
    if (test_expect_success(transpiler_standard_library_generate_abs(&program_text),
            "abs generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "abs generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_fabs_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-FABS.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION ABS(LNK-OPERAND).\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-FABS.\n";
    if (test_expect_success(transpiler_standard_library_generate_fabs(&program_text),
            "fabs generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "fabs generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_floor_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-FLOOR.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION INTEGER-PART(LNK-OPERAND).\n"
        "           IF LNK-OPERAND NOT = LNK-RESULT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               IF LNK-OPERAND < 0\n"
        "                   COMPUTE LNK-RESULT = LNK-RESULT - 1\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-FLOOR.\n";
    if (test_expect_success(transpiler_standard_library_generate_floor(&program_text),
            "floor generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "floor generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_ceil_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-CEIL.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION INTEGER-PART(LNK-OPERAND).\n"
        "           IF LNK-OPERAND NOT = LNK-RESULT\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               IF LNK-OPERAND > 0\n"
        "                   COMPUTE LNK-RESULT = LNK-RESULT + 1\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-CEIL.\n";
    if (test_expect_success(transpiler_standard_library_generate_ceil(&program_text),
            "ceil generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "ceil generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_exp_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-EXP.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION EXP(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-EXP.\n";
    if (test_expect_success(transpiler_standard_library_generate_exp(&program_text),
            "exp generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "exp generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_log_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-LOG.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-OPERAND <= 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION LOG(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-LOG.\n";
    if (test_expect_success(transpiler_standard_library_generate_log(&program_text),
            "log generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "log generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strtod_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-STRTOD.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 START-INDEX PIC 9(9) VALUE 000000001.\n"
        "       01 END-INDEX PIC 9(9) VALUE 000000000.\n"
        "       01 NORMALIZED-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       01 NORMALIZED-BUFFER PIC X(255) VALUE SPACES.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       01 HAS-ANY-DIGIT PIC 9 VALUE 0.\n"
        "       01 HAS-DECIMAL PIC 9 VALUE 0.\n"
        "       01 HAS-EXPONENT PIC 9 VALUE 0.\n"
        "       01 EXPONENT-DIGITS PIC 9(9) VALUE 000000000.\n"
        "       01 EXPECT-EXPONENT-SIGN PIC 9 VALUE 0.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-SOURCE PIC X(255).\n"
        "       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE\n"
        "           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-SOURCE-LENGTH TO SCAN-LIMIT.\n"
        "           IF SCAN-LIMIT > 255\n"
        "               MOVE 255 TO SCAN-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO ACTUAL-LENGTH.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT\n"
        "               IF LNK-SOURCE(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE IDX TO ACTUAL-LENGTH\n"
        "           END-PERFORM.\n"
        "           IF ACTUAL-LENGTH = 0\n"
        "               MOVE SCAN-LIMIT TO ACTUAL-LENGTH\n"
        "           END-IF.\n"
        "           MOVE 1 TO START-INDEX.\n"
        "           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH\n"
        "               IF LNK-SOURCE(START-INDEX:1) NOT = SPACE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "           END-PERFORM.\n"
        "           IF START-INDEX > ACTUAL-LENGTH\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE ACTUAL-LENGTH TO END-INDEX.\n"
        "           PERFORM UNTIL END-INDEX < START-INDEX\n"
        "               MOVE LNK-SOURCE(END-INDEX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR NOT = SPACE AND CURRENT-CHAR NOT = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               COMPUTE END-INDEX = END-INDEX - 1\n"
        "           END-PERFORM.\n"
        "           IF END-INDEX < START-INDEX\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE 0 TO NORMALIZED-LENGTH.\n"
        "           MOVE SPACES TO NORMALIZED-BUFFER.\n"
        "           MOVE 0 TO HAS-ANY-DIGIT.\n"
        "           MOVE 0 TO HAS-DECIMAL.\n"
        "           MOVE 0 TO HAS-EXPONENT.\n"
        "           MOVE 0 TO EXPONENT-DIGITS.\n"
        "           MOVE 0 TO EXPECT-EXPONENT-SIGN.\n"
        "           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > END-INDEX\n"
        "               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR\n"
        "               IF CURRENT-CHAR = SPACE OR CURRENT-CHAR = LOW-VALUE\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "               END-IF\n"
        "               EVALUATE TRUE\n"
        "                   WHEN CURRENT-CHAR >= \"0\" AND CURRENT-CHAR <= \"9\"\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-ANY-DIGIT\n"
        "                       IF HAS-EXPONENT = 1\n"
        "                           ADD 1 TO EXPONENT-DIGITS\n"
        "                       END-IF\n"
        "                       MOVE 0 TO EXPECT-EXPONENT-SIGN\n"
        "                   WHEN CURRENT-CHAR = \".\"\n"
        "                       IF HAS-DECIMAL = 1 OR HAS-EXPONENT = 1\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE \".\" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-DECIMAL\n"
        "                   WHEN CURRENT-CHAR = \"E\" OR CURRENT-CHAR = \"e\"\n"
        "                       IF HAS-EXPONENT = 1 OR HAS-ANY-DIGIT = 0\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       IF NORMALIZED-LENGTH >= 255\n"
        "                           MOVE 1 TO LNK-STATUS\n"
        "                           MOVE 0 TO LNK-RESULT\n"
        "                           GOBACK\n"
        "                       END-IF\n"
        "                       ADD 1 TO NORMALIZED-LENGTH\n"
        "                       MOVE \"E\" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       MOVE 1 TO HAS-EXPONENT\n"
        "                       MOVE 0 TO EXPONENT-DIGITS\n"
        "                       MOVE 1 TO EXPECT-EXPONENT-SIGN\n"
        "                   WHEN CURRENT-CHAR = \"+\" OR CURRENT-CHAR = \"-\"\n"
        "                       IF NORMALIZED-LENGTH = 0\n"
        "                           IF NORMALIZED-LENGTH >= 255\n"
        "                               MOVE 1 TO LNK-STATUS\n"
        "                               MOVE 0 TO LNK-RESULT\n"
        "                               GOBACK\n"
        "                           END-IF\n"
        "                           ADD 1 TO NORMALIZED-LENGTH\n"
        "                           MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                       ELSE\n"
        "                           IF EXPECT-EXPONENT-SIGN = 1\n"
        "                               IF NORMALIZED-LENGTH >= 255\n"
        "                                   MOVE 1 TO LNK-STATUS\n"
        "                                   MOVE 0 TO LNK-RESULT\n"
        "                                   GOBACK\n"
        "                               END-IF\n"
        "                               ADD 1 TO NORMALIZED-LENGTH\n"
        "                               MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)\n"
        "                               MOVE 0 TO EXPECT-EXPONENT-SIGN\n"
        "                           ELSE\n"
        "                               MOVE 1 TO LNK-STATUS\n"
        "                               MOVE 0 TO LNK-RESULT\n"
        "                               GOBACK\n"
        "                           END-IF\n"
        "                       END-IF\n"
        "                   WHEN OTHER\n"
        "                       MOVE 1 TO LNK-STATUS\n"
        "                       MOVE 0 TO LNK-RESULT\n"
        "                       GOBACK\n"
        "               END-EVALUATE\n"
        "           END-PERFORM.\n"
        "           IF NORMALIZED-LENGTH = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF HAS-ANY-DIGIT = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF HAS-EXPONENT = 1 AND EXPONENT-DIGITS = 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           IF EXPECT-EXPONENT-SIGN = 1\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = FUNCTION NUMVAL(NORMALIZED-BUFFER(1:NORMALIZED-LENGTH))\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "                   GOBACK\n"
        "           END-COMPUTE.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-STRTOD.\n";
    if (test_expect_success(transpiler_standard_library_generate_strtod(&program_text),
            "strtod generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "strtod generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strtod_parses_scientific_notation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strtod_val_lib.cob";
    driver_path = "stdlib_strtod_val_drv.cob";
    binary_path = "stdlib_strtod_val.bin";
    output_path = "stdlib_strtod_val.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRTOD-VALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(255) VALUE \"  -125.0\".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +8.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRTOD' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRTOD-VALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strtod(&library_text),
            "strtod generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -125.0, g_default_float_tolerance,
            "strtod helper should parse signed decimal input accurately") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "strtod helper should report success for valid input") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "strtod helper should only emit expected transcript lines for valid input") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strtod_rejects_invalid_input)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strtod_inv_lib.cob";
    driver_path = "stdlib_strtod_inv_drv.cob";
    binary_path = "stdlib_strtod_inv.bin";
    output_path = "stdlib_strtod_inv.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRTOD-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 SOURCE-BUFFER PIC X(255) VALUE \"123A45\".\n"
        "       01 DECLARED-LENGTH PIC S9(9) COMP-5 VALUE +6.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRTOD' USING BY REFERENCE SOURCE-BUFFER\n"
        "               BY VALUE DECLARED-LENGTH BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRTOD-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strtod(&library_text),
            "strtod generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, " 0000.0000\n1\n",
            "strtod helper should reject invalid characters and report failure") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_abs_executes_for_common_cases)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[256];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_abs_common_lib.cob";
    driver_path = "stdlib_abs_common_drv.cob";
    binary_path = "stdlib_abs_common.bin";
    output_path = "stdlib_abs_common.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ABS-COMMON-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS PIC S9(18) COMP-5 VALUE +123.\n"
        "       01 RESULT-POS PIC S9(18) COMP-5 VALUE +0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 DISPLAY-POS PIC +9(9).\n"
        "       01 OPERAND-NEG PIC S9(18) COMP-5 VALUE -456.\n"
        "       01 RESULT-NEG PIC S9(18) COMP-5 VALUE +0.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 DISPLAY-NEG PIC +9(9).\n"
        "       01 OPERAND-ZRO PIC S9(18) COMP-5 VALUE +0.\n"
        "       01 RESULT-ZRO PIC S9(18) COMP-5 VALUE +999.\n"
        "       01 STATUS-ZRO PIC 9 VALUE 9.\n"
        "       01 DISPLAY-ZRO PIC +9(9).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO DISPLAY-POS.\n"
        "           DISPLAY DISPLAY-POS.\n"
        "           DISPLAY STATUS-POS.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO DISPLAY-NEG.\n"
        "           DISPLAY DISPLAY-NEG.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-ZRO\n"
        "               BY REFERENCE RESULT-ZRO BY REFERENCE STATUS-ZRO.\n"
        "           MOVE RESULT-ZRO TO DISPLAY-ZRO.\n"
        "           DISPLAY DISPLAY-ZRO.\n"
        "           DISPLAY STATUS-ZRO.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ABS-COMMON-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_abs(&library_text),
            "abs generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "+000000123\n0\n+000000456\n0\n+000000000\n0\n",
            "abs helper should return magnitudes and report success") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_abs_reports_overflow_for_min_value)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_abs_min_lib.cob";
    driver_path = "stdlib_abs_min_drv.cob";
    binary_path = "stdlib_abs_min.bin";
    output_path = "stdlib_abs_min.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ABS-MIN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-MIN PIC S9(18) COMP-5 VALUE -9223372036854775808.\n"
        "       01 RESULT-MIN PIC S9(18) COMP-5 VALUE +777.\n"
        "       01 STATUS-MIN PIC 9 VALUE 0.\n"
        "       01 DISPLAY-MIN PIC +9(19).\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ABS' USING BY REFERENCE OPERAND-MIN\n"
        "               BY REFERENCE RESULT-MIN BY REFERENCE STATUS-MIN.\n"
        "           MOVE RESULT-MIN TO DISPLAY-MIN.\n"
        "           DISPLAY DISPLAY-MIN.\n"
        "           DISPLAY STATUS-MIN.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ABS-MIN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_abs(&library_text),
            "abs generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "+0000000000000000000\n1\n",
            "abs helper should flag overflow for minimum negative value") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_fabs_executes_for_negative_and_positive_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_fabs_lib.cob";
    driver_path = "stdlib_fabs_drv.cob";
    binary_path = "stdlib_fabs.bin";
    output_path = "stdlib_fabs.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. FABS-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 DISPLAY-NEG PIC 9(4).9(4).\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 DISPLAY-POS PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -12.5 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-FABS' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO DISPLAY-NEG.\n"
        "           DISPLAY DISPLAY-NEG.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           MOVE 3.25 TO OPERAND-POS.\n"
        "           CALL 'CBLC-FABS' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO DISPLAY-POS.\n"
        "           DISPLAY DISPLAY-POS.\n"
        "           DISPLAY STATUS-POS.\n"
        "           STOP RUN.\n"
        "       END PROGRAM FABS-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_fabs(&library_text),
            "fabs generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 12.5, g_default_float_tolerance,
            "fabs helper should return magnitude for negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "fabs helper should report success for negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 3.25, g_default_float_tolerance,
            "fabs helper should return magnitude for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "fabs helper should report success for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "fabs helper should only emit expected transcript lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcat_executes_without_truncation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strcat_lib.cob";
    driver_path = "stdlib_strcat_drv.cob";
    binary_path = "stdlib_strcat.bin";
    output_path = "stdlib_strcat.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCAT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(20) VALUE \"HELLO\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +20.\n"
        "       01 LEFT-BUFFER PIC X(5) VALUE \" \".\n"
        "       01 LEFT-LENGTH PIC S9(9) COMP-5 VALUE +1.\n"
        "       01 RIGHT-BUFFER PIC X(10) VALUE \"WORLD\".\n"
        "       01 RIGHT-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 CAT-STATUS PIC 9(9) VALUE 000000009.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCAT' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE LEFT-BUFFER\n"
        "               BY VALUE LEFT-LENGTH BY REFERENCE RIGHT-BUFFER\n"
        "               BY VALUE RIGHT-LENGTH BY REFERENCE CAT-STATUS\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY CAT-STATUS.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCAT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcat(&library_text),
            "strcat generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HELLO WORLD         <\n000000000\n000000011\n",
            "strcat helper should append both sources and report final length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_strcat_reports_truncation)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_strcat_trc_lib.cob";
    driver_path = "stdlib_strcat_trc_drv.cob";
    binary_path = "stdlib_strcat_trc.bin";
    output_path = "stdlib_strcat_trc.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRCAT-TRUNC-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DEST-BUFFER PIC X(8) VALUE \"HELLO\".\n"
        "       01 DEST-LENGTH PIC S9(9) COMP-5 VALUE +8.\n"
        "       01 LEFT-BUFFER PIC X(5) VALUE \" \".\n"
        "       01 LEFT-LENGTH PIC S9(9) COMP-5 VALUE +1.\n"
        "       01 RIGHT-BUFFER PIC X(10) VALUE \"WORLD\".\n"
        "       01 RIGHT-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 CAT-STATUS PIC 9(9) VALUE 000000000.\n"
        "       01 RESULT-LENGTH PIC 9(9) VALUE 000000000.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-STRCAT' USING BY REFERENCE DEST-BUFFER\n"
        "               BY VALUE DEST-LENGTH BY REFERENCE LEFT-BUFFER\n"
        "               BY VALUE LEFT-LENGTH BY REFERENCE RIGHT-BUFFER\n"
        "               BY VALUE RIGHT-LENGTH BY REFERENCE CAT-STATUS\n"
        "               BY REFERENCE RESULT-LENGTH.\n"
        "           DISPLAY \">\" DEST-BUFFER \"<\".\n"
        "           DISPLAY CAT-STATUS.\n"
        "           DISPLAY RESULT-LENGTH.\n"
        "           STOP RUN.\n"
        "       END PROGRAM STRCAT-TRUNC-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_strcat(&library_text),
            "strcat generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">HELLO WO<\n000000001\n000000008\n",
            "strcat helper should truncate when destination is too small") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_sin_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-SIN.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION SIN(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-SIN.\n";
    if (test_expect_success(transpiler_standard_library_generate_sin(&program_text),
            "sin generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "sin generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_cos_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-COS.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION COS(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-COS.\n";
    if (test_expect_success(transpiler_standard_library_generate_cos(&program_text),
            "cos generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "cos generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tan_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TAN.\n"
        "       DATA DIVISION.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-OPERAND USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND\n"
        "           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           COMPUTE LNK-RESULT = FUNCTION TAN(LNK-OPERAND)\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TAN.\n";
    if (test_expect_success(transpiler_standard_library_generate_tan(&program_text),
            "tan generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "tan generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_powerof_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-POWEROF.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 EXP-INTEGER USAGE COMP-2 VALUE 0.\n"
        "       01 EXP-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 FRACTION-TOLERANCE USAGE COMP-2 VALUE 0.0000000000001.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-BASE USAGE COMP-2.\n"
        "       01 LNK-EXPONENT USAGE COMP-2.\n"
        "       01 LNK-RESULT USAGE COMP-2.\n"
        "       01 LNK-STATUS PIC 9.\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-BASE\n"
        "           BY REFERENCE LNK-EXPONENT BY REFERENCE LNK-RESULT\n"
        "           BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           IF LNK-BASE = 0 AND LNK-EXPONENT <= 0\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           MOVE FUNCTION INTEGER(LNK-EXPONENT) TO EXP-INTEGER.\n"
        "           COMPUTE EXP-FRACTION = FUNCTION ABS(LNK-EXPONENT - EXP-INTEGER).\n"
        "           IF LNK-BASE < 0 AND EXP-FRACTION > FRACTION-TOLERANCE\n"
        "               MOVE 1 TO LNK-STATUS\n"
        "               MOVE 0 TO LNK-RESULT\n"
        "               GOBACK\n"
        "           END-IF.\n"
        "           COMPUTE LNK-RESULT = LNK-BASE ** LNK-EXPONENT\n"
        "               ON SIZE ERROR\n"
        "                   MOVE 1 TO LNK-STATUS\n"
        "                   MOVE 0 TO LNK-RESULT\n"
        "           END-COMPUTE.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-POWEROF.\n";
    if (test_expect_success(transpiler_standard_library_generate_powerof(&program_text),
            "powerof generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "powerof generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_powerof_handles_fractional_exponent)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_powerof_frac_lib.cob";
    driver_path = "stdlib_powerof_frac_drv.cob";
    binary_path = "stdlib_powerof_frac.bin";
    output_path = "stdlib_powerof_frac.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. POWEROF-FRACTIONAL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 BASE-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 EXPONENT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 9 TO BASE-VALUE.\n"
        "           MOVE 0.5 TO EXPONENT-VALUE.\n"
        "           CALL 'CBLC-POWEROF' USING BY REFERENCE BASE-VALUE\n"
        "               BY REFERENCE EXPONENT-VALUE BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM POWEROF-FRACTIONAL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_powerof(&library_text),
            "powerof generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 3.0, g_default_float_tolerance,
            "powerof helper should compute square root of positive base") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "powerof helper should report success for fractional exponent with positive base") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "powerof helper should only emit expected transcript lines for valid fractional exponent") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_powerof_rejects_invalid_domain)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_powerof_invalid_lib.cob";
    driver_path = "stdlib_powerof_invalid_drv.cob";
    binary_path = "stdlib_powerof_invalid.bin";
    output_path = "stdlib_powerof_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. POWEROF-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 BASE-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 EXPONENT-VALUE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-VALUE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -2 TO BASE-VALUE.\n"
        "           MOVE 0.5 TO EXPONENT-VALUE.\n"
        "           CALL 'CBLC-POWEROF' USING BY REFERENCE BASE-VALUE\n"
        "               BY REFERENCE EXPONENT-VALUE BY REFERENCE RESULT-VALUE\n"
        "               BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-VALUE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM POWEROF-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_powerof(&library_text),
            "powerof generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "0000.0000\n1\n",
            "powerof helper should reject negative bases with fractional exponents") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_toupper_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TOUPPER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 TARGET-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-TARGET PIC X(255).\n"
        "       01 LNK-TARGET-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-TARGET\n"
        "           BY VALUE LNK-TARGET-LENGTH BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-TARGET-LENGTH TO TARGET-LIMIT.\n"
        "           IF TARGET-LIMIT > 255\n"
        "               MOVE 255 TO TARGET-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TARGET-LIMIT\n"
        "               IF LNK-TARGET(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-TARGET(IDX:1) TO CURRENT-CHAR\n"
        "               INSPECT CURRENT-CHAR CONVERTING \"abcdefghijklmnopqrstuvwxyz\"\n"
        "                   TO \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\n"
        "               MOVE CURRENT-CHAR TO LNK-TARGET(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TOUPPER.\n";
    if (test_expect_success(transpiler_standard_library_generate_toupper(&program_text),
            "toupper generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "toupper generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_toupper_executes_for_mixed_case_buffer)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_toupper_lib.cob";
    driver_path = "stdlib_toupper_drv.cob";
    binary_path = "stdlib_toupper.bin";
    output_path = "stdlib_toupper.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. TOUPPER-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 TARGET-BUFFER PIC X(12) VALUE \"Cobol Mixed\".\n"
        "       01 TARGET-LENGTH PIC S9(9) COMP-5 VALUE +12.\n"
        "       01 STATUS-FLAG PIC 9(9) VALUE 000000007.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-TOUPPER' USING BY REFERENCE TARGET-BUFFER\n"
        "               BY VALUE TARGET-LENGTH BY REFERENCE STATUS-FLAG.\n"
        "           DISPLAY \">\" TARGET-BUFFER \"<\".\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM TOUPPER-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_toupper(&library_text),
            "toupper generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_equal(output_buffer, ">COBOL MIXED <\n000000000\n") != FT_SUCCESS)
    {
        pf_printf("Assertion failed: toupper helper should uppercase entire buffer and report success\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tolower_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-TOLOWER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 IDX PIC 9(9) VALUE 000000000.\n"
        "       01 TARGET-LIMIT PIC 9(9) VALUE 000000000.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-TARGET PIC X(255).\n"
        "       01 LNK-TARGET-LENGTH PIC S9(9) COMP-5.\n"
        "       01 LNK-STATUS PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-TARGET\n"
        "           BY VALUE LNK-TARGET-LENGTH BY REFERENCE LNK-STATUS.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-STATUS.\n"
        "           MOVE LNK-TARGET-LENGTH TO TARGET-LIMIT.\n"
        "           IF TARGET-LIMIT > 255\n"
        "               MOVE 255 TO TARGET-LIMIT\n"
        "           END-IF.\n"
        "           MOVE 0 TO IDX.\n"
        "           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TARGET-LIMIT\n"
        "               IF LNK-TARGET(IDX:1) = LOW-VALUE\n"
        "                   EXIT PERFORM\n"
        "               END-IF\n"
        "               MOVE LNK-TARGET(IDX:1) TO CURRENT-CHAR\n"
        "               INSPECT CURRENT-CHAR CONVERTING \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\n"
        "                   TO \"abcdefghijklmnopqrstuvwxyz\"\n"
        "               MOVE CURRENT-CHAR TO LNK-TARGET(IDX:1)\n"
        "           END-PERFORM.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-TOLOWER.\n";
    if (test_expect_success(transpiler_standard_library_generate_tolower(&program_text),
            "tolower generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "tolower generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tolower_respects_declared_length)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_tolower_lib.cob";
    driver_path = "stdlib_tolower_drv.cob";
    binary_path = "stdlib_tolower.bin";
    output_path = "stdlib_tolower.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. TOLOWER-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 TARGET-BUFFER PIC X(9) VALUE \"UPPER XYZ\".\n"
        "       01 TARGET-LENGTH PIC S9(9) COMP-5 VALUE +5.\n"
        "       01 STATUS-FLAG PIC 9(9) VALUE 000000005.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-TOLOWER' USING BY REFERENCE TARGET-BUFFER\n"
        "               BY VALUE TARGET-LENGTH BY REFERENCE STATUS-FLAG.\n"
        "           DISPLAY \">\" TARGET-BUFFER \"<\".\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM TOLOWER-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_tolower(&library_text),
            "tolower generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, ">upper XYZ<\n000000000\n",
            "tolower helper should honor declared length and leave excess unchanged") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_isdigit_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ISDIGIT.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-CHAR PIC X.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-CHAR\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-CHAR TO CURRENT-CHAR.\n"
        "           IF CURRENT-CHAR >= \"0\" AND CURRENT-CHAR <= \"9\"\n"
        "               MOVE 1 TO LNK-RESULT\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ISDIGIT.\n";
    if (test_expect_success(transpiler_standard_library_generate_isdigit(&program_text),
            "isdigit generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "isdigit generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_isdigit_classifies_digits_and_non_digits)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_isdigit_lib.cob";
    driver_path = "stdlib_isdigit_drv.cob";
    binary_path = "stdlib_isdigit.bin";
    output_path = "stdlib_isdigit.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ISDIGIT-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DIGIT-CHAR PIC X VALUE \"7\".\n"
        "       01 DIGIT-RESULT PIC 9(9) VALUE 000000007.\n"
        "       01 OTHER-CHAR PIC X VALUE \"Q\".\n"
        "       01 OTHER-RESULT PIC 9(9) VALUE 000000003.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ISDIGIT' USING BY REFERENCE DIGIT-CHAR\n"
        "               BY REFERENCE DIGIT-RESULT.\n"
        "           CALL 'CBLC-ISDIGIT' USING BY REFERENCE OTHER-CHAR\n"
        "               BY REFERENCE OTHER-RESULT.\n"
        "           DISPLAY DIGIT-RESULT.\n"
        "           DISPLAY OTHER-RESULT.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ISDIGIT-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_isdigit(&library_text),
            "isdigit generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000001\n000000000\n",
            "isdigit helper should accept digits and reject other characters") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_isalpha_generates_expected_text)
{
    char *program_text;
    const char *expected_text;
    int status;

    program_text = NULL;
    expected_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CBLC-ISALPHA.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 CURRENT-CHAR PIC X VALUE SPACE.\n"
        "       LINKAGE SECTION.\n"
        "       01 LNK-CHAR PIC X.\n"
        "       01 LNK-RESULT PIC 9(9).\n"
        "       PROCEDURE DIVISION USING BY REFERENCE LNK-CHAR\n"
        "           BY REFERENCE LNK-RESULT.\n"
        "       MAIN.\n"
        "           MOVE 0 TO LNK-RESULT.\n"
        "           MOVE LNK-CHAR TO CURRENT-CHAR.\n"
        "           IF CURRENT-CHAR >= \"A\" AND CURRENT-CHAR <= \"Z\"\n"
        "               MOVE 1 TO LNK-RESULT\n"
        "           ELSE\n"
        "               IF CURRENT-CHAR >= \"a\" AND CURRENT-CHAR <= \"z\"\n"
        "                   MOVE 1 TO LNK-RESULT\n"
        "               END-IF\n"
        "           END-IF.\n"
        "           GOBACK.\n"
        "       END PROGRAM CBLC-ISALPHA.\n";
    if (test_expect_success(transpiler_standard_library_generate_isalpha(&program_text),
            "isalpha generator should succeed") != FT_SUCCESS)
    {
        if (program_text)
            cma_free(program_text);
        return (FT_FAILURE);
    }
    status = test_expect_cstring_equal(program_text, expected_text,
        "isalpha generator should emit expected COBOL subprogram");
    if (program_text)
        cma_free(program_text);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_isalpha_identifies_letter_cases)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_isalpha_lib.cob";
    driver_path = "stdlib_isalpha_drv.cob";
    binary_path = "stdlib_isalpha.bin";
    output_path = "stdlib_isalpha.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ISALPHA-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 LOWER-CHAR PIC X VALUE \"g\".\n"
        "       01 LOWER-RESULT PIC 9(9) VALUE 000000004.\n"
        "       01 UPPER-CHAR PIC X VALUE \"Z\".\n"
        "       01 UPPER-RESULT PIC 9(9) VALUE 000000004.\n"
        "       01 OTHER-CHAR PIC X VALUE \"5\".\n"
        "       01 OTHER-RESULT PIC 9(9) VALUE 000000004.\n"
        "       PROCEDURE DIVISION.\n"
        "           CALL 'CBLC-ISALPHA' USING BY REFERENCE LOWER-CHAR\n"
        "               BY REFERENCE LOWER-RESULT.\n"
        "           CALL 'CBLC-ISALPHA' USING BY REFERENCE UPPER-CHAR\n"
        "               BY REFERENCE UPPER-RESULT.\n"
        "           CALL 'CBLC-ISALPHA' USING BY REFERENCE OTHER-CHAR\n"
        "               BY REFERENCE OTHER-RESULT.\n"
        "           DISPLAY LOWER-RESULT.\n"
        "           DISPLAY UPPER-RESULT.\n"
        "           DISPLAY OTHER-RESULT.\n"
        "           STOP RUN.\n"
        "       END PROGRAM ISALPHA-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_isalpha(&library_text),
            "isalpha generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "000000001\n000000001\n000000000\n",
            "isalpha helper should accept uppercase and lowercase letters only") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_floor_executes_for_diverse_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_floor_lib.cob";
    driver_path = "stdlib_floor_drv.cob";
    binary_path = "stdlib_floor.bin";
    output_path = "stdlib_floor.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. FLOOR-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-INT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-INT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 STATUS-INT PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 5.75 TO OPERAND-POS.\n"
        "           CALL 'CBLC-FLOOR' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-POS.\n"
        "           MOVE -2.25 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-FLOOR' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           MOVE 4 TO OPERAND-INT.\n"
        "           CALL 'CBLC-FLOOR' USING BY REFERENCE OPERAND-INT\n"
        "               BY REFERENCE RESULT-INT BY REFERENCE STATUS-INT.\n"
        "           MOVE RESULT-INT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-INT.\n"
        "           STOP RUN.\n"
        "       END PROGRAM FLOOR-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_floor(&library_text),
            "floor generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 5.0, g_default_float_tolerance,
            "floor helper should round positive fractional operands toward negative infinity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "floor helper should report fractional positive operand as truncated") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -3.0, g_default_float_tolerance,
            "floor helper should round negative fractional operands toward negative infinity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "floor helper should report fractional negative operand as truncated") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 4.0, g_default_float_tolerance,
            "floor helper should preserve integral operands") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "floor helper should report success for integral operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "floor helper should only emit expected transcript lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_ceil_executes_for_diverse_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_ceil_lib.cob";
    driver_path = "stdlib_ceil_drv.cob";
    binary_path = "stdlib_ceil.bin";
    output_path = "stdlib_ceil.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CEIL-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-INT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-INT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 STATUS-INT PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 5.25 TO OPERAND-POS.\n"
        "           CALL 'CBLC-CEIL' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-POS.\n"
        "           MOVE -2.75 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-CEIL' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           MOVE 4 TO OPERAND-INT.\n"
        "           CALL 'CBLC-CEIL' USING BY REFERENCE OPERAND-INT\n"
        "               BY REFERENCE RESULT-INT BY REFERENCE STATUS-INT.\n"
        "           MOVE RESULT-INT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-INT.\n"
        "           STOP RUN.\n"
        "       END PROGRAM CEIL-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_ceil(&library_text),
            "ceil generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 6.0, g_default_float_tolerance,
            "ceil helper should round positive fractional operands toward positive infinity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "ceil helper should report fractional positive operand as truncated") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -2.0, g_default_float_tolerance,
            "ceil helper should round negative fractional operands toward positive infinity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "1",
            "ceil helper should report fractional negative operand as truncated") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 4.0, g_default_float_tolerance,
            "ceil helper should preserve integral operands") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "ceil helper should report success for integral operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "ceil helper should only emit expected transcript lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_exp_executes_for_positive_and_negative_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_exp_lib.cob";
    driver_path = "stdlib_exp_drv.cob";
    binary_path = "stdlib_exp.bin";
    output_path = "stdlib_exp.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. EXP-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-POS USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-POS USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-POS PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 1 TO OPERAND-POS.\n"
        "           CALL 'CBLC-EXP' USING BY REFERENCE OPERAND-POS\n"
        "               BY REFERENCE RESULT-POS BY REFERENCE STATUS-POS.\n"
        "           MOVE RESULT-POS TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-POS.\n"
        "           MOVE -1 TO OPERAND-NEG.\n"
        "           CALL 'CBLC-EXP' USING BY REFERENCE OPERAND-NEG\n"
        "               BY REFERENCE RESULT-NEG BY REFERENCE STATUS-NEG.\n"
        "           MOVE RESULT-NEG TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM EXP-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_exp(&library_text),
            "exp generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 2.718281828, g_default_float_tolerance,
            "exp helper should compute e raised to positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "exp helper should report success for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.367879441, g_default_float_tolerance,
            "exp helper should compute e raised to negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "exp helper should report success for negative operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "exp helper should only emit expected transcript lines for valid operands") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_exp_reports_overflow_for_large_operand)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_exp_overflow_lib.cob";
    driver_path = "stdlib_exp_overflow_drv.cob";
    binary_path = "stdlib_exp_overflow.bin";
    output_path = "stdlib_exp_overflow.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. EXP-OVERFLOW-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-LARGE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-LARGE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-LARGE PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 1000 TO OPERAND-LARGE.\n"
        "           CALL 'CBLC-EXP' USING BY REFERENCE OPERAND-LARGE\n"
        "               BY REFERENCE RESULT-LARGE BY REFERENCE STATUS-LARGE.\n"
        "           MOVE RESULT-LARGE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-LARGE.\n"
        "           STOP RUN.\n"
        "       END PROGRAM EXP-OVERFLOW-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_exp(&library_text),
            "exp generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "0000.0000\n1\n",
            "exp helper should detect overflow and report failure") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_log_executes_for_positive_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_log_lib.cob";
    driver_path = "stdlib_log_drv.cob";
    binary_path = "stdlib_log.bin";
    output_path = "stdlib_log.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. LOG-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-E USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-E USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FRACTION USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-E PIC 9 VALUE 9.\n"
        "       01 STATUS-FRACTION PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 2.718281828 TO OPERAND-E.\n"
        "           CALL 'CBLC-LOG' USING BY REFERENCE OPERAND-E\n"
        "               BY REFERENCE RESULT-E BY REFERENCE STATUS-E.\n"
        "           MOVE RESULT-E TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-E.\n"
        "           MOVE 0.5 TO OPERAND-FRACTION.\n"
        "           CALL 'CBLC-LOG' USING BY REFERENCE OPERAND-FRACTION\n"
        "               BY REFERENCE RESULT-FRACTION BY REFERENCE STATUS-FRACTION.\n"
        "           MOVE RESULT-FRACTION TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FRACTION.\n"
        "           STOP RUN.\n"
        "       END PROGRAM LOG-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_log(&library_text),
            "log generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0, g_default_float_tolerance,
            "log helper should evaluate ln(e)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "log helper should report success for e operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -0.6931471805599453, g_default_float_tolerance,
            "log helper should evaluate ln(0.5)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "log helper should report success for fractional operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "log helper should only emit expected transcript lines for valid operands") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_log_rejects_non_positive_operand)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_log_invalid_lib.cob";
    driver_path = "stdlib_log_invalid_drv.cob";
    binary_path = "stdlib_log_invalid.bin";
    output_path = "stdlib_log_invalid.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. LOG-INVALID-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-NONPOSITIVE USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NONPOSITIVE USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 0 TO OPERAND-NONPOSITIVE.\n"
        "           CALL 'CBLC-LOG' USING BY REFERENCE OPERAND-NONPOSITIVE\n"
        "               BY REFERENCE RESULT-NONPOSITIVE BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-NONPOSITIVE TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM LOG-INVALID-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_log(&library_text),
            "log generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, " 0000.0000\n1\n",
            "log helper should reject non-positive operands and zero the result") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_sin_executes_for_representative_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_sin_lib.cob";
    driver_path = "stdlib_sin_drv.cob";
    binary_path = "stdlib_sin.bin";
    output_path = "stdlib_sin.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SIN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-PI-HALF USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG-PI-HALF USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-PI-HALF USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG-PI-HALF USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-ZERO PIC 9 VALUE 9.\n"
        "       01 STATUS-PI-HALF PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG-PI-HALF PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 0 TO OPERAND-ZERO.\n"
        "           CALL 'CBLC-SIN' USING BY REFERENCE OPERAND-ZERO\n"
        "               BY REFERENCE RESULT-ZERO BY REFERENCE STATUS-ZERO.\n"
        "           MOVE RESULT-ZERO TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-ZERO.\n"
        "           COMPUTE OPERAND-PI-HALF = FUNCTION PI / 2.\n"
        "           CALL 'CBLC-SIN' USING BY REFERENCE OPERAND-PI-HALF\n"
        "               BY REFERENCE RESULT-PI-HALF BY REFERENCE STATUS-PI-HALF.\n"
        "           MOVE RESULT-PI-HALF TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-PI-HALF.\n"
        "           COMPUTE OPERAND-NEG-PI-HALF = 0 - OPERAND-PI-HALF.\n"
        "           CALL 'CBLC-SIN' USING BY REFERENCE OPERAND-NEG-PI-HALF\n"
        "               BY REFERENCE RESULT-NEG-PI-HALF BY REFERENCE STATUS-NEG-PI-HALF.\n"
        "           MOVE RESULT-NEG-PI-HALF TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG-PI-HALF.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SIN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sin(&library_text),
            "sin generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.0, g_default_float_tolerance,
            "sin helper should evaluate sin(0)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "sin helper should report success for zero operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0, g_default_float_tolerance,
            "sin helper should evaluate sin(pi/2)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "sin helper should report success for pi/2 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -1.0, g_default_float_tolerance,
            "sin helper should evaluate sin(-pi/2)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "sin helper should report success for -pi/2 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "sin helper should only emit expected transcript lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_cos_executes_for_representative_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_cos_lib.cob";
    driver_path = "stdlib_cos_drv.cob";
    binary_path = "stdlib_cos.bin";
    output_path = "stdlib_cos.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. COS-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-PI USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-PI-HALF USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-PI USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-PI-HALF USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-ZERO PIC 9 VALUE 9.\n"
        "       01 STATUS-PI PIC 9 VALUE 9.\n"
        "       01 STATUS-PI-HALF PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 0 TO OPERAND-ZERO.\n"
        "           CALL 'CBLC-COS' USING BY REFERENCE OPERAND-ZERO\n"
        "               BY REFERENCE RESULT-ZERO BY REFERENCE STATUS-ZERO.\n"
        "           MOVE RESULT-ZERO TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-ZERO.\n"
        "           COMPUTE OPERAND-PI = FUNCTION PI.\n"
        "           CALL 'CBLC-COS' USING BY REFERENCE OPERAND-PI\n"
        "               BY REFERENCE RESULT-PI BY REFERENCE STATUS-PI.\n"
        "           MOVE RESULT-PI TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-PI.\n"
        "           COMPUTE OPERAND-PI-HALF = FUNCTION PI / 2.\n"
        "           CALL 'CBLC-COS' USING BY REFERENCE OPERAND-PI-HALF\n"
        "               BY REFERENCE RESULT-PI-HALF BY REFERENCE STATUS-PI-HALF.\n"
        "           MOVE RESULT-PI-HALF TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-PI-HALF.\n"
        "           STOP RUN.\n"
        "       END PROGRAM COS-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_cos(&library_text),
            "cos generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0, g_default_float_tolerance,
            "cos helper should evaluate cos(0)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "cos helper should report success for zero operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -1.0, g_default_float_tolerance,
            "cos helper should evaluate cos(pi)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "cos helper should report success for pi operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.0, g_default_float_tolerance,
            "cos helper should evaluate cos(pi/2)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "cos helper should report success for pi/2 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "cos helper should only emit expected transcript lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_tan_executes_for_representative_operands)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_tan_lib.cob";
    driver_path = "stdlib_tan_drv.cob";
    binary_path = "stdlib_tan.bin";
    output_path = "stdlib_tan.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. TAN-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 OPERAND-NEG-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-ZERO USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-NEG-PI-QUARTER USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-ZERO PIC 9 VALUE 9.\n"
        "       01 STATUS-PI-QUARTER PIC 9 VALUE 9.\n"
        "       01 STATUS-NEG-PI-QUARTER PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC -9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 0 TO OPERAND-ZERO.\n"
        "           CALL 'CBLC-TAN' USING BY REFERENCE OPERAND-ZERO\n"
        "               BY REFERENCE RESULT-ZERO BY REFERENCE STATUS-ZERO.\n"
        "           MOVE RESULT-ZERO TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-ZERO.\n"
        "           COMPUTE OPERAND-PI-QUARTER = FUNCTION PI / 4.\n"
        "           CALL 'CBLC-TAN' USING BY REFERENCE OPERAND-PI-QUARTER\n"
        "               BY REFERENCE RESULT-PI-QUARTER BY REFERENCE STATUS-PI-QUARTER.\n"
        "           MOVE RESULT-PI-QUARTER TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-PI-QUARTER.\n"
        "           COMPUTE OPERAND-NEG-PI-QUARTER = 0 - OPERAND-PI-QUARTER.\n"
        "           CALL 'CBLC-TAN' USING BY REFERENCE OPERAND-NEG-PI-QUARTER\n"
        "               BY REFERENCE RESULT-NEG-PI-QUARTER BY REFERENCE STATUS-NEG-PI-QUARTER.\n"
        "           MOVE RESULT-NEG-PI-QUARTER TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-NEG-PI-QUARTER.\n"
        "           STOP RUN.\n"
        "       END PROGRAM TAN-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_tan(&library_text),
            "tan generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 0.0, g_default_float_tolerance,
            "tan helper should evaluate tan(0)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "tan helper should report success for zero operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 1.0, g_default_float_tolerance,
            "tan helper should evaluate tan(pi/4)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "tan helper should report success for pi/4 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, -1.0, g_default_float_tolerance,
            "tan helper should evaluate tan(-pi/4)") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "tan helper should report success for -pi/4 operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "tan helper should only emit expected transcript lines") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_sqrt_executes_for_positive_operand)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_sqrt_pos_lib.cob";
    driver_path = "stdlib_sqrt_pos_drv.cob";
    binary_path = "stdlib_sqrt_pos.bin";
    output_path = "stdlib_sqrt_pos.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SQRT-POSITIVE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 9.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE 49 TO OPERAND-FLOAT.\n"
        "           CALL 'CBLC-SQRT' USING BY REFERENCE OPERAND-FLOAT\n"
        "               BY REFERENCE RESULT-FLOAT BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SQRT-POSITIVE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&library_text),
            "sqrt generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    size_t output_offset;

    output_offset = 0;
    if (test_expect_transcript_double_line(output_buffer, &output_offset, 7.0, g_default_float_tolerance,
            "sqrt helper should compute square root of positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_status_line(output_buffer, &output_offset, "0",
            "sqrt helper should report success for positive operand") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_complete(output_buffer, output_offset,
            "sqrt helper should only emit expected transcript lines for positive operand") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_sqrt_rejects_negative_operand)
{
    const char *library_path;
    const char *driver_path;
    const char *binary_path;
    const char *output_path;
    const char *driver_text;
    char *library_text;
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    library_path = "stdlib_sqrt_neg_lib.cob";
    driver_path = "stdlib_sqrt_neg_drv.cob";
    binary_path = "stdlib_sqrt_neg.bin";
    output_path = "stdlib_sqrt_neg.txt";
    driver_text =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SQRT-NEGATIVE-DRIVER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 OPERAND-FLOAT USAGE COMP-2 VALUE 0.\n"
        "       01 RESULT-FLOAT USAGE COMP-2 VALUE 123.\n"
        "       01 STATUS-FLAG PIC 9 VALUE 0.\n"
        "       01 RESULT-DISPLAY PIC 9(4).9(4).\n"
        "       PROCEDURE DIVISION.\n"
        "           MOVE -4 TO OPERAND-FLOAT.\n"
        "           CALL 'CBLC-SQRT' USING BY REFERENCE OPERAND-FLOAT\n"
        "               BY REFERENCE RESULT-FLOAT BY REFERENCE STATUS-FLAG.\n"
        "           MOVE RESULT-FLOAT TO RESULT-DISPLAY.\n"
        "           DISPLAY RESULT-DISPLAY.\n"
        "           DISPLAY STATUS-FLAG.\n"
        "           STOP RUN.\n"
        "       END PROGRAM SQRT-NEGATIVE-DRIVER.\n";
    library_text = NULL;
    status = FT_FAILURE;
    if (test_expect_success(transpiler_standard_library_generate_sqrt(&library_text),
            "sqrt generator should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(library_path, library_text) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(driver_path, driver_text) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s", binary_path, driver_path, library_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(output_buffer, "0000.0000\n1\n",
            "sqrt helper should reject negative operands and zero the result") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (library_text)
        cma_free(library_text);
    test_cleanup_generated_artifacts(binary_path, output_path);
    test_remove_file(library_path);
    test_remove_file(driver_path);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_standard_library_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"standard_library_lookup_enforces_std_prefix", test_standard_library_lookup_enforces_std_prefix},
        {"standard_library_lookup_is_case_sensitive", test_standard_library_lookup_is_case_sensitive},
        {"standard_library_catalog_lists_all_entries", test_standard_library_catalog_lists_all_entries},
        {"standard_library_generators_validate_out_parameter", test_standard_library_generators_validate_out_parameter},
        {"standard_library_strlen_generates_expected_text", test_standard_library_strlen_generates_expected_text},
        {"standard_library_strlen_string_generates_expected_text", test_standard_library_strlen_string_generates_expected_text},
        {"standard_library_strlen_executes", test_standard_library_strlen_executes},
        {"standard_library_strlen_handles_all_spaces", test_standard_library_strlen_handles_all_spaces},
        {"standard_library_strnlen_generates_expected_text", test_standard_library_strnlen_generates_expected_text},
        {"standard_library_strnlen_respects_request_limit", test_standard_library_strnlen_respects_request_limit},
        {"standard_library_strnlen_honors_declared_length", test_standard_library_strnlen_honors_declared_length},
        {"standard_library_strnlen_trims_trailing_spaces", test_standard_library_strnlen_trims_trailing_spaces},
        {"standard_library_strnlen_stops_at_low_value", test_standard_library_strnlen_stops_at_low_value},
        {"standard_library_strcmp_generates_expected_text", test_standard_library_strcmp_generates_expected_text},
        {"standard_library_strcmp_executes", test_standard_library_strcmp_executes},
        {"standard_library_strcpy_generates_expected_text", test_standard_library_strcpy_generates_expected_text},
        {"standard_library_strcpy_executes_without_truncation", test_standard_library_strcpy_executes_without_truncation},
        {"standard_library_strcpy_blanks_destination", test_standard_library_strcpy_blanks_destination},
        {"standard_library_strcpy_reports_truncation", test_standard_library_strcpy_reports_truncation},
        {"standard_library_strncpy_generates_expected_text", test_standard_library_strncpy_generates_expected_text},
        {"standard_library_strncpy_executes_without_truncation", test_standard_library_strncpy_executes_without_truncation},
        {"standard_library_strncpy_reports_truncation", test_standard_library_strncpy_reports_truncation},
        {"standard_library_strncpy_reports_short_source", test_standard_library_strncpy_reports_short_source},
        {"standard_library_strncpy_honors_zero_request", test_standard_library_strncpy_honors_zero_request},
        {"standard_library_strcat_generates_expected_text", test_standard_library_strcat_generates_expected_text},
        {"standard_library_strcat_executes_without_truncation", test_standard_library_strcat_executes_without_truncation},
        {"standard_library_strcat_reports_truncation", test_standard_library_strcat_reports_truncation},
        {"standard_library_strtod_generates_expected_text", test_standard_library_strtod_generates_expected_text},
        {"standard_library_strtod_parses_scientific_notation", test_standard_library_strtod_parses_scientific_notation},
        {"standard_library_strtod_rejects_invalid_input", test_standard_library_strtod_rejects_invalid_input},
        {"standard_library_abs_generates_expected_text", test_standard_library_abs_generates_expected_text},
        {"standard_library_abs_executes_for_common_cases", test_standard_library_abs_executes_for_common_cases},
        {"standard_library_abs_reports_overflow_for_min_value", test_standard_library_abs_reports_overflow_for_min_value},
        {"standard_library_fabs_generates_expected_text", test_standard_library_fabs_generates_expected_text},
        {"standard_library_fabs_executes_for_negative_and_positive_operands", test_standard_library_fabs_executes_for_negative_and_positive_operands},
        {"standard_library_floor_generates_expected_text", test_standard_library_floor_generates_expected_text},
        {"standard_library_floor_executes_for_diverse_operands", test_standard_library_floor_executes_for_diverse_operands},
        {"standard_library_ceil_generates_expected_text", test_standard_library_ceil_generates_expected_text},
        {"standard_library_ceil_executes_for_diverse_operands", test_standard_library_ceil_executes_for_diverse_operands},
        {"standard_library_exp_generates_expected_text", test_standard_library_exp_generates_expected_text},
        {"standard_library_exp_executes_for_positive_and_negative_operands", test_standard_library_exp_executes_for_positive_and_negative_operands},
        {"standard_library_exp_reports_overflow_for_large_operand", test_standard_library_exp_reports_overflow_for_large_operand},
        {"standard_library_log_generates_expected_text", test_standard_library_log_generates_expected_text},
        {"standard_library_log_executes_for_positive_operands", test_standard_library_log_executes_for_positive_operands},
        {"standard_library_log_rejects_non_positive_operand", test_standard_library_log_rejects_non_positive_operand},
        {"standard_library_sin_generates_expected_text", test_standard_library_sin_generates_expected_text},
        {"standard_library_cos_generates_expected_text", test_standard_library_cos_generates_expected_text},
        {"standard_library_tan_generates_expected_text", test_standard_library_tan_generates_expected_text},
        {"standard_library_sin_executes_for_representative_operands", test_standard_library_sin_executes_for_representative_operands},
        {"standard_library_cos_executes_for_representative_operands", test_standard_library_cos_executes_for_representative_operands},
        {"standard_library_tan_executes_for_representative_operands", test_standard_library_tan_executes_for_representative_operands},
        {"standard_library_powerof_generates_expected_text", test_standard_library_powerof_generates_expected_text},
        {"standard_library_powerof_handles_fractional_exponent", test_standard_library_powerof_handles_fractional_exponent},
        {"standard_library_powerof_rejects_invalid_domain", test_standard_library_powerof_rejects_invalid_domain},
        {"standard_library_toupper_generates_expected_text", test_standard_library_toupper_generates_expected_text},
        {"standard_library_toupper_executes_for_mixed_case_buffer", test_standard_library_toupper_executes_for_mixed_case_buffer},
        {"standard_library_tolower_generates_expected_text", test_standard_library_tolower_generates_expected_text},
        {"standard_library_tolower_respects_declared_length", test_standard_library_tolower_respects_declared_length},
        {"standard_library_isdigit_generates_expected_text", test_standard_library_isdigit_generates_expected_text},
        {"standard_library_isdigit_classifies_digits_and_non_digits", test_standard_library_isdigit_classifies_digits_and_non_digits},
        {"standard_library_isalpha_generates_expected_text", test_standard_library_isalpha_generates_expected_text},
        {"standard_library_isalpha_identifies_letter_cases", test_standard_library_isalpha_identifies_letter_cases},
        {"standard_library_sqrt_generates_expected_text", test_standard_library_sqrt_generates_expected_text},
        {"standard_library_sqrt_executes_for_positive_operand", test_standard_library_sqrt_executes_for_positive_operand},
        {"standard_library_sqrt_rejects_negative_operand", test_standard_library_sqrt_rejects_negative_operand}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
