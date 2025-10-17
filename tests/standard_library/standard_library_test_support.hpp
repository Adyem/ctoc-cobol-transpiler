#ifndef STANDARD_LIBRARY_TEST_SUPPORT_HPP
#define STANDARD_LIBRARY_TEST_SUPPORT_HPP

#include "../compiler/compiler_test_support.hpp"
#include "../../libft/CMA/CMA.hpp"
#include "../test_suites.hpp"

#include <cerrno>
#include <cmath>
#include <cstdlib>

static const double g_default_float_tolerance = 0.0001;

#if defined(__GNUC__)
#define STANDARD_LIBRARY_TEST_UNUSED __attribute__((unused))
#else
#define STANDARD_LIBRARY_TEST_UNUSED
#endif

static int STANDARD_LIBRARY_TEST_UNUSED test_read_transcript_line(const char *buffer, size_t *offset, char *line_buffer,
    size_t buffer_size)
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

static int STANDARD_LIBRARY_TEST_UNUSED test_expect_transcript_double(const char *line, double expected, double tolerance,
    const char *message)
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

static int STANDARD_LIBRARY_TEST_UNUSED test_expect_transcript_double_line(const char *buffer, size_t *offset,
    double expected, double tolerance, const char *message)
{
    char line_buffer[64];

    if (test_read_transcript_line(buffer, offset, line_buffer, sizeof(line_buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: %s (missing floating-point transcript line)\n", message);
        return (FT_FAILURE);
    }
    return (test_expect_transcript_double(line_buffer, expected, tolerance, message));
}

static int STANDARD_LIBRARY_TEST_UNUSED test_expect_transcript_status_line(const char *buffer, size_t *offset,
    const char *expected, const char *message)
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

static int STANDARD_LIBRARY_TEST_UNUSED test_expect_transcript_complete(const char *buffer, size_t offset,
    const char *message)
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

#undef STANDARD_LIBRARY_TEST_UNUSED

#endif
