#ifndef TEST_SUPPORT_HPP
#define TEST_SUPPORT_HPP

#include <cstddef>
#include <sys/types.h>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "lexer_token.hpp"

typedef struct s_test_case
{
    const char *name;
    int (*execute)(void);
}   t_test_case;

typedef struct s_test_output_capture
{
    int saved_fd;
    int pipe_read_fd;
    int target_fd;
}   t_test_output_capture;

#define FT_TEST(name) int name(void); int name(void)

int test_assert_failure(const char *expression, const char *file, int line);

#define FT_ASSERT(condition) \
    if (!(condition)) \
    { \
        return (test_assert_failure(#condition, __FILE__, __LINE__)); \
    }

#define FT_ASSERT_SUCCESS(status, message) \
    if (test_expect_success((status), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

#define FT_ASSERT_INT_EQUAL(actual, expected, message) \
    if (test_expect_int_equal((actual), (expected), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

#define FT_ASSERT_SIZE_T_EQUAL(actual, expected, message) \
    if (test_expect_size_t_equal((actual), (expected), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

#define FT_ASSERT_CHAR_EQUAL(actual, expected, message) \
    if (test_expect_char_equal((actual), (expected), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

#define FT_ASSERT_CSTRING_EQUAL(actual, expected, message) \
    if (test_expect_cstring_equal((actual), (expected), (message)) != FT_SUCCESS) \
        return (FT_FAILURE);

#define FT_ASSERT_TOKEN(token, kind, lexeme, line, column) \
    if (test_expect_token((token), (kind), (lexeme), (line), (column)) != FT_SUCCESS) \
        return (FT_FAILURE);

int test_expect_success(int status, const char *message);
int test_expect_int_equal(int actual, int expected, const char *message);
int test_expect_size_t_equal(size_t actual, size_t expected, const char *message);
int test_expect_char_equal(char actual, char expected, const char *message);
int test_expect_cstring_equal(const char *actual, const char *expected, const char *message);
int test_expect_token(const t_lexer_token *token, t_lexer_token_kind expected_kind,
    const char *expected_lexeme, size_t expected_line, size_t expected_column);
int test_write_text_file(const char *path, const char *contents);
int test_read_text_file(const char *path, char *buffer, size_t buffer_size);
int test_cobc_available(void);
int test_run_command(const char *command);
int test_run_command_expect_failure(const char *command);
void test_remove_file(const char *path);
int run_test_case(const t_test_case *test);
int run_test_suite(const t_test_case *tests, size_t count);
void test_report_summary(void);
int test_capture_stdout_begin(t_test_output_capture *capture);
int test_capture_stdout_end(t_test_output_capture *capture, char *buffer, size_t buffer_size,
    ssize_t *length);
int test_capture_stderr_begin(t_test_output_capture *capture);
int test_capture_stderr_end(t_test_output_capture *capture, char *buffer, size_t buffer_size,
    ssize_t *length);

#define FT_REQUIRE_COBC() \
    if (!test_cobc_available()) \
        return (FT_SUCCESS);

#endif
