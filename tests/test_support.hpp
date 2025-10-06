#ifndef TEST_SUPPORT_HPP
#define TEST_SUPPORT_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "lexer_token.hpp"

typedef struct s_test_case
{
    const char *name;
    int (*execute)(void);
}   t_test_case;

int test_expect_success(int status, const char *message);
int test_expect_int_equal(int actual, int expected, const char *message);
int test_expect_char_equal(char actual, char expected, const char *message);
int test_expect_cstring_equal(const char *actual, const char *expected, const char *message);
int test_expect_token(const t_lexer_token *token, t_lexer_token_kind expected_kind,
    const char *expected_lexeme, size_t expected_line, size_t expected_column);
int test_write_text_file(const char *path, const char *contents);
int test_read_text_file(const char *path, char *buffer, size_t buffer_size);
int test_run_command(const char *command);
int test_run_command_expect_failure(const char *command);
void test_remove_file(const char *path);
int run_test_case(const t_test_case *test);
int run_test_suite(const t_test_case *tests, size_t count);

#endif
