#include "test_support.hpp"

#include <cstdlib>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>

int test_expect_success(int status, const char *message)
{
    if (status == FT_SUCCESS)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s\n", message);
    return (FT_FAILURE);
}

int test_expect_int_equal(int actual, int expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %d, got %d)\n", message, expected, actual);
    return (FT_FAILURE);
}

int test_expect_char_equal(char actual, char expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %c, got %c)\n", message, expected, actual);
    return (FT_FAILURE);
}

int test_expect_cstring_equal(const char *actual, const char *expected, const char *message)
{
    if (!actual && !expected)
        return (FT_SUCCESS);
    if (!actual || !expected)
    {
        if (message)
            pf_printf("Assertion failed: %s (expected %s, got %s)\n", message,
                expected ? expected : "(null)", actual ? actual : "(null)");
        return (FT_FAILURE);
    }
    if (ft_strncmp(actual, expected, ft_strlen(expected) + 1) == 0)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %s, got %s)\n", message, expected, actual);
    return (FT_FAILURE);
}

int test_expect_token(const t_lexer_token *token, t_lexer_token_kind expected_kind,
    const char *expected_lexeme, size_t expected_line, size_t expected_column)
{
    size_t index;
    size_t expected_length;

    if (!token)
        return (FT_FAILURE);
    if (token->kind != expected_kind)
    {
        pf_printf("Assertion failed: token kind mismatch (expected %d, got %d)\n", expected_kind, token->kind);
        return (FT_FAILURE);
    }
    if (token->line != expected_line)
    {
        pf_printf("Assertion failed: token line mismatch (expected %zu, got %zu)\n", expected_line, token->line);
        return (FT_FAILURE);
    }
    if (token->column != expected_column)
    {
        pf_printf("Assertion failed: token column mismatch (expected %zu, got %zu)\n", expected_column, token->column);
        return (FT_FAILURE);
    }
    if (!expected_lexeme)
    {
        if (token->length != 0)
        {
            pf_printf("Assertion failed: token length mismatch (expected 0, got %zu)\n", token->length);
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    expected_length = ft_strlen(expected_lexeme);
    if (token->length != expected_length)
    {
        pf_printf("Assertion failed: token length mismatch (expected %zu, got %zu)\n", expected_length, token->length);
        return (FT_FAILURE);
    }
    if (!token->lexeme)
    {
        pf_printf("Assertion failed: token lexeme should not be null\n");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < expected_length)
    {
        if (token->lexeme[index] != expected_lexeme[index])
        {
            pf_printf("Assertion failed: token lexeme mismatch at index %zu (expected %c, got %c)\n",
                index, expected_lexeme[index], token->lexeme[index]);
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

int test_write_text_file(const char *path, const char *contents)
{
    int fd;
    size_t length;
    size_t offset;
    ssize_t result;

    if (!path)
        return (FT_FAILURE);
    if (!contents)
        return (FT_FAILURE);
    fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0)
        return (FT_FAILURE);
    length = ft_strlen(contents);
    offset = 0;
    while (offset < length)
    {
        result = write(fd, contents + offset, length - offset);
        if (result < 0)
        {
            close(fd);
            return (FT_FAILURE);
        }
        offset += static_cast<size_t>(result);
    }
    if (close(fd) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int test_read_text_file(const char *path, char *buffer, size_t buffer_size)
{
    int fd;
    size_t offset;
    ssize_t result;

    if (!path)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    fd = open(path, O_RDONLY);
    if (fd < 0)
        return (FT_FAILURE);
    offset = 0;
    while (offset + 1 < buffer_size)
    {
        result = read(fd, buffer + offset, buffer_size - 1 - offset);
        if (result < 0)
        {
            close(fd);
            return (FT_FAILURE);
        }
        if (result == 0)
            break;
        offset += static_cast<size_t>(result);
    }
    if (offset + 1 >= buffer_size)
    {
        close(fd);
        return (FT_FAILURE);
    }
    buffer[offset] = '\0';
    if (close(fd) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int test_run_command(const char *command)
{
    int status;

    if (!command)
        return (FT_FAILURE);
    status = system(command);
    if (status == -1)
        return (FT_FAILURE);
    if (WIFEXITED(status) == 0)
        return (FT_FAILURE);
    if (WEXITSTATUS(status) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int test_run_command_expect_failure(const char *command)
{
    int status;

    if (!command)
        return (FT_FAILURE);
    status = system(command);
    if (status == -1)
        return (FT_FAILURE);
    if (WIFEXITED(status) == 0)
        return (FT_FAILURE);
    if (WEXITSTATUS(status) == 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

void test_remove_file(const char *path)
{
    if (!path)
        return ;
    unlink(path);
}

int run_test_case(const t_test_case *test)
{
    int status;

    if (!test)
        return (FT_FAILURE);
    pf_printf("Running %s...\n", test->name);
    status = test->execute();
    if (status != FT_SUCCESS)
        pf_printf("FAILED %s\n", test->name);
    else
        pf_printf("PASSED %s\n", test->name);
    return (status);
}

int run_test_suite(const t_test_case *tests, size_t count)
{
    size_t index;
    int status;

    if (!tests && count != 0)
        return (FT_FAILURE);
    index = 0;
    status = FT_SUCCESS;
    while (index < count)
    {
        if (run_test_case(&tests[index]) != FT_SUCCESS)
            status = FT_FAILURE;
        index += 1;
    }
    return (status);
}
