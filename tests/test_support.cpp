#include "test_support.hpp"

#include <cerrno>
#include <cstdlib>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>

static size_t g_total_tests = 0;
static size_t g_failed_tests = 0;
static int g_checked_cobc = 0;
static int g_has_cobc = 0;

static int test_capture_stream_begin(t_test_output_capture *capture, int fd)
{
    int pipe_fds[2];

    if (!capture)
        return (FT_FAILURE);
    capture->saved_fd = -1;
    capture->pipe_read_fd = -1;
    capture->target_fd = -1;
    if (pipe(pipe_fds) < 0)
        return (FT_FAILURE);
    capture->saved_fd = dup(fd);
    if (capture->saved_fd < 0)
    {
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        return (FT_FAILURE);
    }
    if (dup2(pipe_fds[1], fd) < 0)
    {
        close(capture->saved_fd);
        capture->saved_fd = -1;
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        return (FT_FAILURE);
    }
    close(pipe_fds[1]);
    capture->pipe_read_fd = pipe_fds[0];
    capture->target_fd = fd;
    return (FT_SUCCESS);
}

static int test_capture_stream_end(t_test_output_capture *capture, int fd, char *buffer, size_t buffer_size,
    ssize_t *length)
{
    char discard[64];
    ssize_t total;
    ssize_t chunk;
    ssize_t remaining;

    if (!capture || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (capture->saved_fd < 0 || capture->pipe_read_fd < 0)
        return (FT_FAILURE);
    if (capture->target_fd != fd)
    {
        close(capture->saved_fd);
        capture->saved_fd = -1;
        close(capture->pipe_read_fd);
        capture->pipe_read_fd = -1;
        capture->target_fd = -1;
        return (FT_FAILURE);
    }
    if (dup2(capture->saved_fd, fd) < 0)
    {
        close(capture->saved_fd);
        capture->saved_fd = -1;
        close(capture->pipe_read_fd);
        capture->pipe_read_fd = -1;
        return (FT_FAILURE);
    }
    close(capture->saved_fd);
    capture->saved_fd = -1;
    capture->target_fd = -1;
    total = 0;
    while (total + 1 < static_cast<ssize_t>(buffer_size))
    {
        remaining = static_cast<ssize_t>(buffer_size) - 1 - total;
        chunk = read(capture->pipe_read_fd, buffer + total, static_cast<size_t>(remaining));
        if (chunk < 0)
        {
            close(capture->pipe_read_fd);
            capture->pipe_read_fd = -1;
            return (FT_FAILURE);
        }
        if (chunk == 0)
            break ;
        total += chunk;
        if (chunk < remaining)
            break ;
    }
    buffer[total] = '\0';
    if (length)
        *length = total;
    while (1)
    {
        chunk = read(capture->pipe_read_fd, discard, sizeof(discard));
        if (chunk <= 0)
            break ;
    }
    close(capture->pipe_read_fd);
    capture->pipe_read_fd = -1;
    return (FT_SUCCESS);
}

int test_capture_stdout_begin(t_test_output_capture *capture)
{
    return (test_capture_stream_begin(capture, 1));
}

int test_capture_stdout_end(t_test_output_capture *capture, char *buffer, size_t buffer_size,
    ssize_t *length)
{
    return (test_capture_stream_end(capture, 1, buffer, buffer_size, length));
}

int test_capture_stderr_begin(t_test_output_capture *capture)
{
    return (test_capture_stream_begin(capture, 2));
}

int test_capture_stderr_end(t_test_output_capture *capture, char *buffer, size_t buffer_size,
    ssize_t *length)
{
    return (test_capture_stream_end(capture, 2, buffer, buffer_size, length));
}

int test_cobc_available(void)
{
    pid_t pid;
    int status;

    if (g_checked_cobc)
        return (g_has_cobc);
    g_checked_cobc = 1;
    pid = fork();
    if (pid < 0)
        return (0);
    if (pid == 0)
    {
        execl("/bin/sh", "sh", "-c", "cobc --version >/dev/null 2>&1", (char *)NULL);
        _exit(127);
    }
    while (waitpid(pid, &status, 0) < 0)
    {
        if (errno != EINTR)
            return (0);
    }
    if (WIFEXITED(status) == 0)
        return (0);
    if (WEXITSTATUS(status) != 0)
        return (0);
    g_has_cobc = 1;
    return (g_has_cobc);
}

static void test_format_index(size_t value, char *buffer, size_t buffer_size)
{
    size_t position;
    size_t left;
    size_t right;
    char temp;

    if (!buffer || buffer_size == 0)
        return ;
    if (buffer_size == 1)
    {
        buffer[0] = '\0';
        return ;
    }
    if (value == 0)
    {
        buffer[0] = '0';
        buffer[1] = '\0';
        return ;
    }
    position = 0;
    while (value > 0 && position + 1 < buffer_size)
    {
        buffer[position] = static_cast<char>('0' + (value % 10));
        value /= 10;
        position += 1;
    }
    if (value > 0)
    {
        buffer[buffer_size - 1] = '\0';
        return ;
    }
    buffer[position] = '\0';
    if (position == 0)
        return ;
    left = 0;
    right = position - 1;
    while (left < right)
    {
        temp = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = temp;
        left += 1;
        right -= 1;
    }
}

static void test_format_description(const char *name, char *buffer, size_t buffer_size)
{
    size_t index;
    size_t output;
    size_t start;
    int capitalize;
    char current;

    if (!buffer || buffer_size == 0)
        return ;
    buffer[0] = '\0';
    if (!name)
        return ;
    start = 0;
    if (ft_strncmp(name, "test_", 5) == 0)
        start = 5;
    index = start;
    output = 0;
    capitalize = 1;
    while (name[index] != '\0' && output + 1 < buffer_size)
    {
        current = name[index];
        if (current == '_')
        {
            if (output > 0 && buffer[output - 1] != ' ')
            {
                buffer[output] = ' ';
                output += 1;
            }
            capitalize = 1;
        }
        else
        {
            if (capitalize && current >= 'a' && current <= 'z')
                current = static_cast<char>(current - 'a' + 'A');
            buffer[output] = current;
            output += 1;
            capitalize = 0;
        }
        index += 1;
    }
    if (output > 0 && buffer[output - 1] == ' ')
        output -= 1;
    buffer[output] = '\0';
}

int test_assert_failure(const char *expression, const char *file, int line)
{
    if (expression)
        pf_printf("Assertion failed: %s (%s:%d)\n", expression, file, line);
    else
        pf_printf("Assertion failed at %s:%d\n", file, line);
    return (FT_FAILURE);
}

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

int test_expect_size_t_equal(size_t actual, size_t expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %zu, got %zu)\n", message, expected, actual);
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

static int test_execute_command(const char *command, int expect_success)
{
    int pipe_fds[2];
    pid_t pid;
    char buffer[256];
    ssize_t bytes_read;
    int status;

    if (!command)
        return (FT_FAILURE);
    if (pipe(pipe_fds) != 0)
        return (FT_FAILURE);
    pid = fork();
    if (pid < 0)
    {
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        return (FT_FAILURE);
    }
    if (pid == 0)
    {
        if (dup2(pipe_fds[1], STDOUT_FILENO) < 0)
            _exit(127);
        if (dup2(pipe_fds[1], STDERR_FILENO) < 0)
            _exit(127);
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        execl("/bin/sh", "sh", "-c", command, (char *)NULL);
        _exit(127);
    }
    close(pipe_fds[1]);
    while (1)
    {
        bytes_read = read(pipe_fds[0], buffer, sizeof(buffer));
        if (bytes_read > 0)
            continue ;
        if (bytes_read == 0)
            break ;
        if (errno == EINTR)
            continue ;
        close(pipe_fds[0]);
        while (waitpid(pid, &status, 0) < 0)
        {
            if (errno != EINTR)
                return (FT_FAILURE);
        }
        return (FT_FAILURE);
    }
    close(pipe_fds[0]);
    while (waitpid(pid, &status, 0) < 0)
    {
        if (errno != EINTR)
            return (FT_FAILURE);
    }
    if (WIFEXITED(status) == 0)
        return (FT_FAILURE);
    if (expect_success)
    {
        if (WEXITSTATUS(status) != 0)
            return (FT_FAILURE);
    }
    else if (WEXITSTATUS(status) == 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int test_run_command(const char *command)
{
    return (test_execute_command(command, 1));
}

int test_run_command_expect_failure(const char *command)
{
    return (test_execute_command(command, 0));
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
    size_t test_index;
    char description[128];
    char index_text[16];

    if (!test)
        return (FT_FAILURE);
    test_index = g_total_tests + 1;
    g_total_tests += 1;
    test_format_description(test->name, description, sizeof(description));
    if (description[0] == '\0' && test->name)
        ft_strlcpy(description, test->name, sizeof(description));
    test_format_index(test_index, index_text, sizeof(index_text));
    status = test->execute();
    if (status != FT_SUCCESS)
    {
        pf_printf("FT_TEST KO test number ");
        pf_printf("%s", index_text);
        pf_printf(" - ");
        pf_printf("%s\n", description);
        g_failed_tests += 1;
    }
    else
    {
        pf_printf("FT_TEST OK test number ");
        pf_printf("%s", index_text);
        pf_printf(" - ");
        pf_printf("%s\n", description);
    }
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

void test_report_summary(void)
{
    size_t passed;

    passed = g_total_tests - g_failed_tests;
    pf_printf("============================================\n");
    pf_printf("Total: %zu | Passed: %zu | Failed: %zu\n", g_total_tests, passed, g_failed_tests);
    if (g_failed_tests == 0)
        pf_printf("Result: ALL TESTS PASSED\n");
    else
        pf_printf("Result: TESTS FAILED\n");
}
