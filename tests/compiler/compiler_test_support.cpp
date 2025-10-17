#include "compiler_test_support.hpp"

#include "../test_support.hpp"

#include <cerrno>
#include <cstdlib>
#include <sys/wait.h>
#include <unistd.h>

static int test_trim_transcript_lines(const char *source, char *buffer, size_t buffer_size)
{
    size_t read_index;
    size_t write_index;

    if (!source || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    read_index = 0;
    write_index = 0;
    while (source[read_index] != '\0')
    {
        size_t line_start;
        size_t line_end;
        size_t trim_end;

        line_start = read_index;
        line_end = read_index;
        while (source[line_end] != '\0' && source[line_end] != '\n')
            line_end += 1;
        trim_end = line_end;
        while (trim_end > line_start && source[trim_end - 1] == ' ')
            trim_end -= 1;
        while (line_start < trim_end)
        {
            if (write_index + 1 >= buffer_size)
                return (FT_FAILURE);
            buffer[write_index] = source[line_start];
            write_index += 1;
            line_start += 1;
        }
        if (source[line_end] == '\n')
        {
            if (write_index + 1 >= buffer_size)
                return (FT_FAILURE);
            buffer[write_index] = '\n';
            write_index += 1;
            line_end += 1;
        }
        read_index = line_end;
    }
    if (write_index >= buffer_size)
        return (FT_FAILURE);
    buffer[write_index] = '\0';
    return (FT_SUCCESS);
}

void test_cleanup_example_artifacts(const char *source_path, const char *binary_path, const char *output_path)
{
    test_remove_file(output_path);
    test_remove_file(binary_path);
    test_remove_file(source_path);
}

void test_cleanup_generated_artifacts(const char *binary_path, const char *output_path)
{
    test_remove_file(output_path);
    test_remove_file(binary_path);
}

void test_cleanup_example_artifacts_with_log(const char *source_path, const char *binary_path,
    const char *output_path, const char *log_path)
{
    if (log_path)
        test_remove_file(log_path);
    test_cleanup_example_artifacts(source_path, binary_path, output_path);
}

int test_create_temp_directory(char *buffer, size_t buffer_size)
{
    char template_path[256];
    char *result;
    size_t required;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    ft_strlcpy(template_path, "/tmp/ctoc_compiler_testXXXXXX", sizeof(template_path));
    result = mkdtemp(template_path);
    if (!result)
        return (FT_FAILURE);
    required = ft_strlcpy(buffer, result, buffer_size);
    if (required >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int test_join_path(const char *directory, const char *name, char *buffer, size_t buffer_size)
{
    int length;

    if (!directory || !name || !buffer)
        return (FT_FAILURE);
    length = pf_snprintf(buffer, buffer_size, "%s/%s", directory, name);
    if (length < 0)
        return (FT_FAILURE);
    if (static_cast<size_t>(length) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

void test_remove_directory(const char *path)
{
    if (!path || path[0] == '\0')
        return ;
    rmdir(path);
}

void test_cleanup_module_directory(const char *directory, const char *module_path, const char *binary_path,
    const char *output_path)
{
    if (output_path && output_path[0] != '\0')
        test_remove_file(output_path);
    if (binary_path && binary_path[0] != '\0')
        test_remove_file(binary_path);
    if (module_path && module_path[0] != '\0')
        test_remove_file(module_path);
    if (directory && directory[0] != '\0')
        test_remove_directory(directory);
}

int test_run_command_capture_status(const char *command, int *exit_status)
{
    int pipe_fds[2];
    pid_t pid;
    char buffer[256];
    ssize_t bytes_read;
    int status;

    if (!command || !exit_status)
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
    *exit_status = WEXITSTATUS(status);
    return (FT_SUCCESS);
}

int test_cobol_fixture_contains(const char *path, const char *snippet)
{
    char buffer[4096];

    if (!path || !snippet)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to read COBOL fixture %s\n", path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, snippet, ft_strlen(buffer)))
    {
        pf_printf("Assertion failed: COBOL fixture %s should contain snippet:\n%s\n", path, snippet);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

int test_expect_file_equals(const char *path, const char *expected)
{
    char buffer[4096];

    if (!path || !expected)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to read file %s\n", path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, expected, ft_strlen(expected) + 1) != 0)
    {
        pf_printf("Assertion failed: file %s did not match expected content\n", path);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

int test_expect_transcript_equal(const char *actual, const char *expected)
{
    char normalized[4096];

    if (!actual || !expected)
        return (FT_FAILURE);
    if (test_trim_transcript_lines(actual, normalized, sizeof(normalized)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: unable to normalize transcript before comparison\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(normalized, expected, ft_strlen(expected) + 1) != 0)
    {
        pf_printf("Assertion failed: transcript did not match expected text\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

int test_expect_compiler_output_allowed(const char *path)
{
    char buffer[4096];
    const char *warning;
    size_t length;

    if (!path)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to read compiler output from %s\n", path);
        return (FT_FAILURE);
    }
    length = ft_strlen(buffer);
    warning = "<command-line>: warning: \"_FORTIFY_SOURCE\" redefined";
    if (length > 0 && !ft_strnstr(buffer, warning, length))
    {
        pf_printf("Assertion failed: compiler output should include expected warning message\n");
        return (FT_FAILURE);
    }
    if (ft_strnstr(buffer, "error:", length))
    {
        pf_printf("Assertion failed: compiler output should not report errors\n");
        return (FT_FAILURE);
    }
    if (ft_strnstr(buffer, "Error", length))
    {
        pf_printf("Assertion failed: compiler output should not contain fatal errors\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}
