#include "compiler_test_support.hpp"

#include "../test_support.hpp"

#include <cstdlib>
#include <sys/wait.h>
#include <unistd.h>

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
    int status;

    if (!command || !exit_status)
        return (FT_FAILURE);
    status = system(command);
    if (status == -1)
        return (FT_FAILURE);
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
