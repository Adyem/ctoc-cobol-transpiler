#include "test_suites.hpp"

#include <cstdlib>
#include <unistd.h>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int cobol_create_temp_directory(char *buffer, size_t buffer_size)
{
    char template_path[256];
    char *result;
    size_t required;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    ft_strlcpy(template_path, "/tmp/ctoc_cobol_testXXXXXX", sizeof(template_path));
    result = mkdtemp(template_path);
    if (!result)
        return (FT_FAILURE);
    required = ft_strlcpy(buffer, result, buffer_size);
    if (required >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cobol_join_path(const char *directory, const char *name, char *buffer, size_t buffer_size)
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

static void cobol_cleanup_artifacts(char *directory, char *first, char *second, char *third)
{
    if (first && first[0] != '\0')
        test_remove_file(first);
    if (second && second[0] != '\0')
        test_remove_file(second);
    if (third && third[0] != '\0')
        test_remove_file(third);
    if (directory && directory[0] != '\0')
        rmdir(directory);
}

FT_TEST(test_cobol_sample_copy_file_executes)
{
    char directory[256];
    char binary_path[256];
    char input_path[256];
    char output_path[256];
    char command[512];
    char output_buffer[512];
    const char *input_contents;
    const char *expected_output;
    int command_length;

    directory[0] = '\0';
    binary_path[0] = '\0';
    input_path[0] = '\0';
    output_path[0] = '\0';
    if (cobol_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_join_path(directory, "copy_file.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cobc -x -o %s samples/cobol/copy_file.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile copy_file fixture\n");
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    if (cobol_join_path(directory, "input.dat", input_path, sizeof(input_path)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    input_contents = "ALPHA\nBRAVO\n";
    if (test_write_text_file(input_path, input_contents) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    if (cobol_join_path(directory, "output.dat", output_path, sizeof(output_path)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cd %s && ./copy_file.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiled copy_file fixture should execute successfully\n");
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    expected_output = "ALPHA\nBRAVO\n";
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: copy_file fixture should copy input contents\n");
        cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
        return (FT_FAILURE);
    }
    cobol_cleanup_artifacts(directory, binary_path, input_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_sample_record_writer_executes)
{
    char directory[256];
    char binary_path[256];
    char report_path[256];
    char command[512];
    char output_buffer[128];
    const char *expected_output;
    int command_length;

    directory[0] = '\0';
    binary_path[0] = '\0';
    report_path[0] = '\0';
    if (cobol_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_join_path(directory, "record_writer.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cobc -x -o %s samples/cobol/record_writer.cob", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: cobc should compile record_writer fixture\n");
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cd %s && ./record_writer.bin", directory);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: record_writer fixture should execute successfully\n");
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    if (cobol_join_path(directory, "report.dat", report_path, sizeof(report_path)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    if (test_read_text_file(report_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    expected_output = "0001INITIAL ENTRY           ";
    if (ft_strncmp(output_buffer, expected_output, ft_strlen(expected_output) + 1) != 0)
    {
        pf_printf("Assertion failed: record_writer fixture should emit expected report record\n");
        cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
        return (FT_FAILURE);
    }
    cobol_cleanup_artifacts(directory, binary_path, report_path, NULL);
    return (FT_SUCCESS);
}

const t_test_case *get_cobol_execution_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cobol_sample_copy_file_executes", test_cobol_sample_copy_file_executes},
        {"cobol_sample_record_writer_executes", test_cobol_sample_record_writer_executes}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
