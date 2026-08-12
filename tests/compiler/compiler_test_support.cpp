#include "compiler_test_support.hpp"

#include "../test_support.hpp"

#include <cerrno>
#include <cstdlib>
#include <filesystem>
#include <string>

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
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    std::filesystem::path directory = std::filesystem::temp_directory_path()
        / std::filesystem::path("ctoc_compiler_test");
    int suffix = 0;
    while (std::filesystem::exists(directory))
        directory = std::filesystem::temp_directory_path()
            / std::filesystem::path("ctoc_compiler_test_" + std::to_string(++suffix));
    if (!std::filesystem::create_directory(directory))
        return (FT_FAILURE);
    size_t required = ft_strlcpy(buffer, directory.string().c_str(), buffer_size);
    if (required >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int test_join_path(const char *directory, const char *name, char *buffer, size_t buffer_size)
{
    int length;

    if (!directory || !name || !buffer)
        return (FT_FAILURE);
    length = std::snprintf(buffer, buffer_size, "%s/%s", directory, name);
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
    std::filesystem::remove_all(path);
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
    std::string command_text;

    if (!command || !exit_status)
        return (FT_FAILURE);
    command_text = command;
    {
        const std::string current_directory = std::filesystem::path("./").make_preferred().string();
        size_t position = 0;
        while ((position = command_text.find("./", position)) != std::string::npos)
        {
            command_text.replace(position, 2, current_directory);
            position += current_directory.length();
        }
        position = 0;
        while ((position = command_text.find(current_directory, position)) != std::string::npos)
        {
            const size_t next = position + current_directory.length();
            if (next < command_text.length()
                && (command_text[next] == '/' || (next + 1 < command_text.length()
                    && command_text[next + 1] == ':')))
                command_text.erase(position, current_directory.length());
            else
                position = next;
        }
        position = 0;
        while ((position = command_text.find("./C:", position)) != std::string::npos)
            command_text.erase(position, 2);
        position = 0;
        while ((position = command_text.find(".\\C:", position)) != std::string::npos)
            command_text.erase(position, 2);
    }
#if defined(_WIN32)
    if (command_text.find("cobc") == std::string::npos
        && command_text.find(" -o ") == std::string::npos)
    {
        size_t position = 0;
        while ((position = command_text.find(".bin", position)) != std::string::npos)
        {
            size_t token_start = command_text.rfind(' ', position);
            std::string candidate;

            token_start = (token_start == std::string::npos ? 0 : token_start + 1);
            if (token_start < position)
            {
                candidate = command_text.substr(token_start, position + 4 - token_start);
                if (std::filesystem::exists(candidate.substr(0, candidate.length() - 4) + ".exe")
                    || command_text.find("cd ") != std::string::npos)
                {
                    command_text.replace(position, 4, ".exe");
                    position += 4;
                    continue;
                }
            }
            position += 4;
        }
    }
#endif
    if (std::system("cc --version > cc_probe.log 2>&1") != 0)
    {
        size_t position = 0;
        while ((position = command_text.find("cc ", position)) != std::string::npos)
        {
            command_text.replace(position, 3, "g++ ");
            position += 4;
        }
    }
    std::remove("cc_probe.log");
    {
        const size_t separator = command_text.find(' ');
        const size_t equals = command_text.find('=');

        const std::string temporary_directory = std::filesystem::temp_directory_path().string();
        if (temporary_directory.size() > 1 && temporary_directory[1] == ':'
            && separator != std::string::npos && equals != std::string::npos && equals < separator)
        {
            const std::string assignment = command_text.substr(0, separator);
            command_text = "set \"" + assignment + "\" &&" + command_text.substr(separator);
        }
    }
    if (std::filesystem::temp_directory_path().string().size() > 1
        && std::filesystem::temp_directory_path().string()[1] == ':')
    {
        size_t marker = 0;
        while ((marker = command_text.find("&& ", marker)) != std::string::npos)
        {
            const size_t start = marker + 3;
            const size_t separator = command_text.find(' ', start);
            const size_t equals = command_text.find('=', start);
            if (separator != std::string::npos && equals != std::string::npos && equals < separator)
            {
                const std::string assignment = command_text.substr(start, separator - start);
                command_text.replace(start, separator - start, "set \"" + assignment + "\" &&");
                marker = start + assignment.length() + 8;
            }
            else
                marker += 3;
        }
    }
    *exit_status = std::system(command_text.c_str());
    return (FT_SUCCESS);
}

int test_resolve_module_path(const char *directory, const char *base_name, char *buffer, size_t buffer_size)
{
    if (!directory || !base_name || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    for (const std::filesystem::directory_entry &entry : std::filesystem::directory_iterator(directory))
    {
        if (entry.is_regular_file() && entry.path().stem().string() == base_name)
        {
            if (ft_strlcpy(buffer, entry.path().string().c_str(), buffer_size) >= buffer_size)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
    }
    return (FT_FAILURE);
}

int test_cobol_fixture_contains(const char *path, const char *snippet)
{
    char buffer[4096];

    if (!path || !snippet)
        return (FT_FAILURE);
    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        std::printf("Assertion failed: expected to read COBOL fixture %s\n", path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, snippet, std::strlen(buffer)))
    {
        std::printf("Assertion failed: COBOL fixture %s should contain snippet:\n%s\n", path, snippet);
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
        std::printf("Assertion failed: expected to read file %s\n", path);
        return (FT_FAILURE);
    }
    if (std::strncmp(buffer, expected, std::strlen(expected) + 1) != 0)
    {
        std::printf("Assertion failed: file %s did not match expected content\n", path);
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
        std::printf("Assertion failed: unable to normalize transcript before comparison\n");
        return (FT_FAILURE);
    }
    if (std::strncmp(normalized, expected, std::strlen(expected) + 1) != 0)
    {
        std::printf("Assertion failed: transcript did not match expected text\n");
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
        std::printf("Assertion failed: expected to read compiler output from %s\n", path);
        return (FT_FAILURE);
    }
    length = std::strlen(buffer);
    warning = "<command-line>: warning: \"_FORTIFY_SOURCE\" redefined";
    if (length > 0 && !ft_strnstr(buffer, warning, length))
    {
        std::printf("Assertion failed: compiler output should include expected warning message\n");
        return (FT_FAILURE);
    }
    if (ft_strnstr(buffer, "error:", length))
    {
        std::printf("Assertion failed: compiler output should not report errors\n");
        return (FT_FAILURE);
    }
    if (ft_strnstr(buffer, "Error", length))
    {
        std::printf("Assertion failed: compiler output should not contain fatal errors\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}
