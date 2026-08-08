#include "test_support.hpp"

#include <cerrno>
#include <cstdlib>
#include <cstdio>
#include <fstream>
#include <filesystem>
#include <string>

static size_t g_total_tests = 0;
static size_t g_failed_tests = 0;
static int g_checked_cobc = 0;
static int g_has_cobc = 0;
static int g_checked_forward_translation = 0;
static int g_forward_translation_supported = 0;
static int g_forward_translation_requested = 0;
static int g_forward_translation_notice_emitted = 0;

static int test_parse_truthy_env(const char *value)
{
    if (!value || *value == '\0')
        return (0);
    if (value[0] == '1' && value[1] == '\0')
        return (1);
    if ((value[0] == 'y' || value[0] == 'Y') && value[1] == '\0')
        return (1);
    if ((value[0] == 't' || value[0] == 'T') && value[1] == '\0')
        return (1);
    if ((value[0] == 'o' || value[0] == 'O') && (value[1] == 'n' || value[1] == 'N')
        && value[2] == '\0')
        return (1);
    return (0);
}

static void test_forward_translation_probe(void)
{
    const char *env;

    if (g_checked_forward_translation)
        return ;
    g_checked_forward_translation = 1;
    g_forward_translation_supported = 0;
    g_forward_translation_requested = 0;
    env = getenv("CTOC_ENABLE_FORWARD_TRANSLATION");
    if (env && test_parse_truthy_env(env))
        g_forward_translation_requested = 1;
    if (g_forward_translation_requested && test_cobc_available())
        g_forward_translation_supported = 1;
}

static int test_capture_stream_begin(t_test_output_capture *capture, int fd)
{
    if (!capture)
        return (FT_FAILURE);
    capture->target = fd;
    capture->active = 1;
    std::snprintf(capture->path, sizeof(capture->path),
        "ctoc_test_capture_%s.tmp", fd == 1 ? "stdout" : "stderr");
    if (!std::freopen(capture->path, "w", fd == 1 ? stdout : stderr))
    {
        capture->active = 0;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_capture_stream_end(t_test_output_capture *capture, int fd, char *buffer, size_t buffer_size,
    std::ptrdiff_t *length)
{
    if (!capture || !buffer || buffer_size == 0 || !capture->active)
        return (FT_FAILURE);
    if (capture->target != fd)
        return (FT_FAILURE);
    std::fflush(fd == 1 ? stdout : stderr);
    {
        std::ifstream stream(capture->path, std::ios::in | std::ios::binary);
        stream.read(buffer, static_cast<std::streamsize>(buffer_size - 1));
        if (!stream && !stream.eof())
            return (FT_FAILURE);
        std::streamsize read_length = stream.gcount();
        buffer[read_length] = '\0';
        if (length)
            *length = static_cast<std::ptrdiff_t>(read_length);
    }
    if (!std::freopen("ctoc_test_capture_sink.tmp", "w", fd == 1 ? stdout : stderr))
        return (FT_FAILURE);
    std::remove(capture->path);
    capture->active = 0;
    return (FT_SUCCESS);
}

int test_capture_stdout_begin(t_test_output_capture *capture)
{
    return (test_capture_stream_begin(capture, 1));
}

int test_capture_stdout_end(t_test_output_capture *capture, char *buffer, size_t buffer_size,
    std::ptrdiff_t *length)
{
    return (test_capture_stream_end(capture, 1, buffer, buffer_size, length));
}

int test_capture_stderr_begin(t_test_output_capture *capture)
{
    return (test_capture_stream_begin(capture, 2));
}

int test_capture_stderr_end(t_test_output_capture *capture, char *buffer, size_t buffer_size,
    std::ptrdiff_t *length)
{
    return (test_capture_stream_end(capture, 2, buffer, buffer_size, length));
}

int test_cobc_available(void)
{
    if (g_checked_cobc)
        return (g_has_cobc);
    g_checked_cobc = 1;
    g_has_cobc = (std::system("cobc --version > cobc_probe.log 2>&1") == 0);
    std::remove("cobc_probe.log");
    return (g_has_cobc);
}

int test_forward_translation_available(void)
{
    if (!g_checked_forward_translation)
        test_forward_translation_probe();
    return (g_forward_translation_supported);
}

int test_forward_translation_requested(void)
{
    if (!g_checked_forward_translation)
        test_forward_translation_probe();
    return (g_forward_translation_requested);
}

int test_require_cobc_dependency(const char *test_name)
{
    if (test_cobc_available())
        return (FT_SUCCESS);
    if (!test_name || test_name[0] == '\0')
        test_name = "COBOL compiler dependent test";
    std::printf("Missing dependency: install 'cobc' to run %s.\n", test_name);
    return (FT_FAILURE);
}

int test_require_forward_translation_dependency(const char *test_name)
{
    if (!test_forward_translation_requested())
    {
        if (!g_forward_translation_notice_emitted)
        {
            std::printf("Skipping forward translation tests; set CTOC_ENABLE_FORWARD_TRANSLATION=1 to enable.\n");
            g_forward_translation_notice_emitted = 1;
        }
        return (FT_SKIP);
    }
    if (test_cobc_available())
        return (FT_SUCCESS);
    if (!test_name || test_name[0] == '\0')
        test_name = "forward translation test";
    std::printf("Missing dependency: install 'cobc' to run %s.\n", test_name);
    return (FT_FAILURE);
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
    if (std::strncmp(name, "test_", 5) == 0)
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
        std::printf("Assertion failed: %s (%s:%d)\n", expression, file, line);
    else
        std::printf("Assertion failed at %s:%d\n", file, line);
    return (FT_FAILURE);
}

int test_expect_success(int status, const char *message)
{
    if (status == FT_SUCCESS)
        return (FT_SUCCESS);
    if (message)
        std::printf("Assertion failed: %s\n", message);
    return (FT_FAILURE);
}

int test_expect_int_equal(int actual, int expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        std::printf("Assertion failed: %s (expected %d, got %d)\n", message, expected, actual);
    return (FT_FAILURE);
}

int test_expect_size_t_equal(size_t actual, size_t expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        std::printf("Assertion failed: %s (expected %zu, got %zu)\n", message, expected, actual);
    return (FT_FAILURE);
}

int test_expect_char_equal(char actual, char expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        std::printf("Assertion failed: %s (expected %c, got %c)\n", message, expected, actual);
    return (FT_FAILURE);
}

int test_expect_cstring_equal(const char *actual, const char *expected, const char *message)
{
    if (!actual && !expected)
        return (FT_SUCCESS);
    if (!actual || !expected)
    {
        if (message)
            std::printf("Assertion failed: %s (expected %s, got %s)\n", message,
                expected ? expected : "(null)", actual ? actual : "(null)");
        return (FT_FAILURE);
    }
    {
        size_t actual_index = 0;
        size_t expected_index = 0;
        while (actual[actual_index] != '\0' && expected[expected_index] != '\0')
        {
            if (actual[actual_index] == '\r')
                actual_index += 1;
            if (expected[expected_index] == '\r')
                expected_index += 1;
            if (actual[actual_index] != expected[expected_index])
                break;
            actual_index += 1;
            expected_index += 1;
        }
        if (actual[actual_index] == '\0' && expected[expected_index] == '\0')
            return (FT_SUCCESS);
    }
    if (message)
        std::printf("Assertion failed: %s (expected %s, got %s)\n", message, expected, actual);
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
        std::printf("Assertion failed: token kind mismatch (expected %d, got %d)\n", expected_kind, token->kind);
        return (FT_FAILURE);
    }
    if (token->line != expected_line)
    {
        std::printf("Assertion failed: token line mismatch (expected %zu, got %zu)\n", expected_line, token->line);
        return (FT_FAILURE);
    }
    if (token->column != expected_column)
    {
        std::printf("Assertion failed: token column mismatch (expected %zu, got %zu)\n", expected_column, token->column);
        return (FT_FAILURE);
    }
    if (!expected_lexeme)
    {
        if (token->length != 0)
        {
            std::printf("Assertion failed: token length mismatch (expected 0, got %zu)\n", token->length);
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    expected_length = std::strlen(expected_lexeme);
    if (token->length != expected_length)
    {
        std::printf("Assertion failed: token length mismatch (expected %zu, got %zu)\n", expected_length, token->length);
        return (FT_FAILURE);
    }
    if (!token->lexeme)
    {
        std::printf("Assertion failed: token lexeme should not be null\n");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < expected_length)
    {
        if (token->lexeme[index] != expected_lexeme[index])
        {
            std::printf("Assertion failed: token lexeme mismatch at index %zu (expected %c, got %c)\n",
                index, expected_lexeme[index], token->lexeme[index]);
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

int test_write_text_file(const char *path, const char *contents)
{
    size_t length;
    std::ofstream stream;

    if (!path)
        return (FT_FAILURE);
    if (!contents)
        return (FT_FAILURE);
    stream.open(path, std::ios::out | std::ios::binary | std::ios::trunc);
    if (!stream)
        return (FT_FAILURE);
    length = std::strlen(contents);
    stream.write(contents, static_cast<std::streamsize>(length));
    return (FT_SUCCESS);
}

int test_read_text_file(const char *path, char *buffer, size_t buffer_size)
{
    std::ifstream stream;
    size_t offset;

    if (!path)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    stream.open(path, std::ios::in);
    if (!stream)
        return (FT_FAILURE);
    stream.read(buffer, static_cast<std::streamsize>(buffer_size - 1));
    offset = static_cast<size_t>(stream.gcount());
    buffer[offset] = '\0';
    return (FT_SUCCESS);
}

static int test_execute_command(const char *command, int expect_success)
{
    std::string command_text;

    if (!command)
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
    if ((std::system(command_text.c_str()) == 0) != (expect_success != 0))
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
    std::string executable_path;

    if (!path)
        return ;
    std::remove(path);
    executable_path = path;
    if (executable_path.size() >= 4
        && executable_path.compare(executable_path.size() - 4, 4, ".bin") == 0)
    {
        executable_path.replace(executable_path.size() - 4, 4, ".exe");
        std::remove(executable_path.c_str());
    }
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
        std::printf("FT_TEST KO test number ");
        std::printf("%s", index_text);
        std::printf(" - ");
        std::printf("%s\n", description);
        g_failed_tests += 1;
    }
    else
    {
        std::printf("FT_TEST OK test number ");
        std::printf("%s", index_text);
        std::printf(" - ");
        std::printf("%s\n", description);
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
    std::printf("============================================\n");
    std::printf("Total: %zu | Passed: %zu | Failed: %zu\n", g_total_tests, passed, g_failed_tests);
    if (g_failed_tests == 0)
        std::printf("Result: ALL TESTS PASSED\n");
    else
        std::printf("Result: TESTS FAILED\n");
}
