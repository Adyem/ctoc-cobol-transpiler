#include <cstddef>
#include <cstdlib>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "runtime_scalar.hpp"
#include "transpiler_context.hpp"
#include "transpiler_pipeline.hpp"

typedef struct s_test_case
{
    const char *name;
    int (*execute)(void);
}   t_test_case;

static int test_expect_success(int status, const char *message)
{
    if (status == FT_SUCCESS)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s\n", message);
    return (FT_FAILURE);
}

static int test_expect_int_equal(int actual, int expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %d, got %d)\n", message, expected, actual);
    return (FT_FAILURE);
}

static int test_expect_char_equal(char actual, char expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %c, got %c)\n", message, expected, actual);
    return (FT_FAILURE);
}

static int test_write_text_file(const char *path, const char *contents)
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

static int test_read_text_file(const char *path, char *buffer, size_t buffer_size)
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

static int test_run_command(const char *command)
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

static void test_remove_file(const char *path)
{
    if (!path)
        return ;
    unlink(path);
}

static int test_run_command_expect_failure(const char *command)
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

static void test_cleanup_example_artifacts(const char *source_path, const char *binary_path, const char *output_path)
{
    test_remove_file(output_path);
    test_remove_file(binary_path);
    test_remove_file(source_path);
}

static int test_runtime_int_add_and_subtract(void)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, 72);
    runtime_int_set(&right, 30);
    if (test_expect_success(runtime_int_add(left, right, &result), "runtime_int_add should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result.value, 102, "runtime_int_add should add values") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_int_subtract(result, left, &result), "runtime_int_subtract should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result.value, 30, "runtime_int_subtract should subtract values") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_int_add_detects_overflow(void)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, FT_INT_MAX);
    runtime_int_set(&right, 1);
    runtime_int_set(&result, 73);
    if (runtime_int_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_add should detect overflow\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(result.value, 73, "runtime_int_add should not update on overflow") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_int_to_string(void)
{
    t_runtime_int value;
    char buffer[32];

    runtime_int_set(&value, 512);
    if (test_expect_success(runtime_int_to_string(value, buffer, sizeof(buffer)), "runtime_int_to_string should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strncmp(buffer, "512", 4) != 0)
    {
        pf_printf("Assertion failed: runtime_int_to_string should write textual representation\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_runtime_int_to_string_rejects_small_buffer(void)
{
    t_runtime_int value;
    char buffer[1];

    runtime_int_set(&value, 8);
    buffer[0] = 'Z';
    if (runtime_int_to_string(value, buffer, sizeof(buffer)) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_to_string should reject undersized buffers\n");
        return (FT_FAILURE);
    }
    if (test_expect_char_equal(buffer[0], 'Z', "runtime_int_to_string should leave buffer unchanged on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_int_from_string(void)
{
    t_runtime_int value;

    runtime_int_set(&value, 0);
    if (test_expect_success(runtime_int_from_string(&value, "77"), "runtime_int_from_string should parse integers") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(value.value, 77, "runtime_int_from_string should store parsed values") != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_int_set(&value, 42);
    if (runtime_int_from_string(&value, "abc") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_from_string should reject invalid text\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, 42, "runtime_int_from_string should keep destination on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_char_transforms(void)
{
    t_runtime_char value;

    runtime_char_from_string(&value, "a");
    runtime_char_to_upper(&value);
    if (test_expect_char_equal(value.value, 'A', "runtime_char_to_upper should uppercase values") != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_char_to_lower(&value);
    if (test_expect_char_equal(value.value, 'a', "runtime_char_to_lower should lowercase values") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_char_from_string_rejects_empty_input(void)
{
    t_runtime_char value;

    runtime_char_set(&value, 'q');
    if (runtime_char_from_string(&value, "") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_char_from_string should reject empty input\n");
        return (FT_FAILURE);
    }
    if (test_expect_char_equal(value.value, 'q', "runtime_char_from_string should keep destination on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_runtime_char_to_string_and_compare(void)
{
    t_runtime_char left;
    t_runtime_char right;
    char buffer[4];

    runtime_char_set(&left, 'X');
    runtime_char_set(&right, 'Y');
    if (runtime_char_to_string(left, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: runtime_char_to_string should succeed\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, "X", 2) != 0)
    {
        pf_printf("Assertion failed: runtime_char_to_string should copy character to buffer\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(runtime_char_compare(left, right), -1, "runtime_char_compare should order characters") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(runtime_char_compare(right, left), 1, "runtime_char_compare should reverse ordering") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(runtime_char_compare(left, left), 0, "runtime_char_compare should detect equality") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_stage_callback(t_transpiler_context *context, void *user_data)
{
    int *counter;

    (void)context;
    counter = static_cast<int *>(user_data);
    if (!counter)
        return (FT_FAILURE);
    *counter += 1;
    return (FT_SUCCESS);
}

static int test_transpiler_pipeline_executes_stage(void)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_CBL_C, TRANSPILE_LANGUAGE_COBOL);
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "unit-stage", test_stage_callback, &counter), "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_execute(&pipeline, &context), "pipeline should execute successfully") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (test_expect_int_equal(counter, 1, "pipeline should execute the stage once") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int failing_stage_callback(t_transpiler_context *context, void *user_data)
{
    int *counter;

    counter = static_cast<int *>(user_data);
    if (!counter)
        return (FT_FAILURE);
    *counter += 1;
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int early_failing_stage_callback(t_transpiler_context *context, void *user_data)
{
    (void)user_data;
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int test_transpiler_pipeline_reports_failure(void)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "failing-stage", failing_stage_callback, &counter), "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (transpiler_pipeline_execute(&pipeline, &context) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        pf_printf("Assertion failed: pipeline execute should fail when a stage fails\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(counter, 1, "failing stage should still run once") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(pipeline.last_error, FT_FAILURE, "pipeline should retain last error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.count > 0, 1, "diagnostics should record pipeline failure") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.last_error_code, FT_FAILURE, "context should track last error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    return (FT_SUCCESS);
}

static int test_transpiler_pipeline_stops_after_failure(void)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "early-failure", early_failing_stage_callback, NULL), "failing stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "later-stage", test_stage_callback, &counter), "second stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (transpiler_pipeline_execute(&pipeline, &context) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        pf_printf("Assertion failed: pipeline should stop execution when an early stage fails\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(counter, 0, "pipeline should not run later stages after a failure") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    return (FT_SUCCESS);
}

static int test_compiler_builds_example_c_file(void)
{
    const char *source_path;
    const char *binary_path;
    const char *output_path;
    const char *source_code;
    char command[256];
    char output_buffer[128];
    int command_length;

    source_path = "test_example_compiler.c";
    binary_path = "test_example_compiler.bin";
    output_path = "test_example_compiler.txt";
    source_code = "#include <stdio.h>\n"
        "int main(void)\n"
        "{\n"
        "    printf(\"example-ok\\n\");\n"
        "    return 0;\n"
        "}\n";
    if (test_write_text_file(source_path, source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s", source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build sample source\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiled program should run successfully\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, "example-ok\n", 12) != 0)
    {
        pf_printf("Assertion failed: compiled program should emit expected output\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(source_path, binary_path, output_path);
    return (FT_SUCCESS);
}

static int test_compiler_rejects_invalid_c_file(void)
{
    const char *source_path;
    const char *binary_path;
    const char *source_code;
    char command[256];
    int command_length;

    source_path = "test_example_invalid_compiler.c";
    binary_path = "test_example_invalid_compiler.bin";
    source_code = "int main(void)\n"
        "{\n"
        "    return 0\n";
    if (test_write_text_file(source_path, source_code) != FT_SUCCESS)
    {
        test_remove_file(binary_path);
        test_remove_file(source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s", source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_remove_file(binary_path);
        test_remove_file(source_path);
        return (FT_FAILURE);
    }
    if (test_run_command_expect_failure(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should reject invalid source\n");
        test_remove_file(binary_path);
        test_remove_file(source_path);
        return (FT_FAILURE);
    }
    test_remove_file(binary_path);
    test_remove_file(source_path);
    return (FT_SUCCESS);
}

static int run_test_case(const t_test_case *test)
{
    int status;

    pf_printf("Running %s...\n", test->name);
    status = test->execute();
    if (status != FT_SUCCESS)
        pf_printf("FAILED %s\n", test->name);
    else
        pf_printf("PASSED %s\n", test->name);
    return (status);
}

static int run_all_tests(void)
{
    static const t_test_case tests[] = {
        {"runtime_int_add_and_subtract", test_runtime_int_add_and_subtract},
        {"runtime_int_add_detects_overflow", test_runtime_int_add_detects_overflow},
        {"runtime_int_to_string", test_runtime_int_to_string},
        {"runtime_int_to_string_rejects_small_buffer", test_runtime_int_to_string_rejects_small_buffer},
        {"runtime_int_from_string", test_runtime_int_from_string},
        {"runtime_char_transforms", test_runtime_char_transforms},
        {"runtime_char_from_string_rejects_empty_input", test_runtime_char_from_string_rejects_empty_input},
        {"runtime_char_to_string_and_compare", test_runtime_char_to_string_and_compare},
        {"transpiler_pipeline_executes_stage", test_transpiler_pipeline_executes_stage},
        {"transpiler_pipeline_reports_failure", test_transpiler_pipeline_reports_failure},
        {"transpiler_pipeline_stops_after_failure", test_transpiler_pipeline_stops_after_failure},
        {"compiler_builds_example_c_file", test_compiler_builds_example_c_file},
        {"compiler_rejects_invalid_c_file", test_compiler_rejects_invalid_c_file}
    };
    size_t index;
    int status;

    index = 0;
    status = FT_SUCCESS;
    while (index < sizeof(tests) / sizeof(tests[0]))
    {
        if (run_test_case(&tests[index]) != FT_SUCCESS)
            status = FT_FAILURE;
        index += 1;
    }
    return (status);
}

int main(void)
{
    if (run_all_tests() != FT_SUCCESS)
        return (1);
    return (0);
}
