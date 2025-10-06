#include "test_suites.hpp"

static void test_cleanup_example_artifacts(const char *source_path, const char *binary_path, const char *output_path)
{
    test_remove_file(output_path);
    test_remove_file(binary_path);
    test_remove_file(source_path);
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

const t_test_case *get_compiler_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"compiler_builds_example_c_file", test_compiler_builds_example_c_file},
        {"compiler_rejects_invalid_c_file", test_compiler_rejects_invalid_c_file}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
