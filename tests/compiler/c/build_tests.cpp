#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_c_build_tests(size_t *count);

FT_TEST(test_compiler_builds_example_c_file)
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

FT_TEST(test_compiler_rejects_invalid_c_file)
{
    const char *source_path;
    const char *binary_path;
    const char *output_path;
    const char *source_code;
    const char *expected_fragment;
    char command[256];
    char output_buffer[512];
    int command_length;

    source_path = "test_invalid_compiler.c";
    binary_path = "test_invalid_compiler.bin";
    output_path = "test_invalid_compiler.txt";
    source_code = "int main(void) { return (0) }";
    expected_fragment = "error:";
    if (test_write_text_file(source_path, source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s 2> %s", source_path, binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) == FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should reject invalid source\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(output_buffer, expected_fragment, ft_strlen(output_buffer)))
    {
        pf_printf("Assertion failed: compiler output should include error fragment\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(source_path, binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_builds_multi_file_project)
{
    const char *main_source_path;
    const char *helper_source_path;
    const char *binary_path;
    const char *output_path;
    const char *main_source_code;
    const char *helper_source_code;
    char command[256];
    char output_buffer[128];
    int command_length;

    main_source_path = "test_multi_file_main.c";
    helper_source_path = "test_multi_file_helper.c";
    binary_path = "test_multi_file.bin";
    output_path = "test_multi_file.txt";
    main_source_code = "#include <stdio.h>\n"
        "int helper(void);\n"
        "int main(void)\n"
        "{\n"
        "    printf(\"multi-file:%d\\n\", helper());\n"
        "    return 0;\n"
        "}\n";
    helper_source_code = "int helper(void)\n"
        "{\n"
        "    return 42;\n"
        "}\n";
    if (test_write_text_file(main_source_path, main_source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    if (test_write_text_file(helper_source_path, helper_source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s %s -o %s", main_source_path, helper_source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build multi-file project\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiled multi-file program should run successfully\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, "multi-file:42\n", 14) != 0)
    {
        pf_printf("Assertion failed: multi-file program should emit expected output\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
    test_remove_file(helper_source_path);
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_builds_multi_file_project_from_objects)
{
    const char *main_source_path;
    const char *helper_source_path;
    const char *main_object_path;
    const char *helper_object_path;
    const char *binary_path;
    const char *output_path;
    const char *main_source_code;
    const char *helper_source_code;
    char command[256];
    char output_buffer[128];
    int command_length;

    main_source_path = "test_multi_obj_main.c";
    helper_source_path = "test_multi_obj_helper.c";
    main_object_path = "test_multi_obj_main.o";
    helper_object_path = "test_multi_obj_helper.o";
    binary_path = "test_multi_obj.bin";
    output_path = "test_multi_obj.txt";
    main_source_code = "#include <stdio.h>\n"
        "int helper(void);\n"
        "int main(void)\n"
        "{\n"
        "    printf(\"multi-obj:%d\\n\", helper());\n"
        "    return 0;\n"
        "}\n";
    helper_source_code = "int helper(void)\n"
        "{\n"
        "    return 24;\n"
        "}\n";
    if (test_write_text_file(main_source_path, main_source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_write_text_file(helper_source_path, helper_source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc -c %s -o %s", main_source_path, main_object_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build main object\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc -c %s -o %s", helper_source_path, helper_object_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build helper object\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s %s -o %s", main_object_path, helper_object_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: linker should build binary from objects\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: binary should run successfully\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, "multi-obj:24\n", 13) != 0)
    {
        pf_printf("Assertion failed: binary should emit expected output\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
    test_remove_file(helper_source_path);
    test_remove_file(main_object_path);
    test_remove_file(helper_object_path);
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_builds_multi_file_project_with_header)
{
    const char *main_source_path;
    const char *helper_source_path;
    const char *header_path;
    const char *binary_path;
    const char *output_path;
    const char *main_source_code;
    const char *helper_source_code;
    const char *header_code;
    char command[256];
    char output_buffer[128];
    int command_length;

    main_source_path = "test_multi_header_main.c";
    helper_source_path = "test_multi_header_helper.c";
    header_path = "test_multi_header.h";
    binary_path = "test_multi_header.bin";
    output_path = "test_multi_header.txt";
    main_source_code = "#include <stdio.h>\n"
        "#include \"test_multi_header.h\"\n"
        "int main(void)\n"
        "{\n"
        "    printf(\"multi-header:%d\\n\", helper());\n"
        "    return 0;\n"
        "}\n";
    helper_source_code = "#include \"test_multi_header.h\"\n"
        "int helper(void)\n"
        "{\n"
        "    return 33;\n"
        "}\n";
    header_code = "int helper(void);\n";
    if (test_write_text_file(main_source_path, main_source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    if (test_write_text_file(helper_source_path, helper_source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    if (test_write_text_file(header_path, header_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s %s -o %s", main_source_path, helper_source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build header-based project\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: header project should run successfully\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, "multi-header:33\n", 17) != 0)
    {
        pf_printf("Assertion failed: header project should emit expected output\n");
        test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
        test_remove_file(helper_source_path);
        test_remove_file(header_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(main_source_path, binary_path, output_path);
    test_remove_file(helper_source_path);
    test_remove_file(header_path);
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_c_build_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"compiler_builds_example_c_file", test_compiler_builds_example_c_file},
        {"compiler_rejects_invalid_c_file", test_compiler_rejects_invalid_c_file},
        {"compiler_builds_multi_file_project", test_compiler_builds_multi_file_project},
        {"compiler_builds_multi_file_project_from_objects", test_compiler_builds_multi_file_project_from_objects},
        {"compiler_builds_multi_file_project_with_header", test_compiler_builds_multi_file_project_with_header}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

