#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_c_exit_tests(size_t *count);

FT_TEST(test_compiler_multi_file_propagates_exit_status)
{
    const char *main_source_path;
    const char *helper_source_path;
    const char *binary_path;
    char command[256];
    int command_length;
    int exit_status;

    main_source_path = "test_exit_main.c";
    helper_source_path = "test_exit_helper.c";
    binary_path = "test_exit_multi.bin";
    if (test_write_text_file(main_source_path,
            "int helper(void);\n"
            "int main(void)\n"
            "{\n"
            "    return (helper());\n"
            "}\n") != FT_SUCCESS)
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        return (FT_FAILURE);
    }
    if (test_write_text_file(helper_source_path,
            "int helper(void)\n"
            "{\n"
            "    return (29);\n"
            "}\n") != FT_SUCCESS)
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s %s -o %s", main_source_path, helper_source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build exit-status project\n");
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture exit status\n");
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, NULL);
    test_remove_file(helper_source_path);
    test_remove_file(main_source_path);
    if (exit_status != 29)
    {
        pf_printf("Assertion failed: exit status should propagate from helper\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_object_files_propagate_exit_status)
{
    const char *main_source_path;
    const char *helper_source_path;
    const char *main_object_path;
    const char *helper_object_path;
    const char *binary_path;
    char command[256];
    int command_length;
    int exit_status;

    main_source_path = "test_exit_obj_main.c";
    helper_source_path = "test_exit_obj_helper.c";
    main_object_path = "test_exit_obj_main.o";
    helper_object_path = "test_exit_obj_helper.o";
    binary_path = "test_exit_obj.bin";
    if (test_write_text_file(main_source_path,
            "int helper(void);\n"
            "int main(void)\n"
            "{\n"
            "    return (helper());\n"
            "}\n") != FT_SUCCESS)
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        return (FT_FAILURE);
    }
    if (test_write_text_file(helper_source_path,
            "int helper(void)\n"
            "{\n"
            "    return (17);\n"
            "}\n") != FT_SUCCESS)
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc -c %s -o %s", main_source_path, main_object_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build main object\n");
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc -c %s -o %s", helper_source_path, helper_object_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build helper object\n");
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s %s -o %s", main_object_path, helper_object_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: linker should build binary from objects\n");
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: harness should capture exit status\n");
        test_cleanup_generated_artifacts(binary_path, NULL);
        test_remove_file(helper_source_path);
        test_remove_file(main_object_path);
        test_remove_file(helper_object_path);
        test_remove_file(main_source_path);
        return (FT_FAILURE);
    }
    test_cleanup_generated_artifacts(binary_path, NULL);
    test_remove_file(helper_source_path);
    test_remove_file(main_object_path);
    test_remove_file(helper_object_path);
    test_remove_file(main_source_path);
    if (exit_status != 17)
    {
        pf_printf("Assertion failed: exit status should propagate through object files\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_reports_program_exit_status)
{
    const char *source_path;
    const char *binary_path;
    char command[256];
    int command_length;
    int exit_status;

    source_path = "test_exit_single.c";
    binary_path = "test_exit_single.bin";
    if (test_write_text_file(source_path,
            "int main(void)\n"
            "{\n"
            "    return (42);\n"
            "}\n") != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, NULL);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s", source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, NULL);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build exit-status sample\n");
        test_cleanup_example_artifacts(source_path, binary_path, NULL);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s", binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, NULL);
        return (FT_FAILURE);
    }
    if (test_run_command_capture_status(command, &exit_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: harness should capture program exit status\n");
        test_cleanup_example_artifacts(source_path, binary_path, NULL);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(source_path, binary_path, NULL);
    if (exit_status != 42)
    {
        pf_printf("Assertion failed: compiled program should return expected status\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

const t_test_case *get_compiler_c_exit_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"compiler_multi_file_propagates_exit_status", test_compiler_multi_file_propagates_exit_status},
        {"compiler_object_files_propagate_exit_status", test_compiler_object_files_propagate_exit_status},
        {"compiler_reports_program_exit_status", test_compiler_reports_program_exit_status}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

