#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

const t_test_case *get_compiler_c_multi_module_tests(size_t *count);

static int test_compiler_c_backend_write_source(const char *path, const char *contents)
{
    if (test_write_text_file(path, contents) != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_c_backend_multi_module_generates_and_runs)
{
    const char *main_source;
    const char *worker_source;
    const char *expected_output;
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char main_output_path[256];
    char worker_output_path[256];
    char binary_path[256];
    char output_path[256];
    char command[512];
    char output_buffer[128];
    int command_length;
    int status;

    main_source = "import \"test_worker.cblc\";\n"
        "\n"
        "int counter;\n"
        "\n"
        "function void increment()\n"
        "{\n"
        "    counter = counter + 1;\n"
        "    return;\n"
        "}\n"
        "\n"
        "function void main()\n"
        "{\n"
        "    counter = 0;\n"
        "    announce();\n"
        "    increment();\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    worker_source = "function void announce()\n"
        "{\n"
        "    display(\"C BACKEND READY\");\n"
        "    return;\n"
        "}\n";
    expected_output = "C BACKEND READY\n1\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    status = FT_FAILURE;
    if (test_join_path(directory, "test_main.cblc", main_source_path,
            sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "test_worker.cblc", worker_source_path,
            sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "test_main.c", main_output_path,
            sizeof(main_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "test_worker.c", worker_output_path,
            sizeof(worker_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "test_program.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "test_program.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_compiler_c_backend_write_source(main_source_path, main_source) != FT_SUCCESS)
        goto cleanup;
    if (test_compiler_c_backend_write_source(worker_source_path, worker_source) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --direction cblc-to-c --input %s --output %s --input %s --output %s",
        main_source_path, main_output_path, worker_source_path, worker_output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "cc %s %s -o %s", main_output_path, worker_output_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(output_buffer, expected_output,
            ft_strlen(expected_output) + 1) != 0)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    test_remove_file(output_path);
    test_remove_file(binary_path);
    test_remove_file(worker_output_path);
    test_remove_file(main_output_path);
    test_remove_file(worker_source_path);
    test_remove_file(main_source_path);
    test_remove_directory(directory);
    return (status);
}

FT_TEST(test_compiler_c_backend_multi_module_call_order_stable)
{
    const char *main_source;
    const char *worker_source;
    char directory[256];
    char main_source_path[256];
    char worker_source_path[256];
    char main_output_path[256];
    char worker_output_path[256];
    char command[512];
    char generated_main[65536];
    size_t announce_position;
    size_t increment_position;
    size_t index;
    int command_length;
    int status;

    main_source = "import \"worker_module.cblc\";\n"
        "\n"
        "int counter;\n"
        "\n"
        "function void increment()\n"
        "{\n"
        "    counter = counter + 1;\n"
        "    return;\n"
        "}\n"
        "\n"
        "function void main()\n"
        "{\n"
        "    counter = 0;\n"
        "    announce();\n"
        "    increment();\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    worker_source = "function void announce()\n"
        "{\n"
        "    display(\"C BACKEND READY\");\n"
        "    return;\n"
        "}\n";
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    status = FT_FAILURE;
    if (test_join_path(directory, "main_module.cblc", main_source_path,
            sizeof(main_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "worker_module.cblc", worker_source_path,
            sizeof(worker_source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "main_module.c", main_output_path,
            sizeof(main_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "worker_module.c", worker_output_path,
            sizeof(worker_output_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_compiler_c_backend_write_source(main_source_path, main_source) != FT_SUCCESS)
        goto cleanup;
    if (test_compiler_c_backend_write_source(worker_source_path, worker_source) != FT_SUCCESS)
        goto cleanup;
    command_length = pf_snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --direction cblc-to-c --input %s --output %s --input %s --output %s",
        main_source_path, main_output_path, worker_source_path, worker_output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(main_output_path, generated_main,
            sizeof(generated_main)) != FT_SUCCESS)
        goto cleanup;
    announce_position = static_cast<size_t>(-1);
    increment_position = static_cast<size_t>(-1);
    index = 0;
    while (generated_main[index] != '\0')
    {
        if (announce_position == static_cast<size_t>(-1)
            && ft_strncmp(&generated_main[index], "announce();", 11) == 0)
            announce_position = index;
        if (increment_position == static_cast<size_t>(-1)
            && ft_strncmp(&generated_main[index], "increment();", 12) == 0)
            increment_position = index;
        index += 1;
    }
    if (announce_position == static_cast<size_t>(-1)
        || increment_position == static_cast<size_t>(-1))
        goto cleanup;
    if (announce_position >= increment_position)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    test_remove_file(main_output_path);
    test_remove_file(worker_output_path);
    test_remove_file(worker_source_path);
    test_remove_file(main_source_path);
    test_remove_directory(directory);
    return (status);
}

const t_test_case *get_compiler_c_multi_module_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"compiler_c_backend_multi_module_generates_and_runs",
            test_compiler_c_backend_multi_module_generates_and_runs},
        {"compiler_c_backend_multi_module_call_order_stable",
            test_compiler_c_backend_multi_module_call_order_stable}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

