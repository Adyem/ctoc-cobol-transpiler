#include "round_trip_pipeline_helpers.hpp"

#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

int compile_generated_cobol(const char *binary_path, const char *source_path,
    const char *log_path, const char *failure_context)
{
    char command[512];
    int command_length;

    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s > %s 2>&1", binary_path, source_path, log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        return (FT_FAILURE);
    if (test_run_command(command) != FT_SUCCESS)
    {
        if (failure_context)
            pf_printf("Assertion failed: %s\n", failure_context);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int compile_generated_cobol_with_module(const char *binary_path,
    const char *primary_source_path, const char *secondary_source_path,
    const char *log_path, const char *failure_context)
{
    char command[512];
    int command_length;

    command_length = pf_snprintf(command, sizeof(command),
        "cobc -x -free -o %s %s %s > %s 2>&1", binary_path,
        primary_source_path, secondary_source_path, log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        return (FT_FAILURE);
    if (test_run_command(command) != FT_SUCCESS)
    {
        if (failure_context)
            pf_printf("Assertion failed: %s\n", failure_context);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int execute_binary(const char *directory, const char *binary_name,
    const char *output_name, const char *failure_context)
{
    char command[512];
    int command_length;

    if (output_name)
        command_length = pf_snprintf(command, sizeof(command),
            "cd %s && ./%s > %s", directory, binary_name, output_name);
    else
        command_length = pf_snprintf(command, sizeof(command),
            "cd %s && ./%s", directory, binary_name);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        return (FT_FAILURE);
    if (test_run_command(command) != FT_SUCCESS)
    {
        if (failure_context)
            pf_printf("Assertion failed: %s\n", failure_context);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}
