#include "round_trip_pipeline_helpers.hpp"

#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include <cerrno>
#include <cstdio>
#include <fstream>
#include <string>

static std::string cblc_test_short_binary_path(const char *binary_path)
{
    std::string path;
    const std::string::size_type separator =
        binary_path ? std::string(binary_path).find_last_of("/\\")
            : std::string::npos;

    if (!binary_path)
        return (std::string());
    path = separator == std::string::npos
        ? std::string() : std::string(binary_path, separator + 1);
    path += "cblct.bin";
    return (path);
}

static std::string cblc_test_executable_path(const std::string &binary_path)
{
    std::string executable_path(binary_path);
    const std::string::size_type extension = executable_path.find_last_of('.');

    if (extension == std::string::npos)
        executable_path += ".exe";
    else
        executable_path.replace(extension, std::string::npos, ".exe");
    return (executable_path);
}

static std::string cblc_test_short_source_path(const char *source_path,
    char prefix)
{
    std::string path;
    const std::string source(source_path ? source_path : "");
    const std::string::size_type separator = source.find_last_of("/\\");
    unsigned long hash = 2166136261UL;
    std::size_t index;
    char short_name[24];

    if (source.empty())
        return (std::string());
    for (index = 0; index < source.size(); ++index)
    {
        hash ^= static_cast<unsigned char>(source[index]);
        hash *= 16777619UL;
    }
    if (std::snprintf(short_name, sizeof(short_name), "%c%08lX.cob",
            prefix, hash) < 0)
        return (std::string());
    path = separator == std::string::npos
        ? std::string() : source.substr(0, separator + 1);
    path += short_name;
    return (path);
}

static int cblc_test_copy_source(const char *source_path,
    const std::string &short_source_path)
{
    std::ifstream existing(short_source_path.c_str(), std::ios::binary);
    std::ifstream source(source_path, std::ios::binary);
    std::ofstream destination;

    if (existing.good() || !source)
        return (FT_FAILURE);
    destination.open(short_source_path.c_str(),
        std::ios::binary | std::ios::out);
    if (!destination)
        return (FT_FAILURE);
    destination << source.rdbuf();
    return (source.bad() || !destination ? FT_FAILURE : FT_SUCCESS);
}

static int cblc_test_compile_command(const char *binary_path,
    const char *source_path, const char *second_source_path, const char *log_path,
    const char *failure_context)
{
    std::string temporary_binary_path;
    std::string short_source_path;
    std::string short_second_source_path;
    std::string command_text;
    std::string temporary_exe_path;
    std::string compiled_output_path;
    std::string final_output_path;
    int copied_source;
    int copied_second_source;

    if (!binary_path || !source_path || !log_path)
        return (FT_FAILURE);
    copied_source = 0;
    copied_second_source = 0;
    temporary_binary_path = cblc_test_short_binary_path(binary_path);
    temporary_exe_path = cblc_test_executable_path(temporary_binary_path);
    short_source_path = cblc_test_short_source_path(source_path, 's');
    if (second_source_path)
        short_second_source_path = cblc_test_short_source_path(
            second_source_path, 'm');
    if (temporary_binary_path.empty() || short_source_path.empty()
        || (second_source_path && short_second_source_path.empty())
        || std::ifstream(temporary_binary_path.c_str(), std::ios::binary).good()
        || std::ifstream(temporary_exe_path.c_str(),
            std::ios::binary).good())
    {
        std::printf("COBOL test compile setup failed (binary=%s exe=%s source=%s)\n",
            temporary_binary_path.c_str(), temporary_exe_path.c_str(),
            short_source_path.c_str());
        return (FT_FAILURE);
    }
    if (cblc_test_copy_source(source_path, short_source_path) != FT_SUCCESS)
    {
        std::printf("COBOL test source staging failed (source=%s staged=%s)\n",
            source_path, short_source_path.c_str());
        return (FT_FAILURE);
    }
    copied_source = 1;
    if (second_source_path && cblc_test_copy_source(second_source_path,
            short_second_source_path) != FT_SUCCESS)
    {
        if (copied_source)
            std::remove(short_source_path.c_str());
        return (FT_FAILURE);
    }
    copied_second_source = second_source_path != NULL;
    command_text = "cobc -x -free -o \"" + temporary_binary_path
        + "\" \"" + short_source_path + "\"";
    if (second_source_path)
        command_text += " \"" + short_second_source_path + "\"";
    command_text += " > \"" + std::string(log_path) + "\" 2>&1";
    if (test_run_command(command_text.c_str()) != FT_SUCCESS)
    {
        std::remove(temporary_binary_path.c_str());
        if (copied_source)
            std::remove(short_source_path.c_str());
        if (copied_second_source)
            std::remove(short_second_source_path.c_str());
        if (failure_context)
            std::printf("Assertion failed: %s\n", failure_context);
        return (FT_FAILURE);
    }
    if (test_expect_compiler_output_allowed(log_path) != FT_SUCCESS)
    {
        std::remove(temporary_binary_path.c_str());
        if (copied_source)
            std::remove(short_source_path.c_str());
        if (copied_second_source)
            std::remove(short_second_source_path.c_str());
        return (FT_FAILURE);
    }
    if (copied_source)
        std::remove(short_source_path.c_str());
    if (copied_second_source)
        std::remove(short_second_source_path.c_str());
    compiled_output_path = std::ifstream(temporary_binary_path.c_str(),
        std::ios::binary).good() ? temporary_binary_path : temporary_exe_path;
    final_output_path = compiled_output_path == temporary_exe_path
        ? cblc_test_executable_path(binary_path) : std::string(binary_path);
    if (std::ifstream(final_output_path.c_str(), std::ios::binary).good()
        || std::rename(compiled_output_path.c_str(),
            final_output_path.c_str()) != 0)
    {
        std::printf("COBOL test executable staging failed (source=%s destination=%s errno=%d)\n",
            compiled_output_path.c_str(), final_output_path.c_str(), errno);
        if (failure_context)
            std::printf("Assertion failed: could not move compiled COBOL executable to its test artifact path\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

int compile_generated_cobol(const char *binary_path, const char *source_path,
    const char *log_path, const char *failure_context)
{
    return (cblc_test_compile_command(binary_path, source_path, NULL, log_path,
        failure_context));
}

int compile_generated_cobol_with_module(const char *binary_path,
    const char *primary_source_path, const char *secondary_source_path,
    const char *log_path, const char *failure_context)
{
    return (cblc_test_compile_command(binary_path, primary_source_path,
        secondary_source_path, log_path, failure_context));
}

int execute_binary(const char *directory, const char *binary_name,
    const char *output_name, const char *failure_context)
{
    char command[512];
    int command_length;

    if (output_name)
        command_length = std::snprintf(command, sizeof(command),
            "cd %s && ./%s > %s", directory, binary_name, output_name);
    else
        command_length = std::snprintf(command, sizeof(command),
            "cd %s && ./%s", directory, binary_name);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        return (FT_FAILURE);
    if (test_run_command(command) != FT_SUCCESS)
    {
        if (failure_context)
            std::printf("Assertion failed: %s\n", failure_context);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}
