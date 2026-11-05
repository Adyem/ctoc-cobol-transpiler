#include "standard_library_test_support.hpp"

#include "../compiler/cobol/round_trip_pipeline_helpers.hpp"

#include <chrono>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <string>
#include <vector>

static int vector_cobol_text_has_line_with_tokens(const char *text,
    const char *first_token, const char *second_token)
{
    const char *line_start;

    if (!text || !first_token || !second_token)
        return (FT_FAILURE);
    line_start = text;
    while (*line_start != '\0')
    {
        const char *line_end;
        const char *first_match;
        const char *second_match;

        line_end = std::strchr(line_start, '\n');
        if (!line_end)
            line_end = line_start + std::strlen(line_start);
        first_match = std::strstr(line_start, first_token);
        second_match = std::strstr(line_start, second_token);
        if (first_match && second_match && first_match < line_end
            && second_match < line_end)
            return (FT_SUCCESS);
        if (*line_end == '\0')
            break ;
        line_start = line_end + 1;
    }
    return (FT_FAILURE);
}

static int vector_cobol_contains(const char *source_path, const char *needle)
{
    std::ifstream source(source_path);
    std::string line;

    if (!source || !needle)
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        if (line.find(needle) != std::string::npos)
            return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static size_t vector_count_output_lines(const char *output, const char *expected_line)
{
    const char *line_start;
    size_t match_count;
    const size_t expected_length = expected_line
        ? std::strlen(expected_line) : 0;

    if (!output || !expected_line || expected_length == 0)
        return (0);
    line_start = output;
    match_count = 0;
    while (*line_start != '\0')
    {
        const char *line_end = std::strchr(line_start, '\n');
        const size_t line_length = line_end
            ? static_cast<size_t>(line_end - line_start)
            : std::strlen(line_start);

        if (line_length == expected_length
            && std::memcmp(line_start, expected_line, expected_length) == 0)
            match_count += 1;
        if (!line_end)
            break ;
        line_start = line_end + 1;
    }
    return (match_count);
}

static int vector_output_marker_has_value(const char *output,
    const char *marker, const char *expected_value)
{
    const char *marker_line;
    const char *value_start;
    const char *value_end;
    const size_t expected_length = expected_value
        ? std::strlen(expected_value) : 0;

    if (!output || !marker || !expected_value || expected_length == 0
        || vector_count_output_lines(output, marker) != 1)
        return (FT_FAILURE);
    marker_line = std::strstr(output, marker);
    if (!marker_line || !(value_start = std::strchr(marker_line, '\n')))
        return (FT_FAILURE);
    value_start += 1;
    value_end = std::strchr(value_start, '\n');
    if (!value_end || static_cast<size_t>(value_end - value_start)
            != expected_length
        || std::memcmp(value_start, expected_value, expected_length) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static void vector_report_first_output_difference(const char *expected,
    const char *actual)
{
    size_t line_number = 1;

    if (!expected || !actual)
        return ;
    while (*expected != '\0' && *actual != '\0' && *expected == *actual)
    {
        if (*expected == '\n')
            line_number += 1;
        expected += 1;
        actual += 1;
    }
    if (*expected == '\0' && *actual == '\0')
        return ;
    std::printf("First output difference at line %zu; expected near: %.100s; actual near: %.100s\n",
        line_number, expected, actual);
}

static int vector_cobol_has_paragraph(const char *source_path, const char *paragraph_name)
{
    std::ifstream source(source_path);
    std::string line;

    if (!source || !paragraph_name || paragraph_name[0] == '\0')
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        std::string::size_type token = 0;
        while (token < line.size() && line[token] == ' ')
            token += 1;
        if (token == 7
            && line.compare(token, std::strlen(paragraph_name), paragraph_name) == 0
            && line.size() > token + std::strlen(paragraph_name)
            && line[token + std::strlen(paragraph_name)] == '.')
            return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int vector_cobol_paragraph_performs_defined_method(
    const char *source_path, const char *paragraph_name)
{
    std::ifstream source(source_path);
    std::string line;
    int in_paragraph;

    if (!source || !paragraph_name || paragraph_name[0] == '\0')
        return (FT_FAILURE);
    in_paragraph = 0;
    while (std::getline(source, line))
    {
        size_t leading_spaces;

        leading_spaces = 0;
        while (leading_spaces < line.size() && line[leading_spaces] == ' ')
            leading_spaces += 1;
        if (!in_paragraph)
        {
            if (leading_spaces == 7
                && line.compare(leading_spaces, std::strlen(paragraph_name),
                    paragraph_name) == 0
                && line.size() == leading_spaces + std::strlen(paragraph_name) + 1
                && line.back() == '.')
                in_paragraph = 1;
            continue ;
        }
        if (leading_spaces == 7 && line.size() > leading_spaces
            && line.back() == '.')
            return (FT_FAILURE);
        {
            const std::string::size_type perform = line.find("PERFORM ");

            if (perform != std::string::npos)
            {
                const std::string::size_type name_start = perform + 8;
                const std::string::size_type name_end = line.find_first_of(
                    " .\t", name_start);

                if (name_end != std::string::npos
                    && line.compare(name_start, 12, "CBLC-METHOD-") == 0)
                {
                    const std::string method_name = line.substr(name_start,
                        name_end - name_start);

                    return (vector_cobol_has_paragraph(source_path,
                            method_name.c_str()));
                }
            }
        }
    }
    return (FT_FAILURE);
}

static int vector_cobol_paragraph_contains_line_token(
    const char *source_path, const char *paragraph_name, const char *token)
{
    std::ifstream source(source_path);
    std::string line;
    int in_paragraph = 0;

    if (!source || !paragraph_name || paragraph_name[0] == '\0'
        || !token || token[0] == '\0')
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        size_t leading_spaces = 0;

        while (leading_spaces < line.size() && line[leading_spaces] == ' ')
            leading_spaces += 1;
        if (!in_paragraph)
        {
            if (leading_spaces == 7
                && line.size() == leading_spaces + std::strlen(paragraph_name) + 1
                && line.compare(leading_spaces, std::strlen(paragraph_name),
                    paragraph_name) == 0
                && line.back() == '.')
                in_paragraph = 1;
            continue ;
        }
        if (leading_spaces == 7 && line.size() > leading_spaces
            && line.back() == '.')
            return (FT_FAILURE);
        if (line.find(token) != std::string::npos)
            return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int vector_compile_and_run_depth_16_cobol(const char *generated_cobol)
{
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char output_buffer[131072];
    std::string expected_output;
    int directory_created = 0;
    int status = FT_FAILURE;

    if (!generated_cobol)
        return (FT_FAILURE);
    expected_output.clear();
    for (size_t level = 1; level <= 16; ++level)
        expected_output += "NESTED SELF ASSIGN LEVEL "
            + std::to_string(level) + " OK\n";
    for (size_t level = 1; level <= 16; ++level)
        expected_output += "NESTED EDGE END INSERT ERASE LEVEL "
            + std::to_string(level) + " OK\nNESTED EDGE RESET LEVEL "
            + std::to_string(level) + " OK\n";
    expected_output += "NESTED EDGE REUSE OK\n";
    for (size_t level = 1; level <= 16; ++level)
    {
        expected_output += "EMPTY AT NEGATIVE LEVEL " + std::to_string(level)
            + "\nEMPTY AT END LEVEL " + std::to_string(level)
            + "\nEMPTY ERASE LEVEL " + std::to_string(level)
            + "\nEMPTY POP BACK LEVEL " + std::to_string(level)
            + "\nEMPTY RESIZE NEGATIVE LEVEL " + std::to_string(level)
            + "\nEMPTY BOUNDS STATE LEVEL " + std::to_string(level)
            + "\n+000000000\n";
    }
    expected_output += "EMPTY NESTED BOUNDS\n+000000001\n"
        "LEAF CONSTRUCTED\nLEAF PARSED\n"
        "STRING COPY CHECK\n+000000001\n";
    for (size_t level = 1; level <= 16; ++level)
        expected_output += "NESTED LEVEL " + std::to_string(level)
            + " INSERTED\n";
    expected_output.insert(expected_output.find("NESTED LEVEL 1 INSERTED"),
        "NESTED LEVEL 1 REALLOCATED\n");
    expected_output += "NESTED VALUE COPIED\nBORROWED LEAF RESOLVED\n"
        "NESTED VALUE MUTATED\nNESTED VECTOR DEPTH 16 OK\n"
        "DEPTH 16 CLEAR PRESERVES SOURCE AND CAPACITY\n"
        "DEPTH 16 REPEATED CLEAR\n"
        "DEPTH 16 REUSE AFTER CLEAR\n"
        "DEPTH 16 POP BACK AFTER REUSE\n";
    expected_output.insert(expected_output.find("NESTED VALUE COPIED"),
        "BEFORE NESTED RETURN COPY\nAFTER NESTED RETURN COPY\n");
    {
        std::string chain_checkpoints;

        for (size_t step = 1; step <= 16; ++step)
            chain_checkpoints += "CHAIN STEP " + std::to_string(step) + "\n";
        expected_output.insert(expected_output.find("NESTED VALUE COPIED"),
            chain_checkpoints);
    }
    {
        std::string populated_boundary_checkpoints;
        static const char *const boundary_cases[] = {
            "EMPTY AT NEGATIVE LEVEL", "EMPTY AT END LEVEL",
            "EMPTY ERASE LEVEL", "EMPTY RESIZE NEGATIVE LEVEL",
            "EMPTY POP BACK LEVEL",
            "POPULATED AT NEGATIVE LEVEL", "POPULATED AT END LEVEL",
            "POPULATED INSERT NEGATIVE LEVEL", "POPULATED INSERT END LEVEL",
            "POPULATED ERASE NEGATIVE LEVEL", "POPULATED ERASE END LEVEL",
            "POPULATED RESIZE NEGATIVE LEVEL",
            "POPULATED ASSIGN NEGATIVE LEVEL"
        };

        populated_boundary_checkpoints = "BEFORE DIRECT CHAIN\n";
        for (size_t level = 1; level <= 16; ++level)
        {
            for (size_t case_index = 5;
                case_index < sizeof(boundary_cases) / sizeof(boundary_cases[0]);
                ++case_index)
                populated_boundary_checkpoints += boundary_cases[case_index]
                    + std::string(" ") + std::to_string(level) + "\n";
            populated_boundary_checkpoints += "POPULATED BOUNDS STATE LEVEL "
                + std::to_string(level) + "\n"
                + ((level == 1 || level == 16)
                    ? "+000000002\n" : "+000000001\n");
        }
        populated_boundary_checkpoints += "POPULATED NESTED BOUNDS\n+000000001\n"
            "AFTER DIRECT CHAIN\n"
            "BEFORE SUFFIX CONSTRUCT\nAFTER SUFFIX CONSTRUCT\n";
        {
            std::string reserve_checkpoints;

            for (size_t level = 1; level <= 16; ++level)
                reserve_checkpoints += "NESTED NEGATIVE RESERVE LEVEL "
                    + std::to_string(level) + "\nNESTED RESERVE LEVEL "
                    + std::to_string(level) + " OK\n";
            reserve_checkpoints += "NESTED RESERVE LIFECYCLE OK\n";
            populated_boundary_checkpoints.insert(
                populated_boundary_checkpoints.find("AFTER DIRECT CHAIN"),
                reserve_checkpoints);
        }
        expected_output.insert(expected_output.find("NESTED VALUE COPIED"),
            populated_boundary_checkpoints);
    }
    expected_output.insert(expected_output.find("CHAIN STEP 1"),
        "NESTED COPY ASSIGNMENT INDEPENDENCE\n+000000001\n");
    {
        std::string size_checkpoints;

        for (size_t level = 1; level <= 16; ++level)
            size_checkpoints += "SIZE CHECK LEVEL " + std::to_string(level)
                + "\n";
        expected_output.insert(expected_output.find("NESTED VALUE COPIED"),
            size_checkpoints);
    }
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "nested_vector_depth_16.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "nested_vector_depth_16.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, "nested_vector_depth_16.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS
        || test_join_path(directory, "nested_vector_depth_16_compile.log",
            compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS
        || test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile the depth-16 nested vector program") != FT_SUCCESS)
    {
        if (test_read_text_file(compile_log_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
            std::printf("Depth-16 COBOL compiler diagnostics:\n%s\n", output_buffer);
        goto cleanup;
    }
    if (execute_binary(directory, "nested_vector_depth_16.bin",
            "nested_vector_depth_16.txt",
            "depth-16 nested vector COBOL should execute") != FT_SUCCESS
        || test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS
        || std::strcmp(output_buffer, expected_output.c_str()) != 0)
    {
        std::printf("Depth-16 nested vector execution did not produce its success marker\n");
        if (test_read_text_file(output_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
        {
            vector_report_first_output_difference(expected_output.c_str(),
                output_buffer);
            std::printf("Depth-16 COBOL runtime output:\n%s\n", output_buffer);
        }
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        const char *preserve_failed_artifacts =
            std::getenv("CTOC_KEEP_FAILED_TEST_ARTIFACTS");

        if (status == FT_SUCCESS || !preserve_failed_artifacts
            || preserve_failed_artifacts[0] == '\0'
            || std::strcmp(preserve_failed_artifacts, "0") == 0)
        {
            test_cleanup_example_artifacts_with_log(source_path, binary_path,
                output_path, compile_log_path);
            test_remove_directory(directory);
        }
        else
            std::printf("Depth-16 artifacts retained at %s\n", directory);
    }
    return (status);
}

static int vector_compile_and_run_nested_depth_cobol(
    const char *generated_cobol, size_t depth)
{
    char directory[256];
    char source_name[80];
    char binary_name[80];
    char output_name[80];
    char compile_name[80];
    char source_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char output_buffer[131072];
    char success_marker[80];
    int directory_created;
    int status;

    if (!generated_cobol || depth == 0
        || std::snprintf(source_name, sizeof(source_name),
            "nested_vector_depth_%zu.cob", depth) < 0
        || std::snprintf(binary_name, sizeof(binary_name),
            "nested_vector_depth_%zu.bin", depth) < 0
        || std::snprintf(output_name, sizeof(output_name),
            "nested_vector_depth_%zu.txt", depth) < 0
        || std::snprintf(compile_name, sizeof(compile_name),
            "nested_vector_depth_%zu_compile.log", depth) < 0
        || std::snprintf(success_marker, sizeof(success_marker),
            "NESTED VECTOR DEPTH %zu OK", depth) < 0)
        return (FT_FAILURE);
    if (!test_cobc_available())
        return (test_require_cobc_dependency(__func__));
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    directory_created = 0;
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, source_name, source_path,
            sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, binary_name, binary_path,
            sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, output_name, output_path,
            sizeof(output_path)) != FT_SUCCESS
        || test_join_path(directory, compile_name, compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS
        || test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile a nested vector specialization") != FT_SUCCESS)
    {
        if (test_read_text_file(compile_log_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
            std::printf("Depth-%zu COBOL compiler diagnostics:\n%s\n",
                depth, output_buffer);
        goto cleanup;
    }
    if (execute_binary(directory, binary_name, output_name,
            "nested vector COBOL specialization should execute") != FT_SUCCESS
        || test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS
        || !std::strstr(output_buffer, success_marker)
        || !std::strstr(output_buffer,
            "EMPTY NESTED BOUNDS\n+000000001\n")
        || !std::strstr(output_buffer,
            "POPULATED NESTED BOUNDS\n+000000001\n")
        || !std::strstr(output_buffer,
            "NESTED COPY ASSIGNMENT INDEPENDENCE\n+000000001\n")
        || !std::strstr(output_buffer, "NESTED RESERVE LIFECYCLE OK")
        || std::strstr(output_buffer, "NESTED RESERVE LIFECYCLE FAILED")
        || (depth != 16
            && !std::strstr(output_buffer, "NESTED ERASE LIFECYCLE OK"))
        || std::strstr(output_buffer, "NESTED ERASE LIFECYCLE FAILED")
        || std::strstr(output_buffer, "EMPTY NESTED BOUNDS\n+000000000\n")
        || std::strstr(output_buffer, "POPULATED NESTED BOUNDS\n+000000000\n")
        || std::strstr(output_buffer, "Fatal CBL-C double exception")
        || std::strstr(output_buffer, "Unhandled CBL-C exception")
        || std::strstr(output_buffer, "NESTED EDGE REUSE FAILED"))
    {
        std::printf("Depth-%zu COBOL runtime did not complete its nested lifecycle checks\n",
            depth);
        if (test_read_text_file(output_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
            std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                depth, output_buffer);
        goto cleanup;
    }
    for (size_t level = 1; level <= depth; ++level)
    {
        static const char *const boundary_cases[] = {
            "EMPTY AT NEGATIVE LEVEL", "EMPTY AT END LEVEL",
            "EMPTY ERASE LEVEL", "EMPTY RESIZE NEGATIVE LEVEL",
            "EMPTY POP BACK LEVEL",
            "POPULATED AT NEGATIVE LEVEL", "POPULATED AT END LEVEL",
            "POPULATED INSERT NEGATIVE LEVEL", "POPULATED INSERT END LEVEL",
            "POPULATED ERASE NEGATIVE LEVEL", "POPULATED ERASE END LEVEL",
            "POPULATED RESIZE NEGATIVE LEVEL"
        };

        for (size_t case_index = 0;
            case_index < sizeof(boundary_cases) / sizeof(boundary_cases[0]);
            ++case_index)
        {
            char expected_line[96];

            if (std::snprintf(expected_line, sizeof(expected_line), "%s %zu",
                    boundary_cases[case_index], level) < 0
                || vector_count_output_lines(output_buffer, expected_line) != 1)
            {
                std::printf("Depth-%zu COBOL runtime did not report boundary case '%s %zu' exactly once\n",
                    depth, boundary_cases[case_index], level);
                std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                    depth, output_buffer);
                goto cleanup;
            }
        }
        {
            char empty_state_marker[96];
            char populated_state_marker[96];
            char expected_size[32];
            const size_t populated_size = level == depth ? 2
                : (depth == 16 && level == 1 ? 2 : 1);

            if (std::snprintf(empty_state_marker,
                    sizeof(empty_state_marker),
                    "EMPTY BOUNDS STATE LEVEL %zu", level) < 0
                || std::snprintf(populated_state_marker,
                    sizeof(populated_state_marker),
                    "POPULATED BOUNDS STATE LEVEL %zu", level) < 0
                || vector_output_marker_has_value(output_buffer,
                    empty_state_marker, "+000000000") != FT_SUCCESS
                || std::snprintf(expected_size, sizeof(expected_size),
                    "+%09zu", populated_size) < 0
                || vector_output_marker_has_value(output_buffer,
                    populated_state_marker, expected_size) != FT_SUCCESS)
            {
                std::printf("Depth-%zu COBOL runtime did not preserve the exact empty/populated size after all invalid operations at nested level %zu\n",
                    depth, level);
                std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                    depth, output_buffer);
                goto cleanup;
            }
        }
        {
            char reserve_success[96];
            char reserve_failure[96];
            char reserve_exception[96];

            if (std::snprintf(reserve_success, sizeof(reserve_success),
                    "NESTED RESERVE LEVEL %zu OK", level) < 0
                || std::snprintf(reserve_failure, sizeof(reserve_failure),
                    "NESTED RESERVE LEVEL %zu FAILED", level) < 0
                || std::snprintf(reserve_exception, sizeof(reserve_exception),
                    "NESTED NEGATIVE RESERVE LEVEL %zu", level) < 0
                || vector_count_output_lines(output_buffer, reserve_success) != 1
                || vector_count_output_lines(output_buffer, reserve_failure) != 0
                || vector_count_output_lines(output_buffer, reserve_exception) != 1)
            {
                std::printf("Depth-%zu COBOL runtime did not prove reallocation and failed-reserve state preservation at nested level %zu\n",
                    depth, level);
                std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                    depth, output_buffer);
                goto cleanup;
            }
        }
    }
    for (size_t level = 1; level <= depth; ++level)
    {
        char edge_reset[96];
        char edge_failure[96];

        if (std::snprintf(edge_reset, sizeof(edge_reset),
                "NESTED EDGE RESET LEVEL %zu OK", level) < 0
            || std::snprintf(edge_failure, sizeof(edge_failure),
                "NESTED EDGE RESET LEVEL %zu FAILED", level) < 0
            || vector_count_output_lines(output_buffer, edge_reset) != 1
            || vector_count_output_lines(output_buffer, edge_failure) != 0)
        {
            std::printf("Depth-%zu COBOL runtime did not verify valid self-aliased insert, erase, clear, and empty state at nested level %zu\n",
                depth, level);
            std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                depth, output_buffer);
            goto cleanup;
        }
    }
    for (size_t level = 1; level <= depth; ++level)
    {
        char end_insert_erase[112];
        char end_insert_erase_failure[112];

        if (std::snprintf(end_insert_erase, sizeof(end_insert_erase),
                "NESTED EDGE END INSERT ERASE LEVEL %zu OK", level) < 0
            || std::snprintf(end_insert_erase_failure,
                sizeof(end_insert_erase_failure),
                "NESTED EDGE END INSERT ERASE LEVEL %zu FAILED", level) < 0
            || vector_count_output_lines(output_buffer, end_insert_erase) != 1
            || vector_count_output_lines(output_buffer,
                end_insert_erase_failure) != 0)
        {
            std::printf("Depth-%zu COBOL runtime did not verify valid end insertion and last-element erase at nested level %zu\n",
                depth, level);
            std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                depth, output_buffer);
            goto cleanup;
        }
    }
    if (vector_count_output_lines(output_buffer,
            "NESTED EDGE REUSE OK") != 1
        || vector_count_output_lines(output_buffer,
            "NESTED EDGE REUSE FAILED") != 0)
    {
        std::printf("Depth-%zu COBOL runtime did not verify nested reuse after clear\n",
            depth);
        std::printf("Depth-%zu COBOL runtime output:\n%s\n",
            depth, output_buffer);
        goto cleanup;
    }
    for (size_t level = 1; level <= depth; ++level)
    {
        char self_assignment[112];
        char self_assignment_failure[112];

        if (std::snprintf(self_assignment, sizeof(self_assignment),
                "NESTED SELF ASSIGN LEVEL %zu OK", level) < 0
            || std::snprintf(self_assignment_failure,
                sizeof(self_assignment_failure),
                "NESTED SELF ASSIGN LEVEL %zu FAILED", level) < 0
            || vector_count_output_lines(output_buffer, self_assignment) != 1
            || vector_count_output_lines(output_buffer,
                self_assignment_failure) != 0)
        {
            std::printf("Depth-%zu COBOL runtime did not prove repeated self-assignment preserves nested value state at level %zu\n",
                depth, level);
            std::printf("Depth-%zu COBOL runtime output:\n%s\n",
                depth, output_buffer);
            goto cleanup;
        }
    }
    {
        char failure_marker[80];

        std::snprintf(failure_marker, sizeof(failure_marker),
            "NESTED VECTOR DEPTH %zu FAILED", depth);
        if (vector_count_output_lines(output_buffer, success_marker) != 1
            || vector_count_output_lines(output_buffer, failure_marker) != 0)
        {
            std::printf("Depth-%zu COBOL runtime must emit exactly one success marker and no failure marker\n",
                depth);
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        const char *preserve_failed_artifacts =
            std::getenv("CTOC_KEEP_FAILED_TEST_ARTIFACTS");

        if (status == FT_SUCCESS || !preserve_failed_artifacts
            || preserve_failed_artifacts[0] == '\0'
            || std::strcmp(preserve_failed_artifacts, "0") == 0)
        {
            test_cleanup_example_artifacts_with_log(source_path, binary_path,
                output_path, compile_log_path);
            test_remove_directory(directory);
        }
        else
            std::printf("Depth-%zu artifacts retained at %s\n", depth, directory);
    }
    return (status);
}

static int vector_make_transpile_command(char *command, size_t command_size,
    const char *source_path, const char *cobol_path)
{
    const char *compiler_path;

    if (!command || command_size == 0 || !source_path || !cobol_path)
        return (-1);
    compiler_path = std::getenv("CTOC_COMPILER_PATH");
    if (!compiler_path || compiler_path[0] == '\0')
        compiler_path = "./ctoc_cobol_transpiler";
    return (std::snprintf(command, command_size,
        "%s --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\"",
        compiler_path, source_path, cobol_path));
}

static int vector_cobol_contains_normalized(const char *source_path, const char *needle)
{
    std::ifstream source(source_path);
    std::string text;
    std::string normalized_text;
    std::string normalized_needle;
    char character;
    int pending_space;

    if (!source || !needle)
        return (FT_FAILURE);
    while (source.get(character))
        text += character;
    pending_space = 0;
    for (std::string::size_type index = 0; index < text.size(); ++index)
    {
        if (std::isspace(static_cast<unsigned char>(text[index])))
        {
            pending_space = normalized_text.empty() ? 0 : 1;
            continue ;
        }
        if (pending_space)
            normalized_text += ' ';
        normalized_text += text[index];
        pending_space = 0;
    }
    pending_space = 0;
    for (std::string::size_type index = 0; needle[index] != '\0'; ++index)
    {
        if (std::isspace(static_cast<unsigned char>(needle[index])))
        {
            pending_space = normalized_needle.empty() ? 0 : 1;
            continue ;
        }
        if (pending_space)
            normalized_needle += ' ';
        normalized_needle += needle[index];
        pending_space = 0;
    }
    return (normalized_text.find(normalized_needle) != std::string::npos
        ? FT_SUCCESS : FT_FAILURE);
}

static int vector_cobol_excludes(const char *source_path, const char *needle)
{
    return (vector_cobol_contains(source_path, needle) == FT_SUCCESS
        ? FT_FAILURE : FT_SUCCESS);
}

static int vector_cobol_has_move_from_parameter_to(const char *source_path,
    const char *destination)
{
    std::ifstream source(source_path);
    std::string line;

    if (!source || !destination)
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        if (line.find("MOVE CBLC-P-") != std::string::npos
            && line.find(destination) != std::string::npos)
            return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int vector_cobol_has_parameter_member_element_address(const char *source_path)
{
    std::ifstream source(source_path);
    std::string text;
    std::string normalized_text;
    char character;
    int pending_space;
    int pointer_derived_from_parameter;
    int based_view_uses_parameter_pointer;

    if (!source)
        return (FT_FAILURE);
    while (source.get(character))
        text += character;
    pending_space = 0;
    for (std::string::size_type index = 0; index < text.size(); ++index)
    {
        if (std::isspace(static_cast<unsigned char>(text[index])))
        {
            pending_space = normalized_text.empty() ? 0 : 1;
            continue ;
        }
        if (pending_space)
            normalized_text += ' ';
        normalized_text += text[index];
        pending_space = 0;
    }
    pointer_derived_from_parameter = 0;
    based_view_uses_parameter_pointer = 0;
    {
        std::string::size_type address_start;
        std::string::size_type parameter_address;
        std::string::size_type based_name_end;
        std::string::size_type based_member_use;

        address_start = normalized_text.find("SET CBLC-PTR-CURSOR TO CBLC-P-");
        parameter_address = address_start == std::string::npos
            ? std::string::npos
            : normalized_text.find("-ELEMENTS", address_start);
        if (parameter_address != std::string::npos)
            pointer_derived_from_parameter = 1;

        address_start = normalized_text.find("SET ADDRESS OF CBLC-SPTR-P");
        parameter_address = address_start == std::string::npos
            ? std::string::npos
            : normalized_text.find("TO CBLC-P-", address_start);
        based_name_end = address_start == std::string::npos
            ? std::string::npos
            : normalized_text.find(" TO ", address_start);
        based_member_use = address_start == std::string::npos
                || based_name_end == std::string::npos
            ? std::string::npos
            : normalized_text.find(normalized_text.substr(address_start +
                    std::strlen("SET ADDRESS OF "),
                    based_name_end - (address_start +
                        std::strlen("SET ADDRESS OF "))) + "-",
                parameter_address);
        if (parameter_address != std::string::npos
            && based_member_use != std::string::npos)
            based_view_uses_parameter_pointer = 1;
    }
    return (pointer_derived_from_parameter && based_view_uses_parameter_pointer
        ? FT_SUCCESS : FT_FAILURE);
}

static int vector_cobol_has_unique_level01_names(const char *source_path)
{
    std::ifstream source(source_path);
    std::vector<std::string> names;
    std::vector<std::string> declarations;
    std::string line;

    if (!source)
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        size_t name_start;
        size_t name_end;
        std::string name;

        if (line.compare(0, 10, "       01 ") != 0)
            continue ;
        name_start = 10;
        name_end = line.find_first_of(" .", name_start);
        if (name_end == std::string::npos || name_end == name_start)
            return (FT_FAILURE);
        name = line.substr(name_start, name_end - name_start);
        for (size_t name_index = 0; name_index < names.size(); ++name_index)
            if (names[name_index] == name)
            {
                std::printf("Duplicate level-01 COBOL data name: %s\n",
                    name.c_str());
                std::printf("  first: %s\n  second: %s\n",
                    declarations[name_index].c_str(), line.c_str());
                return (FT_FAILURE);
            }
        names.push_back(name);
        declarations.push_back(line);
    }
    return (FT_SUCCESS);
}

static int vector_cobol_method_paragraph_reaches_line_tokens(
    const char *source_path, const std::string &paragraph_name,
    const char *first_line_token, const char *second_line_token,
    std::vector<std::string> &visited, size_t depth)
{
    std::ifstream source(source_path);
    std::string line;
    std::vector<std::string> callees;
    int in_paragraph = 0;

    if (!source || depth > 16)
        return (FT_FAILURE);
    for (size_t index = 0; index < visited.size(); ++index)
        if (visited[index] == paragraph_name)
            return (FT_FAILURE);
    visited.push_back(paragraph_name);
    while (std::getline(source, line))
    {
        size_t leading_spaces = 0;

        while (leading_spaces < line.size() && line[leading_spaces] == ' ')
            leading_spaces += 1;
        if (!in_paragraph)
        {
            if (leading_spaces == 7
                && line.size() == leading_spaces + paragraph_name.size() + 1
                && line.compare(leading_spaces, paragraph_name.size(),
                    paragraph_name) == 0
                && line.back() == '.')
                in_paragraph = 1;
            continue ;
        }
        if (leading_spaces == 7 && !line.empty() && line.back() == '.')
            break ;
        if (line.find(first_line_token) != std::string::npos
            && line.find(second_line_token) != std::string::npos)
            return (FT_SUCCESS);
        {
            const std::string::size_type perform = line.find("PERFORM ");

            if (perform != std::string::npos)
            {
                const std::string::size_type name_start = perform + 8;
                const std::string::size_type name_end = line.find_first_of(
                    " .\t", name_start);

                if (name_end != std::string::npos
                    && line.compare(name_start, 12, "CBLC-METHOD-") == 0)
                    callees.push_back(line.substr(name_start,
                        name_end - name_start));
            }
        }
    }
    for (size_t index = 0; index < callees.size(); ++index)
        if (vector_cobol_method_paragraph_reaches_line_tokens(source_path,
                callees[index], first_line_token, second_line_token,
                visited, depth + 1) == FT_SUCCESS)
            return (FT_SUCCESS);
    return (FT_FAILURE);
}

static int vector_cobol_main_performs_method_writing_line_tokens(
    const char *source_path, const char *method_prefix,
    const char *first_line_token, const char *second_line_token)
{
    std::ifstream source(source_path);
    std::string line;
    std::vector<std::string> method_paragraphs;
    int in_main = 0;

    if (!source || !method_prefix || !first_line_token || !second_line_token)
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        if (line.find("       MAIN.") != std::string::npos)
        {
            in_main = 1;
            continue ;
        }
        if (in_main && line.find("           STOP RUN.") != std::string::npos)
            break ;
        if (!in_main)
            continue ;
        {
            const std::string::size_type perform = line.find("PERFORM ");

            if (perform == std::string::npos)
                continue ;
            const std::string::size_type name_start = perform + 8;
            const std::string::size_type name_end = line.find_first_of(
                " .\t", name_start);

            if (name_end != std::string::npos
                && line.compare(name_start, std::strlen(method_prefix),
                    method_prefix) == 0)
                method_paragraphs.push_back(line.substr(name_start,
                    name_end - name_start));
        }
    }
    for (size_t index = 0; index < method_paragraphs.size(); ++index)
    {
        std::vector<std::string> visited;

        if (vector_cobol_method_paragraph_reaches_line_tokens(source_path,
                method_paragraphs[index], first_line_token, second_line_token,
                visited, 0) == FT_SUCCESS)
            return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int vector_cobol_main_contains_in_order(const char *source_path,
    const char *first, const char *second, const char *third)
{
    std::ifstream source(source_path);
    std::string main_contents;
    std::string line;
    size_t first_position;
    size_t second_position;
    size_t third_position;
    size_t second_search_start;
    size_t third_search_start;
    int in_main;

    if (!source || !first || !second || !third)
        return (FT_FAILURE);
    in_main = 0;
    while (std::getline(source, line))
    {
        if (line.find("       MAIN.") != std::string::npos)
            in_main = 1;
        else if (in_main && line.find("           STOP RUN.") != std::string::npos)
            break ;
        else if (in_main)
        {
            main_contents += line;
            main_contents += '\n';
        }
    }
    first_position = main_contents.find(first);
    second_search_start = first_position == std::string::npos
        ? std::string::npos : first_position + std::strlen(first);
    second_position = second_search_start == std::string::npos
        ? std::string::npos : main_contents.find(second, second_search_start);
    third_search_start = second_position == std::string::npos
        ? std::string::npos : second_position + std::strlen(second);
    third_position = third_search_start == std::string::npos
        ? std::string::npos : main_contents.find(third, third_search_start);
    return (first_position != std::string::npos
        && second_position != std::string::npos
        && third_position != std::string::npos
        && first_position < second_position && second_position < third_position
        ? FT_SUCCESS : FT_FAILURE);
}

static const char *g_vector_runtime_source =
    "int tracked_constructions;\n"
    "int tracked_destructions;\n"
    "int value;\n"
    "int rollback_construct_attempts;\n"
    "int rollback_fail_at;\n"
    "int rollback_successful_constructions;\n"
    "int rollback_destructions;\n"
    "class Tracked\n"
    "{\n"
    "    public:\n"
    "    int payload;\n"
    "    Tracked()\n"
    "    {\n"
    "        tracked_constructions = tracked_constructions + 1;\n"
    "        payload = 1;\n"
    "    }\n"
    "    Tracked(Tracked *source)\n"
    "    {\n"
    "        tracked_constructions = tracked_constructions + 1;\n"
    "        payload = source->payload;\n"
    "    }\n"
    "    Tracked(Tracked source)\n"
    "    {\n"
    "        tracked_constructions = tracked_constructions + 1;\n"
    "        payload = source.payload;\n"
    "    }\n"
    "    ~Tracked()\n"
    "    {\n"
    "        tracked_destructions = tracked_destructions + 1;\n"
    "    }\n"
    "};\n"
    "class ResizeRollbackProbe\n"
    "{\n"
    "    public:\n"
    "    int payload;\n"
    "    ResizeRollbackProbe()\n"
    "    {\n"
    "        rollback_construct_attempts = rollback_construct_attempts + 1;\n"
    "        if (rollback_construct_attempts == rollback_fail_at) { throw 71; }\n"
    "        rollback_successful_constructions = rollback_successful_constructions + 1;\n"
    "        payload = 41;\n"
    "    }\n"
    "    ResizeRollbackProbe(ResizeRollbackProbe source)\n"
    "    {\n"
    "        rollback_successful_constructions = rollback_successful_constructions + 1;\n"
    "        payload = source.payload;\n"
    "    }\n"
    "    ~ResizeRollbackProbe()\n"
    "    {\n"
    "        rollback_destructions = rollback_destructions + 1;\n"
    "    }\n"
    "};\n"
    "int lifetime_probe_constructions;\n"
    "int lifetime_probe_destructions;\n"
    "class LifetimeProbe\n"
    "{\n"
    "    public:\n"
    "    int payload;\n"
    "    LifetimeProbe()\n"
    "    {\n"
    "        lifetime_probe_constructions = lifetime_probe_constructions + 1;\n"
    "        payload = 1;\n"
    "    }\n"
    "    LifetimeProbe(LifetimeProbe source)\n"
    "    {\n"
    "        lifetime_probe_constructions = lifetime_probe_constructions + 1;\n"
    "        payload = source.payload;\n"
    "    }\n"
    "    LifetimeProbe(LifetimeProbe *source)\n"
    "    {\n"
    "        lifetime_probe_constructions = lifetime_probe_constructions + 1;\n"
    "        payload = source->payload;\n"
    "        source->payload = 0;\n"
    "    }\n"
    "    ~LifetimeProbe()\n"
    "    {\n"
    "        lifetime_probe_destructions = lifetime_probe_destructions + 1;\n"
    "    }\n"
    "};\n"
    "\n"
    "void exercise_tracked_vector()\n"
    "{\n"
    "    tracked_constructions = 0;\n"
    "    tracked_destructions = 0;\n"
    "    {\n"
    "        vector<Tracked> tracked_values;\n"
    "        Tracked seed;\n"
    "        tracked_values.push_back(seed);\n"
    "        tracked_values.push_back(seed);\n"
    "        tracked_values.push_back(seed);\n"
    "        tracked_values.erase(1);\n"
    "        tracked_values.clear();\n"
    "    }\n"
    "    if (tracked_constructions > 0 && tracked_constructions == tracked_destructions)\n"
    "    {\n"
    "        display(\"TRACKED LIFETIME BALANCED\");\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        display(\"TRACKED LIFETIME LEAK OR DOUBLE DESTROY\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_reserved_lifecycle()\n"
    "{\n"
    "    lifetime_probe_constructions = 0;\n"
    "    lifetime_probe_destructions = 0;\n"
    "    {\n"
    "        vector<LifetimeProbe> probes;\n"
    "        probes.reserve(4);\n"
    "        if (lifetime_probe_constructions == 0 && lifetime_probe_destructions == 0)\n"
    "        {\n"
    "            display(\"RESERVE DOES NOT CONSTRUCT\");\n"
    "        }\n"
    "        probes.emplace_back();\n"
    "        probes.emplace_back();\n"
    "        if (lifetime_probe_constructions == 2 && lifetime_probe_destructions == 0)\n"
    "        {\n"
    "            display(\"TWO LIVE ELEMENTS\");\n"
    "        }\n"
    "        probes.pop_back();\n"
    "        if (lifetime_probe_constructions == 2 && lifetime_probe_destructions == 1)\n"
    "        {\n"
    "            display(\"POP DESTROYS ONE\");\n"
    "        }\n"
    "        probes.clear();\n"
    "        if (lifetime_probe_constructions == 2 && lifetime_probe_destructions == 2)\n"
    "        {\n"
    "            display(\"CLEAR DESTROYS REMAINDER\");\n"
    "        }\n"
    "    }\n"
    "    if (lifetime_probe_constructions == 2 && lifetime_probe_destructions == 2)\n"
    "    {\n"
    "        display(\"SCOPE DOES NOT DOUBLE DESTROY\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_assign_zero_lifecycle()\n"
    "{\n"
    "    lifetime_probe_constructions = 0;\n"
    "    lifetime_probe_destructions = 0;\n"
    "    {\n"
    "        LifetimeProbe fill;\n"
    "        vector<LifetimeProbe> probes;\n"
    "        probes.reserve(4);\n"
    "        probes.emplace_back();\n"
    "        probes.emplace_back();\n"
    "        probes.assign(0, fill);\n"
    "        if (probes.size() == 0 && lifetime_probe_constructions == 4\n"
    "            && lifetime_probe_destructions == 3)\n"
    "        {\n"
    "            display(\"ZERO ASSIGN DESTROYS LIVE ELEMENTS\");\n"
    "        }\n"
    "        if (probes.capacity() == 4 && probes.size() == 0)\n"
    "        {\n"
    "            display(\"ZERO ASSIGN RETAINS CAPACITY\");\n"
    "        }\n"
    "    }\n"
    "    if (lifetime_probe_constructions == 4\n"
    "        && lifetime_probe_destructions == 4)\n"
    "    {\n"
    "        display(\"ZERO ASSIGN LIFETIME BALANCED\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_last_erase_lifecycle()\n"
    "{\n"
    "    lifetime_probe_constructions = 0;\n"
    "    lifetime_probe_destructions = 0;\n"
    "    {\n"
    "        vector<LifetimeProbe> probes;\n"
    "        probes.reserve(3);\n"
    "        probes.emplace_back();\n"
    "        probes.emplace_back();\n"
    "        probes.erase(1);\n"
    "        if (probes.size() == 1 && probes.capacity() == 3\n"
    "            && lifetime_probe_constructions == 2\n"
    "            && lifetime_probe_destructions == 1)\n"
    "        {\n"
    "            display(\"ERASE LAST DOES NOT RELOCATE\");\n"
    "        }\n"
    "    }\n"
    "    if (lifetime_probe_constructions == 2\n"
    "        && lifetime_probe_destructions == 2)\n"
    "    {\n"
    "        display(\"ERASE LAST LIFETIME BALANCED\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_insert_at_end_uses_spare_capacity()\n"
    "{\n"
    "    int construction_baseline;\n"
    "    int destruction_baseline;\n"
    "    lifetime_probe_constructions = 0;\n"
    "    lifetime_probe_destructions = 0;\n"
    "    {\n"
    "        vector<LifetimeProbe> probes;\n"
    "        LifetimeProbe fill;\n"
    "        probes.reserve(3);\n"
    "        probes.emplace_back();\n"
    "        construction_baseline = lifetime_probe_constructions;\n"
    "        destruction_baseline = lifetime_probe_destructions;\n"
    "        probes.insert(probes.size(), fill);\n"
    "        display(\"END INSERT LIFETIME COUNTS\");\n"
    "        display(lifetime_probe_constructions);\n"
    "        display(lifetime_probe_destructions);\n"
    "        if (probes.size() == 2 && probes.capacity() == 3\n"
    "            && lifetime_probe_constructions == construction_baseline + 2\n"
    "            && lifetime_probe_destructions == destruction_baseline + 1)\n"
    "        {\n"
    "            display(\"END INSERT USES SPARE CAPACITY\");\n"
    "        }\n"
    "    }\n"
    "    if (lifetime_probe_constructions == lifetime_probe_destructions)\n"
    "    {\n"
    "        display(\"END INSERT LIFETIME BALANCED\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_count_construction()\n"
    "{\n"
    "    tracked_constructions = 0;\n"
    "    tracked_destructions = 0;\n"
    "    {\n"
    "        vector<Tracked> defaults(3);\n"
    "        if (tracked_constructions == 3 && tracked_destructions == 0)\n"
    "        {\n"
    "            display(\"COUNT CONSTRUCTS LIVE ELEMENTS\");\n"
    "        }\n"
    "    }\n"
    "    if (tracked_constructions == 3 && tracked_destructions == 3)\n"
    "    {\n"
    "        display(\"COUNT CONSTRUCTION CLEANUP\");\n"
    "    }\n"
    "    tracked_constructions = 0;\n"
    "    tracked_destructions = 0;\n"
    "    {\n"
    "        Tracked seed;\n"
    "        vector<Tracked> copies(2, seed);\n"
        "        display(\"COUNT VALUE PARAMETER CLEANED\");\n"
        "        display(tracked_destructions);\n"
        "        if (tracked_constructions == 4 && tracked_destructions == 1)\n"
    "        {\n"
    "            display(\"COUNT VALUE CONSTRUCTION\");\n"
    "        }\n"
    "    }\n"
    "    display(\"COUNT VALUE CLEANUP COUNTS\");\n"
    "    display(tracked_constructions);\n"
    "    display(tracked_destructions);\n"
    "    if (tracked_constructions == 4 && tracked_destructions == 4)\n"
    "    {\n"
    "        display(\"COUNT VALUE CLEANUP\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_value_resize_lifecycle()\n"
    "{\n"
    "    tracked_constructions = 0;\n"
    "    tracked_destructions = 0;\n"
    "    {\n"
    "        Tracked fill;\n"
    "        vector<Tracked> values;\n"
    "        values.reserve(4);\n"
    "        values.emplace_back();\n"
    "        values.emplace_back();\n"
    "        values.emplace_back();\n"
    "        values.resize(1, fill);\n"
    "        if (values.size() == 1)\n"
    "        {\n"
    "            display(\"VALUE RESIZE DESTROYS TAIL\");\n"
    "        }\n"
    "        while (values.size() > 0 && values.capacity() > 0)\n"
    "        {\n"
    "            values.pop_back();\n"
    "        }\n"
    "        if (values.empty())\n"
    "        {\n"
    "            display(\"VALUE RESIZE WHILE RECHECKS\");\n"
    "        }\n"
    "    }\n"
    "    if (tracked_constructions == tracked_destructions)\n"
    "    {\n"
    "        display(\"VALUE RESIZE LIFETIME BALANCED\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_constructor_failure_rollback()\n"
    "{\n"
    "    rollback_construct_attempts = 0;\n"
    "    rollback_fail_at = 3;\n"
    "    rollback_successful_constructions = 0;\n"
    "    rollback_destructions = 0;\n"
    "    {\n"
    "        vector<ResizeRollbackProbe> values;\n"
    "        ResizeRollbackProbe *borrowed_probe;\n"
    "        int observed_payload;\n"
    "        int observed_size;\n"
    "        values.reserve(4);\n"
    "        values.emplace_back();\n"
    "        try { values.resize(4); }\n"
    "        catch (int construction_error)\n"
    "        {\n"
    "            if (values.size() == 1)\n"
    "            {\n"
    "                borrowed_probe = values.at(0);\n"
    "                observed_payload = borrowed_probe->payload;\n"
    "                observed_size = values.size();\n"
    "                display(\"RESIZE FAILURE OBSERVED\");\n"
    "                display(observed_size);\n"
    "                display(observed_payload);\n"
    "                display(rollback_construct_attempts);\n"
    "                display(rollback_successful_constructions);\n"
    "                display(rollback_destructions);\n"
    "                if (observed_payload == 41)\n"
    "                {\n"
    "                    if (rollback_successful_constructions == 2)\n"
    "                    {\n"
    "                        if (rollback_destructions == 1)\n"
    "                        {\n"
    "                            display(\"RESIZE FAILURE PRESERVES ORIGINAL\");\n"
    "                        }\n"
    "                    }\n"
    "                }\n"
    "            }\n"
    "        }\n"
    "    }\n"
    "    display(\"RESIZE FAILURE AFTER TRY\");\n"
    "    display(rollback_construct_attempts);\n"
    "    display(rollback_successful_constructions);\n"
    "    display(rollback_destructions);\n"
    "    if (rollback_construct_attempts == 3\n"
    "        && rollback_successful_constructions == 2\n"
    "        && rollback_destructions == 2)\n"
    "    {\n"
    "        display(\"RESIZE FAILURE CLEANUP BALANCED\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_each_resize_failure_position()\n"
    "{\n"
    "    int failure_attempt;\n"
    "    int rollback_ok;\n"
    "    failure_attempt = 2;\n"
    "    rollback_ok = 1;\n"
    "    while (failure_attempt <= 4)\n"
    "    {\n"
    "        rollback_construct_attempts = 0;\n"
    "        rollback_fail_at = failure_attempt;\n"
    "        rollback_successful_constructions = 0;\n"
    "        rollback_destructions = 0;\n"
    "        {\n"
    "            vector<ResizeRollbackProbe> values;\n"
    "            ResizeRollbackProbe *borrowed_probe;\n"
    "            values.reserve(4);\n"
    "            values.emplace_back();\n"
    "            try { values.resize(4); rollback_ok = 0; }\n"
    "            catch (int construction_error)\n"
    "            {\n"
    "                if (values.size() != 1) { rollback_ok = 0; }\n"
    "                if (rollback_successful_constructions != failure_attempt - 1) { rollback_ok = 0; }\n"
    "                if (rollback_destructions != failure_attempt - 2) { rollback_ok = 0; }\n"
    "                borrowed_probe = values.at(0);\n"
    "                if (borrowed_probe->payload != 41) { rollback_ok = 0; }\n"
    "            }\n"
    "        }\n"
    "        if (rollback_destructions != rollback_successful_constructions) { rollback_ok = 0; }\n"
    "        failure_attempt = failure_attempt + 1;\n"
    "    }\n"
    "    if (rollback_ok == 1)\n"
    "    {\n"
    "        display(\"EVERY RESIZE CONSTRUCTOR POSITION ROLLS BACK\");\n"
    "    }\n"
    "}\n"
    "\n"
    "void exercise_scope()\n"
    "{\n"
    "    vector<int> scoped;\n"
    "    scoped.push_back(41);\n"
    "    scoped.push_back(42);\n"
    "    value = scoped.size();\n"
    "    display(value);\n"
    "    value = scoped.front_value();\n"
    "    display(value);\n"
    "    value = scoped.back_value();\n"
    "    display(value);\n"
    "    return;\n"
    "}\n"
    "\n"
    "int count_string_bytes(string candidate)\n"
    "{\n"
    "    int length;\n"
    "    length = candidate.byte_length();\n"
    "    return length;\n"
    "}\n"
    "\n"
    "void main()\n"
    "{\n"
    "    vector<int> zero_assignment;\n"
    "    vector<int> score_history;\n"
    "    vector<int> default_scores(3);\n"
    "    vector<int> filled_scores(3, 7);\n"
    "    vector<int> copied(score_history);\n"
    "    vector<int> assigned;\n"
    "    vector<int> scalar_argument_values;\n"
    "    vector<int> scalar_argument_source;\n"
    "    vector<string> words;\n"
    "    vector<string> copied_words;\n"
    "    vector<vector<string>> nested_words;\n"
    "    string text_probe(\"alpha\");\n"
    "    string codepoint_probe;\n"
    "    string capacity_constructed(12);\n"
    "    char suffix_byte;\n"
    "    string *borrowed_word;\n"
    "    vector<string> *borrowed_words;\n"
    "    zero_assignment.reserve(4);\n"
    "    zero_assignment.push_back(9);\n"
    "    zero_assignment.assign(0, 11);\n"
    "    display(\"ZERO ASSIGN\");\n"
    "    value = zero_assignment.size();\n"
    "    display(value);\n"
    "    value = zero_assignment.capacity();\n"
    "    display(value);\n"
    "    display(\"STRING CAPACITY\");\n"
    "    display(\"VECTOR MAX SIZE\");\n"
    "    value = score_history.max_size();\n"
    "    display(value);\n"
    "    value = words.max_size();\n"
    "    display(value);\n"
    "    try { score_history.reserve(250000000); }\n"
    "    catch (const length_error& error) { display(\"INT RESERVE ABOVE MAX CAUGHT\"); }\n"
    "    try { words.reserve(62500000); }\n"
    "    catch (const length_error& error) { display(\"STRING RESERVE ABOVE MAX CAUGHT\"); }\n"
    "    value = text_probe.capacity();\n"
    "    display(value);\n"
    "    value = capacity_constructed.capacity();\n"
    "    display(value);\n"
    "    text_probe.reserve(12);\n"
    "    value = text_probe.capacity();\n"
    "    if (value >= 12)\n"
    "    {\n"
    "        display(\"STRING RESERVE OK\");\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        display(\"STRING RESERVE FAILED\");\n"
    "    }\n"
    "    try { text_probe.reserve(-1); }\n"
    "    catch (const invalid_argument& error) { display(\"STRING RESERVE NEGATIVE CAUGHT\"); }\n"
    "    try { text_probe.reserve(999999999); }\n"
    "    catch (const length_error& error) { display(\"STRING RESERVE TOO LARGE CAUGHT\"); }\n"
    "    value = text_probe.byte_length();\n"
    "    display(value);\n"
    "    codepoint_probe.push_codepoint(233);\n"
    "    display(\"UTF8 CODEPOINT LENGTH\");\n"
    "    value = codepoint_probe.byte_length();\n"
    "    display(value);\n"
    "    value = codepoint_probe.codepoint_length();\n"
    "    display(value);\n"
    "    display(\"UTF8 PREFIX SUFFIX\");\n"
    "    try { codepoint_probe.remove_prefix(1); }\n"
    "    catch (const invalid_argument& error) { display(\"PREFIX SPLIT CAUGHT\"); }\n"
    "    value = codepoint_probe.byte_length();\n"
    "    display(value);\n"
    "    codepoint_probe.remove_prefix(2);\n"
    "    value = codepoint_probe.byte_length();\n"
    "    display(value);\n"
    "    codepoint_probe.push_codepoint(233);\n"
    "    suffix_byte = 'x';\n"
    "    codepoint_probe.push_back(suffix_byte);\n"
    "    try { codepoint_probe.remove_suffix(2); }\n"
    "    catch (const invalid_argument& error) { display(\"SUFFIX SPLIT CAUGHT\"); }\n"
    "    value = codepoint_probe.byte_length();\n"
    "    display(value);\n"
    "    codepoint_probe.remove_suffix(1);\n"
    "    value = codepoint_probe.byte_length();\n"
    "    display(value);\n"
    "    codepoint_probe.remove_suffix(2);\n"
    "    value = codepoint_probe.byte_length();\n"
    "    display(value);\n"
    "    display(\"DEFAULT\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    value = score_history.empty();\n"
    "    display(value);\n"
    "    value = score_history.max_size();\n"
    "    display(value);\n"
    "\n"
    "    score_history.reserve(4);\n"
    "    display(\"RESERVE\");\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.reserve(2);\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.push_back(10);\n"
    "    score_history.push_back(20);\n"
    "    score_history.emplace_back(30);\n"
    "    display(\"PUSH\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at_value(0);\n"
    "    display(value);\n"
    "    value = score_history.at_value(2);\n"
    "    display(value);\n"
    "    value = score_history.front_value();\n"
    "    display(value);\n"
    "    value = score_history.back_value();\n"
    "    display(value);\n"
    "    display(\"COUNT CONSTRUCTORS\");\n"
    "    value = default_scores.size();\n"
    "    display(value);\n"
    "    value = default_scores.at_value(2);\n"
    "    display(value);\n"
    "    value = filled_scores.size();\n"
    "    display(value);\n"
    "    value = filled_scores.at_value(1);\n"
    "    display(value);\n"
    "    try { vector<int> negative_scores(-1); }\n"
    "    catch (const invalid_argument& error) { display(\"COUNT NEGATIVE CAUGHT\"); }\n"
    "    try { vector<int> oversized_scores(250000000); }\n"
    "    catch (const length_error& error) { display(\"COUNT ABOVE MAX CAUGHT\"); }\n"
    "    display(\"DATA\");\n"
    "    value = score_history.at_value(1);\n"
    "    display(value);\n"
    "\n"
    "    score_history.insert(1, 15);\n"
    "    score_history.insert(0, 5);\n"
    "    score_history.insert(5, 35);\n"
    "    try { score_history.insert(-1, 99); }\n"
    "    catch (const out_of_range& error) { display(\"INSERT_NEGATIVE_CAUGHT\"); "
    "value = error.code; display(value); }\n"
    "    try { score_history.insert(7, 99); }\n"
    "    catch (const out_of_range& error) { display(\"INSERT_PAST_END_CAUGHT\"); }\n"
    "    display(\"INSERT\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at_value(0);\n"
    "    display(value);\n"
    "    value = score_history.at_value(1);\n"
    "    display(value);\n"
    "    value = score_history.at_value(2);\n"
    "    display(value);\n"
    "    value = score_history.at_value(3);\n"
    "    display(value);\n"
    "    value = score_history.at_value(4);\n"
    "    display(value);\n"
    "    value = score_history.at_value(5);\n"
    "    display(value);\n"
    "\n"
    "    score_history.erase(2);\n"
    "    try { score_history.erase(-1); }\n"
    "    catch (const out_of_range& error) { display(\"ERASE_NEGATIVE_CAUGHT\"); }\n"
    "    try { score_history.erase(5); }\n"
    "    catch (const out_of_range& error) { display(\"ERASE_END_CAUGHT\"); }\n"
    "    score_history.pop_back();\n"
    "    display(\"ERASE_POP\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at_value(2);\n"
    "    display(value);\n"
    "    value = score_history.back_value();\n"
    "    display(value);\n"
    "\n"
    "    score_history.resize(8);\n"
    "    display(\"RESIZE_UP\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at_value(5);\n"
    "    display(value);\n"
    "    value = score_history.at_value(7);\n"
    "    display(value);\n"
    "    score_history.resize(3);\n"
    "    display(\"RESIZE_DOWN\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.back_value();\n"
    "    display(value);\n"
    "    try { score_history.resize(-1); }\n"
    "    catch (const invalid_argument& error) { display(\"RESIZE_NEGATIVE_CAUGHT\"); }\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "\n"
    "    score_history.assign(4, 7);\n"
    "    display(\"ASSIGN\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at_value(0);\n"
    "    display(value);\n"
    "    value = score_history.at_value(3);\n"
    "    display(value);\n"
    "    try { score_history.assign(-2, 9); }\n"
    "    catch (const invalid_argument& error) { display(\"ASSIGN_NEGATIVE_CAUGHT\"); }\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "\n"
    "    score_history.reserve(8);\n"
    "    score_history.push_back(1);\n"
    "    score_history.push_back(2);\n"
    "    score_history.shrink_to_fit();\n"
    "    display(\"SHRINK\");\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.clear();\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.shrink_to_fit();\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    try { score_history.pop_back(); }\n"
    "    catch (const out_of_range& error) { display(\"POP_EMPTY_CAUGHT\"); }\n"
    "    try { score_history.erase(0); }\n"
    "    catch (const out_of_range& error) { display(\"ERASE_EMPTY_CAUGHT\"); }\n"
    "\n"
    "    score_history.push_back(11);\n"
    "    score_history.push_back(22);\n"
    "    score_history.push_back(33);\n"
    "    assigned = score_history;\n"
    "    assigned = assigned;\n"
    "    score_history.erase(0);\n"
    "    display(\"COPY_ASSIGN\");\n"
    "    value = assigned.size();\n"
    "    display(value);\n"
    "    value = assigned.at_value(0);\n"
    "    display(value);\n"
    "    value = score_history.at_value(0);\n"
    "    display(value);\n"
    "\n"
    "    scalar_argument_values.push_back(scalar_argument_source.size());\n"
    "    display(\"SCALAR METHOD ARGUMENT\");\n"
    "    value = scalar_argument_values.at_value(0);\n"
    "    display(value);\n"

    "    {\n"
    "        string first(\"alpha\");\n"
    "        string second(\"beta\");\n"
    "        string alias_seed(\"stable\");\n"
    "        vector<string> alias_words;\n"
    "        string exclamation(\"!\");\n"
    "        string question(\"?\");\n"
    "        string tilde(\"~\");\n"
    "        string e_acute(\"é\");\n"
    "        string e_acute_original(\"é\");\n"
    "        words.push_back(first);\n"
    "        words.push_back(second);\n"
    "        copied_words = words;\n"
    "        copied_words = copied_words;\n"
    "        second.clear();\n"
    "        borrowed_word = words.at(1);\n"
    "        borrowed_word->append(exclamation);\n"
    "        display(\"STRING_COPY\");\n"
    "        borrowed_word = words.at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word = words.at(1);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word = copied_words.at(1);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        alias_words.push_back(alias_seed);\n"
    "        alias_words.push_back(alias_words.at_value(0));\n"
    "        alias_words.push_back(alias_words.at_value(0));\n"
    "        borrowed_word = alias_words.at(0);\n"
    "        borrowed_word->append(exclamation);\n"
    "        display(\"SELF-ALIAS GROWTH\");\n"
    "        value = alias_words.size();\n"
    "        display(value);\n"
    "        borrowed_word = alias_words.at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word = alias_words.at(1);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word = alias_words.at(2);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        value = alias_seed.byte_length();\n"
    "        display(value);\n"
    "        value = count_string_bytes(alias_words.at_value(0));\n"
    "        display(\"GLOBAL CLASS ARGUMENT\");\n"
    "        display(value);\n"
    "        value = tilde.compare(e_acute);\n"
    "        display(value);\n"
    "        value = tilde.strcmp(e_acute);\n"
    "        display(value);\n"
    "        e_acute.toupper();\n"
    "        value = e_acute.equals(e_acute_original);\n"
    "        display(value);\n"
    "        words.erase(0);\n"
    "        borrowed_word = words.at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        nested_words.push_back(words);\n"
    "        nested_words.push_back(nested_words.at_value(0));\n"
    "        display(\"NESTED SELF-ALIAS\");\n"
    "        value = nested_words.size();\n"
    "        display(value);\n"
    "        borrowed_words = nested_words.at(0);\n"
    "        borrowed_word = borrowed_words->at(0);\n"
    "        borrowed_word->append(question);\n"
    "        display(\"NESTED_STRING\");\n"
    "        borrowed_word = borrowed_words->at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word = words.at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_words = nested_words.at(1);\n"
    "        borrowed_word = borrowed_words->at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word->append(tilde);\n"
    "        borrowed_words = nested_words.at(0);\n"
    "        borrowed_word = borrowed_words->at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_words = nested_words.at(1);\n"
    "        borrowed_word = borrowed_words->at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        borrowed_word = words.at(0);\n"
    "        value = borrowed_word->byte_length();\n"
    "        display(value);\n"
    "        nested_words.clear();\n"
    "        words.clear();\n"
    "        copied_words.clear();\n"
    "        value = nested_words.size();\n"
    "        display(value);\n"
    "        value = words.size();\n"
    "        display(value);\n"
    "        value = copied_words.size();\n"
    "        display(value);\n"
    "    }\n"
    "\n"
    "    exercise_tracked_vector();\n"
    "    exercise_reserved_lifecycle();\n"
    "    exercise_assign_zero_lifecycle();\n"
    "    exercise_last_erase_lifecycle();\n"
    "    exercise_insert_at_end_uses_spare_capacity();\n"
    "    exercise_count_construction();\n"
    "    exercise_value_resize_lifecycle();\n"
    "    exercise_constructor_failure_rollback();\n"
    "    exercise_each_resize_failure_position();\n"
    "\n"
    "    exercise_scope();\n"
    "    display(\"VECTOR COMPLETE\");\n"
    "    return;\n"
    "}\n";

static const char *g_vector_string_source =
    "void main()\n"
    "{\n"
    "    vector<string> names;\n"
    "    string fill_value;\n"
    "    string *borrowed_name;\n"
    "    int value;\n"
    "    int slot;\n"
    "    fill_value.append(fill_value);\n"
    "    names.resize(2, fill_value);\n"
    "    slot = 0;\n"
    "    borrowed_name = names.at(slot);\n"
    "    value = borrowed_name->byte_length();\n"
    "    value = names[slot]->len();\n"
    "    value = names[slot]->byte_length();\n"
    "    names[slot]->clear();\n"
    "    value = names->len();\n"
    "    return;\n"
    "}\n";

static const char *g_vector_nested_string_source =
    "int nested_length;\n"
    "\n"
    "void consume_by_move(string *source)\n"
    "{\n"
    "    string moved;\n"
    "    moved.move_from(source);\n"
    "    nested_length = moved.byte_length();\n"
    "    moved.clear();\n"
    "    return;\n"
    "}\n"
    "\n"
    "void transfer_vector(vector<string> *destination, vector<string> *source)\n"
    "{\n"
    "    vector<string> *source_alias;\n"
    "    source_alias = source;\n"
    "    destination->move_from(source_alias);\n"
    "    return;\n"
    "}\n"
    "\n"
    "void main()\n"
    "{\n"
    "    vector<vector<string>> groups;\n"
    "    vector<vector<string>> alternate_groups;\n"
    "    vector<vector<string>> *alternate_groups_pointer;\n"
    "    vector<vector<vector<string>>> deeper_groups;\n"
    "    vector<vector<vector<vector<string>>>> four;\n"
    "    vector<vector<vector<vector<vector<string>>>>> five;\n"
    "    vector<string> *move_destination;\n"
    "    vector<string> *move_source;\n"
    "    string *borrowed_name;\n"
    "    int outer_index;\n"
    "    int move_state_ok;\n"
    "    int move_check_ok;\n"
    "    int move_observed;\n"
    "    vector<string> inner;\n"
    "    string seed(3);\n"
    "    string payload(\"abc\");\n"
    "    seed.append(payload);\n"
    "    move_state_ok = 1;\n"
    "    inner.push_back(seed);\n"
    "    move_source = &inner;\n"
    "    vector<string> transferred(move_source);\n"
    "    move_check_ok = 1;\n"
    "    if (inner.size() != 0 || transferred.size() != 1) { move_check_ok = 0; }\n"
    "    borrowed_name = transferred.at(0);\n"
    "    if (borrowed_name->byte_length() != 3) { move_check_ok = 0; }\n"
    "    move_observed = borrowed_name->codepoint_at(0);\n"
    "    if (move_observed != 97) { move_check_ok = 0; }\n"
    "    if (move_check_ok == 1) { display(\"VECTOR MOVE CONSTRUCT OK\"); }\n"
    "    else { display(\"VECTOR MOVE CONSTRUCT FAILED\"); move_state_ok = 0; }\n"
    "    inner.push_back(seed);\n"
    "    borrowed_name = inner.front();\n"
    "    seed.swap(borrowed_name);\n"
    "    nested_length = borrowed_name->byte_length();\n"
    "    consume_by_move(borrowed_name);\n"
    "    groups.push_back(inner);\n"
    "    groups.push_back(inner);\n"
    "    groups.at(0)->at(0)->append(seed);\n"
    "    move_destination = groups.front();\n"
    "    move_source = groups.back();\n"
    "    borrowed_name = move_destination->at(0);\n"
    "    move_observed = borrowed_name->byte_length();\n"
    "    display(move_observed);\n"
    "    borrowed_name = move_source->at(0);\n"
    "    move_observed = borrowed_name->byte_length();\n"
    "    display(move_observed);\n"
    "    transfer_vector(move_destination, move_source);\n"
    "    move_check_ok = 1;\n"
    "    if (groups.size() == 2) { display(\"MOVE_FROM OUTER SIZE OK\"); }\n"
    "    else { display(\"MOVE_FROM OUTER SIZE FAILED\"); move_check_ok = 0; }\n"
    "    if (move_destination->size() == 1) { display(\"MOVE_FROM DESTINATION SIZE OK\"); }\n"
    "    else { display(\"MOVE_FROM DESTINATION SIZE FAILED\"); move_check_ok = 0; }\n"
    "    if (move_source->size() == 0) { display(\"MOVE_FROM SOURCE EMPTY OK\"); }\n"
    "    else { display(\"MOVE_FROM SOURCE EMPTY FAILED\"); move_check_ok = 0; }\n"
    "    borrowed_name = move_destination->at(0);\n"
    "    move_observed = borrowed_name->byte_length();\n"
    "    display(move_observed);\n"
    "    if (move_observed == 3) { display(\"MOVE_FROM VALUE OK\"); }\n"
    "    else { display(\"MOVE_FROM VALUE FAILED\"); move_check_ok = 0; }\n"
    "    if (move_check_ok == 1) { display(\"NESTED VECTOR MOVE_FROM OK\"); }\n"
    "    else { display(\"NESTED VECTOR MOVE_FROM FAILED\"); move_state_ok = 0; }\n"
    "    deeper_groups.push_back(groups);\n"
    "    four.push_back(deeper_groups);\n"
    "    five.push_back(four);\n"
    "    outer_index = 0;\n"
    "    borrowed_name = groups[outer_index]->at(0);\n"
    "    borrowed_name->clear();\n"
    "    groups[outer_index]->clear();\n"
    "    borrowed_name = five[outer_index]->at(0)->at(0)->at(0)->at(0);\n"
    "    five.at(0)->at(0)->at(0)->at(0)->at(0)->append(seed);\n"
    "    nested_length = borrowed_name->byte_length();\n"
    "    borrowed_name->clear();\n"
    "    groups.erase(0);\n"
    "    inner.clear();\n"
    "    inner.push_back(seed);\n"
    "    move_source = &inner;\n"
    "    transferred.move_assign(move_source);\n"
    "    move_check_ok = 1;\n"
    "    if (inner.size() != 0 || transferred.size() != 1) { move_check_ok = 0; }\n"
    "    borrowed_name = transferred.at(0);\n"
    "    if (borrowed_name->byte_length() != 3) { move_check_ok = 0; }\n"
    "    move_observed = borrowed_name->codepoint_at(0);\n"
    "    if (move_observed != 97) { move_check_ok = 0; }\n"
    "    if (move_check_ok == 1) { display(\"VECTOR MOVE_ASSIGN OK\"); }\n"
    "    else { display(\"VECTOR MOVE_ASSIGN FAILED\"); move_state_ok = 0; }\n"
    "    move_source = &transferred;\n"
    "    transferred.move_assign(move_source);\n"
    "    move_check_ok = 1;\n"
    "    if (transferred.size() != 1) { move_check_ok = 0; }\n"
    "    borrowed_name = transferred.at(0);\n"
    "    if (borrowed_name->byte_length() != 3) { move_check_ok = 0; }\n"
    "    move_observed = borrowed_name->codepoint_at(0);\n"
    "    if (move_observed != 97) { move_check_ok = 0; }\n"
    "    if (move_check_ok == 1) { display(\"VECTOR SELF MOVE OK\"); }\n"
    "    else { display(\"VECTOR SELF MOVE FAILED\"); move_state_ok = 0; }\n"
    "    alternate_groups_pointer = &alternate_groups;\n"
    "    groups.swap(alternate_groups_pointer);\n"
    "    if (move_state_ok == 1) { display(\"NESTED VECTOR LIFECYCLE OK\"); }\n"
    "    else { display(\"NESTED VECTOR LIFECYCLE FAILED\"); }\n"
    "    return;\n"
    "}\n";

static const char *g_vector_throwing_element_source =
    "class Throwing\n"
    "{\n"
    "    public:\n"
    "    Throwing()\n"
    "    {\n"
    "        throw 1;\n"
    "    }\n"
    "    Throwing(Throwing *source) { }\n"
    "};\n"
    "int copy_attempts;\n"
    "class CopyThrowing\n"
    "{\n"
    "    public:\n"
    "    int value;\n"
    "    CopyThrowing() { value = 1; }\n"
    "    CopyThrowing(CopyThrowing source)\n"
    "    {\n"
    "        copy_attempts = copy_attempts + 1;\n"
    "        if (copy_attempts > 2) { throw 2; }\n"
    "        value = source.value;\n"
    "    }\n"
    "    ~CopyThrowing() { display(\"COPY DESTROY\"); }\n"
    "};\n"
    "void main()\n"
    "{\n"
    "    vector<Throwing> items;\n"
    "    vector<CopyThrowing> copies;\n"
    "    CopyThrowing *second_copy;\n"
    "    try { items.emplace_back(); }\n"
    "    catch (int construction_error) { }\n"
    "    copies.emplace_back();\n"
    "    copies.emplace_back();\n"
    "    copies.at(1)->value = 2;\n"
    "    second_copy = copies.at(1);\n"
    "    second_copy->value = 2;\n"
    "    try { copies.emplace_back(); }\n"
    "    catch (int copy_error) { }\n"
    "    return;\n"
    "}\n";

static const char *g_vector_based_receiver_method_source =
    "class Probe\n"
    "{\n"
    "    public:\n"
    "    int value;\n"
    "    Probe() { value = 42; }\n"
    "    Probe(Probe *source) { value = source->value; source->value = 0; }\n"
    "    void operator=(Probe source) { value = source.value; }\n"
    "    void clear() { value = 0; }\n"
    "    void reset() { clear(); }\n"
    "};\n"
    "void main()\n"
    "{\n"
    "    vector<Probe> values;\n"
    "    Probe seed;\n"
    "    Probe *observed;\n"
    "    int observed_value;\n"
    "    values.emplace_back(seed);\n"
    "    values.emplace_back();\n"
    "    values[0]->reset();\n"
    "    observed = values.at(0);\n"
    "    observed_value = observed->value;\n"
    "    display(observed_value);\n"
    "    return;\n"
    "}\n";

static const char *g_class_member_destruction_order_source =
    "class FirstMember\n"
    "{\n"
    "    public:\n"
    "    ~FirstMember() { display(\"FIRST_MEMBER\"); }\n"
    "};\n"
    "class SecondMember\n"
    "{\n"
    "    public:\n"
    "    ~SecondMember() { display(\"SECOND_MEMBER\"); }\n"
    "};\n"
    "class Owner\n"
    "{\n"
    "    FirstMember first;\n"
    "    SecondMember second;\n"
    "    public:\n"
    "    ~Owner() { display(\"OWNER_BODY\"); }\n"
    "};\n"
    "void main()\n"
    "{\n"
    "    Owner owner;\n"
    "    return;\n"
    "}\n";

static const char *g_vector_checked_access_source =
    "void main()\n"
    "{\n"
    "    vector<int> values;\n"
    "    vector<string> strings;\n"
    "    int *borrowed_front;\n"
    "    int *borrowed_back;\n"
    "    int observed;\n"
    "    try { observed = values.front_value(); }\n"
    "    catch (const out_of_range& error) { display(\"FRONT_EMPTY_CAUGHT\"); }\n"
    "    try { observed = values.back_value(); }\n"
    "    catch (const out_of_range& error) { display(\"BACK_EMPTY_CAUGHT\"); }\n"
    "    try { borrowed_front = values.front(); }\n"
    "    catch (const out_of_range& error) { display(\"FRONT_BORROW_EMPTY_CAUGHT\"); }\n"
    "    try { borrowed_back = values.back(); }\n"
    "    catch (const out_of_range& error) { display(\"BACK_BORROW_EMPTY_CAUGHT\"); }\n"
    "    try { observed = strings->len(); }\n"
    "    catch (const out_of_range& error) { display(\"ARROW_EMPTY_CAUGHT\"); }\n"
    "    try { values.pop_back(); }\n"
    "    catch (const out_of_range& error) { display(\"POP_EMPTY_CAUGHT\"); }\n"
    "    try { strings[0]->clear(); }\n"
    "    catch (const out_of_range& error) { display(\"INDEX_EMPTY_CAUGHT\"); }\n"
    "\n"
    "    values.push_back(7);\n"
    "    borrowed_front = values.front();\n"
    "    borrowed_back = values.back();\n"
    "    try { values.insert(-1, 99); }\n"
    "    catch (const out_of_range& error) { display(\"INSERT_NEGATIVE_CAUGHT\"); }\n"
    "    try { values.insert(2, 99); }\n"
    "    catch (const out_of_range& error) { display(\"INSERT_PAST_END_CAUGHT\"); }\n"
    "    try { values.erase(-1); }\n"
    "    catch (const out_of_range& error) { display(\"ERASE_NEGATIVE_CAUGHT\"); }\n"
    "    try { values.erase(1); }\n"
    "    catch (const out_of_range& error) { display(\"ERASE_END_CAUGHT\"); }\n"
    "    try { values.resize(-1); }\n"
    "    catch (const invalid_argument& error) { display(\"RESIZE_NEGATIVE_CAUGHT\"); }\n"
    "    try { values.assign(-1, 9); }\n"
    "    catch (const invalid_argument& error) { display(\"ASSIGN_NEGATIVE_CAUGHT\"); }\n"
    "    try { values.reserve(-1); }\n"
    "    catch (const invalid_argument& error) { display(\"RESERVE_NEGATIVE_CAUGHT\"); }\n"
    "\n"
    "    observed = values.at_value(0);\n"
    "    observed = observed - 7;\n"
    "    display(observed);\n"
    "    return;\n"
    "}\n";

FT_TEST(test_standard_library_vector_checked_errors_translate)
{
    char directory[512];
    char source_path[512];
    char cobol_path[512];
    char command[1024];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_checked.cblc", source_path, sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_checked.cob", cobol_path, sizeof(cobol_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, g_vector_checked_access_source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: checked vector error source should transpile\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "CBLC-EX-RAISING") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-PAYLOAD-OUT-OF-RANGE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-METHOD-MAIN-VALUES-CBLC-TPL-VECTOR-INT-POP-BACK") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-METHOD-MAIN-VALUES-CBLC-TPL-VECTOR-INT-INSERT") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-METHOD-MAIN-VALUES-CBLC-TPL-VECTOR-INT-ERASE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-METHOD-MAIN-VALUES-CBLC-TPL-VECTOR-INT-RESIZE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-METHOD-MAIN-VALUES-CBLC-TPL-VECTOR-INT-ASSIGN") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "RESERVE_NEGATIVE_CAUGHT") != FT_SUCCESS)
    {
        std::printf("Assertion failed: checked vector methods should lower explicit error paths\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    test_remove_file(source_path);
    test_remove_file(cobol_path);
    test_remove_directory(directory);
    return (status);
}

FT_TEST(test_standard_library_vector_storage_fields_are_private)
{
    const char *vector_source;
    const char *error_source;
    std::string source;
    t_cblc_translation_unit unit;
    int status;

    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!vector_source || !error_source)
        return (FT_FAILURE);
    if (std::strstr(vector_source, "T *data()"))
    {
        std::printf("Assertion failed: vector must not expose a raw data pointer before borrow lifetimes are checked\n");
        return (FT_FAILURE);
    }
    source = std::string(error_source) + "\n" + vector_source
        + "\nvector<int> values;\n"
        "void main() { int observed; observed = values.elements; }\n";
    cblc_translation_unit_init(&unit);
    status = cblc_parse_translation_unit(source.c_str(), &unit);
    if (status == FT_SUCCESS)
    {
        std::printf("Assertion failed: vector storage fields must not be publicly accessible\n");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_append_growth_stages_new_element_before_commit)
{
    const char *vector_source;
    const char *push_method;
    const char *push_allocate;
    const char *push_copy_existing;
    const char *push_construct_value;
    const char *push_catch;
    const char *push_rollback;
    const char *push_commit;
    const char *emplace_method;
    const char *emplace_construct_default;
    const char *emplace_catch;
    const char *emplace_value_method;
    const char *emplace_value_allocate;
    const char *emplace_value_copy;
    const char *emplace_value_construct;
    const char *emplace_value_catch;
    const char *emplace_value_rollback;
    const char *emplace_value_commit;
    const char *error_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    std::string source;
    int parse_status;

    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    push_method = vector_source
        ? std::strstr(vector_source, "void push_back(T value)") : NULL;
    push_allocate = push_method
        ? std::strstr(push_method, "relocation = new T[requested_capacity];") : NULL;
    push_copy_existing = push_allocate
        ? std::strstr(push_allocate, "while (relocation_index < count)") : NULL;
    push_construct_value = push_allocate
        ? std::strstr(push_allocate,
            "cblc_construct_at(relocation[count], value);") : NULL;
    push_catch = push_construct_value
        ? std::strstr(push_construct_value, "catch (...)") : NULL;
    push_rollback = push_catch
        ? std::strstr(push_catch, "cblc_destroy_at(relocation[relocation_index]);") : NULL;
    push_commit = push_rollback
        ? std::strstr(push_rollback, "destroy_current_elements();") : NULL;
    emplace_method = vector_source
        ? std::strstr(vector_source, "void emplace_back()") : NULL;
    emplace_construct_default = emplace_method
        ? std::strstr(emplace_method,
            "cblc_construct_at(relocation[count]);") : NULL;
    emplace_catch = emplace_construct_default
        ? std::strstr(emplace_construct_default, "catch (...)") : NULL;
    emplace_value_method = vector_source
        ? std::strstr(vector_source, "void emplace_back(T value)") : NULL;
    emplace_value_allocate = emplace_value_method
        ? std::strstr(emplace_value_method,
            "relocation = new T[requested_capacity];") : NULL;
    emplace_value_copy = emplace_value_allocate
        ? std::strstr(emplace_value_allocate,
            "while (relocation_index < count)") : NULL;
    emplace_value_construct = emplace_value_allocate
        ? std::strstr(emplace_value_allocate,
            "cblc_construct_at(relocation[count], value);") : NULL;
    emplace_value_catch = emplace_value_construct
        ? std::strstr(emplace_value_construct, "catch (...)") : NULL;
    emplace_value_rollback = emplace_value_catch
        ? std::strstr(emplace_value_catch,
            "cblc_destroy_at(relocation[relocation_index]);") : NULL;
    emplace_value_commit = emplace_value_rollback
        ? std::strstr(emplace_value_rollback, "destroy_current_elements();") : NULL;
    if (!vector_source || !push_method || !push_allocate || !push_copy_existing
        || !push_construct_value || !push_catch || !push_rollback || !push_commit
        || !emplace_method || !emplace_construct_default || !emplace_catch
        || !emplace_value_method || !emplace_value_allocate || !emplace_value_copy
        || !emplace_value_construct || !emplace_value_catch
        || !emplace_value_rollback || !emplace_value_commit
        || !(push_allocate < push_construct_value
            && push_construct_value < push_copy_existing
            && push_construct_value < push_catch
            && push_catch < push_rollback
            && push_rollback < push_commit
            && emplace_construct_default < emplace_catch
            && emplace_value_allocate < emplace_value_construct
            && emplace_value_construct < emplace_value_copy
            && emplace_value_construct < emplace_value_catch
            && emplace_value_catch < emplace_value_rollback
            && emplace_value_rollback < emplace_value_commit))
    {
        std::printf("Assertion failed: vector append growth must stage existing and new element lifetimes, roll back failures, and commit only after all construction succeeds\n");
        return (FT_FAILURE);
    }
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!error_source)
        return (FT_FAILURE);
    source = std::string(error_source) + "\n" + vector_source
        + "\nvoid main() { vector<int> values; values.push_back(7); "
        "values.emplace_back(8); values.emplace_back(); }\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    parse_status = cblc_parse_translation_unit(source.c_str(), &unit);
    if (parse_status != FT_SUCCESS
        || cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        std::printf("Append-growth specialization failed to generate COBOL: %s\n",
            unit.parse_error_message);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_insert_stages_before_commit)
{
    const char *vector_source;
    const char *insert_body;
    const char *end_capacity_guard;
    const char *end_construct;
    const char *end_count_commit;
    const char *end_return;
    const char *stage_allocation;
    const char *insert_construct;
    const char *prefix_loop;
    const char *prefix_relocate;
    const char *suffix_loop;
    const char *suffix_relocate;
    const char *catch_body;
    const char *rollback_suffix;
    const char *rollback_prefix;
    const char *rollback_insert;
    const char *rollback_release;
    const char *commit_destroy;
    const char *commit_release;
    const char *publish_storage;
    const char *publish_count;

    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    insert_body = vector_source ? std::strstr(vector_source, "void insert(int index, T value)") : NULL;
    end_capacity_guard = insert_body ? std::strstr(insert_body,
        "if (index == count && count < reserved)") : NULL;
    end_construct = end_capacity_guard ? std::strstr(end_capacity_guard,
        "cblc_construct_at(elements[count], value);") : NULL;
    end_count_commit = end_construct ? std::strstr(end_construct,
        "count = count + 1;") : NULL;
    end_return = end_count_commit ? std::strstr(end_count_commit,
        "return;") : NULL;
    stage_allocation = end_return ? std::strstr(end_return,
        "relocation = new T[requested_capacity];") : NULL;
    insert_construct = stage_allocation ? std::strstr(stage_allocation,
        "cblc_construct_at(relocation[index], value);") : NULL;
    prefix_loop = insert_construct ? std::strstr(insert_construct,
        "while (prefix_count < index)") : NULL;
    prefix_relocate = prefix_loop ? std::strstr(prefix_loop,
        "cblc_relocate_at(relocation[prefix_count],") : NULL;
    suffix_loop = prefix_relocate ? std::strstr(prefix_relocate,
        "while (suffix_count < count - index)") : NULL;
    suffix_relocate = suffix_loop ? std::strstr(suffix_loop,
        "cblc_relocate_at(relocation[index + suffix_count + 1],") : NULL;
    catch_body = suffix_relocate ? std::strstr(suffix_relocate, "catch (...)") : NULL;
    rollback_suffix = catch_body ? std::strstr(catch_body,
        "while (suffix_count > 0)") : NULL;
    rollback_prefix = rollback_suffix ? std::strstr(rollback_suffix,
        "while (prefix_count > 0)") : NULL;
    rollback_insert = rollback_prefix ? std::strstr(rollback_prefix,
        "cblc_destroy_at(relocation[index]);") : NULL;
    rollback_release = rollback_insert ? std::strstr(rollback_insert,
        "delete[] relocation;") : NULL;
    commit_destroy = rollback_release ? std::strstr(rollback_release,
        "destroy_current_elements();") : NULL;
    commit_release = commit_destroy ? std::strstr(commit_destroy,
        "delete[] elements;") : NULL;
    publish_storage = commit_release ? std::strstr(commit_release,
        "elements = relocation;") : NULL;
    publish_count = publish_storage ? std::strstr(publish_storage,
        "count = count + 1;") : NULL;
    if (!vector_source || !insert_body || !end_capacity_guard || !end_construct
        || !end_count_commit || !end_return || !stage_allocation || !insert_construct
        || !prefix_loop || !prefix_relocate || !suffix_loop || !suffix_relocate
        || !catch_body || !rollback_suffix || !rollback_prefix || !rollback_insert
        || !rollback_release || !commit_destroy || !commit_release
        || !publish_storage || !publish_count
        || !(end_capacity_guard < end_construct
            && end_construct < end_count_commit && end_count_commit < end_return
            && end_return < stage_allocation
            && stage_allocation < insert_construct && insert_construct < prefix_loop
            && prefix_loop < prefix_relocate && prefix_relocate < suffix_loop
            && suffix_loop < suffix_relocate && suffix_relocate < catch_body
            && catch_body < rollback_suffix && rollback_suffix < rollback_prefix
            && rollback_prefix < rollback_insert && rollback_insert < rollback_release
            && rollback_release < commit_destroy && commit_destroy < commit_release
            && commit_release < publish_storage && publish_storage < publish_count))
    {
        std::printf("Assertion failed: vector insert must stage its value before no-throw-or-copy relocation, roll back in reverse construction order, and commit only after success\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_erase_stages_before_commit)
{
    const char *vector_source;
    const char *erase_body;
    const char *range_guard;
    const char *last_element_guard;
    const char *last_destroy;
    const char *last_count_commit;
    const char *last_return;
    const char *stage_allocation;
    const char *copy_loop;
    const char *skip_erased_element;
    const char *element_copy;
    const char *catch_body;
    const char *rollback_destroy;
    const char *rollback_release;
    const char *commit_destroy;
    const char *commit_release;
    const char *publish_storage;
    const char *publish_count;

    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    erase_body = vector_source ? std::strstr(vector_source,
        "void erase(int index)") : NULL;
    range_guard = erase_body ? std::strstr(erase_body,
        "if (index < 0 || index >= count)") : NULL;
    last_element_guard = range_guard ? std::strstr(range_guard,
        "if (index == count - 1)") : NULL;
    last_destroy = last_element_guard ? std::strstr(last_element_guard,
        "cblc_destroy_at(elements[index]);") : NULL;
    last_count_commit = last_destroy ? std::strstr(last_destroy,
        "count = count - 1;") : NULL;
    last_return = last_count_commit ? std::strstr(last_count_commit,
        "return;") : NULL;
    stage_allocation = last_return ? std::strstr(last_return,
        "relocation = new T[reserved];") : NULL;
    copy_loop = stage_allocation ? std::strstr(stage_allocation,
        "while (source_index < count)") : NULL;
    skip_erased_element = copy_loop ? std::strstr(copy_loop,
        "if (source_index != index)") : NULL;
    element_copy = skip_erased_element ? std::strstr(skip_erased_element,
        "cblc_relocate_at(relocation[relocation_index],") : NULL;
    catch_body = element_copy ? std::strstr(element_copy, "catch (...)") : NULL;
    rollback_destroy = catch_body ? std::strstr(catch_body,
        "cblc_destroy_at(relocation[relocation_index]);") : NULL;
    rollback_release = rollback_destroy ? std::strstr(rollback_destroy,
        "delete[] relocation;") : NULL;
    commit_destroy = rollback_release ? std::strstr(rollback_release,
        "destroy_current_elements();") : NULL;
    commit_release = commit_destroy ? std::strstr(commit_destroy,
        "delete[] elements;") : NULL;
    publish_storage = commit_release ? std::strstr(commit_release,
        "elements = relocation;") : NULL;
    publish_count = publish_storage ? std::strstr(publish_storage,
        "count = count - 1;") : NULL;
    if (!vector_source || !erase_body || !range_guard || !last_element_guard
        || !last_destroy || !last_count_commit || !last_return
        || !stage_allocation || !copy_loop
        || !skip_erased_element || !element_copy || !catch_body
        || !rollback_destroy || !rollback_release || !commit_destroy
        || !commit_release || !publish_storage || !publish_count
        || std::strstr(erase_body, "elements[source_index] =") != NULL
        || !(range_guard < last_element_guard
            && last_element_guard < last_destroy
            && last_destroy < last_count_commit
            && last_count_commit < last_return && last_return < stage_allocation
            && stage_allocation < copy_loop
            && copy_loop < skip_erased_element
            && skip_erased_element < element_copy && element_copy < catch_body
            && catch_body < rollback_destroy && rollback_destroy < rollback_release
            && rollback_release < commit_destroy && commit_destroy < commit_release
            && commit_release < publish_storage && publish_storage < publish_count))
    {
        std::printf("Assertion failed: vector erase must stage surviving elements, roll back failed copies, and publish the reduced vector only after successful construction\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_resize_growth_builds_tail_before_relocation)
{
    const char *vector_source;
    const char *resize_method;
    const char *allocation;
    const char *tail_loop;
    const char *tail_construct;
    const char *relocation_loop;
    const char *relocation_call;
    const char *catch_body;
    const char *rollback_relocated;
    const char *rollback_tail;
    const char *resize_value_method;
    const char *value_allocation;
    const char *value_tail_loop;
    const char *value_relocation_loop;
    const char *value_relocation_call;
    const char *value_catch_body;
    const char *value_rollback_relocated;
    const char *value_rollback_tail;

    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    resize_method = vector_source
        ? std::strstr(vector_source, "void resize(int requested)") : NULL;
    allocation = resize_method
        ? std::strstr(resize_method,
            "relocation = new T[replacement_capacity];") : NULL;
    tail_loop = allocation
        ? std::strstr(allocation, "while (appended_index < requested)") : NULL;
    tail_construct = tail_loop
        ? std::strstr(tail_loop, "cblc_construct_at(relocation[appended_index]);") : NULL;
    relocation_loop = tail_construct
        ? std::strstr(tail_construct, "while (relocation_index < count)") : NULL;
    relocation_call = relocation_loop
        ? std::strstr(relocation_loop,
            "cblc_relocate_at(relocation[relocation_index],") : NULL;
    catch_body = relocation_call ? std::strstr(relocation_call, "catch (...)" ) : NULL;
    rollback_relocated = catch_body
        ? std::strstr(catch_body,
            "cblc_destroy_at(relocation[relocation_index]);") : NULL;
    rollback_tail = rollback_relocated
        ? std::strstr(rollback_relocated,
            "while (appended_index > count)") : NULL;

    resize_value_method = vector_source
        ? std::strstr(vector_source, "void resize(int requested, T value)") : NULL;
    value_allocation = resize_value_method
        ? std::strstr(resize_value_method,
            "relocation = new T[replacement_capacity];") : NULL;
    value_tail_loop = value_allocation
        ? std::strstr(value_allocation, "while (appended_index < requested)") : NULL;
    value_relocation_loop = value_tail_loop
        ? std::strstr(value_tail_loop, "while (relocation_index < count)") : NULL;
    value_relocation_call = value_relocation_loop
        ? std::strstr(value_relocation_loop,
            "cblc_relocate_at(relocation[relocation_index],") : NULL;
    value_catch_body = value_relocation_call
        ? std::strstr(value_relocation_call, "catch (...)" ) : NULL;
    value_rollback_relocated = value_catch_body
        ? std::strstr(value_catch_body,
            "cblc_destroy_at(relocation[relocation_index]);") : NULL;
    value_rollback_tail = value_rollback_relocated
        ? std::strstr(value_rollback_relocated,
            "while (appended_index > count)") : NULL;

    if (!vector_source || !resize_method || !allocation || !tail_loop
        || !tail_construct || !relocation_loop || !relocation_call
        || !catch_body || !rollback_relocated || !rollback_tail
        || !resize_value_method || !value_allocation || !value_tail_loop
        || !value_relocation_loop || !value_relocation_call || !value_catch_body
        || !value_rollback_relocated || !value_rollback_tail
        || !(allocation < tail_loop && tail_loop < tail_construct
            && tail_construct < relocation_loop && relocation_loop < relocation_call
            && relocation_call < catch_body && catch_body < rollback_relocated
            && rollback_relocated < rollback_tail
            && value_allocation < value_tail_loop
            && value_tail_loop < value_relocation_loop
            && value_relocation_loop < value_relocation_call
            && value_relocation_call < value_catch_body
            && value_catch_body < value_rollback_relocated
            && value_rollback_relocated < value_rollback_tail))
    {
        std::printf("Assertion failed: resize growth must stage potentially-throwing new elements before relocating live objects, then roll back in reverse construction order\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_resize_value_destroys_before_shrinking_live_range)
{
    const char *vector_source;
    const char *resize_method;
    const char *shrink_branch;
    const char *shrink_loop;
    const char *destroy_element;
    const char *shrink_count;

    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    resize_method = vector_source
        ? std::strstr(vector_source, "void resize(int requested, T value)") : NULL;
    shrink_branch = resize_method
        ? std::strstr(resize_method, "if (requested < count)") : NULL;
    shrink_loop = shrink_branch
        ? std::strstr(shrink_branch, "while (count > requested)") : NULL;
    destroy_element = shrink_loop
        ? std::strstr(shrink_loop, "cblc_destroy_at(elements[count - 1]);") : NULL;
    shrink_count = destroy_element
        ? std::strstr(destroy_element, "count = count - 1;") : NULL;

    if (!vector_source || !resize_method || !shrink_branch || !shrink_loop
        || !destroy_element || !shrink_count
        || !(shrink_loop < destroy_element && destroy_element < shrink_count)
        || std::strstr(shrink_loop, "cblc_destroy_at(elements[count]);"))
    {
        std::printf("Assertion failed: value-resize must destroy the last live element before reducing the vector live range\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_size_call_in_if_is_materialized_before_branch)
{
    const char *errors_source;
    const char *string_source;
    const char *vector_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    const t_cblc_function *target_function;
    const t_cblc_statement *size_call;
    const t_cblc_statement *if_statement;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    if (!errors_source || !string_source || !vector_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source + "\n"
        + vector_source + "\n" + g_vector_runtime_source;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("condition method-call parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    target_function = NULL;
    for (size_t function_index = 0; function_index < unit.function_count;
        ++function_index)
    {
        if (std::strcmp(unit.functions[function_index].source_name,
                "exercise_value_resize_lifecycle") == 0)
        {
            target_function = &unit.functions[function_index];
            break ;
        }
    }
    size_call = NULL;
    if_statement = NULL;
    if (target_function)
    {
        for (size_t statement_index = 0;
            statement_index < target_function->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement =
                &target_function->statements[statement_index];

            if (!size_call
                && statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "size") == 0)
                size_call = statement;
            if (size_call && statement->type == CBLC_STATEMENT_IF_BEGIN
                && statement_index > static_cast<size_t>(size_call
                    - target_function->statements))
            {
                if_statement = statement;
                break ;
            }
        }
    }
    if (!size_call || !if_statement
        || !std::strstr(if_statement->source, size_call->target)
        || std::strstr(if_statement->source, ".size()"))
    {
        std::printf("Assertion failed: scalar method calls in an if condition must be emitted once before the branch and replaced by their typed result temporary\n");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_string_chain_in_condition_uses_borrowed_element)
{
    const char *errors_source;
    const char *string_source;
    const char *vector_source;
    const char *program_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    const t_cblc_function *main_function;
    const t_cblc_statement *at_calls[9];
    const t_cblc_statement *len_calls[3];
    const t_cblc_statement *size_calls[1];
    const t_cblc_statement *if_statements[4];
    char *generated_cobol;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;
    size_t at_count;
    size_t len_count;
    size_t size_count;
    size_t if_count;
    int chain_calls_valid;
    int char_condition_temporary_valid;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    program_source = "void main() {\n"
        "    vector<string> items;\n"
        "    vector<vector<vector<vector<vector<string>>>>> five;\n"
        "    string value(\"hello\");\n"
        "    items.push_back(value);\n"
        "    if (items.at(0)->len() > 0) { }\n"
        "    if (five.at(0)->at(0)->at(0)->at(0)->at(0)->len() > 0) { }\n"
        "    if (items.at(0)->at(0) == 'h') { }\n"
        "    if (items.at(items.size())->len() > 0) { }\n"
        "}\n";
    if (!errors_source || !string_source || !vector_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source + "\n"
        + vector_source + "\n" + program_source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic[0] = '\0';
    diagnostic_code = 0;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("vector<string> condition-chain parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    main_function = NULL;
    for (size_t function_index = 0; function_index < unit.function_count;
        ++function_index)
    {
        if (std::strcmp(unit.functions[function_index].source_name, "main") == 0)
        {
            main_function = &unit.functions[function_index];
            break ;
        }
    }
    at_count = 0;
    len_count = 0;
    size_count = 0;
    if_count = 0;
    chain_calls_valid = 1;
    if (main_function)
    {
        for (size_t statement_index = 0;
            statement_index < main_function->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement =
                &main_function->statements[statement_index];

            if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "at") == 0)
            {
                if (at_count >= sizeof(at_calls) / sizeof(at_calls[0]))
                    chain_calls_valid = 0;
                else
                    at_calls[at_count++] = statement;
            }
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "len") == 0)
            {
                if (len_count >= sizeof(len_calls) / sizeof(len_calls[0]))
                    chain_calls_valid = 0;
                else
                    len_calls[len_count++] = statement;
            }
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "size") == 0)
            {
                if (size_count >= sizeof(size_calls) / sizeof(size_calls[0]))
                    chain_calls_valid = 0;
                else
                    size_calls[size_count++] = statement;
            }
            else if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && std::strstr(statement->source, "CONDITION-CHAIN"))
            {
                if (if_count >= sizeof(if_statements) / sizeof(if_statements[0]))
                    chain_calls_valid = 0;
                else
                    if_statements[if_count++] = statement;
            }
        }
    }
    if (!main_function || at_count != 9 || len_count != 3
        || size_count != 1 || if_count != 4)
        chain_calls_valid = 0;
    char_condition_temporary_valid = 0;
    for (size_t data_index = 0; data_index < unit.data_count; ++data_index)
    {
        const t_cblc_data_item *item = &unit.data_items[data_index];

        if (item->is_function_local
            && std::strstr(item->source_name, "CONDITION_CHAIN")
            && item->kind == CBLC_DATA_KIND_CHAR)
        {
            if (item->length == 1)
                char_condition_temporary_valid = 1;
            else
                chain_calls_valid = 0;
        }
    }
    if (!char_condition_temporary_valid)
        chain_calls_valid = 0;
    for (size_t call_index = 0; chain_calls_valid && call_index < at_count;
        ++call_index)
    {
        const t_cblc_data_item *borrowed_item;

        borrowed_item = NULL;
        for (size_t data_index = 0; data_index < unit.data_count; ++data_index)
        {
            if (std::strcmp(unit.data_items[data_index].cobol_name,
                    at_calls[call_index]->target) == 0)
            {
                borrowed_item = &unit.data_items[data_index];
                break ;
            }
        }
        if (!borrowed_item
            || (call_index == 7
                ? borrowed_item->kind != CBLC_DATA_KIND_CHAR
                : (borrowed_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                    || borrowed_item->struct_type_name[0] == '\0'))
            || at_calls[call_index]->call_argument_count != 1
            || (call_index > 0
                && at_calls[call_index]->source_offset
                    <= at_calls[call_index - 1]->source_offset))
            chain_calls_valid = 0;
    }
    for (size_t chain_index = 0; chain_calls_valid && chain_index < len_count;
        ++chain_index)
    {
        size_t last_at_index;

        last_at_index = chain_index == 0 ? 0
            : (chain_index == 1 ? 5 : 8);
        {
            size_t if_index;

            if_index = chain_index == 2 ? 3 : chain_index;
            if (!std::strstr(len_calls[chain_index]->target, "CONDITION-CHAIN")
                || len_calls[chain_index]->source_offset
                    <= at_calls[last_at_index]->source_offset
                || !std::strstr(if_statements[if_index]->source,
                    len_calls[chain_index]->target)
                || std::strstr(if_statements[if_index]->source,
                    "items.at(0)->len()"))
                chain_calls_valid = 0;
        }
    }
    if (chain_calls_valid
        && (size_calls[0]->source_offset <= at_calls[8]->source_offset
            || size_calls[0]->source_offset >= len_calls[2]->source_offset))
        chain_calls_valid = 0;
    if (!chain_calls_valid)
    {
        std::printf("Assertion failed: one- and five-level vector<string> chains, a char-returning chained method, and a method-valued index must preserve borrowed pointers, evaluation/source order, and condition materialization (at=%zu len=%zu size=%zu if=%zu)\n",
            at_count, len_count, size_count, if_count);
        if (main_function)
        {
            for (size_t statement_index = 0;
                statement_index < main_function->statement_count; ++statement_index)
            {
                const t_cblc_statement *statement =
                    &main_function->statements[statement_index];

                std::printf("condition chain IR[%zu]: type=%d target=%s source=%s method=%s args=%s\n",
                    statement_index, statement->type, statement->target,
                    statement->source, statement->call_identifier,
                    statement->call_arguments);
            }
        }
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("vector<string> condition-chain COBOL generation failed (%d): %s\n",
            diagnostic_code, diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (transpiler_validate_generated_cobol_with_diagnostic(generated_cobol,
            diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("vector<string> condition-chain COBOL validation failed (%d): %s\n",
            diagnostic_code, diagnostic);
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_short_circuit_guards_string_chain)
{
    const char *errors_source;
    const char *string_source;
    const char *vector_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    const t_cblc_function *main_function;
    size_t size_call_index;
    size_t guard_index;
    size_t at_call_index;
    size_t len_call_index;
    size_t guard_else_index;
    char *generated_cobol;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    if (!errors_source || !string_source || !vector_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source + "\n"
        + vector_source + "\n"
        + "void main() {\n"
        + "    vector<string> items;\n"
        + "    if (items.size() > 0 && items.at(0)->len() > 0) { }\n"
        + "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic[0] = '\0';
    diagnostic_code = 0;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("short-circuit vector<string> chain parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    main_function = NULL;
    for (size_t function_index = 0; function_index < unit.function_count;
        ++function_index)
    {
        if (std::strcmp(unit.functions[function_index].source_name, "main") == 0)
        {
            main_function = &unit.functions[function_index];
            break ;
        }
    }
    size_call_index = static_cast<size_t>(-1);
    guard_index = static_cast<size_t>(-1);
    at_call_index = static_cast<size_t>(-1);
    len_call_index = static_cast<size_t>(-1);
    guard_else_index = static_cast<size_t>(-1);
    if (main_function)
    {
        for (size_t statement_index = 0;
            statement_index < main_function->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement =
                &main_function->statements[statement_index];

            if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "size") == 0)
                size_call_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && std::strstr(statement->source, "BOOLEAN_OPERAND"))
                guard_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "at") == 0)
                at_call_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "len") == 0)
                len_call_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_ELSE
                && guard_index != static_cast<size_t>(-1)
                && statement_index > guard_index)
                guard_else_index = statement_index;
        }
    }
    if (!main_function || size_call_index == static_cast<size_t>(-1)
        || guard_index == static_cast<size_t>(-1)
        || at_call_index == static_cast<size_t>(-1)
        || len_call_index == static_cast<size_t>(-1)
        || guard_else_index == static_cast<size_t>(-1)
        || !(size_call_index < guard_index && guard_index < at_call_index
            && at_call_index < len_call_index
            && len_call_index < guard_else_index))
    {
        std::printf("Assertion failed: vector<string> checked access and its chained method must remain inside the RHS short-circuit branch (size=%zu guard=%zu at=%zu len=%zu else=%zu)\n",
            size_call_index, guard_index, at_call_index, len_call_index,
            guard_else_index);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS
        || !generated_cobol
        || transpiler_validate_generated_cobol_with_diagnostic(generated_cobol,
            diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("short-circuit vector<string> chain COBOL validation failed (%d): %s\n",
            diagnostic_code, diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_subscript_string_chain_in_condition_borrows_element)
{
    const char *errors_source;
    const char *string_source;
    const char *vector_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    const t_cblc_function *main_function;
    const t_cblc_statement *len_call;
    const t_cblc_statement *if_statement;
    const t_cblc_data_item *receiver_item;
    char *generated_cobol;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    string_source = transpiler_standard_library_get_native_source("CBLC-STRING");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    if (!errors_source || !string_source || !vector_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + string_source + "\n"
        + vector_source + "\n"
        + "void main() {\n"
        + "    vector<string> items;\n"
        + "    int index;\n"
        + "    if (items[index]->len() > 0) { }\n"
        + "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic[0] = '\0';
    diagnostic_code = 0;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("subscript vector<string> condition parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    main_function = NULL;
    for (size_t function_index = 0; function_index < unit.function_count;
        ++function_index)
    {
        if (std::strcmp(unit.functions[function_index].source_name, "main") == 0)
        {
            main_function = &unit.functions[function_index];
            break ;
        }
    }
    len_call = NULL;
    if_statement = NULL;
    if (main_function)
    {
        for (size_t statement_index = 0;
            statement_index < main_function->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement =
                &main_function->statements[statement_index];

            if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "len") == 0)
                len_call = statement;
            else if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && std::strstr(statement->source, "CONDITION-CHAIN"))
                if_statement = statement;
        }
    }
    receiver_item = NULL;
    if (len_call)
    {
        for (size_t data_index = 0; data_index < unit.data_count; ++data_index)
        {
            if (std::strcmp(unit.data_items[data_index].cobol_name,
                    len_call->source) == 0)
            {
                receiver_item = &unit.data_items[data_index];
                break ;
            }
        }
    }
    if (!main_function || !len_call || !if_statement || !receiver_item
        || receiver_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
        || std::strcmp(receiver_item->struct_type_name, "string") != 0
        || !std::strchr(receiver_item->source_name, '[')
        || !std::strstr(if_statement->source, len_call->target))
    {
        std::printf("Assertion failed: vector<string>::operator[](index)->len() in a condition must lower to a scalar result over the indexed borrow, not copy the element\n");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS
        || !generated_cobol
        || std::strstr(generated_cobol, "CBLC-SPTR-MAIN-ITEMS[")
        || transpiler_validate_generated_cobol_with_diagnostic(generated_cobol,
            diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("subscript vector<string> condition COBOL validation failed (%d): %s\n",
            diagnostic_code, diagnostic);
        if (generated_cobol)
        {
            const char *bad_based_name = std::strstr(generated_cobol,
                "CBLC-SPTR-MAIN-ITEMS[");

            if (bad_based_name)
                std::printf("Invalid indexed BASED declaration: %.96s\n",
                    bad_based_name);
        }
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_logical_conditions_guard_rhs_method_calls)
{
    const char *program_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    const t_cblc_function *main_function;
    size_t logical_and_guard;
    size_t logical_or_guard;
    size_t and_else_index;
    size_t and_right_call_index;
    size_t or_else_index;
    size_t or_right_call_index;
    size_t left_call_count;
    size_t right_call_count;
    size_t previous_call_source_offset;
    size_t index;
    int call_source_order_valid;
    char *generated_cobol;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;

    program_source = "class ConditionProbe {\n"
        "public:\n"
        "    int calls;\n"
        "    int left() { calls = calls + 1; return 0; }\n"
        "    int right() { calls = calls + 1; return 1; }\n"
        "};\n"
        "void main() {\n"
        "    ConditionProbe probe;\n"
        "    if ((probe.left() != 0) && (probe.right() != 0)) { }\n"
        "    if ((probe.left() != 0) || (probe.right() != 0)) { }\n"
        "    if (probe.left() == 0 || (probe.right() != 0 && probe.left() != 0)) { }\n"
        "}\n";
    combined_source = std::string(program_source);
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    diagnostic[0] = '\0';
    diagnostic_code = 0;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("short-circuit method-call parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    main_function = NULL;
    for (index = 0; index < unit.function_count; ++index)
    {
        if (std::strcmp(unit.functions[index].source_name, "main") == 0)
        {
            main_function = &unit.functions[index];
            break ;
        }
    }
    logical_and_guard = static_cast<size_t>(-1);
    logical_or_guard = static_cast<size_t>(-1);
    and_else_index = static_cast<size_t>(-1);
    and_right_call_index = static_cast<size_t>(-1);
    or_else_index = static_cast<size_t>(-1);
    or_right_call_index = static_cast<size_t>(-1);
    left_call_count = 0;
    right_call_count = 0;
    previous_call_source_offset = static_cast<size_t>(-1);
    call_source_order_valid = 1;
    if (main_function)
    {
        for (index = 0; index < main_function->statement_count; ++index)
        {
            const t_cblc_statement *statement = &main_function->statements[index];

            if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && std::strstr(statement->source, "BOOLEAN_OPERAND"))
            {
                if (logical_and_guard == static_cast<size_t>(-1))
                    logical_and_guard = index;
                else if (logical_or_guard == static_cast<size_t>(-1))
                    logical_or_guard = index;
            }
            else if (statement->type == CBLC_STATEMENT_IF_ELSE)
            {
                if (logical_and_guard != static_cast<size_t>(-1)
                    && and_else_index == static_cast<size_t>(-1)
                    && index > logical_and_guard)
                    and_else_index = index;
                else if (logical_or_guard != static_cast<size_t>(-1)
                    && or_else_index == static_cast<size_t>(-1)
                    && index > logical_or_guard)
                    or_else_index = index;
            }
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "left") == 0)
            {
                if (previous_call_source_offset != static_cast<size_t>(-1)
                    && statement->source_offset <= previous_call_source_offset)
                    call_source_order_valid = 0;
                previous_call_source_offset = statement->source_offset;
                left_call_count += 1;
            }
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "right") == 0)
            {
                if (previous_call_source_offset != static_cast<size_t>(-1)
                    && statement->source_offset <= previous_call_source_offset)
                    call_source_order_valid = 0;
                previous_call_source_offset = statement->source_offset;
                right_call_count += 1;
                if (and_right_call_index == static_cast<size_t>(-1))
                    and_right_call_index = index;
                else if (or_right_call_index == static_cast<size_t>(-1))
                    or_right_call_index = index;
            }
        }
    }
    if (!main_function || left_call_count != 4 || right_call_count != 3
        || !call_source_order_valid
        || logical_and_guard == static_cast<size_t>(-1)
        || logical_or_guard == static_cast<size_t>(-1)
        || and_right_call_index <= logical_and_guard
        || and_else_index <= and_right_call_index
        || or_else_index == static_cast<size_t>(-1)
        || or_else_index >= or_right_call_index)
    {
        std::printf("Assertion failed: logical RHS calls must remain inside their short-circuit control-flow branches (left=%zu right=%zu &&-guard=%zu &&-rhs=%zu &&-else=%zu ||-guard=%zu ||-else=%zu ||-rhs=%zu)\n",
            left_call_count, right_call_count, logical_and_guard,
            and_right_call_index, and_else_index, logical_or_guard,
            or_else_index, or_right_call_index);
        if (main_function)
        {
            for (index = 0; index < main_function->statement_count; ++index)
            {
                const t_cblc_statement *statement =
                    &main_function->statements[index];

                std::printf("short-circuit IR[%zu]: type=%d target=%s source=%s method=%s\n",
                    index, statement->type, statement->target, statement->source,
                    statement->call_identifier);
            }
        }
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) != FT_SUCCESS
        || !generated_cobol
        || transpiler_validate_generated_cobol_with_diagnostic(generated_cobol,
            diagnostic, sizeof(diagnostic)) != FT_SUCCESS)
    {
        std::printf("short-circuit method-call COBOL validation failed (%d): %s\n",
            diagnostic_code, diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_size_call_in_while_is_recomputed_each_iteration)
{
    const char *errors_source;
    const char *vector_source;
    const char *program_source;
    std::string combined_source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    char cobol_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;
    const t_cblc_function *target_function;
    size_t active_initialization_index;
    size_t while_begin_index;
    size_t size_call_index;
    size_t short_circuit_guard_index;
    size_t capacity_call_index;
    size_t condition_index;
    size_t result_condition_index;
    size_t loop_else_index;
    size_t active_exit_index;
    size_t condition_end_index;
    size_t while_end_index;
    size_t function_index;

    errors_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    program_source = "void main() {\n"
        "    vector<int> items;\n"
        "    items.push_back(1);\n"
        "    while (items.size() > 0 && items.capacity() > 0) {\n"
        "        items.pop_back();\n"
        "    }\n"
        "}\n";
    if (!errors_source || !vector_source)
        return (FT_FAILURE);
    combined_source = std::string(errors_source) + "\n" + vector_source + "\n"
        + program_source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    cobol_diagnostic[0] = '\0';
    diagnostic_code = 0;
    if (cblc_parse_translation_unit(combined_source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("while condition method-call parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    target_function = NULL;
    function_index = 0;
    while (function_index < unit.function_count)
    {
        if (std::strcmp(unit.functions[function_index].source_name,
                "main") == 0)
        {
            target_function = &unit.functions[function_index];
            break ;
        }
        function_index += 1;
    }
    active_initialization_index = static_cast<size_t>(-1);
    while_begin_index = static_cast<size_t>(-1);
    size_call_index = static_cast<size_t>(-1);
    short_circuit_guard_index = static_cast<size_t>(-1);
    capacity_call_index = static_cast<size_t>(-1);
    condition_index = static_cast<size_t>(-1);
    result_condition_index = static_cast<size_t>(-1);
    loop_else_index = static_cast<size_t>(-1);
    active_exit_index = static_cast<size_t>(-1);
    condition_end_index = static_cast<size_t>(-1);
    while_end_index = static_cast<size_t>(-1);
    if (target_function)
    {
        size_t statement_index;

        statement_index = 0;
        while (statement_index < target_function->statement_count)
        {
            const t_cblc_statement *statement =
                &target_function->statements[statement_index];

            if (statement->type == CBLC_STATEMENT_ASSIGNMENT
                && std::strcmp(statement->source, "0") == 0
                && std::strstr(statement->target, "WHILE-ACTIVE"))
                active_initialization_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_WHILE_BEGIN
                && std::strstr(statement->source, "WHILE_ACTIVE"))
                while_begin_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "size") == 0)
                size_call_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(statement->call_identifier, "capacity") == 0)
                capacity_call_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && std::strstr(statement->source, "BOOLEAN_OPERAND"))
                short_circuit_guard_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && std::strstr(statement->source, "BOOLEAN-RESULT"))
                result_condition_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_BEGIN
                && size_call_index != static_cast<size_t>(-1)
                && condition_index == static_cast<size_t>(-1)
                && std::strstr(statement->source, "WHILE-ACTIVE") == NULL)
                condition_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_ELSE)
                loop_else_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_ASSIGNMENT
                && std::strcmp(statement->source, "1") == 0
                && std::strstr(statement->target, "WHILE-ACTIVE"))
                active_exit_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_IF_END)
                condition_end_index = statement_index;
            else if (statement->type == CBLC_STATEMENT_WHILE_END)
                while_end_index = statement_index;
            statement_index += 1;
        }
    }
    if (!target_function
        || active_initialization_index == static_cast<size_t>(-1)
        || while_begin_index == static_cast<size_t>(-1)
        || size_call_index == static_cast<size_t>(-1)
        || short_circuit_guard_index == static_cast<size_t>(-1)
        || capacity_call_index == static_cast<size_t>(-1)
        || condition_index == static_cast<size_t>(-1)
        || result_condition_index == static_cast<size_t>(-1)
        || loop_else_index == static_cast<size_t>(-1)
        || active_exit_index == static_cast<size_t>(-1)
        || condition_end_index == static_cast<size_t>(-1)
        || while_end_index == static_cast<size_t>(-1)
        || !(active_initialization_index < while_begin_index
            && while_begin_index < size_call_index
            && size_call_index < condition_index
            && short_circuit_guard_index < capacity_call_index
            && capacity_call_index < result_condition_index
            && condition_index < loop_else_index
            && loop_else_index < active_exit_index
            && active_exit_index < condition_end_index
            && condition_end_index < while_end_index)
        || !std::strstr(target_function->statements[condition_index].source,
            "CONDITION_VALUE")
        || std::strstr(target_function->statements[condition_index].source,
            ".size()"))
    {
        std::printf("Assertion failed: while conditions must reevaluate operands per iteration, keep short-circuit RHS calls inside their guard, and exit after a false result (indices %zu %zu %zu %zu %zu %zu %zu %zu %zu %zu %zu)\n",
            active_initialization_index, while_begin_index, size_call_index,
            condition_index, short_circuit_guard_index, capacity_call_index,
            result_condition_index, loop_else_index, active_exit_index,
            condition_end_index, while_end_index);
        if (target_function)
        {
            size_t statement_index;

            statement_index = 0;
            while (statement_index < target_function->statement_count)
            {
                const t_cblc_statement *statement =
                    &target_function->statements[statement_index];

                std::printf("while IR[%zu]: type=%d target=%s source=%s method=%s\n",
                    statement_index, statement->type, statement->target,
                    statement->source, statement->call_identifier);
                statement_index += 1;
            }
        }
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, cobol_diagnostic,
            sizeof(cobol_diagnostic)) != FT_SUCCESS
        || !generated_cobol
        || transpiler_validate_generated_cobol_with_diagnostic(generated_cobol,
            cobol_diagnostic, sizeof(cobol_diagnostic)) != FT_SUCCESS)
    {
        std::printf("while method-call COBOL validation failed (%d): %s\n",
            diagnostic_code, cobol_diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_copy_assignment_commits_staged_parameter)
{
    const char *vector_source =
        transpiler_standard_library_get_native_source("CBLC-VECTOR");
    const char *assignment = vector_source
        ? std::strstr(vector_source, "void operator=(vector source)") : NULL;
    const char *assignment_end = assignment
        ? std::strstr(assignment, "\n    ~vector()") : NULL;

    if (!assignment || !assignment_end)
    {
        std::printf("Vector copy-assignment definition or its following destructor is missing\n");
        return FT_FAILURE;
    }
    const std::string assignment_body(assignment,
        static_cast<std::size_t>(assignment_end - assignment));
    if (assignment_body.find("elements = source.elements;") == std::string::npos
        || assignment_body.find("source.elements = temporary_data;")
            == std::string::npos
        || assignment_body.find("count = source.count;") == std::string::npos
        || assignment_body.find("reserved = source.reserved;")
            == std::string::npos
        || assignment_body.find("new T[") != std::string::npos
        || assignment_body.find("cblc_construct_at(") != std::string::npos)
    {
        std::printf("Vector copy assignment must exchange ownership with its already-staged by-value parameter without making a second element copy\n");
        return FT_FAILURE;
    }
    return FT_SUCCESS;
}

FT_TEST(test_standard_library_vector_assign_zero_skips_replacement_allocation)
{
    const char *vector_source =
        transpiler_standard_library_get_native_source("CBLC-VECTOR");
    const char *assignment = vector_source
        ? std::strstr(vector_source, "void assign(int requested, T value)") : NULL;
    const char *assignment_end = assignment
        ? std::strstr(assignment, "\n    void resize(int requested)") : NULL;

    if (!assignment || !assignment_end)
    {
        std::printf("Vector assign definition or its following resize method is missing\n");
        return FT_FAILURE;
    }
    const std::string assignment_body(assignment,
        static_cast<std::size_t>(assignment_end - assignment));
    const std::string zero_count_guard = "if (requested == 0)";
    const std::string allocation = "new T[replacement_capacity]";
    const std::string destroy_live_values = "destroy_current_elements();";
    const std::string publish_empty_size = "count = 0;";
    const std::string::size_type zero_guard_position =
        assignment_body.find(zero_count_guard);
    const std::string::size_type allocation_position =
        assignment_body.find(allocation);

    if (zero_guard_position == std::string::npos
        || allocation_position == std::string::npos
        || zero_guard_position > allocation_position
        || assignment_body.find(destroy_live_values, zero_guard_position)
            == std::string::npos
        || assignment_body.find(publish_empty_size, zero_guard_position)
            == std::string::npos
        || assignment_body.find("return;", zero_guard_position)
            == std::string::npos)
    {
        std::printf("assign(0, value) must destroy only live elements, publish size zero, and return before allocating replacement storage\n");
        return FT_FAILURE;
    }
    return FT_SUCCESS;
}

FT_TEST(test_standard_library_vector_translates_full_lifecycle)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char binary_path[256];
    char compiler_log_path[256];
    char runtime_output_path[256];
    char command[1024];
    char compiler_diagnostics[8192];
    char runtime_output[8192];
    int command_length;
    int status;
    size_t output_offset;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    binary_path[0] = '\0';
    compiler_log_path[0] = '\0';
    runtime_output_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
#ifdef _WIN32
    if (test_join_path(directory, "vector_runtime.exe", binary_path, sizeof(binary_path)) != FT_SUCCESS)
#else
    if (test_join_path(directory, "vector_runtime.bin", binary_path, sizeof(binary_path)) != FT_SUCCESS)
#endif
    {
        std::printf("Assertion failed: vector executable path should be constructible\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_join_path(directory, "vector_runtime.cblc", source_path, sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_runtime.cob", cobol_path, sizeof(cobol_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_runtime_compile.txt", compiler_log_path,
            sizeof(compiler_log_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_runtime_output.txt", runtime_output_path,
            sizeof(runtime_output_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, g_vector_runtime_source) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector source fixture should be writable at %s\n",
            source_path);
        goto cleanup;
    }
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector source should transpile\\n");
        goto cleanup;
    }
    {
        /* Reusable class methods now share receiver-independent COBOL
         * paragraphs, so paragraph names are intentionally not synthesized
         * from one source variable such as MAIN-SCORE-HISTORY. Verify the
         * operation-specific lowering state here; the runtime transcript
         * below exercises the value/read-only methods and their error paths. */
        static const char *const required_lowering[] = {
            "VECTOR-PUSH-BACK-RELOCATION",
            "VECTOR-POP-BACK-ERROR",
            "VECTOR-AT-ERROR-REQUESTED",
            "VECTOR-FRONT-ERROR-REQUESTED",
            "VECTOR-BACK-ERROR-REQUESTED",
            "VECTOR-EMPLACE-BACK-RELOCATION",
            "VECTOR-INSERT-SUFFIX-COUNT",
            "VECTOR-ERASE-SOURCE-INDEX",
            "VECTOR-ASSIGN-RELOCATION",
            "VECTOR-RESIZE-RELOCATION",
            "VECTOR-SHRINK-TO-FIT-RELOCATION",
            "CBLC-METHOD-CBLC-MRET-"
        };
        size_t index;

        for (index = 0; index < sizeof(required_lowering)
                / sizeof(required_lowering[0]); ++index)
        {
            if (vector_cobol_contains(cobol_path,
                    required_lowering[index]) != FT_SUCCESS)
            {
                std::printf("Assertion failed: vector translation should emit operation lowering %s\n",
                    required_lowering[index]);
                goto cleanup;
            }
        }
    }
    if (vector_cobol_contains(cobol_path,
            "CBLC-CONSTRUCTOR-MAIN-COPIED-3") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "ALLOCATE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "FREE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-RAISING") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-PAYLOAD-OUT-OF-RANGE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-PAYLOAD-OUT-OF-RANGE-REQUESTED") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-PAYLOAD-OUT-OF-RANGE-EXTENT") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "      -    ") == FT_SUCCESS)
    {
        std::printf("Assertion failed: vector output should emit ownership/bounds paths with safe continuation lines\n");
        goto cleanup;
    }
    command_length = std::snprintf(command, sizeof(command),
        "cobc -x -free -o \"%s\" \"%s\" > \"%s\" 2>&1",
        binary_path, cobol_path, compiler_log_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        if (test_read_text_file(compiler_log_path, compiler_diagnostics,
                sizeof(compiler_diagnostics)) == FT_SUCCESS)
            std::printf("Generated vector COBOL compiler diagnostics:\n%s\n",
                compiler_diagnostics);
        else
            std::printf("Assertion failed: generated vector COBOL did not compile\n");
        goto cleanup;
    }
#ifdef _WIN32
    command_length = std::snprintf(command, sizeof(command),
        "cmd /d /s /c \"\"%s\" > \"%s\" 2>&1\"", binary_path, runtime_output_path);
#else
    command_length = std::snprintf(command, sizeof(command), "\"%s\" > \"%s\" 2>&1",
        binary_path, runtime_output_path);
#endif
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        if (test_read_text_file(runtime_output_path, runtime_output,
                sizeof(runtime_output)) == FT_SUCCESS)
            std::printf("Generated vector runtime output:\n%s\n", runtime_output);
        else
            std::printf("Assertion failed: generated vector COBOL did not execute\n");
        goto cleanup;
    }
    if (test_read_text_file(runtime_output_path, runtime_output,
            sizeof(runtime_output)) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector runtime transcript should be readable\n");
        goto cleanup;
    }
    output_offset = 0;
#define VECTOR_EXPECT_MARKER(expected, message) \
    if (test_expect_transcript_status_line(runtime_output, &output_offset, expected, message) != FT_SUCCESS) \
    { \
        std::printf("Vector runtime transcript at assertion failure:\n%s\n", runtime_output); \
        goto cleanup; \
    }
#define VECTOR_EXPECT_VALUE(expected, message) \
    if (test_expect_transcript_double_line(runtime_output, &output_offset, expected, \
            g_default_float_tolerance, message) != FT_SUCCESS) \
    { \
        std::printf("Vector runtime transcript at assertion failure:\n%s\n", runtime_output); \
        goto cleanup; \
    }
    VECTOR_EXPECT_MARKER("ZERO ASSIGN", "zero-count assign marker");
    VECTOR_EXPECT_VALUE(0, "assign(0, value) destroys all live values");
    VECTOR_EXPECT_VALUE(4, "assign(0, value) retains existing capacity");
    VECTOR_EXPECT_MARKER("STRING CAPACITY", "string capacity marker");
    VECTOR_EXPECT_MARKER("VECTOR MAX SIZE", "vector max_size marker");
    VECTOR_EXPECT_VALUE(249999999, "vector<int> max_size respects the byte-offset limit");
    VECTOR_EXPECT_VALUE(62499999, "vector<string> max_size scales by the owning string record size");
    VECTOR_EXPECT_MARKER("INT RESERVE ABOVE MAX CAUGHT", "reserve rejects element counts exceeding the int storage bound");
    VECTOR_EXPECT_MARKER("STRING RESERVE ABOVE MAX CAUGHT", "reserve rejects element counts exceeding the string storage bound");
    VECTOR_EXPECT_VALUE(5, "string capacity excludes its terminating NUL byte");
    VECTOR_EXPECT_VALUE(12, "string capacity constructor accepts character capacity");
    VECTOR_EXPECT_MARKER("STRING RESERVE OK", "string reserve grows to requested character capacity");
    VECTOR_EXPECT_MARKER("STRING RESERVE NEGATIVE CAUGHT", "negative string reserve throws invalid_argument");
    VECTOR_EXPECT_MARKER("STRING RESERVE TOO LARGE CAUGHT", "oversized string reserve throws length_error");
    VECTOR_EXPECT_VALUE(5, "string reserve preserves the current byte length");
    VECTOR_EXPECT_MARKER("UTF8 CODEPOINT LENGTH", "UTF-8 codepoint length marker");
    VECTOR_EXPECT_VALUE(2, "U+00E9 occupies two UTF-8 bytes");
    VECTOR_EXPECT_VALUE(1, "codepoint_length counts Unicode scalar values rather than bytes");
    VECTOR_EXPECT_MARKER("UTF8 PREFIX SUFFIX", "UTF-8 prefix/suffix marker");
    VECTOR_EXPECT_MARKER("PREFIX SPLIT CAUGHT", "removing a partial UTF-8 prefix throws invalid_argument");
    VECTOR_EXPECT_VALUE(2, "failed prefix removal leaves the original string unchanged");
    VECTOR_EXPECT_VALUE(0, "removing a complete codepoint prefix empties the string");
    VECTOR_EXPECT_MARKER("SUFFIX SPLIT CAUGHT", "removing a partial UTF-8 suffix throws invalid_argument");
    VECTOR_EXPECT_VALUE(3, "failed suffix removal leaves all bytes unchanged");
    VECTOR_EXPECT_VALUE(2, "removing an ASCII suffix preserves the preceding codepoint");
    VECTOR_EXPECT_VALUE(0, "removing a complete codepoint suffix empties the string");
    VECTOR_EXPECT_MARKER("DEFAULT", "vector should begin with the default-state marker");
    VECTOR_EXPECT_VALUE(0, "default vector size");
    VECTOR_EXPECT_VALUE(0, "default vector capacity");
    VECTOR_EXPECT_VALUE(1, "empty default vector");
    VECTOR_EXPECT_VALUE(249999999, "vector<int> max_size accounts for four-byte elements");
    VECTOR_EXPECT_MARKER("RESERVE", "reserve marker");
    VECTOR_EXPECT_VALUE(4, "reserve grows capacity to the requested size");
    VECTOR_EXPECT_VALUE(4, "reserve with a smaller request preserves capacity");
    VECTOR_EXPECT_MARKER("PUSH", "push marker");
    VECTOR_EXPECT_VALUE(3, "push and emplace update size");
    VECTOR_EXPECT_VALUE(10, "first pushed element");
    VECTOR_EXPECT_VALUE(30, "last emplaced element");
    VECTOR_EXPECT_VALUE(10, "front returns the first element");
    VECTOR_EXPECT_VALUE(30, "back returns the last element");
    VECTOR_EXPECT_MARKER("COUNT CONSTRUCTORS", "count and count/value constructors marker");
    VECTOR_EXPECT_VALUE(3, "count constructor sets size");
    VECTOR_EXPECT_VALUE(0, "count constructor default-constructs scalar values");
    VECTOR_EXPECT_VALUE(3, "count/value constructor sets size");
    VECTOR_EXPECT_VALUE(7, "count/value constructor copies the supplied value");
    VECTOR_EXPECT_MARKER("COUNT NEGATIVE CAUGHT", "negative count constructor throws invalid_argument");
    VECTOR_EXPECT_MARKER("COUNT ABOVE MAX CAUGHT", "count above max_size throws length_error before allocation");
    VECTOR_EXPECT_MARKER("DATA", "data returns a pointer to contiguous element storage");
    VECTOR_EXPECT_VALUE(20, "data pointer indexes the second live vector element");
    VECTOR_EXPECT_MARKER("INSERT_NEGATIVE_CAUGHT", "negative insert throws out_of_range");
    VECTOR_EXPECT_VALUE(1, "out_of_range preserves its stable error category code");
    VECTOR_EXPECT_MARKER("INSERT_PAST_END_CAUGHT", "past-end insert throws out_of_range");
    VECTOR_EXPECT_MARKER("INSERT", "insert marker");
    VECTOR_EXPECT_VALUE(6, "valid inserts update vector size");
    VECTOR_EXPECT_VALUE(5, "insert at begin");
    VECTOR_EXPECT_VALUE(10, "insert shifts the old first element");
    VECTOR_EXPECT_VALUE(15, "insert at an interior position");
    VECTOR_EXPECT_VALUE(20, "insert preserves middle elements");
    VECTOR_EXPECT_VALUE(30, "insert preserves the old last element");
    VECTOR_EXPECT_VALUE(35, "insert at end");
    VECTOR_EXPECT_MARKER("ERASE_NEGATIVE_CAUGHT", "negative erase throws out_of_range");
    VECTOR_EXPECT_MARKER("ERASE_END_CAUGHT", "erase at size throws out_of_range");
    VECTOR_EXPECT_MARKER("ERASE_POP", "erase/pop marker");
    VECTOR_EXPECT_VALUE(4, "erase and pop update size");
    VECTOR_EXPECT_VALUE(20, "erase shifts the following element");
    VECTOR_EXPECT_VALUE(30, "pop removes the previous last element");
    VECTOR_EXPECT_MARKER("RESIZE_UP", "resize growth marker");
    VECTOR_EXPECT_VALUE(8, "resize growth updates size");
    VECTOR_EXPECT_VALUE(0, "new int elements are value initialized");
    VECTOR_EXPECT_VALUE(0, "last new int element is value initialized");
    VECTOR_EXPECT_MARKER("RESIZE_DOWN", "resize shrink marker");
    VECTOR_EXPECT_VALUE(3, "resize shrink updates size");
    VECTOR_EXPECT_VALUE(20, "resize shrink preserves remaining values");
    VECTOR_EXPECT_MARKER("RESIZE_NEGATIVE_CAUGHT", "negative resize throws invalid_argument");
    VECTOR_EXPECT_VALUE(3, "failed resize leaves the size unchanged");
    VECTOR_EXPECT_MARKER("ASSIGN", "assign marker");
    VECTOR_EXPECT_VALUE(4, "assign sets vector size");
    VECTOR_EXPECT_VALUE(7, "assign fills the first element");
    VECTOR_EXPECT_VALUE(7, "assign fills the last element");
    VECTOR_EXPECT_MARKER("ASSIGN_NEGATIVE_CAUGHT", "negative assign throws invalid_argument");
    VECTOR_EXPECT_VALUE(4, "failed assign leaves the size unchanged");
    VECTOR_EXPECT_MARKER("SHRINK", "shrink_to_fit marker");
    VECTOR_EXPECT_VALUE(6, "shrink_to_fit reduces capacity to size");
    VECTOR_EXPECT_VALUE(0, "clear empties the vector");
    VECTOR_EXPECT_VALUE(6, "clear retains allocated capacity");
    VECTOR_EXPECT_VALUE(0, "shrink_to_fit releases empty storage");
    VECTOR_EXPECT_MARKER("POP_EMPTY_CAUGHT", "pop_back on empty throws out_of_range");
    VECTOR_EXPECT_MARKER("ERASE_EMPTY_CAUGHT", "erase on empty throws out_of_range");
    VECTOR_EXPECT_MARKER("COPY_ASSIGN", "copy and self-assignment marker");
    VECTOR_EXPECT_VALUE(3, "copy assignment copies the element count");
    VECTOR_EXPECT_VALUE(11, "copy assignment copies the first element");
    VECTOR_EXPECT_VALUE(22, "mutating the source does not mutate its copy");
    VECTOR_EXPECT_MARKER("SCALAR METHOD ARGUMENT", "scalar-returning method argument marker");
    VECTOR_EXPECT_VALUE(0, "scalar-returning method argument is evaluated before vector insertion");
    VECTOR_EXPECT_MARKER("STRING_COPY", "vector<string> copy/reallocation marker");
    VECTOR_EXPECT_VALUE(5, "vector<string> copy preserves the first value");
    VECTOR_EXPECT_VALUE(5, "vector<string> mutation affects its own second value");
    VECTOR_EXPECT_VALUE(4, "vector<string> copy remains independent after source mutation");
    VECTOR_EXPECT_MARKER("SELF-ALIAS GROWTH", "vector<string> self-aliasing growth marker");
    VECTOR_EXPECT_VALUE(3, "self-aliased append grows the vector without losing elements");
    VECTOR_EXPECT_VALUE(7, "mutating the original after self-aliased appends does not corrupt it");
    VECTOR_EXPECT_VALUE(6, "self-aliased append constructs an independent first copy");
    VECTOR_EXPECT_VALUE(6, "self-aliased append constructs an independent second copy");
    VECTOR_EXPECT_VALUE(6, "self-aliased append leaves the source string unchanged");
    VECTOR_EXPECT_MARKER("GLOBAL CLASS ARGUMENT", "global function receives a class-valued method result");
    VECTOR_EXPECT_VALUE(7, "global function argument temporary preserves the selected string value");
    VECTOR_EXPECT_VALUE(-1, "UTF-8 string comparison uses unsigned byte order independent of the host collating sequence");
    VECTOR_EXPECT_VALUE(-1, "string strcmp uses the same unsigned byte ordering as compare");
    VECTOR_EXPECT_VALUE(1, "ASCII case conversion leaves non-ASCII UTF-8 bytes unchanged");
    VECTOR_EXPECT_VALUE(5, "vector<string> erase relocates the surviving string");
    VECTOR_EXPECT_MARKER("NESTED SELF-ALIAS", "nested vector self-aliasing growth marker");
    VECTOR_EXPECT_VALUE(2, "nested self-aliased append grows without losing the inner vector");
    VECTOR_EXPECT_MARKER("NESTED_STRING", "nested vector<string> borrow marker");
    VECTOR_EXPECT_VALUE(6, "nested borrowed string mutation changes only the nested copy");
    VECTOR_EXPECT_VALUE(5, "nested string mutation leaves the source string unchanged");
    VECTOR_EXPECT_VALUE(5, "nested self-aliased append creates an independent inner vector");
    VECTOR_EXPECT_VALUE(6, "mutating one nested copy leaves the other nested copy unchanged");
    VECTOR_EXPECT_VALUE(6, "the second nested copy accepts its own string mutation");
    VECTOR_EXPECT_VALUE(5, "nested self-aliasing leaves the original string vector unchanged");
    VECTOR_EXPECT_VALUE(0, "clear destroys and empties the nested vector");
    VECTOR_EXPECT_VALUE(0, "clear destroys and empties vector<string>");
    VECTOR_EXPECT_VALUE(0, "clear destroys and empties copied vector<string>");
    VECTOR_EXPECT_MARKER("TRACKED LIFETIME BALANCED", "all tracked objects are destroyed by scope exit after vector growth, erase, and clear");
    VECTOR_EXPECT_MARKER("RESERVE DOES NOT CONSTRUCT", "reserve allocates slots without constructing unused elements");
    VECTOR_EXPECT_MARKER("TWO LIVE ELEMENTS", "only successful emplace calls construct live vector elements");
    VECTOR_EXPECT_MARKER("POP DESTROYS ONE", "pop_back destroys exactly the removed element");
    VECTOR_EXPECT_MARKER("CLEAR DESTROYS REMAINDER", "clear destroys each remaining live element exactly once");
    VECTOR_EXPECT_MARKER("SCOPE DOES NOT DOUBLE DESTROY", "scope exit does not destroy elements already removed or cleared");
    VECTOR_EXPECT_MARKER("ZERO ASSIGN DESTROYS LIVE ELEMENTS", "assign(0, value) destroys live objects and its value parameter exactly once");
    VECTOR_EXPECT_MARKER("ZERO ASSIGN RETAINS CAPACITY", "assign(0, value) retains allocated capacity without treating slots as live");
    VECTOR_EXPECT_MARKER("ZERO ASSIGN LIFETIME BALANCED", "assign(0, value) leaves no live objects for vector scope exit to destroy");
    VECTOR_EXPECT_MARKER("ERASE LAST DOES NOT RELOCATE", "erasing the final vector element preserves capacity without allocating or reconstructing survivors");
    VECTOR_EXPECT_MARKER("ERASE LAST LIFETIME BALANCED", "last-element erase destroys exactly one live object and scope exit destroys the remaining object once");
    VECTOR_EXPECT_MARKER("END INSERT LIFETIME COUNTS", "end insertion exposes exact constructor and destructor counts");
    VECTOR_EXPECT_VALUE(4, "end insertion constructs the vector elements, insertion value, and parameter copy as expected");
    VECTOR_EXPECT_VALUE(1, "end insertion destroys its by-value parameter before scope exit");
    VECTOR_EXPECT_MARKER("END INSERT USES SPARE CAPACITY", "insertion at end with available capacity constructs only the new element");
    VECTOR_EXPECT_MARKER("END INSERT LIFETIME BALANCED", "end insertion preserves exactly-once destruction for the inserted value, parameter copy, and vector elements");
    VECTOR_EXPECT_MARKER("COUNT CONSTRUCTS LIVE ELEMENTS", "count constructor constructs exactly the requested live class elements");
    VECTOR_EXPECT_MARKER("COUNT CONSTRUCTION CLEANUP", "count-constructed class elements are destroyed exactly once");
    VECTOR_EXPECT_MARKER("COUNT VALUE PARAMETER CLEANED", "the by-value constructor parameter is destroyed when construction returns");
    VECTOR_EXPECT_VALUE(1, "exactly the constructor parameter copy is destroyed before the caller continues");
    VECTOR_EXPECT_MARKER("COUNT VALUE CONSTRUCTION", "count/value construction copies class values into live slots");
    VECTOR_EXPECT_MARKER("COUNT VALUE CLEANUP COUNTS", "count/value construction exposes exact total and destruction counts");
    VECTOR_EXPECT_VALUE(4, "count/value construction creates one seed, one parameter copy, and two elements");
    VECTOR_EXPECT_VALUE(4, "count/value scope exit destroys elements, seed, and parameter copy exactly once");
    VECTOR_EXPECT_MARKER("COUNT VALUE CLEANUP", "count/value elements and their seed are each destroyed exactly once");
    VECTOR_EXPECT_MARKER("VALUE RESIZE DESTROYS TAIL", "value-resize destroys removed class elements and publishes the reduced size");
    VECTOR_EXPECT_MARKER("VALUE RESIZE WHILE RECHECKS", "scalar condition methods are reevaluated on each loop iteration until the vector is empty");
    VECTOR_EXPECT_MARKER("VALUE RESIZE LIFETIME BALANCED", "value-resize leaves no live-element leaks or double destruction at scope exit");
    VECTOR_EXPECT_MARKER("RESIZE FAILURE OBSERVED", "failed growth preserves the original vector and reports exact rollback counters");
    VECTOR_EXPECT_VALUE(1, "failed growth leaves the original element count unchanged");
    VECTOR_EXPECT_VALUE(41, "failed growth leaves the original element payload unchanged");
    VECTOR_EXPECT_VALUE(3, "failed growth attempts exactly three constructions before failure");
    VECTOR_EXPECT_VALUE(2, "failed growth completes exactly two constructions");
    VECTOR_EXPECT_VALUE(1, "failed growth destroys only the successfully staged tail element");
    VECTOR_EXPECT_MARKER("RESIZE FAILURE PRESERVES ORIGINAL", "a staged constructor failure preserves the original vector size and value and destroys only completed tail objects");
    VECTOR_EXPECT_MARKER("RESIZE FAILURE AFTER TRY", "rollback state remains observable after catch dispatch");
    VECTOR_EXPECT_VALUE(3, "failure occurs on the third constructor attempt");
    VECTOR_EXPECT_VALUE(2, "two constructors complete before failure");
    VECTOR_EXPECT_VALUE(2, "one staged element and the original are destroyed by scope exit");
    VECTOR_EXPECT_MARKER("RESIZE FAILURE CLEANUP BALANCED", "failed resize rolls back completed tail objects and later destroys the original exactly once");
    VECTOR_EXPECT_MARKER("EVERY RESIZE CONSTRUCTOR POSITION ROLLS BACK", "resize failure at each tail-element construction position preserves old state and balances exact destruction counts");
    VECTOR_EXPECT_VALUE(2, "local vector is usable during its scope");
    VECTOR_EXPECT_VALUE(41, "local vector front");
    VECTOR_EXPECT_VALUE(42, "local vector back");
    VECTOR_EXPECT_MARKER("VECTOR COMPLETE", "vector runtime completion marker");
    if (test_expect_transcript_complete(runtime_output, output_offset,
            "vector runtime should emit exactly the documented transcript") != FT_SUCCESS)
    {
        std::printf("Vector runtime transcript has trailing or missing output:\n%s\n",
            runtime_output);
        goto cleanup;
    }
#undef VECTOR_EXPECT_MARKER
#undef VECTOR_EXPECT_VALUE
    status = FT_SUCCESS;
cleanup:
    if (status == FT_SUCCESS)
    {
        test_remove_file(binary_path);
        test_remove_file(compiler_log_path);
        test_remove_file(runtime_output_path);
        test_remove_file(source_path);
        test_remove_file(cobol_path);
        test_remove_directory(directory);
    }
    else
        std::printf("Generated vector probe retained at %s\n", directory);
    return (status);
}

FT_TEST(test_standard_library_vector_string_runtime_fixture_generates)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[1024];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_string_runtime.cblc", source_path,
            sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_string_runtime.cob", cobol_path,
            sizeof(cobol_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, g_vector_runtime_source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "VECTOR-PUSH-BACK-APPENDED-CONSTRUCTED") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "CBLC-METHOD-CBLC-MRET-") != FT_SUCCESS)
    {
        std::printf("Missing vector<string> push_back lowering\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "MOVE MAIN-WORDS TO CBLC-P-") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "MOVE MAIN-COPIED-WORDS TO CBLC-P-") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "PERFORM CBLC-METHOD-CBLC-MRET-") != FT_SUCCESS)
    {
        std::printf("Missing vector<string> copy-assignment lowering\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "01 CBLC-SPTR-HFF8327CEBE12D4B1 BASED.") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "01 MAIN-NESTED-WORDS.") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "05 MAIN-NESTED-WORDS-ELEMENTS USAGE POINTER") != FT_SUCCESS)
    {
        std::printf("Missing vector<string> element layout or vector<vector<string>> owner storage\n");
        goto cleanup;
    }
    if (vector_cobol_main_contains_in_order(cobol_path,
            "TO MAIN-CBLC-CHAIN-ARG-",
            "MOVE MAIN-CBLC-CHAIN-ARG-",
            "DISPLAY \"SELF-ALIAS GROWTH\"") != FT_SUCCESS
        || vector_cobol_main_contains_in_order(cobol_path,
            "COUNT-STRING-BYTES",
            "PERFORM CBLC-DESTRUCTOR-MAIN-CBLC-CHAIN-ARG-",
            "DISPLAY \"GLOBAL CLASS ARGUMENT\"") != FT_SUCCESS
        || vector_cobol_main_contains_in_order(cobol_path,
            "MOVE MAIN-CBLC-CHAIN-ARG-",
            "PERFORM CBLC-DESTRUCTOR-MAIN-CBLC-CHAIN-ARG-",
            "DISPLAY \"SELF-ALIAS GROWTH\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-CHAIN-ARG") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "CBLC-DESTRUCTOR-MAIN-CBLC-CHAIN-ARG") != FT_SUCCESS)
    {
        std::printf("Nested method results must be materialized before the vector call and receive scoped cleanup\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "CBLC-EX-RAISING") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "STORED-LENGTH") != FT_SUCCESS)
    {
        std::printf("Missing string-storage or exception lowering\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "DISPLAY \"RESERVE DOES NOT CONSTRUCT\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"TWO LIVE ELEMENTS\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"POP DESTROYS ONE\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"CLEAR DESTROYS REMAINDER\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"SCOPE DOES NOT DOUBLE DESTROY\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"ERASE LAST DOES NOT RELOCATE\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"ERASE LAST LIFETIME BALANCED\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"END INSERT USES SPARE CAPACITY\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"END INSERT LIFETIME BALANCED\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"VALUE RESIZE DESTROYS TAIL\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"VALUE RESIZE WHILE RECHECKS\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"VALUE RESIZE LIFETIME BALANCED\"") != FT_SUCCESS)
    {
        std::printf("Generated vector COBOL must retain exact live-element lifecycle assertions\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "DISPLAY \"RESIZE FAILURE PRESERVES ORIGINAL\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"RESIZE FAILURE CLEANUP BALANCED\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"EVERY RESIZE CONSTRUCTOR POSITION ROLLS BACK\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "CBLC-EX-RAISING") != FT_SUCCESS)
    {
        std::printf("Generated vector COBOL must retain the constructor-failure rollback checks and exception path\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "-ELEMENTS(") == FT_SUCCESS)
    {
        std::printf("Generated COBOL must access pointer-backed vector elements through BASED data items, not pointer subscripting\n");
        goto cleanup;
    }
    if (vector_cobol_contains_normalized(cobol_path,
            "SET ADDRESS OF CBLC-PTR-INT-2 TO CBLC-PTR-CURSOR") != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "MOVE CBLC-PTR-INT-2 TO CBLC-PTR-INT") != FT_SUCCESS)
    {
        std::printf("Relocation source and destination must use distinct BASED references\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "(relocation_index)") == FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "ELEMENTS(relocation_index + 1)") == FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "COMPUTE CBLC-PTR-OFFSET = (CBLC-TEMPLATE-LOCAL-") != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            ") * 4") != FT_SUCCESS)
    {
        std::printf("Pointer element offsets must use the concrete constructor index storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (status == FT_SUCCESS)
        test_remove_directory(directory);
    else
        std::printf("Generated vector<string> probe retained at %s\n", directory);
    return (status);
}

FT_TEST(test_standard_library_vector_string_indexed_string_methods)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[1024];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_string.cblc", source_path, sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_string.cob", cobol_path, sizeof(cobol_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, g_vector_string_source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector<string> indexed string methods should transpile\n");
        goto cleanup;
    }
    {
        static const char *const required_output[] = {
            "01 CBLC-SPTR-MAIN-NAMES-ELEMENTS BASED.",
            "STORED-LENGTH",
            "STORAGE-CAPACITY",
            "ALLOCATE 1 CHARACTERS RETURNING",
            "COMPUTE CBLC-PTR-OFFSET",
            "RESIZE-OVL-1"
        };
        size_t index;

        index = 0;
        while (index < sizeof(required_output) / sizeof(required_output[0]))
        {
            if (vector_cobol_contains(cobol_path, required_output[index]) != FT_SUCCESS)
            {
                std::printf("Assertion failed: indexed string COBOL is missing: %s\n",
                    required_output[index]);
                goto cleanup;
            }
            index += 1;
        }
        const int has_data_return = vector_cobol_contains_normalized(cobol_path,
                "SET CBLC-RESULT-MAIN-NAMES-CBLC-TPL-VECTOR-STRING-CBLC-USER-DATA "
                "TO MAIN-NAMES-ELEMENTS");
        const int has_data_paragraph = vector_cobol_has_paragraph(cobol_path,
            "CBLC-METHOD-MAIN-NAMES-CBLC-TPL-VECTOR-STRING-CBLC-USER-DATA");
        if (has_data_return == FT_SUCCESS || has_data_paragraph == FT_SUCCESS)
        {
            std::printf("Assertion failed: vector must not emit an unscoped raw data borrow\n");
            goto cleanup;
        }
        if (vector_cobol_excludes(cobol_path,
                "CBLC-SPTR-MAIN-NAMES-CBLC-USER-DATA-LEN") != FT_SUCCESS
            || vector_cobol_excludes(cobol_path,
                "CBLC-SPTR-MAIN-NAMES-CBLC-USER-DATA-BUF") != FT_SUCCESS)
        {
            std::printf("Assertion failed: vector<string> must not use the removed buffer-backed string layout\n");
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    if (status == FT_SUCCESS)
        test_remove_directory(directory);
    else
        std::printf("Generated indexed string probe retained at %s\n", directory);
    return (status);
}

FT_TEST(test_standard_library_vector_unscoped_data_borrow_is_rejected)
{
    static const char *source =
        "void main()\n"
        "{\n"
        "    vector<int> values;\n"
        "    int *borrowed_storage;\n"
        "    borrowed_storage = values.data();\n"
        "    return;\n"
        "}\n";
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[1024];
    int command_length;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_raw_data_rejected.cblc", source_path,
            sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_raw_data_rejected.cob", cobol_path,
            sizeof(cobol_path)) != FT_SUCCESS
        || test_write_text_file(source_path, source) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_run_command(command) == FT_SUCCESS)
    {
        std::printf("Assertion failed: vector::data() must remain unavailable until checked borrow lifetimes are implemented\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_nested_string_specialization_transpiles)
{
    const char *vector_source;
    const char *error_source;
    std::string parse_source;
    t_cblc_translation_unit parse_unit;
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char output_buffer[256];
    char command[1024];
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    status = FT_FAILURE;
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!vector_source || !error_source)
        return (FT_FAILURE);
    parse_source = std::string(error_source) + "\n" + vector_source + "\n"
        + g_vector_nested_string_source;
    cblc_translation_unit_init(&parse_unit);
    if (cblc_parse_translation_unit(parse_source.c_str(), &parse_unit)
        != FT_SUCCESS)
    {
        const size_t context_start = parse_unit.template_error_use_offset > 80
            ? parse_unit.template_error_use_offset - 80 : 0;
        size_t context_length = parse_source.size() - context_start;

        if (context_length > 200)
            context_length = 200;
        std::printf("Nested vector semantic parse failed: code=%d message=%s template=%s arguments=%s use_offset=%zu\n",
            parse_unit.parse_error_code, parse_unit.parse_error_message,
            parse_unit.template_error_name, parse_unit.template_error_arguments,
            parse_unit.template_error_use_offset);
        std::printf("Instantiation source context: %s\n",
            parse_source.substr(context_start, context_length).c_str());
        cblc_translation_unit_dispose(&parse_unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&parse_unit);
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_nested_string.cblc", source_path,
                sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_nested_string.cob", cobol_path,
                sizeof(cobol_path)) != FT_SUCCESS
        || test_join_path(directory, "nested_string.bin", binary_path,
                sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, "nested_string.txt", output_path,
                sizeof(output_path)) != FT_SUCCESS
        || test_join_path(directory, "nested_string_compile.log",
                compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, g_vector_nested_string_source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector<vector<string>> should transpile\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "STORED-LENGTH") != FT_SUCCESS)
        std::printf("Assertion failed: nested output is missing STORED-LENGTH\n");
    else if (vector_cobol_contains(cobol_path, "CBLC-TRY-") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-RAISING") != FT_SUCCESS)
        std::printf("Assertion failed: templated rollback catch/rethrow paths were not emitted\n");
    else if (vector_cobol_contains(cobol_path,
            "PERFORM CBLC-CONSTRUCTOR-MAIN-SEED-1 THRU CBLC-CRET-")
            != FT_SUCCESS)
        std::printf("Assertion failed: capacity-constructed string local must dispatch through its constructor before entering the vector lifecycle\n");
    else if (vector_cobol_has_unique_level01_names(cobol_path) != FT_SUCCESS)
        std::printf("Assertion failed: nested output contains duplicate level-01 names\n");
    else if (vector_cobol_contains(cobol_path,
            "01 CBLC-SPTR-HFF8327CEBE12D4B1 BASED.") != FT_SUCCESS)
        std::printf("Assertion failed: nested output is missing the shared vector<string> element layout\n");
    else if (vector_cobol_contains(cobol_path,
            "01 CBLC-SPTR-H53E1514516EA5068 BASED.") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "05 CBLC-SPTR-H53E1514516EA5068-ELEMENTS USAGE POINTER")
            != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "SET CBLC-PTR-CURSOR TO CBLC-SPTR-H53E1514516EA5068-ELEMENTS "
            "COMPUTE CBLC-PTR-OFFSET = "
            "(CBLC-TEMPLATE-LOCAL-4404EC1B2F93CF96) * 16 "
            "SET CBLC-PTR-CURSOR UP BY CBLC-PTR-OFFSET "
            "SET ADDRESS OF CBLC-SPTR-HFF8327CEBE12D4B1 TO CBLC-PTR-CURSOR")
            != FT_SUCCESS)
        std::printf("Assertion failed: nested vector layout must transition through the inner vector element view\n");
    else if (vector_cobol_contains(cobol_path,
            "01 CBLC-SPTR-H6137BF350A850E78 BASED.") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "05 CBLC-SPTR-H6137BF350A850E78-ELEMENTS USAGE POINTER")
            != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "SET CBLC-PTR-CURSOR TO CBLC-SPTR-H6137BF350A850E78-ELEMENTS "
            "COMPUTE CBLC-PTR-OFFSET = "
            "(CBLC-TEMPLATE-LOCAL-6BE707689C718FEE) * 16 "
            "SET CBLC-PTR-CURSOR UP BY CBLC-PTR-OFFSET "
            "SET ADDRESS OF CBLC-SPTR-H0C9A7324645D7361 TO CBLC-PTR-CURSOR")
            != FT_SUCCESS)
        std::printf("Assertion failed: five-level nested specialization must link its outer and inner vector layouts\n");
    else if (vector_cobol_contains(cobol_path,
            "SET ADDRESS OF CBLC-SPTR-H53E1514516EA5068 TO CBLC-PTR-CURSOR")
            != FT_SUCCESS)
        std::printf("Assertion failed: nested output never binds the shared inner-specialization layout\n");
    else if (vector_cobol_contains(cobol_path,
            "SET MAIN-BORROWED-NAME") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "COMPUTE CBLC-PTR-OFFSET = (MAIN-OUTER-INDEX) *") != FT_SUCCESS)
        std::printf("Assertion failed: nested at() must return a borrow to the indexed inner element\n");
    else if (vector_cobol_contains_normalized(cobol_path,
            "REALLOCATE-RELOCATION-INDEX")
            != FT_SUCCESS)
        std::printf("Assertion failed: nested reallocation must use operation-local construction progress\n");
    else if (vector_cobol_contains(cobol_path, "REALLOCATE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-CONSTRUCTOR-") != FT_SUCCESS)
        std::printf("Assertion failed: nested vector reallocation must emit its type-directed element construction path\n");
    else if (vector_cobol_contains_normalized(cobol_path,
            "CBLC-CONSTRUCTOR-CBLC-SPTR-HFF8327CEBE12D4B1-3 THRU")
            != FT_SUCCESS)
        std::printf("Assertion failed: nested push_back must invoke the element copy constructor\n");
    else if (vector_cobol_contains_normalized(cobol_path,
            "ELEMENTS(relocation-index + 1)") == FT_SUCCESS)
        std::printf("Assertion failed: pointer-backed vector elements must be addressed through a based view, not COBOL table indexing\n");
    else if (vector_cobol_has_parameter_member_element_address(cobol_path) != FT_SUCCESS)
        std::printf("Assertion failed: nested copy construction must address the source element through the copy-constructor parameter layout\n");
    else if (vector_cobol_excludes(cobol_path,
            "source.CBLC-SPTR-MAIN-GROUPS-ELEMENTS") != FT_SUCCESS)
        std::printf("Assertion failed: nested copy construction leaked a source-level name into generated COBOL\n");
    else if (vector_cobol_contains(cobol_path, "CBLC-P-") != FT_SUCCESS)
        std::printf("Assertion failed: nested push_back parameter storage is missing\n");
    else if (vector_cobol_contains_normalized(cobol_path,
            "IF CBLC-P-8594A2A22315A61A NOT = NULL "
            "SET ADDRESS OF CBLC-SPTR-P20D6A11F793934FD "
            "TO CBLC-P-8594A2A22315A61A END-IF") != FT_SUCCESS)
        std::printf("Assertion failed: move_assign must bind its source parameter view before accessing nested elements\n");
    else if (vector_cobol_contains(cobol_path,
            "VECTOR-PUSH-BACK-RELOCATION") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "CBLC-CONSTRUCTOR-CBLC-SPTR-HFF8327CEBE12D4B1-3") != FT_SUCCESS)
        std::printf("Assertion failed: nested push_back method is missing\n");
    else if (vector_cobol_contains(cobol_path,
            "VECTOR-SWAP-TEMPORARY-DATA") != FT_SUCCESS)
        std::printf("Assertion failed: nested vector swap must stage its owned element storage\n");
    else if (vector_cobol_contains(cobol_path,
            "VECTOR-ERASE-RELOCATION") != FT_SUCCESS)
        std::printf("Assertion failed: nested vector erase must stage replacement storage\n");
    else if (vector_cobol_contains(cobol_path, "CBLC-METHOD-MAIN-SEED-SWAP") != FT_SUCCESS)
        std::printf("Assertion failed: string swap method is missing\n");
    else if (vector_cobol_contains(cobol_path,
            "CBLC-USER-STRING-MOVE-FROM-SOURCE") != FT_SUCCESS)
        std::printf("Assertion failed: string move_from must lower its source-pointer path\n");
    else if (vector_cobol_paragraph_performs_defined_method(cobol_path,
            "TRANSFER-VECTOR") != FT_SUCCESS)
        std::printf("Assertion failed: pointer-receiver move_from call must target an emitted method paragraph\n");
    else if (vector_cobol_contains(cobol_path,
            "VECTOR-MOVE-ASSIGN-CBLC-BOOLEAN-RESULT") != FT_SUCCESS)
        std::printf("Assertion failed: vector<string> move assignment must lower its self-assignment check\n");
    else if (vector_cobol_contains(cobol_path,
            "CBLC-CONSTRUCTOR-MAIN-TRANSFERRED") != FT_SUCCESS)
        std::printf("Assertion failed: vector<string> move construction paragraph is missing\n");
    else if (vector_cobol_paragraph_contains_line_token(cobol_path,
            "CBLC-CONSTRUCTOR-MAIN-TRANSFERRED-4",
            "SET ADDRESS OF CBLC-SPTR-P6E9F2878B5F2DCC7") != FT_SUCCESS)
        std::printf("Assertion failed: the move-constructor pointer parameter BASED view must be rebound inside the constructor paragraph on each call\n");
    else if (vector_cobol_contains(cobol_path, "CBLC-TRY-CALL-") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "IF CBLC-EX-ACTIVE = 'Y'") != FT_SUCCESS)
        std::printf("Assertion failed: nested vector allocation must route failure through generated exception dispatch\n");
    else if (vector_cobol_contains(cobol_path, "CBLC-MRET-") != FT_SUCCESS)
        std::printf("Assertion failed: allocating vector methods need an exception return paragraph\n");
    else if (vector_cobol_contains(cobol_path, "DESTROY-INDEX") != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "FREE CBLC-SPTR-H704BE0D8FAAFFC58-CBLC-USER-DATA") != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "FREE CBLC-SPTR-HFF8327CEBE12D4B1-ELEMENTS") != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "FREE CBLC-SPTR-H53E1514516EA5068-ELEMENTS") != FT_SUCCESS)
        std::printf("Assertion failed: nested destruction must visit outer elements in reverse order, destroy inner string storage, then release inner vector storage\n");
    else if (vector_cobol_has_move_from_parameter_to(cobol_path,
            "TO MAIN-GROUPS-ELEMENTS") == FT_SUCCESS)
        std::printf("Assertion failed: nested push_back shallow-copies its element\n");
    else if (vector_cobol_excludes(cobol_path, "CBLC-SPTR-CBLC-SPTR-")
            != FT_SUCCESS)
        std::printf("Assertion failed: nested pointer layout name is double-prefixed\n");
    else
        status = FT_SUCCESS;
    if (status != FT_SUCCESS)
        goto cleanup;
    /* Structural inspection is only an intermediate gate. Keep the test
     * failed until the generated COBOL has compiled and completed at runtime. */
    status = FT_FAILURE;
    if (compile_generated_cobol(binary_path, cobol_path, compile_log_path,
            "cobc should compile nested vector<string> lifecycle COBOL")
        != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "nested_string.bin", "nested_string.txt",
            "nested vector<string> lifecycle should execute") != FT_SUCCESS
        || test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS
        || std::strcmp(output_buffer,
            "VECTOR MOVE CONSTRUCT OK\n"
            "+000000006\n+000000003\n"
            "MOVE_FROM OUTER SIZE OK\n"
            "MOVE_FROM DESTINATION SIZE OK\n"
            "MOVE_FROM SOURCE EMPTY OK\n"
            "+000000000\n"
            "MOVE_FROM VALUE OK\n"
            "NESTED VECTOR MOVE_FROM OK\n"
            "VECTOR MOVE_ASSIGN OK\n"
            "VECTOR SELF MOVE OK\n"
            "NESTED VECTOR LIFECYCLE OK\n") != 0)
    {
        std::printf("Assertion failed: nested vector<string> lifecycle should complete and emit its success marker\n");
        if (test_read_text_file(output_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
            std::printf("Nested vector<string> runtime output:\n%s\n", output_buffer);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (status == FT_SUCCESS)
        test_remove_directory(directory);
    else
        std::printf("Generated nested vector probe retained at %s\n", directory);
    return (status);
}

FT_TEST(test_standard_library_vector_throwing_element_constructor_emits_exception_path)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[1024];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_throwing_element.cblc", source_path,
                sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_throwing_element.cob", cobol_path,
                sizeof(cobol_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, g_vector_throwing_element_source)
        != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector<Throwing> should generate COBOL\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "REALLOCATE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-CONSTRUCTOR-") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "DISPLAY \"COPY DESTROY\"") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-RAISING") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-TRY-CALL-")
            != FT_SUCCESS)
    {
        std::printf("Assertion failed: throwing vector element lifecycle must emit a constructor paragraph and exception transfer path\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (status == FT_SUCCESS)
        test_remove_directory(directory);
    else
        std::printf("Generated throwing-element vector probe retained at %s\n",
            directory);
    return (status);
}

FT_TEST(test_standard_library_vector_reserve_relocates_move_only_elements)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char binary_path[256];
    char compiler_log_path[256];
    char output_path[256];
    char output_buffer[256];
    char command[1024];
    const char *source;
    int command_length;
    int status;

    FT_REQUIRE_COBC();
    source = "class MoveOnly {\n"
        "    public:\n"
        "    int value;\n"
        "    MoveOnly() { value = 7; }\n"
        "    MoveOnly(MoveOnly *source) { value = source->value; source->value = 0; }\n"
        "    ~MoveOnly() { value = 0; }\n"
        "};\n"
        "void main() { vector<MoveOnly> values; MoveOnly *borrowed; "
        "int observed_size; int observed_value; "
        "values.emplace_back(); values.emplace_back(); "
        "values.erase(0); values.resize(3); values.reserve(4); "
        "values.shrink_to_fit(); borrowed = values.at(0); "
        "observed_size = values.size(); display(observed_size); "
        "observed_value = borrowed->value; display(observed_value); "
        "values.clear(); observed_size = values.size(); "
        "display(observed_size); return; }\n";
    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    binary_path[0] = '\0';
    compiler_log_path[0] = '\0';
    output_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_move_only.cblc", source_path,
                sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_move_only.cob", cobol_path,
                sizeof(cobol_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_move_only.bin", binary_path,
                sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_move_only_compile.log",
                compiler_log_path, sizeof(compiler_log_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_move_only.txt", output_path,
                sizeof(output_path)) != FT_SUCCESS
        || test_write_text_file(source_path, source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0
        || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector<MoveOnly> append growth, erase, resize, reserve, and shrink should generate COBOL\n");
        goto cleanup;
    }
    if (vector_cobol_contains_normalized(cobol_path,
            "SET MOVEONLY-CTOR-1-SOURCE TO ADDRESS OF CBLC-SPTR-") != FT_SUCCESS
        || vector_cobol_contains_normalized(cobol_path,
            "PERFORM CBLC-CONSTRUCTOR-")
            != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "VECTOR-REALLOCATE-RELOCATION")
            != FT_SUCCESS)
    {
        std::printf("Assertion failed: reserve must capture and pass the source element address to the non-throwing move constructor\n");
        goto cleanup;
    }
    if (compile_generated_cobol(binary_path, cobol_path, compiler_log_path,
            "cobc should compile move-only vector relocation") != FT_SUCCESS)
    {
        char compiler_diagnostics[4096];

        if (test_read_text_file(compiler_log_path, compiler_diagnostics,
                sizeof(compiler_diagnostics)) == FT_SUCCESS)
            std::printf("Move-only vector COBOL compiler diagnostics:\n%s\n",
                compiler_diagnostics);
        goto cleanup;
    }
    if (execute_binary(directory, "vector_move_only.bin",
            "vector_move_only.txt",
            "move-only vector relocation should execute") != FT_SUCCESS
        || test_read_text_file(output_path, output_buffer,
            sizeof(output_buffer)) != FT_SUCCESS
        || std::strcmp(output_buffer,
            "+000000003\n+000000007\n+000000000\n") != 0)
    {
        std::printf("Move-only vector relocation did not preserve values through erase, resize, reserve, shrink_to_fit, and clear\n");
        if (test_read_text_file(output_path, output_buffer,
                sizeof(output_buffer)) == FT_SUCCESS)
            std::printf("Move-only vector runtime output:\n%s\n", output_buffer);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (status == FT_SUCCESS)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path,
            output_path, compiler_log_path);
        test_remove_directory(directory);
    }
    else
        std::printf("Generated move-only vector probe retained at %s\n",
            directory);
    return (status);
}

FT_TEST(test_standard_library_vector_reserve_rejects_throwing_move_without_copy_constructor)
{
    const char *vector_source;
    const char *error_source;
    const char *fixture_source;
    std::string source;
    t_cblc_translation_unit unit;
    const t_cblc_translation_unit *units[1];
    const char *paths[1];
    t_transpiler_parallel_result *parallel_results;
    char *generated_cobol;
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int diagnostic_code;

    fixture_source = "class ThrowingHelper {\n"
        "    public:\n"
        "    ThrowingHelper() { throw 9; }\n"
        "};\n"
        "class MoveMayThrow {\n"
        "    public:\n"
        "    int value;\n"
        "    void fail_move() { ThrowingHelper local; }\n"
        "    MoveMayThrow() { value = 7; }\n"
        "    MoveMayThrow(MoveMayThrow *source) { value = source->value; "
        "source->value = 0; if (value == 9) { fail_move(); } }\n"
        "    void operator=(MoveMayThrow source) { value = source.value; }\n"
        "};\n"
        "void main() { vector<MoveMayThrow> values; values.emplace_back(); "
        "values.reserve(4); return; }\n";
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!vector_source || !error_source)
        return (FT_FAILURE);
    source = std::string(error_source) + "\n" + vector_source + "\n" + fixture_source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    parallel_results = NULL;
    diagnostic_code = 0;
    diagnostic[0] = '\0';
    if (cblc_parse_translation_unit(source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: throwing move-only vector fixture should parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic, sizeof(diagnostic)) == FT_SUCCESS
        || diagnostic_code != TRANSPILE_ERROR_VECTOR_THROWING_MOVE_RELOCATION
        || !std::strstr(diagnostic,
            "vector reallocation requires a copy constructor or a non-throwing move constructor")
        || !std::strstr(diagnostic, "MoveMayThrow"))
    {
        std::printf("Assertion failed: throwing move-only reallocation should produce the dedicated capability diagnostic; code=%d message=%s\n",
            diagnostic_code, diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    units[0] = &unit;
    paths[0] = "throwing_move_only_vector.cblc";
    if (transpiler_parallel_generate_cobol(units, paths, 1,
            &parallel_results) != FT_FAILURE
        || !parallel_results
        || parallel_results[0].status != FT_FAILURE
        || !std::strstr(parallel_results[0].error_message,
            "vector reallocation requires a copy constructor or a non-throwing move constructor")
        || !std::strstr(parallel_results[0].error_message, paths[0]))
    {
        std::printf("Assertion failed: parallel compiler path should preserve the specific relocation diagnostic; results=%d status=%d message=%s\n",
            parallel_results != NULL,
            parallel_results ? parallel_results[0].status : 0,
            parallel_results ? parallel_results[0].error_message : "<none>");
        if (parallel_results)
            transpiler_parallel_results_dispose(parallel_results, 1);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    transpiler_parallel_results_dispose(parallel_results, 1);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_reserve_copies_when_move_may_throw)
{
    const char *vector_source;
    const char *error_source;
    const char *fixture_source;
    const t_cblc_struct_type *element_type;
    const t_cblc_constructor *copy_constructor;
    const t_cblc_constructor *move_constructor;
    std::string source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int generation_diagnostic_code;
    size_t type_index;
    size_t constructor_index;

    fixture_source = "class ThrowingHelper {\n"
        "    public:\n"
        "    ThrowingHelper() { throw 9; }\n"
        "};\n"
        "class CopyPreferred {\n"
        "    public:\n"
        "    int value;\n"
        "    void fail_move() { ThrowingHelper local; }\n"
        "    CopyPreferred() { value = 7; }\n"
        "    CopyPreferred(CopyPreferred source) { value = source.value; }\n"
        "    CopyPreferred(CopyPreferred *source) { value = source->value; "
        "source->value = 0; fail_move(); }\n"
        "};\n"
        "void main() { vector<CopyPreferred> values; values.emplace_back(); "
        "values.reserve(4); return; }\n";
    vector_source = transpiler_standard_library_get_native_source("CBLC-VECTOR");
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!vector_source || !error_source)
        return (FT_FAILURE);
    source = std::string(error_source) + "\n" + vector_source + "\n" + fixture_source;
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    generation_diagnostic[0] = '\0';
    generation_diagnostic_code = 0;
    if (cblc_parse_translation_unit(source.c_str(), &unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: copy-preferred vector fixture should parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    element_type = NULL;
    type_index = 0;
    while (type_index < unit.struct_type_count)
    {
        if (std::strcmp(unit.struct_types[type_index].source_name,
                "CopyPreferred") == 0)
        {
            element_type = &unit.struct_types[type_index];
            break ;
        }
        type_index += 1;
    }
    copy_constructor = NULL;
    move_constructor = NULL;
    if (element_type)
    {
        constructor_index = 0;
        while (constructor_index < element_type->constructor_count)
        {
            if (element_type->constructors[constructor_index].is_copy_constructor)
                copy_constructor = &element_type->constructors[constructor_index];
            if (element_type->constructors[constructor_index].is_move_constructor)
                move_constructor = &element_type->constructors[constructor_index];
            constructor_index += 1;
        }
    }
    if (!copy_constructor || !move_constructor)
    {
        std::printf("Assertion failed: copy/move constructor metadata was not retained\n");
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("Assertion failed: copy-preferred vector should generate COBOL: %s (code=%d diagnostic=%s; copy kind=%d type=%s move kind=%d type=%s)\n",
            unit.parse_error_message, generation_diagnostic_code,
            generation_diagnostic, copy_constructor->parameters[0].kind,
            copy_constructor->parameters[0].type_name,
            move_constructor->parameters[0].kind,
            move_constructor->parameters[0].type_name);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    {
        const size_t copy_constructor_index = static_cast<size_t>(
            copy_constructor - element_type->constructors);
        const size_t move_constructor_index = static_cast<size_t>(
            move_constructor - element_type->constructors);
        std::string normalized_cobol;
        const char *generated_cursor = generated_cobol;
        int previous_was_space = 0;
        const std::string copy_constructor_suffix = "-"
            + std::to_string(copy_constructor_index);
        const std::string move_constructor_suffix = "-"
            + std::to_string(move_constructor_index);
        bool performs_copy_constructor = false;
        bool performs_move_constructor = false;

        while (*generated_cursor != '\0')
        {
            const unsigned char character =
                static_cast<unsigned char>(*generated_cursor);

            if (std::isspace(character))
            {
                if (!previous_was_space)
                    normalized_cobol.push_back(' ');
                previous_was_space = 1;
            }
            else
            {
                normalized_cobol.push_back(static_cast<char>(character));
                previous_was_space = 0;
            }
            generated_cursor += 1;
        }
        {
            std::string::size_type search_position = 0;
            const std::string dispatch_prefix = "PERFORM CBLC-CON";

            while ((search_position = normalized_cobol.find(dispatch_prefix,
                        search_position)) != std::string::npos)
            {
                const std::string::size_type name_start = search_position
                    + dispatch_prefix.size();
                const std::string::size_type name_end =
                    normalized_cobol.find_first_of(" .", name_start);

                if (name_end == std::string::npos)
                    break;
                const std::string paragraph_name = normalized_cobol.substr(
                    name_start, name_end - name_start);
                if (paragraph_name.size() >= copy_constructor_suffix.size()
                    && paragraph_name.compare(paragraph_name.size()
                            - copy_constructor_suffix.size(),
                        copy_constructor_suffix.size(),
                        copy_constructor_suffix) == 0)
                    performs_copy_constructor = true;
                if (paragraph_name.size() >= move_constructor_suffix.size()
                    && paragraph_name.compare(paragraph_name.size()
                            - move_constructor_suffix.size(),
                        move_constructor_suffix.size(),
                        move_constructor_suffix) == 0)
                    performs_move_constructor = true;
                search_position = name_end + 1;
            }
        }
        if (!performs_copy_constructor || performs_move_constructor)
        {
            std::printf("Assertion failed: vector reallocation should dispatch the copy constructor and not the throwing move constructor (copy=%d move=%d)\n",
                performs_copy_constructor, performs_move_constructor);
            if (generated_cobol)
                cma_free(generated_cobol);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_standard_library_vector_template_depth_boundary)
{
    static const size_t chain_depths[] = {
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
    };
    const size_t supported_depth = CBLC_TEMPLATE_DEFAULT_MAX_DEPTH;
    const char *vector_source =
        transpiler_standard_library_get_native_source("CBLC-VECTOR");
    const char *error_source =
        transpiler_standard_library_get_native_source("CBLC-ERRORS");
    size_t generated_sizes[sizeof(chain_depths) / sizeof(chain_depths[0])] = {};
    std::string nested_type;
    std::string level_types[17];
    std::string source;
    t_cblc_translation_unit unit;
    const int trace_depth = std::getenv("CTOC_TRACE_TEMPLATE_DEPTH") != NULL;
    const char *selected_depth_setting =
        std::getenv("CTOC_TEST_TEMPLATE_DEPTH_ONLY");
    std::chrono::steady_clock::time_point phase_started;
    size_t index;
    size_t selected_depth;

    nested_type.reserve(supported_depth * 16 + 8);
    selected_depth = 0;
    if (selected_depth_setting && selected_depth_setting[0] != '\0')
    {
        char *parse_end = NULL;
        const unsigned long parsed_depth = std::strtoul(
            selected_depth_setting, &parse_end, 10);

        if (!parse_end || *parse_end != '\0' || parsed_depth == 0
            || parsed_depth > supported_depth
            || parsed_depth > sizeof(chain_depths) / sizeof(chain_depths[0]))
        {
            std::printf("CTOC_TEST_TEMPLATE_DEPTH_ONLY must be an integer from 1 through %zu\n",
                supported_depth);
            return (FT_FAILURE);
        }
        selected_depth = static_cast<size_t>(parsed_depth);
    }
    /* Keep an initialized string object in the fixture: merely naming string
     * as the leaf type of an empty nested vector does not require its
     * constructor or UTF-8 validation path to be emitted. */
    for (index = 0; index < supported_depth; ++index)
        nested_type += "vector<";
    nested_type += "string";
    for (index = 0; index < supported_depth; ++index)
        nested_type += ">";
    source = "void main() {\n    string leaf(\"nested\");\n    " + nested_type
        + " value;\n}\n";

    cblc_translation_unit_init(&unit);
    if (test_expect_int_equal(static_cast<int>(unit.template_max_instantiation_depth),
            static_cast<int>(supported_depth),
            "default template nesting limit is the published depth") != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!vector_source || !error_source)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    source = std::string(error_source) + "\n" + vector_source + "\n" + source;
    phase_started = std::chrono::steady_clock::now();
    if (test_expect_success(cblc_parse_translation_unit(source.c_str(), &unit),
            "the maximum supported nested vector<string> type parses") != FT_SUCCESS)
    {
        std::printf("Depth-%zu template parse diagnostic (code=%d, instances=%zu): %s\n",
            supported_depth, unit.parse_error_code,
            unit.template_instantiation_count, unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (trace_depth)
        std::fprintf(stderr, "template depth %zu baseline parse: %.3f s\n",
            supported_depth, std::chrono::duration<double>(
                std::chrono::steady_clock::now() - phase_started).count());
    if (test_expect_int_equal(static_cast<int>(unit.template_instantiation_count),
            static_cast<int>(supported_depth),
            "each nested vector layer receives a concrete specialization") != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    {
        const t_cblc_struct_type *vector_of_string;
        size_t matching_specialization_count;
        size_t type_index;
        int all_specializations_use_class_storage;

        vector_of_string = NULL;
        matching_specialization_count = 0;
        all_specializations_use_class_storage = 1;
        type_index = 0;
        while (type_index < unit.struct_type_count)
        {
            if (std::strcmp(unit.struct_types[type_index].source_name,
                    "vector__string") == 0)
            {
                const t_cblc_struct_type *candidate = &unit.struct_types[type_index];

                matching_specialization_count += 1;
                if (!vector_of_string)
                    vector_of_string = candidate;
                if (candidate->field_count != 3
                    || candidate->fields[0].kind != CBLC_DATA_KIND_STRUCT_POINTER
                    || std::strcmp(candidate->fields[0].struct_type_name,
                        "string") != 0
                    || std::strcmp(candidate->fields[1].source_name, "count") != 0
                    || std::strcmp(candidate->fields[2].source_name, "reserved") != 0)
                    all_specializations_use_class_storage = 0;
                else
                {
                    size_t field_index = 1;

                    while (field_index < candidate->field_count)
                    {
                        if (candidate->fields[field_index].kind
                            == CBLC_DATA_KIND_STRUCT_POINTER)
                            all_specializations_use_class_storage = 0;
                        field_index += 1;
                    }
                }
            }
            type_index += 1;
        }
        if (!vector_of_string || matching_specialization_count != 1
            || !all_specializations_use_class_storage)
        {
            std::printf("vector<string> specialization is missing or duplicated: type=%s matches=%zu fields=%zu first-kind=%d first-type=%s\n",
                vector_of_string ? vector_of_string->source_name : "<missing>",
                matching_specialization_count,
                vector_of_string ? vector_of_string->field_count : 0,
                vector_of_string && vector_of_string->field_count > 0
                    ? static_cast<int>(vector_of_string->fields[0].kind) : -1,
                vector_of_string && vector_of_string->field_count > 0
                    ? vector_of_string->fields[0].struct_type_name : "<missing>");
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
    }
    cblc_translation_unit_dispose(&unit);

    for (size_t depth_index = 0;
        depth_index < sizeof(chain_depths) / sizeof(chain_depths[0]);
        ++depth_index)
    {
        const size_t depth = chain_depths[depth_index];
        char *generated_cobol = NULL;
        int generation_diagnostic_code = 0;
        char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX] = {};
        std::string previous_type;
        size_t level;

        if (selected_depth != 0 && depth != selected_depth)
            continue ;
        if (depth > supported_depth)
        {
            std::printf("Configured template nesting limit %zu is below required proof depth %zu\n",
                supported_depth, depth);
            return (FT_FAILURE);
        }

        source.clear();
        source += "void main() {\nstring leaf(\"0\");\n"
            "string copied_leaf(leaf);\nint copy_probe_ok;\n"
            "string *borrowed_leaf;\nint erase_chain_size;\n"
            "string assignment_suffix(\"1\");\n"
            "int nested_copy_assignment_ok;\n"
            "int empty_bounds_ok;\n"
            "int populated_bounds_ok;\n"
            "int layer_capacity_before;\n"
            "int layer_capacity_after;\n"
            "int layer_reserve_request;\n"
            "int layer_size_before;\n"
            "int layer_check_ok;\n"
            "int reserve_lifecycle_ok;\n"
            "int leaf_codepoint;\n"
            "int branch_index;\n"
            "int invalid_insert_index;\n"
            "int nested_edge_ok;\n"
            "int nested_self_assignment_ok;\n"
            "string edge_leaf(\"7\");\n";
        previous_type = "string";
        level = 1;
        while (level <= depth)
        {
            level_types[level] = "vector<" + previous_type + ">";
            source += level_types[level] + " level"
                + std::to_string(level) + ";\n";
            source += level_types[level] + " edge_probe"
                + std::to_string(level) + ";\n";
            previous_type = level_types[level];
            level += 1;
        }
        source += previous_type + " assigned_level;\n";
        source += "edge_probe1.push_back(edge_leaf);\n";
        for (level = 2; level <= depth; ++level)
            source += "edge_probe" + std::to_string(level)
                + ".push_back(edge_probe" + std::to_string(level - 1)
                + ");\n";
        source += "nested_self_assignment_ok = 1;\n";
        for (level = 1; level <= depth; ++level)
        {
            const std::string probe = "edge_probe" + std::to_string(level);

            source += probe + " = " + probe + ";\n"
                + probe + " = " + probe + ";\n"
                "if (" + probe
                + ".size() != 1) { nested_self_assignment_ok = 0; }\n";
        }
        source += "borrowed_leaf = edge_probe" + std::to_string(depth)
            + ".front()";
        for (index = 1; index < depth; ++index)
            source += "->at(0)";
        source += ";\n"
            "if (borrowed_leaf->len() != 1) { nested_self_assignment_ok = 0; }\n"
            "if (borrowed_leaf->parse_int() != 7) { nested_self_assignment_ok = 0; }\n";
        for (level = 1; level <= depth; ++level)
        {
            source += "if (nested_self_assignment_ok == 1) { display(\"NESTED SELF ASSIGN LEVEL "
                + std::to_string(level)
                + " OK\"); } else { display(\"NESTED SELF ASSIGN LEVEL "
                + std::to_string(level) + " FAILED\"); }\n";
        }
        source += "nested_edge_ok = 1;\n";
        for (level = 1; level <= depth; ++level)
        {
            const std::string probe = "edge_probe" + std::to_string(level);

            source += probe + ".insert(0, " + probe
                + ".at_value(0));\n"
                "if (" + probe
                + ".size() != 2) { nested_edge_ok = 0; }\n"
                + probe + ".insert(" + probe + ".size(), " + probe
                + ".at_value(0));\n"
                "if (" + probe
                + ".size() != 3) { nested_edge_ok = 0; }\n"
                + probe + ".erase(2);\n"
                "if (" + probe
                + ".size() != 2) { nested_edge_ok = 0; }\n"
                "if (nested_edge_ok == 1) { display(\"NESTED EDGE END INSERT ERASE LEVEL "
                + std::to_string(level)
                + " OK\"); } else { display(\"NESTED EDGE END INSERT ERASE LEVEL "
                + std::to_string(level) + " FAILED\"); }\n"
                + probe + ".erase(0);\n"
                "if (" + probe
                + ".size() != 1) { nested_edge_ok = 0; }\n"
                + probe + ".clear();\n"
                "if (" + probe
                + ".size() != 0) { nested_edge_ok = 0; }\n"
                + probe + ".clear();\n"
                "if (" + probe
                + ".size() != 0) { nested_edge_ok = 0; }\n"
                "if (nested_edge_ok == 1) { display(\"NESTED EDGE RESET LEVEL "
                + std::to_string(level)
                + " OK\"); } else { display(\"NESTED EDGE RESET LEVEL "
                + std::to_string(level) + " FAILED\"); }\n";
        }
        source += "edge_probe1.push_back(edge_leaf);\n";
        for (level = 2; level <= depth; ++level)
            source += "edge_probe" + std::to_string(level)
                + ".push_back(edge_probe" + std::to_string(level - 1)
                + ");\n";
        source += "nested_edge_ok = 1;\n";
        /* Exercise value-taking operations with an element borrowed from the
         * same vector. At depths > 1 each element is itself a class, so these
         * calls also cover nested copy construction during assign and resize
         * growth, plus shrink preservation at every concrete specialization. */
        for (level = 1; level <= depth; ++level)
        {
            const std::string probe = "edge_probe" + std::to_string(level);

            source += probe + ".assign(2, " + probe + ".at_value(0));\n"
                "if (" + probe
                + ".size() != 2) { nested_edge_ok = 0; }\n"
                + probe + ".resize(1);\n"
                "if (" + probe
                + ".size() != 1) { nested_edge_ok = 0; }\n"
                + probe + ".resize(2, " + probe + ".at_value(0));\n"
                "if (" + probe
                + ".size() != 2) { nested_edge_ok = 0; }\n"
                + probe + ".assign(1, " + probe + ".at_value(0));\n"
                "if (" + probe
                + ".size() != 1) { nested_edge_ok = 0; }\n";
        }
        for (level = 1; level <= depth; ++level)
            source += "if (edge_probe" + std::to_string(level)
                + ".size() != 1) { nested_edge_ok = 0; }\n";
        source += "borrowed_leaf = edge_probe" + std::to_string(depth)
            + ".front()";
        for (index = 1; index < depth; ++index)
            source += "->at(0)";
        source += ";\n"
            "if (borrowed_leaf->parse_int() != 7) { nested_edge_ok = 0; }\n"
            "if (nested_edge_ok == 1) { display(\"NESTED EDGE REUSE OK\"); }\n"
            "else { display(\"NESTED EDGE REUSE FAILED\"); }\n";
        for (level = 1; level < depth; ++level)
            source += level_types[level] + " *erase_chain"
                + std::to_string(level) + ";\n";
        if (depth == 16)
        {
            for (level = 1; level < depth; ++level)
                source += level_types[level] + " *chain" + std::to_string(level)
                    + ";\n";
        }
        source += "empty_bounds_ok = 1;\n";
        for (level = 1; level <= depth; ++level)
        {
            const std::string level_name = "level" + std::to_string(level);
            const std::string borrowed_target = level == 1
                ? "borrowed_leaf" : "erase_chain" + std::to_string(level - 1);

            source += "try { " + borrowed_target + " = " + level_name
                + ".at(-1); empty_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"EMPTY AT NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + borrowed_target + " = " + level_name
                + ".at(0); empty_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"EMPTY AT END LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name
                + ".erase(0); empty_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"EMPTY ERASE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name
                + ".pop_back(); empty_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"EMPTY POP BACK LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name
                + ".resize(-1); empty_bounds_ok = 0; }\n"
                "catch (const invalid_argument& error) { display(\"EMPTY RESIZE NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "if (" + level_name
                + ".size() != 0) { empty_bounds_ok = 0; }\n"
                "display(\"EMPTY BOUNDS STATE LEVEL "
                + std::to_string(level) + "\");\n"
                "erase_chain_size = " + level_name + ".size();\n"
                "display(erase_chain_size);\n";
        }
        source += "display(\"EMPTY NESTED BOUNDS\");\n"
            "display(empty_bounds_ok);\n"
            "display(\"LEAF CONSTRUCTED\");\n"
            "int parsed_leaf;\nparsed_leaf = leaf.parse_int();\n"
            "display(\"LEAF PARSED\");\n"
            "copy_probe_ok = 1;\n"
            "leaf_codepoint = copied_leaf.codepoint_at(0);\n"
            "if (leaf_codepoint != 48) { copy_probe_ok = 0; }\n"
            "display(\"STRING COPY CHECK\");\n"
            "display(copy_probe_ok);\n";
        source += "level1.push_back(leaf);\n";
        if (depth == 16)
            source += "level1.push_back(level1.at_value(0));\n"
                "display(\"NESTED LEVEL 1 REALLOCATED\");\n"
                "display(\"NESTED LEVEL 1 INSERTED\");\n";
        for (level = 2; level <= depth; ++level)
        {
            source += "level" + std::to_string(level) + ".push_back(level"
                + std::to_string(level - 1) + ");\n";
            if (depth == 16)
                source += "display(\"NESTED LEVEL " + std::to_string(level)
                    + " INSERTED\");\n";
        }
        if (depth == 16)
            source += "display(\"BEFORE NESTED RETURN COPY\");\n";
        source += "level" + std::to_string(depth) + ".push_back(level"
            + std::to_string(depth) + ".at_value(0));\n";
        if (depth == 16)
            source += "display(\"AFTER NESTED RETURN COPY\");\n";
        source += "assigned_level = level" + std::to_string(depth) + ";\n"
            "borrowed_leaf = assigned_level.front()";
        for (index = 1; index < depth; ++index)
            source += "->at(0)";
        source += ";\nborrowed_leaf->append(assignment_suffix);\n"
            "borrowed_leaf = level" + std::to_string(depth) + ".front()";
        for (index = 1; index < depth; ++index)
            source += "->at(0)";
        source += ";\nnested_copy_assignment_ok = 1;\n"
            "if (borrowed_leaf->len() != 1) { nested_copy_assignment_ok = 0; }\n"
            "if (borrowed_leaf->parse_int() != 0) { nested_copy_assignment_ok = 0; }\n"
            "display(\"NESTED COPY ASSIGNMENT INDEPENDENCE\");\n"
            "display(nested_copy_assignment_ok);\n";
        if (depth == 16)
        {
            source += "chain15 = level16.front();\n"
                "display(\"CHAIN STEP 1\");\n";
            for (level = 15; level > 1; --level)
            {
                source += "chain" + std::to_string(level - 1) + " = chain"
                    + std::to_string(level) + "->at(0);\n"
                    "display(\"CHAIN STEP "
                    + std::to_string(17 - level) + "\");\n";
            }
            source += "borrowed_leaf = chain1->at(0);\n"
                "display(\"CHAIN STEP 16\");\n";
        }
        if (depth == 16)
            source += "display(\"BEFORE DIRECT CHAIN\");\n";
        source += "borrowed_leaf = level" + std::to_string(depth) + ".front()";
        for (index = 1; index < depth; ++index)
            source += "->at(0)";
        source += ";\n";
        source += "populated_bounds_ok = 1;\n";
        for (level = 1; level <= depth; ++level)
        {
            const std::string level_name = "level" + std::to_string(level);
            const std::string borrowed_target = level == 1
                ? "borrowed_leaf" : "erase_chain" + std::to_string(level - 1);

            source += "erase_chain_size = " + level_name + ".size();\n"
                "try { " + borrowed_target + " = " + level_name
                + ".at(-1); populated_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"POPULATED AT NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + borrowed_target + " = " + level_name
                + ".at(erase_chain_size); populated_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"POPULATED AT END LEVEL "
                + std::to_string(level) + "\"); }\n"
                "if (" + level_name
                + ".size() != erase_chain_size) { populated_bounds_ok = 0; }\n";
            source += "invalid_insert_index = erase_chain_size + 1;\n"
                "try { " + level_name + ".insert(-1, " + level_name
                + ".at_value(0)); populated_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"POPULATED INSERT NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name + ".insert(invalid_insert_index, "
                + level_name + ".at_value(0)); populated_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"POPULATED INSERT END LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name
                + ".erase(-1); populated_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"POPULATED ERASE NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name
                + ".erase(erase_chain_size); populated_bounds_ok = 0; }\n"
                "catch (const out_of_range& error) { display(\"POPULATED ERASE END LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name
                + ".resize(-1); populated_bounds_ok = 0; }\n"
                "catch (const invalid_argument& error) { display(\"POPULATED RESIZE NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "try { " + level_name + ".assign(-1, " + level_name
                + ".at_value(0)); populated_bounds_ok = 0; }\n"
                "catch (const invalid_argument& error) { display(\"POPULATED ASSIGN NEGATIVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "if (" + level_name
                + ".size() != erase_chain_size) { populated_bounds_ok = 0; }\n"
                "display(\"POPULATED BOUNDS STATE LEVEL "
                + std::to_string(level) + "\");\n"
                "erase_chain_size = " + level_name + ".size();\n"
                "display(erase_chain_size);\n";
        }
        source += "display(\"POPULATED NESTED BOUNDS\");\n"
            "display(populated_bounds_ok);\n";
        source += "reserve_lifecycle_ok = 1;\n";
        for (level = 1; level <= depth; ++level)
        {
            const std::string level_name = "level" + std::to_string(level);

            source += "layer_check_ok = 1;\n"
                "layer_capacity_before = " + level_name
                + ".capacity();\n"
                "layer_reserve_request = layer_capacity_before + 1;\n"
                "layer_size_before = " + level_name + ".size();\n"
                + level_name + ".reserve(layer_reserve_request);\n"
                "if (" + level_name
                + ".capacity() < layer_reserve_request) { layer_check_ok = 0; }\n"
                "if (" + level_name
                + ".size() != layer_size_before) { layer_check_ok = 0; }\n"
                "layer_capacity_after = " + level_name + ".capacity();\n"
                "try { " + level_name + ".reserve(-1); layer_check_ok = 0; }\n"
                "catch (const invalid_argument& error) { display(\"NESTED NEGATIVE RESERVE LEVEL "
                + std::to_string(level) + "\"); }\n"
                "if (" + level_name
                + ".capacity() != layer_capacity_after) { layer_check_ok = 0; }\n"
                "if (" + level_name
                + ".size() != layer_size_before) { layer_check_ok = 0; }\n"
                "if (layer_check_ok == 1) { display(\"NESTED RESERVE LEVEL "
                + std::to_string(level) + " OK\"); }\n"
                "else { reserve_lifecycle_ok = 0; display(\"NESTED RESERVE LEVEL "
                + std::to_string(level) + " FAILED\"); }\n";
        }
        source += "borrowed_leaf = level" + std::to_string(depth) + ".front()";
        for (index = 1; index < depth; ++index)
            source += "->at(0)";
        source += ";\n"
            "if (borrowed_leaf->len() != 1) { reserve_lifecycle_ok = 0; }\n"
            "if (borrowed_leaf->parse_int() != 0) { reserve_lifecycle_ok = 0; }\n"
            "if (reserve_lifecycle_ok == 1) { display(\"NESTED RESERVE LIFECYCLE OK\"); }\n"
            "else { display(\"NESTED RESERVE LIFECYCLE FAILED\"); }\n";
        if (depth == 16)
            source += "display(\"AFTER DIRECT CHAIN\");\n";
        if (depth == 16)
        {
            source += "int depth16_ok;\nint depth16_capacity;\ndepth16_ok = copy_probe_ok;\n"
                "display(\"BEFORE SUFFIX CONSTRUCT\");\n"
                "string depth16_suffix(\"1\");\n"
                "display(\"AFTER SUFFIX CONSTRUCT\");\n"
                "depth16_ok = 1;\n";
            for (level = 1; level < depth; ++level)
                source += "if (level" + std::to_string(level)
                    + ".size() != "
                    + (level == 1 ? "2" : "1")
                    + ") { depth16_ok = 0; }\n"
                    "display(\"SIZE CHECK LEVEL " + std::to_string(level)
                    + "\");\n";
            source += "if (level16.size() != 2) { depth16_ok = 0; }\n"
                "display(\"SIZE CHECK LEVEL 16\");\n"
                "display(\"NESTED VALUE COPIED\");\n"
                "display(\"BORROWED LEAF RESOLVED\");\n"
                "borrowed_leaf->append(depth16_suffix);\n"
                "display(\"NESTED VALUE MUTATED\");\n"
                "if (borrowed_leaf->len() != 2) { depth16_ok = 0; }\n"
                "if (borrowed_leaf->parse_int() != 1) { depth16_ok = 0; }\n"
                "if (depth16_ok == 1) { display(\"NESTED VECTOR DEPTH 16 OK\"); }\n"
                "else { display(\"NESTED VECTOR DEPTH 16 FAILED\"); }\n"
                "depth16_capacity = level16.capacity();\n"
                "level16.clear();\n"
                "if (level16.size() == 0 && level16.capacity() == depth16_capacity && level15.size() == 1) { display(\"DEPTH 16 CLEAR PRESERVES SOURCE AND CAPACITY\"); }\n"
                "else { display(\"DEPTH 16 CLEAR PRESERVES SOURCE FAILED\"); depth16_ok = 0; }\n"
                "level16.clear();\n"
                "if (level16.size() == 0 && level16.capacity() == depth16_capacity) { display(\"DEPTH 16 REPEATED CLEAR\"); }\n"
                "else { display(\"DEPTH 16 REPEATED CLEAR FAILED\"); depth16_ok = 0; }\n"
                "level16.push_back(level15);\n"
                "borrowed_leaf = level16.front()";
            for (index = 1; index < depth; ++index)
                source += "->at(0)";
            source += ";\n"
                "if (level16.size() != 1 || level16.capacity() != depth16_capacity || borrowed_leaf->len() != 1 || borrowed_leaf->parse_int() != 0) { depth16_ok = 0; }\n"
                "if (depth16_ok == 1) { display(\"DEPTH 16 REUSE AFTER CLEAR\"); }\n"
                "else { display(\"DEPTH 16 REUSE AFTER CLEAR FAILED\"); }\n"
                "level16.pop_back();\n"
                "if (level16.size() == 0 && level16.capacity() == depth16_capacity && level15.size() == 1) { display(\"DEPTH 16 POP BACK AFTER REUSE\"); }\n"
                "else { display(\"DEPTH 16 POP BACK AFTER REUSE FAILED\"); }\n";
        }
        else
        {
            source += "int nested_depth_ok;\n"
                "nested_depth_ok = copy_probe_ok;\n";
            for (level = 1; level < depth; ++level)
                source += "level" + std::to_string(level)
                    + ".push_back(level" + std::to_string(level)
                    + ".at_value(0));\n";
            for (level = 1; level <= depth; ++level)
                source += "if (level" + std::to_string(level)
                    + ".size() != 2) { nested_depth_ok = 0; }\n";
            source += "display(\"NESTED SIZE TWO CHECK\");\n"
                "display(nested_depth_ok);\n";
            for (level = 1; level <= depth; ++level)
                source += "level" + std::to_string(level)
                    + ".push_back(level" + std::to_string(level)
                    + ".at_value(0));\n";
            source += "borrowed_leaf = level" + std::to_string(depth)
                + ".front()";
            for (index = 1; index < depth; ++index)
                source += "->at(0)";
            source += ";\n";
            for (level = 1; level <= depth; ++level)
                source += "if (level" + std::to_string(level)
                    + ".size() != 3) { nested_depth_ok = 0; }\n";
            source += "if (borrowed_leaf->len() != 1) { nested_depth_ok = 0; }\n"
                "if (borrowed_leaf->parse_int() != 0) { nested_depth_ok = 0; }\n";
            source += "display(\"NESTED VALUE CHECK\");\n"
                "display(nested_depth_ok);\n";
            source += "branch_index = 0;\nwhile (branch_index < 3) {\n"
                "borrowed_leaf = level1.at(branch_index);\n"
                "try { erase_chain_size = borrowed_leaf->parse_int(); }\n"
                "catch (...) { nested_depth_ok = 0; }\n"
                "if (erase_chain_size != 0) { nested_depth_ok = 0; }\n"
                "branch_index = branch_index + 1;\n}\n";
            for (level = 2; level <= depth; ++level)
            {
                source += "branch_index = 0;\nwhile (branch_index < 3) {\n"
                    "erase_chain" + std::to_string(level - 1) + " = level"
                    + std::to_string(level) + ".at(branch_index);\n"
                    "erase_chain_size = erase_chain"
                    + std::to_string(level - 1) + "->size();\n"
                    "display(\"PREERASE LEVEL " + std::to_string(level)
                    + " BRANCH\");\ndisplay(branch_index);\n"
                    "display(erase_chain_size);\n"
                    "if (erase_chain_size != 1) { nested_depth_ok = 0; }\n"
                    "branch_index = branch_index + 1;\n}\n";
            }
            source += "if (nested_depth_ok == 1) { display(\"NESTED VECTOR DEPTH "
                + std::to_string(depth)
                + " OK\"); } else { display(\"NESTED VECTOR DEPTH "
                + std::to_string(depth) + " FAILED\"); }\n";
        }
        /* The depth-16 scenario deliberately keeps a single live path in
         * levels 2..15, so it cannot erase index 0 and then borrow index 0
         * from each level as the depth-4/5 three-branch erase scenario does. */
        for (level = depth; depth != 16 && level > 0; --level)
        {
            const std::string level_name = "level" + std::to_string(level);
            const std::string check_name = depth == 16
                ? "depth16_ok" : "nested_depth_ok";

            source += level_name + ".erase(0);\n"
                "display(\"NESTED ERASE LEVEL " + std::to_string(level)
                + " RETURNED\");\n"
                "if (" + level_name + ".size() != 2) { "
                + check_name + " = 0; }\n"
                "display(\"NESTED ERASE LEVEL " + std::to_string(level)
                + " SIZE CHECKED\");\n";
            if (level == 1)
                source += "borrowed_leaf = " + level_name + ".at(0);\n";
            else
            {
                source += "erase_chain" + std::to_string(level - 1) + " = "
                    + level_name + ".at(0);\n"
                    "display(\"NESTED ERASE BORROW LEVEL "
                    + std::to_string(level - 1) + "\");\n"
                    "erase_chain_size = erase_chain" + std::to_string(level - 1)
                    + "->size();\n"
                    "display(erase_chain_size);\n";
                for (index = level - 1; index > 1; --index)
                    source += "erase_chain" + std::to_string(index - 1)
                        + " = erase_chain" + std::to_string(index)
                        + "->at(0);\ndisplay(\"NESTED ERASE BORROW LEVEL "
                        + std::to_string(index - 1) + "\");\n"
                        "erase_chain_size = erase_chain"
                        + std::to_string(index - 1)
                        + "->size();\ndisplay(erase_chain_size);\n";
                source += "borrowed_leaf = erase_chain1->at(0);\n";
            }
            source += "display(\"NESTED ERASE LEVEL "
                + std::to_string(level) + " BORROWED STRING\");\n"
                "if (borrowed_leaf->len() != 1) { " + check_name
                + " = 0; }\ndisplay(\"NESTED ERASE LEVEL "
                + std::to_string(level) + " LENGTH CHECKED\");\n"
                "leaf_codepoint = borrowed_leaf->codepoint_at(0);\n"
                "display(leaf_codepoint);\n"
                "if (leaf_codepoint != 48) { " + check_name + " = 0; }\n"
                "try { erase_chain_size = borrowed_leaf->parse_int(); }\n"
                "catch (...) { " + check_name
                + " = 0; display(\"NESTED ERASE PARSE ERROR\"); }\n"
                "display(\"NESTED ERASE PARSE CHECKED\");\n"
                "display(erase_chain_size);\n"
                "if (erase_chain_size != 0) { " + check_name
                + " = 0; }\ndisplay(\"NESTED ERASE LEVEL "
                + std::to_string(level) + " OK\");\n";
        }
        if (depth != 16)
            source += "if (nested_depth_ok == 1) { display(\"NESTED ERASE LIFECYCLE OK\"); }\n"
                "else { display(\"NESTED ERASE LIFECYCLE FAILED\"); }\n";
        source += "}\n";
        source = std::string(error_source) + "\n" + vector_source + "\n" + source;
        cblc_translation_unit_init(&unit);
        phase_started = std::chrono::steady_clock::now();
        if (trace_depth)
            std::fprintf(stderr, "template depth %zu parse started (%zu source bytes)\n",
                depth, source.size());
        if (cblc_parse_translation_unit(source.c_str(), &unit) != FT_SUCCESS)
        {
            std::printf("Nested vector<string> parsing failed at depth %zu: %s\n",
                depth, unit.parse_error_message);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        if (trace_depth)
            std::fprintf(stderr, "template depth %zu parse completed: %.3f s\n", depth,
                std::chrono::duration<double>(
                    std::chrono::steady_clock::now() - phase_started).count());
        if (test_expect_int_equal(static_cast<int>(unit.template_instantiation_count),
                static_cast<int>(depth),
                "each nested depth creates one canonical specialization per layer")
            != FT_SUCCESS)
        {
            std::printf("Nested vector<string> specialization count at depth %zu: %zu\n",
                depth, unit.template_instantiation_count);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        {
            const t_cblc_function *main_function;
            int found_scoped_nested_result;

            main_function = NULL;
            found_scoped_nested_result = 0;
            for (size_t function_index = 0;
                function_index < unit.function_count; ++function_index)
            {
                if (std::strcmp(unit.functions[function_index].source_name,
                        "main") == 0)
                {
                    main_function = &unit.functions[function_index];
                    break ;
                }
            }
            if (main_function)
            {
                for (size_t statement_index = 0;
                    statement_index < main_function->statement_count;
                    ++statement_index)
                {
                    const t_cblc_statement *statement =
                        &main_function->statements[statement_index];
                    const char *temporary_cleanup_target;

                    if (statement->type != CBLC_STATEMENT_METHOD_CALL
                        || std::strcmp(statement->call_identifier, "push_back") != 0
                        || !std::strstr(statement->call_arguments, "CBLC_CHAIN_ARG"))
                        continue ;
                    temporary_cleanup_target = NULL;
                    for (size_t cleanup_index = 0;
                        cleanup_index < statement->exception_cleanup_count;
                        ++cleanup_index)
                    {
                        if (std::strstr(statement->exception_cleanup_targets[
                                cleanup_index], "CBLC-CHAIN-ARG"))
                            temporary_cleanup_target =
                                statement->exception_cleanup_targets[cleanup_index];
                    }
                    if (statement_index + 1 < main_function->statement_count
                        && main_function->statements[statement_index + 1].type
                            == CBLC_STATEMENT_DESTRUCT
                        && temporary_cleanup_target
                        && std::strcmp(main_function->statements[
                                statement_index + 1].target,
                            temporary_cleanup_target) == 0)
                        found_scoped_nested_result = 1;
                }
            }
            if (!found_scoped_nested_result)
            {
                std::printf("Nested class result at depth %zu must be destroyed after its parent call and included in that call's exceptional cleanup\n",
                    depth);
                cblc_translation_unit_dispose(&unit);
                return (FT_FAILURE);
            }
        }
        phase_started = std::chrono::steady_clock::now();
        if (trace_depth)
            std::fprintf(stderr, "template depth %zu COBOL generation started\n",
                depth);
        if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
                &generation_diagnostic_code, generation_diagnostic,
                sizeof(generation_diagnostic)) != FT_SUCCESS
            || !generated_cobol
            || !std::strstr(generated_cobol, "PROCEDURE DIVISION")
            || !std::strstr(generated_cobol, "SET MAIN-BORROWED-LEAF")
            || !std::strstr(generated_cobol, "CBLC-TRY-")
            || !std::strstr(generated_cobol, "CBLC-EX-RAISING")
            || !std::strstr(generated_cobol, "CBLC-CHAIN-ARG")
            || !std::strstr(generated_cobol,
                "CBLC-DESTRUCTOR-MAIN-CBLC-CHAIN-ARG")
            || vector_cobol_text_has_line_with_tokens(generated_cobol,
                "PERFORM CBLC-METHOD-MAIN-LEAF-PARSE-INT",
                "CBLC-METHOD-MAIN-LEAF-PARSE-INT") != FT_SUCCESS)
        {
            std::printf("Nested vector<string> lifecycle COBOL generation failed at depth %zu\n",
                depth);
            std::printf("generation diagnostic (%d): %s\n",
                generation_diagnostic_code, generation_diagnostic);
            std::printf("generation=%d procedure=%d borrowed=%d try=%d raising=%d\n",
                generated_cobol != NULL,
                generated_cobol && std::strstr(generated_cobol, "PROCEDURE DIVISION") != NULL,
                generated_cobol && std::strstr(generated_cobol, "SET BORROWED-LEAF") != NULL,
                generated_cobol && std::strstr(generated_cobol, "CBLC-TRY-") != NULL,
                generated_cobol && std::strstr(generated_cobol, "CBLC-EX-RAISING") != NULL);
            if (generated_cobol)
                cma_free(generated_cobol);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        generated_sizes[depth_index] = std::strlen(generated_cobol);
        if (trace_depth)
            std::fprintf(stderr, "template depth %zu COBOL generation: %.3f s (%zu bytes)\n",
                depth, std::chrono::duration<double>(
                    std::chrono::steady_clock::now() - phase_started).count(),
                generated_sizes[depth_index]);
        for (level = 1; level <= depth; ++level)
        {
            char level_name[TRANSPILE_IDENTIFIER_MAX];
            char element_source_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_data_item *level_item = NULL;
            const t_cblc_data_item *element_item = NULL;
            const t_cblc_struct_type *level_type = NULL;
            const t_cblc_struct_type *element_type = NULL;
            size_t type_index;
            unsigned long long specialization_hash;
            char specialization_based_name[TRANSPILE_IDENTIFIER_MAX];
            std::string element_constructor_marker;
            const std::string destructor_marker = "CBLC-DESTRUCTOR-MAIN-LEVEL"
                + std::to_string(level) + ".";

            std::snprintf(level_name, sizeof(level_name), "level%zu", level);
            element_source_name[0] = '\0';
            if (level == 1)
                ft_strlcpy(element_source_name, "string",
                    sizeof(element_source_name));
            else
            {
                char element_item_name[TRANSPILE_IDENTIFIER_MAX];

                std::snprintf(element_item_name, sizeof(element_item_name),
                    "level%zu", level - 1);
                for (size_t data_index = 0; data_index < unit.data_count;
                    ++data_index)
                    if (std::strcmp(unit.data_items[data_index].source_name,
                            element_item_name) == 0)
                    {
                        element_item = &unit.data_items[data_index];
                        break ;
                    }
                if (element_item)
                    ft_strlcpy(element_source_name,
                        element_item->struct_type_name,
                        sizeof(element_source_name));
            }
            for (size_t data_index = 0; data_index < unit.data_count;
                ++data_index)
                if (std::strcmp(unit.data_items[data_index].source_name,
                        level_name) == 0)
                {
                    level_item = &unit.data_items[data_index];
                    break ;
                }
            if (level_item)
                for (type_index = 0; type_index < unit.struct_type_count;
                    ++type_index)
                    if (std::strcmp(unit.struct_types[type_index].source_name,
                            level_item->struct_type_name) == 0)
                    {
                        level_type = &unit.struct_types[type_index];
                        break ;
                    }
            if (level_type && level_type->is_template_instantiation)
            {
                for (type_index = 0; type_index < unit.struct_type_count;
                    ++type_index)
                    if (std::strcmp(unit.struct_types[type_index].source_name,
                            element_source_name) == 0)
                    {
                        element_type = &unit.struct_types[type_index];
                        break ;
                    }
                if (!element_type)
                {
                    std::printf("Missing concrete element type '%s' at depth %zu (level %zu)\n",
                        element_source_name, depth, level);
                    cma_free(generated_cobol);
                    cblc_translation_unit_dispose(&unit);
                    return (FT_FAILURE);
                }
                const unsigned char *hash_cursor =
                    reinterpret_cast<const unsigned char *>(
                        element_type->source_name);

                specialization_hash = 14695981039346656037ULL;
                while (*hash_cursor)
                {
                    specialization_hash ^=
                        static_cast<unsigned long long>(*hash_cursor);
                    specialization_hash *= 1099511628211ULL;
                    ++hash_cursor;
                }
                std::snprintf(specialization_based_name,
                    sizeof(specialization_based_name),
                    "CBLC-SPTR-H%016llX", specialization_hash);
                /* A vector owns instances of its element specialization.
                 * Check the constructor ABI for that concrete element type;
                 * the owning vector's destructor is checked separately. */
                element_constructor_marker = "CBLC-CONSTRUCTOR-"
                    + std::string(specialization_based_name) + "-";
            }
            if (!level_type || element_constructor_marker.empty()
                || !std::strstr(generated_cobol,
                    element_constructor_marker.c_str())
                || !std::strstr(generated_cobol, destructor_marker.c_str()))
            {
                std::printf("Nested vector<string> lifecycle call graph is incomplete at depth %zu (level %zu): owner=%s element=%s constructor-prefix=%s constructor-found=%d destructor=%s destructor-found=%d\n",
                    depth, level, level_type ? level_type->source_name : "<missing>",
                    element_type ? element_type->source_name : "<missing>",
                    element_constructor_marker.c_str(),
                    generated_cobol && !element_constructor_marker.empty()
                        && std::strstr(generated_cobol,
                            element_constructor_marker.c_str()) != NULL,
                    destructor_marker.c_str(), generated_cobol
                        && std::strstr(generated_cobol,
                            destructor_marker.c_str()) != NULL);
                cma_free(generated_cobol);
                cblc_translation_unit_dispose(&unit);
                return (FT_FAILURE);
            }
            {
                const std::string based_declaration =
                    "       01 " + std::string(specialization_based_name)
                    + " BASED.";
                size_t declaration_count = 0;
                const char *declaration_cursor = generated_cobol;

                while ((declaration_cursor = std::strstr(declaration_cursor,
                            based_declaration.c_str())) != NULL)
                {
                    declaration_count += 1;
                    declaration_cursor += based_declaration.size();
                }
                if (declaration_count != 1)
                {
                    std::printf("Template specialization '%s' at depth %zu must have one shared BASED layout, found %zu\n",
                        level_type->source_name, depth, declaration_count);
                    cma_free(generated_cobol);
                    cblc_translation_unit_dispose(&unit);
                    return (FT_FAILURE);
                }
            }
        }
        if (depth == 16)
        {
            const std::string outer_destructor = "CBLC-DESTRUCTOR-MAIN-LEVEL16.";

            if (!std::strstr(generated_cobol, outer_destructor.c_str()))
            {
                std::printf("Depth-16 nested vector outer destructor is missing\n");
                cma_free(generated_cobol);
                cblc_translation_unit_dispose(&unit);
                return (FT_FAILURE);
            }
            if (trace_depth)
                std::fprintf(stderr, "template depth 16 COBOL compile/run started\n");
            const int execution_status =
                vector_compile_and_run_depth_16_cobol(generated_cobol);

            if (trace_depth)
                std::fprintf(stderr, "template depth 16 COBOL compile/run returned %d\n",
                    execution_status);
            if (execution_status != FT_SUCCESS)
            {
                std::printf("Depth-16 generated COBOL failed compile-and-execute verification\n");
                cma_free(generated_cobol);
                cblc_translation_unit_dispose(&unit);
                return (FT_FAILURE);
            }
        }
        else
        {
            if (trace_depth)
                std::fprintf(stderr, "template depth %zu COBOL compile/run started\n",
                    depth);
            const int execution_status =
                vector_compile_and_run_nested_depth_cobol(generated_cobol, depth);

            if (trace_depth)
                std::fprintf(stderr, "template depth %zu COBOL compile/run returned %d\n",
                    depth, execution_status);
            if (execution_status != FT_SUCCESS)
            {
                std::printf("Depth-%zu generated COBOL failed compile-and-execute verification\n",
                    depth);
                cma_free(generated_cobol);
                cblc_translation_unit_dispose(&unit);
                return (FT_FAILURE);
            }
        }
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
    }
    if (selected_depth == 0
        && generated_sizes[15] > generated_sizes[4] * chain_depths[15] * 2
            / chain_depths[4])
    {
        std::printf("Depth-16 generated COBOL grew beyond the specialization-linear budget: depth 5=%zu bytes, depth 16=%zu bytes\n",
            generated_sizes[4], generated_sizes[15]);
        return (FT_FAILURE);
    }

    source.clear();
    for (index = 0; index <= supported_depth; ++index)
        source += "vector<";
    source += "string";
    for (index = 0; index <= supported_depth; ++index)
        source += ">";
    source += " value;\nvoid main() { }\n";

    cblc_translation_unit_init(&unit);
    source = std::string(error_source) + "\n" + vector_source + "\n" + source;
    {
        const int parse_status = cblc_parse_translation_unit(source.c_str(), &unit);
        if (test_expect_int_equal(parse_status,
            FT_FAILURE,
            "one level beyond the supported nesting depth is rejected") != FT_SUCCESS
        || test_expect_int_equal(unit.parse_error_code,
            TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED,
            "excessive type nesting has a stable template diagnostic") != FT_SUCCESS
        || test_expect_success(std::strstr(unit.parse_error_message,
                "template type nesting limit exceeded") != nullptr
                ? FT_SUCCESS : FT_FAILURE,
            "depth diagnostic identifies the exhausted type-nesting limit")
            != FT_SUCCESS)
    {
        std::printf("Depth-%zu template rejection diagnostic: %s\n",
            supported_depth + 1, unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
        }
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_vector_indexed_method_can_call_another_method_on_based_receiver)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char binary_path[256];
    char output_path[256];
    char compile_log_path[256];
    char runtime_output[256];
    char command[1024];
    int command_length;
    int status;
    int indexed_reset_status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    binary_path[0] = '\0';
    output_path[0] = '\0';
    compile_log_path[0] = '\0';
    runtime_output[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "based_receiver.cblc", source_path,
                sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "based_receiver.cob", cobol_path,
                sizeof(cobol_path)) != FT_SUCCESS
        || test_join_path(directory, "based_receiver.bin", binary_path,
                sizeof(binary_path)) != FT_SUCCESS
        || test_join_path(directory, "based_receiver.txt", output_path,
                sizeof(output_path)) != FT_SUCCESS
        || test_join_path(directory, "based_receiver_compile.txt",
                compile_log_path, sizeof(compile_log_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path,
            g_vector_based_receiver_method_source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: indexed class method with nested self-call should transpile\n");
        goto cleanup;
    }
    indexed_reset_status = vector_cobol_main_performs_method_writing_line_tokens(
        cobol_path, "CBLC-METHOD-CBLC-MRET-", "CBLC-SPTR-H",
        "CBLC-USER-VALUE = 0");
    if (indexed_reset_status != FT_SUCCESS)
    {
        std::printf("Assertion failed: indexed values[0]->reset() must call its resolved method paragraph and write zero into the concrete based element\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "CBLC-CONSTRUCTOR-CBLC-SPTR-H") != FT_SUCCESS
        || vector_cobol_contains(cobol_path,
            "PERFORM CBLC-CONSTRUCTOR-CBLC-SPTR-H") != FT_SUCCESS)
    {
        std::printf("Assertion failed: pointer-backed Probe elements should dispatch through their concrete constructor\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "VECTOR-EMPLACE-BACK-OVL-1-RELOCATION")
        != FT_SUCCESS)
    {
        std::printf("Assertion failed: the explicit-argument emplace_back overload must have distinct relocation state\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "VECTOR-EMPLACE-BACK-RELOCATION") != FT_SUCCESS
        || vector_cobol_main_contains_in_order(cobol_path,
            "PERFORM CBLC-METHOD-CBLC-MRET-",
            "PERFORM CBLC-METHOD-CBLC-MRET-",
            "PERFORM CBLC-METHOD-CBLC-MRET-") != FT_SUCCESS)
    {
        std::printf("Assertion failed: zero-argument and explicit-argument emplace_back overloads must both be dispatched from main\n");
        goto cleanup;
    }
    if (!test_cobc_available())
    {
        status = test_require_cobc_dependency(__func__);
        goto cleanup;
    }
    if (compile_generated_cobol(binary_path, cobol_path, compile_log_path,
            "cobc should compile indexed based-receiver method dispatch") != FT_SUCCESS)
    {
        if (test_read_text_file(compile_log_path, runtime_output,
                sizeof(runtime_output)) == FT_SUCCESS)
            std::printf("Indexed based-receiver COBOL diagnostics:\n%s\n",
                runtime_output);
        goto cleanup;
    }
    if (execute_binary(directory, "based_receiver.bin", "based_receiver.txt",
            "indexed based-receiver method should execute") != FT_SUCCESS
        || test_read_text_file(output_path, runtime_output,
            sizeof(runtime_output)) != FT_SUCCESS
        || std::strcmp(runtime_output, "+000000000\n") != 0)
    {
        std::printf("Assertion failed: values[0]->reset() should mutate the indexed Probe and display zero; got '%s'\n",
            runtime_output);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    test_remove_directory(directory);
    return (status);
}

FT_TEST(test_class_destructor_cleans_members_in_reverse_declaration_order)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[1024];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "member_destruction.cblc", source_path,
                sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "member_destruction.cob", cobol_path,
                sizeof(cobol_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path,
            g_class_member_destruction_order_source) != FT_SUCCESS)
        goto cleanup;
    command_length = vector_make_transpile_command(command, sizeof(command),
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: nested class destructor fixture should transpile\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path,
            "DISPLAY \"OWNER_BODY\".") != FT_SUCCESS
        || vector_cobol_main_contains_in_order(cobol_path,
            "PERFORM CBLC-DESTRUCTOR-MAIN-OWNER.",
            "DISPLAY \"SECOND_MEMBER\".", "DISPLAY \"FIRST_MEMBER\".")
            != FT_SUCCESS)
    {
        std::printf("Assertion failed: destructor body must run before members, which must be destroyed in reverse declaration order\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    test_remove_directory(directory);
    return (status);
}
