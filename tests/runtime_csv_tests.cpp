#include "cblc_transpiler.hpp"

#include "libft/Libft/libft.hpp"

#include "test_suites.hpp"

static int runtime_csv_init_fields(t_runtime_string *fields, size_t count, size_t capacity)
{
    size_t index;

    index = 0;
    while (index < count)
    {
        if (runtime_string_init(&fields[index], capacity) != FT_SUCCESS)
        {
            while (index > 0)
            {
                index -= 1;
                runtime_string_dispose(&fields[index]);
            }
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static void runtime_csv_dispose_fields(t_runtime_string *fields, size_t count)
{
    size_t index;

    index = 0;
    while (index < count)
    {
        runtime_string_dispose(&fields[index]);
        index += 1;
    }
}

FT_TEST(test_runtime_csv_parse_basic_line)
{
    t_runtime_string fields[3];
    size_t field_count;

    if (runtime_csv_init_fields(fields, 3, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_csv_parse_line("alpha,beta,gamma", fields, 3, &field_count),
            "runtime_csv_parse_line should parse comma-separated values") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(field_count, 3, "runtime_csv_parse_line should report field count") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(fields[0].data, "alpha", "runtime_csv_parse_line should capture first field")
        != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(fields[1].data, "beta", "runtime_csv_parse_line should capture second field")
        != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(fields[2].data, "gamma", "runtime_csv_parse_line should capture third field")
        != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    runtime_csv_dispose_fields(fields, 3);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_csv_parse_handles_quoted_values)
{
    const char *line;
    t_runtime_string fields[3];
    size_t field_count;

    line = "\"quoted, field\",plain,\"escaped \"\"quote\"\"\"";
    if (runtime_csv_init_fields(fields, 3, 32) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_csv_parse_line(line, fields, 3, &field_count),
            "runtime_csv_parse_line should parse quoted fields") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(field_count, 3, "runtime_csv_parse_line should count quoted fields") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(fields[0].data, "quoted, field",
            "runtime_csv_parse_line should unescape commas in quotes") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(fields[1].data, "plain",
            "runtime_csv_parse_line should capture unquoted field") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(fields[2].data, "escaped \"quote\"",
            "runtime_csv_parse_line should collapse doubled quotes") != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    runtime_csv_dispose_fields(fields, 3);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_csv_format_quotes_and_commas)
{
    t_runtime_string fields[3];
    t_runtime_string line;

    if (runtime_csv_init_fields(fields, 3, 32) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_string_init(&line, 64) != FT_SUCCESS)
    {
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (runtime_string_assign(&fields[0], "value one") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (runtime_string_assign(&fields[1], "two,three") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (runtime_string_assign(&fields[2], " spaced ") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_csv_format_line(fields, 3, &line),
            "runtime_csv_format_line should format CSV line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "\"value one\",\"two,three\",\" spaced \"",
            "runtime_csv_format_line should quote special fields") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_csv_dispose_fields(fields, 3);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&line);
    runtime_csv_dispose_fields(fields, 3);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_line_read_fixed_and_write_fixed)
{
    const char *path;
    t_runtime_file file;
    t_runtime_string line;
    int end_of_file;

    path = "test_runtime_line_fixed.txt";
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_write(&file, path),
            "runtime_file_open_write should create test file") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_line_write_fixed(&file, "record-one", 10, 12),
            "runtime_line_write_fixed should pad first record") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_line_write_fixed(&file, "second", 6, 12),
            "runtime_line_write_fixed should pad second record") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should flush records") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_read(&file, path),
            "runtime_file_open_read should reopen fixed file") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (runtime_string_init(&line, 16) != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    end_of_file = 0;
    if (test_expect_success(runtime_line_read_fixed(&file, 12, &line, &end_of_file),
            "runtime_line_read_fixed should read first record") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "record-one  ",
            "runtime_line_read_fixed should include padded spaces") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(end_of_file, 0, "runtime_line_read_fixed should not set EOF after first record")
        != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_line_read_fixed(&file, 12, &line, &end_of_file),
            "runtime_line_read_fixed should read second record") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "second      ",
            "runtime_line_read_fixed should report padded second record") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(end_of_file, 0,
            "runtime_line_read_fixed should leave EOF cleared after exact read") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    end_of_file = 0;
    if (test_expect_success(runtime_line_read_fixed(&file, 12, &line, &end_of_file),
            "runtime_line_read_fixed should handle EOF with empty record") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "",
            "runtime_line_read_fixed should return empty string after EOF") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(end_of_file, 1, "runtime_line_read_fixed should flag EOF on empty read")
        != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&line);
    runtime_file_close(&file);
    test_remove_file(path);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_line_read_variable_and_write_variable)
{
    const char *path;
    t_runtime_file file;
    t_runtime_string line;
    int end_of_file;

    path = "test_runtime_line_variable.txt";
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_write(&file, path),
            "runtime_file_open_write should create variable file") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_line_write_variable(&file, "first", 5, '\n'),
            "runtime_line_write_variable should write first line") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_line_write_variable(&file, "second line", 11, '\n'),
            "runtime_line_write_variable should write second line") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_line_write_variable(&file, "third", 5, '\n'),
            "runtime_line_write_variable should write third line") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should flush variable data") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_read(&file, path),
            "runtime_file_open_read should reopen variable file") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (runtime_string_init(&line, 16) != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    end_of_file = 0;
    if (test_expect_success(runtime_line_read_variable(&file, '\n', &line, &end_of_file),
            "runtime_line_read_variable should read first line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "first",
            "runtime_line_read_variable should trim newline terminator") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(end_of_file, 0, "runtime_line_read_variable should not set EOF mid-stream")
        != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_line_read_variable(&file, '\n', &line, &end_of_file),
            "runtime_line_read_variable should read second line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "second line",
            "runtime_line_read_variable should capture middle line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_line_read_variable(&file, '\n', &line, &end_of_file),
            "runtime_line_read_variable should read final line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "third",
            "runtime_line_read_variable should read last line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(end_of_file, 0,
            "runtime_line_read_variable should leave EOF cleared until stream ends") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    end_of_file = 0;
    if (test_expect_success(runtime_line_read_variable(&file, '\n', &line, &end_of_file),
            "runtime_line_read_variable should handle EOF with empty line") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(line.data, "",
            "runtime_line_read_variable should return empty string after EOF") != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(end_of_file, 1, "runtime_line_read_variable should report EOF on empty read")
        != FT_SUCCESS)
    {
        runtime_string_dispose(&line);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&line);
    runtime_file_close(&file);
    test_remove_file(path);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_csv_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_csv_parse_basic_line", test_runtime_csv_parse_basic_line},
        {"runtime_csv_parse_handles_quoted_values", test_runtime_csv_parse_handles_quoted_values},
        {"runtime_csv_format_quotes_and_commas", test_runtime_csv_format_quotes_and_commas},
        {"runtime_line_read_fixed_and_write_fixed", test_runtime_line_read_fixed_and_write_fixed},
        {"runtime_line_read_variable_and_write_variable", test_runtime_line_read_variable_and_write_variable}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
