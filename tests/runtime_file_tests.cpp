#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

FT_TEST(test_runtime_file_write_and_read_text)
{
    const char *path;
    const char *contents;
    t_runtime_file file;
    char buffer[128];
    size_t bytes_read;
    size_t expected_length;

    path = "test_runtime_file.txt";
    contents = "runtime-file-contents";
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_write(&file, path), "runtime_file_open_write should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_file_write(&file, contents, ft_strlen(contents)),
        "runtime_file_write should succeed") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should succeed") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_read(&file, path), "runtime_file_open_read should succeed") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    ft_memset(buffer, 0, sizeof(buffer));
    if (test_expect_success(runtime_file_read(&file, buffer, sizeof(buffer), &bytes_read),
        "runtime_file_read should succeed") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    expected_length = static_cast<size_t>(ft_strlen(contents));
    if (bytes_read != expected_length)
    {
        pf_printf("Assertion failed: runtime_file_read should report full length (expected %zu, got %zu)\n",
            expected_length, bytes_read);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(buffer, contents, "runtime_file_read should capture file contents") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_file_close(&file);
    test_remove_file(path);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_file_open_read_missing_path)
{
    t_runtime_file file;

    runtime_file_init(&file);
    if (runtime_file_open_read(&file, "nonexistent-runtime-file.txt") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_file_open_read should fail for missing files\n");
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should allow closing unopened file") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_file_reopen_transitions_modes)
{
    const char *path;
    const char *contents;
    t_runtime_file file;
    char buffer[64];
    size_t bytes_read;
    size_t expected_length;

    path = "test_runtime_file_reopen.txt";
    contents = "runtime-file-reopen";
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_write(&file, path), "runtime_file_open_write should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_file_write(&file, contents, ft_strlen(contents)),
        "runtime_file_write should succeed") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_open_read(&file, path),
        "runtime_file_open_read should replace existing descriptor") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    ft_memset(buffer, 0, sizeof(buffer));
    if (test_expect_success(runtime_file_read(&file, buffer, sizeof(buffer), &bytes_read),
        "runtime_file_read should succeed after reopening") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    expected_length = ft_strlen(contents);
    if (bytes_read != expected_length)
    {
        pf_printf("Assertion failed: runtime_file_read should report full length after reopening (expected %zu, got %zu)\n",
            expected_length, bytes_read);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(buffer, contents, "runtime_file_read should return written contents") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should succeed") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    test_remove_file(path);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_file_requires_open_descriptor)
{
    t_runtime_file file;
    char buffer[8];
    size_t bytes_read;

    runtime_file_init(&file);
    if (runtime_file_read(&file, buffer, sizeof(buffer), &bytes_read) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_file_read should fail without open descriptor\n");
        return (FT_FAILURE);
    }
    if (runtime_file_write(&file, "x", 1) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_file_write should fail without open descriptor\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should allow closing unopened file") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_file_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_file_write_and_read_text", test_runtime_file_write_and_read_text},
        {"runtime_file_open_read_missing_path", test_runtime_file_open_read_missing_path},
        {"runtime_file_reopen_transitions_modes", test_runtime_file_reopen_transitions_modes},
        {"runtime_file_requires_open_descriptor", test_runtime_file_requires_open_descriptor}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
