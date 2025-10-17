#ifndef COMPILER_TEST_SUPPORT_HPP
#define COMPILER_TEST_SUPPORT_HPP

#include <cstddef>

void test_cleanup_example_artifacts(const char *source_path, const char *binary_path, const char *output_path);
void test_cleanup_generated_artifacts(const char *binary_path, const char *output_path);
int test_create_temp_directory(char *buffer, size_t buffer_size);
int test_join_path(const char *directory, const char *name, char *buffer, size_t buffer_size);
void test_remove_directory(const char *path);
void test_cleanup_module_directory(const char *directory, const char *module_path, const char *binary_path,
    const char *output_path);
void test_cleanup_example_artifacts_with_log(const char *source_path, const char *binary_path,
    const char *output_path, const char *log_path);
int test_run_command_capture_status(const char *command, int *exit_status);
int test_cobol_fixture_contains(const char *path, const char *snippet);
int test_expect_file_equals(const char *path, const char *expected);
int test_expect_transcript_equal(const char *actual, const char *expected);
int test_expect_compiler_output_allowed(const char *path);

#endif
