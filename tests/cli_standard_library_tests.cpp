#include "test_suites.hpp"
#include "cli_test_registry.hpp"

#include <cstdlib>
#include <cstring>
#include <unistd.h>

static int cli_standard_library_set_environment(const char *name, const char *value)
{
#if defined(_WIN32)
    if (_putenv_s(name, value) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
#else
    if (setenv(name, value, 1) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
#endif
}

static void cli_standard_library_unset_environment(const char *name)
{
#if defined(_WIN32)
    _putenv_s(name, "");
#else
    unsetenv(name);
#endif
}

FT_TEST(test_cli_standard_library_emits_all_programs)
{
    const t_transpiler_standard_library_entry *entries;
    char directory_template[64];
    char command[512];
    char program_path[TRANSPILE_FILE_PATH_MAX];
    char helper_path[TRANSPILE_FILE_PATH_MAX];
    char manifest_path[TRANSPILE_FILE_PATH_MAX];
    char cleanup_path[TRANSPILE_FILE_PATH_MAX];
    char identifier[256];
    char file_buffer[131072];
    size_t entry_count;
    size_t index;
    int command_length;
    int status;
    int created_directory;
    int validation_env_set;

    entries = transpiler_standard_library_get_entries(&entry_count);
    created_directory = 0;
    validation_env_set = 0;
    ft_strlcpy(directory_template, "/tmp/ctoc_standard_libraryXXXXXX", sizeof(directory_template));
    if (!mkdtemp(directory_template))
        return (FT_FAILURE);
    created_directory = 1;
    if (cli_standard_library_set_environment("CTOC_SKIP_STANDARD_LIBRARY_VALIDATION", "1") != FT_SUCCESS)
    {
        std::printf("Assertion failed: unable to skip standard library validation for CLI run\n");
        goto cleanup;
    }
    validation_env_set = 1;
    status = FT_FAILURE;
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --direction standard-library --output-dir %s",
        directory_template);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
        goto cleanup;
    if (test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: standard-library emission command should succeed\n");
        goto cleanup;
    }
    index = 0;
    while (index < entry_count)
    {
        if (std::snprintf(program_path, sizeof(program_path), "%s/%s.cob", directory_template,
                entries[index].program_name) < 0)
            goto cleanup;
        if (test_read_text_file(program_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS)
        {
            std::printf("Assertion failed: standard library program '%s' should be emitted\n",
                entries[index].program_name);
            goto cleanup;
        }
        if (std::snprintf(identifier, sizeof(identifier), "PROGRAM-ID. %s.",
                entries[index].program_name) < 0)
            goto cleanup;
        if (!ft_strnstr(file_buffer, identifier, std::strlen(file_buffer)))
        {
            std::printf("Assertion failed: standard library program '%s' should declare PROGRAM-ID\n",
                entries[index].program_name);
            goto cleanup;
        }
        test_remove_file(program_path);
        index += 1;
    }
    if (std::snprintf(helper_path, sizeof(helper_path), "%s/cblc_runtime_helpers.c",
            directory_template) < 0)
        goto cleanup;
    if (test_read_text_file(helper_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS)
    {
        std::printf("Assertion failed: runtime helper source should be emitted\n");
        goto cleanup;
    }
    if (!ft_strnstr(file_buffer, "#include <stddef.h>", std::strlen(file_buffer)))
    {
        std::printf("Assertion failed: runtime helper source should include required headers\n");
        goto cleanup;
    }
    test_remove_file(helper_path);
    if (std::snprintf(manifest_path, sizeof(manifest_path), "%s/cblc.manifest.json",
            directory_template) < 0)
        goto cleanup;
    if (test_read_text_file(manifest_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS)
    {
        std::printf("Assertion failed: standard library manifest should be emitted\n");
        goto cleanup;
    }
    if (!ft_strnstr(file_buffer, "\"schema_version\": 1", std::strlen(file_buffer))
        || !ft_strnstr(file_buffer, "CBLC-STRLEN", std::strlen(file_buffer))
        || !ft_strnstr(file_buffer, "cblc-runtime-helpers", std::strlen(file_buffer)))
    {
        std::printf("Assertion failed: standard library manifest should describe generated artifacts\n");
        goto cleanup;
    }
    test_remove_file(manifest_path);
    status = FT_SUCCESS;
cleanup:
    if (validation_env_set)
        cli_standard_library_unset_environment("CTOC_SKIP_STANDARD_LIBRARY_VALIDATION");
    if (created_directory)
    {
        index = 0;
        while (index < entry_count)
        {
            if (std::snprintf(cleanup_path, sizeof(cleanup_path), "%s/%s.cob", directory_template,
                    entries[index].program_name) >= 0)
                test_remove_file(cleanup_path);
            index += 1;
        }
        if (std::snprintf(helper_path, sizeof(helper_path), "%s/cblc_runtime_helpers.c",
                directory_template) >= 0)
            test_remove_file(helper_path);
        if (std::snprintf(manifest_path, sizeof(manifest_path), "%s/cblc.manifest.json",
                directory_template) >= 0)
            test_remove_file(manifest_path);
        rmdir(directory_template);
    }
    return (status);
}

FT_TEST(test_cli_cobol_translation_emits_required_standard_library_programs)
{
    char directory_template[64];
    char source_path[TRANSPILE_FILE_PATH_MAX];
    char target_path[TRANSPILE_FILE_PATH_MAX];
    char required_path[TRANSPILE_FILE_PATH_MAX];
    char unrelated_path[TRANSPILE_FILE_PATH_MAX];
    char manifest_path[TRANSPILE_FILE_PATH_MAX];
    char command[1024];
    char file_buffer[131072];
    const char *source;
    int status;
    int created_directory;

    status = FT_FAILURE;
    created_directory = 0;
    ft_strlcpy(directory_template, "/tmp/ctoc_required_libraryXXXXXX",
        sizeof(directory_template));
    if (!mkdtemp(directory_template))
        return (FT_FAILURE);
    created_directory = 1;
    if (std::snprintf(source_path, sizeof(source_path), "%s/main.cblc", directory_template) < 0
        || std::snprintf(target_path, sizeof(target_path), "%s/main.cob", directory_template) < 0
        || std::snprintf(required_path, sizeof(required_path), "%s/CBLC-STRLEN.cob",
            directory_template) < 0
        || std::snprintf(unrelated_path, sizeof(unrelated_path), "%s/CBLC-ABS.cob",
            directory_template) < 0
        || std::snprintf(manifest_path, sizeof(manifest_path), "%s/cblc.manifest.json",
            directory_template) < 0)
        goto cleanup;
    source = "char text[8];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    text = \"HI\";\n"
        "    total = std::strlen(text);\n"
        "    return;\n"
        "}\n";
    if (test_write_text_file(source_path, source) != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(command, sizeof(command),
            "./ctoc_cobol_transpiler --direction cblc-to-cobol --input %s --output %s --output-dir %s",
            source_path, "main.cob", directory_template) < 0
        || test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(required_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS
        || !ft_strnstr(file_buffer, "PROGRAM-ID. CBLC-STRLEN.",
            std::strlen(file_buffer)))
        goto cleanup;
    if (test_read_text_file(manifest_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS
        || !ft_strnstr(file_buffer, "CBLC-STRLEN", std::strlen(file_buffer))
        || !ft_strnstr(file_buffer, "\"dependencies\": [\"CBLC-STRLEN\"]",
            std::strlen(file_buffer))
        || ft_strnstr(file_buffer, "CBLC-ABS", std::strlen(file_buffer)))
        goto cleanup;
    if (access(unrelated_path, F_OK) == 0)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    test_remove_file(source_path);
    test_remove_file(target_path);
    test_remove_file(required_path);
    test_remove_file(unrelated_path);
    test_remove_file(manifest_path);
    if (created_directory)
        rmdir(directory_template);
    return (status);
}

FT_TEST(test_cli_c_translation_manifest_records_embedded_runtime_helpers)
{
    char directory_template[64];
    char source_path[TRANSPILE_FILE_PATH_MAX];
    char target_path[TRANSPILE_FILE_PATH_MAX];
    char manifest_path[TRANSPILE_FILE_PATH_MAX];
    char command[1024];
    char file_buffer[131072];
    const char *source;
    int status;
    int created_directory;

    status = FT_FAILURE;
    created_directory = 0;
    ft_strlcpy(directory_template, "/tmp/ctoc_c_runtime_manifestXXXXXX",
        sizeof(directory_template));
    if (!mkdtemp(directory_template))
        return (FT_FAILURE);
    created_directory = 1;
    if (std::snprintf(source_path, sizeof(source_path), "%s/main.cblc", directory_template) < 0
        || std::snprintf(target_path, sizeof(target_path), "%s/main.c", directory_template) < 0
        || std::snprintf(manifest_path, sizeof(manifest_path), "%s/cblc.manifest.json",
            directory_template) < 0)
        goto cleanup;
    source = "char text[8];\n"
        "void main()\n"
        "{\n"
        "    text = \"HI\";\n"
        "    return;\n"
        "}\n";
    if (test_write_text_file(source_path, source) != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(command, sizeof(command),
            "./ctoc_cobol_transpiler --direction cblc-to-c --input %s --output %s --output-dir %s",
            source_path, "main.c", directory_template) < 0
        || test_run_command(command) != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(target_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS
        || !ft_strnstr(file_buffer, "cblc_char_assign_literal",
            std::strlen(file_buffer))
        || ft_strnstr(file_buffer, "cblc_display_literal", std::strlen(file_buffer)))
        goto cleanup;
    if (test_read_text_file(manifest_path, file_buffer, sizeof(file_buffer)) != FT_SUCCESS
        || !ft_strnstr(file_buffer, "\"runtime_helpers\": [{\"id\": \"cblc_string_length\"",
            std::strlen(file_buffer))
        || !ft_strnstr(file_buffer, "\"id\": \"cblc_char_assign_literal\"",
            std::strlen(file_buffer))
        || !ft_strnstr(file_buffer, "\"dependencies\": [\"cblc_string_length\"]",
            std::strlen(file_buffer)))
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    test_remove_file(source_path);
    test_remove_file(target_path);
    test_remove_file(manifest_path);
    if (created_directory)
        rmdir(directory_template);
    return (status);
}

static const t_test_case g_cli_standard_library_tests[] = {
    {"cli_standard_library_emits_all_programs", test_cli_standard_library_emits_all_programs},
    {"cli_cobol_translation_emits_required_standard_library_programs",
        test_cli_cobol_translation_emits_required_standard_library_programs},
    {"cli_c_translation_manifest_records_embedded_runtime_helpers",
        test_cli_c_translation_manifest_records_embedded_runtime_helpers}
};

const t_test_case *get_cli_standard_library_tests(size_t *count)
{
    if (count)
        *count = sizeof(g_cli_standard_library_tests) / sizeof(g_cli_standard_library_tests[0]);
    return (g_cli_standard_library_tests);
}
