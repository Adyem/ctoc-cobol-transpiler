#include "test_suites.hpp"

#include <filesystem>

#include "transpiler_semantic_dump.hpp"

FT_TEST(test_transpiler_context_captures_semantic_ir_snapshots)
{
    t_transpiler_context context;
    t_transpiler_copybook_item items[1];
    const char *before;
    const char *after;

    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_semantic_diff_enabled(&context, 1);
    transpiler_context_set_semantic_diff_directory(&context, "snapshots");
    if (transpiler_context_register_module(&context, "MAIN", "main.cblc") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_function(&context, "MAIN", "helper",
            TRANSPILE_FUNCTION_RETURN_VOID, TRANSPILE_SYMBOL_PUBLIC) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_data_item(&context, "BUFFER",
            TRANSPILE_DATA_ITEM_ALPHANUMERIC, 16, 0, NULL) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_capture_semantic_snapshot_before(&context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    before = transpiler_context_get_semantic_snapshot_before(&context);
    if (!before)
    {
        pf_printf("Assertion failed: semantic snapshot before should exist\n");
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(before, "modules:", ft_strlen(before)))
    {
        pf_printf("Assertion failed: semantic snapshot before should list modules\n");
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    ft_bzero(&items[0], sizeof(items[0]));
    ft_strlcpy(items[0].name, "COPY-FIELD", sizeof(items[0].name));
    items[0].kind = TRANSPILE_DATA_ITEM_NUMERIC;
    items[0].declared_length = 4;
    items[0].is_read_only = 0;
    if (transpiler_context_register_copybook(&context, "WORKING", items, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_capture_semantic_snapshot_after(&context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    after = transpiler_context_get_semantic_snapshot_after(&context);
    if (!after)
    {
        pf_printf("Assertion failed: semantic snapshot after should exist\n");
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(after, "copybooks:", ft_strlen(after)))
    {
        pf_printf("Assertion failed: semantic snapshot after should list copybooks\n");
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_clear_semantic_snapshots(&context);
    if (transpiler_context_get_semantic_snapshot_before(&context))
    {
        pf_printf("Assertion failed: clearing snapshots should remove before snapshot\n");
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_get_semantic_snapshot_after(&context))
    {
        pf_printf("Assertion failed: clearing snapshots should remove after snapshot\n");
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

static int semantic_dump_read_file(const char *path, char *buffer, size_t buffer_size)
{
    t_runtime_file file;
    size_t total;
    size_t bytes_read;

    if (!path || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    runtime_file_init(&file);
    if (runtime_file_open_read(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    total = 0;
    while (total + 1 < buffer_size)
    {
        if (runtime_file_read(&file, buffer + total, buffer_size - total - 1,
                &bytes_read) != FT_SUCCESS)
        {
            runtime_file_close(&file);
            return (FT_FAILURE);
        }
        if (bytes_read == 0)
            break ;
        total += bytes_read;
    }
    buffer[total] = '\0';
    if (runtime_file_close(&file) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_semantic_dump_emits_files_with_custom_directory)
{
    t_transpiler_context context;
    const char *directory;
    const char *before_path;
    const char *after_path;
    char buffer[256];

    directory = "semantic-dump-integration";
    before_path = "semantic-dump-integration/input.semantic.before.txt";
    after_path = "semantic-dump-integration/input.semantic.after.txt";
    std::filesystem::remove_all(directory);
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_semantic_diff_enabled(&context, 1);
    transpiler_context_set_semantic_diff_directory(&context, directory);
    context.semantic_snapshot_before = static_cast<char *>(cma_calloc(7, sizeof(char)));
    context.semantic_snapshot_after = static_cast<char *>(cma_calloc(6, sizeof(char)));
    if (!context.semantic_snapshot_before || !context.semantic_snapshot_after)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    ft_memcpy(context.semantic_snapshot_before, "before", 6);
    ft_memcpy(context.semantic_snapshot_after, "after", 5);
    if (transpiler_semantic_dump_emit(&context, "input.cob", "output/result.cbl") != FT_SUCCESS)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all(directory);
        return (FT_FAILURE);
    }
    if (!std::filesystem::exists(before_path) || !std::filesystem::exists(after_path))
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all(directory);
        return (FT_FAILURE);
    }
    if (semantic_dump_read_file(before_path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, "before", 6) != 0)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all(directory);
        return (FT_FAILURE);
    }
    if (semantic_dump_read_file(after_path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all(directory);
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, "after", 5) != 0)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all(directory);
        return (FT_FAILURE);
    }
    transpiler_context_clear_semantic_snapshots(&context);
    transpiler_context_dispose(&context);
    std::filesystem::remove_all(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_semantic_dump_uses_resolved_output_path)
{
    t_transpiler_context context;
    const char *before_path;
    const char *after_path;
    char buffer[256];

    before_path = "semantic-output/input.semantic.before.txt";
    after_path = "semantic-output/input.semantic.after.txt";
    std::filesystem::remove_all("semantic-output");
    std::filesystem::create_directories("semantic-output");
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_semantic_diff_enabled(&context, 1);
    context.semantic_snapshot_before = static_cast<char *>(cma_calloc(4, sizeof(char)));
    context.semantic_snapshot_after = static_cast<char *>(cma_calloc(5, sizeof(char)));
    if (!context.semantic_snapshot_before || !context.semantic_snapshot_after)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all("semantic-output");
        return (FT_FAILURE);
    }
    ft_memcpy(context.semantic_snapshot_before, "one", 3);
    ft_memcpy(context.semantic_snapshot_after, "two", 3);
    if (transpiler_semantic_dump_emit(&context, "input.cob", "semantic-output/input.cbl") != FT_SUCCESS)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all("semantic-output");
        return (FT_FAILURE);
    }
    if (semantic_dump_read_file(before_path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all("semantic-output");
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, "one", 3) != 0)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all("semantic-output");
        return (FT_FAILURE);
    }
    if (semantic_dump_read_file(after_path, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all("semantic-output");
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, "two", 3) != 0)
    {
        transpiler_context_clear_semantic_snapshots(&context);
        transpiler_context_dispose(&context);
        std::filesystem::remove_all("semantic-output");
        return (FT_FAILURE);
    }
    transpiler_context_clear_semantic_snapshots(&context);
    transpiler_context_dispose(&context);
    std::filesystem::remove_all("semantic-output");
    return (FT_SUCCESS);
}
