#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int sample_manifest_extract_line(const char *buffer, size_t *offset, char *line, size_t line_size)
{
    size_t index;
    size_t length;

    if (!buffer)
        return (FT_FAILURE);
    if (!offset)
        return (FT_FAILURE);
    if (!line)
        return (FT_FAILURE);
    if (line_size == 0)
        return (FT_FAILURE);
    index = *offset;
    if (buffer[index] == '\0')
        return (FT_FAILURE);
    length = 0;
    while (buffer[index] != '\0' && buffer[index] != '\n')
    {
        if (length + 1 >= line_size)
            return (FT_FAILURE);
        line[length] = buffer[index];
        length++;
        index++;
    }
    line[length] = '\0';
    if (buffer[index] == '\n')
        index++;
    *offset = index;
    return (FT_SUCCESS);
}

static int validate_manifest_entries(const char *manifest_path)
{
    char manifest_buffer[1024];
    char sample_buffer[2048];
    char line[256];
    size_t offset;

    if (test_read_text_file(manifest_path, manifest_buffer, sizeof(manifest_buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    offset = 0;
    while (manifest_buffer[offset] != '\0')
    {
        if (sample_manifest_extract_line(manifest_buffer, &offset, line, sizeof(line)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (ft_strlen(line) == 0)
            continue;
        if (test_read_text_file(line, sample_buffer, sizeof(sample_buffer)) != FT_SUCCESS)
        {
            pf_printf("Assertion failed: sample '%s' listed in manifest '%s' should exist and be readable\n", line, manifest_path);
            return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int validate_inventory_mentions(const char *manifest_path, const char *inventory_path)
{
    char line[256];
    size_t offset;
    char manifest_buffer[1024];
    char inventory_buffer[4096];
    size_t inventory_length;

    if (test_read_text_file(manifest_path, manifest_buffer, sizeof(manifest_buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_read_text_file(inventory_path, inventory_buffer, sizeof(inventory_buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    inventory_length = ft_strlen(inventory_buffer);
    offset = 0;
    while (manifest_buffer[offset] != '\0')
    {
        if (sample_manifest_extract_line(manifest_buffer, &offset, line, sizeof(line)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (ft_strlen(line) == 0)
            continue;
        if (!ft_strnstr(inventory_buffer, line, inventory_length))
        {
            pf_printf("Assertion failed: inventory document '%s' should mention sample '%s'\n", inventory_path, line);
            return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int test_sample_manifest_entries_exist(void)
{
    if (validate_manifest_entries("samples/cblc/manifest.txt") != FT_SUCCESS)
        return (FT_FAILURE);
    if (validate_manifest_entries("samples/cobol/manifest.txt") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_sample_inventory_documents_manifest(void)
{
    if (validate_inventory_mentions("samples/cblc/manifest.txt", "docs/cblc_sample_inventory.md") != FT_SUCCESS)
        return (FT_FAILURE);
    if (validate_inventory_mentions("samples/cobol/manifest.txt", "docs/cobol_sample_inventory.md") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_sample_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"sample_manifest_entries_exist", test_sample_manifest_entries_exist},
        {"sample_inventory_documents_manifest", test_sample_inventory_documents_manifest}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
