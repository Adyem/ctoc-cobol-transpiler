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

static int sample_validate_unique_functions(const char *sample_path, const char *sample_source)
{
    char function_names[16][64];
    size_t function_count;
    size_t offset;
    size_t length;
    const char *needle;
    size_t needle_length;

    if (!sample_path)
        return (FT_FAILURE);
    if (!sample_source)
        return (FT_FAILURE);
    function_count = 0;
    offset = 0;
    needle = "function ";
    needle_length = ft_strlen(needle);
    length = ft_strlen(sample_source);
    while (offset < length)
    {
        size_t remaining;
        const char *found;
        size_t relative;
        size_t name_start;
        size_t name_length;
        char name[64];
        size_t index;

        remaining = length - offset;
        found = ft_strnstr(sample_source + offset, needle, remaining);
        if (!found)
            break;
        relative = static_cast<size_t>(found - sample_source);
        name_start = relative + needle_length;
        while (name_start < length && sample_source[name_start] == ' ')
            name_start++;
        if (name_start >= length)
        {
            pf_printf("Assertion failed: sample '%s' should declare a function name after keyword\n", sample_path);
            return (FT_FAILURE);
        }
        name_length = 0;
        while (name_start + name_length < length && sample_source[name_start + name_length] != '(')
        {
            if (sample_source[name_start + name_length] == ' ')
            {
                pf_printf("Assertion failed: sample '%s' should not include spaces in function names\n", sample_path);
                return (FT_FAILURE);
            }
            if (sample_source[name_start + name_length] == '\n')
            {
                pf_printf("Assertion failed: sample '%s' should keep function name on declaration line\n", sample_path);
                return (FT_FAILURE);
            }
            if (name_length + 1 >= sizeof(name))
            {
                pf_printf("Assertion failed: sample '%s' declares an unexpectedly long function name\n", sample_path);
                return (FT_FAILURE);
            }
            name[name_length] = sample_source[name_start + name_length];
            name_length++;
        }
        if (name_start + name_length >= length)
        {
            pf_printf("Assertion failed: sample '%s' should terminate function name with parentheses\n", sample_path);
            return (FT_FAILURE);
        }
        if (name_length == 0)
        {
            pf_printf("Assertion failed: sample '%s' should not omit function names\n", sample_path);
            return (FT_FAILURE);
        }
        name[name_length] = '\0';
        index = 0;
        while (index < function_count)
        {
            if (ft_strncmp(function_names[index], name, ft_strlen(name) + 1) == 0)
            {
                pf_printf("Assertion failed: sample '%s' should not declare duplicate function '%s'\n", sample_path, name);
                return (FT_FAILURE);
            }
            index++;
        }
        if (function_count >= sizeof(function_names) / sizeof(function_names[0]))
        {
            pf_printf("Assertion failed: sample '%s' declares more functions than expected\n", sample_path);
            return (FT_FAILURE);
        }
        index = 0;
        while (index <= name_length)
        {
            function_names[function_count][index] = name[index];
            index++;
        }
        function_count++;
        offset = name_start + name_length + 1;
    }
    return (FT_SUCCESS);
}

static int validate_cblc_sample_functions_unique(const char *manifest_path)
{
    char manifest_buffer[1024];
    char sample_buffer[4096];
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
            return (FT_FAILURE);
        if (sample_validate_unique_functions(line, sample_buffer) != FT_SUCCESS)
            return (FT_FAILURE);
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

static int test_cblc_samples_define_unique_functions(void)
{
    if (validate_cblc_sample_functions_unique("samples/cblc/manifest.txt") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_sample_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"sample_manifest_entries_exist", test_sample_manifest_entries_exist},
        {"sample_inventory_documents_manifest", test_sample_inventory_documents_manifest},
        {"cblc_samples_define_unique_functions", test_cblc_samples_define_unique_functions}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
