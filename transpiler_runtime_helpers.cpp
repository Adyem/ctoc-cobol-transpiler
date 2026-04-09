#include "cblc_transpiler.hpp"

#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"

static const char g_runtime_helper_min_size[] =
    "static size_t cblc_min_size(size_t left, size_t right)\n"
    "{\n"
    "    if (left < right)\n"
    "        return (left);\n"
    "    return (right);\n"
    "}\n";

static const char g_runtime_helper_string_length[] =
    "static size_t cblc_string_length(const char *text)\n"
    "{\n"
    "    size_t length;\n"
    "\n"
    "    if (!text)\n"
    "        return (0);\n"
    "    length = 0;\n"
    "    while (text[length] != '\\0')\n"
    "        length += 1;\n"
    "    return (length);\n"
    "}\n";

static const char g_runtime_helper_string_assign_literal[] =
    "static void cblc_string_assign_literal(char *buffer, size_t capacity, size_t *length, const char *literal)\n"
    "{\n"
    "    size_t literal_length;\n"
    "    size_t index;\n"
    "\n"
    "    literal_length = cblc_string_length(literal);\n"
    "    if (literal_length > capacity)\n"
    "        literal_length = capacity;\n"
    "    index = 0;\n"
    "    while (index < literal_length)\n"
    "    {\n"
    "        buffer[index] = literal[index];\n"
    "        index += 1;\n"
    "    }\n"
    "    while (index < capacity)\n"
    "    {\n"
    "        buffer[index] = ' ';\n"
    "        index += 1;\n"
    "    }\n"
    "    if (length)\n"
    "        *length = literal_length;\n"
    "}\n";

static const char g_runtime_helper_string_copy[] =
    "static void cblc_string_copy(char *destination, size_t destination_capacity, size_t *destination_length, const char *source, size_t source_length)\n"
    "{\n"
    "    size_t copy_length;\n"
    "    size_t index;\n"
    "\n"
    "    copy_length = source_length;\n"
    "    if (copy_length > destination_capacity)\n"
    "        copy_length = destination_capacity;\n"
    "    index = 0;\n"
    "    while (index < copy_length)\n"
    "    {\n"
    "        destination[index] = source[index];\n"
    "        index += 1;\n"
    "    }\n"
    "    while (index < destination_capacity)\n"
    "    {\n"
    "        destination[index] = ' ';\n"
    "        index += 1;\n"
    "    }\n"
    "    if (destination_length)\n"
    "        *destination_length = copy_length;\n"
    "}\n";

static const char g_runtime_helper_string_append_literal[] =
    "static void cblc_string_append_literal(char *buffer, size_t capacity, size_t *length, const char *literal)\n"
    "{\n"
    "    size_t start;\n"
    "    size_t literal_length;\n"
    "    size_t write_length;\n"
    "    size_t index;\n"
    "\n"
    "    if (!buffer || !length)\n"
    "        return ;\n"
    "    start = *length;\n"
    "    if (start > capacity)\n"
    "        start = capacity;\n"
    "    literal_length = cblc_string_length(literal);\n"
    "    write_length = literal_length;\n"
    "    if (start + write_length > capacity)\n"
    "        write_length = capacity - start;\n"
    "    index = 0;\n"
    "    while (index < write_length)\n"
    "    {\n"
    "        buffer[start + index] = literal[index];\n"
    "        index += 1;\n"
    "    }\n"
    "    *length = start + write_length;\n"
    "    while (start + index < capacity)\n"
    "    {\n"
    "        buffer[start + index] = ' ';\n"
    "        index += 1;\n"
    "    }\n"
    "}\n";

static const char g_runtime_helper_string_append[] =
    "static void cblc_string_append(char *destination, size_t destination_capacity, size_t *destination_length, const char *source, size_t source_length)\n"
    "{\n"
    "    size_t start;\n"
    "    size_t copy_length;\n"
    "    size_t index;\n"
    "\n"
    "    if (!destination || !destination_length)\n"
    "        return ;\n"
    "    start = *destination_length;\n"
    "    if (start > destination_capacity)\n"
    "        start = destination_capacity;\n"
    "    copy_length = source_length;\n"
    "    if (start + copy_length > destination_capacity)\n"
    "        copy_length = destination_capacity - start;\n"
    "    index = 0;\n"
    "    while (index < copy_length)\n"
    "    {\n"
    "        destination[start + index] = source[index];\n"
    "        index += 1;\n"
    "    }\n"
    "    *destination_length = start + copy_length;\n"
    "    while (start + index < destination_capacity)\n"
    "    {\n"
    "        destination[start + index] = ' ';\n"
    "        index += 1;\n"
    "    }\n"
    "}\n";

static const char g_runtime_helper_string_equals[] =
    "static int cblc_string_equals(const char *left, size_t left_length, const char *right, size_t right_length)\n"
    "{\n"
    "    size_t index;\n"
    "\n"
    "    if (left_length != right_length)\n"
    "        return (0);\n"
    "    index = 0;\n"
    "    while (index < left_length)\n"
    "    {\n"
    "        if (left[index] != right[index])\n"
    "            return (0);\n"
    "        index += 1;\n"
    "    }\n"
    "    return (1);\n"
    "}\n";

static const char g_runtime_helper_string_starts_with[] =
    "static int cblc_string_starts_with(const char *text, size_t text_length, const char *prefix, size_t prefix_length)\n"
    "{\n"
    "    size_t index;\n"
    "\n"
    "    if (prefix_length > text_length)\n"
    "        return (0);\n"
    "    index = 0;\n"
    "    while (index < prefix_length)\n"
    "    {\n"
    "        if (text[index] != prefix[index])\n"
    "            return (0);\n"
    "        index += 1;\n"
    "    }\n"
    "    return (1);\n"
    "}\n";

static const char g_runtime_helper_string_ends_with[] =
    "static int cblc_string_ends_with(const char *text, size_t text_length, const char *suffix, size_t suffix_length)\n"
    "{\n"
    "    size_t start;\n"
    "    size_t index;\n"
    "\n"
    "    if (suffix_length > text_length)\n"
    "        return (0);\n"
    "    start = text_length - suffix_length;\n"
    "    index = 0;\n"
    "    while (index < suffix_length)\n"
    "    {\n"
    "        if (text[start + index] != suffix[index])\n"
    "            return (0);\n"
    "        index += 1;\n"
    "    }\n"
    "    return (1);\n"
    "}\n";

static const char g_runtime_helper_string_compare[] =
    "static int cblc_string_compare(const char *left, size_t left_length, const char *right, size_t right_length)\n"
    "{\n"
    "    size_t index;\n"
    "    size_t compare_length;\n"
    "\n"
    "    compare_length = cblc_min_size(left_length, right_length);\n"
    "    index = 0;\n"
    "    while (index < compare_length)\n"
    "    {\n"
    "        if (left[index] < right[index])\n"
    "            return (-1);\n"
    "        if (left[index] > right[index])\n"
    "            return (1);\n"
    "        index += 1;\n"
    "    }\n"
    "    if (left_length < right_length)\n"
    "        return (-1);\n"
    "    if (left_length > right_length)\n"
    "        return (1);\n"
    "    return (0);\n"
    "}\n";

static const char g_runtime_helper_string_contains[] =
    "static int cblc_string_contains(const char *text, size_t text_length, const char *needle, size_t needle_length)\n"
    "{\n"
    "    size_t start;\n"
    "    size_t index;\n"
    "\n"
    "    if (needle_length == 0)\n"
    "        return (1);\n"
    "    if (needle_length > text_length)\n"
    "        return (0);\n"
    "    start = 0;\n"
    "    while (start + needle_length <= text_length)\n"
    "    {\n"
    "        index = 0;\n"
    "        while (index < needle_length && text[start + index] == needle[index])\n"
    "            index += 1;\n"
    "        if (index == needle_length)\n"
    "            return (1);\n"
    "        start += 1;\n"
    "    }\n"
    "    return (0);\n"
    "}\n";

static const char g_runtime_helper_char_assign_literal[] =
    "static void cblc_char_assign_literal(char *buffer, size_t capacity, const char *literal)\n"
    "{\n"
    "    size_t literal_length;\n"
    "    size_t index;\n"
    "\n"
    "    literal_length = cblc_string_length(literal);\n"
    "    if (literal_length > capacity)\n"
    "        literal_length = capacity;\n"
    "    index = 0;\n"
    "    while (index < literal_length)\n"
    "    {\n"
    "        buffer[index] = literal[index];\n"
    "        index += 1;\n"
    "    }\n"
    "    while (index < capacity)\n"
    "    {\n"
    "        buffer[index] = ' ';\n"
    "        index += 1;\n"
    "    }\n"
    "}\n";

static const char g_runtime_helper_char_copy[] =
    "static void cblc_char_copy(char *destination, size_t destination_length, const char *source, size_t source_length)\n"
    "{\n"
    "    size_t copy_length;\n"
    "    size_t index;\n"
    "\n"
    "    copy_length = cblc_min_size(destination_length, source_length);\n"
    "    index = 0;\n"
    "    while (index < copy_length)\n"
    "    {\n"
    "        destination[index] = source[index];\n"
    "        index += 1;\n"
    "    }\n"
    "    while (index < destination_length)\n"
    "    {\n"
    "        destination[index] = ' ';\n"
    "        index += 1;\n"
    "    }\n"
    "}\n";

static const char g_runtime_helper_display_string[] =
    "static void cblc_display_string(const char *buffer, size_t length)\n"
    "{\n"
    "    size_t index;\n"
    "\n"
    "    if (!buffer)\n"
    "        return ;\n"
    "    index = 0;\n"
    "    while (index < length)\n"
    "    {\n"
    "        fputc(buffer[index], stdout);\n"
    "        index += 1;\n"
    "    }\n"
    "    fputc('\\n', stdout);\n"
    "}\n";

static const char g_runtime_helper_display_char_buffer[] =
    "static void cblc_display_char_buffer(const char *buffer, size_t length)\n"
    "{\n"
    "    size_t index;\n"
    "\n"
    "    if (!buffer)\n"
    "        return ;\n"
    "    index = 0;\n"
    "    while (index < length)\n"
    "    {\n"
    "        fputc(buffer[index], stdout);\n"
    "        index += 1;\n"
    "    }\n"
    "    fputc('\\n', stdout);\n"
    "}\n";

static const char g_runtime_helper_display_int[] =
    "static void cblc_display_int(int value)\n"
    "{\n"
    "    printf(\"%d\\n\", value);\n"
    "}\n";

static const char g_runtime_helper_display_size[] =
    "static void cblc_display_size(size_t value)\n"
    "{\n"
    "    printf(\"%zu\\n\", value);\n"
    "}\n";

static const char g_runtime_helper_display_literal[] =
    "static void cblc_display_literal(const char *literal)\n"
    "{\n"
    "    if (!literal)\n"
    "        return ;\n"
    "    printf(\"%s\\n\", literal);\n"
    "}\n";

static const t_transpiler_runtime_helper_entry g_runtime_helper_entries[] = {
    {"cblc_min_size", g_runtime_helper_min_size},
    {"cblc_string_length", g_runtime_helper_string_length},
    {"cblc_string_assign_literal", g_runtime_helper_string_assign_literal},
    {"cblc_string_copy", g_runtime_helper_string_copy},
    {"cblc_string_append_literal", g_runtime_helper_string_append_literal},
    {"cblc_string_append", g_runtime_helper_string_append},
    {"cblc_string_equals", g_runtime_helper_string_equals},
    {"cblc_string_starts_with", g_runtime_helper_string_starts_with},
    {"cblc_string_ends_with", g_runtime_helper_string_ends_with},
    {"cblc_string_compare", g_runtime_helper_string_compare},
    {"cblc_string_contains", g_runtime_helper_string_contains},
    {"cblc_char_assign_literal", g_runtime_helper_char_assign_literal},
    {"cblc_char_copy", g_runtime_helper_char_copy},
    {"cblc_display_string", g_runtime_helper_display_string},
    {"cblc_display_char_buffer", g_runtime_helper_display_char_buffer},
    {"cblc_display_int", g_runtime_helper_display_int},
    {"cblc_display_size", g_runtime_helper_display_size},
    {"cblc_display_literal", g_runtime_helper_display_literal}
};

const t_transpiler_runtime_helper_entry *transpiler_runtime_helpers_get_entries(size_t *count)
{
    if (count)
        *count = sizeof(g_runtime_helper_entries) / sizeof(g_runtime_helper_entries[0]);
    return (g_runtime_helper_entries);
}

int transpiler_runtime_helpers_render_c_source(char **out_text)
{
    const t_transpiler_runtime_helper_entry *entries;
    size_t entry_count;
    size_t total_length;
    size_t index;
    size_t offset;
    char *buffer;

    if (!out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    entries = transpiler_runtime_helpers_get_entries(&entry_count);
    total_length = 0;
    index = 0;
    while (index < entry_count)
    {
        total_length += std::strlen(entries[index].source);
        if (index + 1 < entry_count)
            total_length += 1;
        index += 1;
    }
    buffer = static_cast<char *>(cma_calloc(total_length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    offset = 0;
    index = 0;
    while (index < entry_count)
    {
        size_t length;

        length = std::strlen(entries[index].source);
        if (length > 0)
        {
            std::memcpy(buffer + offset, entries[index].source, length);
            offset += length;
        }
        if (index + 1 < entry_count)
        {
            buffer[offset] = '\n';
            offset += 1;
        }
        index += 1;
    }
    buffer[offset] = '\0';
    *out_text = buffer;
    return (FT_SUCCESS);
}
