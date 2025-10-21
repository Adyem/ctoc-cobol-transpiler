#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

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
        total_length += ft_strlen(entries[index].source);
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

        length = ft_strlen(entries[index].source);
        if (length > 0)
        {
            ft_memcpy(buffer + offset, entries[index].source, length);
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
