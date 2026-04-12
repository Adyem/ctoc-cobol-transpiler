#include "cblc_transpiler.hpp"

#include <cstdarg>

#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

typedef struct s_c_backend_buffer
{
    char *data;
    size_t length;
    size_t capacity;
}   t_c_backend_buffer;

static int c_backend_translate_expression(const t_cblc_translation_unit *unit, const char *expression,
    char *buffer, size_t buffer_size);

static void c_backend_buffer_init(t_c_backend_buffer *buffer)
{
    if (!buffer)
        return ;
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static void c_backend_buffer_dispose(t_c_backend_buffer *buffer)
{
    if (!buffer)
        return ;
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static int c_backend_buffer_reserve(t_c_backend_buffer *buffer, size_t desired_capacity)
{
    char *new_data;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 64)
        desired_capacity = 64;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (buffer->data && buffer->length > 0)
        std::memcpy(new_data, buffer->data, buffer->length);
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = new_data;
    buffer->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int c_backend_buffer_append_span(t_c_backend_buffer *buffer, const char *text, size_t length)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (c_backend_buffer_reserve(buffer, buffer->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memcpy(buffer->data + buffer->length, text, length);
    buffer->length += length;
    buffer->data[buffer->length] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_buffer_append_string(t_c_backend_buffer *buffer, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (c_backend_buffer_append_span(buffer, text, std::strlen(text)));
}

static int c_backend_buffer_append_vformat(t_c_backend_buffer *buffer, const char *format, va_list args)
{
    va_list copy;
    char stack_buffer[256];
    char *heap_buffer;
    int required_length;
    int status;

    if (!buffer)
        return (FT_FAILURE);
    if (!format)
        return (FT_FAILURE);
    va_copy(copy, args);
    required_length = std::vsnprintf(stack_buffer, sizeof(stack_buffer), format, copy);
    va_end(copy);
    if (required_length < 0)
    {
        return (FT_FAILURE);
    }
    if (static_cast<size_t>(required_length) < sizeof(stack_buffer))
    {
        status = c_backend_buffer_append_span(buffer, stack_buffer,
            static_cast<size_t>(required_length));
        return (status);
    }
    heap_buffer = static_cast<char *>(cma_calloc(static_cast<size_t>(required_length) + 1, sizeof(char)));
    if (!heap_buffer)
        return (FT_FAILURE);
    if (std::vsnprintf(heap_buffer, static_cast<size_t>(required_length) + 1, format, args) < 0)
    {
        cma_free(heap_buffer);
        return (FT_FAILURE);
    }
    status = c_backend_buffer_append_span(buffer, heap_buffer,
        static_cast<size_t>(required_length));
    cma_free(heap_buffer);
    return (status);
}

static int c_backend_buffer_append_line(t_c_backend_buffer *buffer, const char *text)
{
    if (c_backend_buffer_append_string(buffer, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_buffer_append_string(buffer, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_is_builtin_string_type_name(const char *type_name)
{
    if (!type_name)
        return (0);
    return (std::strncmp(type_name, "string", TRANSPILE_IDENTIFIER_MAX) == 0);
}

static int c_backend_is_pointer_kind(t_cblc_data_kind kind)
{
    if (kind == CBLC_DATA_KIND_VOID_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_INT_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_STRUCT_POINTER)
        return (1);
    return (0);
}

static int c_backend_pointer_element_kind(t_cblc_data_kind kind, t_cblc_data_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_CHAR;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_INT_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_INT;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int c_backend_pointer_type_name(const t_cblc_data_item *item, char *buffer,
    size_t buffer_size)
{
    if (!item || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_VOID_POINTER)
        return (std::snprintf(buffer, buffer_size, "void *") < 0 ? FT_FAILURE : FT_SUCCESS);
    if (item->kind == CBLC_DATA_KIND_CHAR_POINTER)
        return (std::snprintf(buffer, buffer_size, "char *") < 0 ? FT_FAILURE : FT_SUCCESS);
    if (item->kind == CBLC_DATA_KIND_INT_POINTER)
        return (std::snprintf(buffer, buffer_size, "int *") < 0 ? FT_FAILURE : FT_SUCCESS);
    if (item->kind == CBLC_DATA_KIND_STRUCT_POINTER && item->struct_type_name[0] != '\0')
    {
        if (std::snprintf(buffer, buffer_size, "t_%s *", item->struct_type_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int c_backend_buffer_append_format_line(t_c_backend_buffer *buffer, const char *format, ...)
{
    va_list args;
    int status;

    if (!buffer)
        return (FT_FAILURE);
    if (!format)
        return (FT_FAILURE);
    va_start(args, format);
    status = c_backend_buffer_append_vformat(buffer, format, args);
    va_end(args);
    if (status != FT_SUCCESS)
        return (status);
    return (c_backend_buffer_append_string(buffer, "\n"));
}

static const t_cblc_data_item *c_backend_find_data_item_by_cobol(const t_cblc_translation_unit *unit,
    const char *cobol_name)
{
    size_t index;
    const t_cblc_data_item *alias_match;

    if (!unit || !cobol_name)
        return (NULL);
    alias_match = NULL;
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].cobol_name, cobol_name,
                sizeof(unit->data_items[index].cobol_name)) == 0)
        {
            if (!unit->data_items[index].is_active)
            {
                index += 1;
                continue ;
            }
            if (!unit->data_items[index].is_alias)
                return (&unit->data_items[index]);
            if (!alias_match)
                alias_match = &unit->data_items[index];
        }
        index += 1;
    }
    return (alias_match);
}

static const t_cblc_data_item *c_backend_find_data_item(const t_cblc_translation_unit *unit,
    const char *source_name)
{
    size_t index;
    const t_cblc_data_item *alias_match;

    if (!unit || !source_name)
        return (NULL);
    alias_match = NULL;
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].source_name, source_name,
                sizeof(unit->data_items[index].source_name)) == 0
            && unit->data_items[index].is_active)
        {
            if (!unit->data_items[index].is_alias)
                return (&unit->data_items[index]);
            if (!alias_match)
                alias_match = &unit->data_items[index];
        }
        index += 1;
    }
    return (alias_match);
}

static const t_cblc_struct_type *c_backend_find_struct_type(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (std::strncmp(unit->struct_types[index].source_name, identifier,
                sizeof(unit->struct_types[index].source_name)) == 0)
            return (&unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_data_item *c_backend_find_data_item_by_source(
    const t_cblc_translation_unit *unit, const char *source_name)
{
    size_t index;
    const t_cblc_data_item *alias_match;

    if (!unit || !source_name)
        return (NULL);
    alias_match = NULL;
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].source_name, source_name,
                sizeof(unit->data_items[index].source_name)) == 0)
        {
            if (!unit->data_items[index].is_active)
            {
                index += 1;
                continue ;
            }
            if (!unit->data_items[index].is_alias)
                return (&unit->data_items[index]);
            if (!alias_match)
                alias_match = &unit->data_items[index];
        }
        index += 1;
    }
    return (alias_match);
}

static int c_backend_has_function_owner(const t_cblc_translation_unit *unit, const char *owner_name)
{
    size_t index;

    if (!unit || !owner_name || owner_name[0] == '\0')
        return (0);
    index = 0;
    while (index < unit->function_count)
    {
        if (std::strncmp(unit->functions[index].source_name, owner_name,
                sizeof(unit->functions[index].source_name)) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int c_backend_strip_suffix(const char *text, const char *suffix, char *buffer, size_t buffer_size)
{
    size_t text_length;
    size_t suffix_length;

    if (!text || !suffix || !buffer || buffer_size == 0)
        return (0);
    text_length = std::strlen(text);
    suffix_length = std::strlen(suffix);
    if (suffix_length > text_length)
        return (0);
    if (std::strncmp(text + text_length - suffix_length, suffix, suffix_length) != 0)
        return (0);
    if (text_length - suffix_length + 1 > buffer_size)
        return (0);
    if (text_length - suffix_length > 0)
        std::memcpy(buffer, text, text_length - suffix_length);
    buffer[text_length - suffix_length] = '\0';
    return (1);
}

static int c_backend_parse_cobol_reference_token(const char *token, char *base_name,
    size_t base_name_size, char *suffix, size_t suffix_size, int *has_index,
    size_t *one_based_index)
{
    const char *open_paren;
    size_t head_length;
    size_t value;
    char head[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!token || !base_name || base_name_size == 0 || !suffix || suffix_size == 0
        || !has_index || !one_based_index)
        return (FT_FAILURE);
    open_paren = std::strchr(token, '(');
    *has_index = 0;
    *one_based_index = 0;
    if (open_paren)
    {
        const char *cursor;

        head_length = static_cast<size_t>(open_paren - token);
        cursor = open_paren + 1;
        if (*cursor < '0' || *cursor > '9')
            return (FT_FAILURE);
        value = 0;
        while (*cursor >= '0' && *cursor <= '9')
        {
            value = value * 10 + static_cast<size_t>(*cursor - '0');
            cursor += 1;
        }
        if (*cursor != ')' || cursor[1] != '\0' || value == 0)
            return (FT_FAILURE);
        *has_index = 1;
        *one_based_index = value;
    }
    else
        head_length = std::strlen(token);
    if (head_length + 1 >= sizeof(head))
        return (FT_FAILURE);
    std::memcpy(head, token, head_length);
    head[head_length] = '\0';
    suffix[0] = '\0';
    if (c_backend_strip_suffix(head, "-LEN", base_name, base_name_size))
    {
        ft_strlcpy(suffix, "-LEN", suffix_size);
        return (FT_SUCCESS);
    }
    if (c_backend_strip_suffix(head, "-BUF", base_name, base_name_size))
    {
        ft_strlcpy(suffix, "-BUF", suffix_size);
        return (FT_SUCCESS);
    }
    ft_strlcpy(base_name, head, base_name_size);
    return (FT_SUCCESS);
}

static int c_backend_parse_expression_indexed_token(const char *token, char *base_name,
    size_t base_name_size, char *suffix, size_t suffix_size, char *index_expression,
    size_t index_expression_size)
{
    const char *open_bracket;
    size_t head_length;
    size_t expr_length;
    char head[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!token || !base_name || base_name_size == 0 || !suffix || suffix_size == 0
        || !index_expression || index_expression_size == 0)
        return (FT_FAILURE);
    open_bracket = std::strchr(token, '[');
    if (!open_bracket)
        return (FT_FAILURE);
    head_length = static_cast<size_t>(open_bracket - token);
    expr_length = std::strcspn(open_bracket + 1, "]");
    if (open_bracket[1 + expr_length] != ']' || open_bracket[2 + expr_length] != '\0')
        return (FT_FAILURE);
    if (head_length + 1 >= sizeof(head) || expr_length + 1 > index_expression_size)
        return (FT_FAILURE);
    std::memcpy(head, token, head_length);
    head[head_length] = '\0';
    std::memcpy(index_expression, open_bracket + 1, expr_length);
    index_expression[expr_length] = '\0';
    suffix[0] = '\0';
    if (c_backend_strip_suffix(head, "-LEN", base_name, base_name_size))
    {
        ft_strlcpy(suffix, "-LEN", suffix_size);
        return (FT_SUCCESS);
    }
    if (c_backend_strip_suffix(head, "-BUF", base_name, base_name_size))
    {
        ft_strlcpy(suffix, "-BUF", suffix_size);
        return (FT_SUCCESS);
    }
    ft_strlcpy(base_name, head, base_name_size);
    return (FT_SUCCESS);
}

static int c_backend_decode_cobol_literal(const char *literal, char *buffer, size_t buffer_size)
{
    size_t length;
    size_t index;
    size_t output_index;

    if (!literal || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    length = std::strlen(literal);
    if (length < 2)
        return (FT_FAILURE);
    if (literal[0] != '"' || literal[length - 1] != '"')
        return (FT_FAILURE);
    output_index = 0;
    index = 1;
    while (index + 1 < length)
    {
        char character;

        character = literal[index];
        if (character == '"' && literal[index + 1] == '"')
        {
            if (output_index + 1 >= buffer_size)
                return (FT_FAILURE);
            buffer[output_index] = '"';
            output_index += 1;
            index += 2;
            continue ;
        }
        if (output_index + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[output_index] = character;
        output_index += 1;
        index += 1;
    }
    if (output_index >= buffer_size)
        return (FT_FAILURE);
    buffer[output_index] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_encode_c_string(const char *input, char *output, size_t output_size)
{
    size_t index;
    size_t out_index;

    if (!input || !output || output_size == 0)
        return (FT_FAILURE);
    index = 0;
    out_index = 0;
    while (input[index] != '\0')
    {
        char character;

        character = input[index];
        if (character == '\\' || character == '"')
        {
            if (out_index + 2 >= output_size)
                return (FT_FAILURE);
            output[out_index] = '\\';
            output[out_index + 1] = character;
            out_index += 2;
        }
        else if (character == '\n')
        {
            if (out_index + 2 >= output_size)
                return (FT_FAILURE);
            output[out_index] = '\\';
            output[out_index + 1] = 'n';
            out_index += 2;
        }
        else if (character == '\r')
        {
            if (out_index + 2 >= output_size)
                return (FT_FAILURE);
            output[out_index] = '\\';
            output[out_index + 1] = 'r';
            out_index += 2;
        }
        else if (character == '\t')
        {
            if (out_index + 2 >= output_size)
                return (FT_FAILURE);
            output[out_index] = '\\';
            output[out_index + 1] = 't';
            out_index += 2;
        }
        else
        {
            if (out_index + 1 >= output_size)
                return (FT_FAILURE);
            output[out_index] = character;
            out_index += 1;
        }
        index += 1;
    }
    if (out_index >= output_size)
        return (FT_FAILURE);
    output[out_index] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_expression_append(char *buffer, size_t buffer_size, const char *token)
{
    size_t length;
    size_t token_length;

    if (!buffer || !token)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length > 0)
    {
        if (length + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[length] = ' ';
        length += 1;
        buffer[length] = '\0';
    }
    token_length = std::strlen(token);
    if (length + token_length >= buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer + length, token, token_length);
    buffer[length + token_length] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_map_identifier_to_c(const t_cblc_translation_unit *unit, const char *token,
    char *buffer, size_t buffer_size)
{
    const t_cblc_data_item *item;
    char base[TRANSPILE_IDENTIFIER_MAX];
    char suffix[16];
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];
    int has_index;
    size_t one_based_index;

    if (!unit || !token || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (token[0] == '&' || token[0] == '*')
    {
        char mapped[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_map_identifier_to_c(unit, token + 1, mapped, sizeof(mapped)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(buffer, buffer_size, "%c%s", token[0], mapped) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (c_backend_parse_expression_indexed_token(token, base, sizeof(base), suffix, sizeof(suffix),
            index_expression, sizeof(index_expression)) == FT_SUCCESS)
    {
        item = c_backend_find_data_item_by_cobol(unit, base);
        if (!item)
            return (FT_FAILURE);
        if (c_backend_translate_expression(unit, index_expression, translated_index,
                sizeof(translated_index)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (suffix[0] != '\0')
        {
            if (item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (std::strncmp(suffix, "-LEN", sizeof(suffix)) == 0)
            {
                if (std::snprintf(buffer, buffer_size, "%s[%s].len", item->source_name,
                        translated_index) < 0)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (std::strncmp(suffix, "-BUF", sizeof(suffix)) == 0)
            {
                if (std::snprintf(buffer, buffer_size, "%s[%s].buf", item->source_name,
                        translated_index) < 0)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            return (FT_FAILURE);
        }
        if (std::snprintf(buffer, buffer_size, "%s[%s]", item->source_name,
                translated_index) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (c_backend_parse_cobol_reference_token(token, base, sizeof(base), suffix, sizeof(suffix),
            &has_index, &one_based_index) == FT_SUCCESS
        && (suffix[0] != '\0' || has_index))
    {
        item = c_backend_find_data_item_by_cobol(unit, base);
        if (!item)
            return (FT_FAILURE);
        if (suffix[0] != '\0')
        {
            if (item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (std::strncmp(suffix, "-LEN", sizeof(suffix)) == 0)
            {
                if (has_index)
                {
                    if (std::snprintf(buffer, buffer_size, "%s[%zu].len", item->source_name,
                            one_based_index - 1) < 0)
                        return (FT_FAILURE);
                }
                else if (std::strchr(item->source_name, '.'))
                {
                    if (std::snprintf(buffer, buffer_size, "%s.len", item->source_name) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(buffer, buffer_size, "%s_len", item->source_name) < 0)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (std::strncmp(suffix, "-BUF", sizeof(suffix)) == 0)
            {
                if (has_index)
                {
                    if (std::snprintf(buffer, buffer_size, "%s[%zu].buf", item->source_name,
                            one_based_index - 1) < 0)
                        return (FT_FAILURE);
                }
                else if (std::strchr(item->source_name, '.'))
                {
                    if (std::snprintf(buffer, buffer_size, "%s.buf", item->source_name) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(buffer, buffer_size, "%s_buf", item->source_name) < 0)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            return (FT_FAILURE);
        }
        if (has_index)
        {
            if (std::snprintf(buffer, buffer_size, "%s[%zu]", item->source_name,
                    one_based_index - 1) < 0)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
    }
    item = c_backend_find_data_item_by_cobol(unit, token);
    if (item)
    {
        if (std::snprintf(buffer, buffer_size, "%s", item->source_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (std::snprintf(buffer, buffer_size, "%s", token) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_translate_pointer_cast_expression(const t_cblc_translation_unit *unit,
    const char *expression, char *buffer, size_t buffer_size)
{
    const char *cursor;
    const char *type_text;
    char identifier[TRANSPILE_STATEMENT_TEXT_MAX];
    char mapped_identifier[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;

    if (!unit || !expression || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    type_text = NULL;
    if (std::strncmp(expression, "(void *)", 8) == 0)
        type_text = "void *";
    else if (std::strncmp(expression, "(char *)", 8) == 0)
        type_text = "char *";
    else if (std::strncmp(expression, "(int *)", 7) == 0)
        type_text = "int *";
    if (!type_text)
        return (FT_FAILURE);
    cursor = expression + std::strlen(type_text) + 2;
    while (*cursor == ' ' || *cursor == '\t')
        cursor += 1;
    length = std::strlen(cursor);
    if (length == 0 || length + 1 > sizeof(identifier))
        return (FT_FAILURE);
    std::memcpy(identifier, cursor, length);
    identifier[length] = '\0';
    if (c_backend_map_identifier_to_c(unit, identifier, mapped_identifier,
            sizeof(mapped_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "(%s)%s", type_text, mapped_identifier) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_build_string_len_ref(const t_cblc_data_item *item, char *buffer, size_t buffer_size)
{
    if (!item || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::strchr(item->source_name, '.'))
    {
        if (std::snprintf(buffer, buffer_size, "%s.len", item->source_name) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(buffer, buffer_size, "%s_len", item->source_name) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_build_string_buf_ref(const t_cblc_data_item *item, char *buffer, size_t buffer_size)
{
    if (!item || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::strchr(item->source_name, '.'))
    {
        if (std::snprintf(buffer, buffer_size, "%s.buf", item->source_name) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(buffer, buffer_size, "%s_buf", item->source_name) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_translate_expression(const t_cblc_translation_unit *unit, const char *expression,
    char *buffer, size_t buffer_size)
{
    size_t index;
    size_t expression_index;
    int has_operator;

    if (!buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    if (!unit || !expression)
        return (FT_FAILURE);
    if (expression[0] == '('
        && c_backend_translate_pointer_cast_expression(unit, expression, buffer,
            buffer_size) == FT_SUCCESS)
        return (FT_SUCCESS);
    if ((expression[0] == '&' || expression[0] == '*')
        && c_backend_map_identifier_to_c(unit, expression, buffer, buffer_size) == FT_SUCCESS)
        return (FT_SUCCESS);
    expression_index = 0;
    has_operator = 0;
    while (expression[expression_index] != '\0')
    {
        if (expression[expression_index] == ' '
            || expression[expression_index] == '+'
            || expression[expression_index] == '&'
            || expression[expression_index] == '*'
            || expression[expression_index] == '/')
        {
            has_operator = 1;
            break ;
        }
        if (expression[expression_index] == '-')
        {
            if (expression_index == 0
                || expression[expression_index - 1] == ' '
                || expression[expression_index + 1] == ' ')
            {
                has_operator = 1;
                break ;
            }
        }
        expression_index += 1;
    }
    if (!has_operator
        && c_backend_map_identifier_to_c(unit, expression, buffer, buffer_size) == FT_SUCCESS)
        return (FT_SUCCESS);
    index = 0;
    while (expression[index] != '\0')
    {
        char character;

        character = expression[index];
        if (character == ' ')
        {
            index += 1;
            continue ;
        }
        if (character == '+' || character == '-' || character == '*' || character == '/'
            || character == '&')
        {
            char operator_buffer[2];

            operator_buffer[0] = character;
            operator_buffer[1] = '\0';
            if (c_backend_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (character >= '0' && character <= '9')
        {
            size_t start;
            size_t length;
            char token[TRANSPILE_STATEMENT_TEXT_MAX];

            start = index;
            length = 0;
            while (expression[index] >= '0' && expression[index] <= '9')
            {
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[index];
                length += 1;
                index += 1;
            }
            token[length] = '\0';
            if (c_backend_expression_append(buffer, buffer_size, token) != FT_SUCCESS)
                return (FT_FAILURE);
            (void)start;
            continue ;
        }
        if ((character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')
            || character == '_')
        {
            char token[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t length;
            size_t token_index;
            int saw_paren;

            length = 0;
            token_index = index;
            saw_paren = 0;
            while ((expression[token_index] >= 'A' && expression[token_index] <= 'Z')
                || (expression[token_index] >= 'a' && expression[token_index] <= 'z')
                || (expression[token_index] >= '0' && expression[token_index] <= '9')
                || expression[token_index] == '-' || expression[token_index] == '_'
                || expression[token_index] == '.' || expression[token_index] == '>')
            {
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[token_index];
                length += 1;
                token_index += 1;
            }
            if (expression[token_index] == '[')
            {
                while (expression[token_index] != '\0' && expression[token_index] != ']')
                {
                    if (length + 1 >= sizeof(token))
                        return (FT_FAILURE);
                    token[length] = expression[token_index];
                    length += 1;
                    token_index += 1;
                }
                if (expression[token_index] != ']')
                    return (FT_FAILURE);
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[token_index];
                length += 1;
                token_index += 1;
            }
            if (expression[token_index] == '(')
            {
                saw_paren = 1;
                while (expression[token_index] != '\0' && expression[token_index] != ')')
                {
                    if (length + 1 >= sizeof(token))
                        return (FT_FAILURE);
                    token[length] = expression[token_index];
                    length += 1;
                    token_index += 1;
                }
                if (expression[token_index] != ')')
                    return (FT_FAILURE);
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[token_index];
                length += 1;
                token_index += 1;
            }
            if (saw_paren)
            {
                while ((expression[token_index] >= 'A' && expression[token_index] <= 'Z')
                    || (expression[token_index] >= 'a' && expression[token_index] <= 'z')
                    || (expression[token_index] == '-') || (expression[token_index] == '_')
                    || (expression[token_index] == '>'))
                {
                    if (length + 1 >= sizeof(token))
                        return (FT_FAILURE);
                    token[length] = expression[token_index];
                    length += 1;
                    token_index += 1;
                }
            }
            token[length] = '\0';
            if (c_backend_map_identifier_to_c(unit, token, token, sizeof(token)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_expression_append(buffer, buffer_size, token) != FT_SUCCESS)
                return (FT_FAILURE);
            index = token_index;
            continue ;
        }
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int c_backend_external_append(char (**out_array)[TRANSPILE_IDENTIFIER_MAX], size_t *count,
    size_t *capacity, const char *name)
{
    char (*array)[TRANSPILE_IDENTIFIER_MAX];
    size_t index;
    size_t new_capacity;
    char (*new_array)[TRANSPILE_IDENTIFIER_MAX];

    if (!out_array || !count || !capacity || !name)
        return (FT_FAILURE);
    array = *out_array;
    index = 0;
    while (index < *count)
    {
        if (std::strncmp(array[index], name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (FT_SUCCESS);
        index += 1;
    }
    if (*count >= *capacity)
    {
        new_capacity = *capacity == 0 ? 4 : *capacity * 2;
        new_array = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(new_capacity,
            sizeof(*new_array)));
        if (!new_array)
            return (FT_FAILURE);
        if (array && *count > 0)
            std::memcpy(new_array, array, *count * sizeof(*new_array));
        if (array)
            cma_free(array);
        *out_array = new_array;
        array = new_array;
        *capacity = new_capacity;
    }
    ft_strlcpy(array[*count], name, TRANSPILE_IDENTIFIER_MAX);
    *count += 1;
    return (FT_SUCCESS);
}

static int c_backend_emit_char_literal_assignment(const t_cblc_data_item *item, const char *literal,
    t_c_backend_buffer *buffer)
{
    char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
    char encoded[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;

    if (!item || !literal || !buffer)
        return (FT_FAILURE);
    if (c_backend_decode_cobol_literal(literal, decoded, sizeof(decoded)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
        return (FT_FAILURE);
    length = item->length;
    if (length == 0)
        length = 1;
    if (c_backend_buffer_append_format_line(buffer,
            "    cblc_char_assign_literal(%s, %zu, \"%s\");",
            item->source_name, length, encoded) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_char_pointer_element_literal_assignment(const char *target,
    const char *literal, t_c_backend_buffer *buffer)
{
    char decoded[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!target || !literal || !buffer)
        return (FT_FAILURE);
    if (c_backend_decode_cobol_literal(literal, decoded, sizeof(decoded)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (decoded[0] == '\0' || decoded[1] != '\0')
        return (FT_FAILURE);
    if (c_backend_buffer_append_format_line(buffer,
            "    %s = (char)%d;", target,
            static_cast<int>(static_cast<unsigned char>(decoded[0]))) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_assignment(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    const t_cblc_statement *next_statement, t_c_backend_buffer *buffer, size_t *consumed)
{
    char base[TRANSPILE_IDENTIFIER_MAX];
    char suffix[16];
    char mapped_target[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    const t_cblc_data_item *item;
    t_cblc_data_kind effective_kind;
    int has_index;
    int has_expression_index;
    int is_dereference_target;
    size_t one_based_index;

    if (!unit || !statement || !buffer || !consumed)
        return (FT_FAILURE);
    *consumed = 1;
    target_index_expression[0] = '\0';
    has_expression_index = 0;
    is_dereference_target = (statement->target[0] == '*');
    if (std::strstr(statement->target, "->"))
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_translate_expression(unit, statement->target, mapped_target,
                sizeof(mapped_target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (statement->is_literal)
            ft_strlcpy(expression, statement->source, sizeof(expression));
        else if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer, "    %s = %s;",
                mapped_target, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (is_dereference_target)
    {
        item = c_backend_find_data_item_by_cobol(unit, statement->target + 1);
        if (!item || c_backend_pointer_element_kind(item->kind, &effective_kind) != FT_SUCCESS)
            return (FT_FAILURE);
        has_index = 0;
        one_based_index = 0;
    }
    else if (std::strchr(statement->target, '['))
    {
        if (c_backend_parse_expression_indexed_token(statement->target, base, sizeof(base), suffix,
                sizeof(suffix), target_index_expression, sizeof(target_index_expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        has_index = 0;
        one_based_index = 0;
        has_expression_index = 1;
    }
    else if (c_backend_parse_cobol_reference_token(statement->target, base, sizeof(base), suffix,
                sizeof(suffix), &has_index, &one_based_index) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!is_dereference_target && std::strncmp(suffix, "-BUF", sizeof(suffix)) == 0)
    {
        char length_token[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_length[TRANSPILE_STATEMENT_TEXT_MAX];

        item = c_backend_find_data_item_by_cobol(unit, base);
        if (!item || item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (!statement->is_literal)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, statement->target, mapped_target,
                sizeof(mapped_target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (has_index)
        {
            if (std::snprintf(length_token, sizeof(length_token), "%s-LEN(%zu)", base,
                    one_based_index) < 0)
                return (FT_FAILURE);
        }
        else if (has_expression_index)
        {
            if (std::snprintf(length_token, sizeof(length_token), "%s-LEN[%s]", base,
                    target_index_expression) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(length_token, sizeof(length_token), "%s-LEN", base) < 0)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, length_token, mapped_length,
                sizeof(mapped_length)) != FT_SUCCESS)
            return (FT_FAILURE);
        {
            char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
            char encoded[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t capacity;

            if (c_backend_decode_cobol_literal(statement->source, decoded, sizeof(decoded)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
                return (FT_FAILURE);
            capacity = item->length;
            if (capacity == 0)
                capacity = 1;
            if (c_backend_buffer_append_format_line(buffer,
                    "    cblc_string_assign_literal(%s, %zu, &%s, \"%s\");",
                    mapped_target, capacity, mapped_length, encoded) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (next_statement && next_statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            char next_base[TRANSPILE_IDENTIFIER_MAX];
            char next_suffix[16];
            char next_index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
            int next_has_index;
            int next_has_expression_index;
            size_t next_one_based_index;

            next_has_expression_index = 0;
            next_index_expression[0] = '\0';
            if (std::strchr(next_statement->target, '['))
            {
                if (c_backend_parse_expression_indexed_token(next_statement->target, next_base,
                        sizeof(next_base), next_suffix, sizeof(next_suffix), next_index_expression,
                        sizeof(next_index_expression)) == FT_SUCCESS)
                {
                    next_has_index = 0;
                    next_one_based_index = 0;
                    next_has_expression_index = 1;
                }
            }
            else if (c_backend_parse_cobol_reference_token(next_statement->target, next_base,
                    sizeof(next_base), next_suffix, sizeof(next_suffix), &next_has_index,
                    &next_one_based_index) != FT_SUCCESS)
                next_suffix[0] = '\0';
            if (std::strncmp(next_suffix, "-LEN", sizeof(next_suffix)) == 0
                && std::strncmp(next_base, base, sizeof(next_base)) == 0)
            {
                if ((has_index && next_has_index && one_based_index == next_one_based_index)
                    || (has_expression_index && next_has_expression_index
                        && std::strncmp(target_index_expression, next_index_expression,
                            sizeof(target_index_expression)) == 0)
                    || (!has_index && !has_expression_index
                        && !next_has_index && !next_has_expression_index))
                    *consumed = 2;
            }
        }
        return (FT_SUCCESS);
    }
    if (!is_dereference_target && std::strncmp(suffix, "-LEN", sizeof(suffix)) == 0)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_data_item *length_item;

        length_item = c_backend_find_data_item_by_cobol(unit, base);
        if (!length_item || length_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, statement->target, mapped_target,
                sizeof(mapped_target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = %s;", mapped_target, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!is_dereference_target)
    {
        item = c_backend_find_data_item_by_cobol(unit, base);
        if (!item)
            item = c_backend_find_data_item_by_cobol(unit, statement->target);
        if (!item)
            return (FT_FAILURE);
        effective_kind = item->kind;
        if ((has_index || has_expression_index) && c_backend_is_pointer_kind(item->kind))
        {
            if (c_backend_pointer_element_kind(item->kind, &effective_kind) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    if (effective_kind == CBLC_DATA_KIND_STRING)
    {
        const t_cblc_data_item *source_item;
        char source_base[TRANSPILE_IDENTIFIER_MAX];
        char source_suffix[16];
        char target_buffer_token[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_length_token[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_buffer_token[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_length_token[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        int source_has_index;
        size_t source_one_based_index;

        if (c_backend_parse_cobol_reference_token(statement->source, source_base, sizeof(source_base),
                source_suffix, sizeof(source_suffix), &source_has_index, &source_one_based_index)
            != FT_SUCCESS || source_suffix[0] != '\0')
            return (FT_FAILURE);
        source_item = c_backend_find_data_item_by_cobol(unit, source_base);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (has_index)
        {
            if (std::snprintf(target_buffer_token, sizeof(target_buffer_token), "%s-BUF(%zu)", base,
                    one_based_index) < 0)
                return (FT_FAILURE);
            if (std::snprintf(target_length_token, sizeof(target_length_token), "%s-LEN(%zu)", base,
                    one_based_index) < 0)
                return (FT_FAILURE);
        }
        else
        {
            if (std::snprintf(target_buffer_token, sizeof(target_buffer_token), "%s-BUF", base) < 0)
                return (FT_FAILURE);
            if (std::snprintf(target_length_token, sizeof(target_length_token), "%s-LEN", base) < 0)
                return (FT_FAILURE);
        }
        if (source_has_index)
        {
            if (std::snprintf(source_buffer_token, sizeof(source_buffer_token), "%s-BUF(%zu)",
                    source_base, source_one_based_index) < 0)
                return (FT_FAILURE);
            if (std::snprintf(source_length_token, sizeof(source_length_token), "%s-LEN(%zu)",
                    source_base, source_one_based_index) < 0)
                return (FT_FAILURE);
        }
        else
        {
            if (std::snprintf(source_buffer_token, sizeof(source_buffer_token), "%s-BUF", source_base) < 0)
                return (FT_FAILURE);
            if (std::snprintf(source_length_token, sizeof(source_length_token), "%s-LEN", source_base) < 0)
                return (FT_FAILURE);
        }
        if (c_backend_map_identifier_to_c(unit, target_buffer_token, target_buffer_ref,
                sizeof(target_buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, target_length_token, target_length_ref,
                sizeof(target_length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, source_buffer_token, source_buffer_ref,
                sizeof(source_buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, source_length_token, source_length_ref,
                sizeof(source_length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    cblc_string_copy(%s, %zu, &%s, %s, %s);",
                target_buffer_ref, item->length, target_length_ref,
                source_buffer_ref, source_length_ref) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (effective_kind == CBLC_DATA_KIND_CHAR)
    {
        size_t length;

        if (is_dereference_target || has_index || has_expression_index)
        {
            const t_cblc_data_item *source_item;
            char expression[TRANSPILE_STATEMENT_TEXT_MAX];

            if (c_backend_map_identifier_to_c(unit, statement->target, mapped_target,
                    sizeof(mapped_target)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (statement->is_literal)
                return (c_backend_emit_char_pointer_element_literal_assignment(mapped_target,
                        statement->source, buffer));
            source_item = c_backend_find_data_item_by_cobol(unit, statement->source);
            if (!source_item && c_backend_translate_expression(unit, statement->source, expression,
                    sizeof(expression)) == FT_SUCCESS)
            {
                if (c_backend_buffer_append_format_line(buffer,
                        "    %s = %s;", mapped_target, expression) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer,
                    "    %s = %s;", mapped_target, source_item->source_name)
                != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        length = item->length;
        if (length == 0)
            length = 1;
        if (statement->is_literal)
            return (c_backend_emit_char_literal_assignment(item, statement->source, buffer));
        else
        {
            const t_cblc_data_item *source_item;
            char expression[TRANSPILE_STATEMENT_TEXT_MAX];

            if (statement->source[0] == '*')
            {
                if (c_backend_map_identifier_to_c(unit, statement->target, mapped_target,
                        sizeof(mapped_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_translate_expression(unit, statement->source, expression,
                        sizeof(expression)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    %s = %s;", mapped_target, expression) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            source_item = c_backend_find_data_item_by_cobol(unit, statement->source);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer,
                    "    cblc_char_copy(%s, %zu, %s, %zu);",
                    item->source_name, length, source_item->source_name, source_item->length)
                != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
    }
    if (effective_kind == CBLC_DATA_KIND_INT)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, statement->target, mapped_target,
                sizeof(mapped_target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = %s;", mapped_target, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (c_backend_is_pointer_kind(item->kind))
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_map_identifier_to_c(unit, statement->target, mapped_target,
                sizeof(mapped_target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (statement->is_literal
            && std::strncmp(statement->source, "NULL", sizeof(statement->source)) == 0)
        {
            if (c_backend_buffer_append_format_line(buffer,
                    "    %s = NULL;", mapped_target) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = %s;", mapped_target, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int c_backend_emit_compute(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    const t_cblc_data_item *item;
    t_cblc_data_kind effective_kind;
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char target[TRANSPILE_STATEMENT_TEXT_MAX];
    char base[TRANSPILE_IDENTIFIER_MAX];
    char suffix[16];
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    int is_dereference_target;
    int has_index;
    size_t one_based_index;

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    is_dereference_target = (statement->target[0] == '*');
    if (std::strstr(statement->target, "->"))
    {
        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_translate_expression(unit, statement->target, target,
                sizeof(target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = %s;", target, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (is_dereference_target)
    {
        item = c_backend_find_data_item_by_cobol(unit, statement->target + 1);
        if (!item || c_backend_pointer_element_kind(item->kind, &effective_kind) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (c_backend_parse_expression_indexed_token(statement->target, base, sizeof(base), suffix,
            sizeof(suffix), index_expression, sizeof(index_expression)) == FT_SUCCESS
        && suffix[0] == '\0')
        item = c_backend_find_data_item_by_cobol(unit, base);
    else if (c_backend_parse_cobol_reference_token(statement->target, base, sizeof(base), suffix,
            sizeof(suffix), &has_index, &one_based_index) == FT_SUCCESS
        && suffix[0] == '\0')
        item = c_backend_find_data_item_by_cobol(unit, base);
    else
        item = c_backend_find_data_item_by_cobol(unit, statement->target);
    if (!item)
        return (FT_FAILURE);
    if (!is_dereference_target)
    {
        effective_kind = item->kind;
        if (std::strchr(statement->target, '[') && c_backend_is_pointer_kind(item->kind))
        {
            if (c_backend_pointer_element_kind(item->kind, &effective_kind) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    if (effective_kind != CBLC_DATA_KIND_INT)
        return (FT_FAILURE);
    if (c_backend_translate_expression(unit, statement->source, expression,
            sizeof(expression)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_map_identifier_to_c(unit, statement->target, target, sizeof(target)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_buffer_append_format_line(buffer,
            "    %s = %s;", target, expression) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_display(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    char base[TRANSPILE_IDENTIFIER_MAX];
    char suffix[16];
    int has_index;
    size_t one_based_index;

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    if (statement->is_literal)
    {
        char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
        char encoded[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_decode_cobol_literal(statement->source, decoded, sizeof(decoded)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    cblc_display_literal(\"%s\");", encoded) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    else
    {
        char value_ref[TRANSPILE_STATEMENT_TEXT_MAX];

        if (statement->source[0] == '*')
        {
            const t_cblc_data_item *item;
            char mapped_ref[TRANSPILE_STATEMENT_TEXT_MAX];
            t_cblc_data_kind dereference_kind;

            item = c_backend_find_data_item_by_cobol(unit, statement->source + 1);
            if (!item || c_backend_pointer_element_kind(item->kind, &dereference_kind) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_map_identifier_to_c(unit, statement->source, mapped_ref,
                    sizeof(mapped_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (dereference_kind == CBLC_DATA_KIND_INT)
            {
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_int(%s);", mapped_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (dereference_kind == CBLC_DATA_KIND_CHAR)
            {
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_char_buffer(&%s, 1);", mapped_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            return (FT_FAILURE);
        }
        if (c_backend_parse_expression_indexed_token(statement->source, base, sizeof(base), suffix,
                sizeof(suffix), value_ref, sizeof(value_ref)) == FT_SUCCESS)
        {
            const t_cblc_data_item *item;
            char buffer_token[TRANSPILE_STATEMENT_TEXT_MAX];
            char length_token[TRANSPILE_STATEMENT_TEXT_MAX];
            char buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
            char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

            item = c_backend_find_data_item_by_cobol(unit, base);
            if (!item)
                return (FT_FAILURE);
            if (item->kind == CBLC_DATA_KIND_STRING && suffix[0] == '\0')
            {
                if (std::snprintf(buffer_token, sizeof(buffer_token), "%s-BUF[%s]", base, value_ref) < 0)
                    return (FT_FAILURE);
                if (std::snprintf(length_token, sizeof(length_token), "%s-LEN[%s]", base, value_ref) < 0)
                    return (FT_FAILURE);
                if (c_backend_map_identifier_to_c(unit, buffer_token, buffer_ref,
                        sizeof(buffer_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_map_identifier_to_c(unit, length_token, length_ref,
                        sizeof(length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_string(%s, %s);",
                        buffer_ref, length_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (item->kind == CBLC_DATA_KIND_STRING
                && std::strncmp(suffix, "-LEN", sizeof(suffix)) == 0)
            {
                char length_ref_expr[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_map_identifier_to_c(unit, statement->source, length_ref_expr,
                        sizeof(length_ref_expr)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_size(%s);", length_ref_expr) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (item->kind == CBLC_DATA_KIND_INT
                || item->kind == CBLC_DATA_KIND_INT_POINTER)
            {
                char int_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_map_identifier_to_c(unit, statement->source, int_ref,
                        sizeof(int_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_int(%s);", int_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
        }
        else if (c_backend_parse_cobol_reference_token(statement->source, base, sizeof(base), suffix,
                sizeof(suffix), &has_index, &one_based_index) == FT_SUCCESS)
        {
            const t_cblc_data_item *item;

            item = c_backend_find_data_item_by_cobol(unit, base);
            if (item && item->kind == CBLC_DATA_KIND_STRING && suffix[0] == '\0')
            {
                char buffer_token[TRANSPILE_STATEMENT_TEXT_MAX];
                char length_token[TRANSPILE_STATEMENT_TEXT_MAX];
                char buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
                char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                if (has_index)
                {
                    if (std::snprintf(buffer_token, sizeof(buffer_token), "%s-BUF(%zu)", base,
                            one_based_index) < 0)
                        return (FT_FAILURE);
                    if (std::snprintf(length_token, sizeof(length_token), "%s-LEN(%zu)", base,
                            one_based_index) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(buffer_token, sizeof(buffer_token), "%s-BUF", base) < 0)
                        return (FT_FAILURE);
                    if (std::snprintf(length_token, sizeof(length_token), "%s-LEN", base) < 0)
                        return (FT_FAILURE);
                }
                if (c_backend_map_identifier_to_c(unit, buffer_token, buffer_ref,
                        sizeof(buffer_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_map_identifier_to_c(unit, length_token, length_ref,
                        sizeof(length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_string(%s, %s);",
                        buffer_ref, length_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (item && item->kind == CBLC_DATA_KIND_STRING
                && std::strncmp(suffix, "-LEN", sizeof(suffix)) == 0)
            {
                char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_map_identifier_to_c(unit, statement->source, length_ref,
                        sizeof(length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_size(%s);", length_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (item && item->kind == CBLC_DATA_KIND_INT)
            {
                if (c_backend_map_identifier_to_c(unit, statement->source, value_ref,
                        sizeof(value_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_int(%s);", value_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (item && item->kind == CBLC_DATA_KIND_CHAR)
            {
                if (has_index)
                    return (FT_FAILURE);
                if (c_backend_map_identifier_to_c(unit, statement->source, value_ref,
                        sizeof(value_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_char_buffer(%s, %zu);",
                        value_ref, item->length) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
        }
        else
        {
            const t_cblc_data_item *item;

            item = c_backend_find_data_item_by_cobol(unit, statement->source);
            if (item)
            {
                if (item->kind == CBLC_DATA_KIND_INT)
                {
                    if (c_backend_buffer_append_format_line(buffer,
                            "    cblc_display_int(%s);", item->source_name) != FT_SUCCESS)
                        return (FT_FAILURE);
                    return (FT_SUCCESS);
                }
                if (item->kind == CBLC_DATA_KIND_CHAR)
                {
                    if (c_backend_buffer_append_format_line(buffer,
                            "    cblc_display_char_buffer(%s, %zu);",
                            item->source_name, item->length) != FT_SUCCESS)
                        return (FT_FAILURE);
                    return (FT_SUCCESS);
                }
            }
        }
    }
    return (FT_FAILURE);
}

static const t_cblc_function *c_backend_find_function(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->function_count)
    {
        if (std::strncmp(unit->functions[index].source_name, identifier,
                sizeof(unit->functions[index].source_name)) == 0)
            return (&unit->functions[index]);
        index += 1;
    }
    return (NULL);
}

static int c_backend_extract_call_argument(const t_cblc_statement *statement,
    size_t argument_index, char *buffer, size_t buffer_size)
{
    const char *cursor;
    size_t current_index;
    size_t length;

    if (!statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    cursor = statement->call_arguments;
    current_index = 0;
    while (*cursor != '\0' && current_index < argument_index)
    {
        while (*cursor != '\0' && *cursor != ',')
            cursor += 1;
        if (*cursor == ',')
        {
            cursor += 1;
            current_index += 1;
        }
    }
    if (current_index != argument_index)
        return (FT_FAILURE);
    while (*cursor == ' ' || *cursor == '\t')
        cursor += 1;
    length = 0;
    while (cursor[length] != '\0' && cursor[length] != ',')
        length += 1;
    while (length > 0
        && (cursor[length - 1] == ' ' || cursor[length - 1] == '\t'))
        length -= 1;
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, cursor, length);
    buffer[length] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_emit_parameter_argument_moves(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count,
    const t_cblc_parameter *parameters, size_t parameter_count,
    t_c_backend_buffer *buffer)
{
    size_t index;
    t_cblc_statement statement;

    if (!unit || !call_arguments || !parameters || !buffer)
        return (FT_FAILURE);
    if (parameter_count != call_argument_count)
        return (FT_FAILURE);
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = call_argument_count;
    index = 0;
    while (index < parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_extract_call_argument(&statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        {
            const t_cblc_data_item *item;

            item = c_backend_find_data_item_by_source(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer, "    %s = %s;",
                    parameters[index].actual_source_name, item->source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
        {
            const t_cblc_data_item *item;
            const t_cblc_data_item *target_item;
            char target_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
            char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
            char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

            item = c_backend_find_data_item_by_source(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            target_item = c_backend_find_data_item_by_source(unit, parameters[index].actual_source_name);
            if (!target_item || target_item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (c_backend_build_string_buf_ref(target_item, target_buffer_ref,
                    sizeof(target_buffer_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_build_string_len_ref(target_item, target_length_ref,
                    sizeof(target_length_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_build_string_buf_ref(item, source_buffer_ref,
                    sizeof(source_buffer_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_build_string_len_ref(item, source_length_ref,
                    sizeof(source_length_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer,
                    "    cblc_string_assign(%s, %zu, &%s, %s, %s);",
                    target_buffer_ref, target_item->length, target_length_ref,
                    source_buffer_ref, source_length_ref) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (c_backend_translate_expression(unit, argument, expression,
                    sizeof(expression)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer, "    %s = %s;",
                    parameters[index].actual_source_name,
                    expression) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static const t_cblc_constructor *c_backend_find_constructor_for_statement(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const t_cblc_statement *statement)
{
    size_t constructor_index;

    if (!unit || !type || !statement)
        return (NULL);
    constructor_index = 0;
    while (constructor_index < type->constructor_count)
    {
        const t_cblc_constructor *constructor;
        size_t parameter_index;
        int matches;

        constructor = &type->constructors[constructor_index];
        if (constructor->parameter_count != statement->call_argument_count)
        {
            constructor_index += 1;
            continue ;
        }
        parameter_index = 0;
        matches = 1;
        while (parameter_index < constructor->parameter_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_data_item *item;

            if (c_backend_extract_call_argument(statement, parameter_index, argument,
                    sizeof(argument)) != FT_SUCCESS)
            {
                matches = 0;
                break ;
            }
            if (constructor->parameters[parameter_index].kind
                == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            {
                item = c_backend_find_data_item_by_source(unit, argument);
                if (!item || item->kind != CBLC_DATA_KIND_STRUCT
                    || std::strncmp(item->struct_type_name,
                        constructor->parameters[parameter_index].type_name,
                        sizeof(item->struct_type_name)) != 0)
                {
                    matches = 0;
                    break ;
                }
            }
            else
            {
                item = c_backend_find_data_item_by_source(unit, argument);
                if (item && item->kind == CBLC_DATA_KIND_STRUCT)
                {
                    matches = 0;
                    break ;
                }
            }
            parameter_index += 1;
        }
        if (matches)
            return (constructor);
        constructor_index += 1;
    }
    return (NULL);
}

static int c_backend_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    if (!unit || !target_function || !statement || !buffer)
        return (FT_FAILURE);
    return (c_backend_emit_parameter_argument_moves(unit, statement->call_arguments,
            statement->call_argument_count, target_function->parameters,
            target_function->parameter_count, buffer));
}

static int c_backend_build_external_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!unit || !statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    index = 0;
    while (index < statement->call_argument_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_extract_call_argument(statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_translate_expression(unit, argument, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (index > 0)
            ft_strlcat(buffer, ", ", buffer_size);
        ft_strlcat(buffer, expression, buffer_size);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int c_backend_emit_call(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_c_backend_buffer *buffer)
{
    const t_cblc_function *target_function;
    char arguments[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    if (std::strncmp(statement->call_identifier, "std::free",
            sizeof(statement->call_identifier)) == 0)
    {
        if (statement->call_argument_count != 1)
            return (FT_FAILURE);
        if (c_backend_build_external_call_arguments(unit, statement, arguments,
                sizeof(arguments)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    free(%s);", arguments) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!statement->call_is_external)
    {
        target_function = c_backend_find_function(unit, statement->call_identifier);
        if (!target_function)
            return (FT_FAILURE);
        if (c_backend_emit_local_call_argument_moves(unit, target_function,
                statement, buffer) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!statement || !buffer)
        return (FT_FAILURE);
    if (statement->call_is_external && statement->call_argument_count > 0)
    {
        if (c_backend_build_external_call_arguments(unit, statement, arguments,
                sizeof(arguments)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s(%s);", statement->call_identifier, arguments) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (c_backend_buffer_append_format_line(buffer,
            "    %s();", statement->call_identifier) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_call_assignment(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_c_backend_buffer *buffer)
{
    const t_cblc_function *target_function;
    char target[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    if (statement->call_identifier[0] == '\0')
        return (FT_FAILURE);
    if (std::strncmp(statement->call_identifier, "std::malloc",
            sizeof(statement->call_identifier)) == 0
        || std::strncmp(statement->call_identifier, "std::realloc",
            sizeof(statement->call_identifier)) == 0)
    {
        const t_cblc_data_item *item;
        char arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char pointer_type[TRANSPILE_IDENTIFIER_MAX];

        item = c_backend_find_data_item_by_cobol(unit, statement->target);
        if (!item || !c_backend_is_pointer_kind(item->kind))
            return (FT_FAILURE);
        if (c_backend_map_identifier_to_c(unit, statement->target, target,
                sizeof(target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_external_call_arguments(unit, statement, arguments,
                sizeof(arguments)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_pointer_type_name(item, pointer_type, sizeof(pointer_type))
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(statement->call_identifier, "std::malloc",
                sizeof(statement->call_identifier)) == 0)
        {
            if (c_backend_buffer_append_format_line(buffer,
                    "    %s = (%s)malloc(%s);", target, pointer_type, arguments) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = (%s)realloc(%s);", target, pointer_type, arguments) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    target_function = c_backend_find_function(unit, statement->call_identifier);
    if (!target_function)
        return (FT_FAILURE);
    if (c_backend_emit_local_call_argument_moves(unit, target_function,
            statement, buffer) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_map_identifier_to_c(unit, statement->target, target,
            sizeof(target)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_buffer_append_format_line(buffer, "    %s = %s();", target,
            statement->call_identifier) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_method(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_c_backend_buffer *buffer);

static int c_backend_emit_return(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    if (!function || !statement || !buffer)
        return (FT_FAILURE);
    if (function->return_kind == CBLC_FUNCTION_RETURN_VOID)
    {
        if (c_backend_buffer_append_line(buffer, "    return ;") != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!unit)
        return (FT_FAILURE);
    if (statement->source[0] == '\0')
        return (FT_FAILURE);
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer, "    return (%s);",
                expression) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int c_backend_emit_lifecycle_recursive(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *target, t_c_backend_buffer *buffer)
{
    size_t index;

    if (!unit || !type || !target || !buffer)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char member_target[TRANSPILE_STATEMENT_TEXT_MAX];

        if (std::snprintf(member_target, sizeof(member_target), "%s.%s", target,
                type->fields[index].source_name) < 0)
            return (FT_FAILURE);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (type->fields[index].array_count > 0)
            {
                if (c_backend_buffer_append_format_line(buffer,
                        "    memset(%s, 0, sizeof(%s));", member_target, member_target)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (c_backend_buffer_append_format_line(buffer, "    %s.len = 0;", member_target)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    memset(%s.buf, 0, sizeof(%s.buf));", member_target, member_target)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (c_backend_buffer_append_format_line(buffer,
                    "    memset(%s, ' ', sizeof(%s));", member_target, member_target)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_INT)
        {
            if (type->fields[index].array_count > 0)
            {
                if (c_backend_buffer_append_format_line(buffer,
                        "    memset(%s, 0, sizeof(%s));", member_target, member_target)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (c_backend_buffer_append_format_line(buffer, "    %s = 0;", member_target)
                    != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = c_backend_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (c_backend_emit_lifecycle_recursive(unit, field_type, member_target, buffer)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int c_backend_replace_this_prefix(const char *input, const char *replacement, char *output,
    size_t output_size)
{
    const char *needle;
    size_t needle_length;
    size_t replacement_length;
    size_t out_length;

    if (!input || !replacement || !output || output_size == 0)
        return (FT_FAILURE);
    needle = "CBLC-THIS";
    needle_length = std::strlen(needle);
    replacement_length = std::strlen(replacement);
    out_length = 0;
    while (*input != '\0')
    {
        if (std::strncmp(input, needle, needle_length) == 0)
        {
            if (out_length + replacement_length >= output_size)
                return (FT_FAILURE);
            std::memcpy(output + out_length, replacement, replacement_length);
            out_length += replacement_length;
            input += needle_length;
            continue ;
        }
        if (out_length + 1 >= output_size)
            return (FT_FAILURE);
        output[out_length] = *input;
        out_length += 1;
        input += 1;
    }
    output[out_length] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_replace_this_for_receiver(const t_cblc_struct_type *receiver_type,
    const t_cblc_data_item *receiver, const char *input, char *output, size_t output_size)
{
    const char *needle;
    size_t needle_length;
    size_t out_length;

    if (!receiver_type || !receiver || !input || !output || output_size == 0)
        return (FT_FAILURE);
    if (receiver->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        return (c_backend_replace_this_prefix(input, receiver->cobol_name, output, output_size));
    needle = "CBLC-THIS";
    needle_length = std::strlen(needle);
    out_length = 0;
    while (*input != '\0')
    {
        if (std::strncmp(input, needle, needle_length) == 0)
        {
            size_t field_index;
            int replaced_field;

            field_index = 0;
            replaced_field = 0;
            while (field_index < receiver_type->field_count)
            {
                char field_token[TRANSPILE_STATEMENT_TEXT_MAX];
                char replacement[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t field_token_length;
                size_t replacement_length;

                if (std::snprintf(field_token, sizeof(field_token), "CBLC-THIS-%s",
                        receiver_type->fields[field_index].cobol_name) < 0)
                    return (FT_FAILURE);
                field_token_length = std::strlen(field_token);
                if (std::strncmp(input, field_token, field_token_length) == 0)
                {
                    if (std::snprintf(replacement, sizeof(replacement), "%s->%s",
                            receiver->source_name, receiver_type->fields[field_index].source_name) < 0)
                        return (FT_FAILURE);
                    replacement_length = std::strlen(replacement);
                    if (out_length + replacement_length >= output_size)
                        return (FT_FAILURE);
                    std::memcpy(output + out_length, replacement, replacement_length);
                    out_length += replacement_length;
                    input += field_token_length;
                    replaced_field = 1;
                    break ;
                }
                field_index += 1;
            }
            if (replaced_field)
                continue ;
            {
                size_t replacement_length;

                replacement_length = std::strlen(receiver->source_name);
                if (out_length + replacement_length >= output_size)
                    return (FT_FAILURE);
                std::memcpy(output + out_length, receiver->source_name, replacement_length);
                out_length += replacement_length;
                input += needle_length;
                continue ;
            }
        }
        if (out_length + 1 >= output_size)
            return (FT_FAILURE);
        output[out_length] = *input;
        out_length += 1;
        input += 1;
    }
    output[out_length] = '\0';
    return (FT_SUCCESS);
}

static int c_backend_emit_lifecycle(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    const t_cblc_data_item *item;
    const t_cblc_struct_type *type;
    const t_cblc_statement *body;
    size_t count;
    size_t index;

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    if (statement->target[0] == '\0')
        return (FT_FAILURE);
    item = c_backend_find_data_item_by_cobol(unit, statement->target);
    if (!item)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_STRING)
    {
        t_cblc_statement argument_statement;
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
        char encoded[TRANSPILE_STATEMENT_TEXT_MAX];
        char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_data_item *source_item;

        if (item->array_count > 0)
        {
            if (c_backend_buffer_append_format_line(buffer,
                    "    memset(%s, 0, sizeof(%s));", item->source_name, item->source_name)
                != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (c_backend_build_string_buf_ref(item, buffer_ref, sizeof(buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_len_ref(item, length_ref, sizeof(length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer, "    %s = 0;", length_ref) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    memset(%s, 0, sizeof(%s));", buffer_ref, buffer_ref) != FT_SUCCESS)
            return (FT_FAILURE);
        if (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->call_argument_count == 0)
            return (FT_SUCCESS);
        if (statement->call_argument_count != 1)
            return (FT_FAILURE);
        std::memset(&argument_statement, 0, sizeof(argument_statement));
        ft_strlcpy(argument_statement.call_arguments, statement->call_arguments,
            sizeof(argument_statement.call_arguments));
        argument_statement.call_argument_count = statement->call_argument_count;
        if (c_backend_extract_call_argument(&argument_statement, 0, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (argument[0] == '"')
        {
            if (c_backend_decode_cobol_literal(argument, decoded, sizeof(decoded)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
                return (FT_FAILURE);
            return (c_backend_buffer_append_format_line(buffer,
                    "    cblc_string_assign_literal(%s, %zu, &%s, \"%s\");",
                    buffer_ref, item->length, length_ref, encoded));
        }
        source_item = c_backend_find_data_item_by_source(unit, argument);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        {
            char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
            char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

            if (c_backend_build_string_buf_ref(source_item, source_buffer_ref,
                    sizeof(source_buffer_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_build_string_len_ref(source_item, source_length_ref,
                    sizeof(source_length_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            return (c_backend_buffer_append_format_line(buffer,
                    "    cblc_string_assign(%s, %zu, &%s, %s, %s);",
                    buffer_ref, item->length, length_ref,
                    source_buffer_ref, source_length_ref));
        }
    }
    if (item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_FAILURE);
    type = c_backend_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && ((statement->call_argument_count == 0 && type->has_default_constructor)
            || statement->call_argument_count > 0))
    {
        const t_cblc_constructor *constructor;

        constructor = c_backend_find_constructor_for_statement(unit, type, statement);
        if (!constructor)
        {
            if (statement->call_argument_count == 0)
                return (c_backend_emit_lifecycle_recursive(unit, type, item->source_name,
                        buffer));
            return (FT_FAILURE);
        }
        if (constructor->statement_count == 0)
            return (c_backend_emit_lifecycle_recursive(unit, type, item->source_name, buffer));
        if (constructor->parameter_count > 0)
        {
            if (c_backend_emit_parameter_argument_moves(unit, statement->call_arguments,
                    statement->call_argument_count, constructor->parameters,
                    constructor->parameter_count, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        body = constructor->statements;
        count = constructor->statement_count;
        index = 0;
        while (index < count)
        {
            t_cblc_statement substituted;
            size_t consumed;

            substituted = body[index];
            consumed = 1;
            if (c_backend_replace_this_prefix(body[index].target, item->cobol_name, substituted.target,
                    sizeof(substituted.target)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_replace_this_prefix(body[index].source, item->cobol_name, substituted.source,
                    sizeof(substituted.source)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (substituted.type == CBLC_STATEMENT_ASSIGNMENT)
            {
                if (c_backend_emit_assignment(unit, &substituted, NULL, buffer, &consumed) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_COMPUTE)
            {
                if (c_backend_emit_compute(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_DISPLAY)
            {
                if (c_backend_emit_display(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_CALL)
            {
                if (c_backend_emit_call(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
            {
                if (c_backend_emit_call_assignment(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_METHOD_CALL
                || substituted.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            {
                if (c_backend_emit_method(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                || substituted.type == CBLC_STATEMENT_DESTRUCT)
            {
                if (c_backend_emit_lifecycle(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type != CBLC_STATEMENT_RETURN)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    if (statement->type == CBLC_STATEMENT_DESTRUCT && type->destructor_statement_count > 0)
    {
        body = type->destructor_statements;
        count = type->destructor_statement_count;
        index = 0;
        while (index < count)
        {
            t_cblc_statement substituted;
            size_t consumed;

            substituted = body[index];
            consumed = 1;
            if (c_backend_replace_this_prefix(body[index].target, item->cobol_name, substituted.target,
                    sizeof(substituted.target)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_replace_this_prefix(body[index].source, item->cobol_name, substituted.source,
                    sizeof(substituted.source)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (substituted.type == CBLC_STATEMENT_ASSIGNMENT)
            {
                if (c_backend_emit_assignment(unit, &substituted, NULL, buffer, &consumed) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_COMPUTE)
            {
                if (c_backend_emit_compute(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_DISPLAY)
            {
                if (c_backend_emit_display(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_CALL)
            {
                if (c_backend_emit_call(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
            {
                if (c_backend_emit_call_assignment(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_METHOD_CALL
                || substituted.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            {
                if (c_backend_emit_method(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                || substituted.type == CBLC_STATEMENT_DESTRUCT)
            {
                if (c_backend_emit_lifecycle(unit, &substituted, buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (substituted.type != CBLC_STATEMENT_RETURN)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    return (c_backend_emit_lifecycle_recursive(unit, type, item->source_name, buffer));
}

static int c_backend_emit_method_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method, const char *assign_target,
    t_c_backend_buffer *buffer)
{
    size_t index;

    if (!unit || !receiver || !method || !buffer)
        return (FT_FAILURE);
    index = 0;
    while (index < method->statement_count)
    {
        t_cblc_statement substituted;
        const t_cblc_statement *body_statement;
        size_t consumed;

        body_statement = &method->statements[index];
        substituted = *body_statement;
        consumed = 1;
        if (c_backend_replace_this_for_receiver(c_backend_find_struct_type(unit,
                    receiver->declared_type_name), receiver, body_statement->target,
                substituted.target, sizeof(substituted.target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_replace_this_for_receiver(c_backend_find_struct_type(unit,
                    receiver->declared_type_name), receiver, body_statement->source,
                substituted.source, sizeof(substituted.source)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (substituted.type == CBLC_STATEMENT_RETURN)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];
            char expression[TRANSPILE_STATEMENT_TEXT_MAX];

            if (method->return_kind == CBLC_FUNCTION_RETURN_INT
                || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT
                || method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            {
                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (c_backend_map_identifier_to_c(unit, assign_target, target, sizeof(target)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_translate_expression(unit, substituted.source, expression,
                        sizeof(expression)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer, "    %s = %s;",
                        target, expression) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            return (FT_SUCCESS);
        }
        if (substituted.type == CBLC_STATEMENT_ASSIGNMENT)
        {
            if (c_backend_emit_assignment(unit, &substituted, NULL, buffer, &consumed) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (substituted.type == CBLC_STATEMENT_COMPUTE)
        {
            if (c_backend_emit_compute(unit, &substituted, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (substituted.type == CBLC_STATEMENT_DISPLAY)
        {
            if (c_backend_emit_display(unit, &substituted, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (substituted.type == CBLC_STATEMENT_CALL)
        {
            if (c_backend_emit_call(unit, &substituted, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            if (c_backend_emit_call_assignment(unit, &substituted, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || substituted.type == CBLC_STATEMENT_DESTRUCT)
        {
            if (c_backend_emit_lifecycle(unit, &substituted, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (substituted.type == CBLC_STATEMENT_METHOD_CALL
            || substituted.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            if (c_backend_emit_method(unit, &substituted, buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int c_backend_emit_method(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_c_backend_buffer *buffer)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    const char *receiver_name;
    const char *assign_target;

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_METHOD_CALL)
    {
        receiver_name = statement->target;
        assign_target = NULL;
    }
    else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
    {
        receiver_name = statement->source;
        assign_target = statement->target;
    }
    else
        return (FT_FAILURE);
    receiver = c_backend_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver)
        return (FT_FAILURE);
    if (receiver->declared_type_name[0] != '\0')
        receiver_type = c_backend_find_struct_type(unit, receiver->declared_type_name);
    else
        receiver_type = c_backend_find_struct_type(unit, receiver->struct_type_name);
    if (!receiver_type)
        return (FT_FAILURE);
    method = NULL;
    {
        size_t index;

        index = 0;
        while (index < receiver_type->method_count)
        {
            if (std::strncmp(receiver_type->methods[index].source_name, statement->call_identifier,
                    sizeof(receiver_type->methods[index].source_name)) == 0)
            {
                method = &receiver_type->methods[index];
                break ;
            }
            index += 1;
        }
    }
    if (!method)
        return (FT_FAILURE);
    if (receiver_type->is_builtin
        && std::strncmp(receiver_type->source_name, "string", TRANSPILE_IDENTIFIER_MAX) == 0)
    {
        char receiver_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char receiver_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_build_string_buf_ref(receiver, receiver_buffer_ref,
                sizeof(receiver_buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_len_ref(receiver, receiver_length_ref,
                sizeof(receiver_length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(statement->call_identifier, "append",
                sizeof(statement->call_identifier)) == 0)
        {
            if (statement->is_literal)
            {
                char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
                char encoded[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_decode_cobol_literal(statement->source, decoded, sizeof(decoded))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (c_backend_buffer_append_format_line(buffer,
                        "    cblc_string_append_literal(%s, %zu, &%s, \"%s\");",
                        receiver_buffer_ref, receiver->length, receiver_length_ref, encoded));
            }
            else
            {
                const t_cblc_data_item *source_item;
                char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                source_item = c_backend_find_data_item_by_cobol(unit, statement->source);
                if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (c_backend_build_string_buf_ref(source_item, source_buffer_ref,
                        sizeof(source_buffer_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_build_string_len_ref(source_item, source_length_ref,
                        sizeof(source_length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (c_backend_buffer_append_format_line(buffer,
                        "    cblc_string_append(%s, %zu, &%s, %s, %s);",
                        receiver_buffer_ref, receiver->length, receiver_length_ref,
                        source_buffer_ref, source_length_ref));
            }
        }
        if (std::strncmp(statement->call_identifier, "len",
                sizeof(statement->call_identifier)) == 0)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];

            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (c_backend_map_identifier_to_c(unit, assign_target, target, sizeof(target)) != FT_SUCCESS)
                return (FT_FAILURE);
            return (c_backend_buffer_append_format_line(buffer, "    %s = (int)%s;",
                    target, receiver_length_ref));
        }
        if (std::strncmp(statement->call_identifier, "capacity",
                sizeof(statement->call_identifier)) == 0)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];

            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (c_backend_map_identifier_to_c(unit, assign_target, target, sizeof(target))
                != FT_SUCCESS)
                return (FT_FAILURE);
            return (c_backend_buffer_append_format_line(buffer, "    %s = %zu;",
                    target, receiver->length));
        }
        if (std::strncmp(statement->call_identifier, "clear",
                sizeof(statement->call_identifier)) == 0)
        {
            return (c_backend_buffer_append_format_line(buffer,
                    "    cblc_string_assign_literal(%s, %zu, &%s, \"\");",
                    receiver_buffer_ref, receiver->length, receiver_length_ref));
        }
        if (std::strncmp(statement->call_identifier, "empty",
                sizeof(statement->call_identifier)) == 0)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];

            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (c_backend_map_identifier_to_c(unit, assign_target, target, sizeof(target)) != FT_SUCCESS)
                return (FT_FAILURE);
            return (c_backend_buffer_append_format_line(buffer, "    %s = (%s == 0);",
                    target, receiver_length_ref));
        }
        if (std::strncmp(statement->call_identifier, "equals",
                sizeof(statement->call_identifier)) == 0)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (c_backend_map_identifier_to_c(unit, assign_target, target, sizeof(target)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
                char encoded[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_decode_cobol_literal(argument, decoded, sizeof(decoded)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (c_backend_buffer_append_format_line(buffer,
                        "    %s = cblc_string_equals(%s, %s, \"%s\", cblc_string_length(\"%s\"));",
                        target, receiver_buffer_ref, receiver_length_ref, encoded, encoded));
            }
            else
            {
                const t_cblc_data_item *source_item;
                char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                source_item = c_backend_find_data_item(unit, argument);
                if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (source_item->is_alias)
                {
                    source_item = c_backend_find_data_item_by_cobol(unit, source_item->cobol_name);
                    if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                        return (FT_FAILURE);
                }
                if (c_backend_build_string_buf_ref(source_item, source_buffer_ref,
                        sizeof(source_buffer_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_build_string_len_ref(source_item, source_length_ref,
                        sizeof(source_length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (c_backend_buffer_append_format_line(buffer,
                        "    %s = cblc_string_equals(%s, %s, %s, %s);",
                        target, receiver_buffer_ref, receiver_length_ref,
                        source_buffer_ref, source_length_ref));
            }
        }
        if (std::strncmp(statement->call_identifier, "starts_with",
                sizeof(statement->call_identifier)) == 0
            || std::strncmp(statement->call_identifier, "ends_with",
                sizeof(statement->call_identifier)) == 0
            || std::strncmp(statement->call_identifier, "compare",
                sizeof(statement->call_identifier)) == 0
            || std::strncmp(statement->call_identifier, "contains",
                sizeof(statement->call_identifier)) == 0)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            const char *helper_name;

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (c_backend_map_identifier_to_c(unit, assign_target, target, sizeof(target))
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_extract_call_argument(statement, 0, argument, sizeof(argument))
                != FT_SUCCESS)
                return (FT_FAILURE);
            helper_name = "cblc_string_starts_with";
            if (std::strncmp(statement->call_identifier, "ends_with",
                    sizeof(statement->call_identifier)) == 0)
                helper_name = "cblc_string_ends_with";
            if (std::strncmp(statement->call_identifier, "compare",
                    sizeof(statement->call_identifier)) == 0)
                helper_name = "cblc_string_compare";
            if (std::strncmp(statement->call_identifier, "contains",
                    sizeof(statement->call_identifier)) == 0)
                helper_name = "cblc_string_contains";
            if (argument[0] == '"')
            {
                char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
                char encoded[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_decode_cobol_literal(argument, decoded, sizeof(decoded)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (c_backend_buffer_append_format_line(buffer,
                        "    %s = %s(%s, %s, \"%s\", cblc_string_length(\"%s\"));",
                        target, helper_name, receiver_buffer_ref, receiver_length_ref,
                        encoded, encoded));
            }
            else
            {
                const t_cblc_data_item *source_item;
                char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                source_item = c_backend_find_data_item(unit, argument);
                if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (source_item->is_alias)
                {
                    source_item = c_backend_find_data_item_by_cobol(unit, source_item->cobol_name);
                    if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                        return (FT_FAILURE);
                }
                if (c_backend_build_string_buf_ref(source_item, source_buffer_ref,
                        sizeof(source_buffer_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_build_string_len_ref(source_item, source_length_ref,
                        sizeof(source_length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (c_backend_buffer_append_format_line(buffer,
                        "    %s = %s(%s, %s, %s, %s);",
                        target, helper_name, receiver_buffer_ref, receiver_length_ref,
                        source_buffer_ref, source_length_ref));
            }
        }
    }
    if (method->parameter_count > 0)
    {
        if (c_backend_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count, method->parameters,
                method->parameter_count, buffer) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (c_backend_emit_method_body(unit, receiver, method, assign_target, buffer));
}

static int c_backend_emit_helper_functions(t_c_backend_buffer *buffer)
{
    const t_transpiler_runtime_helper_entry *entries;
    size_t entry_count;
    size_t index;

    if (!buffer)
        return (FT_FAILURE);
    entries = transpiler_runtime_helpers_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        if (c_backend_buffer_append_string(buffer, entries[index].source) != FT_SUCCESS)
            return (FT_FAILURE);
        if (index + 1 < entry_count)
        {
            if (c_backend_buffer_append_string(buffer, "\n") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int c_backend_function_return_type(const t_cblc_function *function, char *buffer,
    size_t buffer_size)
{
    if (!buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (!function)
    {
        ft_strlcpy(buffer, "void", buffer_size);
        return (FT_SUCCESS);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
    {
        ft_strlcpy(buffer, "int", buffer_size);
        return (FT_SUCCESS);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
    {
        ft_strlcpy(buffer, "void *", buffer_size);
        return (FT_SUCCESS);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
    {
        ft_strlcpy(buffer, "char *", buffer_size);
        return (FT_SUCCESS);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
    {
        ft_strlcpy(buffer, "int *", buffer_size);
        return (FT_SUCCESS);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER
        && function->return_type_name[0] != '\0')
    {
        if (std::snprintf(buffer, buffer_size, "t_%s *", function->return_type_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT
        && function->return_type_name[0] != '\0')
    {
        if (std::snprintf(buffer, buffer_size, "t_%s", function->return_type_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    ft_strlcpy(buffer, "void", buffer_size);
    return (FT_SUCCESS);
}

static int c_backend_emit_struct_type(const t_cblc_struct_type *type, t_c_backend_buffer *buffer)
{
    size_t index;

    if (!type || !buffer)
        return (FT_FAILURE);
    if (type->is_builtin && c_backend_is_builtin_string_type_name(type->source_name))
        return (FT_SUCCESS);
    if (c_backend_buffer_append_format_line(buffer, "typedef struct s_%s", type->source_name) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_buffer_append_line(buffer, "{") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (type->fields[index].array_count > 0)
            {
                if (c_backend_buffer_append_format_line(buffer,
                        "    struct { size_t len; char buf[%zu]; } %s[%zu];",
                        type->fields[index].length, type->fields[index].source_name,
                        type->fields[index].array_count) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (c_backend_buffer_append_format_line(buffer,
                    "    struct { size_t len; char buf[%zu]; } %s;",
                    type->fields[index].length, type->fields[index].source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (c_backend_buffer_append_format_line(buffer, "    char %s[%zu];",
                    type->fields[index].source_name, type->fields[index].length) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_INT)
        {
            if (type->fields[index].array_count > 0)
            {
                if (c_backend_buffer_append_format_line(buffer, "    int %s[%zu];",
                        type->fields[index].source_name, type->fields[index].array_count) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (c_backend_buffer_append_format_line(buffer, "    int %s;",
                    type->fields[index].source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            if (type->fields[index].struct_type_name[0] == '\0')
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer, "    t_%s %s;",
                    type->fields[index].struct_type_name,
                    type->fields[index].source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    if (c_backend_buffer_append_format_line(buffer, "} t_%s;", type->source_name) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int cblc_generate_c(const t_cblc_translation_unit *unit, char **out_text)
{
    t_c_backend_buffer buffer;
    char (*external_calls)[TRANSPILE_IDENTIFIER_MAX];
    size_t external_count;
    size_t external_capacity;
    size_t index;
    size_t statement_index;
    size_t entry_index;
    const t_cblc_function *entry_function;
    int generate_main;
    int status;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    c_backend_buffer_init(&buffer);
    external_calls = NULL;
    external_count = 0;
    external_capacity = 0;
    status = FT_FAILURE;
    index = 0;
    while (index < unit->function_count)
    {
        const t_cblc_function *function;

        function = &unit->functions[index];
        statement_index = 0;
        while (statement_index < function->statement_count)
        {
            const t_cblc_statement *statement;

            statement = &function->statements[statement_index];
            if (statement->type == CBLC_STATEMENT_CALL && statement->call_is_external)
            {
                if (c_backend_external_append(&external_calls, &external_count, &external_capacity,
                        statement->call_identifier) != FT_SUCCESS)
                    goto cleanup;
            }
            statement_index += 1;
        }
        index += 1;
    }
    if (c_backend_buffer_append_line(&buffer, "#include <stddef.h>") != FT_SUCCESS)
        goto cleanup;
    if (c_backend_buffer_append_line(&buffer, "#include <stdlib.h>") != FT_SUCCESS)
        goto cleanup;
    if (c_backend_buffer_append_line(&buffer, "#include <stdio.h>") != FT_SUCCESS)
        goto cleanup;
    if (c_backend_buffer_append_line(&buffer, "#include <string.h>") != FT_SUCCESS)
        goto cleanup;
    if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
        goto cleanup;
    if (c_backend_emit_helper_functions(&buffer) != FT_SUCCESS)
        goto cleanup;
    if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (c_backend_emit_struct_type(&unit->struct_types[index], &buffer) != FT_SUCCESS)
            goto cleanup;
        if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    index = 0;
    while (index < unit->data_count)
    {
        const t_cblc_data_item *item;
        size_t length;

        item = &unit->data_items[index];
        if (item->is_alias || std::strchr(item->source_name, '.')
            || std::strstr(item->source_name, "->"))
        {
            index += 1;
            continue ;
        }
        if (item->is_function_local
            && c_backend_has_function_owner(unit, item->owner_function_name))
        {
            index += 1;
            continue ;
        }
        if (item->kind == CBLC_DATA_KIND_STRING)
        {
            length = item->length;
            if (length == 0)
                length = 1;
            if (item->array_count > 0)
            {
                if (c_backend_buffer_append_format_line(&buffer,
                        "static struct { size_t len; char buf[%zu]; } %s[%zu] = {{0}};",
                        length, item->source_name, item->array_count) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (item->is_const && item->has_initializer)
            {
                if (c_backend_buffer_append_format_line(&buffer,
                        "static const size_t %s_len = %zu;", item->source_name,
                        item->initializer_length) != FT_SUCCESS)
                    goto cleanup;
                if (c_backend_buffer_append_format_line(&buffer,
                        "static const char %s_buf[%zu] = %s;", item->source_name,
                        length, item->initializer_text) != FT_SUCCESS)
                    goto cleanup;
            }
            else
            {
                if (c_backend_buffer_append_format_line(&buffer,
                        "static size_t %s_len = 0;", item->source_name) != FT_SUCCESS)
                    goto cleanup;
                if (c_backend_buffer_append_format_line(&buffer,
                        "static char %s_buf[%zu] = {0};", item->source_name,
                        length) != FT_SUCCESS)
                    goto cleanup;
            }
        }
        else if (item->kind == CBLC_DATA_KIND_CHAR)
        {
            length = item->length;
            if (length == 0)
                length = 1;
            if (item->is_const && item->has_initializer)
            {
                if (item->initializer_text[0] == '\'')
                {
                    if (c_backend_buffer_append_format_line(&buffer,
                            "static const char %s[%zu] = {'%c'};", item->source_name,
                            length, item->initializer_text[1]) != FT_SUCCESS)
                        goto cleanup;
                }
                else if (c_backend_buffer_append_format_line(&buffer,
                        "static const char %s[%zu] = %s;", item->source_name,
                        length, item->initializer_text) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (c_backend_buffer_append_format_line(&buffer,
                    "static char %s[%zu] = {0};", item->source_name,
                    length) != FT_SUCCESS)
                goto cleanup;
        }
        else if (item->kind == CBLC_DATA_KIND_INT)
        {
            if (item->array_count > 0)
            {
                if (c_backend_buffer_append_format_line(&buffer,
                        "static int %s[%zu] = {0};", item->source_name,
                        item->array_count) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (item->is_const && item->has_initializer)
            {
                if (c_backend_buffer_append_format_line(&buffer,
                        "static const int %s = %s;", item->source_name,
                        item->initializer_text) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (c_backend_buffer_append_format_line(&buffer,
                    "static int %s = 0;", item->source_name) != FT_SUCCESS)
                goto cleanup;
        }
        else if (c_backend_is_pointer_kind(item->kind))
        {
            char pointer_type[TRANSPILE_IDENTIFIER_MAX];

            if (c_backend_pointer_type_name(item, pointer_type, sizeof(pointer_type))
                != FT_SUCCESS)
                goto cleanup;
            if (c_backend_buffer_append_format_line(&buffer,
                    "static %s%s = NULL;", pointer_type, item->source_name) != FT_SUCCESS)
                goto cleanup;
        }
        else if (item->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (c_backend_find_struct_type(unit, item->struct_type_name) == NULL)
                goto cleanup;
            if (c_backend_buffer_append_format_line(&buffer,
                    "static t_%s %s = {0};", item->struct_type_name,
                    item->source_name) != FT_SUCCESS)
                goto cleanup;
        }
        index += 1;
    }
    if (unit->data_count > 0)
    {
        if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
            goto cleanup;
    }
    index = 0;
    while (index < external_count)
    {
        if (c_backend_buffer_append_format_line(&buffer,
                "extern void %s(void);", external_calls[index]) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (external_count > 0)
    {
        if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
            goto cleanup;
    }
    entry_index = unit->entry_function_index;
    if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
        entry_index = 0;
    entry_function = NULL;
    generate_main = 0;
    if (unit->function_count > 0)
    {
        entry_function = &unit->functions[entry_index];
        if (std::strncmp(entry_function->source_name, "main", TRANSPILE_IDENTIFIER_MAX) == 0)
            generate_main = 1;
    }
    index = 0;
    while (index < unit->function_count)
    {
        const t_cblc_function *function;

        function = &unit->functions[index];
        char return_type[TRANSPILE_IDENTIFIER_MAX];

        if (c_backend_function_return_type(function, return_type, sizeof(return_type))
            != FT_SUCCESS)
            goto cleanup;
        if (generate_main && index == entry_index)
        {
            if (c_backend_buffer_append_format_line(&buffer,
                    "static %s cblc_entry_main(void);", return_type) != FT_SUCCESS)
                goto cleanup;
        }
        else
        {
            if (c_backend_buffer_append_format_line(&buffer,
                    "%s %s(void);", return_type, function->source_name) != FT_SUCCESS)
                goto cleanup;
        }
        index += 1;
    }
    if (unit->function_count > 0)
    {
        if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
            goto cleanup;
    }
    if (generate_main)
    {
        if (c_backend_buffer_append_line(&buffer, "int main(void)") != FT_SUCCESS)
            goto cleanup;
        if (c_backend_buffer_append_line(&buffer, "{") != FT_SUCCESS)
            goto cleanup;
        if (entry_function && entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            if (c_backend_buffer_append_line(&buffer, "    int result;") != FT_SUCCESS)
                goto cleanup;
            if (c_backend_buffer_append_line(&buffer,
                    "    result = cblc_entry_main();") != FT_SUCCESS)
                goto cleanup;
            if (c_backend_buffer_append_line(&buffer, "    return (result);") != FT_SUCCESS)
                goto cleanup;
        }
        else
        {
            if (c_backend_buffer_append_line(&buffer, "    cblc_entry_main();") != FT_SUCCESS)
                goto cleanup;
            if (c_backend_buffer_append_line(&buffer, "    return (0);") != FT_SUCCESS)
                goto cleanup;
        }
        if (c_backend_buffer_append_line(&buffer, "}") != FT_SUCCESS)
            goto cleanup;
        if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
            goto cleanup;
    }
    index = 0;
    while (index < unit->function_count)
    {
        const t_cblc_function *function;
        const t_cblc_statement *next_statement;
        size_t consumed;
        int is_entry;

        function = &unit->functions[index];
        is_entry = generate_main && index == entry_index;
        if (is_entry)
        {
            char return_type[TRANSPILE_IDENTIFIER_MAX];

            if (c_backend_function_return_type(function, return_type, sizeof(return_type))
                != FT_SUCCESS)
                goto cleanup;
            if (c_backend_buffer_append_format_line(&buffer,
                    "static %s cblc_entry_main(void)", return_type) != FT_SUCCESS)
                goto cleanup;
        }
        else
        {
            char return_type[TRANSPILE_IDENTIFIER_MAX];

            if (c_backend_function_return_type(function, return_type, sizeof(return_type))
                != FT_SUCCESS)
                goto cleanup;
            if (c_backend_buffer_append_format_line(&buffer,
                    "%s %s(void)", return_type, function->source_name) != FT_SUCCESS)
                goto cleanup;
        }
        if (c_backend_buffer_append_line(&buffer, "{") != FT_SUCCESS)
            goto cleanup;
        {
            size_t local_index;

            local_index = 0;
            while (local_index < unit->data_count)
            {
                const t_cblc_data_item *item;
                size_t length;

                item = &unit->data_items[local_index];
                if (item->is_alias || !item->is_function_local || std::strchr(item->source_name, '.')
                    || std::strstr(item->source_name, "->")
                    || std::strncmp(item->owner_function_name, function->source_name,
                        sizeof(item->owner_function_name)) != 0)
                {
                    local_index += 1;
                    continue ;
                }
                if (item->kind == CBLC_DATA_KIND_STRING)
                {
                    length = item->length == 0 ? 1 : item->length;
                    if (item->array_count > 0)
                    {
                        if (c_backend_buffer_append_format_line(&buffer,
                                "    struct { size_t len; char buf[%zu]; } %s[%zu] = {{0}};",
                                length, item->source_name, item->array_count) != FT_SUCCESS)
                            goto cleanup;
                    }
                    else
                    {
                        if (c_backend_buffer_append_format_line(&buffer,
                                "    size_t %s_len = 0;", item->source_name) != FT_SUCCESS)
                            goto cleanup;
                        if (c_backend_buffer_append_format_line(&buffer,
                                "    char %s_buf[%zu] = {0};", item->source_name,
                                length) != FT_SUCCESS)
                            goto cleanup;
                    }
                }
                else if (item->kind == CBLC_DATA_KIND_CHAR)
                {
                    length = item->length == 0 ? 1 : item->length;
                    if (c_backend_buffer_append_format_line(&buffer,
                            "    char %s[%zu] = {0};", item->source_name,
                            length) != FT_SUCCESS)
                        goto cleanup;
                }
                else if (item->kind == CBLC_DATA_KIND_INT)
                {
                    if (item->array_count > 0)
                    {
                        if (c_backend_buffer_append_format_line(&buffer,
                                "    int %s[%zu] = {0};", item->source_name,
                                item->array_count) != FT_SUCCESS)
                            goto cleanup;
                    }
                    else if (c_backend_buffer_append_format_line(&buffer,
                            "    int %s = 0;", item->source_name) != FT_SUCCESS)
                        goto cleanup;
                }
                else if (c_backend_is_pointer_kind(item->kind))
                {
                    char pointer_type[TRANSPILE_IDENTIFIER_MAX];

                    if (c_backend_pointer_type_name(item, pointer_type, sizeof(pointer_type))
                        != FT_SUCCESS)
                        goto cleanup;
                    if (c_backend_buffer_append_format_line(&buffer,
                            "    %s%s = NULL;", pointer_type, item->source_name) != FT_SUCCESS)
                        goto cleanup;
                }
                else if (item->kind == CBLC_DATA_KIND_STRUCT)
                {
                    if (c_backend_buffer_append_format_line(&buffer,
                            "    t_%s %s = {0};", item->struct_type_name,
                            item->source_name) != FT_SUCCESS)
                        goto cleanup;
                }
                local_index += 1;
            }
            if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
                goto cleanup;
        }
        statement_index = 0;
        while (statement_index < function->statement_count)
        {
            const t_cblc_statement *statement;

            statement = &function->statements[statement_index];
            next_statement = NULL;
            consumed = 1;
            if (statement_index + 1 < function->statement_count)
                next_statement = &function->statements[statement_index + 1];
            if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
            {
                if (c_backend_emit_assignment(unit, statement, next_statement, &buffer, &consumed)
                    != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_COMPUTE)
            {
                if (c_backend_emit_compute(unit, statement, &buffer) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_DISPLAY)
            {
                if (c_backend_emit_display(unit, statement, &buffer) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_CALL)
            {
                if (c_backend_emit_call(unit, statement, &buffer) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_CALL_ASSIGN)
            {
                if (c_backend_emit_call_assignment(unit, statement, &buffer)
                    != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_RETURN)
            {
                if (c_backend_emit_return(unit, function, statement, &buffer)
                    != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                || statement->type == CBLC_STATEMENT_DESTRUCT)
            {
                if (c_backend_emit_lifecycle(unit, statement, &buffer) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (statement->type == CBLC_STATEMENT_METHOD_CALL
                || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            {
                if (c_backend_emit_method(unit, statement, &buffer) != FT_SUCCESS)
                    goto cleanup;
            }
            else
                goto cleanup;
            statement_index += consumed;
        }
        if (c_backend_buffer_append_line(&buffer, "}") != FT_SUCCESS)
            goto cleanup;
        if (c_backend_buffer_append_line(&buffer, "") != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    status = FT_SUCCESS;
    *out_text = buffer.data;
    buffer.data = NULL;
cleanup:
    if (external_calls)
        cma_free(external_calls);
    if (buffer.data)
        c_backend_buffer_dispose(&buffer);
    return (status);
}
