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

    if (!unit || !token || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (c_backend_strip_suffix(token, "-LEN", base, sizeof(base)))
    {
        item = c_backend_find_data_item_by_cobol(unit, base);
        if (!item || item->kind != CBLC_DATA_KIND_STRING)
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
    item = c_backend_find_data_item_by_cobol(unit, token);
    if (item)
    {
        if (item->kind == CBLC_DATA_KIND_STRUCT)
            return (FT_FAILURE);
        if (std::snprintf(buffer, buffer_size, "%s", item->source_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (std::snprintf(buffer, buffer_size, "%s", token) < 0)
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

    if (!buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    if (!unit || !expression)
        return (FT_FAILURE);
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
        if (character == '+' || character == '-' || character == '*' || character == '/')
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

            length = 0;
            token_index = index;
            while ((expression[token_index] >= 'A' && expression[token_index] <= 'Z')
                || (expression[token_index] >= 'a' && expression[token_index] <= 'z')
                || (expression[token_index] >= '0' && expression[token_index] <= '9')
                || expression[token_index] == '-' || expression[token_index] == '_')
            {
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[token_index];
                length += 1;
                token_index += 1;
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

static int c_backend_emit_string_literal_assignment(const t_cblc_data_item *item, const char *literal,
    t_c_backend_buffer *buffer)
{
    char decoded[TRANSPILE_STATEMENT_TEXT_MAX];
    char encoded[TRANSPILE_STATEMENT_TEXT_MAX];
    char buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
    char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t capacity;

    if (!item || !literal || !buffer)
        return (FT_FAILURE);
    if (c_backend_decode_cobol_literal(literal, decoded, sizeof(decoded)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_encode_c_string(decoded, encoded, sizeof(encoded)) != FT_SUCCESS)
        return (FT_FAILURE);
    capacity = item->length;
    if (capacity == 0)
        capacity = 1;
    if (c_backend_build_string_buf_ref(item, buffer_ref, sizeof(buffer_ref)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_build_string_len_ref(item, length_ref, sizeof(length_ref)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_buffer_append_format_line(buffer,
            "    cblc_string_assign_literal(%s, %zu, &%s, \"%s\");",
            buffer_ref, capacity, length_ref, encoded)
        != FT_SUCCESS)
        return (FT_FAILURE);
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

static int c_backend_emit_assignment(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    const t_cblc_statement *next_statement, t_c_backend_buffer *buffer, size_t *consumed)
{
    char base[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_data_item *item;

    if (!unit || !statement || !buffer || !consumed)
        return (FT_FAILURE);
    *consumed = 1;
    if (c_backend_strip_suffix(statement->target, "-BUF", base, sizeof(base)))
    {
        item = c_backend_find_data_item_by_cobol(unit, base);
        if (!item || item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (!statement->is_literal)
            return (FT_FAILURE);
        if (c_backend_emit_string_literal_assignment(item, statement->source, buffer) != FT_SUCCESS)
            return (FT_FAILURE);
        if (next_statement && next_statement->type == CBLC_STATEMENT_ASSIGNMENT
            && c_backend_strip_suffix(next_statement->target, "-LEN", base, sizeof(base)))
        {
            const t_cblc_data_item *length_item;

            length_item = c_backend_find_data_item_by_cobol(unit, base);
            if (length_item == item)
                *consumed = 2;
        }
        return (FT_SUCCESS);
    }
    if (c_backend_strip_suffix(statement->target, "-LEN", base, sizeof(base)))
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];
        char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_data_item *length_item;

        length_item = c_backend_find_data_item_by_cobol(unit, base);
        if (!length_item || length_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_len_ref(length_item, length_ref, sizeof(length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = %s;", length_ref, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    item = c_backend_find_data_item_by_cobol(unit, statement->target);
    if (!item)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_STRING)
    {
        const t_cblc_data_item *source_item;
        char target_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

        source_item = c_backend_find_data_item_by_cobol(unit, statement->source);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (c_backend_build_string_buf_ref(item, target_buffer_ref, sizeof(target_buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_len_ref(item, target_length_ref, sizeof(target_length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_buf_ref(source_item, source_buffer_ref, sizeof(source_buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_len_ref(source_item, source_length_ref, sizeof(source_length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    cblc_string_copy(%s, %zu, &%s, %s, %s);",
                target_buffer_ref, item->length, target_length_ref,
                source_buffer_ref, source_length_ref) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (item->kind == CBLC_DATA_KIND_CHAR)
    {
        size_t length;

        length = item->length;
        if (length == 0)
            length = 1;
        if (statement->is_literal)
            return (c_backend_emit_char_literal_assignment(item, statement->source, buffer));
        else
        {
            const t_cblc_data_item *source_item;

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
    if (item->kind == CBLC_DATA_KIND_INT)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_translate_expression(unit, statement->source, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    %s = %s;", item->source_name, expression) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int c_backend_emit_compute(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    const t_cblc_data_item *item;
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    item = c_backend_find_data_item_by_cobol(unit, statement->target);
    if (!item || item->kind != CBLC_DATA_KIND_INT)
        return (FT_FAILURE);
    if (c_backend_translate_expression(unit, statement->source, expression,
            sizeof(expression)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (c_backend_buffer_append_format_line(buffer,
            "    %s = %s;", item->source_name, expression) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_display(const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_c_backend_buffer *buffer)
{
    char base[TRANSPILE_IDENTIFIER_MAX];

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
        const char *paren;

        paren = std::strchr(statement->source, '(');
        if (paren)
        {
            char prefix[TRANSPILE_IDENTIFIER_MAX];
            size_t length;

            length = static_cast<size_t>(paren - statement->source);
            if (length + 1 >= sizeof(prefix))
                return (FT_FAILURE);
            std::memcpy(prefix, statement->source, length);
            prefix[length] = '\0';
            if (c_backend_strip_suffix(prefix, "-BUF", prefix, sizeof(prefix)))
            {
                const t_cblc_data_item *item;

                item = c_backend_find_data_item_by_cobol(unit, prefix);
                if (!item || item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                char buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
                char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

                if (c_backend_build_string_buf_ref(item, buffer_ref, sizeof(buffer_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_build_string_len_ref(item, length_ref, sizeof(length_ref)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (c_backend_buffer_append_format_line(buffer,
                        "    cblc_display_string(%s, %s);",
                        buffer_ref, length_ref) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
        }
        if (c_backend_strip_suffix(statement->source, "-LEN", base, sizeof(base)))
        {
            const t_cblc_data_item *item;

            item = c_backend_find_data_item_by_cobol(unit, base);
            if (!item || item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

            if (c_backend_build_string_len_ref(item, length_ref, sizeof(length_ref)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer,
                    "    cblc_display_size(%s);", length_ref) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
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

static int c_backend_emit_call(const t_cblc_statement *statement, t_c_backend_buffer *buffer)
{
    if (!statement || !buffer)
        return (FT_FAILURE);
    if (c_backend_buffer_append_format_line(buffer,
            "    %s();", statement->call_identifier) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int c_backend_emit_call_assignment(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_c_backend_buffer *buffer)
{
    char target[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !statement || !buffer)
        return (FT_FAILURE);
    if (statement->call_identifier[0] == '\0')
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
            if (c_backend_buffer_append_format_line(buffer, "    %s.len = 0;", member_target)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (c_backend_buffer_append_format_line(buffer,
                    "    memset(%s.buf, 0, sizeof(%s.buf));", member_target, member_target)
                != FT_SUCCESS)
                return (FT_FAILURE);
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
            if (c_backend_buffer_append_format_line(buffer, "    %s = 0;", member_target)
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
        char buffer_ref[TRANSPILE_STATEMENT_TEXT_MAX];
        char length_ref[TRANSPILE_STATEMENT_TEXT_MAX];

        if (c_backend_build_string_buf_ref(item, buffer_ref, sizeof(buffer_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_build_string_len_ref(item, length_ref, sizeof(length_ref)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer, "    %s = 0;", length_ref) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_buffer_append_format_line(buffer,
                "    memset(%s, 0, sizeof(%s));", buffer_ref, buffer_ref) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_FAILURE);
    type = c_backend_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT && type->constructor_statement_count > 0)
    {
        body = type->constructor_statements;
        count = type->constructor_statement_count;
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
                if (c_backend_emit_call(&substituted, buffer) != FT_SUCCESS)
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
                if (c_backend_emit_call(&substituted, buffer) != FT_SUCCESS)
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
        if (c_backend_replace_this_prefix(body_statement->target, receiver->cobol_name, substituted.target,
                sizeof(substituted.target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (c_backend_replace_this_prefix(body_statement->source, receiver->cobol_name, substituted.source,
                sizeof(substituted.source)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (substituted.type == CBLC_STATEMENT_RETURN)
        {
            char target[TRANSPILE_IDENTIFIER_MAX];
            char expression[TRANSPILE_STATEMENT_TEXT_MAX];

            if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
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
            if (c_backend_emit_call(&substituted, buffer) != FT_SUCCESS)
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

static const char *c_backend_function_return_type(const t_cblc_function *function)
{
    if (!function)
        return ("void");
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
        return ("int");
    return ("void");
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
            if (c_backend_buffer_append_format_line(buffer,
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
            if (c_backend_buffer_append_format_line(buffer, "    int %s;",
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
        if (item->is_alias || item->is_function_local || std::strchr(item->source_name, '.'))
        {
            index += 1;
            continue ;
        }
        if (item->kind == CBLC_DATA_KIND_STRING)
        {
            length = item->length;
            if (length == 0)
                length = 1;
            if (item->is_const && item->has_initializer)
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
            if (item->is_const && item->has_initializer)
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
        const char *return_type;

        return_type = c_backend_function_return_type(function);
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
            const char *return_type;

            return_type = c_backend_function_return_type(function);
            if (c_backend_buffer_append_format_line(&buffer,
                    "static %s cblc_entry_main(void)", return_type) != FT_SUCCESS)
                goto cleanup;
        }
        else
        {
            const char *return_type;

            return_type = c_backend_function_return_type(function);
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
                    || std::strncmp(item->owner_function_name, function->source_name,
                        sizeof(item->owner_function_name)) != 0)
                {
                    local_index += 1;
                    continue ;
                }
                if (item->kind == CBLC_DATA_KIND_STRING)
                {
                    length = item->length == 0 ? 1 : item->length;
                    if (c_backend_buffer_append_format_line(&buffer,
                            "    size_t %s_len = 0;", item->source_name) != FT_SUCCESS)
                        goto cleanup;
                    if (c_backend_buffer_append_format_line(&buffer,
                            "    char %s_buf[%zu] = {0};", item->source_name,
                            length) != FT_SUCCESS)
                        goto cleanup;
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
                    if (c_backend_buffer_append_format_line(&buffer,
                            "    int %s = 0;", item->source_name) != FT_SUCCESS)
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
                if (c_backend_emit_call(statement, &buffer) != FT_SUCCESS)
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
