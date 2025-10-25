#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"

typedef struct s_transpiler_cobol_buffer
{
    char *data;
    size_t length;
    size_t capacity;
}   t_transpiler_cobol_buffer;

static void transpiler_cobol_buffer_init(t_transpiler_cobol_buffer *buffer)
{
    if (!buffer)
        return ;
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static void transpiler_cobol_buffer_dispose(t_transpiler_cobol_buffer *buffer)
{
    if (!buffer)
        return ;
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static int transpiler_cobol_buffer_reserve(t_transpiler_cobol_buffer *buffer, size_t desired_capacity)
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
        ft_memcpy(new_data, buffer->data, buffer->length);
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = new_data;
    buffer->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_cobol_buffer_append_span(t_transpiler_cobol_buffer *buffer, const char *text, size_t length)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (transpiler_cobol_buffer_reserve(buffer, buffer->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_memcpy(buffer->data + buffer->length, text, length);
    buffer->length += length;
    buffer->data[buffer->length] = '\0';
    return (FT_SUCCESS);
}

static int transpiler_cobol_buffer_append_string(t_transpiler_cobol_buffer *buffer, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (transpiler_cobol_buffer_append_span(buffer, text, ft_strlen(text)));
}

static int transpiler_cobol_buffer_append_number_with_min_digits(t_transpiler_cobol_buffer *buffer,
    unsigned long long value, size_t min_digits)
{
    char digits[32];
    size_t index;
    size_t left;
    size_t right;

    if (!buffer)
        return (FT_FAILURE);
    if (min_digits > sizeof(digits))
        min_digits = sizeof(digits);
    index = 0;
    while (value > 0 && index < sizeof(digits))
    {
        digits[index] = static_cast<char>('0' + (value % 10));
        value /= 10;
        index += 1;
    }
    if (index == 0)
    {
        digits[index] = '0';
        index += 1;
    }
    while (index < min_digits && index < sizeof(digits))
    {
        digits[index] = '0';
        index += 1;
    }
    if (index > sizeof(digits))
        index = sizeof(digits);
    left = 0;
    if (index > 0)
    {
        right = index - 1;
        while (left < right)
        {
            char temp;

            temp = digits[left];
            digits[left] = digits[right];
            digits[right] = temp;
            left += 1;
            right -= 1;
        }
    }
    return (transpiler_cobol_buffer_append_span(buffer, digits, index));
}

static int transpiler_cobol_buffer_append_level(t_transpiler_cobol_buffer *buffer, size_t level)
{
    return (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
        static_cast<unsigned long long>(level), 2));
}

static int transpiler_cobol_buffer_append_indent(t_transpiler_cobol_buffer *buffer, size_t indentation)
{
    size_t index;

    if (!buffer)
        return (FT_FAILURE);
    index = 0;
    while (index < indentation)
    {
        if (transpiler_cobol_buffer_append_string(buffer, "    ") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int transpiler_cobol_elementary_append_picture(t_transpiler_cobol_buffer *buffer,
    const t_transpiler_cobol_elementary *element)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!element)
        return (FT_FAILURE);
    if (element->kind == TRANSPILE_COBOL_ELEMENTARY_ALPHABETIC)
    {
        if (element->length <= 1)
            return (transpiler_cobol_buffer_append_string(buffer, "PIC A"));
        if (transpiler_cobol_buffer_append_string(buffer, "PIC A(") != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
                static_cast<unsigned long long>(element->length), 1) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_cobol_buffer_append_string(buffer, ")") != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (element->kind == TRANSPILE_COBOL_ELEMENTARY_ALPHANUMERIC)
    {
        if (element->length <= 1)
            return (transpiler_cobol_buffer_append_string(buffer, "PIC X"));
        if (transpiler_cobol_buffer_append_string(buffer, "PIC X(") != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
                static_cast<unsigned long long>(element->length), 1) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_cobol_buffer_append_string(buffer, ")") != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (element->kind == TRANSPILE_COBOL_ELEMENTARY_BOOLEAN)
    {
        if (transpiler_cobol_buffer_append_string(buffer, "PIC X") != FT_SUCCESS)
            return (FT_FAILURE);
        if (element->length > 1)
        {
            if (transpiler_cobol_buffer_append_string(buffer, "(") != FT_SUCCESS)
                return (FT_FAILURE);
            if (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
                    static_cast<unsigned long long>(element->length), 1) != FT_SUCCESS)
                return (FT_FAILURE);
            if (transpiler_cobol_buffer_append_string(buffer, ")") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    if (element->kind == TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED
        || element->kind == TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED)
    {
        size_t digits;
        int has_integral_digits;

        digits = element->length;
        has_integral_digits = digits > 0;
        if (!has_integral_digits && element->scale == 0)
        {
            digits = 1;
            has_integral_digits = 1;
        }
        if (has_integral_digits)
        {
            if (digits == 1)
            {
                if (transpiler_cobol_buffer_append_string(buffer, "PIC 9") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (transpiler_cobol_buffer_append_string(buffer, "PIC 9(") != FT_SUCCESS)
                    return (FT_FAILURE);
                if (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
                        static_cast<unsigned long long>(digits), 1) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (transpiler_cobol_buffer_append_string(buffer, ")") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        else
        {
            if (transpiler_cobol_buffer_append_string(buffer, "PIC") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (element->scale > 0)
        {
            if (has_integral_digits)
            {
                if (element->scale == 1)
                {
                    if (transpiler_cobol_buffer_append_string(buffer, "V9") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    if (transpiler_cobol_buffer_append_string(buffer, "V9(") != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
                            static_cast<unsigned long long>(element->scale), 1) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (transpiler_cobol_buffer_append_string(buffer, ")") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else
            {
                if (element->scale == 1)
                {
                    if (transpiler_cobol_buffer_append_string(buffer, " V9") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    if (transpiler_cobol_buffer_append_string(buffer, " V9(") != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (transpiler_cobol_buffer_append_number_with_min_digits(buffer,
                            static_cast<unsigned long long>(element->scale), 1) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (transpiler_cobol_buffer_append_string(buffer, ")") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
        }
        if (element->kind == TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED)
        {
            if (transpiler_cobol_buffer_append_string(buffer, " SIGN IS LEADING SEPARATE") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (element->usage == TRANSPILE_COBOL_USAGE_COMP_5)
        {
            if (transpiler_cobol_buffer_append_string(buffer, " USAGE COMP-5") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int transpiler_cobol_copy_buffer(const t_transpiler_cobol_buffer *buffer, char **out)
{
    char *copy;

    if (!buffer || !out)
        return (FT_FAILURE);
    copy = static_cast<char *>(cma_calloc(buffer->length + 1, sizeof(char)));
    if (!copy)
        return (FT_FAILURE);
    if (buffer->length > 0)
        ft_memcpy(copy, buffer->data, buffer->length);
    copy[buffer->length] = '\0';
    *out = copy;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_c_int(size_t digits, int is_signed, t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    if (digits == 0)
        digits = 10;
    out->kind = is_signed ? TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED
        : TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED;
    out->length = digits;
    out->scale = 0;
    out->usage = TRANSPILE_COBOL_USAGE_COMP_5;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_c_long(int is_signed, t_transpiler_cobol_elementary *out)
{
    return (transpiler_cobol_describe_c_int(18, is_signed, out));
}

int transpiler_cobol_describe_c_long_long(int is_signed, t_transpiler_cobol_elementary *out)
{
    return (transpiler_cobol_describe_c_int(36, is_signed, out));
}

int transpiler_cobol_describe_c_float(t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    out->kind = TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED;
    out->length = 9;
    out->scale = 9;
    out->usage = TRANSPILE_COBOL_USAGE_DISPLAY;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_c_double(t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    out->kind = TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED;
    out->length = 18;
    out->scale = 18;
    out->usage = TRANSPILE_COBOL_USAGE_DISPLAY;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_c_char_array(size_t length, t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    if (length == 0)
        length = 1;
    out->kind = TRANSPILE_COBOL_ELEMENTARY_ALPHANUMERIC;
    out->length = length;
    out->scale = 0;
    out->usage = TRANSPILE_COBOL_USAGE_DISPLAY;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_c_bool(t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    out->kind = TRANSPILE_COBOL_ELEMENTARY_BOOLEAN;
    out->length = 1;
    out->scale = 0;
    out->usage = TRANSPILE_COBOL_USAGE_DISPLAY;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_numeric(size_t digits, int is_signed, t_transpiler_cobol_usage usage,
    t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    if (usage != TRANSPILE_COBOL_USAGE_DISPLAY && usage != TRANSPILE_COBOL_USAGE_COMP_5)
        return (FT_FAILURE);
    if (digits == 0)
        digits = 1;
    out->kind = is_signed ? TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED
        : TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED;
    out->length = digits;
    out->scale = 0;
    out->usage = usage;
    return (FT_SUCCESS);
}

int transpiler_cobol_describe_fixed_point(size_t integral_digits, size_t fractional_digits, int is_signed,
    t_transpiler_cobol_usage usage, t_transpiler_cobol_elementary *out)
{
    if (!out)
        return (FT_FAILURE);
    if (usage != TRANSPILE_COBOL_USAGE_DISPLAY && usage != TRANSPILE_COBOL_USAGE_COMP_5)
        return (FT_FAILURE);
    if (integral_digits == 0 && fractional_digits == 0)
        integral_digits = 1;
    out->kind = is_signed ? TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED
        : TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED;
    out->length = integral_digits;
    out->scale = fractional_digits;
    out->usage = usage;
    return (FT_SUCCESS);
}

int transpiler_cobol_format_elementary(const char *name, size_t level,
    const t_transpiler_cobol_elementary *element, size_t indentation,
    const char *value_text, char **out)
{
    t_transpiler_cobol_buffer buffer;
    int needs_boolean_default;

    if (out)
        *out = NULL;
    if (!name)
        return (FT_FAILURE);
    if (!element)
        return (FT_FAILURE);
    if (!out)
        return (FT_FAILURE);
    transpiler_cobol_buffer_init(&buffer);
    if (transpiler_cobol_buffer_append_indent(&buffer, indentation) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_level(&buffer, level) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_string(&buffer, " ") != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_string(&buffer, name) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    needs_boolean_default = 0;
    if (transpiler_cobol_buffer_append_string(&buffer, " ") != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_elementary_append_picture(&buffer, element) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (element->kind == TRANSPILE_COBOL_ELEMENTARY_BOOLEAN)
    {
        if (!value_text || value_text[0] == '\0')
            needs_boolean_default = 1;
    }
    if (value_text && value_text[0] != '\0')
    {
        if (transpiler_cobol_buffer_append_string(&buffer, " VALUE ") != FT_SUCCESS)
        {
            transpiler_cobol_buffer_dispose(&buffer);
            return (FT_FAILURE);
        }
        if (transpiler_cobol_buffer_append_string(&buffer, value_text) != FT_SUCCESS)
        {
            transpiler_cobol_buffer_dispose(&buffer);
            return (FT_FAILURE);
        }
    }
    else if (needs_boolean_default)
    {
        if (transpiler_cobol_buffer_append_string(&buffer, " VALUE 'N'") != FT_SUCCESS)
        {
            transpiler_cobol_buffer_dispose(&buffer);
            return (FT_FAILURE);
        }
    }
    if (transpiler_cobol_buffer_append_string(&buffer, ".\n") != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_copy_buffer(&buffer, out) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    transpiler_cobol_buffer_dispose(&buffer);
    return (FT_SUCCESS);
}

int transpiler_cobol_format_group(const t_transpiler_cobol_group *group,
    size_t indentation, char **out)
{
    t_transpiler_cobol_buffer buffer;
    size_t index;

    if (out)
        *out = NULL;
    if (!group)
        return (FT_FAILURE);
    if (!out)
        return (FT_FAILURE);
    transpiler_cobol_buffer_init(&buffer);
    if (transpiler_cobol_buffer_append_indent(&buffer, indentation) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_level(&buffer, group->level) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_string(&buffer, " ") != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_string(&buffer, group->name) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    if (transpiler_cobol_buffer_append_string(&buffer, ".\n") != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < group->field_count)
    {
        const t_transpiler_cobol_group_field *field;
        char *formatted;

        field = &group->fields[index];
        formatted = NULL;
        if (transpiler_cobol_format_elementary(field->name, field->level,
                &field->element, indentation + 1, field->value_text,
                &formatted) != FT_SUCCESS)
        {
            transpiler_cobol_buffer_dispose(&buffer);
            return (FT_FAILURE);
        }
        if (transpiler_cobol_buffer_append_string(&buffer, formatted) != FT_SUCCESS)
        {
            cma_free(formatted);
            transpiler_cobol_buffer_dispose(&buffer);
            return (FT_FAILURE);
        }
        cma_free(formatted);
        index += 1;
    }
    if (transpiler_cobol_copy_buffer(&buffer, out) != FT_SUCCESS)
    {
        transpiler_cobol_buffer_dispose(&buffer);
        return (FT_FAILURE);
    }
    transpiler_cobol_buffer_dispose(&buffer);
    return (FT_SUCCESS);
}
