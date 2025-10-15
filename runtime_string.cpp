#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

static int runtime_string_reserve(t_runtime_string *value, size_t desired_capacity)
{
    char *new_data;
    size_t index;

    if (!value)
        return (FT_FAILURE);
    if (value->capacity >= desired_capacity)
        return (FT_SUCCESS);
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (value->data)
    {
        index = 0;
        while (index < value->length && index + 1 < desired_capacity)
        {
            new_data[index] = value->data[index];
            index += 1;
        }
        cma_free(value->data);
    }
    value->data = new_data;
    if (value->length + 1 > desired_capacity)
        value->length = desired_capacity - 1;
    value->capacity = desired_capacity;
    value->data[value->length] = '\0';
    return (FT_SUCCESS);
}

int runtime_string_init(t_runtime_string *value, size_t initial_capacity)
{
    size_t capacity;

    if (!value)
        return (FT_FAILURE);
    value->data = NULL;
    value->length = 0;
    value->capacity = 0;
    capacity = initial_capacity;
    if (capacity < 1)
        capacity = 1;
    if (runtime_string_reserve(value, capacity) != FT_SUCCESS)
        return (FT_FAILURE);
    value->length = 0;
    value->data[0] = '\0';
    return (FT_SUCCESS);
}

void runtime_string_dispose(t_runtime_string *value)
{
    if (!value)
        return ;
    if (value->data)
        cma_free(value->data);
    value->data = NULL;
    value->length = 0;
    value->capacity = 0;
}

int runtime_string_assign(t_runtime_string *value, const char *text)
{
    size_t length;
    size_t index;

    if (!value)
        return (FT_FAILURE);
    if (!text)
        text = "";
    length = ft_strlen(text);
    if (runtime_string_reserve(value, length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < length)
    {
        value->data[index] = text[index];
        index += 1;
    }
    value->data[length] = '\0';
    value->length = length;
    return (FT_SUCCESS);
}

static int runtime_string_is_trim_char(char value)
{
    if (value == ' ')
        return (1);
    if (value == '\t')
        return (1);
    if (value == '\n')
        return (1);
    if (value == '\r')
        return (1);
    if (value == '\f')
        return (1);
    if (value == '\v')
        return (1);
    return (0);
}

int runtime_string_trim(t_runtime_string *value)
{
    size_t start;
    size_t end;
    size_t index;
    size_t length;
    size_t old_length;

    if (!value)
        return (FT_FAILURE);
    if (!value->data)
        return (FT_FAILURE);
    old_length = value->length;
    start = 0;
    while (start < value->length && runtime_string_is_trim_char(value->data[start]))
        start += 1;
    end = value->length;
    while (end > start && runtime_string_is_trim_char(value->data[end - 1]))
        end -= 1;
    length = end - start;
    if (start > 0 && length > 0)
    {
        index = 0;
        while (index < length)
        {
            value->data[index] = value->data[start + index];
            index += 1;
        }
    }
    if (length == 0)
        value->data[0] = '\0';
    else
        value->data[length] = '\0';
    if (length == 0)
        index = 1;
    else
        index = length + 1;
    while (index < old_length)
    {
        value->data[index] = '\0';
        index += 1;
    }
    value->length = length;
    return (FT_SUCCESS);
}

int runtime_string_compare(const t_runtime_string *left, const t_runtime_string *right)
{
    size_t index;
    unsigned char left_char;
    unsigned char right_char;

    if (!left || !right)
        return (0);
    if (!left->data || !right->data)
        return (0);
    index = 0;
    while (index < left->length && index < right->length)
    {
        left_char = static_cast<unsigned char>(left->data[index]);
        right_char = static_cast<unsigned char>(right->data[index]);
        if (left_char < right_char)
            return (-1);
        if (left_char > right_char)
            return (1);
        index += 1;
    }
    if (left->length < right->length)
        return (-1);
    if (left->length > right->length)
        return (1);
    return (0);
}

int runtime_string_to_int(const t_runtime_string *value, t_runtime_int *destination)
{
    size_t start;
    size_t end;
    size_t length;
    size_t index;
    char *buffer;
    int status;

    if (!value || !destination)
        return (FT_FAILURE);
    if (!value->data)
        return (FT_FAILURE);
    start = 0;
    while (start < value->length && runtime_string_is_trim_char(value->data[start]))
        start += 1;
    end = value->length;
    while (end > start && runtime_string_is_trim_char(value->data[end - 1]))
        end -= 1;
    length = end - start;
    buffer = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    index = 0;
    while (index < length)
    {
        buffer[index] = value->data[start + index];
        index += 1;
    }
    buffer[length] = '\0';
    status = runtime_int_from_string(destination, buffer);
    cma_free(buffer);
    return (status);
}

int runtime_string_equals(const t_runtime_string *left, const t_runtime_string *right)
{
    size_t index;

    if (!left || !right)
        return (0);
    if (!left->data || !right->data)
        return (0);
    if (left->length != right->length)
        return (0);
    index = 0;
    while (index < left->length)
    {
        if (left->data[index] != right->data[index])
            return (0);
        index += 1;
    }
    return (1);
}

int runtime_string_concat(t_runtime_string *destination, const t_runtime_string *left,
    const t_runtime_string *right)
{
    size_t total_length;
    size_t index;
    const char *left_data;
    const char *right_data;
    char *left_copy;
    char *right_copy;

    if (!destination || !left || !right)
        return (FT_FAILURE);
    if (!left->data || !right->data)
        return (FT_FAILURE);
    total_length = left->length + right->length;
    if (runtime_string_reserve(destination, total_length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    left_copy = NULL;
    right_copy = NULL;
    left_data = left->data;
    right_data = right->data;
    if (left == destination)
    {
        left_copy = static_cast<char *>(cma_calloc(left->length + 1, sizeof(char)));
        if (!left_copy)
            return (FT_FAILURE);
        if (left->length > 0)
            ft_memcpy(left_copy, left->data, left->length);
        left_data = left_copy;
    }
    if (right == destination)
    {
        right_copy = static_cast<char *>(cma_calloc(right->length + 1, sizeof(char)));
        if (!right_copy)
        {
            if (left_copy)
                cma_free(left_copy);
            return (FT_FAILURE);
        }
        if (right->length > 0)
            ft_memcpy(right_copy, right->data, right->length);
        right_data = right_copy;
    }
    index = 0;
    while (index < left->length)
    {
        destination->data[index] = left_data[index];
        index += 1;
    }
    index = 0;
    while (index < right->length)
    {
        destination->data[left->length + index] = right_data[index];
        index += 1;
    }
    destination->data[total_length] = '\0';
    destination->length = total_length;
    if (left_copy)
        cma_free(left_copy);
    if (right_copy)
        cma_free(right_copy);
    return (FT_SUCCESS);
}
