#include "runtime_record.hpp"

#include "libft/CMA/CMA.hpp"

static int runtime_record_validate(t_runtime_record *record)
{
    if (!record)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_record_reserve(t_runtime_record *record, size_t desired_length)
{
    char *new_data;
    size_t desired_capacity;
    size_t index;
    size_t copy_length;

    if (runtime_record_validate(record) != FT_SUCCESS)
        return (FT_FAILURE);
    desired_capacity = desired_length;
    if (desired_capacity < 1)
        desired_capacity = 1;
    if (record->capacity >= desired_capacity)
        return (FT_SUCCESS);
    new_data = static_cast<char *>(cma_calloc(desired_capacity + 1, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    index = 0;
    copy_length = record->length;
    if (copy_length > desired_capacity)
        copy_length = desired_capacity;
    while (index < copy_length)
    {
        new_data[index] = record->data[index];
        index += 1;
    }
    if (record->data)
        cma_free(record->data);
    record->data = new_data;
    record->capacity = desired_capacity;
    record->length = copy_length;
    record->data[record->length] = '\0';
    return (FT_SUCCESS);
}

int runtime_record_init(t_runtime_record *record, size_t initial_capacity)
{
    if (runtime_record_validate(record) != FT_SUCCESS)
        return (FT_FAILURE);
    record->data = NULL;
    record->length = 0;
    record->capacity = 0;
    if (runtime_record_reserve(record, initial_capacity) != FT_SUCCESS)
        return (FT_FAILURE);
    record->length = 0;
    record->data[0] = '\0';
    return (FT_SUCCESS);
}

void runtime_record_dispose(t_runtime_record *record)
{
    if (runtime_record_validate(record) != FT_SUCCESS)
        return ;
    if (record->data)
        cma_free(record->data);
    record->data = NULL;
    record->length = 0;
    record->capacity = 0;
}

int runtime_record_set_length(t_runtime_record *record, size_t length)
{
    size_t index;
    size_t old_length;

    if (runtime_record_validate(record) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_record_reserve(record, length) != FT_SUCCESS)
        return (FT_FAILURE);
    old_length = record->length;
    if (old_length < length)
    {
        index = old_length;
        while (index < length)
        {
            record->data[index] = ' ';
            index += 1;
        }
    }
    else
    {
        index = length;
        while (index < old_length)
        {
            record->data[index] = ' ';
            index += 1;
        }
    }
    record->length = length;
    record->data[record->length] = '\0';
    return (FT_SUCCESS);
}

int runtime_record_fill(t_runtime_record *record, char value)
{
    size_t index;

    if (runtime_record_validate(record) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!record->data)
        return (FT_FAILURE);
    index = 0;
    while (index < record->length)
    {
        record->data[index] = value;
        index += 1;
    }
    record->data[record->length] = '\0';
    return (FT_SUCCESS);
}

int runtime_record_copy_from_buffer(t_runtime_record *record, const char *buffer, size_t length)
{
    size_t index;

    if (runtime_record_validate(record) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!record->data)
        return (FT_FAILURE);
    if (!buffer && length > 0)
        return (FT_FAILURE);
    if (length > record->length)
        return (FT_FAILURE);
    index = 0;
    while (index < length)
    {
        record->data[index] = buffer[index];
        index += 1;
    }
    while (index < record->length)
    {
        record->data[index] = ' ';
        index += 1;
    }
    record->data[record->length] = '\0';
    return (FT_SUCCESS);
}

int runtime_record_copy_to_buffer(const t_runtime_record *record, char *buffer, size_t buffer_size, size_t *written)
{
    size_t index;

    if (!record)
        return (FT_FAILURE);
    if (!record->data)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size <= record->length)
        return (FT_FAILURE);
    index = 0;
    while (index < record->length)
    {
        buffer[index] = record->data[index];
        index += 1;
    }
    buffer[index] = '\0';
    if (written)
        *written = record->length;
    return (FT_SUCCESS);
}
