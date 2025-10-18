#include "cblc_transpiler.hpp"

#include <unistd.h>

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

static int runtime_line_validate_file(t_runtime_file *file)
{
    if (!file)
        return (FT_FAILURE);
    if (file->descriptor < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int runtime_line_read_fixed(t_runtime_file *file, size_t record_length, t_runtime_string *destination,
    int *end_of_file)
{
    char *buffer;
    size_t total_read;
    ssize_t chunk;

    if (end_of_file)
        *end_of_file = 0;
    if (!destination)
        return (FT_FAILURE);
    if (record_length == 0)
        return (FT_FAILURE);
    if (runtime_line_validate_file(file) != FT_SUCCESS)
        return (FT_FAILURE);
    buffer = static_cast<char *>(cma_calloc(record_length + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    total_read = 0;
    while (total_read < record_length)
    {
        chunk = read(file->descriptor, buffer + total_read, record_length - total_read);
        if (chunk < 0)
        {
            cma_free(buffer);
            return (FT_FAILURE);
        }
        if (chunk == 0)
        {
            if (total_read == 0)
            {
                if (runtime_string_assign(destination, "") != FT_SUCCESS)
                {
                    cma_free(buffer);
                    return (FT_FAILURE);
                }
                if (end_of_file)
                    *end_of_file = 1;
                cma_free(buffer);
                return (FT_SUCCESS);
            }
            break ;
        }
        total_read += static_cast<size_t>(chunk);
    }
    buffer[total_read] = '\0';
    if (runtime_string_assign(destination, buffer) != FT_SUCCESS)
    {
        cma_free(buffer);
        return (FT_FAILURE);
    }
    if (end_of_file && total_read < record_length)
        *end_of_file = 1;
    cma_free(buffer);
    return (FT_SUCCESS);
}

int runtime_line_read_variable(t_runtime_file *file, char terminator, t_runtime_string *destination,
    int *end_of_file)
{
    char *buffer;
    size_t capacity;
    size_t length;
    ssize_t chunk;
    unsigned char byte;
    int reached_end;
    size_t index;

    if (end_of_file)
        *end_of_file = 0;
    if (!destination)
        return (FT_FAILURE);
    if (runtime_line_validate_file(file) != FT_SUCCESS)
        return (FT_FAILURE);
    capacity = 64;
    buffer = static_cast<char *>(cma_calloc(capacity + 1, sizeof(char)));
    if (!buffer)
        return (FT_FAILURE);
    length = 0;
    reached_end = 0;
    while (1)
    {
        chunk = read(file->descriptor, &byte, 1);
        if (chunk < 0)
        {
            cma_free(buffer);
            return (FT_FAILURE);
        }
        if (chunk == 0)
        {
            if (length == 0)
            {
                if (runtime_string_assign(destination, "") != FT_SUCCESS)
                {
                    cma_free(buffer);
                    return (FT_FAILURE);
                }
                if (end_of_file)
                    *end_of_file = 1;
                cma_free(buffer);
                return (FT_SUCCESS);
            }
            reached_end = 1;
            break ;
        }
        if (byte == static_cast<unsigned char>('\r'))
            continue ;
        if (byte == static_cast<unsigned char>(terminator) || byte == static_cast<unsigned char>('\n'))
            break ;
        if (length + 1 >= capacity)
        {
            char *new_buffer;

            new_buffer = static_cast<char *>(cma_calloc((capacity * 2) + 1, sizeof(char)));
            if (!new_buffer)
            {
                cma_free(buffer);
                return (FT_FAILURE);
            }
            index = 0;
            while (index < length)
            {
                new_buffer[index] = buffer[index];
                index += 1;
            }
            cma_free(buffer);
            buffer = new_buffer;
            capacity *= 2;
        }
        buffer[length] = static_cast<char>(byte);
        length += 1;
    }
    buffer[length] = '\0';
    if (runtime_string_assign(destination, buffer) != FT_SUCCESS)
    {
        cma_free(buffer);
        return (FT_FAILURE);
    }
    if (end_of_file && reached_end)
        *end_of_file = 1;
    cma_free(buffer);
    return (FT_SUCCESS);
}

int runtime_line_write_fixed(t_runtime_file *file, const char *buffer, size_t length, size_t record_length)
{
    char *workspace;
    size_t copy_length;

    if (runtime_line_validate_file(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (record_length == 0)
        return (FT_FAILURE);
    if (!buffer && length > 0)
        return (FT_FAILURE);
    workspace = static_cast<char *>(cma_calloc(record_length, sizeof(char)));
    if (!workspace)
        return (FT_FAILURE);
    if (runtime_string_blank(workspace, record_length) != FT_SUCCESS)
    {
        cma_free(workspace);
        return (FT_FAILURE);
    }
    copy_length = length;
    if (copy_length > record_length)
        copy_length = record_length;
    if (copy_length > 0)
        ft_memcpy(workspace, buffer, copy_length);
    if (runtime_file_write(file, workspace, record_length) != FT_SUCCESS)
    {
        cma_free(workspace);
        return (FT_FAILURE);
    }
    cma_free(workspace);
    return (FT_SUCCESS);
}

int runtime_line_write_variable(t_runtime_file *file, const char *buffer, size_t length, char terminator)
{
    char terminator_buffer[1];

    if (runtime_line_validate_file(file) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!buffer && length > 0)
        return (FT_FAILURE);
    if (length > 0)
    {
        if (runtime_file_write(file, buffer, length) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (terminator != '\0')
    {
        terminator_buffer[0] = terminator;
        if (runtime_file_write(file, terminator_buffer, 1) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int runtime_csv_field_analyze(const t_runtime_string *field, int *requires_quotes, size_t *quote_count)
{
    size_t index;
    char value;
    int needs_quotes;
    size_t quotes;

    if (!field || !requires_quotes || !quote_count)
        return (FT_FAILURE);
    if (field->length > 0 && !field->data)
        return (FT_FAILURE);
    needs_quotes = 0;
    quotes = 0;
    index = 0;
    while (index < field->length)
    {
        value = field->data[index];
        if (value == ',' || value == '\n' || value == '\r')
            needs_quotes = 1;
        if (value == ' ')
            needs_quotes = 1;
        if (value == '"')
        {
            needs_quotes = 1;
            quotes += 1;
        }
        index += 1;
    }
    if (field->length > 0)
    {
        if (field->data[0] == ' ' || field->data[field->length - 1] == ' ')
            needs_quotes = 1;
    }
    *requires_quotes = needs_quotes;
    *quote_count = quotes;
    return (FT_SUCCESS);
}

int runtime_csv_format_line(const t_runtime_string *fields, size_t field_count, t_runtime_string *destination)
{
    int *quote_flags;
    size_t *quote_counts;
    size_t index;
    size_t total_length;
    char *buffer;
    size_t position;
    size_t field_index;

    if (!destination)
        return (FT_FAILURE);
    if (!fields && field_count > 0)
        return (FT_FAILURE);
    if (field_count == 0)
        return (runtime_string_assign(destination, ""));
    quote_flags = static_cast<int *>(cma_calloc(field_count, sizeof(int)));
    if (!quote_flags)
        return (FT_FAILURE);
    quote_counts = static_cast<size_t *>(cma_calloc(field_count, sizeof(size_t)));
    if (!quote_counts)
    {
        cma_free(quote_flags);
        return (FT_FAILURE);
    }
    total_length = 0;
    index = 0;
    while (index < field_count)
    {
        if (runtime_csv_field_analyze(&fields[index], &quote_flags[index], &quote_counts[index]) != FT_SUCCESS)
        {
            cma_free(quote_counts);
            cma_free(quote_flags);
            return (FT_FAILURE);
        }
        if (quote_flags[index])
            total_length += fields[index].length + 2 + quote_counts[index];
        else
            total_length += fields[index].length;
        if (index + 1 < field_count)
            total_length += 1;
        index += 1;
    }
    buffer = static_cast<char *>(cma_calloc(total_length + 1, sizeof(char)));
    if (!buffer)
    {
        cma_free(quote_counts);
        cma_free(quote_flags);
        return (FT_FAILURE);
    }
    position = 0;
    index = 0;
    while (index < field_count)
    {
        if (index > 0)
        {
            buffer[position] = ',';
            position += 1;
        }
        if (!quote_flags[index])
        {
            field_index = 0;
            while (field_index < fields[index].length)
            {
                buffer[position] = fields[index].data[field_index];
                position += 1;
                field_index += 1;
            }
        }
        else
        {
            buffer[position] = '"';
            position += 1;
            field_index = 0;
            while (field_index < fields[index].length)
            {
                buffer[position] = fields[index].data[field_index];
                position += 1;
                if (fields[index].data[field_index] == '"')
                {
                    buffer[position] = '"';
                    position += 1;
                }
                field_index += 1;
            }
            buffer[position] = '"';
            position += 1;
        }
        index += 1;
    }
    buffer[position] = '\0';
    if (runtime_string_assign(destination, buffer) != FT_SUCCESS)
    {
        cma_free(buffer);
        cma_free(quote_counts);
        cma_free(quote_flags);
        return (FT_FAILURE);
    }
    cma_free(buffer);
    cma_free(quote_counts);
    cma_free(quote_flags);
    return (FT_SUCCESS);
}

int runtime_csv_parse_line(const char *line, t_runtime_string *fields, size_t field_capacity, size_t *field_count)
{
    size_t length;
    char *workspace;
    size_t index;
    size_t count;
    size_t workspace_length;
    int closed_quote;

    if (field_count)
        *field_count = 0;
    if (!line || !fields)
        return (FT_FAILURE);
    if (field_capacity == 0)
        return (FT_FAILURE);
    length = ft_strlen(line);
    while (length > 0 && (line[length - 1] == '\n' || line[length - 1] == '\r'))
        length -= 1;
    workspace = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!workspace)
        return (FT_FAILURE);
    index = 0;
    count = 0;
    while (index <= length)
    {
        if (count >= field_capacity)
        {
            cma_free(workspace);
            return (FT_FAILURE);
        }
        if (index == length)
        {
            workspace[0] = '\0';
            if (runtime_string_assign(&fields[count], workspace) != FT_SUCCESS)
            {
                cma_free(workspace);
                return (FT_FAILURE);
            }
            count += 1;
            break;
        }
        workspace_length = 0;
        if (line[index] == '"')
        {
            index += 1;
            closed_quote = 0;
            while (index < length)
            {
                if (line[index] == '"')
                {
                    if (index + 1 < length && line[index + 1] == '"')
                    {
                        workspace[workspace_length] = '"';
                        workspace_length += 1;
                        index += 2;
                    }
                    else
                    {
                        index += 1;
                        closed_quote = 1;
                        break;
                    }
                }
                else
                {
                    workspace[workspace_length] = line[index];
                    workspace_length += 1;
                    index += 1;
                }
            }
            if (!closed_quote)
            {
                cma_free(workspace);
                return (FT_FAILURE);
            }
            while (index < length && line[index] == ' ')
                index += 1;
            if (index < length && line[index] != ',')
            {
                cma_free(workspace);
                return (FT_FAILURE);
            }
        }
        else
        {
            while (index < length && line[index] != ',')
            {
                workspace[workspace_length] = line[index];
                workspace_length += 1;
                index += 1;
            }
        }
        workspace[workspace_length] = '\0';
        if (runtime_string_assign(&fields[count], workspace) != FT_SUCCESS)
        {
            cma_free(workspace);
            return (FT_FAILURE);
        }
        count += 1;
        if (index >= length)
            break;
        if (line[index] == ',')
        {
            index += 1;
            if (index == length)
            {
                if (count >= field_capacity)
                {
                    cma_free(workspace);
                    return (FT_FAILURE);
                }
                workspace[0] = '\0';
                if (runtime_string_assign(&fields[count], workspace) != FT_SUCCESS)
                {
                    cma_free(workspace);
                    return (FT_FAILURE);
                }
                count += 1;
                break;
            }
        }
    }
    cma_free(workspace);
    if (field_count)
        *field_count = count;
    return (FT_SUCCESS);
}
