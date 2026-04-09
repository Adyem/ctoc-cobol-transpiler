#include <stddef.h>
#include <stdio.h>

static size_t cblc_min_size(size_t left, size_t right)
{
    if (left < right)
        return (left);
    return (right);
}

static size_t cblc_string_length(const char *text)
{
    size_t length;

    if (!text)
        return (0);
    length = 0;
    while (text[length] != '\0')
        length += 1;
    return (length);
}

static void cblc_string_assign_literal(char *buffer, size_t capacity, size_t *length, const char *literal)
{
    size_t literal_length;
    size_t index;

    literal_length = cblc_string_length(literal);
    if (literal_length > capacity)
        literal_length = capacity;
    index = 0;
    while (index < literal_length)
    {
        buffer[index] = literal[index];
        index += 1;
    }
    while (index < capacity)
    {
        buffer[index] = ' ';
        index += 1;
    }
    if (length)
        *length = literal_length;
}

static void cblc_string_copy(char *destination, size_t destination_capacity, size_t *destination_length, const char *source, size_t source_length)
{
    size_t copy_length;
    size_t index;

    copy_length = source_length;
    if (copy_length > destination_capacity)
        copy_length = destination_capacity;
    index = 0;
    while (index < copy_length)
    {
        destination[index] = source[index];
        index += 1;
    }
    while (index < destination_capacity)
    {
        destination[index] = ' ';
        index += 1;
    }
    if (destination_length)
        *destination_length = copy_length;
}

static void cblc_string_append_literal(char *buffer, size_t capacity, size_t *length, const char *literal)
{
    size_t start;
    size_t literal_length;
    size_t write_length;
    size_t index;

    if (!buffer || !length)
        return ;
    start = *length;
    if (start > capacity)
        start = capacity;
    literal_length = cblc_string_length(literal);
    write_length = literal_length;
    if (start + write_length > capacity)
        write_length = capacity - start;
    index = 0;
    while (index < write_length)
    {
        buffer[start + index] = literal[index];
        index += 1;
    }
    *length = start + write_length;
    while (start + index < capacity)
    {
        buffer[start + index] = ' ';
        index += 1;
    }
}

static void cblc_string_append(char *destination, size_t destination_capacity, size_t *destination_length, const char *source, size_t source_length)
{
    size_t start;
    size_t copy_length;
    size_t index;

    if (!destination || !destination_length)
        return ;
    start = *destination_length;
    if (start > destination_capacity)
        start = destination_capacity;
    copy_length = source_length;
    if (start + copy_length > destination_capacity)
        copy_length = destination_capacity - start;
    index = 0;
    while (index < copy_length)
    {
        destination[start + index] = source[index];
        index += 1;
    }
    *destination_length = start + copy_length;
    while (start + index < destination_capacity)
    {
        destination[start + index] = ' ';
        index += 1;
    }
}

static void cblc_char_assign_literal(char *buffer, size_t capacity, const char *literal)
{
    size_t literal_length;
    size_t index;

    literal_length = cblc_string_length(literal);
    if (literal_length > capacity)
        literal_length = capacity;
    index = 0;
    while (index < literal_length)
    {
        buffer[index] = literal[index];
        index += 1;
    }
    while (index < capacity)
    {
        buffer[index] = ' ';
        index += 1;
    }
}

static void cblc_char_copy(char *destination, size_t destination_length, const char *source, size_t source_length)
{
    size_t copy_length;
    size_t index;

    copy_length = cblc_min_size(destination_length, source_length);
    index = 0;
    while (index < copy_length)
    {
        destination[index] = source[index];
        index += 1;
    }
    while (index < destination_length)
    {
        destination[index] = ' ';
        index += 1;
    }
}

static void cblc_display_string(const char *buffer, size_t length)
{
    size_t index;

    if (!buffer)
        return ;
    index = 0;
    while (index < length)
    {
        fputc(buffer[index], stdout);
        index += 1;
    }
    fputc('\n', stdout);
}

static void cblc_display_char_buffer(const char *buffer, size_t length)
{
    size_t index;

    if (!buffer)
        return ;
    index = 0;
    while (index < length)
    {
        fputc(buffer[index], stdout);
        index += 1;
    }
    fputc('\n', stdout);
}

static void cblc_display_int(int value)
{
    printf("%d\n", value);
}

static void cblc_display_size(size_t value)
{
    printf("%zu\n", value);
}

static void cblc_display_literal(const char *literal)
{
    if (!literal)
        return ;
    printf("%s\n", literal);
}
