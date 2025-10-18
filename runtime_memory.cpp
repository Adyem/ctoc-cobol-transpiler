#include "cblc_transpiler.hpp"

static int runtime_memory_overlaps(const unsigned char *destination, size_t destination_length,
    const unsigned char *source, size_t source_length)
{
    if (destination_length == 0 || source_length == 0)
        return (0);
    if (destination + destination_length <= source)
        return (0);
    if (source + source_length <= destination)
        return (0);
    return (1);
}

static void runtime_memory_move(unsigned char *destination, const unsigned char *source, size_t length)
{
    size_t index;

    if (destination <= source)
    {
        index = 0;
        while (index < length)
        {
            destination[index] = source[index];
            index += 1;
        }
        return ;
    }
    index = length;
    while (index > 0)
    {
        index -= 1;
        destination[index] = source[index];
    }
}

int runtime_memory_copy_checked(void *destination, size_t destination_length, const void *source,
    size_t source_length)
{
    unsigned char *destination_bytes;
    const unsigned char *source_bytes;

    if (!destination || !source)
        return (FT_FAILURE);
    if (source_length > destination_length)
        return (FT_FAILURE);
    if (source_length == 0)
        return (FT_SUCCESS);
    destination_bytes = static_cast<unsigned char *>(destination);
    source_bytes = static_cast<const unsigned char *>(source);
    if (runtime_memory_overlaps(destination_bytes, destination_length, source_bytes, source_length))
    {
        runtime_memory_move(destination_bytes, source_bytes, source_length);
        return (FT_SUCCESS);
    }
    ft_memcpy(destination_bytes, source_bytes, source_length);
    return (FT_SUCCESS);
}
