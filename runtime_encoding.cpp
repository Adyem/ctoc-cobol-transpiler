#include "cblc_transpiler.hpp"

static const unsigned char g_runtime_encoding_default_to_ascii[RUNTIME_ENCODING_TABLE_SIZE] = {
    0, 1, 2, 3, 156, 9, 134, 127, 151, 141, 142, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 157, 133, 8, 135, 24, 25, 146, 143, 28, 29, 30, 31,
    128, 129, 130, 131, 132, 10, 23, 27, 136, 137, 138, 139, 140, 5, 6, 7,
    144, 145, 22, 147, 148, 149, 150, 4, 152, 153, 154, 155, 20, 21, 158, 26,
    32, 160, 226, 228, 224, 225, 227, 229, 231, 241, 162, 46, 60, 40, 43, 124,
    38, 233, 234, 235, 232, 237, 238, 239, 236, 223, 33, 36, 42, 41, 59, 172,
    45, 47, 194, 196, 192, 193, 195, 197, 199, 209, 166, 44, 37, 95, 62, 63,
    248, 201, 202, 203, 200, 205, 206, 207, 204, 96, 58, 35, 64, 39, 61, 34,
    216, 97, 98, 99, 100, 101, 102, 103, 104, 105, 171, 187, 240, 253, 254, 177,
    176, 106, 107, 108, 109, 110, 111, 112, 113, 114, 170, 186, 230, 184, 198, 164,
    181, 126, 115, 116, 117, 118, 119, 120, 121, 122, 161, 191, 208, 221, 222, 174,
    94, 163, 165, 183, 169, 167, 182, 188, 189, 190, 91, 93, 175, 168, 180, 215,
    123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 173, 244, 246, 242, 243, 245,
    125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 185, 251, 252, 249, 250, 255,
    92, 247, 83, 84, 85, 86, 87, 88, 89, 90, 178, 212, 214, 210, 211, 213,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 179, 219, 220, 217, 218, 159
};

static const unsigned char g_runtime_encoding_default_from_ascii[RUNTIME_ENCODING_TABLE_SIZE] = {
    0, 1, 2, 3, 55, 45, 46, 47, 22, 5, 37, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31,
    64, 90, 127, 123, 91, 108, 80, 125, 77, 93, 92, 78, 107, 96, 75, 97,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 122, 94, 76, 126, 110, 111,
    124, 193, 194, 195, 196, 197, 198, 199, 200, 201, 209, 210, 211, 212, 213, 214,
    215, 216, 217, 226, 227, 228, 229, 230, 231, 232, 233, 186, 224, 187, 176, 109,
    121, 129, 130, 131, 132, 133, 134, 135, 136, 137, 145, 146, 147, 148, 149, 150,
    151, 152, 153, 162, 163, 164, 165, 166, 167, 168, 169, 192, 79, 208, 161, 7,
    32, 33, 34, 35, 36, 21, 6, 23, 40, 41, 42, 43, 44, 9, 10, 27,
    48, 49, 26, 51, 52, 53, 54, 8, 56, 57, 58, 59, 4, 20, 62, 255,
    65, 170, 74, 177, 159, 178, 106, 181, 189, 180, 154, 138, 95, 202, 175, 188,
    144, 143, 234, 250, 190, 160, 182, 179, 157, 218, 155, 139, 183, 184, 185, 171,
    100, 101, 98, 102, 99, 103, 158, 104, 116, 113, 114, 115, 120, 117, 118, 119,
    172, 105, 237, 238, 235, 239, 236, 191, 128, 253, 254, 251, 252, 173, 174, 89,
    68, 69, 66, 70, 67, 71, 156, 72, 84, 81, 82, 83, 88, 85, 86, 87,
    140, 73, 205, 206, 203, 207, 204, 225, 112, 221, 222, 219, 220, 141, 142, 223
};

static unsigned char g_runtime_encoding_to_ascii[RUNTIME_ENCODING_TABLE_SIZE];
static unsigned char g_runtime_encoding_from_ascii[RUNTIME_ENCODING_TABLE_SIZE];
static int g_runtime_encoding_initialized = 0;

static void runtime_encoding_initialize(void)
{
    size_t index;

    if (g_runtime_encoding_initialized)
        return ;
    index = 0;
    while (index < RUNTIME_ENCODING_TABLE_SIZE)
    {
        g_runtime_encoding_to_ascii[index] = g_runtime_encoding_default_to_ascii[index];
        g_runtime_encoding_from_ascii[index] = g_runtime_encoding_default_from_ascii[index];
        index += 1;
    }
    g_runtime_encoding_initialized = 1;
}

int runtime_encoding_reset(void)
{
    size_t index;

    runtime_encoding_initialize();
    index = 0;
    while (index < RUNTIME_ENCODING_TABLE_SIZE)
    {
        g_runtime_encoding_to_ascii[index] = g_runtime_encoding_default_to_ascii[index];
        g_runtime_encoding_from_ascii[index] = g_runtime_encoding_default_from_ascii[index];
        index += 1;
    }
    return (FT_SUCCESS);
}

int runtime_encoding_get_active(t_runtime_encoding_table *table)
{
    size_t index;

    if (!table)
        return (FT_FAILURE);
    runtime_encoding_initialize();
    index = 0;
    while (index < RUNTIME_ENCODING_TABLE_SIZE)
    {
        table->to_ascii[index] = g_runtime_encoding_to_ascii[index];
        table->from_ascii[index] = g_runtime_encoding_from_ascii[index];
        index += 1;
    }
    return (FT_SUCCESS);
}

int runtime_encoding_set_active(const t_runtime_encoding_table *table)
{
    size_t index;

    if (!table)
        return (FT_FAILURE);
    runtime_encoding_initialize();
    index = 0;
    while (index < RUNTIME_ENCODING_TABLE_SIZE)
    {
        g_runtime_encoding_to_ascii[index] = table->to_ascii[index];
        g_runtime_encoding_from_ascii[index] = table->from_ascii[index];
        index += 1;
    }
    return (FT_SUCCESS);
}

static int runtime_encoding_validate_buffers(const unsigned char *source, size_t source_length,
    unsigned char *destination, size_t destination_length)
{
    if (!destination)
        return (FT_FAILURE);
    if (source_length > destination_length)
        return (FT_FAILURE);
    if (!source && source_length > 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int runtime_encoding_transcode_to_ascii(const unsigned char *source, size_t source_length,
    unsigned char *destination, size_t destination_length, size_t *written_length)
{
    size_t index;

    if (runtime_encoding_validate_buffers(source, source_length, destination, destination_length)
        != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_encoding_initialize();
    index = 0;
    while (index < source_length)
    {
        destination[index] = g_runtime_encoding_to_ascii[source[index]];
        index += 1;
    }
    if (written_length)
        *written_length = source_length;
    return (FT_SUCCESS);
}

int runtime_encoding_transcode_to_ebcdic(const unsigned char *source, size_t source_length,
    unsigned char *destination, size_t destination_length, size_t *written_length)
{
    size_t index;

    if (runtime_encoding_validate_buffers(source, source_length, destination, destination_length)
        != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_encoding_initialize();
    index = 0;
    while (index < source_length)
    {
        destination[index] = g_runtime_encoding_from_ascii[source[index]];
        index += 1;
    }
    if (written_length)
        *written_length = source_length;
    return (FT_SUCCESS);
}
