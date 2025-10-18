#include "cblc_transpiler.hpp"

static t_runtime_collation_bridge g_runtime_collation_bridge = {NULL, NULL};

void runtime_collation_get_bridge(t_runtime_collation_bridge *bridge)
{
    if (!bridge)
        return ;
    bridge->compare = g_runtime_collation_bridge.compare;
    bridge->user_data = g_runtime_collation_bridge.user_data;
}

int runtime_collation_set_bridge(const t_runtime_collation_bridge *bridge)
{
    if (!bridge)
        return (FT_FAILURE);
    if (!bridge->compare)
        return (FT_FAILURE);
    g_runtime_collation_bridge.compare = bridge->compare;
    g_runtime_collation_bridge.user_data = bridge->user_data;
    return (FT_SUCCESS);
}

void runtime_collation_clear_bridge(void)
{
    g_runtime_collation_bridge.compare = NULL;
    g_runtime_collation_bridge.user_data = NULL;
}

int runtime_collation_compare(const char *left, size_t left_length, const char *right,
    size_t right_length, int *result)
{
    size_t index;
    unsigned char left_char;
    unsigned char right_char;

    if (!result)
        return (FT_FAILURE);
    *result = 0;
    if (!left && left_length > 0)
        return (FT_FAILURE);
    if (!right && right_length > 0)
        return (FT_FAILURE);
    if (g_runtime_collation_bridge.compare)
        return (g_runtime_collation_bridge.compare(left, left_length, right, right_length, result,
            g_runtime_collation_bridge.user_data));
    index = 0;
    while (index < left_length && index < right_length)
    {
        left_char = static_cast<unsigned char>(left[index]);
        right_char = static_cast<unsigned char>(right[index]);
        if (left_char < right_char)
        {
            *result = -1;
            return (FT_SUCCESS);
        }
        if (left_char > right_char)
        {
            *result = 1;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    if (left_length < right_length)
    {
        *result = -1;
        return (FT_SUCCESS);
    }
    if (left_length > right_length)
    {
        *result = 1;
        return (FT_SUCCESS);
    }
    return (FT_SUCCESS);
}
