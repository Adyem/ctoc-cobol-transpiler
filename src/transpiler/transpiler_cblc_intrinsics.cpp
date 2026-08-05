#include "cblc_transpiler.hpp"

#include <cstring>

static const t_cblc_intrinsic_entry g_cblc_intrinsics[] = {
    {"append", "cblc_string_append", NULL, 1, 1, 0, 1},
    {"len", "cblc_string_length", NULL, 0, 0, 1, 0},
    {"clear", "cblc_string_assign_literal", NULL, 0, 0, 0, 1},
    {"empty", "cblc_string_empty", NULL, 0, 0, 1, 0},
    {"equals", "cblc_string_equals", NULL, 1, 1, 1, 0},
    {"capacity", NULL, NULL, 0, 0, 1, 0},
    {"starts_with", "cblc_string_starts_with", NULL, 1, 1, 1, 0},
    {"ends_with", "cblc_string_ends_with", NULL, 1, 1, 1, 0},
    {"compare", "cblc_string_compare", NULL, 1, 1, 1, 0},
    {"contains", "cblc_string_contains", NULL, 1, 1, 1, 0}
};

const t_cblc_intrinsic_entry *cblc_intrinsic_get_entries(size_t *count)
{
    if (count)
        *count = sizeof(g_cblc_intrinsics) / sizeof(g_cblc_intrinsics[0]);
    return (g_cblc_intrinsics);
}

const t_cblc_intrinsic_entry *cblc_intrinsic_lookup(const char *name)
{
    size_t index;

    if (!name)
        return (NULL);
    index = 0;
    while (index < sizeof(g_cblc_intrinsics) / sizeof(g_cblc_intrinsics[0]))
    {
        if (std::strcmp(g_cblc_intrinsics[index].name, name) == 0)
            return (&g_cblc_intrinsics[index]);
        index += 1;
    }
    return (NULL);
}

int cblc_intrinsic_accepts_argument_count(const t_cblc_intrinsic_entry *entry,
    size_t argument_count)
{
    if (!entry || argument_count < entry->minimum_arguments)
        return (0);
    if (entry->maximum_arguments != static_cast<size_t>(-1)
        && argument_count > entry->maximum_arguments)
        return (0);
    return (1);
}
