#include "cblc_transpiler.hpp"

#include "libft/Libft/libft.hpp"

static int runtime_record_key_validate(const t_runtime_record_key *key)
{
    if (!key)
        return (FT_FAILURE);
    if (key->length == 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_record_compare_single_key(const t_runtime_record *left, const t_runtime_record *right,
    const t_runtime_record_key *key, int *result)
{
    size_t index;
    size_t left_index;
    size_t right_index;
    char left_value;
    char right_value;

    if (!left || !right || !key || !result)
        return (FT_FAILURE);
    if (!left->data || !right->data)
        return (FT_FAILURE);
    if (runtime_record_key_validate(key) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < key->length)
    {
        left_value = ' ';
        right_value = ' ';
        left_index = key->offset + index;
        if (left_index < left->length)
            left_value = left->data[left_index];
        right_index = key->offset + index;
        if (right_index < right->length)
            right_value = right->data[right_index];
        if (left_value != right_value)
        {
            if (left_value < right_value)
                *result = -1;
            else
                *result = 1;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    *result = 0;
    return (FT_SUCCESS);
}

static int runtime_record_compare_full(const t_runtime_record *left, const t_runtime_record *right, int *result)
{
    t_runtime_record_key key;

    if (!left || !right || !result)
        return (FT_FAILURE);
    key.offset = 0;
    if (left->length > right->length)
        key.length = left->length;
    else
        key.length = right->length;
    if (key.length == 0)
    {
        *result = 0;
        return (FT_SUCCESS);
    }
    key.ascending = 1;
    return (runtime_record_compare_single_key(left, right, &key, result));
}

int runtime_record_compare_keys(const t_runtime_record *left, const t_runtime_record *right,
    const t_runtime_record_key *keys, size_t key_count, int *result)
{
    size_t index;
    int comparison;

    if (!result)
        return (FT_FAILURE);
    *result = 0;
    if (!left || !right)
        return (FT_FAILURE);
    if (!left->data || !right->data)
        return (FT_FAILURE);
    if (!keys)
    {
        if (key_count == 0)
            return (runtime_record_compare_full(left, right, result));
        return (FT_FAILURE);
    }
    if (key_count == 0)
        return (runtime_record_compare_full(left, right, result));
    index = 0;
    while (index < key_count)
    {
        if (runtime_record_compare_single_key(left, right, keys + index, &comparison) != FT_SUCCESS)
            return (FT_FAILURE);
        if (comparison != 0)
        {
            if (keys[index].ascending)
                *result = comparison;
            else
                *result = -comparison;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    *result = 0;
    return (FT_SUCCESS);
}

int runtime_record_sort(t_runtime_record *records, size_t record_count, const t_runtime_record_key *keys,
    size_t key_count)
{
    size_t index;
    size_t position;
    t_runtime_record current;
    int comparison;

    if (!records)
        return (FT_FAILURE);
    if (record_count < 2)
        return (FT_SUCCESS);
    index = 1;
    while (index < record_count)
    {
        current = records[index];
        position = index;
        while (position > 0)
        {
            if (runtime_record_compare_keys(&records[position - 1], &current, keys, key_count, &comparison)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (comparison <= 0)
                break ;
            records[position] = records[position - 1];
            position -= 1;
        }
        records[position] = current;
        index += 1;
    }
    return (FT_SUCCESS);
}

int runtime_record_search_all(const t_runtime_record *records, size_t record_count, const t_runtime_record *target,
    const t_runtime_record_key *keys, size_t key_count, size_t *found_index, int *found)
{
    size_t left;
    size_t right;
    size_t middle;
    int comparison;
    int has_match;

    has_match = 0;
    if (found)
        *found = 0;
    if (found_index)
        *found_index = 0;
    if (!records || !target)
        return (FT_FAILURE);
    if (record_count == 0)
        return (FT_SUCCESS);
    left = 0;
    right = record_count;
    while (left < right)
    {
        middle = left + (right - left) / 2;
        if (runtime_record_compare_keys(&records[middle], target, keys, key_count, &comparison) != FT_SUCCESS)
            return (FT_FAILURE);
        if (comparison == 0)
        {
            has_match = 1;
            if (found_index)
                *found_index = middle;
            if (found)
                *found = 1;
            return (FT_SUCCESS);
        }
        if (comparison < 0)
            left = middle + 1;
        else
            right = middle;
    }
    if (found_index)
        *found_index = left;
    if (found)
        *found = has_match;
    return (FT_SUCCESS);
}
