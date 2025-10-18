#include "cblc_transpiler.hpp"

#include "libft/Libft/libft.hpp"

#include "test_suites.hpp"

static int runtime_sort_init_record(t_runtime_record *record, size_t length, const char *text)
{
    size_t content_length;

    if (runtime_record_init(record, length) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_record_set_length(record, length) != FT_SUCCESS)
    {
        runtime_record_dispose(record);
        return (FT_FAILURE);
    }
    if (!text)
        text = "";
    content_length = ft_strlen(text);
    if (content_length > length)
        content_length = length;
    if (runtime_record_copy_from_buffer(record, text, content_length) != FT_SUCCESS)
    {
        runtime_record_dispose(record);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static void runtime_sort_dispose_records(t_runtime_record *records, size_t count)
{
    size_t index;

    index = 0;
    while (index < count)
    {
        runtime_record_dispose(&records[index]);
        index += 1;
    }
}

FT_TEST(test_runtime_record_sort_orders_single_key)
{
    t_runtime_record records[4];
    const char *values[] = {"B200", "A100", "D010", "C900"};
    const char *expected[] = {"A100", "B200", "C900", "D010"};
    t_runtime_record_key key;
    size_t index;

    index = 0;
    while (index < 4)
    {
        if (runtime_sort_init_record(&records[index], 4, values[index]) != FT_SUCCESS)
        {
            runtime_sort_dispose_records(records, index);
            return (FT_FAILURE);
        }
        index += 1;
    }
    key.offset = 0;
    key.length = 1;
    key.ascending = 1;
    if (test_expect_success(runtime_record_sort(records, 4, &key, 1),
            "runtime_record_sort should succeed for single-key sort") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 4);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < 4)
    {
        if (test_expect_cstring_equal(records[index].data, expected[index],
                "runtime_record_sort should order records lexicographically") != FT_SUCCESS)
        {
            runtime_sort_dispose_records(records, 4);
            return (FT_FAILURE);
        }
        index += 1;
    }
    runtime_sort_dispose_records(records, 4);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_sort_multiple_keys_with_descending)
{
    t_runtime_record records[4];
    const char *values[] = {"A200", "A500", "A300", "B100"};
    const char *expected[] = {"A500", "A300", "A200", "B100"};
    t_runtime_record_key keys[2];
    size_t index;

    index = 0;
    while (index < 4)
    {
        if (runtime_sort_init_record(&records[index], 4, values[index]) != FT_SUCCESS)
        {
            runtime_sort_dispose_records(records, index);
            return (FT_FAILURE);
        }
        index += 1;
    }
    keys[0].offset = 0;
    keys[0].length = 1;
    keys[0].ascending = 1;
    keys[1].offset = 1;
    keys[1].length = 3;
    keys[1].ascending = 0;
    if (test_expect_success(runtime_record_sort(records, 4, keys, 2),
            "runtime_record_sort should support multiple keys with descending order") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 4);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < 4)
    {
        if (test_expect_cstring_equal(records[index].data, expected[index],
                "runtime_record_sort should respect secondary descending key") != FT_SUCCESS)
        {
            runtime_sort_dispose_records(records, 4);
            return (FT_FAILURE);
        }
        index += 1;
    }
    runtime_sort_dispose_records(records, 4);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_search_all_finds_sorted_record)
{
    t_runtime_record records[4];
    const char *values[] = {"A100", "B200", "C300", "D400"};
    t_runtime_record target;
    t_runtime_record_key key;
    size_t index;
    size_t found_index;
    int found;

    index = 0;
    while (index < 4)
    {
        if (runtime_sort_init_record(&records[index], 4, values[index]) != FT_SUCCESS)
        {
            runtime_sort_dispose_records(records, index);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (runtime_sort_init_record(&target, 4, "C300") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 4);
        return (FT_FAILURE);
    }
    key.offset = 0;
    key.length = 4;
    key.ascending = 1;
    if (test_expect_success(runtime_record_search_all(records, 4, &target, &key, 1, &found_index, &found),
            "runtime_record_search_all should succeed for sorted data") != FT_SUCCESS)
    {
        runtime_record_dispose(&target);
        runtime_sort_dispose_records(records, 4);
        return (FT_FAILURE);
    }
    runtime_record_dispose(&target);
    if (test_expect_int_equal(found, 1, "runtime_record_search_all should report matching record") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 4);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(found_index, 2, "runtime_record_search_all should locate matching index")
        != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 4);
        return (FT_FAILURE);
    }
    runtime_sort_dispose_records(records, 4);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_search_all_reports_insertion_point)
{
    t_runtime_record records[3];
    const char *values[] = {"A100", "B200", "D400"};
    t_runtime_record target;
    t_runtime_record_key key;
    size_t index;
    size_t found_index;
    int found;

    index = 0;
    while (index < 3)
    {
        if (runtime_sort_init_record(&records[index], 4, values[index]) != FT_SUCCESS)
        {
            runtime_sort_dispose_records(records, index);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (runtime_sort_init_record(&target, 4, "C350") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 3);
        return (FT_FAILURE);
    }
    key.offset = 0;
    key.length = 4;
    key.ascending = 1;
    if (test_expect_success(runtime_record_search_all(records, 3, &target, &key, 1, &found_index, &found),
            "runtime_record_search_all should succeed when inserting data") != FT_SUCCESS)
    {
        runtime_record_dispose(&target);
        runtime_sort_dispose_records(records, 3);
        return (FT_FAILURE);
    }
    runtime_record_dispose(&target);
    if (test_expect_int_equal(found, 0, "runtime_record_search_all should indicate missing record") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 3);
        return (FT_FAILURE);
    }
    if (test_expect_size_t_equal(found_index, 2,
            "runtime_record_search_all should propose insertion point") != FT_SUCCESS)
    {
        runtime_sort_dispose_records(records, 3);
        return (FT_FAILURE);
    }
    runtime_sort_dispose_records(records, 3);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_sort_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_record_sort_orders_single_key", test_runtime_record_sort_orders_single_key},
        {"runtime_record_sort_multiple_keys_with_descending", test_runtime_record_sort_multiple_keys_with_descending},
        {"runtime_record_search_all_finds_sorted_record", test_runtime_record_search_all_finds_sorted_record},
        {"runtime_record_search_all_reports_insertion_point", test_runtime_record_search_all_reports_insertion_point}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
