#include "runtime_record.hpp"

#include "test_suites.hpp"

FT_TEST(test_runtime_record_init_sets_defaults)
{
    t_runtime_record record;

    if (test_expect_success(runtime_record_init(&record, 0), "runtime_record_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(record.length), 0, "runtime_record_init should set length to zero") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (record.data == NULL)
    {
        runtime_record_dispose(&record);
        pf_printf("Assertion failed: runtime_record_init should allocate data\n");
        return (FT_FAILURE);
    }
    if (record.capacity < 1)
    {
        runtime_record_dispose(&record);
        pf_printf("Assertion failed: runtime_record_init should provide capacity\n");
        return (FT_FAILURE);
    }
    runtime_record_dispose(&record);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_set_length_preserves_data)
{
    t_runtime_record record;

    if (test_expect_success(runtime_record_init(&record, 0), "runtime_record_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_record_set_length(&record, 4), "runtime_record_set_length should expand") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_record_copy_from_buffer(&record, "ABCD", 4),
            "runtime_record_copy_from_buffer should accept data matching record length") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_record_set_length(&record, 8), "runtime_record_set_length should grow further") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(record.data, "ABCD    ",
            "runtime_record_set_length should preserve prefix data and pad with spaces") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    runtime_record_dispose(&record);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_copy_round_trip)
{
    t_runtime_record record;
    char buffer[32];
    size_t written;

    if (test_expect_success(runtime_record_init(&record, 0), "runtime_record_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_record_set_length(&record, 12), "runtime_record_set_length should set record size") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_record_copy_from_buffer(&record, "HELLO", 5),
            "runtime_record_copy_from_buffer should copy input") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(record.data, "HELLO       ",
            "runtime_record_copy_from_buffer should pad remaining space") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    written = 0;
    if (test_expect_success(runtime_record_copy_to_buffer(&record, buffer, sizeof(buffer), &written),
            "runtime_record_copy_to_buffer should succeed") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(written), 12, "runtime_record_copy_to_buffer should report record length") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(buffer, "HELLO       ",
            "runtime_record_copy_to_buffer should write padded text") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    runtime_record_dispose(&record);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_fill_updates_contents)
{
    t_runtime_record record;

    if (test_expect_success(runtime_record_init(&record, 0), "runtime_record_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_record_set_length(&record, 6), "runtime_record_set_length should set record size") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_record_fill(&record, 'X'), "runtime_record_fill should succeed") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(record.data, "XXXXXX", "runtime_record_fill should populate fill value") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    runtime_record_dispose(&record);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_record_copy_rejects_overflow)
{
    t_runtime_record record;

    if (test_expect_success(runtime_record_init(&record, 0), "runtime_record_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_record_set_length(&record, 4), "runtime_record_set_length should set record size") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    if (runtime_record_copy_from_buffer(&record, "TOO-LONG", 8) != FT_FAILURE)
    {
        runtime_record_dispose(&record);
        pf_printf("Assertion failed: runtime_record_copy_from_buffer should reject oversized input\n");
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(record.data, "    ", "runtime_record_copy_from_buffer should leave existing padding on failure") != FT_SUCCESS)
    {
        runtime_record_dispose(&record);
        return (FT_FAILURE);
    }
    runtime_record_dispose(&record);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_record_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_record_init_sets_defaults", test_runtime_record_init_sets_defaults},
        {"runtime_record_set_length_preserves_data", test_runtime_record_set_length_preserves_data},
        {"runtime_record_copy_round_trip", test_runtime_record_copy_round_trip},
        {"runtime_record_fill_updates_contents", test_runtime_record_fill_updates_contents},
        {"runtime_record_copy_rejects_overflow", test_runtime_record_copy_rejects_overflow}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
