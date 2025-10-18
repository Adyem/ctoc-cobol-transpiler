#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

FT_TEST(test_runtime_encoding_transcodes_default_ascii)
{
    unsigned char ascii_data[3];
    unsigned char ebcdic_data[3];
    size_t written;

    if (test_expect_success(runtime_encoding_reset(),
            "runtime_encoding_reset should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    ascii_data[0] = 'A';
    ascii_data[1] = 'B';
    ascii_data[2] = 'C';
    if (test_expect_success(runtime_encoding_transcode_to_ebcdic(ascii_data, 3, ebcdic_data, 3, &written),
            "runtime_encoding_transcode_to_ebcdic should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(written, 3,
            "runtime_encoding_transcode_to_ebcdic should report written length") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(ebcdic_data[0]), 193,
            "runtime_encoding_transcode_to_ebcdic should map 'A' to CCSID 037") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(ebcdic_data[1]), 194,
            "runtime_encoding_transcode_to_ebcdic should map 'B' to CCSID 037") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(ebcdic_data[2]), 195,
            "runtime_encoding_transcode_to_ebcdic should map 'C' to CCSID 037") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_encoding_transcode_to_ascii(ebcdic_data, 3, ascii_data, 3, &written),
            "runtime_encoding_transcode_to_ascii should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(written, 3,
            "runtime_encoding_transcode_to_ascii should report written length") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(ascii_data[0]), 'A',
            "runtime_encoding_transcode_to_ascii should restore 'A'") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(ascii_data[1]), 'B',
            "runtime_encoding_transcode_to_ascii should restore 'B'") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(ascii_data[2]), 'C',
            "runtime_encoding_transcode_to_ascii should restore 'C'") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_encoding_set_active_overrides_tables)
{
    t_runtime_encoding_table table;
    unsigned char input[2];
    unsigned char output[2];
    size_t index;
    size_t written;

    index = 0;
    while (index < RUNTIME_ENCODING_TABLE_SIZE)
    {
        table.to_ascii[index] = static_cast<unsigned char>(index);
        table.from_ascii[index] = static_cast<unsigned char>(index);
        index += 1;
    }
    if (test_expect_success(runtime_encoding_set_active(&table),
            "runtime_encoding_set_active should copy custom tables") != FT_SUCCESS)
        return (FT_FAILURE);
    input[0] = 1;
    input[1] = 2;
    if (test_expect_success(runtime_encoding_transcode_to_ascii(input, 2, output, 2, &written),
            "runtime_encoding_transcode_to_ascii should use custom table") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(output[0]), 1,
            "runtime_encoding_transcode_to_ascii should apply identity mapping") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(output[1]), 2,
            "runtime_encoding_transcode_to_ascii should apply identity mapping") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_encoding_transcode_to_ebcdic(output, 2, input, 2, &written),
            "runtime_encoding_transcode_to_ebcdic should use custom table") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(input[0]), 1,
            "runtime_encoding_transcode_to_ebcdic should apply identity mapping") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(input[1]), 2,
            "runtime_encoding_transcode_to_ebcdic should apply identity mapping") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_encoding_reset(),
            "runtime_encoding_reset should restore defaults") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_encoding_reset_restores_ccsid_037)
{
    t_runtime_encoding_table table;

    if (test_expect_success(runtime_encoding_reset(),
            "runtime_encoding_reset should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_encoding_get_active(&table),
            "runtime_encoding_get_active should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(table.from_ascii['A']), 193,
            "runtime_encoding_reset should restore CCSID 037 mapping for 'A'") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_runtime_encoding_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"runtime_encoding_transcodes_default_ascii", test_runtime_encoding_transcodes_default_ascii},
        {"runtime_encoding_set_active_overrides_tables", test_runtime_encoding_set_active_overrides_tables},
        {"runtime_encoding_reset_restores_ccsid_037", test_runtime_encoding_reset_restores_ccsid_037}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
