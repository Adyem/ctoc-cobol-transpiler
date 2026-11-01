#include "cblc_transpiler.hpp"

#include "test_suites.hpp"
#include "../compatibility/memory_compat.hpp"

#include <cstring>
#include <limits>

FT_TEST(test_compiler_allocator_rejects_size_multiplication_overflow)
{
    void *allocation;

    allocation = cma_calloc(std::numeric_limits<size_t>::max(), 2);
    if (allocation != NULL)
    {
        cma_free(allocation);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_allocator_injects_exactly_the_selected_failure)
{
    unsigned char *first;
    void *failed;
    unsigned char *after_failure;
    size_t index;
    int status;

    first = NULL;
    failed = NULL;
    after_failure = NULL;
    status = FT_FAILURE;
    cma_allocation_test_fail_on_attempt(2);
    first = static_cast<unsigned char *>(cma_calloc(8, sizeof(*first)));
    if (!first)
        goto cleanup;
    index = 0;
    while (index < 8)
    {
        if (first[index] != 0)
            goto cleanup;
        index += 1;
    }
    failed = cma_calloc(1, sizeof(int));
    if (failed)
        goto cleanup;
    after_failure = static_cast<unsigned char *>(cma_calloc(4,
        sizeof(*after_failure)));
    if (!after_failure)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cma_allocation_test_disable();
    cma_free(first);
    cma_free(failed);
    cma_free(after_failure);
    return (status);
}

FT_TEST(test_cblc_parser_reports_first_required_allocation_failure)
{
    const char *source;
    t_cblc_translation_unit unit;
    int parse_status;

    source = "void main() { int value; value = 1; return; }\n";
    cblc_translation_unit_init(&unit);
    cma_allocation_test_fail_on_attempt(1);
    parse_status = cblc_parse_translation_unit(source, &unit);
    cma_allocation_test_disable();
    if (parse_status != FT_FAILURE
        || unit.parse_error_code != TRANSPILE_ERROR_ALLOCATION_FAILURE
        || std::strcmp(unit.parse_error_message,
            "allocation failed while parsing CBL-C translation unit") != 0)
    {
        std::printf("Injected parser allocation status=%d diagnostic='%s'\n",
            parse_status, unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_memory_copy_checked_detects_overflow)
{
    char destination[4];
    char source[5];
    size_t index;

    index = 0;
    while (index < 5)
    {
        source[index] = static_cast<char>('A' + static_cast<int>(index));
        index += 1;
    }
    if (runtime_memory_copy_checked(destination, 4, source, 5) != FT_FAILURE)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_memory_copy_checked_copies_without_overlap)
{
    char destination[4];
    char source[4];
    size_t index;

    index = 0;
    while (index < 4)
    {
        source[index] = static_cast<char>('a' + static_cast<int>(index));
        destination[index] = '?';
        index += 1;
    }
    if (test_expect_success(runtime_memory_copy_checked(destination, 4, source, 4),
            "runtime_memory_copy_checked should succeed for non-overlapping regions") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < 4)
    {
        if (test_expect_char_equal(destination[index], source[index],
                "runtime_memory_copy_checked should copy requested bytes") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_memory_copy_checked_handles_overlap)
{
    char buffer[8];
    size_t index;
    const char expected[] = {'A', 'B', 'A', 'B', 'C', 'D', 'E', 'F'};

    index = 0;
    while (index < 8)
    {
        buffer[index] = static_cast<char>('A' + static_cast<int>(index));
        index += 1;
    }
    if (test_expect_success(runtime_memory_copy_checked(buffer + 2, 6, buffer, 6),
            "runtime_memory_copy_checked should use memmove semantics for overlap") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < 8)
    {
        if (test_expect_char_equal(buffer[index], expected[index],
                "runtime_memory_copy_checked should preserve bytes with overlap") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}
