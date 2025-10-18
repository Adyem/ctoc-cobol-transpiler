#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

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
