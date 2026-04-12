#include "test_suites.hpp"

#include "compatibility/memory_compat.hpp"

FT_TEST(test_transpiler_validation_accepts_valid_cblc)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    display(\"HELLO\");\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept valid CBL-C") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_cblc_without_return)
{
    const char *source;

    source = "int main()\n"
        "{\n"
        "    display(\"HELLO\");\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject CBL-C without return\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_declaration)
{
    const char *source;

    source = "string greeting(16);\n"
        "void main()\n"
        "{\n"
        "    greeting = \"HI\";\n"
        "    display(greeting.len);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string declarations") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_assignment_and_length_usage)
{
    const char *source;

    source = "string greeting(12);\n"
        "string copy(12);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    copy = greeting;\n"
        "    total = greeting.len + copy.len;\n"
        "    display(copy);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string assignments and length usage")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_int_array_and_string_array_declarations)
{
    const char *source;

    source = "int numbers[16];\n"
        "string names[4](12);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept int arrays and string arrays with explicit capacity")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_capacity_parentheses)
{
    const char *source;

    source = "string greeting(16);\n"
        "void main()\n"
        "{\n"
        "    greeting = \"HI\";\n"
        "    display(greeting.len);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept string capacity in parentheses") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_legacy_string_bracket_capacity)
{
    const char *source;

    source = "string greeting[16];\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject legacy string bracket capacity syntax\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_indexed_array_usage)
{
    const char *source;

    source = "int numbers[3];\n"
        "int total;\n"
        "string names[2](8);\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0];\n"
        "    names[1] = \"HI\";\n"
        "    display(numbers[0]);\n"
        "    display(names[1]);\n"
        "    display(names[1].len);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept indexed int and string array usage") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_declarations_and_allocation_calls)
{
    const char *source;

    source = "void *raw;\n"
        "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(64);\n"
        "    buffer = std::malloc(16);\n"
        "    numbers = std::malloc(32);\n"
        "    numbers = std::realloc(numbers, 64);\n"
        "    buffer = NULL;\n"
        "    std::free(numbers);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept pointer declarations and std allocation calls")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_parse_translation_unit_captures_pointer_kinds)
{
    const char *source;
    t_cblc_translation_unit unit;

    source = "void *raw;\n"
        "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept pointer declarations\n");
        return (FT_FAILURE);
    }
    if (unit.data_count < 3)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should register pointer declarations\n");
        return (FT_FAILURE);
    }
    if (unit.data_items[0].kind != CBLC_DATA_KIND_VOID_POINTER
        || unit.data_items[1].kind != CBLC_DATA_KIND_CHAR_POINTER
        || unit.data_items[2].kind != CBLC_DATA_KIND_INT_POINTER)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should preserve pointer kinds\n");
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_allocations_and_free)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "void *raw;\n"
        "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(64);\n"
        "    buffer = std::malloc(16);\n"
        "    numbers = std::malloc(32);\n"
        "    numbers = std::realloc(numbers, 64);\n"
        "    buffer = NULL;\n"
        "    std::free(numbers);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept pointer allocation source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate pointer allocation source") != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "#include <stdlib.h>", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should include stdlib for malloc and free\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "static void *raw = NULL;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should declare void pointer globals\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "static char *buffer = NULL;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should declare char pointer globals\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "static int *numbers = NULL;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should declare int pointer globals\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "raw = (void *)malloc(64);", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit malloc for void pointers\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "buffer = (char *)malloc(16);", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit malloc for char pointers\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers = (int *)realloc(numbers, 64);",
            std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit realloc for int pointers\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "buffer = NULL;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should preserve NULL assignments\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "free(numbers);", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "free(raw);", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit free calls\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_cobol_emits_pointer_allocation_and_free)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(16);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    generated_cobol = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept the first pointer slice\n");
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: COBOL generation should accept basic pointer allocation\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_cobol, "01 RAW USAGE POINTER VALUE NULL.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "ALLOCATE 16 CHARACTERS RETURNING RAW.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "FREE RAW.",
            std::strlen(generated_cobol)))
    {
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated COBOL should lower malloc/free pointer operations\n");
        return (FT_FAILURE);
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_indexed_reads_and_writes)
{
    const char *source;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    numbers = std::malloc(16);\n"
        "    buffer[0] = 'A';\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0] + 3;\n"
        "    display(numbers[0]);\n"
        "    std::free(buffer);\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept indexed reads and writes on typed pointers")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_indexed_reads_and_writes)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    numbers = std::malloc(16);\n"
        "    buffer[0] = 'A';\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0] + 3;\n"
        "    display(numbers[0]);\n"
        "    std::free(buffer);\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept pointer indexed source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate pointer indexed reads and writes")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "buffer[0] = (char)65;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit indexed char pointer stores\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers[0] = 7;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit indexed int pointer stores\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "total = numbers[0] + 3;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit indexed int pointer reads\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "cblc_display_int(numbers[0]);", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should display indexed int pointer values\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_void_pointer_indexing)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(8);\n"
        "    raw[0] = 1;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject void pointer indexing\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_void_pointer_from_typed_pointer)
{
    const char *source;

    source = "void *raw;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    raw = buffer;\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept assigning typed pointers to void pointers")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_typed_pointer_from_void_pointer)
{
    const char *source;

    source = "void *raw;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(8);\n"
        "    buffer = raw;\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept assigning void pointers to typed pointers")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_char_pointer_from_int_pointer)
{
    const char *source;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    buffer = numbers;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject assigning int pointers to char pointers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_int_pointer_from_char_pointer)
{
    const char *source;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    numbers = buffer;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject assigning char pointers to int pointers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_free_of_non_pointer)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    std::free(total);\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject freeing non-pointer values\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_variable_indexed_int_pointer_usage)
{
    const char *source;

    source = "int *numbers;\n"
        "int index;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    index = 1;\n"
        "    numbers[index] = 9;\n"
        "    total = numbers[index];\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept variable indexed int pointer access")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_variable_indexed_int_pointer_usage)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int *numbers;\n"
        "int index;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    index = 1;\n"
        "    numbers[index] = 9;\n"
        "    total = numbers[index];\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept variable indexed int pointer source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate variable indexed int pointer usage")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers[index] = 9;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit variable indexed int pointer stores\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "total = numbers[index];", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit variable indexed int pointer reads\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_variable_indexed_char_pointer_store)
{
    const char *source;

    source = "char *buffer;\n"
        "int index;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    index = 1;\n"
        "    buffer[index] = 'B';\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept variable indexed char pointer stores")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_variable_indexed_char_pointer_store)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "char *buffer;\n"
        "int index;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    index = 1;\n"
        "    buffer[index] = 'B';\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept variable indexed char pointer source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate variable indexed char pointer stores")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "buffer[index] = (char)66;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit variable indexed char pointer stores\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_arithmetic_assignment)
{
    const char *source;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "int offset;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    numbers = std::malloc(16);\n"
        "    offset = 2;\n"
        "    buffer = buffer + 1;\n"
        "    numbers = numbers + offset;\n"
        "    numbers = numbers - 1;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept typed pointer arithmetic assignments")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_arithmetic_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "int offset;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    numbers = std::malloc(16);\n"
        "    offset = 2;\n"
        "    buffer = buffer + 1;\n"
        "    numbers = numbers + offset;\n"
        "    numbers = numbers - 1;\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept pointer arithmetic source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate pointer arithmetic assignments")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "buffer = buffer + 1;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "numbers = numbers + offset;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "numbers = numbers - 1;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit pointer arithmetic assignments\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_void_pointer_arithmetic_assignment)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(8);\n"
        "    raw = raw + 1;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject void pointer arithmetic for now\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_local_pointer_declarations_and_usage)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    char *buffer;\n"
        "    int *numbers;\n"
        "    raw = NULL;\n"
        "    buffer = std::malloc(4);\n"
        "    numbers = std::malloc(16);\n"
        "    raw = buffer;\n"
        "    numbers[1] = 8;\n"
        "    total = numbers[1];\n"
        "    std::free(buffer);\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept local pointer declarations and usage")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_local_pointer_declarations_and_usage)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    char *buffer;\n"
        "    int *numbers;\n"
        "    raw = NULL;\n"
        "    buffer = std::malloc(4);\n"
        "    numbers = std::malloc(16);\n"
        "    raw = buffer;\n"
        "    numbers[1] = 8;\n"
        "    total = numbers[1];\n"
        "    std::free(buffer);\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept local pointer declaration source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate local pointer declarations and usage")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "    void *main__raw = NULL;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "    char *main__buffer = NULL;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "    int *main__numbers = NULL;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit local pointer declarations\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "main__numbers[1] = 8;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "total = main__numbers[1];", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit local pointer indexed usage\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_variable_size_realloc_for_int_pointer)
{
    const char *source;

    source = "int *numbers;\n"
        "int size;\n"
        "void main()\n"
        "{\n"
        "    size = 32;\n"
        "    numbers = std::malloc(16);\n"
        "    numbers = std::realloc(numbers, size);\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept variable size realloc for int pointers")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_variable_size_realloc_for_int_pointer)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int *numbers;\n"
        "int size;\n"
        "void main()\n"
        "{\n"
        "    size = 32;\n"
        "    numbers = std::malloc(16);\n"
        "    numbers = std::realloc(numbers, size);\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept variable size realloc source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate variable size realloc for int pointers")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers = (int *)realloc(numbers, size);",
            std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit variable size realloc\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_indexed_arithmetic_with_variable_index)
{
    const char *source;

    source = "int *numbers;\n"
        "int index;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    index = 1;\n"
        "    numbers[index] = 9;\n"
        "    total = numbers[index] * 2;\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept indexed pointer arithmetic with variable index")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_indexed_arithmetic_with_variable_index)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int *numbers;\n"
        "int index;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    index = 1;\n"
        "    numbers[index] = 9;\n"
        "    total = numbers[index] * 2;\n"
        "    std::free(numbers);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept indexed arithmetic source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate indexed pointer arithmetic")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "total = numbers[index] * 2;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit indexed pointer arithmetic\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_non_int_pointer_index_variable)
{
    const char *source;

    source = "int *numbers;\n"
        "char label[2];\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    numbers[label] = 7;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject non-int pointer index variables\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_char_pointer_element_from_int_pointer_element)
{
    const char *source;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    numbers = std::malloc(16);\n"
        "    buffer[0] = numbers[0];\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject assigning int pointer elements to char pointer elements\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_int_pointer_element_from_char_pointer_element)
{
    const char *source;

    source = "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    numbers = std::malloc(16);\n"
        "    numbers[0] = buffer[0];\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject assigning char pointer elements to int pointer elements\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_numeric_literal_assignment_to_pointer)
{
    const char *source;

    source = "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = 7;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject numeric literal assignment to pointers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_address_of_scalar_assignments)
{
    const char *source;

    source = "int value;\n"
        "char marker;\n"
        "int *numbers;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    buffer = &marker;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept address-of scalar pointer assignments")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_address_of_scalar_assignments)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int value;\n"
        "char marker;\n"
        "int *numbers;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    buffer = &marker;\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept address-of scalar source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate address-of scalar pointer assignments")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers = &value;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "buffer = &marker;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit address-of scalar pointer assignments\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_address_of_array_elements)
{
    const char *source;

    source = "int values[2];\n"
        "int index;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    numbers = &values[0];\n"
        "    numbers = &values[index];\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept address-of array element pointer assignments")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_address_of_array_elements)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int values[2];\n"
        "int index;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    numbers = &values[0];\n"
        "    numbers = &values[index];\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept address-of array element source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate address-of array element pointer assignments")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers = &values[0];", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "numbers = &values[index];", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit address-of array elements\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_address_of_struct_field)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = &point.x;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept address-of struct field pointer assignments")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_address_of_struct_field)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = &point.x;\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept address-of struct field source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate address-of struct field pointer assignments")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "numbers = &point.x;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit address-of struct fields\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_int_pointer_dereference_reads)
{
    const char *source;

    source = "int value;\n"
        "int total;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    total = *numbers;\n"
        "    display(*numbers);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept int pointer dereference reads")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_int_pointer_dereference_reads)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int value;\n"
        "int total;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    total = *numbers;\n"
        "    display(*numbers);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept int pointer dereference source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate int pointer dereference reads")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "total = *numbers;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_display_int(*numbers);", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit int pointer dereference reads\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_char_pointer_dereference_reads)
{
    const char *source;

    source = "char marker;\n"
        "char other;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    buffer = &marker;\n"
        "    other = *buffer;\n"
        "    display(*buffer);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept char pointer dereference reads")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_char_pointer_dereference_reads)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "char marker;\n"
        "char other;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    buffer = &marker;\n"
        "    other = *buffer;\n"
        "    display(*buffer);\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept char pointer dereference source\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate char pointer dereference reads")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "other = *buffer;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_display_char_buffer(&*buffer, 1);",
            std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit char pointer dereference reads\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_pointer_address_of_arrays)
{
    const char *source;

    source = "int values[2];\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = &values;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject address-of array assignments for now\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_dereference_writes)
{
    const char *source;

    source = "int value;\n"
        "char marker;\n"
        "int *numbers;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    buffer = &marker;\n"
        "    *numbers = 7;\n"
        "    *buffer = 'A';\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept dereference writes on typed pointers")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_dereference_writes)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;

    source = "int value;\n"
        "char marker;\n"
        "int *numbers;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    buffer = &marker;\n"
        "    *numbers = 7;\n"
        "    *buffer = 'A';\n"
        "    return;\n"
        "}\n";
    generated_c = NULL;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: parser should accept pointer dereference writes\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate pointer dereference writes")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(generated_c, "*numbers = 7;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "*buffer = (char)65;", std::strlen(generated_c)))
    {
        cma_free(generated_c);
        cblc_translation_unit_dispose(&unit);
        std::printf("Assertion failed: generated C should emit pointer dereference writes\n");
        return (FT_FAILURE);
    }
    cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_void_pointer_dereference_writes)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(8);\n"
        "    *raw = 1;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject dereference writes on void pointers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_pointer_dereference_write_type_mismatch)
{
    const char *source;

    source = "char marker;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    numbers = std::malloc(16);\n"
        "    *numbers = marker;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched dereference writes\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_pointer_parameters_and_returns)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "int value;\n"
        "int *numbers;\n"
        "int *identity(int *input)\n"
        "{\n"
        "    return input;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    numbers = identity(numbers);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parser should accept pointer parameters and return types") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.function_count, 2,
            "parser should record identity and main") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.functions[0].return_kind,
            CBLC_FUNCTION_RETURN_INT_POINTER,
            "identity should return an int pointer") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.functions[0].parameter_count, 1,
            "identity should record one parameter") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.functions[0].parameters[0].kind,
            TRANSPILE_FUNCTION_PARAMETER_INT_POINTER,
            "identity parameter should be an int pointer") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_pointer_parameter_and_return_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int value;\n"
        "int *numbers;\n"
        "int *identity(int *input)\n"
        "{\n"
        "    return input;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    numbers = &value;\n"
        "    numbers = identity(numbers);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parser should accept pointer function call source") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate pointer function call source") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "int * identity(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit int pointer return type\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "identity__input = numbers;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should move int pointer arguments\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "numbers = identity();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign int pointer return values\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_void_pointer_parameter_and_return)
{
    const char *source;

    source = "void *raw;\n"
        "void *same(void *input)\n"
        "{\n"
        "    return input;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(8);\n"
        "    raw = same(raw);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept void pointer parameters and returns") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_address_of_pointer_return)
{
    const char *source;

    source = "int value;\n"
        "int *numbers;\n"
        "int *value_pointer()\n"
        "{\n"
        "    return &value;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    numbers = value_pointer();\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept returning address-of expressions") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_pointer_return_type_mismatch)
{
    const char *source;

    source = "char *buffer;\n"
        "int *wrong_pointer()\n"
        "{\n"
        "    return buffer;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched pointer return types\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_explicit_pointer_cast_assignments)
{
    const char *source;

    source = "void *raw;\n"
        "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    raw = (void *)buffer;\n"
        "    buffer = (char *)raw;\n"
        "    numbers = (int *)raw;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept explicit pointer cast assignments") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_explicit_pointer_cast_assignments)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "void *raw;\n"
        "char *buffer;\n"
        "int *numbers;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    raw = (void *)buffer;\n"
        "    buffer = (char *)raw;\n"
        "    numbers = (int *)raw;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parser should accept explicit pointer cast assignments") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate explicit pointer cast assignments") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "raw = (void *)buffer;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit void pointer casts\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "buffer = (char *)raw;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit char pointer casts\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "numbers = (int *)raw;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit int pointer casts\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_pointer_cast_call_arguments)
{
    const char *source;

    source = "void *raw;\n"
        "char *buffer;\n"
        "void *same(void *input)\n"
        "{\n"
        "    return input;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    raw = same((void *)buffer);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept pointer casts in call arguments") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_cast_return_expression)
{
    const char *source;

    source = "void *raw;\n"
        "char *buffer;\n"
        "void *as_raw()\n"
        "{\n"
        "    return (void *)buffer;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(8);\n"
        "    raw = as_raw();\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept pointer casts in return expressions") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_pointer_cast_from_non_pointer)
{
    const char *source;

    source = "void *raw;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    raw = (void *)total;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject casts from non-pointer values\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_struct_pointer_field_access)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point *current;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    current = &point;\n"
        "    current->x = 9;\n"
        "    total = current->x;\n"
        "    display(current->x);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept struct pointer field access") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_struct_pointer_field_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point *current;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    current = &point;\n"
        "    current->x = 9;\n"
        "    total = current->x;\n"
        "    display(current->x);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parser should accept struct pointer field access") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate struct pointer field access") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "static t_Point *current = NULL;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare struct pointers\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "current = &point;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign struct pointer addresses\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "current->x = 9;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should write through struct pointers\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = current->x;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should read through struct pointers\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_local_struct_pointer_field_access)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    Point *current;\n"
        "    current = &point;\n"
        "    current->x = 11;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept local struct pointer field access") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_class_pointer_field_access)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "};\n"
        "Counter counter;\n"
        "Counter *current;\n"
        "void main()\n"
        "{\n"
        "    current = &counter;\n"
        "    current->value = 5;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept class pointer field access") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_arrow_on_non_pointer_struct)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point->x = 9;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject arrow on non-pointer structs\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_struct_pointer_return_function)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point *current;\n"
        "Point *get_point()\n"
        "{\n"
        "    return &point;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    current = get_point();\n"
        "    current->x = 13;\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept struct pointer return functions") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_struct_pointer_return_function)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point *current;\n"
        "Point *get_point()\n"
        "{\n"
        "    return &point;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    current = get_point();\n"
        "    current->x = 13;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parser should accept struct pointer return functions") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate struct pointer return functions") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "t_Point * get_point(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit struct pointer return type\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "return (&point);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should return struct addresses\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "current = get_point();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign struct pointer returns\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_rejects_struct_pointer_type_mismatch)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "struct Other\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Other *other;\n"
        "void main()\n"
        "{\n"
        "    other = &point;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched struct pointer types\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_pointer_receiver_method_call)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    void reset()\n"
        "    {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "Counter *current_counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    current_counter = &counter;\n"
        "    current_counter->value = 7;\n"
        "    total = current_counter->current();\n"
        "    current_counter->reset();\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept pointer receiver method calls") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_generate_c_emits_pointer_receiver_method_call)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    void reset()\n"
        "    {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "Counter *current_counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    current_counter = &counter;\n"
        "    current_counter->value = 7;\n"
        "    total = current_counter->current();\n"
        "    current_counter->reset();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parser should accept pointer receiver method calls") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate pointer receiver method calls") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "total = current_counter->value;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should read through pointer receiver methods\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "current_counter->value = 0;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should write through pointer receiver methods\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_const_declarations)
{
    const char *source;

    source = "const int answer = 42;\n"
        "const char marker = 'A';\n"
        "const string greeting(12) = \"HELLO\";\n"
        "void main()\n"
        "{\n"
        "    display(answer);\n"
        "    display(marker);\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept const declarations") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_without_initializer)
{
    const char *source;

    source = "const int answer;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject const declarations without initializer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_reassignment)
{
    const char *source;

    source = "const int answer = 42;\n"
        "void main()\n"
        "{\n"
        "    answer = 7;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject reassignment to const declarations\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_const_char_buffer_declaration)
{
    const char *source;

    source = "const char label[8] = \"HELLO\";\n"
        "void main()\n"
        "{\n"
        "    display(label);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept const char buffer declarations")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_const_string_length_usage)
{
    const char *source;

    source = "const string greeting(12) = \"HELLO\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = greeting.len;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept reading const string length")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_char_scalar_reassignment)
{
    const char *source;

    source = "const char marker = 'A';\n"
        "void main()\n"
        "{\n"
        "    marker = 'B';\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject reassignment to const char declarations\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_const_string_reassignment)
{
    const char *source;

    source = "const string greeting(12) = \"HELLO\";\n"
        "void main()\n"
        "{\n"
        "    greeting = \"BYE\";\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject reassignment to const string declarations\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_strcpy_into_const_char_buffer)
{
    const char *source;

    source = "const char label[8] = \"HELLO\";\n"
        "void main()\n"
        "{\n"
        "    std::strcpy(label, \"BYE\");\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject strcpy into const char buffers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_strlen_string_literal_assignment)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"hello world\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    if (test_expect_success(transpiler_validate_generated_cblc(source),
            "validator should accept std::strlen with string literals")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_parse_translation_unit_records_imports)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "import \"helper.cblc\";\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "import statements should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.import_count != 1)
    {
        std::printf("Assertion failed: expected exactly one import\n");
        goto cleanup;
    }
    if (std::strncmp(unit.imports[0].path, "helper.cblc", TRANSPILE_FILE_PATH_MAX) != 0)
    {
        std::printf("Assertion failed: import path should be recorded\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_copy_includes)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "copy \"shared-status\";\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy directives should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.copy_include_count != 1)
    {
        std::printf("Assertion failed: expected exactly one copy include\\n");
        goto cleanup;
    }
    if (std::strncmp(unit.copy_includes[0].name, "shared-status",
            TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: copy include should record source name\\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_tracks_multiple_functions)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    display(\"SECOND\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "multiple functions should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.function_count != 2)
    {
        std::printf("Assertion failed: expected two functions in translation unit\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[0].source_name, "helper", TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: first function should be named 'helper'\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[1].source_name, "main", TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: second function should be named 'main'\n");
        goto cleanup;
    }
    if (unit.functions[0].statement_count != 2)
    {
        std::printf("Assertion failed: helper function should retain its statements\n");
        goto cleanup;
    }
    if (unit.functions[1].statement_count != 2)
    {
        std::printf("Assertion failed: main function should retain its statements\n");
        goto cleanup;
    }
    if (unit.entry_function_index != 1)
    {
        std::printf("Assertion failed: entry function should resolve to 'main'\n");
        goto cleanup;
    }
    if (std::strncmp(unit.program_name, "MAIN", TRANSPILE_IDENTIFIER_MAX) != 0)
    {
        std::printf("Assertion failed: program name should match entry function\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_functions_without_keyword)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "functions without the function keyword should parse") != FT_SUCCESS)
        goto cleanup;
    if (unit.function_count != 2)
    {
        std::printf("Assertion failed: expected two parsed functions without the function keyword\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[0].source_name, "helper",
            sizeof(unit.functions[0].source_name)) != 0)
    {
        std::printf("Assertion failed: helper should be recorded without the function keyword\n");
        goto cleanup;
    }
    if (std::strncmp(unit.functions[1].source_name, "main",
            sizeof(unit.functions[1].source_name)) != 0)
    {
        std::printf("Assertion failed: main should be recorded without the function keyword\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_string_group)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "string greeting(8);\n"
        "void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string declarations should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string declarations should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 GREETING.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 GREETING-LEN PIC 9(4) COMP VALUE 8.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 GREETING-BUF PIC X(8).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare GREETING buffer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY GREETING-BUF(1:GREETING-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display buffer slice using length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_array_declarations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int numbers[16];\n"
        "string names[4](12);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "array declaration sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "array declaration sample should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "static int numbers[16] = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit int array storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "static struct { size_t len; char buf[12]; } names[4] = {{0}};",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit string array storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_array_declarations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int numbers[16];\n"
        "string names[4](12);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "array declaration COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "array declaration COBOL sample should convert") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 NUMBERS PIC S9(9) OCCURS 16 TIMES.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit int array OCCURS storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "01 NAMES OCCURS 4 TIMES.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit string array group OCCURS storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 NAMES-BUF PIC X(12).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should keep string array element capacity\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_indexed_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "string names[2](8);\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0];\n"
        "    names[1] = \"HI\";\n"
        "    display(numbers[0]);\n"
        "    display(names[1]);\n"
        "    display(names[1].len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "indexed array sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "indexed array sample should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "numbers[0] = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign to int array element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = numbers[0];", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should read from int array element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(names[1].buf, 8, &names[1].len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign string array literal to indexed element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(names[1].buf, names[1].len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display indexed string element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_size(names[1].len);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display indexed string length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_indexed_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "string names[2](8);\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 7;\n"
        "    total = numbers[0];\n"
        "    names[1] = \"HI\";\n"
        "    display(numbers[0]);\n"
        "    display(names[1]);\n"
        "    display(names[1].len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "indexed array COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "indexed array COBOL sample should convert") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE NUMBERS(1) = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign to int array element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = NUMBERS(1).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should read from int array element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO NAMES-BUF(2).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign indexed string buffer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY NAMES-BUF(2)(1:NAMES-LEN(2)).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display indexed string element\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY NAMES-LEN(2).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display indexed string length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_variable_index_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "int index;\n"
        "string names[2](8);\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    total = numbers[index];\n"
        "    names[index] = \"HI\";\n"
        "    display(numbers[index]);\n"
        "    display(names[index]);\n"
        "    display(names[index].len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "variable index sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "variable index sample should convert to C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "total = numbers[index];", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should read int array with variable index\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(names[index].buf, 8, &names[index].len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign indexed string array with variable index\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(names[index].buf, names[index].len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display indexed string array with variable index\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_size(names[index].len);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display indexed string length with variable index\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_variable_index_array_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int numbers[3];\n"
        "int total;\n"
        "int index;\n"
        "string names[2](8);\n"
        "void main()\n"
        "{\n"
        "    index = 1;\n"
        "    total = numbers[index];\n"
        "    names[index] = \"HI\";\n"
        "    display(numbers[index]);\n"
        "    display(names[index]);\n"
        "    display(names[index].len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "variable index COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "variable index COBOL sample should convert") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = NUMBERS(INDEX + 1).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should read int array with variable index\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO NAMES-BUF(INDEX + 1).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign string array with variable index\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY NAMES-BUF(INDEX + 1)(1:NAMES-LEN(INDEX + 1)).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display string array with variable index\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY NAMES-LEN(INDEX + 1).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display string length with variable index\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_copy_includes)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "copy \"shared-status\";\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy directives should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "copy directives should regenerate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
    {
        std::printf("Assertion failed: expected COBOL generation to produce text\\n");
        goto cleanup;
    }
    if (!std::strstr(generated_cobol, "       COPY SHARED-STATUS."))
    {
        std::printf("Assertion failed: COBOL should include COPY directive for shared-status\\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_multiple_paragraphs)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void helper()\n"
        "{\n"
        "    display(\"FIRST\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    display(\"SECOND\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "multiple function program should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "multiple function program should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "HELPER.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: helper paragraph should be emitted\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MAIN.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: main paragraph should be emitted\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY \"FIRST\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: helper statements should be scoped to helper paragraph\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY \"SECOND\"", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: main statements should be scoped to main paragraph\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_int_function_parameters)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "int add_one(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "int function with parameters should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.function_count, 1,
            "translation unit should record one function") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.functions[0].parameter_count, 1,
            "function should record one parameter") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].parameters[0].source_name, "value",
            "parameter source name should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].parameters[0].actual_source_name,
            "add_one__value", "parameter storage name should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].parameters[0].cobol_name,
            "ADD-ONE-VALUE", "parameter COBOL name should be recorded") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_struct_return_type)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    return result;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct-returning function should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.function_count, 1,
            "translation unit should record one struct-returning function") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.functions[0].return_kind, CBLC_FUNCTION_RETURN_STRUCT,
            "function should record a struct return kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.functions[0].return_type_name, "Point",
            "function should record the struct return type name") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_parameter_slot_assignments_for_local_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int total;\n"
        "int result;\n"
        "int add_one(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    result = add_one(total);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized local call sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "parameterized local call sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "static int add_one__value = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit global parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "add_one__value = total;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign argument into parameter storage before local call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "result = add_one();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should still call local helper after assigning parameters\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_struct_returning_function_and_call_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    return result;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    point = make_point();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct return sample should parse for C generation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "struct return sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "t_Point make_point(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit a struct return type for helper functions\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "point = make_point();", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign returned struct values into matching targets\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_parameter_slot_assignments_for_local_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int total;\n"
        "int result;\n"
        "int add_one(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    result = add_one(total);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized local call COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "parameterized local call COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 ADD-ONE-VALUE PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit parameter storage item\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE ADD-ONE-VALUE = total.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign argument into parameter storage before PERFORM\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "PERFORM ADD-ONE.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should perform local helper after assigning parameters\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_struct_returning_function_and_call_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "Point make_point()\n"
        "{\n"
        "    Point result;\n"
        "    result.x = 7;\n"
        "    return result;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    point = make_point();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct return sample should parse for COBOL generation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "struct return sample should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 CBLC-RETURN-MAKE-POINT.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit a grouped return slot for struct-returning functions\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "PERFORM MAKE-POINT.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should perform the struct-returning helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE CBLC-RETURN-MAKE-POINT TO POINT", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should move the grouped return slot into the target variable\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_struct_returning_method)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "class Builder\n"
        "{\n"
        "    public:\n"
        "    Point current()\n"
        "    {\n"
        "        Point result;\n"
        "        result.x = 7;\n"
        "        return result;\n"
        "    }\n"
        "};\n"
        "Builder builder;\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point = builder.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct-returning method sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[2].method_count, 1,
            "class should record one struct-returning method") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[2].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_STRUCT,
            "method should record a struct return kind") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[2].methods[0].return_type_name, "Point",
            sizeof(unit.struct_types[2].methods[0].return_type_name)) != 0)
    {
        std::printf("Assertion failed: method should record the struct return type name\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_and_cobol_emit_struct_returning_method_call_assignment)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "class Builder\n"
        "{\n"
        "    public:\n"
        "    Point value;\n"
        "    void seed()\n"
        "    {\n"
        "        value.x = 7;\n"
        "        return;\n"
        "    }\n"
        "    Point current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Builder builder;\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    builder.seed();\n"
        "    point = builder.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct-returning method generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "struct-returning method generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "struct-returning method generation sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c, "point = builder.value;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign the returned struct method value into the target\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE BUILDER-VALUE TO POINT.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should move the returned struct method value into the target\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_rejects_local_argument_count_mismatch)
{
    const char *source;
    t_cblc_translation_unit unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    source = "void helper(int value)\n"
        "{\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local argument mismatch sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: local call with wrong argument count should fail resolution\n");
        goto cleanup;
    }
    if (context.diagnostics.count == 0
        || context.diagnostics.items[context.diagnostics.count - 1].code
            != TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH)
    {
        std::printf("Assertion failed: local argument count mismatch should record dedicated diagnostic\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_resolve_translation_unit_calls_accepts_external_parameterized_calls)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    worker(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external parameterized call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with parameter should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: external parameterized calls should resolve when imports and signatures match\n");
        goto cleanup;
    }
    if (!main_unit.functions[0].statements[1].call_is_external)
    {
        std::printf("Assertion failed: resolved external parameterized call should stay marked external\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_external_parameterized_call_arguments)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_c;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    worker(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    display(value);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_c = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external parameterized call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with external parameter should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&main_unit, &generated_c),
            "main module with external parameterized call should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "worker(total);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit external call arguments directly\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_perform_and_call_statements)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_cobol;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "void helper()\n"
        "{\n"
        "    display(\"LOCAL\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    worker();\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker()\n"
        "{\n"
        "    display(\"WORKER\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_cobol),
            "main module should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "PERFORM HELPER", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: local call should emit PERFORM statement\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "CALL 'WORKER'", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: external call should emit CALL statement\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_parameterized_call_and_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_main_cobol;
    char *generated_worker_cobol;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = 4;\n"
        "    worker(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "void worker(int value)\n"
        "{\n"
        "    display(value);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external parameterized COBOL call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with external parameterized COBOL call should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "main module with external parameterized call should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_worker_cobol),
            "worker module with parameter should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_main_cobol,
            "CALL 'WORKER' USING BY REFERENCE total", std::strlen(generated_main_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should pass external int parameter by reference\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol, "LINKAGE SECTION.", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit linkage section for entry parameters\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "01 WORKER-VALUE PIC S9(9).", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit linkage storage for parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "PROCEDURE DIVISION USING WORKER-VALUE.", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept parameter in procedure division\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_external_return_call_and_linkage)
{
    const char *main_source;
    const char *worker_source;
    t_cblc_translation_unit main_unit;
    t_cblc_translation_unit worker_unit;
    t_transpiler_context context;
    char *generated_main_cobol;
    char *generated_worker_cobol;
    int status;
    int context_initialized;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker(4);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    worker_source = "int worker(int value)\n"
        "{\n"
        "    return value + 1;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    cblc_translation_unit_init(&worker_unit);
    generated_main_cobol = NULL;
    generated_worker_cobol = NULL;
    status = FT_FAILURE;
    context_initialized = 0;
    if (test_expect_success(cblc_parse_translation_unit(main_source, &main_unit),
            "main module with external return call should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(worker_source, &worker_unit),
            "worker module with external return should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "worker_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_register_module_import(&context, "main_mod", "worker_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "worker_mod", &worker_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &main_unit) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&main_unit, &generated_main_cobol),
            "main module with external return call should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&worker_unit, &generated_worker_cobol),
            "worker module with external return should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_main_cobol,
            "CALL 'WORKER' USING BY VALUE 4 BY REFERENCE TOTAL", std::strlen(generated_main_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should append assignment target as external return slot\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "01 CBLC-RETURN-WORKER PIC S9(9).", std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should emit linkage storage for return slot\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_worker_cobol,
            "PROCEDURE DIVISION USING WORKER-VALUE CBLC-RETURN-WORKER.",
            std::strlen(generated_worker_cobol)))
    {
        std::printf("Assertion failed: worker COBOL should accept parameter and return slot in procedure division\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_main_cobol)
        cma_free(generated_main_cobol);
    if (generated_worker_cobol)
        cma_free(generated_worker_cobol);
    cblc_translation_unit_dispose(&main_unit);
    cblc_translation_unit_dispose(&worker_unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_handles_string_assignments_and_length_computations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "string greeting(8);\n"
        "string target(8);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    target = greeting;\n"
        "    total = greeting.len + target.len;\n"
        "    display(target);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string programs should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string programs should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE \"HELLO\" TO GREETING-BUF",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: literal assignment should target GREETING-BUF\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE 5 TO GREETING-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: literal assignment should update GREETING-LEN\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE GREETING TO TARGET",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: string to string assignment should move groups\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "COMPUTE TOTAL = GREETING-LEN + TARGET-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: length expressions should translate into COMPUTE\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY TARGET-BUF(1:TARGET-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: DISPLAY of string should use buffer slice\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY TOTAL",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: DISPLAY of int should emit standard form\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_resolve_calls_reports_missing_function)
{
    const char *source;
    t_cblc_translation_unit unit;
    t_transpiler_context context;
    int context_initialized;
    int status;

    source = "void main()\n"
        "{\n"
        "    missing();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    context_initialized = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "module should parse") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        goto cleanup;
    context_initialized = 1;
    if (transpiler_context_register_module(&context, "main_mod", "main_mod") != FT_SUCCESS)
        goto cleanup;
    if (cblc_register_translation_unit_exports(&context, "main_mod", &unit) != FT_SUCCESS)
        goto cleanup;
    if (cblc_resolve_translation_unit_calls(&context, "main_mod", &unit) != FT_FAILURE)
    {
        std::printf("Assertion failed: unresolved function should trigger failure\n");
        goto cleanup;
    }
    if (!transpiler_context_has_errors(&context))
    {
        std::printf("Assertion failed: unresolved function should record diagnostics\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    if (context_initialized)
        transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_requires_resolved_imported_calls)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "import \"worker_mod\";\n"
        "void main()\n"
        "{\n"
        "    worker();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "imported external call sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (cblc_generate_cobol(&unit, &generated_cobol) != FT_FAILURE)
    {
        std::printf("Assertion failed: direct COBOL generation should reject unresolved imported calls\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_external_return_assignment)
{
    const char *main_source;
    t_cblc_translation_unit main_unit;
    int status;

    main_source = "import \"worker_mod\";\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = worker();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&main_unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(main_source, &main_unit) != FT_SUCCESS)
    {
        std::printf("Assertion failed: parser should accept external return assignment syntax\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&main_unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_handles_multiplication_and_division)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int left;\n"
        "int right;\n"
        "int product;\n"
        "int quotient;\n"
        "void main()\n"
        "{\n"
        "    product = left * right;\n"
        "    quotient = product / right;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "numeric declarations should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "numeric declarations should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE PRODUCT = LEFT * RIGHT", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: multiplication should translate into COMPUTE statement\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE QUOTIENT = PRODUCT / RIGHT", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: division should translate into COMPUTE statement\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_folds_strlen_string_literal_to_constant)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"hello world\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "std::strlen with string literal should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "std::strlen with string literal should convert to COBOL")
        != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = 11",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should fold std::strlen string literals to constants\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_cobol, "CALL 'CBLC-STRLEN'",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should not call CBLC-STRLEN for string literals\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_struct_fields)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name(8);\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"HI\";\n"
        "    display(point.x);\n"
        "    display(point.name);\n"
        "    display(point.name.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct program should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 2,
            "builtin string plus one user struct type should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_count, 3,
            "struct instance should register parent plus field items") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[0].source_name, "point",
            sizeof(unit.data_items[0].source_name)) != 0
        || unit.data_items[0].kind != CBLC_DATA_KIND_STRUCT)
    {
        std::printf("Assertion failed: first data item should be the struct instance\n");
        goto cleanup;
    }
    if (std::strncmp(unit.data_items[1].source_name, "point.x",
            sizeof(unit.data_items[1].source_name)) != 0
        || unit.data_items[1].kind != CBLC_DATA_KIND_INT)
    {
        std::printf("Assertion failed: struct int field should be registered as data item\n");
        goto cleanup;
    }
    if (std::strncmp(unit.data_items[2].source_name, "point.name",
            sizeof(unit.data_items[2].source_name)) != 0
        || unit.data_items[2].kind != CBLC_DATA_KIND_STRING)
    {
        std::printf("Assertion failed: struct string field should be registered as data item\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_struct_groups)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name(8);\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"HI\";\n"
        "    display(point.name);\n"
        "    display(point.name.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "struct COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 POINT.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit struct group header\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 POINT-X PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit int struct field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 POINT-NAME.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit string struct field group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "10 POINT-NAME-LEN PIC 9(4) COMP VALUE 0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit struct string length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE POINT-X = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign to struct int field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY POINT-NAME-BUF(1:POINT-NAME-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display struct string field via slice\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY POINT-NAME-LEN", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display struct string length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_main_and_helpers)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "string greeting(8);\n"
        "int counter;\n"
        "void helper()\n"
        "{\n"
        "    display(\"HI\");\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    greeting = \"HELLO\";\n"
        "    counter = 1 + 2;\n"
        "    display(greeting);\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "sample CBL-C should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate C code") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "#include <stddef.h>", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should include stddef header\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "static size_t greeting_len = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare greeting length variable\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "static char greeting_buf[8] = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare greeting buffer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void helper(void);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should prototype helper function\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "int main(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should define an int main wrapper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(greeting_buf, 8, &greeting_len, \"HELLO\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign greeting literal via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(greeting_buf, greeting_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display greeting using helper\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_omits_main_for_library)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "void helper()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "library CBL-C should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "C backend should generate library code") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (ft_strnstr(generated_c, "int main(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: library output should not define main\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void helper(void);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: library output should declare helper prototype\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void helper(void)", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: library output should define helper function\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_struct_types_and_field_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "    string name(8);\n"
        "};\n"
        "Point point;\n"
        "void main()\n"
        "{\n"
        "    point.x = 7;\n"
        "    point.name = \"HELLO\";\n"
        "    display(point.name);\n"
        "    display(point.name.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "struct C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "struct C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "typedef struct s_Point", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit struct typedef\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "struct { size_t len; char buf[8]; } name;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit nested string storage in struct\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "static t_Point point = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit struct instance global\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "point.x = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign to struct int field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(point.name.buf, 8, &point.name.len, \"HELLO\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign struct string field via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(point.name.buf, point.name.len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display struct string field via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_size(point.name.len);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display struct string length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_nested_struct_fields)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city(16);\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    person.age = 7;\n"
        "    display(person.address.city);\n"
        "    display(person.address.city.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct program should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 3,
            "builtin string plus two user struct types should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_count, 4,
            "nested struct instance should register aggregate path items recursively") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[1].source_name, "person.address",
            sizeof(unit.data_items[1].source_name)) != 0
        || unit.data_items[1].kind != CBLC_DATA_KIND_STRUCT)
    {
        std::printf("Assertion failed: nested struct field should be registered as aggregate data item\n");
        goto cleanup;
    }
    if (std::strncmp(unit.data_items[2].source_name, "person.address.city",
            sizeof(unit.data_items[2].source_name)) != 0
        || unit.data_items[2].kind != CBLC_DATA_KIND_STRING)
    {
        std::printf("Assertion failed: nested primitive field should be registered recursively\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_nested_struct_groups)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city(16);\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    display(person.address.city);\n"
        "    display(person.address.city.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested struct COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "05 PERSON-ADDRESS.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested struct group header\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "10 PERSON-ADDRESS-CITY.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested string group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "15 PERSON-ADDRESS-CITY-LEN PIC 9(4) COMP VALUE 0.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit nested string length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY PERSON-ADDRESS-CITY-BUF(1:PERSON-ADDRESS-CITY-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should display nested string field via slice\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_nested_struct_types_and_field_access)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "struct Address\n"
        "{\n"
        "    string city(16);\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "    int age;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    person.address.city = \"GHENT\";\n"
        "    display(person.address.city);\n"
        "    display(person.address.city.len);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested struct C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "nested struct C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "typedef struct s_Address", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit nested member type typedef\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "t_Address address;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should embed nested struct field type\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(person.address.city.buf, 16, &person.address.city.len, \"GHENT\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign nested string field via helper\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_display_string(person.address.city.buf, person.address.city.len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should display nested string field via helper\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_class_lifecycle_metadata)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person();\n"
        "    ~Person();\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class lifecycle sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 2,
            "builtin string plus one class type should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].is_class, 1,
            "type should be marked as class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].has_default_constructor, 1,
            "class should record default constructor") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].has_destructor, 1,
            "class should record destructor") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_automatic_class_lifecycle)
{
    const char *source;
    const char *needle;
    const char *scan;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;
    int count;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person();\n"
        "    ~Person();\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class lifecycle C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "class lifecycle C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    needle = "person.name.len = 0;";
    count = 0;
    scan = generated_c;
    while ((scan = std::strstr(scan, needle)) != NULL)
    {
        count += 1;
        scan += std::strlen(needle);
    }
    if (test_expect_int_equal(count, 2,
            "generated C should inject lifecycle before entry and before return") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_c, "memset(person.name.buf, 0, sizeof(person.name.buf));",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should zero string buffer during lifecycle\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_automatic_class_lifecycle)
{
    const char *source;
    const char *needle;
    const char *scan;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;
    int count;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person();\n"
        "    ~Person();\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class lifecycle COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class lifecycle COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    needle = "MOVE 0 TO PERSON-NAME-LEN.";
    count = 0;
    scan = generated_cobol;
    while ((scan = std::strstr(scan, needle)) != NULL)
    {
        count += 1;
        scan += std::strlen(needle);
    }
    if (test_expect_int_equal(count, 2,
            "generated COBOL should inject lifecycle before entry and before return") != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE SPACES TO PERSON-NAME-BUF.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should blank string buffer during lifecycle\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_custom_class_lifecycle_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "custom lifecycle C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "custom lifecycle C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(person.name.buf, 8, &person.name.len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit custom constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(person.name.buf, 8, &person.name.len, \"BYE\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit custom destructor body\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_custom_class_lifecycle_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "custom lifecycle COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "custom lifecycle COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit custom constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"BYE\" TO PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit custom destructor body\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_local_class_lifecycle)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    Person person;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local class lifecycle C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "local class lifecycle C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "t_Person main__person = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare local class storage inside function\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(main__person.name.buf, 8, &main__person.name.len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_string_assign_literal(main__person.name.buf, 8, &main__person.name.len, \"BYE\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local destructor body before return\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_local_class_lifecycle)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    Person person;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local class lifecycle COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "local class lifecycle COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "01 MAIN-PERSON.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit function-owned class storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO MAIN-PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit local constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE \"BYE\" TO MAIN-PERSON-NAME-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit local destructor body before return\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_nested_block_local_class_cleanup_order)
{
    const char *source;
    const char *first_ctor;
    const char *first_dtor;
    const char *second_ctor;
    const char *second_dtor;
    const char *p_first_ctor;
    const char *p_first_dtor;
    const char *p_second_ctor;
    const char *p_second_dtor;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    {\n"
        "        Person first;\n"
        "    }\n"
        "    Person second;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested block local class C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "nested block local class C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    first_ctor = "cblc_string_assign_literal(main__first.name.buf, 8, &main__first.name.len, \"HI\");";
    first_dtor = "cblc_string_assign_literal(main__first.name.buf, 8, &main__first.name.len, \"BYE\");";
    second_ctor = "cblc_string_assign_literal(main__second.name.buf, 8, &main__second.name.len, \"HI\");";
    second_dtor = "cblc_string_assign_literal(main__second.name.buf, 8, &main__second.name.len, \"BYE\");";
    p_first_ctor = std::strstr(generated_c, first_ctor);
    p_first_dtor = std::strstr(generated_c, first_dtor);
    p_second_ctor = std::strstr(generated_c, second_ctor);
    p_second_dtor = std::strstr(generated_c, second_dtor);
    if (!p_first_ctor || !p_first_dtor || !p_second_ctor || !p_second_dtor)
    {
        std::printf("Assertion failed: generated C should emit all nested block lifecycle statements\n");
        goto cleanup;
    }
    if (!(p_first_ctor < p_first_dtor && p_first_dtor < p_second_ctor && p_second_ctor < p_second_dtor))
    {
        std::printf("Assertion failed: generated C should destroy inner block object before later outer object construction/cleanup\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_nested_block_local_class_cleanup_order)
{
    const char *source;
    const char *first_ctor;
    const char *first_dtor;
    const char *second_ctor;
    const char *second_dtor;
    const char *p_first_ctor;
    const char *p_first_dtor;
    const char *p_second_ctor;
    const char *p_second_dtor;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name(8);\n"
        "    Person() {\n"
        "        name = \"HI\";\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = \"BYE\";\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    {\n"
        "        Person first;\n"
        "    }\n"
        "    Person second;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested block local class COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested block local class COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    first_ctor = "MOVE \"HI\" TO MAIN-FIRST-NAME-BUF.";
    first_dtor = "MOVE \"BYE\" TO MAIN-FIRST-NAME-BUF.";
    second_ctor = "MOVE \"HI\" TO MAIN-SECOND-NAME-BUF.";
    second_dtor = "MOVE \"BYE\" TO MAIN-SECOND-NAME-BUF.";
    p_first_ctor = std::strstr(generated_cobol, first_ctor);
    p_first_dtor = std::strstr(generated_cobol, first_dtor);
    p_second_ctor = std::strstr(generated_cobol, second_ctor);
    p_second_dtor = std::strstr(generated_cobol, second_dtor);
    if (!p_first_ctor || !p_first_dtor || !p_second_ctor || !p_second_dtor)
    {
        std::printf("Assertion failed: generated COBOL should emit all nested block lifecycle statements\n");
        goto cleanup;
    }
    if (!(p_first_ctor < p_first_dtor && p_first_dtor < p_second_ctor && p_second_ctor < p_second_dtor))
    {
        std::printf("Assertion failed: generated COBOL should destroy inner block object before later outer object construction/cleanup\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_registers_builtin_string_class)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "string greeting(8);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string class sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 1,
            "builtin string class should be registered even without user classes") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[0].source_name, "string",
            sizeof(unit.struct_types[0].source_name)) != 0)
    {
        std::printf("Assertion failed: builtin string class should be registered under the string name\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.struct_types[0].is_class, 1,
            "builtin string should be marked as a class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[0].is_builtin, 1,
            "builtin string should be marked as builtin") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[0].constructor_count, 1,
            "builtin string should expose a copy-constructor overload") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(unit.struct_types[0].constructors[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_STRING,
            "builtin string copy constructor should use the string parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[0].method_count, 10,
            "builtin string should expose append, len, clear, empty, equals, capacity, starts_with, ends_with, compare, and contains methods") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[0].methods[0].parameter_count, 1,
            "builtin string append method should record one parameter") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(unit.struct_types[0].methods[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_STRING,
            "builtin string append parameter should use the string parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[0].declared_type_name, "string",
            sizeof(unit.data_items[0].declared_type_name)) != 0)
    {
        std::printf("Assertion failed: string variables should record the string class as their declared type\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_string_constructor_style_initialization)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HI\");\n"
        "    string copy(greeting);\n"
        "    display(copy);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string constructor-style sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_items[0].length, 2,
            "literal string constructor should infer capacity from the literal") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_items[0].constructor_argument_count, 1,
            "literal string constructor should store one constructor argument") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_items[2].length, 2,
            "copy string constructor should infer capacity from the source string") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.data_items[2].constructor_argument_count, 1,
            "copy string constructor should store one constructor argument") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_local_string_lifecycle)
{
    const char *source;
    const char *ctor_reset;
    const char *assign_call;
    const char *dtor_reset;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    greeting = \"HI\";\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local string lifecycle sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "local string lifecycle sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "size_t main__greeting_len = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local string length storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "char main__greeting_buf[8] = {0};", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local string buffer storage\n");
        goto cleanup;
    }
    ctor_reset = std::strstr(generated_c, "memset(main__greeting_buf, 0, sizeof(main__greeting_buf));");
    if (!ctor_reset)
    {
        std::printf("Assertion failed: generated C should construct local string objects\n");
        goto cleanup;
    }
    assign_call = std::strstr(ctor_reset, "cblc_string_assign_literal(main__greeting_buf, 8, &main__greeting_len, \"HI\");");
    if (!assign_call)
    {
        std::printf("Assertion failed: generated C should preserve local string assignment after construction\n");
        goto cleanup;
    }
    dtor_reset = std::strstr(assign_call, "memset(main__greeting_buf, 0, sizeof(main__greeting_buf));");
    if (!dtor_reset)
    {
        std::printf("Assertion failed: generated C should destroy local string objects before return\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_builtin_string_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    string suffix(4);\n"
        "    greeting = \"HI\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    total = greeting.len();\n"
        "    total = greeting.capacity();\n"
        "    greeting.clear();\n"
        "    total = greeting.empty();\n"
        "    total = greeting.equals(suffix);\n"
        "    total = greeting.starts_with(suffix);\n"
        "    total = greeting.ends_with(suffix);\n"
        "    total = greeting.compare(suffix);\n"
        "    total = greeting.contains(suffix);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string methods sample should parse") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_builtin_string_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    string suffix(4);\n"
        "    greeting = \"HI\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    total = greeting.len();\n"
        "    total = greeting.capacity();\n"
        "    greeting.clear();\n"
        "    total = greeting.empty();\n"
        "    total = greeting.equals(suffix);\n"
        "    total = greeting.starts_with(suffix);\n"
        "    total = greeting.ends_with(suffix);\n"
        "    total = greeting.compare(suffix);\n"
        "    total = greeting.contains(suffix);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string methods C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "builtin string methods C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c,
            "cblc_string_append(main__greeting_buf, 8, &main__greeting_len, main__suffix_buf, main__suffix_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string append helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = (int)main__greeting_len;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should map builtin string len method to length storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = 8;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should map builtin string capacity method to declared capacity\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(main__greeting_buf, 8, &main__greeting_len, \"\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string clear helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = (main__greeting_len == 0);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should map builtin string empty method to a length check\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "total = cblc_string_equals(main__greeting_buf, main__greeting_len, main__suffix_buf, main__suffix_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string equals helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "total = cblc_string_starts_with(main__greeting_buf, main__greeting_len, main__suffix_buf, main__suffix_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string starts_with helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "total = cblc_string_ends_with(main__greeting_buf, main__greeting_len, main__suffix_buf, main__suffix_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string ends_with helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "total = cblc_string_compare(main__greeting_buf, main__greeting_len, main__suffix_buf, main__suffix_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string compare helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "total = cblc_string_contains(main__greeting_buf, main__greeting_len, main__suffix_buf, main__suffix_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string contains helper call\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_string_constructor_style_initialization)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HI\");\n"
        "    string copy(greeting);\n"
        "    display(copy);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string constructor-style C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "string constructor-style C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(main__greeting_buf, 2, &main__greeting_len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit the literal string constructor assignment\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign(main__copy_buf, 2, &main__copy_len, main__greeting_buf, main__greeting_len);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit the copy string constructor assignment\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_builtin_string_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    string suffix(4);\n"
        "    greeting = \"HI\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    total = greeting.len();\n"
        "    total = greeting.capacity();\n"
        "    greeting.clear();\n"
        "    total = greeting.empty();\n"
        "    total = greeting.equals(suffix);\n"
        "    total = greeting.starts_with(suffix);\n"
        "    total = greeting.ends_with(suffix);\n"
        "    total = greeting.compare(suffix);\n"
        "    total = greeting.contains(suffix);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string methods COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "builtin string methods COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE MAIN-GREETING BY REFERENCE MAIN-GREETING BY REFERENCE MAIN-SUFFIX BY REFERENCE CBLC-HELPER-STATUS.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string append helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = MAIN-GREETING-LEN.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should map builtin string len method to GREETING-LEN\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = 8.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should map builtin string capacity method to declared capacity\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE SPACES TO MAIN-GREETING-BUF.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string clear buffer reset\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "IF MAIN-GREETING-LEN = 0", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string empty length check\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "IF MAIN-GREETING-LEN = MAIN-SUFFIX-LEN",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string equals length comparison\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "IF MAIN-GREETING-BUF(1:MAIN-GREETING-LEN) = MAIN-SUFFIX-BUF(1:MAIN-SUFFIX-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string equals comparison\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "IF MAIN-GREETING-LEN >= MAIN-SUFFIX-LEN AND MAIN-GREETING-BUF(1:MAIN-SUFFIX-LEN) = MAIN-SUFFIX-BUF(1:MAIN-SUFFIX-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string starts_with comparison\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "IF MAIN-GREETING-LEN >= MAIN-SUFFIX-LEN AND MAIN-GREETING-BUF(MAIN-GREETING-LEN - MAIN-SUFFIX-LEN + 1:MAIN-SUFFIX-LEN) = MAIN-SUFFIX-BUF(1:MAIN-SUFFIX-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string ends_with comparison\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "CALL 'CBLC-STRCMP-STRING' USING BY REFERENCE MAIN-GREETING BY REFERENCE MAIN-SUFFIX BY REFERENCE TOTAL.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string compare helper call\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "INSPECT MAIN-GREETING-BUF(1:MAIN-GREETING-LEN) TALLYING TOTAL FOR ALL MAIN-SUFFIX-BUF(1:MAIN-SUFFIX-LEN)",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string contains tallying logic\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_string_constructor_style_initialization)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HI\");\n"
        "    string copy(greeting);\n"
        "    display(copy);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string constructor-style COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string constructor-style COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "MOVE \"HI\" TO MAIN-GREETING-BUF.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit the literal string constructor move\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE MAIN-GREETING-BUF TO MAIN-COPY-BUF.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit the copy string constructor buffer move\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE MAIN-COPY-LEN = MAIN-GREETING-LEN.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit the copy string constructor length move\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class method sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].method_count, 2,
            "class should record both methods") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[1].methods[0].source_name, "reset",
            sizeof(unit.struct_types[1].methods[0].source_name)) != 0)
    {
        std::printf("Assertion failed: first class method should be reset\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.struct_types[1].methods[0].return_kind,
            CBLC_FUNCTION_RETURN_VOID, "reset should be a void method") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.struct_types[1].methods[1].source_name, "current",
            sizeof(unit.struct_types[1].methods[1].source_name)) != 0)
    {
        std::printf("Assertion failed: second class method should be current\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.struct_types[1].methods[1].return_kind,
            CBLC_FUNCTION_RETURN_INT, "current should be an int method") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_parameterized_class_lifecycle_and_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    size_t item_index;
    int found_counter;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    Counter(int start) {\n"
        "        value = start;\n"
        "    }\n"
        "    void add(int delta) {\n"
        "        value = value + delta;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Counter counter(7);\n"
        "void main()\n"
        "{\n"
        "    counter.add(2);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    found_counter = 0;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized class sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].constructor_count, 1,
            "class should record one constructor overload") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].constructors[0].parameter_count, 1,
            "class should record one constructor parameter") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].method_count, 1,
            "class should record one parameterized method") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].methods[0].parameter_count, 1,
            "parameterized method should record one parameter") != FT_SUCCESS)
        goto cleanup;
    item_index = 0;
    while (item_index < unit.data_count)
    {
        if (std::strncmp(unit.data_items[item_index].source_name, "counter",
                sizeof(unit.data_items[item_index].source_name)) == 0)
        {
            found_counter = 1;
            if (test_expect_size_t_equal(unit.data_items[item_index].constructor_argument_count, 1,
                    "class instance should retain one constructor argument") != FT_SUCCESS)
                goto cleanup;
            break;
        }
        item_index++;
    }
    if (!found_counter)
    {
        std::printf("Assertion failed: class instance should be present in data items\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.value = 7;\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class method C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "class method C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "counter.value = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should inline void method body against receiver\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = counter.value;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should inline int-returning method body against receiver\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_and_cobol_emit_parameterized_class_lifecycle_and_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    Counter(int start) {\n"
        "        value = start;\n"
        "    }\n"
        "    void add(int delta) {\n"
        "        value = value + delta;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter(7);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.add(2);\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized class generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "parameterized class generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "parameterized class generation sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c, "static int Counter__ctor__start = 0;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare constructor parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "Counter__ctor__start = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign constructor argument before lifecycle body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "counter.value = Counter__ctor__start;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use constructor parameter inside lifecycle body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "Counter__add__delta = 2;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should assign method argument before inlined method body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "counter.value = counter.value + Counter__add__delta;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use method parameter inside inlined method body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "01 COUNTER-CTOR-START PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare constructor parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-CTOR-START = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign constructor argument before lifecycle body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-VALUE = COUNTER-CTOR-START.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use constructor parameter inside lifecycle body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-ADD-DELTA = 2.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign method argument before inlined method body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-VALUE = COUNTER-VALUE + COUNTER-ADD-DELTA.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use method parameter inside inlined method body\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_constructor_overloads_by_arity)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    Counter()\n"
        "    {\n"
        "        value = 1;\n"
        "    }\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "};\n"
        "Counter first;\n"
        "Counter second(7);\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "constructor overload sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].constructor_count, 2,
            "class should record two constructor overloads") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].has_default_constructor, 1,
            "class should keep a default constructor when zero-arg overload exists") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].constructors[0].parameter_count, 0,
            "first constructor overload should be zero-argument") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].constructors[1].parameter_count, 1,
            "second constructor overload should be one-argument") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_and_cobol_emit_constructor_overloads_by_arity)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter()\n"
        "    {\n"
        "        value = 1;\n"
        "    }\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter first;\n"
        "Counter second(7);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = first.current();\n"
        "    total = second.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "constructor overload generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "constructor overload generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "constructor overload generation sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c, "first.value = 1;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use the zero-argument constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "Counter__ctor_1__start = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should bind the one-argument constructor parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE FIRST-VALUE = 1.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use the zero-argument constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-CTOR-1-START = 7.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should bind the one-argument constructor parameter storage\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_records_copy_constructor_overload)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    Counter(Counter other)\n"
        "    {\n"
        "        this.value = other.value;\n"
        "    }\n"
        "};\n"
        "Counter seeded(7);\n"
        "void main()\n"
        "{\n"
        "    Counter copy(seeded);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy constructor sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_types[1].constructor_count, 2,
            "class should record both int and copy constructor overloads") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].constructors[0].parameters[0].kind,
            TRANSPILE_FUNCTION_PARAMETER_INT,
            "first overload should keep the int parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(unit.struct_types[1].constructors[1].parameters[0].kind,
            TRANSPILE_FUNCTION_PARAMETER_STRUCT,
            "second overload should record the struct parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(unit.struct_types[1].constructors[1].parameters[0].type_name,
            "Counter", "copy constructor should record the struct parameter type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_and_cobol_emit_copy_constructor_overload)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    Counter(Counter other)\n"
        "    {\n"
        "        this.value = other.value;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter seeded(7);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Counter copy(seeded);\n"
        "    total = copy.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy constructor generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "copy constructor generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "copy constructor generation sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c, "Counter__ctor__start = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should keep the int constructor storage distinct\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "Counter__ctor_1__other = seeded;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should bind the copy constructor struct parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "copy.value = Counter__ctor_1__other.value;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use the copy constructor body against the struct parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE SEEDED TO COUNTER-CTOR-1-OTHER.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should bind the copy constructor struct parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE MAIN-COPY-VALUE = COUNTER-CTOR-1-OTHER-VALUE.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use the copy constructor body against the struct parameter\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.value = 7;\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class method COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "class method COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol, "COMPUTE COUNTER-VALUE = 0.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should inline void method body against receiver\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = COUNTER-VALUE.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should inline int-returning method body against receiver\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_private_field_access_outside_class)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    int value;\n"
        "};\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    counter.value = 7;\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: private class fields should not be accessible outside the class\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_private_method_call_outside_class)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    void reset() {\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    counter.reset();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: private class methods should not be callable outside the class\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_public_members_outside_class)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "public class methods should be callable outside the class") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_private_access_within_class_methods)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    void reset() {\n"
        "        value = 0;\n"
        "        return;\n"
        "    }\n"
        "    int current() {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.reset();\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "class methods should be allowed to access private fields internally") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_constructor_initializer_list_in_declaration_order)
{
    const char *source;
    const char *first_assign;
    const char *second_assign;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Pair\n"
        "{\n"
        "    int first;\n"
        "    int second;\n"
        "    Pair() : second(2), first(1) {\n"
        "    }\n"
        "};\n"
        "Pair pair;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "initializer list order sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "initializer list order sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    first_assign = std::strstr(generated_c, "pair.first = 1;");
    second_assign = std::strstr(generated_c, "pair.second = 2;");
    if (!first_assign || !second_assign)
    {
        std::printf("Assertion failed: generated C should emit all constructor initializer list assignments\n");
        goto cleanup;
    }
    if (!(first_assign < second_assign))
    {
        std::printf("Assertion failed: generated C should initialize members in declaration order\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_parses_const_member_initializer_list)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    string name(8);\n"
        "    Person() : name(\"HI\"), id(7) {\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "const member initializer list sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "const member initializer list sample should generate C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c, "person.id = 7;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should initialize const int members from initializer list\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(person.name.buf, 8, &person.name.len, \"HI\");",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should initialize string members from initializer list\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_accepts_const_member_initialized_once_in_constructor_body)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() {\n"
        "        id = 7;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "const member should be assignable once in constructor body") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_const_member_reassignment_in_constructor)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() : id(7) {\n"
        "        id = 8;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: const member should not be assignable more than once during construction\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_const_member_assignment_outside_constructor)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() : id(7) {\n"
        "    }\n"
        "    void reset() {\n"
        "        id = 0;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: const member should not be assignable outside constructors\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_parse_translation_unit_rejects_missing_const_member_initialization)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    const int id;\n"
        "    Person() {\n"
        "    }\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) == FT_SUCCESS)
    {
        std::printf("Assertion failed: const member should require exactly one construction-time initialization\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_transpiler_validation_accepts_valid_cobol)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. VALIDATE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        "           DISPLAY \"HELLO\".\n"
        "           STOP RUN.\n";
    if (test_expect_success(transpiler_validate_generated_cobol(source),
            "validator should accept valid COBOL") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_working_storage_program)
{
    const char *source;

    source = "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DEMO.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 GREETING PIC X(32).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           MOVE \"HELLO FROM CBL-C\" TO GREETING\n"
        "           DISPLAY GREETING\n"
        "           STOP RUN.\n";
    if (test_expect_success(transpiler_validate_generated_cobol(source),
            "validator should accept COBOL with working-storage items") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_invalid_cobol)
{
    const char *source;

    source = "invalid cobol text";
    if (transpiler_validate_generated_cobol(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject invalid COBOL text\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_token_get_span_reports_expected_columns)
{
    t_lexer lexer;
    t_lexer_token token;
    t_transpiler_source_span span;
    int status;

    lexer_init(&lexer, "MOVE VALUE TO TARGET.");
    ft_bzero(&token, sizeof(token));
    ft_bzero(&span, sizeof(span));
    status = FT_FAILURE;
    if (test_expect_success(lexer_next_token(&lexer, &token),
            "lexer should produce first token for span test") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(lexer_token_get_span(&token, "span_test.cob", &span),
            "token span helper should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.start_line, 1,
            "token span should start on line 1") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.start_column, 1,
            "token span should start at column 1") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.end_line, 1,
            "token span should end on line 1") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(span.end_column, 5,
            "token span end column should be one past the token") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(span.path, "span_test.cob",
            "token span should carry the supplied path") != FT_SUCCESS)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    return (status);
}

FT_TEST(test_ast_node_get_span_uses_node_token)
{
    t_lexer_token token;
    t_ast_node *node;
    t_transpiler_source_span span;
    int status;

    ft_bzero(&token, sizeof(token));
    ft_bzero(&span, sizeof(span));
    node = NULL;
    status = FT_FAILURE;
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = "FIELD-NAME";
    token.length = 10;
    token.line = 3;
    token.column = 7;
    node = ast_node_create(AST_NODE_IDENTIFIER);
    if (!node)
        return (FT_FAILURE);
    if (test_expect_success(ast_node_set_token(node, &token),
            "AST token assignment should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ast_node_get_span(node, "node_span.cob", &span),
            "AST node span helper should succeed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(span.start_line, 3,
            "node span should start on the token line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(span.start_column, 7,
            "node span should start on the token column") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(span.end_column, 17,
            "node span should end one past the token length") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (node)
        ast_node_destroy(node);
    return (status);
}

FT_TEST(test_cblc_frontend_analysis_reports_parse_diagnostic)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    int status;

    source = "class Greeter\n"
        "{\n"
        "    string name(16)\n"
        "};\n";
    status = FT_FAILURE;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_analyze_document(&analysis, "broken_sample.cblc", source) == FT_SUCCESS)
    {
        std::printf("Assertion failed: broken CBL-C sample should not parse successfully\n");
        goto cleanup;
    }
    if (test_expect_int_equal(static_cast<int>(analysis.diagnostics.count), 1,
            "broken CBL-C sample should emit one parse diagnostic") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(analysis.diagnostics.items[0].message, "Parse error",
            "CBL-C parse diagnostic should use stable parse message") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(analysis.diagnostics.items[0].span.path, "broken_sample.cblc",
            "CBL-C parse diagnostic should carry the document path") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(analysis.diagnostics.items[0].span.start_line, 1,
            "CBL-C parse diagnostic should currently point at the start of the file") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_collect_document_symbols_returns_types_functions_and_data)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_document_symbol_list symbols;
    size_t index;
    int found_class;
    int found_function;
    int found_counter;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "};\n"
        "\n"
        "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_class = 0;
    found_function = 0;
    found_counter = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for symbol test") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_document_symbol_list_init(&symbols),
            "CBL-C symbol list should initialize") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "symbols_sample.cblc", source),
            "CBL-C symbol sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_collect_document_symbols(&analysis, &symbols),
            "CBL-C symbol collection should succeed") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < symbols.count)
    {
        if (symbols.items[index].kind == CBLC_DOCUMENT_SYMBOL_CLASS
            && std::strncmp(symbols.items[index].name, "Counter",
                sizeof(symbols.items[index].name)) == 0)
            found_class = 1;
        if (symbols.items[index].kind == CBLC_DOCUMENT_SYMBOL_FUNCTION
            && std::strncmp(symbols.items[index].name, "main",
                sizeof(symbols.items[index].name)) == 0)
            found_function = 1;
        if (symbols.items[index].kind == CBLC_DOCUMENT_SYMBOL_DATA_ITEM
            && std::strncmp(symbols.items[index].name, "counter",
                sizeof(symbols.items[index].name)) == 0)
            found_counter = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_class, 1,
            "CBL-C symbol collection should include declared classes") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_function, 1,
            "CBL-C symbol collection should include the main function") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_counter, 1,
            "CBL-C symbol collection should include the global data item") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_document_symbol_list_dispose(&symbols);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_find_definition_resolves_data_item_reference)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_transpiler_source_span definition_span;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(&definition_span, sizeof(definition_span));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for definition lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "definition_sample.cblc", source),
            "CBL-C definition sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_find_definition(&analysis, 4, 13, &definition_span),
            "CBL-C definition lookup should resolve the displayed data item") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_line, 1,
            "CBL-C variable definition should resolve to the declaration line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_column, 5,
            "CBL-C variable definition should resolve to the identifier column") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(definition_span.path, "definition_sample.cblc",
            "CBL-C definition lookup should preserve the analysis path") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_find_definition_resolves_function_call_target)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_transpiler_source_span definition_span;
    int status;

    source = "void helper()\n"
        "{\n"
        "    return;\n"
        "}\n"
        "\n"
        "void main()\n"
        "{\n"
        "    helper();\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(&definition_span, sizeof(definition_span));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for function definition lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "call_definition_sample.cblc", source),
            "CBL-C function definition sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_find_definition(&analysis, 8, 6, &definition_span),
            "CBL-C definition lookup should resolve the function call target") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_line, 1,
            "CBL-C function definition should resolve to the declaration line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(definition_span.start_column, 6,
            "CBL-C function definition should resolve to the function identifier column") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_get_hover_reports_variable_summary)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    char hover[128];
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(hover, sizeof(hover));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for hover lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "hover_sample.cblc", source),
            "CBL-C hover sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_get_hover(&analysis, 4, 13, hover, sizeof(hover)),
            "CBL-C hover should resolve for variable reference") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(hover, "Variable counter : int",
            "CBL-C variable hover should include kind, name, and type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_get_hover_reports_class_summary)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    char hover[128];
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "        int value;\n"
        "};\n"
        "\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    ft_bzero(hover, sizeof(hover));
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for class hover") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "program_hover_sample.cblc", source),
            "CBL-C class hover sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_get_hover(&analysis, 7, 10, hover, sizeof(hover)),
            "CBL-C hover should resolve for a class-typed variable") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(hover, "Variable counter : Counter",
            "CBL-C hover should include the variable name and class type") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_find_references_collects_declaration_and_uses)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_source_span_list references;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    display(counter);\n"
        "    counter = 42;\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for reference lookup") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_source_span_list_init(&references),
            "CBL-C source span list should initialize for reference lookup") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "reference_sample.cblc", source),
            "CBL-C reference sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_find_references(&analysis, 4, 13, &references),
            "CBL-C references should resolve for a variable use") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.count, 3,
            "CBL-C references should include declaration and two uses") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.items[0].start_line, 1,
            "CBL-C references should begin with the declaration") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.items[1].start_line, 4,
            "CBL-C references should include the display use") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(references.items[2].start_line, 5,
            "CBL-C references should include the assignment use") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_source_span_list_dispose(&references);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_collect_semantic_tokens_classifies_core_tokens)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_semantic_token_list tokens;
    size_t index;
    int found_string;
    int found_number;
    int found_variable;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "    string name(\"HI\");\n"
        "    counter = 42;\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_string = 0;
    found_number = 0;
    found_variable = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for semantic token collection")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_semantic_token_list_init(&tokens),
            "CBL-C semantic token list should initialize") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "semantic_tokens_sample.cblc", source),
            "CBL-C semantic token sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_collect_semantic_tokens(&analysis, &tokens),
            "CBL-C semantic token collection should succeed") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < tokens.count)
    {
        if (tokens.items[index].kind == CBLC_SEMANTIC_TOKEN_STRING
            && tokens.items[index].span.start_line == 4)
            found_string = 1;
        if (tokens.items[index].kind == CBLC_SEMANTIC_TOKEN_NUMBER
            && tokens.items[index].span.start_line == 5)
            found_number = 1;
        if (tokens.items[index].kind == CBLC_SEMANTIC_TOKEN_VARIABLE
            && tokens.items[index].span.start_line == 1
            && tokens.items[index].span.start_column == 5)
            found_variable = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_string, 1,
            "CBL-C semantic tokens should include string literals") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_number, 1,
            "CBL-C semantic tokens should include numeric literals") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_variable, 1,
            "CBL-C semantic tokens should include declared variables") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_semantic_token_list_dispose(&tokens);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_keywords_and_symbols)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_display;
    int found_counter;
    int status;

    source = "int counter;\n"
        "void main()\n"
        "{\n"
        "\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_display = 0;
    found_counter = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for completion") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis, "completion_sample.cblc", source),
            "CBL-C completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 4, 1, &completions),
            "CBL-C completion should succeed on an empty statement line") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "display",
                sizeof(completions.items[index].label)) == 0)
            found_display = 1;
        if (std::strncmp(completions.items[index].label, "counter",
                sizeof(completions.items[index].label)) == 0)
            found_counter = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_display, 1,
            "CBL-C completion should include language keywords like display") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_counter, 1,
            "CBL-C completion should include in-document symbols like counter") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_filters_function_local_scope)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_other;
    int status;

    source = "void helper()\n"
        "{\n"
        "    string other(8);\n"
        "    return;\n"
        "}\n"
        "\n"
        "void main()\n"
        "{\n"
        "    string message(8);\n"
        "\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_other = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for local scope completion")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for local scope completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "local_scope_completion_sample.cblc", source),
            "CBL-C local scope completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 11, 5, &completions),
            "CBL-C completion should succeed inside the main function body") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "other",
                sizeof(completions.items[index].label)) == 0)
            found_other = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_other, 0,
            "CBL-C completion should exclude locals from other functions") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_public_members_only)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_name;
    int found_hidden;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
"        int hidden;\n"
        "\n"
        "    public:\n"
        "        int value;\n"
        "        void reset()\n"
        "        {\n"
            "            return;\n"
        "        }\n"
        "};\n"
        "\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    counter.value = 0;\n"
        "    counter.reset();\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_name = 0;
    found_hidden = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for member completion") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for member completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "member_completion_sample.cblc", source),
            "CBL-C member completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 17, 13, &completions),
            "CBL-C completion should succeed for public field member access") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "value",
                sizeof(completions.items[index].label)) == 0)
            found_name = 1;
        if (std::strncmp(completions.items[index].label, "hidden",
                sizeof(completions.items[index].label)) == 0)
            found_hidden = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_name, 1,
            "CBL-C completion should include public fields for member access") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_hidden, 0,
            "CBL-C completion should exclude private members for external access") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_std_namespace_functions)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_strlen;
    int found_strcpy;
    int found_append;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"HI\");\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_strlen = 0;
    found_strcpy = 0;
    found_append = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for std completion") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for std completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "std_completion_sample.cblc", source),
            "CBL-C std completion sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 4, 20, &completions),
            "CBL-C completion should succeed for std namespace access") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "std::strlen",
                sizeof(completions.items[index].label)) == 0)
            found_strlen = 1;
        if (std::strncmp(completions.items[index].label, "std::strcpy",
                sizeof(completions.items[index].label)) == 0)
            found_strcpy = 1;
        if (std::strncmp(completions.items[index].label, "append",
                sizeof(completions.items[index].label)) == 0)
            found_append = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_strlen, 1,
            "CBL-C completion should include matching std namespace functions") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_strcpy, 1,
            "CBL-C completion should include other matching std string helpers") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_append, 0,
            "CBL-C std completion should not mix in unrelated member names") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_returns_builtin_string_members)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    size_t index;
    int found_append;
    int found_clear;
    int found_capacity;
    int found_empty;
    int found_ends_with;
    int found_equals;
    int found_len;
    int found_compare;
    int found_contains;
    int found_starts_with;
    int status;

    source = "void main()\n"
        "{\n"
        "    string.\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    found_append = 0;
    found_capacity = 0;
    found_clear = 0;
    found_empty = 0;
    found_ends_with = 0;
    found_equals = 0;
    found_len = 0;
    found_compare = 0;
    found_contains = 0;
    found_starts_with = 0;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for builtin string completion")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for builtin string completion")
        != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "string_member_completion_sample.cblc", source) != FT_SUCCESS
                ? FT_SUCCESS : FT_FAILURE,
            "CBL-C builtin string completion sample should remain incomplete while typing")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 3, 12, &completions),
            "CBL-C completion should succeed for builtin string member access") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < completions.count)
    {
        if (std::strncmp(completions.items[index].label, "append",
                sizeof(completions.items[index].label)) == 0)
            found_append = 1;
        if (std::strncmp(completions.items[index].label, "clear",
                sizeof(completions.items[index].label)) == 0)
            found_clear = 1;
        if (std::strncmp(completions.items[index].label, "capacity",
                sizeof(completions.items[index].label)) == 0)
            found_capacity = 1;
        if (std::strncmp(completions.items[index].label, "empty",
                sizeof(completions.items[index].label)) == 0)
            found_empty = 1;
        if (std::strncmp(completions.items[index].label, "ends_with",
                sizeof(completions.items[index].label)) == 0)
            found_ends_with = 1;
        if (std::strncmp(completions.items[index].label, "equals",
                sizeof(completions.items[index].label)) == 0)
            found_equals = 1;
        if (std::strncmp(completions.items[index].label, "compare",
                sizeof(completions.items[index].label)) == 0)
            found_compare = 1;
        if (std::strncmp(completions.items[index].label, "contains",
                sizeof(completions.items[index].label)) == 0)
            found_contains = 1;
        if (std::strncmp(completions.items[index].label, "len",
                sizeof(completions.items[index].label)) == 0)
            found_len = 1;
        if (std::strncmp(completions.items[index].label, "starts_with",
                sizeof(completions.items[index].label)) == 0)
            found_starts_with = 1;
        index += 1;
    }
    if (test_expect_int_equal(found_append, 1,
            "CBL-C completion should include builtin string append") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_len, 1,
            "CBL-C completion should include builtin string len") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_clear, 1,
            "CBL-C completion should include builtin string clear") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_capacity, 1,
            "CBL-C completion should include builtin string capacity") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_empty, 1,
            "CBL-C completion should include builtin string empty") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_ends_with, 1,
            "CBL-C completion should include builtin string ends_with") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_equals, 1,
            "CBL-C completion should include builtin string equals") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_compare, 1,
            "CBL-C completion should include builtin string compare") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_contains, 1,
            "CBL-C completion should include builtin string contains") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(found_starts_with, 1,
            "CBL-C completion should include builtin string starts_with") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

FT_TEST(test_cblc_frontend_complete_allows_private_members_on_this_inside_class)
{
    const char *source;
    t_cblc_frontend_analysis analysis;
    t_cblc_completion_list completions;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    void reset()\n"
        "    {\n"
        "        return;\n"
        "    }\n"
        "    public:\n"
        "    void expose()\n"
        "    {\n"
        "        this.\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    status = FT_FAILURE;
    if (test_expect_success(cblc_frontend_analysis_init(&analysis),
            "CBL-C frontend analysis should initialize for this member completion")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(cblc_completion_list_init(&completions),
            "CBL-C completion list should initialize for this member completion") != FT_SUCCESS)
    {
        cblc_frontend_analysis_dispose(&analysis);
        return (FT_FAILURE);
    }
    if (test_expect_success(cblc_frontend_analyze_document(&analysis,
                "this_member_completion_sample.cblc", source) != FT_SUCCESS
                ? FT_SUCCESS : FT_FAILURE,
            "CBL-C this member completion sample should remain incomplete while typing")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_frontend_complete(&analysis, 12, 14, &completions),
            "CBL-C completion should succeed inside a class while typing this member access")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_completion_list_dispose(&completions);
    cblc_frontend_analysis_dispose(&analysis);
    return (status);
}

const t_test_case *get_validation_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_validation_accepts_valid_cblc", test_transpiler_validation_accepts_valid_cblc},
        {"transpiler_validation_rejects_cblc_without_return", test_transpiler_validation_rejects_cblc_without_return},
        {"transpiler_validation_accepts_string_declaration", test_transpiler_validation_accepts_string_declaration},
        {"transpiler_validation_accepts_string_assignment_and_length_usage",
            test_transpiler_validation_accepts_string_assignment_and_length_usage},
        {"transpiler_validation_accepts_int_array_and_string_array_declarations",
            test_transpiler_validation_accepts_int_array_and_string_array_declarations},
        {"transpiler_validation_accepts_string_capacity_parentheses",
            test_transpiler_validation_accepts_string_capacity_parentheses},
        {"transpiler_validation_accepts_indexed_array_usage",
            test_transpiler_validation_accepts_indexed_array_usage},
        {"cblc_parse_translation_unit_records_imports", test_cblc_parse_translation_unit_records_imports},
        {"cblc_parse_translation_unit_records_copy_includes", test_cblc_parse_translation_unit_records_copy_includes},
        {"cblc_parse_translation_unit_tracks_multiple_functions",
            test_cblc_parse_translation_unit_tracks_multiple_functions},
        {"cblc_parse_translation_unit_accepts_functions_without_keyword",
            test_cblc_parse_translation_unit_accepts_functions_without_keyword},
        {"cblc_parse_translation_unit_accepts_struct_fields",
            test_cblc_parse_translation_unit_accepts_struct_fields},
        {"cblc_parse_translation_unit_accepts_nested_struct_fields",
            test_cblc_parse_translation_unit_accepts_nested_struct_fields},
        {"cblc_generate_cobol_emits_string_group", test_cblc_generate_cobol_emits_string_group},
        {"cblc_generate_cobol_emits_struct_groups",
            test_cblc_generate_cobol_emits_struct_groups},
        {"cblc_generate_cobol_emits_nested_struct_groups",
            test_cblc_generate_cobol_emits_nested_struct_groups},
        {"cblc_generate_cobol_emits_copy_includes", test_cblc_generate_cobol_emits_copy_includes},
        {"cblc_generate_cobol_emits_multiple_paragraphs",
            test_cblc_generate_cobol_emits_multiple_paragraphs},
        {"cblc_generate_cobol_emits_perform_and_call_statements",
            test_cblc_generate_cobol_emits_perform_and_call_statements},
        {"cblc_generate_cobol_handles_string_assignments_and_length_computations",
            test_cblc_generate_cobol_handles_string_assignments_and_length_computations},
        {"cblc_generate_cobol_handles_multiplication_and_division",
            test_cblc_generate_cobol_handles_multiplication_and_division},
        {"cblc_generate_c_emits_struct_types_and_field_access",
            test_cblc_generate_c_emits_struct_types_and_field_access},
        {"cblc_generate_c_emits_nested_struct_types_and_field_access",
            test_cblc_generate_c_emits_nested_struct_types_and_field_access},
        {"cblc_generate_c_emits_array_declarations",
            test_cblc_generate_c_emits_array_declarations},
        {"cblc_generate_cobol_emits_array_declarations",
            test_cblc_generate_cobol_emits_array_declarations},
        {"cblc_generate_c_emits_indexed_array_access",
            test_cblc_generate_c_emits_indexed_array_access},
        {"cblc_generate_cobol_emits_indexed_array_access",
            test_cblc_generate_cobol_emits_indexed_array_access},
        {"cblc_generate_c_emits_variable_index_array_access",
            test_cblc_generate_c_emits_variable_index_array_access},
        {"cblc_generate_cobol_emits_variable_index_array_access",
            test_cblc_generate_cobol_emits_variable_index_array_access},
        {"cblc_parse_translation_unit_records_class_lifecycle_metadata",
            test_cblc_parse_translation_unit_records_class_lifecycle_metadata},
        {"cblc_parse_translation_unit_records_parameterized_class_lifecycle_and_methods",
            test_cblc_parse_translation_unit_records_parameterized_class_lifecycle_and_methods},
        {"cblc_parse_translation_unit_records_constructor_overloads_by_arity",
            test_cblc_parse_translation_unit_records_constructor_overloads_by_arity},
        {"cblc_parse_translation_unit_records_copy_constructor_overload",
            test_cblc_parse_translation_unit_records_copy_constructor_overload},
        {"cblc_generate_c_emits_automatic_class_lifecycle",
            test_cblc_generate_c_emits_automatic_class_lifecycle},
        {"cblc_generate_cobol_emits_automatic_class_lifecycle",
            test_cblc_generate_cobol_emits_automatic_class_lifecycle},
        {"cblc_generate_c_emits_custom_class_lifecycle_body",
            test_cblc_generate_c_emits_custom_class_lifecycle_body},
        {"cblc_generate_cobol_emits_custom_class_lifecycle_body",
            test_cblc_generate_cobol_emits_custom_class_lifecycle_body},
        {"cblc_generate_c_emits_local_class_lifecycle",
            test_cblc_generate_c_emits_local_class_lifecycle},
        {"cblc_generate_cobol_emits_local_class_lifecycle",
            test_cblc_generate_cobol_emits_local_class_lifecycle},
        {"cblc_generate_c_emits_nested_block_local_class_cleanup_order",
            test_cblc_generate_c_emits_nested_block_local_class_cleanup_order},
        {"cblc_generate_cobol_emits_nested_block_local_class_cleanup_order",
            test_cblc_generate_cobol_emits_nested_block_local_class_cleanup_order},
        {"cblc_parse_translation_unit_registers_builtin_string_class",
            test_cblc_parse_translation_unit_registers_builtin_string_class},
        {"cblc_generate_c_emits_local_string_lifecycle",
            test_cblc_generate_c_emits_local_string_lifecycle},
        {"cblc_parse_translation_unit_accepts_builtin_string_methods",
            test_cblc_parse_translation_unit_accepts_builtin_string_methods},
        {"cblc_generate_c_emits_builtin_string_methods",
            test_cblc_generate_c_emits_builtin_string_methods},
        {"cblc_generate_cobol_emits_builtin_string_methods",
            test_cblc_generate_cobol_emits_builtin_string_methods},
        {"cblc_parse_translation_unit_records_class_methods",
            test_cblc_parse_translation_unit_records_class_methods},
        {"cblc_parse_translation_unit_records_struct_return_type",
            test_cblc_parse_translation_unit_records_struct_return_type},
        {"cblc_parse_translation_unit_records_struct_returning_method",
            test_cblc_parse_translation_unit_records_struct_returning_method},
        {"cblc_generate_c_emits_class_methods",
            test_cblc_generate_c_emits_class_methods},
        {"cblc_generate_c_and_cobol_emit_parameterized_class_lifecycle_and_methods",
            test_cblc_generate_c_and_cobol_emit_parameterized_class_lifecycle_and_methods},
        {"cblc_generate_c_and_cobol_emit_constructor_overloads_by_arity",
            test_cblc_generate_c_and_cobol_emit_constructor_overloads_by_arity},
        {"cblc_generate_c_and_cobol_emit_copy_constructor_overload",
            test_cblc_generate_c_and_cobol_emit_copy_constructor_overload},
        {"cblc_generate_c_emits_struct_returning_function_and_call_assignment",
            test_cblc_generate_c_emits_struct_returning_function_and_call_assignment},
        {"cblc_generate_c_and_cobol_emit_struct_returning_method_call_assignment",
            test_cblc_generate_c_and_cobol_emit_struct_returning_method_call_assignment},
        {"cblc_generate_cobol_emits_class_methods",
            test_cblc_generate_cobol_emits_class_methods},
        {"cblc_generate_cobol_emits_struct_returning_function_and_call_assignment",
            test_cblc_generate_cobol_emits_struct_returning_function_and_call_assignment},
        {"cblc_parse_translation_unit_rejects_private_field_access_outside_class",
            test_cblc_parse_translation_unit_rejects_private_field_access_outside_class},
        {"cblc_parse_translation_unit_rejects_private_method_call_outside_class",
            test_cblc_parse_translation_unit_rejects_private_method_call_outside_class},
        {"cblc_parse_translation_unit_accepts_public_members_outside_class",
            test_cblc_parse_translation_unit_accepts_public_members_outside_class},
        {"cblc_parse_translation_unit_accepts_private_access_within_class_methods",
            test_cblc_parse_translation_unit_accepts_private_access_within_class_methods},
        {"cblc_generate_c_emits_constructor_initializer_list_in_declaration_order",
            test_cblc_generate_c_emits_constructor_initializer_list_in_declaration_order},
        {"cblc_generate_c_parses_const_member_initializer_list",
            test_cblc_generate_c_parses_const_member_initializer_list},
        {"cblc_parse_translation_unit_accepts_const_member_initialized_once_in_constructor_body",
            test_cblc_parse_translation_unit_accepts_const_member_initialized_once_in_constructor_body},
        {"cblc_parse_translation_unit_rejects_const_member_reassignment_in_constructor",
            test_cblc_parse_translation_unit_rejects_const_member_reassignment_in_constructor},
        {"cblc_parse_translation_unit_rejects_const_member_assignment_outside_constructor",
            test_cblc_parse_translation_unit_rejects_const_member_assignment_outside_constructor},
        {"cblc_parse_translation_unit_rejects_missing_const_member_initialization",
            test_cblc_parse_translation_unit_rejects_missing_const_member_initialization},
        {"cblc_resolve_calls_reports_missing_function",
            test_cblc_resolve_calls_reports_missing_function},
        {"transpiler_validation_accepts_valid_cobol", test_transpiler_validation_accepts_valid_cobol},
        {"transpiler_validation_accepts_working_storage_program", test_transpiler_validation_accepts_working_storage_program},
        {"transpiler_validation_rejects_invalid_cobol", test_transpiler_validation_rejects_invalid_cobol},
        {"lexer_token_get_span_reports_expected_columns",
            test_lexer_token_get_span_reports_expected_columns},
        {"ast_node_get_span_uses_node_token", test_ast_node_get_span_uses_node_token},
        {"cblc_frontend_analysis_reports_parse_diagnostic",
            test_cblc_frontend_analysis_reports_parse_diagnostic},
        {"cblc_frontend_collect_document_symbols_returns_types_functions_and_data",
            test_cblc_frontend_collect_document_symbols_returns_types_functions_and_data},
        {"cblc_frontend_find_definition_resolves_data_item_reference",
            test_cblc_frontend_find_definition_resolves_data_item_reference},
        {"cblc_frontend_find_definition_resolves_function_call_target",
            test_cblc_frontend_find_definition_resolves_function_call_target},
        {"cblc_frontend_get_hover_reports_variable_summary",
            test_cblc_frontend_get_hover_reports_variable_summary},
        {"cblc_frontend_get_hover_reports_class_summary",
            test_cblc_frontend_get_hover_reports_class_summary},
        {"cblc_frontend_find_references_collects_declaration_and_uses",
            test_cblc_frontend_find_references_collects_declaration_and_uses},
        {"cblc_frontend_collect_semantic_tokens_classifies_core_tokens",
            test_cblc_frontend_collect_semantic_tokens_classifies_core_tokens},
        {"cblc_frontend_complete_returns_keywords_and_symbols",
            test_cblc_frontend_complete_returns_keywords_and_symbols},
        {"cblc_frontend_complete_filters_function_local_scope",
            test_cblc_frontend_complete_filters_function_local_scope},
        {"cblc_frontend_complete_returns_public_members_only",
            test_cblc_frontend_complete_returns_public_members_only},
        {"cblc_frontend_complete_returns_std_namespace_functions",
            test_cblc_frontend_complete_returns_std_namespace_functions},
        {"cblc_frontend_complete_returns_builtin_string_members",
            test_cblc_frontend_complete_returns_builtin_string_members}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
FT_TEST(test_cblc_generate_c_emits_builtin_string_equals_literal)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    greeting = \"HI\";\n"
        "    total = greeting.equals(\"HI\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string literal equals C sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "builtin string literal equals C sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c,
            "total = cblc_string_equals(main__greeting_buf, main__greeting_len, \"HI\", cblc_string_length(\"HI\"));",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit builtin string literal equals helper call\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_builtin_string_equals_literal)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    greeting = \"HI\";\n"
        "    total = greeting.equals(\"HI\");\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "builtin string literal equals COBOL sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "builtin string literal equals COBOL sample should generate") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "IF MAIN-GREETING-LEN = 2 AND MAIN-GREETING-BUF(1:MAIN-GREETING-LEN) = \"HI\"",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin string literal equals comparison\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}
