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

FT_TEST(test_transpiler_validation_rejects_mismatched_struct_pointer_cast_assignment)
{
    const char *source;

    source = "struct Left\n"
        "{\n"
        "    int value;\n"
        "};\n"
        "struct Right\n"
        "{\n"
        "    int value;\n"
        "};\n"
        "void *raw;\n"
        "Left *left;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(sizeof(Right));\n"
        "    left = (Right *)raw;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched struct pointer cast assignments\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_mismatched_struct_pointer_cast_argument)
{
    const char *source;

    source = "struct Left\n"
        "{\n"
        "    int value;\n"
        "};\n"
        "struct Right\n"
        "{\n"
        "    int value;\n"
        "};\n"
        "void consume(Left *item)\n"
        "{\n"
        "    return;\n"
        "}\n"
        "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(sizeof(Right));\n"
        "    consume((Right *)raw);\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched struct pointer cast arguments\n");
        return (FT_FAILURE);
    }
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

FT_TEST(test_transpiler_validation_rejects_mismatched_struct_pointer_deref_copy)
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
        "Other other;\n"
        "Other *other_pointer;\n"
        "void main()\n"
        "{\n"
        "    other_pointer = &other;\n"
        "    point = *other_pointer;\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched struct pointer dereference copies\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_rejects_mismatched_struct_pointer_indexed_copy)
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
        "Other *items;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(2 * sizeof(Other));\n"
        "    point = items[1];\n"
        "    return;\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        std::printf("Assertion failed: validator should reject mismatched indexed struct pointer copies\n");
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
