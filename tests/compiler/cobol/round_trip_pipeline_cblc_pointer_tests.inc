FT_TEST(test_cblc_inline_void_pointer_alloc_free_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(16);\n"
        "    std::free(raw);\n"
        "    display(\"PTR ALLOC OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_alloc_free_generated", "PTR ALLOC OK\n"));
}

FT_TEST(test_cblc_inline_int_pointer_index_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int result;\n"
        "int slot;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(12);\n"
        "    slot = 1;\n"
        "    items[0] = 42;\n"
        "    items[slot] = 77;\n"
        "    result = items[slot];\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_int_pointer_index_generated", "+000000077\n"));
}

FT_TEST(test_cblc_inline_char_pointer_index_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *buffer;\n"
        "char letter;\n"
        "int slot;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    slot = 1;\n"
        "    buffer[slot] = 'B';\n"
        "    letter = buffer[slot];\n"
        "    display(letter);\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_index_generated", "B\n"));
}

FT_TEST(test_cblc_inline_pointer_arithmetic_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *rover;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(12);\n"
        "    items[0] = 21;\n"
        "    items[1] = 84;\n"
        "    rover = items + 1;\n"
        "    result = *rover;\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_arithmetic_generated", "+000000084\n"));
}

FT_TEST(test_cblc_inline_address_of_scalar_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "int *items;\n"
        "void main()\n"
        "{\n"
        "    score = 52;\n"
        "    items = &score;\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    *items = 99;\n"
        "    display(score);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_address_of_scalar_generated", "+000000052\n+000000099\n"));
}

FT_TEST(test_cblc_inline_pointer_return_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "int *items;\n"
        "int *pick()\n"
        "{\n"
        "    return &score;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    score = 41;\n"
        "    items = pick();\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_return_generated", "+000000041\n"));
}

FT_TEST(test_cblc_inline_void_pointer_to_int_pointer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void *raw;\n"
        "int *items;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(4);\n"
        "    items = raw;\n"
        "    *items = 33;\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_to_int_pointer_generated", "+000000033\n"));
}

FT_TEST(test_cblc_inline_int_pointer_to_void_pointer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void *raw;\n"
        "int *items;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(4);\n"
        "    raw = items;\n"
        "    *items = 44;\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_int_to_void_pointer_generated", "+000000044\n"));
}

FT_TEST(test_cblc_inline_pointer_subtraction_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *rover;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(12);\n"
        "    items[0] = 11;\n"
        "    items[1] = 22;\n"
        "    items[2] = 33;\n"
        "    rover = items + 2;\n"
        "    rover = rover - 1;\n"
        "    result = *rover;\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_subtraction_generated", "+000000022\n"));
}

FT_TEST(test_cblc_inline_pointer_variable_offset_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *rover;\n"
        "int result;\n"
        "int slot;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(12);\n"
        "    slot = 2;\n"
        "    items[0] = 11;\n"
        "    items[1] = 22;\n"
        "    items[slot] = 99;\n"
        "    rover = items + slot;\n"
        "    result = *rover;\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_variable_offset_generated", "+000000099\n"));
}

FT_TEST(test_cblc_inline_char_pointer_dereference_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *buffer;\n"
        "char letter;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(2);\n"
        "    *buffer = 'Q';\n"
        "    letter = *buffer;\n"
        "    display(letter);\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_dereference_generated", "Q\n"));
}

FT_TEST(test_cblc_inline_address_of_char_scalar_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char letter;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    letter = 'A';\n"
        "    buffer = &letter;\n"
        "    *buffer = 'Z';\n"
        "    display(letter);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_address_of_char_scalar_generated", "Z\n"));
}

FT_TEST(test_cblc_inline_local_pointer_declaration_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    int *local_item;\n"
        "    score = 58;\n"
        "    local_item = &score;\n"
        "    result = *local_item;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_local_pointer_declaration_generated", "+000000058\n"));
}

FT_TEST(test_cblc_inline_pointer_parameter_return_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "int *items;\n"
        "int *rover;\n"
        "int *identity(int *input)\n"
        "{\n"
        "    return input;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    score = 66;\n"
        "    items = &score;\n"
        "    rover = identity(items);\n"
        "    result = *rover;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_parameter_return_generated", "+000000066\n"));
}

FT_TEST(test_cblc_inline_realloc_pointer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(4);\n"
        "    raw = std::realloc(raw, 8);\n"
        "    std::free(raw);\n"
        "    display(\"REALLOC OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_realloc_pointer_generated", "REALLOC OK\n"));
}

FT_TEST(test_cblc_inline_null_pointer_assignment_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = NULL;\n"
        "    display(\"NULL PTR OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_null_pointer_assignment_generated", "NULL PTR OK\n"));
}

FT_TEST(test_cblc_inline_pointer_offset_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *rover;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(12);\n"
        "    rover = items + 1;\n"
        "    *rover = 123;\n"
        "    result = items[1];\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_offset_write_generated", "+000000123\n"));
}

FT_TEST(test_cblc_inline_pointer_argument_mutates_scalar_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "void store(int *target)\n"
        "{\n"
        "    *target = 77;\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    store(&score);\n"
        "    display(score);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_argument_mutates_scalar_generated", "+000000077\n"));
}

FT_TEST(test_cblc_inline_pointer_argument_returns_same_pointer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "int *items;\n"
        "int *pass(int *input)\n"
        "{\n"
        "    return input;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    score = 88;\n"
        "    items = pass(&score);\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_argument_returns_same_pointer_generated", "+000000088\n"));
}

FT_TEST(test_cblc_inline_char_pointer_arithmetic_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *buffer;\n"
        "char *cursor_item;\n"
        "char letter;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(3);\n"
        "    buffer[0] = 'A';\n"
        "    buffer[1] = 'C';\n"
        "    cursor_item = buffer + 1;\n"
        "    letter = *cursor_item;\n"
        "    display(letter);\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_arithmetic_generated", "C\n"));
}

FT_TEST(test_cblc_inline_char_pointer_return_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char letter;\n"
        "char result;\n"
        "char *buffer;\n"
        "char *select_letter()\n"
        "{\n"
        "    return &letter;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    letter = 'M';\n"
        "    buffer = select_letter();\n"
        "    result = *buffer;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_return_generated", "M\n"));
}

FT_TEST(test_cblc_inline_void_pointer_address_of_int_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "void *raw;\n"
        "int *items;\n"
        "void main()\n"
        "{\n"
        "    score = 91;\n"
        "    raw = &score;\n"
        "    items = raw;\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_address_of_int_generated", "+000000091\n"));
}

FT_TEST(test_cblc_inline_realloc_int_pointer_writes_after_resize_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(4);\n"
        "    items = std::realloc(items, 8);\n"
        "    items[1] = 64;\n"
        "    result = items[1];\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_realloc_int_pointer_writes_generated", "+000000064\n"));
}

FT_TEST(test_cblc_inline_pointer_null_after_free_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void *raw;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(4);\n"
        "    std::free(raw);\n"
        "    raw = NULL;\n"
        "    display(\"NULL AFTER FREE OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_null_after_free_generated", "NULL AFTER FREE OK\n"));
}

FT_TEST(test_cblc_inline_std_strlen_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_std_strlen_empty_literal_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_std_strlen_spaces_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"two words\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_std_strlen_spaces_literal_generated", "+000000009\n"));
}

FT_TEST(test_cblc_inline_std_strlen_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"hello world\");\n"
        "    if (total == 11) {\n"
        "        display(\"STRLEN OK\");\n"
        "    } else {\n"
        "        display(\"STRLEN BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_std_strlen_generated", "STRLEN OK\n"));
}

FT_TEST(test_cblc_inline_pointer_argument_mutates_allocated_element_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *target;\n"
        "int result;\n"
        "void store(int *slot)\n"
        "{\n"
        "    *slot = 111;\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(8);\n"
        "    target = items + 1;\n"
        "    store(target);\n"
        "    result = items[1];\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_argument_mutates_allocated_element_generated", "+000000111\n"));
}

FT_TEST(test_cblc_inline_char_pointer_argument_mutates_scalar_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char letter;\n"
        "void stamp(char *target)\n"
        "{\n"
        "    *target = 'R';\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    stamp(&letter);\n"
        "    display(letter);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_argument_mutates_scalar_generated", "R\n"));
}

FT_TEST(test_cblc_inline_pointer_variable_subtraction_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *rover;\n"
        "int result;\n"
        "int slot;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(12);\n"
        "    slot = 2;\n"
        "    items[0] = 31;\n"
        "    items[1] = 62;\n"
        "    items[2] = 93;\n"
        "    rover = items + slot;\n"
        "    rover = rover - slot;\n"
        "    result = *rover;\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_variable_subtraction_generated", "+000000031\n"));
}

FT_TEST(test_cblc_inline_local_char_pointer_declaration_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char letter;\n"
        "char result;\n"
        "void main()\n"
        "{\n"
        "    char *local_buffer;\n"
        "    letter = 'L';\n"
        "    local_buffer = &letter;\n"
        "    result = *local_buffer;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_local_char_pointer_declaration_generated", "L\n"));
}

FT_TEST(test_cblc_inline_string_clear_after_assignment_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"CLEAR ME\";\n"
        "    label.clear();\n"
        "    total = label.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_clear_after_assignment_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_string_copy_constructor_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(\"COPYME\");\n"
        "    string duplicate(label);\n"
        "    total = duplicate.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_copy_constructor_len_generated", "+000000006\n"));
}

FT_TEST(test_cblc_inline_class_multiple_method_calls_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    void reset()\n"
        "    {\n"
        "        value = 1;\n"
        "        return;\n"
        "    }\n"
        "    void add(int delta)\n"
        "    {\n"
        "        value = value + delta;\n"
        "        return;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.reset();\n"
        "    counter.add(4);\n"
        "    counter.add(5);\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_multiple_method_calls_generated", "+000000010\n"));
}

FT_TEST(test_cblc_inline_class_const_member_arithmetic_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Limit\n"
        "{\n"
        "    private:\n"
        "    const int base;\n"
        "    public:\n"
        "    Limit() : base(40)\n"
        "    {\n"
        "    }\n"
        "    int plus(int delta)\n"
        "    {\n"
        "        return base + delta;\n"
        "    }\n"
        "};\n"
        "Limit limit;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = limit.plus(2);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_const_member_arithmetic_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_nested_struct_numeric_fields_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner left;\n"
        "    Inner right;\n"
        "};\n"
        "Outer record;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    record.left.amount = 14;\n"
        "    record.right.amount = 28;\n"
        "    total = record.left.amount + record.right.amount;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_struct_numeric_fields_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_function_return_used_in_expression_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int base()\n"
        "{\n"
        "    return 20;\n"
        "}\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = base() + 22;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_function_return_expression_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_address_of_array_element_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int numbers[3];\n"
        "int *item;\n"
        "void main()\n"
        "{\n"
        "    numbers[1] = 12;\n"
        "    item = &numbers[1];\n"
        "    *item = 37;\n"
        "    display(numbers[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_address_of_array_element_generated", "+000000037\n"));
}

FT_TEST(test_cblc_inline_address_of_struct_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Point\n"
        "{\n"
        "    int x;\n"
        "};\n"
        "Point point;\n"
        "int *item;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    point.x = 18;\n"
        "    item = &point.x;\n"
        "    *item = 54;\n"
        "    result = point.x;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_address_of_struct_field_generated", "+000000054\n"));
}

FT_TEST(test_cblc_inline_typed_pointer_null_assignment_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(4);\n"
        "    std::free(items);\n"
        "    items = NULL;\n"
        "    display(\"TYPED NULL OK\");\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_typed_pointer_null_assignment_generated", "TYPED NULL OK\n"));
}

FT_TEST(test_cblc_inline_local_pointer_allocation_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int result;\n"
        "void main()\n"
        "{\n"
        "    int *items;\n"
        "    items = std::malloc(4);\n"
        "    *items = 72;\n"
        "    result = *items;\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_local_pointer_allocation_generated", "+000000072\n"));
}

FT_TEST(test_cblc_inline_char_pointer_variable_subtraction_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *buffer;\n"
        "char *cursor_item;\n"
        "char letter;\n"
        "int slot;\n"
        "void main()\n"
        "{\n"
        "    buffer = std::malloc(4);\n"
        "    slot = 2;\n"
        "    buffer[0] = 'A';\n"
        "    buffer[1] = 'B';\n"
        "    buffer[2] = 'C';\n"
        "    cursor_item = buffer + slot;\n"
        "    cursor_item = cursor_item - 1;\n"
        "    letter = *cursor_item;\n"
        "    display(letter);\n"
        "    std::free(buffer);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_variable_subtraction_generated", "B\n"));
}

FT_TEST(test_cblc_inline_empty_string_literal_constructor_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(\"\");\n"
        "    total = label.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_empty_string_literal_constructor_len_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_string_assignment_overwrite_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"LONGER\";\n"
        "    label = \"END\";\n"
        "    total = label.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_assignment_overwrite_len_generated", "+000000003\n"));
}

FT_TEST(test_cblc_inline_parameterized_function_return_expression_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int add(int left, int right)\n"
        "{\n"
        "    return left + right;\n"
        "}\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = add(19, 23);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_parameterized_function_return_expression_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_nested_struct_string_overwrite_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Address\n"
        "{\n"
        "    string city(16);\n"
        "};\n"
        "struct Person\n"
        "{\n"
        "    Address address;\n"
        "};\n"
        "Person person;\n"
        "void main()\n"
        "{\n"
        "    person.address.city = \"BRUGES\";\n"
        "    person.address.city = \"GHENT\";\n"
        "    display(person.address.city);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_struct_string_overwrite_generated", "GHENT\n"));
}

FT_TEST(test_cblc_inline_struct_string_array_assign_display_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string labels[2](8);\n"
        "};\n"
        "Record record;\n"
        "void main()\n"
        "{\n"
        "    record.labels[0] = \"FIRST\";\n"
        "    record.labels[1] = \"SECOND\";\n"
        "    display(record.labels[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_array_assign_display_generated", "SECOND\n"));
}

FT_TEST(test_cblc_inline_struct_string_array_len_after_overwrite_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string labels[2](4);\n"
        "};\n"
        "Record record;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    record.labels[0] = \"A\";\n"
        "    record.labels[0] = \"LONGER\";\n"
        "    total = record.labels[0].len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_array_len_after_overwrite_generated", "+000000006\n"));
}

FT_TEST(test_cblc_inline_nested_struct_string_array_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Group\n"
        "{\n"
        "    string names[2](8);\n"
        "};\n"
        "struct Holder\n"
        "{\n"
        "    Group group;\n"
        "};\n"
        "Holder holder;\n"
        "void main()\n"
        "{\n"
        "    holder.group.names[0] = \"ALPHA\";\n"
        "    holder.group.names[1] = \"BETA\";\n"
        "    display(holder.group.names[0]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_struct_string_array_generated", "ALPHA\n"));
}

FT_TEST(test_cblc_inline_class_constructor_string_array_member_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Table\n"
        "{\n"
        "    private:\n"
        "    string cells[2](8);\n"
        "    public:\n"
        "    Table()\n"
        "    {\n"
        "        cells[0] = \"LEFT\";\n"
        "        cells[1] = \"RIGHT\";\n"
        "    }\n"
        "    void show()\n"
        "    {\n"
        "        display(cells[1]);\n"
        "    }\n"
        "};\n"
        "Table table;\n"
        "void main()\n"
        "{\n"
        "    table.show();\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_constructor_string_array_member_generated", "RIGHT\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_assign_display_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[3](8);\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"ONE\";\n"
        "    slots[1] = \"TWO\";\n"
        "    slots[2] = \"THREE\";\n"
        "    display(slots[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_assign_display_generated", "TWO\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_variable_index_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[3](8);\n"
        "int slot_index;\n"
        "void main()\n"
        "{\n"
        "    slot_index = 2;\n"
        "    slots[slot_index] = \"BLUE\";\n"
        "    display(slots[slot_index]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_variable_index_generated", "BLUE\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_len_after_growth_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](2);\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    slots[1] = \"A\";\n"
        "    slots[1] = \"EXPANDED\";\n"
        "    total = slots[1].len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_len_after_growth_generated", "+000000008\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_append_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](2);\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"AB\";\n"
        "    slots[0].append(\"CD\");\n"
        "    display(slots[0]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_append_literal_generated", "ABCD\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_clear_empty_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](8);\n"
        "int is_empty;\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"DATA\";\n"
        "    slots[0].clear();\n"
        "    is_empty = slots[0].empty();\n"
        "    if (is_empty == 1) {\n"
        "        display(\"CLEAR OK\");\n"
        "    } else {\n"
        "        display(\"CLEAR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_clear_empty_generated", "CLEAR OK\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_compare_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](8);\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"ALPHA\";\n"
        "    slots[1] = \"ALPHA\";\n"
        "    result = slots[0].compare(slots[1]);\n"
        "    if (result == 0) {\n"
        "        display(\"COMPARE ARRAY OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE ARRAY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_compare_generated", "COMPARE ARRAY OK\n"));
}

FT_TEST(test_cblc_inline_struct_string_array_variable_index_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string labels[3](8);\n"
        "};\n"
        "Record record;\n"
        "int slot_index;\n"
        "void main()\n"
        "{\n"
        "    slot_index = 1;\n"
        "    record.labels[slot_index] = \"BETA\";\n"
        "    display(record.labels[slot_index]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_array_variable_index_generated", "BETA\n"));
}

FT_TEST(test_cblc_inline_class_method_updates_string_array_slot_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Table\n"
        "{\n"
        "    private:\n"
        "    string cells[2](8);\n"
        "    public:\n"
        "    void update()\n"
        "    {\n"
        "        cells[0] = \"FRESH\";\n"
        "    }\n"
        "    void show()\n"
        "    {\n"
        "        display(cells[0]);\n"
        "    }\n"
        "};\n"
        "Table table;\n"
        "void main()\n"
        "{\n"
        "    table.update();\n"
        "    table.show();\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_updates_string_array_slot_generated", "FRESH\n"));
}

FT_TEST(test_cblc_inline_class_method_string_array_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Table\n"
        "{\n"
        "    private:\n"
        "    string cells[2](4);\n"
        "    public:\n"
        "    Table()\n"
        "    {\n"
        "        cells[1] = \"VALUE\";\n"
        "    }\n"
        "    int size()\n"
        "    {\n"
        "        return cells[1].len();\n"
        "    }\n"
        "};\n"
        "Table table;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = table.size();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_string_array_len_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_nested_class_struct_string_array_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Packet\n"
        "{\n"
        "    string parts[2](4);\n"
        "};\n"
        "class Holder\n"
        "{\n"
        "    private:\n"
        "    Packet packet;\n"
        "    public:\n"
        "    void build()\n"
        "    {\n"
        "        packet.parts[0] = \"AB\";\n"
        "        packet.parts[0].append(\"CD\");\n"
        "    }\n"
        "    void show()\n"
        "    {\n"
        "        display(packet.parts[0]);\n"
        "    }\n"
        "};\n"
        "Holder holder;\n"
        "void main()\n"
        "{\n"
        "    holder.build();\n"
        "    holder.show();\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_class_struct_string_array_append_generated", "ABCD\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_append_other_slot_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](4);\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"AB\";\n"
        "    slots[1] = \"CD\";\n"
        "    slots[0].append(slots[1]);\n"
        "    display(slots[0]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_append_other_slot_generated", "ABCD\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_contains_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](8);\n"
        "int found;\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"ALPHA\";\n"
        "    found = slots[0].contains(\"LPH\");\n"
        "    if (found == 1) {\n"
        "        display(\"ARRAY CONTAINS OK\");\n"
        "    } else {\n"
        "        display(\"ARRAY CONTAINS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_contains_literal_generated", "ARRAY CONTAINS OK\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_starts_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](8);\n"
        "int found;\n"
        "void main()\n"
        "{\n"
        "    slots[1] = \"PREFIX\";\n"
        "    found = slots[1].starts_with(\"PRE\");\n"
        "    if (found == 1) {\n"
        "        display(\"ARRAY STARTS OK\");\n"
        "    } else {\n"
        "        display(\"ARRAY STARTS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_starts_literal_generated", "ARRAY STARTS OK\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_ends_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](8);\n"
        "int found;\n"
        "void main()\n"
        "{\n"
        "    slots[1] = \"SUFFIX\";\n"
        "    found = slots[1].ends_with(\"FIX\");\n"
        "    if (found == 1) {\n"
        "        display(\"ARRAY ENDS OK\");\n"
        "    } else {\n"
        "        display(\"ARRAY ENDS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_ends_literal_generated", "ARRAY ENDS OK\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_empty_unassigned_slot_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](8);\n"
        "int is_empty;\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"USED\";\n"
        "    is_empty = slots[1].empty();\n"
        "    if (is_empty == 1) {\n"
        "        display(\"EMPTY SLOT OK\");\n"
        "    } else {\n"
        "        display(\"EMPTY SLOT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_empty_unassigned_slot_generated", "EMPTY SLOT OK\n"));
}
