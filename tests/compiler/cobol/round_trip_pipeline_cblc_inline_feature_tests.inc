FT_TEST(test_cblc_inline_top_level_string_array_clear_then_assign_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[2](4);\n"
        "void main()\n"
        "{\n"
        "    slots[0] = \"OLD\";\n"
        "    slots[0].clear();\n"
        "    slots[0] = \"NEWVALUE\";\n"
        "    display(slots[0]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_clear_then_assign_generated", "NEWVALUE\n"));
}

FT_TEST(test_cblc_inline_top_level_string_array_variable_index_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "string slots[3](4);\n"
        "int slot_index;\n"
        "void main()\n"
        "{\n"
        "    slot_index = 2;\n"
        "    slots[slot_index] = \"GO\";\n"
        "    slots[slot_index].append(\"AL\");\n"
        "    display(slots[slot_index]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_top_level_string_array_variable_index_append_generated", "GOAL\n"));
}

FT_TEST(test_cblc_inline_struct_string_array_append_other_slot_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string labels[2](4);\n"
        "};\n"
        "Record record;\n"
        "void main()\n"
        "{\n"
        "    record.labels[0] = \"LEFT\";\n"
        "    record.labels[1] = \"SIDE\";\n"
        "    record.labels[0].append(record.labels[1]);\n"
        "    display(record.labels[0]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_array_append_other_slot_generated", "LEFTSIDE\n"));
}

FT_TEST(test_cblc_inline_class_method_string_array_contains_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Table\n"
        "{\n"
        "    private:\n"
        "    string cells[2](8);\n"
        "    public:\n"
        "    Table()\n"
        "    {\n"
        "        cells[0] = \"SEARCH\";\n"
        "    }\n"
        "    int has_arc()\n"
        "    {\n"
        "        return cells[0].contains(\"ARC\");\n"
        "    }\n"
        "};\n"
        "Table table;\n"
        "int found;\n"
        "void main()\n"
        "{\n"
        "    found = table.has_arc();\n"
        "    if (found == 1) {\n"
        "        display(\"CLASS ARRAY CONTAINS OK\");\n"
        "    } else {\n"
        "        display(\"CLASS ARRAY CONTAINS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_string_array_contains_generated",
            "CLASS ARRAY CONTAINS OK\n"));
}

FT_TEST(test_cblc_inline_nested_struct_string_array_variable_index_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Group\n"
        "{\n"
        "    string names[3](4);\n"
        "};\n"
        "struct Holder\n"
        "{\n"
        "    Group group;\n"
        "};\n"
        "Holder holder;\n"
        "int slot_index;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    slot_index = 2;\n"
        "    holder.group.names[slot_index] = \"OMEGA\";\n"
        "    total = holder.group.names[slot_index].len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_struct_string_array_variable_index_len_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_local_class_instance_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Box\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Box(int start)\n"
        "    {\n"
        "        value = start;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return value;\n"
        "    }\n"
        "};\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Box box(42);\n"
        "    total = box.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_local_class_instance_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_pointer_alias_after_arithmetic_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *rover;\n"
        "int *alias;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(8);\n"
        "    rover = items + 1;\n"
        "    alias = rover;\n"
        "    *alias = 55;\n"
        "    result = items[1];\n"
        "    display(result);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_alias_after_arithmetic_generated", "+000000055\n"));
}

FT_TEST(test_cblc_inline_pointer_reassign_between_scalars_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int first;\n"
        "int second;\n"
        "int *item;\n"
        "void main()\n"
        "{\n"
        "    item = &first;\n"
        "    *item = 12;\n"
        "    item = &second;\n"
        "    *item = 34;\n"
        "    display(first);\n"
        "    display(second);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_reassign_between_scalars_generated",
            "+000000012\n+000000034\n"));
}

FT_TEST(test_cblc_inline_void_pointer_round_trip_char_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char letter;\n"
        "char result;\n"
        "void *raw;\n"
        "char *buffer;\n"
        "void main()\n"
        "{\n"
        "    letter = 'A';\n"
        "    raw = &letter;\n"
        "    buffer = raw;\n"
        "    *buffer = 'Y';\n"
        "    result = letter;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_round_trip_char_generated", "Y\n"));
}

FT_TEST(test_cblc_inline_address_of_nested_struct_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner inner;\n"
        "};\n"
        "Outer record;\n"
        "int *item;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    record.inner.amount = 6;\n"
        "    item = &record.inner.amount;\n"
        "    *item = 42;\n"
        "    result = record.inner.amount;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_address_of_nested_struct_field_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_address_of_array_variable_index_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int numbers[3];\n"
        "int slot;\n"
        "int *item;\n"
        "void main()\n"
        "{\n"
        "    slot = 2;\n"
        "    numbers[slot] = 17;\n"
        "    item = &numbers[slot];\n"
        "    *item = 39;\n"
        "    display(numbers[slot]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_address_of_array_variable_index_generated", "+000000039\n"));
}

FT_TEST(test_cblc_inline_local_void_pointer_round_trip_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int result;\n"
        "int *item;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    score = 73;\n"
        "    raw = &score;\n"
        "    item = raw;\n"
        "    result = *item;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_local_void_pointer_round_trip_generated", "+000000073\n"));
}

FT_TEST(test_cblc_inline_std_strlen_symbols_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    total = std::strlen(\"A-B_C!\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_std_strlen_symbols_literal_generated", "+000000006\n"));
}

FT_TEST(test_cblc_inline_string_capacity_after_assignment_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"HI\";\n"
        "    total = label.capacity();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_after_assignment_generated", "+000000016\n"));
}

FT_TEST(test_cblc_inline_class_string_member_update_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Labeler\n"
        "{\n"
        "    private:\n"
        "    string label(12);\n"
        "    public:\n"
        "    Labeler() : label(\"EMPTY\")\n"
        "    {\n"
        "    }\n"
        "    void set()\n"
        "    {\n"
        "        label = \"READY\";\n"
        "        return;\n"
        "    }\n"
        "    void show()\n"
        "    {\n"
        "        display(label);\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Labeler labeler;\n"
        "void main()\n"
        "{\n"
        "    labeler.set();\n"
        "    labeler.show();\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_string_member_update_generated", "READY\n"));
}

FT_TEST(test_cblc_inline_class_method_argument_multiplication_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Accumulator\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Accumulator() : value(3)\n"
        "    {\n"
        "    }\n"
        "    int scale(int factor)\n"
        "    {\n"
        "        return value * factor;\n"
        "    }\n"
        "};\n"
        "Accumulator accumulator;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = accumulator.scale(7);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_argument_multiplication_generated", "+000000021\n"));
}

FT_TEST(test_cblc_inline_pointer_argument_reads_scalar_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int read_ptr(int *input)\n"
        "{\n"
        "    return *input;\n"
        "}\n"
        "int score;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    score = 47;\n"
        "    total = read_ptr(&score);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_argument_reads_scalar_generated", "+000000047\n"));
}

FT_TEST(test_cblc_inline_char_pointer_argument_reads_scalar_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char read_char(char *input)\n"
        "{\n"
        "    return *input;\n"
        "}\n"
        "char letter;\n"
        "char result;\n"
        "void main()\n"
        "{\n"
        "    letter = 'D';\n"
        "    result = read_char(&letter);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_argument_reads_scalar_generated", "D\n"));
}

FT_TEST(test_cblc_inline_pointer_argument_reads_allocated_offset_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int read_ptr(int *input)\n"
        "{\n"
        "    return *input;\n"
        "}\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    int *items;\n"
        "    int *target;\n"
        "    items = std::malloc(3);\n"
        "    items[0] = 12;\n"
        "    items[1] = 144;\n"
        "    target = items + 1;\n"
        "    total = read_ptr(target);\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_argument_reads_allocated_offset_generated", "+000000144\n"));
}

FT_TEST(test_cblc_inline_global_array_pointer_parameter_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void write_value(int *input)\n"
        "{\n"
        "    *input = 88;\n"
        "    return;\n"
        "}\n"
        "int numbers[3];\n"
        "void main()\n"
        "{\n"
        "    numbers[1] = 5;\n"
        "    write_value(&numbers[1]);\n"
        "    display(numbers[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_global_array_pointer_parameter_generated", "+000000088\n"));
}

FT_TEST(test_cblc_inline_string_assignment_chain_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"A\";\n"
        "    label = \"ABCDE\";\n"
        "    total = label.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_assignment_chain_len_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_string_append_literal_result_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string label(\"CO\");\n"
        "    label.append(\"BOL\");\n"
        "    display(label);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_literal_result_generated", "COBOL\n"));
}

FT_TEST(test_cblc_inline_class_method_uses_string_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Banner\n"
        "{\n"
        "    private:\n"
        "    string title(16);\n"
        "    public:\n"
        "    Banner() : title(\"READY\")\n"
        "    {\n"
        "    }\n"
        "    int size()\n"
        "    {\n"
        "        return title.len();\n"
        "    }\n"
        "};\n"
        "Banner banner;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = banner.size();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_uses_string_len_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_struct_string_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Item\n"
        "{\n"
        "    string name(12);\n"
        "};\n"
        "Item item;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    item.name = \"OMEGA\";\n"
        "    total = item.name.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_len_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_function_updates_global_array_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int numbers[2];\n"
        "void seed()\n"
        "{\n"
        "    numbers[1] = 24;\n"
        "    return;\n"
        "}\n"
        "void main()\n"
        "{\n"
        "    seed();\n"
        "    display(numbers[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_function_updates_global_array_generated", "+000000024\n"));
}

FT_TEST(test_cblc_inline_class_method_updates_struct_member_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct CounterState\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "class Counter\n"
        "{\n"
        "    private:\n"
        "    CounterState state;\n"
        "    public:\n"
        "    Counter()\n"
        "    {\n"
        "        state.amount = 6;\n"
        "    }\n"
        "    void add(int amount)\n"
        "    {\n"
        "        state.amount = state.amount + amount;\n"
        "        return;\n"
        "    }\n"
        "    int current()\n"
        "    {\n"
        "        return state.amount;\n"
        "    }\n"
        "};\n"
        "Counter counter;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    counter.add(9);\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_updates_struct_member_generated", "+000000015\n"));
}

FT_TEST(test_cblc_inline_void_pointer_parameter_reads_int_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int read_raw(void *raw)\n"
        "{\n"
        "    int *item;\n"
        "    item = raw;\n"
        "    return *item;\n"
        "}\n"
        "int score;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    score = 61;\n"
        "    total = read_raw(&score);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_parameter_reads_int_generated", "+000000061\n"));
}

FT_TEST(test_cblc_inline_pointer_parameter_reads_second_array_item_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int read_second(int *input)\n"
        "{\n"
        "    return input[1];\n"
        "}\n"
        "int numbers[3];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 3;\n"
        "    numbers[1] = 34;\n"
        "    total = read_second(numbers);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_parameter_reads_second_array_item_generated", "+000000034\n"));
}

FT_TEST(test_cblc_inline_pointer_parameter_updates_second_array_item_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void write_second(int *input)\n"
        "{\n"
        "    input[1] = 52;\n"
        "    return;\n"
        "}\n"
        "int numbers[3];\n"
        "void main()\n"
        "{\n"
        "    numbers[1] = 4;\n"
        "    write_second(numbers);\n"
        "    display(numbers[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_parameter_updates_second_array_item_generated", "+000000052\n"));
}

FT_TEST(test_cblc_inline_allocated_char_pointer_offset_read_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char result;\n"
        "void main()\n"
        "{\n"
        "    char *letters;\n"
        "    letters = std::malloc(3);\n"
        "    letters[0] = 'A';\n"
        "    letters[1] = 'Z';\n"
        "    result = *(letters + 1);\n"
        "    display(result);\n"
        "    std::free(letters);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_allocated_char_pointer_offset_read_generated", "Z\n"));
}

FT_TEST(test_cblc_inline_string_append_string_result_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    string suffix(8);\n"
        "    label = \"DATA\";\n"
        "    suffix = \"SET\";\n"
        "    label.append(suffix);\n"
        "    display(label);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_string_result_generated", "DATASET\n"));
}

FT_TEST(test_cblc_inline_string_contains_after_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"TRANS\";\n"
        "    label.append(\"PILE\");\n"
        "    total = label.contains(\"SPI\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_after_append_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_string_clear_then_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"REMOVE\";\n"
        "    label.clear();\n"
        "    total = label.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_clear_then_len_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_class_string_method_chain_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class TextBox\n"
        "{\n"
        "    private:\n"
        "    string text(16);\n"
        "    public:\n"
        "    TextBox() : text(\"BOX\")\n"
        "    {\n"
        "    }\n"
        "    void extend()\n"
        "    {\n"
        "        text.append(\"ED\");\n"
        "        return;\n"
        "    }\n"
        "    int size()\n"
        "    {\n"
        "        return text.len();\n"
        "    }\n"
        "};\n"
        "TextBox box;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    box.extend();\n"
        "    total = box.size();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_string_method_chain_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_class_method_accepts_struct_argument_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Score\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "class Calculator\n"
        "{\n"
        "    private:\n"
        "    int bonus;\n"
        "    public:\n"
        "    Calculator() : bonus(8)\n"
        "    {\n"
        "    }\n"
        "    int add(Score score)\n"
        "    {\n"
        "        return score.amount + bonus;\n"
        "    }\n"
        "};\n"
        "Calculator calculator;\n"
        "Score score;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    score.amount = 14;\n"
        "    total = calculator.add(score);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_accepts_struct_argument_generated", "+000000022\n"));
}

FT_TEST(test_cblc_inline_class_method_returns_struct_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct BoxState\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "class Box\n"
        "{\n"
        "    private:\n"
        "    BoxState state;\n"
        "    public:\n"
        "    Box()\n"
        "    {\n"
        "        state.amount = 31;\n"
        "    }\n"
        "    int read()\n"
        "    {\n"
        "        return state.amount;\n"
        "    }\n"
        "};\n"
        "Box box;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = box.read();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_returns_struct_field_generated", "+000000031\n"));
}

FT_TEST(test_cblc_inline_void_pointer_parameter_updates_int_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void update_raw(void *raw)\n"
        "{\n"
        "    int *item;\n"
        "    item = raw;\n"
        "    *item = 77;\n"
        "    return;\n"
        "}\n"
        "int score;\n"
        "void main()\n"
        "{\n"
        "    score = 11;\n"
        "    update_raw(&score);\n"
        "    display(score);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_parameter_updates_int_generated", "+000000077\n"));
}

FT_TEST(test_cblc_inline_allocated_void_pointer_round_trip_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    int *items;\n"
        "    raw = std::malloc(2);\n"
        "    items = raw;\n"
        "    items[1] = 93;\n"
        "    total = items[1];\n"
        "    display(total);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_allocated_void_pointer_round_trip_write_generated", "+000000093\n"));
}

FT_TEST(test_cblc_inline_pointer_offset_read_after_reassign_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int numbers[4];\n"
        "int *item;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 4;\n"
        "    numbers[2] = 58;\n"
        "    item = numbers;\n"
        "    item = item + 2;\n"
        "    total = *item;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_offset_read_after_reassign_generated", "+000000058\n"));
}

FT_TEST(test_cblc_inline_char_array_pointer_parameter_reads_second_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char read_second(char *input)\n"
        "{\n"
        "    return input[1];\n"
        "}\n"
        "char letters[3];\n"
        "char result;\n"
        "void main()\n"
        "{\n"
        "    letters[0] = 'L';\n"
        "    letters[1] = 'S';\n"
        "    result = read_second(letters);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_array_pointer_parameter_reads_second_generated", "S\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_after_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"CB\";\n"
        "    label.append(\"LC\");\n"
        "    total = label.starts_with(\"CB\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_with_after_append_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_after_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"CO\";\n"
        "    label.append(\"BOL\");\n"
        "    total = label.ends_with(\"BOL\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_with_after_append_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_string_compare_after_clear_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"TEMP\";\n"
        "    label.clear();\n"
        "    total = label.compare(\"\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_after_clear_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_class_method_string_contains_after_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Matcher\n"
        "{\n"
        "    private:\n"
        "    string text(16);\n"
        "    public:\n"
        "    Matcher() : text(\"CO\")\n"
        "    {\n"
        "    }\n"
        "    int match()\n"
        "    {\n"
        "        text.append(\"BOL\");\n"
        "        return text.contains(\"OB\");\n"
        "    }\n"
        "};\n"
        "Matcher matcher;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = matcher.match();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_string_contains_after_append_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_class_method_accepts_pointer_argument_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Writer\n"
        "{\n"
        "    private:\n"
        "    int amount;\n"
        "    public:\n"
        "    Writer() : amount(64)\n"
        "    {\n"
        "    }\n"
        "    void write(int *target)\n"
        "    {\n"
        "        *target = amount;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Writer writer;\n"
        "int result;\n"
        "void main()\n"
        "{\n"
        "    result = 0;\n"
        "    writer.write(&result);\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_accepts_pointer_argument_generated", "+000000064\n"));
}

FT_TEST(test_cblc_inline_struct_string_contains_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Message\n"
        "{\n"
        "    string text(16);\n"
        "};\n"
        "Message message;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    message.text = \"FEATURE\";\n"
        "    total = message.text.contains(\"AT\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_contains_literal_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_void_pointer_parameter_updates_char_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void update_raw(void *raw)\n"
        "{\n"
        "    char *item;\n"
        "    item = raw;\n"
        "    *item = 'Q';\n"
        "    return;\n"
        "}\n"
        "char letter;\n"
        "void main()\n"
        "{\n"
        "    letter = 'A';\n"
        "    update_raw(&letter);\n"
        "    display(letter);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_parameter_updates_char_generated", "Q\n"));
}

FT_TEST(test_cblc_inline_pointer_function_returns_offset_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *second_item(int *input)\n"
        "{\n"
        "    return input + 1;\n"
        "}\n"
        "int numbers[3];\n"
        "int *item;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[1] = 46;\n"
        "    item = second_item(numbers);\n"
        "    total = *item;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_function_returns_offset_generated", "+000000046\n"));
}

FT_TEST(test_cblc_inline_pointer_function_returns_allocated_offset_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *third_item(int *input)\n"
        "{\n"
        "    return input + 2;\n"
        "}\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    int *items;\n"
        "    int *target;\n"
        "    items = std::malloc(4);\n"
        "    items[2] = 81;\n"
        "    target = third_item(items);\n"
        "    total = *target;\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_function_returns_allocated_offset_generated", "+000000081\n"));
}

FT_TEST(test_cblc_inline_char_pointer_function_returns_offset_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *second_char(char *input)\n"
        "{\n"
        "    return input + 1;\n"
        "}\n"
        "char letters[3];\n"
        "char *item;\n"
        "char result;\n"
        "void main()\n"
        "{\n"
        "    letters[0] = 'A';\n"
        "    letters[1] = 'B';\n"
        "    item = second_char(letters);\n"
        "    result = *item;\n"
        "    display(result);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_function_returns_offset_generated", "B\n"));
}

FT_TEST(test_cblc_inline_nested_struct_pointer_parameter_update_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner inner;\n"
        "};\n"
        "void update(int *target)\n"
        "{\n"
        "    *target = 67;\n"
        "    return;\n"
        "}\n"
        "Outer record;\n"
        "void main()\n"
        "{\n"
        "    record.inner.amount = 3;\n"
        "    update(&record.inner.amount);\n"
        "    display(record.inner.amount);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_nested_struct_pointer_parameter_update_generated", "+000000067\n"));
}

FT_TEST(test_cblc_inline_struct_string_starts_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Message\n"
        "{\n"
        "    string text(16);\n"
        "};\n"
        "Message message;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    message.text = \"TRANSPILE\";\n"
        "    total = message.text.starts_with(\"TRAN\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_starts_with_literal_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_struct_string_ends_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Message\n"
        "{\n"
        "    string text(16);\n"
        "};\n"
        "Message message;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    message.text = \"TRANSPILE\";\n"
        "    total = message.text.ends_with(\"PILE\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_ends_with_literal_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_class_method_clears_string_member_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Cleaner\n"
        "{\n"
        "    private:\n"
        "    string text(12);\n"
        "    public:\n"
        "    Cleaner() : text(\"DIRTY\")\n"
        "    {\n"
        "    }\n"
        "    int clean()\n"
        "    {\n"
        "        text.clear();\n"
        "        return text.empty();\n"
        "    }\n"
        "};\n"
        "Cleaner cleaner;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = cleaner.clean();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_clears_string_member_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_class_method_compares_string_member_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class NameBox\n"
        "{\n"
        "    private:\n"
        "    string name(12);\n"
        "    public:\n"
        "    NameBox() : name(\"ALPHA\")\n"
        "    {\n"
        "    }\n"
        "    int same()\n"
        "    {\n"
        "        return name.compare(\"ALPHA\");\n"
        "    }\n"
        "};\n"
        "NameBox box;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = box.same();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_compares_string_member_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_global_array_pointer_rewrite_multiple_items_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void write_items(int *items)\n"
        "{\n"
        "    items[0] = 19;\n"
        "    items[2] = 23;\n"
        "    return;\n"
        "}\n"
        "int numbers[3];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    write_items(numbers);\n"
        "    total = numbers[0] + numbers[2];\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_global_array_pointer_rewrite_multiple_items_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_char_pointer_parameter_updates_second_item_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void write_second(char *items)\n"
        "{\n"
        "    items[1] = 'R';\n"
        "    return;\n"
        "}\n"
        "char letters[3];\n"
        "void main()\n"
        "{\n"
        "    letters[0] = 'A';\n"
        "    letters[1] = 'B';\n"
        "    write_second(letters);\n"
        "    display(letters[1]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_parameter_updates_second_item_generated", "R\n"));
}

FT_TEST(test_cblc_inline_allocated_int_pointer_sum_offsets_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    int *items;\n"
        "    items = std::malloc(4);\n"
        "    items[0] = 12;\n"
        "    items[3] = 30;\n"
        "    total = items[0] + items[3];\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_allocated_int_pointer_sum_offsets_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_allocated_char_pointer_rewrite_multiple_offsets_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char result;\n"
        "void main()\n"
        "{\n"
        "    char *letters;\n"
        "    letters = std::malloc(4);\n"
        "    letters[0] = 'C';\n"
        "    letters[2] = 'X';\n"
        "    *(letters + 2) = 'Z';\n"
        "    result = letters[2];\n"
        "    display(result);\n"
        "    std::free(letters);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_allocated_char_pointer_rewrite_multiple_offsets_generated", "Z\n"));
}

FT_TEST(test_cblc_inline_void_pointer_reassigned_between_scalars_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int score;\n"
        "int bonus;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    int *item;\n"
        "    score = 15;\n"
        "    bonus = 27;\n"
        "    raw = &score;\n"
        "    item = raw;\n"
        "    total = *item;\n"
        "    raw = &bonus;\n"
        "    item = raw;\n"
        "    total = total + *item;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_reassigned_between_scalars_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_string_contains_after_clear_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"ACTIVE\";\n"
        "    label.clear();\n"
        "    total = label.contains(\"A\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_after_clear_false_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_after_overwrite_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"OLD\";\n"
        "    label = \"NEWVALUE\";\n"
        "    total = label.starts_with(\"NEW\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_with_after_overwrite_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_struct_string_compare_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Message\n"
        "{\n"
        "    string text(16);\n"
        "};\n"
        "Message message;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    message.text = \"MATCH\";\n"
        "    total = message.text.compare(\"MATCH\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_compare_literal_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_class_method_string_starts_with_member_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class PrefixBox\n"
        "{\n"
        "    private:\n"
        "    string text(16);\n"
        "    public:\n"
        "    PrefixBox() : text(\"PREFIX\")\n"
        "    {\n"
        "    }\n"
        "    int check()\n"
        "    {\n"
        "        return text.starts_with(\"PRE\");\n"
        "    }\n"
        "};\n"
        "PrefixBox box;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = box.check();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_string_starts_with_member_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_class_method_string_ends_with_after_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class SuffixBox\n"
        "{\n"
        "    private:\n"
        "    string text(16);\n"
        "    public:\n"
        "    SuffixBox() : text(\"END\")\n"
        "    {\n"
        "    }\n"
        "    int check()\n"
        "    {\n"
        "        text.append(\"ING\");\n"
        "        return text.ends_with(\"ING\");\n"
        "    }\n"
        "};\n"
        "SuffixBox box;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = box.check();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_string_ends_with_after_append_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_class_method_updates_nested_struct_twice_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct State\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "class CounterBox\n"
        "{\n"
        "    private:\n"
        "    State state;\n"
        "    public:\n"
        "    CounterBox()\n"
        "    {\n"
        "        state.amount = 10;\n"
        "    }\n"
        "    void add(int amount)\n"
        "    {\n"
        "        state.amount = state.amount + amount;\n"
        "        return;\n"
        "    }\n"
        "    int read()\n"
        "    {\n"
        "        return state.amount;\n"
        "    }\n"
        "};\n"
        "CounterBox box;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    box.add(11);\n"
        "    box.add(21);\n"
        "    total = box.read();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_updates_nested_struct_twice_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_pointer_parameter_updates_offset_after_reassign_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void write_target(int *items)\n"
        "{\n"
        "    int *target;\n"
        "    target = items + 2;\n"
        "    *target = 74;\n"
        "    return;\n"
        "}\n"
        "int numbers[4];\n"
        "void main()\n"
        "{\n"
        "    numbers[2] = 1;\n"
        "    write_target(numbers);\n"
        "    display(numbers[2]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_parameter_updates_offset_after_reassign_generated", "+000000074\n"));
}

FT_TEST(test_cblc_inline_pointer_parameter_sums_two_offsets_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int sum_edges(int *items)\n"
        "{\n"
        "    return items[0] + items[3];\n"
        "}\n"
        "int numbers[4];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    numbers[0] = 18;\n"
        "    numbers[3] = 24;\n"
        "    total = sum_edges(numbers);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_pointer_parameter_sums_two_offsets_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_allocated_pointer_return_then_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *offset_item(int *items)\n"
        "{\n"
        "    return items + 1;\n"
        "}\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    int *items;\n"
        "    int *target;\n"
        "    items = std::malloc(3);\n"
        "    target = offset_item(items);\n"
        "    *target = 42;\n"
        "    total = items[1];\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_allocated_pointer_return_then_write_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_char_pointer_return_then_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "char *target_char(char *items)\n"
        "{\n"
        "    return items + 2;\n"
        "}\n"
        "char letters[4];\n"
        "char *target;\n"
        "void main()\n"
        "{\n"
        "    letters[2] = 'A';\n"
        "    target = target_char(letters);\n"
        "    *target = 'T';\n"
        "    display(letters[2]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_char_pointer_return_then_write_generated", "T\n"));
}

FT_TEST(test_cblc_inline_string_append_then_compare_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"RUN\";\n"
        "    label.append(\"TIME\");\n"
        "    total = label.compare(\"RUNTIME\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_then_compare_literal_generated", "+000000000\n"));
}

FT_TEST(test_cblc_inline_string_overwrite_then_ends_with_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(16);\n"
        "    label = \"FIRST\";\n"
        "    label = \"SECOND\";\n"
        "    total = label.ends_with(\"OND\");\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_overwrite_then_ends_with_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_struct_string_append_then_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Message\n"
        "{\n"
        "    string text(16);\n"
        "};\n"
        "Message message;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    message.text = \"LOG\";\n"
        "    message.text.append(\"IC\");\n"
        "    total = message.text.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_append_then_len_generated", "+000000005\n"));
}

FT_TEST(test_cblc_inline_struct_string_clear_then_empty_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Message\n"
        "{\n"
        "    string text(16);\n"
        "};\n"
        "Message message;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    message.text = \"CLEAR\";\n"
        "    message.text.clear();\n"
        "    total = message.text.empty();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_string_clear_then_empty_generated", "+000000001\n"));
}

FT_TEST(test_cblc_inline_class_method_appends_string_member_twice_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Builder\n"
        "{\n"
        "    private:\n"
        "    string text(16);\n"
        "    public:\n"
        "    Builder() : text(\"A\")\n"
        "    {\n"
        "    }\n"
        "    int build()\n"
        "    {\n"
        "        text.append(\"B\");\n"
        "        text.append(\"C\");\n"
        "        return text.len();\n"
        "    }\n"
        "};\n"
        "Builder builder;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = builder.build();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_appends_string_member_twice_generated", "+000000003\n"));
}

FT_TEST(test_cblc_inline_class_method_writes_array_pointer_offset_translates_to_cobol_and_executes)
{
    const char *source;

    source = "class Writer\n"
        "{\n"
        "    private:\n"
        "    int amount;\n"
        "    public:\n"
        "    Writer() : amount(55)\n"
        "    {\n"
        "    }\n"
        "    void write(int *items)\n"
        "    {\n"
        "        items[2] = amount;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "Writer writer;\n"
        "int numbers[3];\n"
        "void main()\n"
        "{\n"
        "    numbers[2] = 0;\n"
        "    writer.write(numbers);\n"
        "    display(numbers[2]);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_class_method_writes_array_pointer_offset_generated", "+000000055\n"));
}

FT_TEST(test_cblc_inline_complex_class_struct_string_pointer_pipeline_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Report\n"
        "{\n"
        "    string label(24);\n"
        "    int total;\n"
        "};\n"
        "int *choose_target(int *items)\n"
        "{\n"
        "    return items + 2;\n"
        "}\n"
        "void mark_done(void *raw)\n"
        "{\n"
        "    char *item;\n"
        "    item = raw;\n"
        "    *item = 'Y';\n"
        "    return;\n"
        "}\n"
        "class Pipeline\n"
        "{\n"
        "    private:\n"
        "    string name(16);\n"
        "    int multiplier;\n"
        "    public:\n"
        "    Pipeline() : name(\"CBL\"), multiplier(2)\n"
        "    {\n"
        "    }\n"
        "    void seed(int *items)\n"
        "    {\n"
        "        items[0] = 10;\n"
        "        items[1] = 11;\n"
        "        items[2] = 12;\n"
        "        items[3] = 13;\n"
        "        return;\n"
        "    }\n"
        "    void amplify(int *target)\n"
        "    {\n"
        "        *target = *target * multiplier;\n"
        "        return;\n"
        "    }\n"
        "    int summarize(int *items)\n"
        "    {\n"
        "        return items[0] + items[2] + multiplier;\n"
        "    }\n"
        "    int verify_name()\n"
        "    {\n"
        "        name.append(\"C\");\n"
        "        return name.compare(\"CBLC\");\n"
        "    }\n"
        "};\n"
        "Pipeline pipeline;\n"
        "Report report;\n"
        "int numbers[4];\n"
        "char flags[2];\n"
        "int *target;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    pipeline.seed(numbers);\n"
        "    target = choose_target(numbers);\n"
        "    pipeline.amplify(target);\n"
        "    report.label = \"START\";\n"
        "    report.label.append(\"-OK\");\n"
        "    report.total = pipeline.summarize(numbers);\n"
        "    mark_done(&flags[1]);\n"
        "    total = report.total + report.label.len();\n"
        "    total = total + report.label.starts_with(\"STA\");\n"
        "    total = total + report.label.ends_with(\"OK\");\n"
        "    total = total + pipeline.verify_name();\n"
        "    display(flags[1]);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_complex_class_struct_string_pointer_pipeline_generated",
            "Y\n+000000046\n"));
}

FT_TEST(test_cblc_inline_complex_dynamic_pointer_string_class_flow_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Summary\n"
        "{\n"
        "    string title(24);\n"
        "    int score;\n"
        "};\n"
        "int *pick_middle(void *raw)\n"
        "{\n"
        "    int *items;\n"
        "    items = raw;\n"
        "    return items + 1;\n"
        "}\n"
        "void write_flag(void *raw)\n"
        "{\n"
        "    char *item;\n"
        "    item = raw;\n"
        "    *item = 'D';\n"
        "    return;\n"
        "}\n"
        "class Analyzer\n"
        "{\n"
        "    private:\n"
        "    string prefix(12);\n"
        "    int bonus;\n"
        "    public:\n"
        "    Analyzer() : prefix(\"ANA\"), bonus(5)\n"
        "    {\n"
        "    }\n"
        "    void prepare(int *items)\n"
        "    {\n"
        "        items[0] = 7;\n"
        "        items[1] = 8;\n"
        "        items[2] = 9;\n"
        "        return;\n"
        "    }\n"
        "    void boost(int *target)\n"
        "    {\n"
        "        *target = *target + bonus;\n"
        "        return;\n"
        "    }\n"
        "    int score(int *items)\n"
        "    {\n"
        "        prefix.append(\"LYZE\");\n"
        "        return items[0] + items[1] + items[2] + prefix.len();\n"
        "    }\n"
        "    int valid()\n"
        "    {\n"
        "        return prefix.ends_with(\"LYZE\");\n"
        "    }\n"
        "};\n"
        "Analyzer analyzer;\n"
        "Summary summary;\n"
        "char flags[3];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    int *items;\n"
        "    int *target;\n"
        "    raw = std::malloc(3);\n"
        "    items = raw;\n"
        "    analyzer.prepare(items);\n"
        "    target = pick_middle(raw);\n"
        "    analyzer.boost(target);\n"
        "    summary.title = \"FLOW\";\n"
        "    summary.title.append(\"-DONE\");\n"
        "    summary.score = analyzer.score(items);\n"
        "    write_flag(&flags[2]);\n"
        "    total = summary.score + summary.title.contains(\"DONE\");\n"
        "    total = total + summary.title.starts_with(\"FLOW\");\n"
        "    total = total + analyzer.valid();\n"
        "    display(flags[2]);\n"
        "    display(total);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_complex_dynamic_pointer_string_class_flow_generated",
            "D\n+000000046\n"));
}

FT_TEST(test_cblc_inline_complex_dual_class_record_pointer_flow_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Ledger\n"
        "{\n"
        "    string name(24);\n"
        "    int amount;\n"
        "};\n"
        "int *select_slot(int *items)\n"
        "{\n"
        "    return items + 3;\n"
        "}\n"
        "void stamp(void *raw)\n"
        "{\n"
        "    char *flag;\n"
        "    flag = raw;\n"
        "    *flag = 'S';\n"
        "    return;\n"
        "}\n"
        "class Loader\n"
        "{\n"
        "    private:\n"
        "    int seed_value;\n"
        "    public:\n"
        "    Loader() : seed_value(4)\n"
        "    {\n"
        "    }\n"
        "    void fill(int *items)\n"
        "    {\n"
        "        items[0] = seed_value;\n"
        "        items[1] = seed_value + 3;\n"
        "        items[2] = seed_value + 6;\n"
        "        items[3] = seed_value + 9;\n"
        "        return;\n"
        "    }\n"
        "};\n"
        "class Processor\n"
        "{\n"
        "    private:\n"
        "    string suffix(12);\n"
        "    int factor;\n"
        "    public:\n"
        "    Processor() : suffix(\"-OK\"), factor(3)\n"
        "    {\n"
        "    }\n"
        "    void multiply(int *target)\n"
        "    {\n"
        "        *target = *target * factor;\n"
        "        return;\n"
        "    }\n"
        "    int check()\n"
        "    {\n"
        "        return suffix.ends_with(\"OK\");\n"
        "    }\n"
        "};\n"
        "Loader loader;\n"
        "Processor processor;\n"
        "Ledger ledger;\n"
        "int numbers[4];\n"
        "char flags[2];\n"
        "int *slot;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    loader.fill(numbers);\n"
        "    slot = select_slot(numbers);\n"
        "    processor.multiply(slot);\n"
        "    ledger.name = \"LEDGER\";\n"
        "    ledger.name.append(\"-OK\");\n"
        "    ledger.amount = numbers[0] + numbers[1] + numbers[3];\n"
        "    stamp(&flags[0]);\n"
        "    total = ledger.amount + ledger.name.len();\n"
        "    total = total + ledger.name.contains(\"GER\");\n"
        "    total = total + ledger.name.ends_with(\"OK\");\n"
        "    total = total + processor.check();\n"
        "    display(flags[0]);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_complex_dual_class_record_pointer_flow_generated",
            "S\n+000000062\n"));
}

FT_TEST(test_cblc_inline_complex_allocated_buffer_report_flow_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Report\n"
        "{\n"
        "    string title(24);\n"
        "    int total;\n"
        "};\n"
        "int *front_slot(void *raw)\n"
        "{\n"
        "    int *items;\n"
        "    items = raw;\n"
        "    return items;\n"
        "}\n"
        "int *tail_slot(int *items)\n"
        "{\n"
        "    return items + 3;\n"
        "}\n"
        "void set_status(void *raw)\n"
        "{\n"
        "    char *flag;\n"
        "    flag = raw;\n"
        "    *flag = 'R';\n"
        "    return;\n"
        "}\n"
        "class Reporter\n"
        "{\n"
        "    private:\n"
        "    string tag(16);\n"
        "    int offset;\n"
        "    public:\n"
        "    Reporter() : tag(\"REP\"), offset(6)\n"
        "    {\n"
        "    }\n"
        "    void load(int *items)\n"
        "    {\n"
        "        items[0] = 5;\n"
        "        items[1] = 8;\n"
        "        items[2] = 13;\n"
        "        items[3] = 21;\n"
        "        return;\n"
        "    }\n"
        "    void adjust(int *first, int *last)\n"
        "    {\n"
        "        *first = *first + offset;\n"
        "        *last = *last + offset;\n"
        "        return;\n"
        "    }\n"
        "    int finalize(Report report, int *items)\n"
        "    {\n"
        "        tag.append(\"ORT\");\n"
        "        return items[0] + items[2] + items[3] + report.title.len() + tag.len();\n"
        "    }\n"
        "    int valid()\n"
        "    {\n"
        "        return tag.compare(\"REPORT\");\n"
        "    }\n"
        "};\n"
        "Reporter reporter;\n"
        "Report report;\n"
        "char flags[4];\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    void *raw;\n"
        "    int *items;\n"
        "    int *first;\n"
        "    int *last;\n"
        "    raw = std::malloc(4);\n"
        "    items = raw;\n"
        "    reporter.load(items);\n"
        "    first = front_slot(raw);\n"
        "    last = tail_slot(items);\n"
        "    reporter.adjust(first, last);\n"
        "    report.title = \"DAILY\";\n"
        "    report.title.append(\"-RUN\");\n"
        "    report.total = reporter.finalize(report, items);\n"
        "    set_status(&flags[3]);\n"
        "    total = report.total + report.title.contains(\"RUN\");\n"
        "    total = total + report.title.starts_with(\"DAY\");\n"
        "    total = total + report.title.ends_with(\"RUN\");\n"
        "    total = total + reporter.valid();\n"
        "    display(flags[3]);\n"
        "    display(total);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_complex_allocated_buffer_report_flow_generated",
            "R\n+000000069\n"));
}

FT_TEST(test_cblc_inline_sizeof_primitives_pointers_and_struct_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Packet\n"
        "{\n"
        "    char tag[2];\n"
        "    int value;\n"
        "};\n"
        "Packet packet;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    total = sizeof(char) + sizeof(int);\n"
        "    total = total + sizeof(void *);\n"
        "    total = total + sizeof(int *);\n"
        "    total = total + sizeof(Packet);\n"
        "    total = total + sizeof(packet);\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_sizeof_primitives_pointers_struct_generated", "+000000033\n"));
}

FT_TEST(test_cblc_inline_sizeof_malloc_int_buffer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    int *items;\n"
        "    items = std::malloc(3 * sizeof(int));\n"
        "    items[0] = sizeof(char *);\n"
        "    items[1] = sizeof(int);\n"
        "    items[2] = 30;\n"
        "    total = items[0] + items[1] + items[2];\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_sizeof_malloc_int_buffer_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_int_field_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record record;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    record.amount = 3;\n"
        "    item = &record;\n"
        "    item->amount = 42;\n"
        "    display(record.amount);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_int_field_write_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_int_field_read_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record record;\n"
        "Record *item;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    record.amount = 37;\n"
        "    item = &record;\n"
        "    total = item->amount + 5;\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_int_field_read_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_char_field_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    char marker;\n"
        "};\n"
        "Record record;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    record.marker = 'A';\n"
        "    item = &record;\n"
        "    item->marker = 'Z';\n"
        "    display(record.marker);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_char_field_write_generated", "Z\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_string_field_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string name(12);\n"
        "};\n"
        "Record record;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    record.name = \"OLD\";\n"
        "    item = &record;\n"
        "    item->name = \"READY\";\n"
        "    display(record.name);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_string_field_write_generated", "READY\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_string_field_display_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string name(12);\n"
        "};\n"
        "Record record;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    record.name = \"DIRECT\";\n"
        "    item = &record;\n"
        "    display(item->name);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_string_field_display_generated", "DIRECT\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_string_field_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string name(12);\n"
        "};\n"
        "Record record;\n"
        "Record *item;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    record.name = \"LENGTH\";\n"
        "    item = &record;\n"
        "    total = item->name.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_string_field_len_generated", "+000000006\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_nested_int_field_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner inner;\n"
        "};\n"
        "Outer record;\n"
        "Outer *item;\n"
        "void main()\n"
        "{\n"
        "    record.inner.amount = 7;\n"
        "    item = &record;\n"
        "    item->inner.amount = 42;\n"
        "    display(record.inner.amount);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_nested_int_field_write_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_nested_char_field_read_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    char marker;\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner inner;\n"
        "};\n"
        "Outer record;\n"
        "Outer *item;\n"
        "void main()\n"
        "{\n"
        "    record.inner.marker = 'N';\n"
        "    item = &record;\n"
        "    display(item->inner.marker);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_nested_char_field_read_generated", "N\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_nested_string_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    string name(12);\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner inner;\n"
        "};\n"
        "Outer record;\n"
        "Outer *item;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    record.inner.name = \"OLD\";\n"
        "    item = &record;\n"
        "    item->inner.name = \"NESTED\";\n"
        "    display(item->inner.name);\n"
        "    total = item->inner.name.len();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_nested_string_field_generated", "NESTED\n+000000006\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_indexed_int_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record *items;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(2 * sizeof(Record));\n"
        "    items[0].amount = 11;\n"
        "    items[1].amount = 42;\n"
        "    display(items[1].amount);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_indexed_int_field_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_indexed_nested_int_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "struct Outer\n"
        "{\n"
        "    Inner inner;\n"
        "};\n"
        "Outer *items;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(3 * sizeof(Outer));\n"
        "    items[0].inner.amount = 5;\n"
        "    items[1].inner.amount = 17;\n"
        "    items[2].inner.amount = 20;\n"
        "    total = items[1].inner.amount + items[2].inner.amount + 5;\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_indexed_nested_int_field_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_indexed_string_field_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    string name(12);\n"
        "};\n"
        "Record *items;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(2 * sizeof(Record));\n"
        "    items[0].name = \"FIRST\";\n"
        "    items[1].name = \"SECOND\";\n"
        "    display(items[1].name);\n"
        "    total = items[1].name.len();\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_indexed_string_field_generated", "SECOND\n+000000006\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_cast_from_void_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "void *raw;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(sizeof(Record));\n"
        "    item = (Record *)raw;\n"
        "    item->amount = 42;\n"
        "    display(item->amount);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_cast_from_void_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_void_pointer_cast_from_struct_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "void *raw;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    item = std::malloc(sizeof(Record));\n"
        "    raw = (void *)item;\n"
        "    item = (Record *)raw;\n"
        "    item->amount = 42;\n"
        "    display(item->amount);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_void_pointer_cast_from_struct_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_cast_argument_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "void set_amount(Record *item)\n"
        "{\n"
        "    item->amount = 42;\n"
        "    return;\n"
        "}\n"
        "void *raw;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    raw = std::malloc(sizeof(Record));\n"
        "    set_amount((Record *)raw);\n"
        "    item = (Record *)raw;\n"
        "    display(item->amount);\n"
        "    std::free(raw);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_cast_argument_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_deref_read_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record source;\n"
        "Record copy;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    source.amount = 42;\n"
        "    item = &source;\n"
        "    copy = *item;\n"
        "    display(copy.amount);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_deref_read_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_deref_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record source;\n"
        "Record target;\n"
        "Record *item;\n"
        "void main()\n"
        "{\n"
        "    source.amount = 42;\n"
        "    target.amount = 1;\n"
        "    item = &target;\n"
        "    *item = source;\n"
        "    display(target.amount);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_deref_write_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_deref_copy_between_pointers_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record source;\n"
        "Record target;\n"
        "Record *left;\n"
        "Record *right;\n"
        "void main()\n"
        "{\n"
        "    source.amount = 42;\n"
        "    target.amount = 1;\n"
        "    left = &target;\n"
        "    right = &source;\n"
        "    *left = *right;\n"
        "    display(target.amount);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_deref_copy_between_pointers_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_indexed_whole_read_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record *items;\n"
        "Record copy;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(2 * sizeof(Record));\n"
        "    items[0].amount = 11;\n"
        "    items[1].amount = 42;\n"
        "    copy = items[1];\n"
        "    display(copy.amount);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_indexed_whole_read_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_indexed_whole_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Record\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "Record *items;\n"
        "Record source;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(2 * sizeof(Record));\n"
        "    source.amount = 42;\n"
        "    items[0].amount = 11;\n"
        "    items[1] = source;\n"
        "    display(items[1].amount);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_indexed_whole_write_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_struct_pointer_indexed_whole_nested_string_copy_translates_to_cobol_and_executes)
{
    const char *source;

    source = "struct Inner\n"
        "{\n"
        "    int amount;\n"
        "};\n"
        "struct Record\n"
        "{\n"
        "    Inner inner;\n"
        "    string name(12);\n"
        "};\n"
        "Record *items;\n"
        "Record copy;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(2 * sizeof(Record));\n"
        "    items[1].inner.amount = 42;\n"
        "    items[1].name = \"INDEXED\";\n"
        "    copy = items[1];\n"
        "    display(copy.inner.amount);\n"
        "    display(copy.name);\n"
        "    total = copy.name.len();\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_struct_pointer_indexed_whole_nested_string_copy_generated",
            "+000000042\nINDEXED\n+000000007\n"));
}

FT_TEST(test_cblc_inline_int_pointer_to_pointer_write_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *first;\n"
        "int *second;\n"
        "int **slot;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    first = std::malloc(sizeof(int));\n"
        "    second = std::malloc(sizeof(int));\n"
        "    first[0] = 7;\n"
        "    second[0] = 42;\n"
        "    slot = &first;\n"
        "    *slot = second;\n"
        "    total = first[0];\n"
        "    display(total);\n"
        "    std::free(first);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_int_pointer_to_pointer_write_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_int_pointer_to_pointer_read_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *items;\n"
        "int *picked;\n"
        "int **slot;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    items = std::malloc(sizeof(int));\n"
        "    items[0] = 42;\n"
        "    slot = &items;\n"
        "    picked = *slot;\n"
        "    total = picked[0];\n"
        "    display(total);\n"
        "    std::free(items);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_int_pointer_to_pointer_read_generated", "+000000042\n"));
}

FT_TEST(test_cblc_inline_int_pointer_to_pointer_retargets_pointer_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int *left;\n"
        "int *right;\n"
        "int **slot;\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    left = std::malloc(sizeof(int));\n"
        "    right = std::malloc(sizeof(int));\n"
        "    left[0] = 11;\n"
        "    right[0] = 42;\n"
        "    slot = &left;\n"
        "    *slot = right;\n"
        "    total = left[0];\n"
        "    display(total);\n"
        "    std::free(left);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_int_pointer_to_pointer_retargets_pointer_generated", "+000000042\n"));
}
