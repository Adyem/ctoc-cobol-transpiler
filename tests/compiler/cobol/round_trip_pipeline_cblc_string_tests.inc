FT_TEST(test_cblc_inline_string_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_generated", "HELLO!\n"));
}

FT_TEST(test_cblc_inline_string_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO!\";\n"
        "    total = greeting.len();\n"
        "    if (total == 6) {\n"
        "        display(\"LEN OK\");\n"
        "    } else {\n"
        "        display(\"LEN BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_len_generated", "LEN OK\n"));
}

FT_TEST(test_cblc_inline_string_clear_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    greeting.clear();\n"
        "    total = greeting.len();\n"
        "    if (total == 0) {\n"
        "        display(\"CLEAR OK\");\n"
        "    } else {\n"
        "        display(\"CLEAR BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_clear_generated", "CLEAR OK\n"));
}

FT_TEST(test_cblc_inline_string_empty_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    total = greeting.empty();\n"
        "    if (total == 1) {\n"
        "        display(\"EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_empty_generated", "EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_equals_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"HELLO\";\n"
        "    other = \"HELLO\";\n"
        "    total = greeting.equals(other);\n"
        "    if (total == 1) {\n"
        "        display(\"EQUALS OK\");\n"
        "    } else {\n"
        "        display(\"EQUALS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_equals_generated", "EQUALS OK\n"));
}

FT_TEST(test_cblc_inline_string_equals_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.equals(\"HELLO\");\n"
        "    if (total == 1) {\n"
        "        display(\"LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_equals_literal_generated", "LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_capacity_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    total = greeting.capacity();\n"
        "    if (total == 16) {\n"
        "        display(\"CAPACITY OK\");\n"
        "    } else {\n"
        "        display(\"CAPACITY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_generated", "CAPACITY OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string prefix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    prefix = \"HE\";\n"
        "    total = greeting.starts_with(prefix);\n"
        "    if (total == 1) {\n"
        "        display(\"STARTS OK\");\n"
        "    } else {\n"
        "        display(\"STARTS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_with_generated", "STARTS OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"LO\";\n"
        "    total = greeting.ends_with(suffix);\n"
        "    if (total == 1) {\n"
        "        display(\"ENDS OK\");\n"
        "    } else {\n"
        "        display(\"ENDS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_with_generated", "ENDS OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"HELLO\";\n"
        "    total = greeting.compare(suffix);\n"
        "    if (total == 0) {\n"
        "        display(\"COMPARE OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_generated", "COMPARE OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string needle(8);\n"
        "    greeting = \"HELLO\";\n"
        "    needle = \"ELL\";\n"
        "    total = greeting.contains(needle);\n"
        "    if (total == 1) {\n"
        "        display(\"CONTAINS OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_generated", "CONTAINS OK\n"));
}

FT_TEST(test_cblc_inline_string_equals_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"HELLO\";\n"
        "    other = \"WORLD\";\n"
        "    total = greeting.equals(other);\n"
        "    if (total == 0) {\n"
        "        display(\"EQUALS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"EQUALS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_equals_false_generated", "EQUALS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_empty_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.empty();\n"
        "    if (total == 0) {\n"
        "        display(\"EMPTY FALSE OK\");\n"
        "    } else {\n"
        "        display(\"EMPTY FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_empty_false_generated", "EMPTY FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.starts_with(\"HE\");\n"
        "    if (total == 1) {\n"
        "        display(\"STARTS LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"STARTS LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_with_literal_generated", "STARTS LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.ends_with(\"LO\");\n"
        "    if (total == 1) {\n"
        "        display(\"ENDS LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"ENDS LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_with_literal_generated", "ENDS LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_less_than_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"ALPHA\";\n"
        "    suffix = \"BRAVO\";\n"
        "    total = greeting.compare(suffix);\n"
        "    if (total == -1) {\n"
        "        display(\"COMPARE LT OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE LT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_less_generated", "COMPARE LT OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_greater_than_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"BRAVO\";\n"
        "    suffix = \"ALPHA\";\n"
        "    total = greeting.compare(suffix);\n"
        "    if (total == 1) {\n"
        "        display(\"COMPARE GT OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE GT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_greater_generated", "COMPARE GT OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.compare(\"HELLO\");\n"
        "    if (total == 0) {\n"
        "        display(\"COMPARE LITERAL OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE LITERAL BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_literal_generated", "COMPARE LITERAL OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string needle(8);\n"
        "    greeting = \"HELLO\";\n"
        "    needle = \"XYZ\";\n"
        "    total = greeting.contains(needle);\n"
        "    if (total == 0) {\n"
        "        display(\"CONTAINS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_false_generated", "CONTAINS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.contains(\"\");\n"
        "    if (total == 1) {\n"
        "        display(\"CONTAINS EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_empty_generated", "CONTAINS EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_constructor_capacity_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    total = greeting.capacity();\n"
        "    if (total == 5) {\n"
        "        display(\"CTOR CAPACITY OK\");\n"
        "    } else {\n"
        "        display(\"CTOR CAPACITY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ctor_capacity_generated", "CTOR CAPACITY OK\n"));
}

FT_TEST(test_cblc_inline_string_capacity_constructor_grows_for_assignment_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(1);\n"
        "    label = \"EXPANDED\";\n"
        "    total = label.len();\n"
        "    if (total == 8) {\n"
        "        display(label);\n"
        "    } else {\n"
        "        display(\"STRING GROW BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_assignment_grows_generated", "EXPANDED\n"));
}

FT_TEST(test_cblc_inline_string_capacity_constructor_grows_for_append_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string label(1);\n"
        "    label = \"A\";\n"
        "    label.append(\"BCDE\");\n"
        "    total = label.len();\n"
        "    if (total == 5) {\n"
        "        display(label);\n"
        "    } else {\n"
        "        display(\"APPEND GROW BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_capacity_append_grows_generated", "ABCDE\n"));
}

FT_TEST(test_cblc_inline_string_append_then_len_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"!\";\n"
        "    greeting.append(suffix);\n"
        "    total = greeting.len();\n"
        "    if (total == 6) {\n"
        "        display(\"APPEND LEN OK\");\n"
        "    } else {\n"
        "        display(\"APPEND LEN BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_append_len_generated", "APPEND LEN OK\n"));
}

FT_TEST(test_cblc_inline_string_clear_then_empty_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    greeting.clear();\n"
        "    total = greeting.empty();\n"
        "    if (total == 1) {\n"
        "        display(\"CLEAR EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"CLEAR EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_clear_empty_generated", "CLEAR EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string prefix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    prefix = \"EL\";\n"
        "    total = greeting.starts_with(prefix);\n"
        "    if (total == 0) {\n"
        "        display(\"STARTS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"STARTS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_false_generated", "STARTS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string suffix(8);\n"
        "    greeting = \"HELLO\";\n"
        "    suffix = \"HE\";\n"
        "    total = greeting.ends_with(suffix);\n"
        "    if (total == 0) {\n"
        "        display(\"ENDS FALSE OK\");\n"
        "    } else {\n"
        "        display(\"ENDS FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_false_generated", "ENDS FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_starts_with_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.starts_with(\"\");\n"
        "    if (total == 1) {\n"
        "        display(\"STARTS EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"STARTS EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_starts_empty_generated", "STARTS EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_ends_with_empty_literal_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.ends_with(\"\");\n"
        "    if (total == 1) {\n"
        "        display(\"ENDS EMPTY OK\");\n"
        "    } else {\n"
        "        display(\"ENDS EMPTY BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_ends_empty_generated", "ENDS EMPTY OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_shorter_prefix_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"AL\";\n"
        "    other = \"ALPHA\";\n"
        "    total = greeting.compare(other);\n"
        "    if (total == -1) {\n"
        "        display(\"COMPARE SHORT OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE SHORT BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_short_generated", "COMPARE SHORT OK\n"));
}

FT_TEST(test_cblc_inline_string_compare_longer_prefix_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    string other(16);\n"
        "    greeting = \"ALPHA\";\n"
        "    other = \"AL\";\n"
        "    total = greeting.compare(other);\n"
        "    if (total == 1) {\n"
        "        display(\"COMPARE LONG OK\");\n"
        "    } else {\n"
        "        display(\"COMPARE LONG BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_compare_long_generated", "COMPARE LONG OK\n"));
}

FT_TEST(test_cblc_inline_string_contains_literal_false_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(16);\n"
        "    greeting = \"HELLO\";\n"
        "    total = greeting.contains(\"XYZ\");\n"
        "    if (total == 0) {\n"
        "        display(\"CONTAINS LITERAL FALSE OK\");\n"
        "    } else {\n"
        "        display(\"CONTAINS LITERAL FALSE BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_contains_literal_false_generated", "CONTAINS LITERAL FALSE OK\n"));
}

FT_TEST(test_cblc_inline_string_copy_constructor_equals_translates_to_cobol_and_executes)
{
    const char *source;

    source = "int total;\n"
        "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string copy(greeting);\n"
        "    total = copy.equals(greeting);\n"
        "    if (total == 1) {\n"
        "        display(\"COPY EQUALS OK\");\n"
        "    } else {\n"
        "        display(\"COPY EQUALS BAD\");\n"
        "    }\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_copy_equals_generated", "COPY EQUALS OK\n"));
}

FT_TEST(test_cblc_inline_string_literal_constructor_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    display(greeting);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_literal_constructor_generated", "HELLO\n"));
}

FT_TEST(test_cblc_inline_string_copy_constructor_translates_to_cobol_and_executes)
{
    const char *source;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HELLO\");\n"
        "    string copy(greeting);\n"
        "    display(copy);\n"
        "    return;\n"
        "}\n";
    return (run_inline_cblc_to_cobol_execution_test(source,
            "inline_string_copy_constructor_generated", "HELLO\n"));
}
