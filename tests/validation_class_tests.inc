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
    if (test_expect_size_t_equal(unit.struct_types[0].constructor_count, 2,
            "builtin string should expose capacity and copy constructor overloads") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(unit.struct_types[0].constructors[0].parameters[0].kind),
            TRANSPILE_FUNCTION_PARAMETER_INT,
            "builtin string capacity constructor should use the int parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(unit.struct_types[0].constructors[1].parameters[0].kind),
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
            "cblc_string_append(main__greeting_buf, 12, &main__greeting_len, main__suffix_buf, main__suffix_len);",
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
    if (!ft_strnstr(generated_c, "total = 12;", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should map builtin string capacity method to declared capacity\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "cblc_string_assign_literal(main__greeting_buf, 12, &main__greeting_len, \"\");",
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
            "MOVE MAIN-SUFFIX-BUF(1:MAIN-SUFFIX-LEN) TO MAIN-GREETING-BUF(MAIN-GREETING-LEN + 1:MAIN-SUFFIX-LEN).",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit builtin dynamic string append move\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = MAIN-GREETING-LEN.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should map builtin string len method to GREETING-LEN\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE TOTAL = 12.", std::strlen(generated_cobol)))
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
