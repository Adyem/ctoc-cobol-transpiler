static const t_cblc_struct_type *validation_find_struct_type_by_name(
    const t_cblc_translation_unit *unit, const char *source_name)
{
    size_t index;

    if (!unit || !source_name)
        return (NULL);
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (std::strcmp(unit->struct_types[index].source_name, source_name) == 0)
            return (&unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_struct_type *validation_find_first_non_string_type(
    const t_cblc_translation_unit *unit)
{
    size_t index;

    if (!unit)
        return (NULL);
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (std::strcmp(unit->struct_types[index].source_name, "string") != 0)
            return (&unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

FT_TEST(test_cblc_parse_translation_unit_records_class_lifecycle_metadata)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Person\n"
        "{\n"
        "    string name;\n"
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
    if (test_expect_size_t_equal(unit.struct_type_count, 11,
            "standard error and string classes plus one user class type should be recorded") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Person")->is_class, 1,
            "type should be marked as class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Person")->has_default_constructor, 1,
            "class should record default constructor") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Person")->has_destructor, 0,
            "member-driven cleanup should not be reported as a user-declared destructor") != FT_SUCCESS)
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
        "    int name;\n"
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
    needle = "person.name = 0;";
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
    int diagnostic_code;
    char diagnostic_message[512];
    int status;
    int count;

    source = "class Person\n"
        "{\n"
        "    string name;\n"
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
    diagnostic_code = 0;
    diagnostic_message[0] = '\0';
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &diagnostic_code, diagnostic_message, sizeof(diagnostic_message))
        != FT_SUCCESS)
    {
        std::printf("Class lifecycle generation diagnostic %d: %s\n",
            diagnostic_code, diagnostic_message);
        goto cleanup;
    }
    if (!generated_cobol)
        goto cleanup;
    needle = "ALLOCATE 1 CHARACTERS RETURNING PERSON-NAME-CBLC-USER-DATA.";
    count = 0;
    scan = generated_cobol;
    while ((scan = std::strstr(scan, needle)) != NULL)
    {
        count += 1;
        scan += std::strlen(needle);
    }
    if (test_expect_int_equal(count, 1,
            "generated COBOL should construct the nested string once at entry")
        != FT_SUCCESS)
        goto cleanup;
    if (!ft_strnstr(generated_cobol,
            "10 PERSON-NAME-CBLC-USER-DATA USAGE POINTER VALUE NULL.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use the string class pointer field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "FREE PERSON-NAME-CBLC-USER-DATA.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should invoke the string destructor\n");
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
        "    int name;\n"
        "    Person() {\n"
        "        name = 1;\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = 2;\n"
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
    if (!ft_strnstr(generated_c, "void cblc_constructor_Person_0(t_Person *receiver);",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "receiver->name = 1;",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_constructor_Person_0(&person);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit custom constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "void cblc_destructor_Person(t_Person *receiver);",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "receiver->name = 2;",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_destructor_Person(&person);",
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
    const char *scan;
    size_t body_count;
    int status;

    source = "class Person\n"
        "{\n"
        "    int name;\n"
        "    Person() {\n"
        "        name = 1;\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = 2;\n"
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
    if (!ft_strnstr(generated_cobol, "COMPUTE PERSON-NAME = 1.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit custom constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE PERSON-NAME = 2.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit custom destructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "PERFORM CBLC-CONSTRUCTOR-PERSON-0 THRU CBLC-CRET-",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "PERFORM CBLC-DESTRUCTOR-PERSON.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should call reusable lifecycle paragraphs\n");
        goto cleanup;
    }
    body_count = 0;
    scan = generated_cobol;
    while ((scan = std::strstr(scan, "COMPUTE PERSON-NAME = 1.")) != NULL)
    {
        body_count += 1;
        scan += std::strlen("COMPUTE PERSON-NAME = 1.");
    }
    if (test_expect_size_t_equal(body_count, 1,
            "generated COBOL should emit the constructor body once") != FT_SUCCESS)
        goto cleanup;
    body_count = 0;
    scan = generated_cobol;
    while ((scan = std::strstr(scan, "COMPUTE PERSON-NAME = 2.")) != NULL)
    {
        body_count += 1;
        scan += std::strlen("COMPUTE PERSON-NAME = 2.");
    }
    if (test_expect_size_t_equal(body_count, 1,
            "generated COBOL should emit the destructor body once") != FT_SUCCESS)
        goto cleanup;
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
        "    int name;\n"
        "    Person() {\n"
        "        name = 1;\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = 2;\n"
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
    if (!ft_strnstr(generated_c, "cblc_constructor_Person_0(&main__person);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit local constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_destructor_Person(&main__person);",
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
        "    int name;\n"
        "    Person() {\n"
        "        name = 1;\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = 2;\n"
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
    if (!ft_strnstr(generated_cobol, "COMPUTE MAIN-PERSON-NAME = 1.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should emit local constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE MAIN-PERSON-NAME = 2.", std::strlen(generated_cobol)))
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
        "    int name;\n"
        "    Person() {\n"
        "        name = 1;\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = 2;\n"
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
    first_ctor = "cblc_constructor_Person_0(&main__first);";
    first_dtor = "cblc_destructor_Person(&main__first);";
    second_ctor = "cblc_constructor_Person_0(&main__second);";
    second_dtor = "cblc_destructor_Person(&main__second);";
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
        "    int name;\n"
        "    Person() {\n"
        "        name = 1;\n"
        "    }\n"
        "    ~Person() {\n"
        "        name = 2;\n"
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
    first_ctor = "PERFORM CBLC-CONSTRUCTOR-MAIN-FIRST-0 THRU CBLC-CRET-";
    first_dtor = "PERFORM CBLC-DESTRUCTOR-MAIN-FIRST.";
    second_ctor = "PERFORM CBLC-CONSTRUCTOR-MAIN-SECOND-0 THRU CBLC-CRET-";
    second_dtor = "PERFORM CBLC-DESTRUCTOR-MAIN-SECOND.";
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

FT_TEST(test_cblc_parse_translation_unit_imports_canonical_string_class)
{
    const char *source;
    const t_cblc_struct_type *string_type;
    t_cblc_translation_unit unit;
    size_t item_index;
    int status;

    source = "string greeting;\n"
        "void main()\n"
        "{\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "canonical string class sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(unit.struct_type_count, 10,
            "canonical error and string classes should be imported without duplicates") != FT_SUCCESS)
        goto cleanup;
    string_type = validation_find_struct_type_by_name(&unit, "string");
    if (!string_type)
    {
        std::printf("Assertion failed: standard string class should be registered under the string name\n");
        goto cleanup;
    }
    if (test_expect_int_equal(string_type->is_class, 1,
            "string should be represented as a class") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(string_type->is_builtin, 0,
            "string should come from the standard-library class source") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(string_type->is_standard_library, 1,
            "string should be identified as a native standard-library type")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(string_type->field_count, 3,
            "string should use the owning class representation from its source") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(string_type->fields[0].kind),
            CBLC_DATA_KIND_CHAR_POINTER,
            "string class should own its character storage through a pointer") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(string_type->has_default_constructor, 1,
            "string class should provide its declared default constructor") != FT_SUCCESS)
        goto cleanup;
    if (string_type->method_count < 10)
    {
        std::printf("Assertion failed: canonical string class should expose its declared methods\n");
        goto cleanup;
    }
    item_index = 0;
    while (item_index < unit.data_count
        && std::strncmp(unit.data_items[item_index].source_name, "greeting",
            sizeof(unit.data_items[item_index].source_name)) != 0)
        item_index += 1;
    if (item_index >= unit.data_count)
    {
        std::printf("Assertion failed: string instance should be registered as data\n");
        goto cleanup;
    }
    if (test_expect_int_equal(unit.data_items[item_index].kind, CBLC_DATA_KIND_STRUCT,
            "string instance should use ordinary class storage") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(unit.data_items[item_index].struct_type_name, "string",
            sizeof(unit.data_items[item_index].struct_type_name)) != 0)
    {
        std::printf("Assertion failed: string instance should resolve to the standard class type\n");
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
    size_t greeting_index;
    size_t copy_index;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    string copy(greeting);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string constructor-style sample should parse") != FT_SUCCESS)
        goto cleanup;
    greeting_index = 0;
    while (greeting_index < unit.data_count
        && std::strcmp(unit.data_items[greeting_index].source_name, "greeting") != 0)
        greeting_index += 1;
    copy_index = 0;
    while (copy_index < unit.data_count
        && std::strcmp(unit.data_items[copy_index].source_name, "copy") != 0)
        copy_index += 1;
    if (greeting_index >= unit.data_count || copy_index >= unit.data_count
        || test_expect_int_equal(unit.data_items[greeting_index].kind,
                CBLC_DATA_KIND_STRUCT,
                "string capacity construction should create a class instance") != FT_SUCCESS
        || std::strcmp(unit.data_items[greeting_index].struct_type_name, "string") != 0)
        goto cleanup;
    if (test_expect_int_equal(unit.data_items[copy_index].kind, CBLC_DATA_KIND_STRUCT,
            "copy construction should create an ordinary class instance") != FT_SUCCESS
        || std::strcmp(unit.data_items[copy_index].struct_type_name, "string") != 0)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_local_string_declaration_is_a_class_instance)
{
    const char *source;
    t_cblc_translation_unit unit;
    size_t greeting_index;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(8);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "local string lifecycle sample should parse") != FT_SUCCESS)
        goto cleanup;
    greeting_index = 0;
    while (greeting_index < unit.data_count
        && std::strcmp(unit.data_items[greeting_index].source_name, "greeting") != 0)
        greeting_index += 1;
    if (greeting_index >= unit.data_count
        || unit.data_items[greeting_index].kind != CBLC_DATA_KIND_STRUCT
        || std::strcmp(unit.data_items[greeting_index].struct_type_name, "string") != 0)
    {
        std::printf("Assertion failed: local string must be represented as a class instance\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_constructor_arguments_resolve_to_the_class)
{
    const char *source;
    t_cblc_translation_unit unit;
    size_t greeting_index;
    size_t copy_index;
    int status;

    source = "void main()\n"
        "{\n"
        "    string greeting(\"HI\");\n"
        "    string copy(greeting);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string constructor-style C sample should parse") != FT_SUCCESS)
        goto cleanup;
    greeting_index = 0;
    while (greeting_index < unit.data_count
        && std::strcmp(unit.data_items[greeting_index].source_name, "greeting") != 0)
        greeting_index += 1;
    copy_index = 0;
    while (copy_index < unit.data_count
        && std::strcmp(unit.data_items[copy_index].source_name, "copy") != 0)
        copy_index += 1;
    if (greeting_index >= unit.data_count || copy_index >= unit.data_count
        || test_expect_int_equal(unit.data_items[greeting_index].kind, CBLC_DATA_KIND_STRUCT,
            "literal construction should resolve to the string class") != FT_SUCCESS
        || std::strcmp(unit.data_items[greeting_index].struct_type_name, "string") != 0
        || test_expect_int_equal(unit.data_items[copy_index].kind, CBLC_DATA_KIND_STRUCT,
            "copy construction should resolve to the string class") != FT_SUCCESS
        || std::strcmp(unit.data_items[copy_index].struct_type_name, "string") != 0)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_string_capacity_method_is_callable_and_returns_integer)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    size_t type_index;
    size_t method_index;
    size_t statement_index;
    int saw_capacity_method;
    int saw_capacity_call;
    int status;

    source = "int observed;\n"
        "void main()\n"
        "{\n"
        "    string text(\"abc\");\n"
        "    string reserved(12);\n"
        "    observed = text.capacity();\n"
        "    observed = reserved.capacity();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    saw_capacity_method = 0;
    saw_capacity_call = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "string capacity call should parse") != FT_SUCCESS)
        goto cleanup;
    for (type_index = 0; type_index < unit.struct_type_count; ++type_index)
    {
        const t_cblc_struct_type *type = &unit.struct_types[type_index];

        if (std::strcmp(type->source_name, "string") != 0)
            continue;
        for (method_index = 0; method_index < type->method_count; ++method_index)
            if (std::strcmp(type->methods[method_index].source_name,
                    "capacity") == 0
                && type->methods[method_index].return_kind
                    == CBLC_FUNCTION_RETURN_INT)
                saw_capacity_method = 1;
    }
    for (statement_index = 0; statement_index < unit.function_count; ++statement_index)
    {
        const t_cblc_function *function = &unit.functions[statement_index];
        size_t body_index;

        if (std::strcmp(function->source_name, "main") != 0)
            continue;
        for (body_index = 0; body_index < function->statement_count; ++body_index)
            if (function->statements[body_index].type
                    == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                && std::strcmp(function->statements[body_index].call_identifier,
                    "capacity") == 0)
                saw_capacity_call = 1;
    }
    if (test_expect_int_equal(saw_capacity_method, 1,
            "string should expose its integer capacity method") != FT_SUCCESS
        || test_expect_int_equal(saw_capacity_call, 1,
            "string capacity invocation should resolve as a method call") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "string capacity method should lower to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-TEXT-CAPACITY", std::strlen(generated_cobol)))
    {
        std::printf("Generated COBOL is missing the string capacity dispatch\n");
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
    if (!ft_strnstr(generated_cobol, "MAIN-GREETING-CBLC-USER-DATA",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should store the string class pointer field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MAIN-COPY-CBLC-USER-DATA",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should store the copied string class pointer field\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_cobol, "MAIN-GREETING-BUF", std::strlen(generated_cobol))
        || ft_strnstr(generated_cobol, "MAIN-COPY-BUF", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: string class construction must not use the removed buffer-backed layout\n");
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
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->method_count, 2,
            "class should record both methods") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(validation_find_struct_type_by_name(&unit, "Counter")->methods[0].source_name, "reset",
            sizeof(validation_find_struct_type_by_name(&unit, "Counter")->methods[0].source_name)) != 0)
    {
        std::printf("Assertion failed: first class method should be reset\n");
        goto cleanup;
    }
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Counter")->methods[0].return_kind,
            CBLC_FUNCTION_RETURN_VOID, "reset should be a void method") != FT_SUCCESS)
        goto cleanup;
    if (std::strncmp(validation_find_struct_type_by_name(&unit, "Counter")->methods[1].source_name, "current",
            sizeof(validation_find_struct_type_by_name(&unit, "Counter")->methods[1].source_name)) != 0)
    {
        std::printf("Assertion failed: second class method should be current\n");
        goto cleanup;
    }
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Counter")->methods[1].return_kind,
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
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->constructor_count, 1,
            "class should record one constructor overload") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->constructors[0].parameter_count, 1,
            "class should record one constructor parameter") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->method_count, 1,
            "class should record one parameterized method") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->methods[0].parameter_count, 1,
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

FT_TEST(test_cblc_parse_translation_unit_records_out_of_class_method_definitions)
{
    const char *source;
    t_cblc_translation_unit unit;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    void reset();\n"
        "    int current();\n"
        "};\n"
        "void Counter::reset() {\n"
        "    value = 0;\n"
        "    return;\n"
        "}\n"
        "int Counter::current() {\n"
        "    return value;\n"
        "}\n"
        "Counter counter;\n"
        "void main()\n"
        "{\n"
        "    counter.reset();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "out-of-class method sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->method_count, 2,
            "class should retain declared methods") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Counter")->methods[0].has_definition, 1,
            "first declared method should gain a later definition") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Counter")->methods[1].has_definition, 1,
            "second declared method should gain a later definition") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_c_emits_out_of_class_method_definitions)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    int status;

    source = "class Counter\n"
        "{\n"
        "    public:\n"
        "    int value;\n"
        "    Counter(int start);\n"
        "    void add(int delta);\n"
        "    int current();\n"
        "};\n"
        "Counter::Counter(int start) {\n"
        "    value = start;\n"
        "}\n"
        "void Counter::add(int delta) {\n"
        "    value = value + delta;\n"
        "    return;\n"
        "}\n"
        "int Counter::current() {\n"
        "    return value;\n"
        "}\n"
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
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "out-of-class lifecycle sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "out-of-class lifecycle sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c)
        goto cleanup;
    if (!ft_strnstr(generated_c,
            "void cblc_method_Counter_add(t_Counter *receiver, int Counter__add__delta)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_method_Counter_add(&counter, 2);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should pass out-of-class method arguments to reusable bodies\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "receiver->value = receiver->value + Counter__add__delta;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should reuse out-of-class method bodies\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "total = cblc_method_Counter_current(&counter);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should call reusable out-of-class int methods\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_c)
        cma_free(generated_c);
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
    if (!ft_strnstr(generated_c, "void cblc_method_Counter_reset(t_Counter *receiver)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "receiver->value = 0;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_method_Counter_reset(&counter);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit and call a reusable void method\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "int cblc_method_Counter_current(t_Counter *receiver)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "return (receiver->value);", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "total = cblc_method_Counter_current(&counter);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should emit and call a reusable returning method\n");
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
    int generation_diagnostic_code;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
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
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Counter counter(7);\n"
        "    counter.add(2);\n"
        "    total = counter.current();\n"
        "    display(total);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    generation_diagnostic_code = 0;
    generation_diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "parameterized class generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "parameterized class generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS)
    {
        std::printf("Parameterized-class COBOL generation diagnostic (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic);
        test_expect_success(FT_FAILURE,
            "parameterized class generation sample should emit COBOL");
        goto cleanup;
    }
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c,
            "void cblc_constructor_Counter_0(t_Counter *receiver, int Counter__ctor__start)",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should declare a parameterized constructor function\n");
        goto cleanup;
    }
    if (ft_strnstr(generated_c, "static int Counter__ctor__start", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: reusable constructor should not allocate global parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "cblc_constructor_Counter_0(&main__counter, 7);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should pass constructor arguments directly\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "receiver->value = Counter__ctor__start;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use the direct constructor parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "void cblc_method_Counter_add(t_Counter *receiver, int Counter__add__delta)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_method_Counter_add(&main__counter, 2);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should pass method arguments to reusable method bodies\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "receiver->value = receiver->value + Counter__add__delta;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use method parameter inside reusable method body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "01 COUNTER-CTOR-START PIC S9(9).", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should declare constructor parameter storage\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE 7 TO COUNTER-CTOR-START.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign constructor argument before lifecycle body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE MAIN-COUNTER-VALUE = COUNTER-CTOR-START.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use constructor parameter inside lifecycle body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE 2 TO COUNTER-CBLC-USER-ADD-DELTA.", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "PERFORM CBLC-METHOD-MAIN-COUNTER-CBLC-USER-ADD.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should assign method arguments before performing the method paragraph; output follows:\n%s\n",
            generated_cobol);
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "COMPUTE MAIN-COUNTER-CBLC-USER-VALUE = MAIN-COUNTER-CBLC-USER-VALUE + COUNTER-CBLC-USER-ADD-DELTA.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use method parameter inside reusable method paragraph\n");
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

FT_TEST(test_cblc_method_overloads_resolve_by_argument_conversion)
{
    const char *source;
    const t_cblc_struct_type *type;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    const t_cblc_data_item *receiver;
    char expected_cobol_call[3][TRANSPILE_STATEMENT_TEXT_MAX];
    char expected_cobol_argument[3][TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t data_index;
    int cobol_diagnostic_code;
    int status;

    source = "class Chooser\n"
        "{\n"
        "    int value;\n"
        "    public:\n"
        "    void choose(int input) { value = input; return; }\n"
        "    void choose(double input) { value = 2; return; }\n"
        "    void accept(double input) { value = 3; return; }\n"
        "};\n"
        "Chooser chooser;\n"
        "void main()\n"
        "{\n"
        "    chooser.choose(3);\n"
        "    chooser.choose(3.5);\n"
        "    chooser.accept(9);\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    cobol_diagnostic_code = 0;
    cobol_diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "overloaded methods with distinct parameter types should parse") != FT_SUCCESS)
        goto cleanup;
    type = validation_find_struct_type_by_name(&unit, "Chooser");
    if (!type || test_expect_size_t_equal(type->method_count, 3,
            "both overloads and the widening-only method should be retained") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(type->methods[0].overload_index), 0,
            "first method overload should retain the unsuffixed identity") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(type->methods[1].overload_index), 1,
            "second method overload should have a unique overload identity") != FT_SUCCESS
        || test_expect_success(std::strcmp(type->methods[0].cobol_name,
                type->methods[1].cobol_name) != 0 ? FT_SUCCESS : FT_FAILURE,
            "overloaded method paragraphs must have distinct COBOL names") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "overloaded method calls should generate C") != FT_SUCCESS
        || !generated_c
        || !std::strstr(generated_c, "cblc_method_Chooser_choose(&chooser, 3);")
        || !std::strstr(generated_c,
            "cblc_method_Chooser_choose_overload_1(&chooser, 3.5);")
        || !std::strstr(generated_c,
            "cblc_method_Chooser_accept(&chooser, 9);"))
    {
        std::printf("Assertion failed: each call should dispatch to its lowest-cost matching method overload\n");
        goto cleanup;
    }
    receiver = NULL;
    data_index = 0;
    while (data_index < unit.data_count)
    {
        if (std::strcmp(unit.data_items[data_index].source_name, "chooser") == 0
            && unit.data_items[data_index].kind == CBLC_DATA_KIND_STRUCT)
        {
            receiver = &unit.data_items[data_index];
            break ;
        }
        data_index += 1;
    }
    if (!receiver
        || std::snprintf(expected_cobol_call[0], sizeof(expected_cobol_call[0]),
            "PERFORM CBLC-METHOD-%s-%s.", receiver->cobol_name,
            type->methods[0].cobol_name) < 0
        || std::snprintf(expected_cobol_call[1], sizeof(expected_cobol_call[1]),
            "PERFORM CBLC-METHOD-%s-%s.", receiver->cobol_name,
            type->methods[1].cobol_name) < 0
        || std::snprintf(expected_cobol_argument[0],
            sizeof(expected_cobol_argument[0]), "MOVE 3 TO %s.",
            type->methods[0].parameters[0].cobol_name) < 0
        || std::snprintf(expected_cobol_argument[1],
            sizeof(expected_cobol_argument[1]), "COMPUTE %s = 3.5.",
            type->methods[1].parameters[0].cobol_name) < 0
        || std::snprintf(expected_cobol_call[2],
            sizeof(expected_cobol_call[2]), "PERFORM CBLC-METHOD-%s-%s.",
            receiver->cobol_name, type->methods[2].cobol_name) < 0
        || std::snprintf(expected_cobol_argument[2],
            sizeof(expected_cobol_argument[2]), "COMPUTE %s = 9.",
            type->methods[2].parameters[0].cobol_name) < 0
        || cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &cobol_diagnostic_code, cobol_diagnostic,
            sizeof(cobol_diagnostic)) != FT_SUCCESS
        || !generated_cobol
        || !std::strstr(generated_cobol, expected_cobol_call[0])
        || !std::strstr(generated_cobol, expected_cobol_call[1])
        || !std::strstr(generated_cobol, expected_cobol_call[2])
        || !std::strstr(generated_cobol, expected_cobol_argument[0])
        || !std::strstr(generated_cobol, expected_cobol_argument[1])
        || !std::strstr(generated_cobol, expected_cobol_argument[2]))
    {
        std::printf("Assertion failed: COBOL method calls should use the exact typed overload target; diagnostic %d: %s\n",
            cobol_diagnostic_code, cobol_diagnostic);
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
    if (test_expect_size_t_equal(validation_find_first_non_string_type(&unit)->constructor_count, 2,
            "class should record two constructor overloads") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_first_non_string_type(&unit)->has_default_constructor, 1,
            "class should keep a default constructor when zero-arg overload exists") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(validation_find_first_non_string_type(&unit)->constructors[0].parameter_count, 0,
            "first constructor overload should be zero-argument") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_size_t_equal(validation_find_first_non_string_type(&unit)->constructors[1].parameter_count, 1,
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
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Counter first;\n"
        "    Counter second(7);\n"
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
    if (!ft_strnstr(generated_c,
            "void cblc_constructor_Counter_0(t_Counter *receiver)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "receiver->value = 1;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_constructor_Counter_0(&main__first);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use the zero-argument constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
            "void cblc_constructor_Counter_1(t_Counter *receiver, int Counter__ctor_1__start)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_constructor_Counter_1(&main__second, 7);",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "receiver->value = Counter__ctor_1__start;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should reuse the one-argument constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "COMPUTE MAIN-FIRST-VALUE = 1.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should use the zero-argument constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE 7 TO COUNTER-CTOR-1-START.", std::strlen(generated_cobol)))
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
    if (test_expect_size_t_equal(validation_find_struct_type_by_name(&unit, "Counter")->constructor_count, 2,
            "class should record both int and copy constructor overloads") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Counter")->constructors[0].parameters[0].kind,
            TRANSPILE_FUNCTION_PARAMETER_INT,
            "first overload should keep the int parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(validation_find_struct_type_by_name(&unit, "Counter")->constructors[1].parameters[0].kind,
            TRANSPILE_FUNCTION_PARAMETER_STRUCT,
            "second overload should record the struct parameter kind") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(validation_find_struct_type_by_name(&unit, "Counter")->constructors[1].parameters[0].type_name,
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
    int generation_diagnostic_code;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
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
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Counter seeded(7);\n"
        "    Counter copy(seeded);\n"
        "    total = copy.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    generation_diagnostic_code = 0;
    generation_diagnostic[0] = '\0';
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy constructor generation sample should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "copy constructor generation sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS)
    {
        std::printf("Copy-constructor COBOL generation diagnostic (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic);
        test_expect_success(FT_FAILURE,
            "copy constructor generation sample should emit COBOL");
        goto cleanup;
    }
    if (!generated_c || !generated_cobol)
        goto cleanup;
    if (!ft_strnstr(generated_c, "cblc_constructor_Counter_0(&main__seeded, 7);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should call the int constructor directly\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c,
        "void cblc_constructor_Counter_1(t_Counter *receiver, t_Counter Counter__ctor_1__other)",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c,
            "cblc_constructor_Counter_1(&main__copy, main__seeded);", std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should pass the copy constructor directly; output follows:\n%s\n",
            generated_c);
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "receiver->value = Counter__ctor_1__other.value;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should use the copy constructor body against the struct parameter\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE MAIN-SEEDED TO COUNTER-CTOR-1-OTHER.",
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

FT_TEST(test_cblc_generate_c_and_cobol_resolve_nested_local_class_copy_argument)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    size_t data_index;
    int saw_canonical_local_field;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter() { value = 5; }\n"
        "    Counter(Counter source) { this.value = source.value; }\n"
        "    int current() { return value; }\n"
        "};\n"
        "struct Holder { Counter value; };\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Holder holder;\n"
        "    Counter copy(holder.value);\n"
        "    total = copy.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    saw_canonical_local_field = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "nested local class copy sample should parse") != FT_SUCCESS)
        goto cleanup;
    for (data_index = 0; data_index < unit.data_count; ++data_index)
    {
        const t_cblc_data_item *item = &unit.data_items[data_index];

        if (item->is_function_local && !item->is_alias
            && std::strcmp(item->local_source_name, "holder.value") == 0
            && item->kind == CBLC_DATA_KIND_STRUCT
            && std::strcmp(item->struct_type_name, "Counter") == 0)
        {
            saw_canonical_local_field = 1;
            break;
        }
    }
    if (test_expect_int_equal(saw_canonical_local_field, 1,
            "nested local fields should retain their source path for scoped type resolution")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "nested local class copy sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested local class copy sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol
        || !ft_strnstr(generated_c,
            "cblc_constructor_Counter_1(&main__copy, main__holder.value);",
            std::strlen(generated_c)))
    {
        std::printf("Nested class argument did not resolve to the canonical local field; generated C follows:\n%s\n",
            generated_c ? generated_c : "<null>");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "MOVE MAIN-HOLDER-VALUE TO COUNTER-CTOR-1-SOURCE.",
            std::strlen(generated_cobol)))
    {
        std::printf("Generated COBOL did not bind the nested class field to the copy-constructor parameter\n");
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

FT_TEST(test_cblc_generate_c_and_cobol_resolve_deep_local_class_copy_argument)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_c;
    char *generated_cobol;
    size_t data_index;
    int saw_canonical_local_field;
    int status;

    source = "class Counter\n"
        "{\n"
        "    private:\n"
        "    int value;\n"
        "    public:\n"
        "    Counter() { value = 5; }\n"
        "    Counter(Counter source) { this.value = source.value; }\n"
        "    int current() { return value; }\n"
        "};\n"
        "struct Inner { Counter value; };\n"
        "struct Middle { Inner inner; };\n"
        "struct Holder { Middle middle; };\n"
        "int total;\n"
        "void main()\n"
        "{\n"
        "    Holder holder;\n"
        "    Counter copy(holder.middle.inner.value);\n"
        "    total = copy.current();\n"
        "    return;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_c = NULL;
    generated_cobol = NULL;
    saw_canonical_local_field = 0;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "deep nested local class copy sample should parse") != FT_SUCCESS)
        goto cleanup;
    for (data_index = 0; data_index < unit.data_count; ++data_index)
    {
        const t_cblc_data_item *item = &unit.data_items[data_index];

        if (item->is_function_local && !item->is_alias
            && std::strcmp(item->local_source_name,
                "holder.middle.inner.value") == 0
            && item->kind == CBLC_DATA_KIND_STRUCT
            && std::strcmp(item->struct_type_name, "Counter") == 0)
        {
            saw_canonical_local_field = 1;
            break;
        }
    }
    if (test_expect_int_equal(saw_canonical_local_field, 1,
            "deep nested local fields should retain their complete source path")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_c(&unit, &generated_c),
            "deep nested local class copy sample should emit C") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "deep nested local class copy sample should emit COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_c || !generated_cobol
        || !ft_strnstr(generated_c,
            "cblc_constructor_Counter_1(&main__copy, main__holder.middle.inner.value);",
            std::strlen(generated_c)))
    {
        std::printf("Deep nested class argument did not resolve to the canonical local field; generated C follows:\n%s\n",
            generated_c ? generated_c : "<null>");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "MOVE MAIN-HOLDER-MIDDLE-INNER-VALUE TO COUNTER-CTOR-1-SOURCE.",
            std::strlen(generated_cobol)))
    {
        std::printf("Generated COBOL did not bind the deep nested class field to the copy-constructor parameter\n");
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
    if (!ft_strnstr(generated_cobol, "PERFORM CBLC-METHOD-COUNTER-RESET.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "CBLC-METHOD-COUNTER-RESET.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should call a reusable void method paragraph\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "PERFORM CBLC-METHOD-COUNTER-CURRENT.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "COMPUTE CBLC-RESULT-COUNTER-CURRENT = COUNTER-CBLC-USER-VALUE.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "COMPUTE TOTAL = CBLC-RESULT-COUNTER-CURRENT.",
            std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should reuse returning method paragraphs and result slots\n");
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
    first_assign = std::strstr(generated_c, "receiver->first = 1;");
    second_assign = std::strstr(generated_c, "receiver->second = 2;");
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
    if (!ft_strnstr(generated_c, "cblc_constructor_Pair_0(&pair);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should call the reusable constructor body\n");
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
        "    int name;\n"
        "    Person() : name(8), id(7) {\n"
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
    if (!ft_strnstr(generated_c, "void cblc_constructor_Person_0(t_Person *receiver);",
            std::strlen(generated_c))
        || !ft_strnstr(generated_c, "receiver->id = 7;", std::strlen(generated_c))
        || !ft_strnstr(generated_c, "cblc_constructor_Person_0(&person);",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: generated C should reuse the initializer-list constructor body\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_c, "receiver->name = 8;",
            std::strlen(generated_c)))
    {
        std::printf("Assertion failed: reusable constructor should initialize ordinary members through receiver\n");
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

FT_TEST(test_cblc_records_copy_constructor_and_assignment_operator)
{
    const char *source;
    t_cblc_translation_unit unit;
    const t_cblc_struct_type *box_type;
    int status;

    source = "class Box\n"
        "{\n"
        "    int value;\n"
        "    Box() { value = 0; }\n"
        "    Box(Box source);\n"
        "    void operator=(Box source);\n"
        "    ~Box();\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    Box first;\n"
        "    Box second(first);\n"
        "    second = first;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy operations should parse") != FT_SUCCESS)
        goto cleanup;
    box_type = validation_find_struct_type_by_name(&unit, "Box");
    if (!box_type)
    {
        std::printf("Assertion failed: parsed Box type should be discoverable by source name\n");
        goto cleanup;
    }
    if (test_expect_int_equal(box_type->has_copy_constructor, 1,
            "class should record a copy constructor") != FT_SUCCESS
        || test_expect_int_equal(box_type->has_copy_assignment, 1,
            "class should record a copy assignment operator") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_copy_operations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "class Box\n"
        "{\n"
        "    int value;\n"
        "    Box() { value = 0; }\n"
        "    Box(Box source) { value = source.value; }\n"
        "    void operator=(Box source) { value = source.value; }\n"
        "    ~Box() { value = 0; }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    Box first;\n"
        "    Box second(first);\n"
        "    second = first;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "copy operation source should parse") != FT_SUCCESS
        || test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "copy operation source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "MOVE MAIN-FIRST TO BOX-CTOR-1-SOURCE.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "COMPUTE MAIN-SECOND-CBLC-USER-VALUE =", std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "MOVE MAIN-FIRST TO BOX-OPERATOR-ASSIGN-SOURCE.", std::strlen(generated_cobol)))
    {
        std::printf("Assertion failed: generated COBOL should lower copy construction and assignment\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_emits_native_new_delete_lifecycle)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;
    const char *based_declaration;

    source = "class Box\n"
        "{\n"
        "    int value;\n"
        "    Box() { value = 3; }\n"
        "    ~Box() { value = 0; }\n"
        "};\n"
        "void main()\n"
        "{\n"
        "    Box *value;\n"
        "    value = new Box();\n"
        "    delete value;\n"
        "}\n";
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    status = FT_FAILURE;
    if (test_expect_success(cblc_parse_translation_unit(source, &unit),
            "new and delete source should parse") != FT_SUCCESS
        || test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "new and delete source should generate COBOL") != FT_SUCCESS)
        goto cleanup;
    based_declaration = generated_cobol
        ? ft_strnstr(generated_cobol, "01 CBLC-SPTR-MAIN-VALUE BASED.",
            std::strlen(generated_cobol)) : NULL;
    if (!generated_cobol
        || !ft_strnstr(generated_cobol, "ALLOCATE 4 CHARACTERS RETURNING MAIN-VALUE",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol,
            "SET ADDRESS OF CBLC-SPTR-MAIN-VALUE TO MAIN-VALUE.",
            std::strlen(generated_cobol))
        || !ft_strnstr(generated_cobol, "FREE MAIN-VALUE", std::strlen(generated_cobol))
        || !based_declaration
        || ft_strnstr(based_declaration + std::strlen("01 CBLC-SPTR-MAIN-VALUE BASED."),
            "01 CBLC-SPTR-MAIN-VALUE BASED.",
            std::strlen(generated_cobol)
                - static_cast<size_t>(based_declaration - generated_cobol)
                - std::strlen("01 CBLC-SPTR-MAIN-VALUE BASED.")) != NULL)
    {
        std::printf("Assertion failed: new/delete should emit one based object and both lifecycle calls\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}
