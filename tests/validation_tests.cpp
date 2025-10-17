#include "test_suites.hpp"

#include "libft/CMA/CMA.hpp"

FT_TEST(test_transpiler_validation_accepts_valid_cblc)
{
    const char *source;

    source = "function void main()\n"
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

    source = "function void main()\n"
        "{\n"
        "    display(\"HELLO\");\n"
        "}\n";
    if (transpiler_validate_generated_cblc(source) != FT_FAILURE)
    {
        pf_printf("Assertion failed: validator should reject CBL-C without return\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_validation_accepts_string_declaration)
{
    const char *source;

    source = "string greeting[16];\n"
        "function void main()\n"
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

    source = "string greeting[12];\n"
        "string copy[12];\n"
        "int total;\n"
        "function void main()\n"
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

FT_TEST(test_cblc_generate_cobol_emits_string_group)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "string greeting[8];\n"
        "function void main()\n"
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
    if (!ft_strnstr(generated_cobol, "01 GREETING.", ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: generated COBOL should declare GREETING group\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 GREETING-LEN PIC 9(4) COMP VALUE 8.",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: generated COBOL should declare GREETING length field\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "05 GREETING-BUF PIC X(8).",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: generated COBOL should declare GREETING buffer\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY GREETING-BUF(1:GREETING-LEN)",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: generated COBOL should display buffer slice using length\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_cblc_generate_cobol_handles_string_assignments_and_length_computations)
{
    const char *source;
    t_cblc_translation_unit unit;
    char *generated_cobol;
    int status;

    source = "string greeting[8];\n"
        "string target[8];\n"
        "int total;\n"
        "function void main()\n"
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
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: literal assignment should target GREETING-BUF\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE 5 TO GREETING-LEN",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: literal assignment should update GREETING-LEN\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "MOVE GREETING TO TARGET",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: string to string assignment should move groups\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol,
            "COMPUTE TOTAL = GREETING-LEN + TARGET-LEN",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: length expressions should translate into COMPUTE\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY TARGET-BUF(1:TARGET-LEN)",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: DISPLAY of string should use buffer slice\n");
        goto cleanup;
    }
    if (!ft_strnstr(generated_cobol, "DISPLAY TOTAL",
            ft_strlen(generated_cobol)))
    {
        pf_printf("Assertion failed: DISPLAY of int should emit standard form\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
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
        pf_printf("Assertion failed: validator should reject invalid COBOL text\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

const t_test_case *get_validation_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_validation_accepts_valid_cblc", test_transpiler_validation_accepts_valid_cblc},
        {"transpiler_validation_rejects_cblc_without_return", test_transpiler_validation_rejects_cblc_without_return},
        {"transpiler_validation_accepts_string_declaration", test_transpiler_validation_accepts_string_declaration},
        {"transpiler_validation_accepts_string_assignment_and_length_usage",
            test_transpiler_validation_accepts_string_assignment_and_length_usage},
        {"cblc_generate_cobol_emits_string_group", test_cblc_generate_cobol_emits_string_group},
        {"cblc_generate_cobol_handles_string_assignments_and_length_computations",
            test_cblc_generate_cobol_handles_string_assignments_and_length_computations},
        {"transpiler_validation_accepts_valid_cobol", test_transpiler_validation_accepts_valid_cobol},
        {"transpiler_validation_accepts_working_storage_program", test_transpiler_validation_accepts_working_storage_program},
        {"transpiler_validation_rejects_invalid_cobol", test_transpiler_validation_rejects_invalid_cobol}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
