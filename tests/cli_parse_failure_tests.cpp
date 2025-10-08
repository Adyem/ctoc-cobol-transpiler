#include "transpiler_cli.hpp"

#include "test_suites.hpp"
#include "cli_test_registry.hpp"

FT_TEST(test_cli_rejects_mismatched_path_counts)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "first.cblc",
        "--input",
        "second.cblc",
        "--output",
        "only.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 9, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject mismatched path counts\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_unknown_direction)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "invalid-direction",
        "--input",
        "first.cblc",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 7, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject unknown direction\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_missing_direction_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "--input",
        "first.cblc",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 7, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject missing direction value\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_requires_direction_without_environment)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--input",
        "first.cblc",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 5, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should require direction without CTOC_DEFAULT_DIRECTION\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_requires_input_path)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 5, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should require at least one input path\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_requires_output_path)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "first.cblc"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 5, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should require at least one output path\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_unknown_option)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--unknown",
        "value",
        "--input",
        "first.cblc",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 9, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject unknown option\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_invalid_format)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--format",
        "fancy",
        "--input",
        "first.cblc",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 9, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject unknown format\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_invalid_diagnostics)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--diagnostics",
        "extreme",
        "--input",
        "first.cblc",
        "--output",
        "first.cob"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 9, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject unknown diagnostics level\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_missing_input_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 4, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject missing input value\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_missing_output_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "first.cblc",
        "--output"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 6, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject missing output value\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_missing_output_directory_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "first.cblc",
        "--output",
        "first.cob",
        "--output-dir"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 8, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject missing output directory value\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_missing_format_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "first.cblc",
        "--output",
        "first.cob",
        "--format"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 8, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject missing format value\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_rejects_missing_diagnostics_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "first.cblc",
        "--output",
        "first.cob",
        "--diagnostics"
    };
    t_transpiler_cli_options options;

    if (transpiler_cli_parse(&options, 8, argv) == FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        pf_printf("Assertion failed: transpiler_cli_parse should reject missing diagnostics value\\n");
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

const t_test_case *get_cli_parse_failure_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cli_rejects_mismatched_path_counts", test_cli_rejects_mismatched_path_counts},
        {"cli_rejects_unknown_direction", test_cli_rejects_unknown_direction},
        {"cli_rejects_missing_direction_value", test_cli_rejects_missing_direction_value},
        {"cli_requires_direction_without_environment", test_cli_requires_direction_without_environment},
        {"cli_requires_input_path", test_cli_requires_input_path},
        {"cli_requires_output_path", test_cli_requires_output_path},
        {"cli_rejects_unknown_option", test_cli_rejects_unknown_option},
        {"cli_rejects_invalid_format", test_cli_rejects_invalid_format},
        {"cli_rejects_invalid_diagnostics", test_cli_rejects_invalid_diagnostics},
        {"cli_rejects_missing_input_value", test_cli_rejects_missing_input_value},
        {"cli_rejects_missing_output_value", test_cli_rejects_missing_output_value},
        {"cli_rejects_missing_output_directory_value", test_cli_rejects_missing_output_directory_value},
        {"cli_rejects_missing_format_value", test_cli_rejects_missing_format_value},
        {"cli_rejects_missing_diagnostics_value", test_cli_rejects_missing_diagnostics_value}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
