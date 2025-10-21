#include <cstddef>

#include "test_suites.hpp"
#include "cli_test_registry.hpp"

static int expect_cli_parse_failure(const char **argv, size_t argc,
    const char *expected_output, const char *assertion)
{
    t_transpiler_cli_options options;
    t_test_output_capture stdout_capture;
    char buffer[512];
    int status;

    if (!argv || !expected_output || !assertion)
        return (FT_FAILURE);
    if (test_capture_stdout_begin(&stdout_capture) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should capture stdout\\n");
        return (FT_FAILURE);
    }
    status = transpiler_cli_parse(&options, static_cast<int>(argc), argv);
    transpiler_cli_options_dispose(&options);
    if (test_capture_stdout_end(&stdout_capture, buffer, sizeof(buffer), NULL) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test harness should restore stdout\\n");
        return (FT_FAILURE);
    }
    if (status == FT_SUCCESS)
    {
        pf_printf("Assertion failed: %s\\n", assertion);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(buffer, expected_output, assertion) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Input and output file counts must match (got 2 inputs and 1 outputs).\n",
        "CLI should reject mismatched path counts"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Unknown direction 'invalid-direction'. Expected 'cblc-to-cobol', 'cblc-to-c', 'cobol-to-cblc', or 'standard-library'.\n",
        "CLI should reject unknown direction"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Unknown direction '--input'. Expected 'cblc-to-cobol', 'cblc-to-c', 'cobol-to-cblc', or 'standard-library'.\n",
        "CLI should reject missing direction value"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing required --direction option or CTOC_DEFAULT_DIRECTION environment variable.\n",
        "CLI should require direction without CTOC_DEFAULT_DIRECTION"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing required --input option.\n",
        "CLI should require input path"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing required --output option.\n",
        "CLI should require output path"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Unknown option '--unknown'.\n",
        "CLI should report unknown option"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Unknown format 'fancy'. Expected 'default', 'minimal', or 'pretty'.\n",
        "CLI should reject unknown format"));
}

FT_TEST(test_cli_rejects_invalid_layout)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cobol-to-cblc",
        "--layout",
        "sideways",
        "--input",
        "legacy.cob",
        "--output",
        "modern.cblc"
    };
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Unknown layout 'sideways'. Expected 'normalize' or 'preserve'.\n",
        "CLI should reject unknown layout"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Unknown diagnostics level 'extreme'. Expected 'silent', 'normal', or 'verbose'.\n",
        "CLI should reject unknown diagnostics"));
}

FT_TEST(test_cli_rejects_missing_layout_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cobol-to-cblc",
        "--input",
        "legacy.cob",
        "--output",
        "modern.cblc",
        "--layout"
    };
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing value for --layout option.\n",
        "CLI should report missing layout value"));
}

FT_TEST(test_cli_rejects_missing_input_value)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input"
    };
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing value for --input option.\n",
        "CLI should reject missing input value"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing value for --output option.\n",
        "CLI should reject missing output value"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing value for --output-dir option.\n",
        "CLI should reject missing output directory value"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing value for --format option.\n",
        "CLI should reject missing format value"));
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
    size_t argc;

    argc = sizeof(argv) / sizeof(argv[0]);
    return (expect_cli_parse_failure(argv, argc,
        "Missing value for --diagnostics option.\n",
        "CLI should report missing diagnostics value"));
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
        {"cli_rejects_invalid_layout", test_cli_rejects_invalid_layout},
        {"cli_rejects_invalid_diagnostics", test_cli_rejects_invalid_diagnostics},
        {"cli_rejects_missing_input_value", test_cli_rejects_missing_input_value},
        {"cli_rejects_missing_output_value", test_cli_rejects_missing_output_value},
        {"cli_rejects_missing_output_directory_value", test_cli_rejects_missing_output_directory_value},
        {"cli_rejects_missing_format_value", test_cli_rejects_missing_format_value},
        {"cli_rejects_missing_layout_value", test_cli_rejects_missing_layout_value},
        {"cli_rejects_missing_diagnostics_value", test_cli_rejects_missing_diagnostics_value}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
