#include "transpiler_cli.hpp"

#include <cstdlib>

#include "test_suites.hpp"

static int test_set_environment(const char *name, const char *value)
{
#if defined(_WIN32)
    if (_putenv_s(name, value) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
#else
    if (setenv(name, value, 1) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
#endif
}

static void test_unset_environment(const char *name)
{
#if defined(_WIN32)
    _putenv_s(name, "");
#else
    unsetenv(name);
#endif
}

static int test_cli_parse_direction_flag(void)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "input.cblc",
        "--output",
        "output.cob"
    };
    t_transpiler_cli_options options;

    if (test_expect_success(transpiler_cli_parse(&options, 7, argv), "transpiler_cli_parse should accept explicit direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.source_language, TRANSPILE_LANGUAGE_CBL_C, "source language should match direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.target_language, TRANSPILE_LANGUAGE_COBOL, "target language should match direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(options.input_path, "input.cblc", "input path should be recorded") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(options.output_path, "output.cob", "output path should be recorded") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.show_help, 0, "show_help should remain disabled") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_cli_direction_from_environment(void)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--input",
        "program.cob",
        "--output",
        "program.cblc"
    };
    t_transpiler_cli_options options;

    if (test_set_environment("CTOC_DEFAULT_DIRECTION", "cobol-to-cblc") != FT_SUCCESS)
    {
        pf_printf("Assertion failed: test_set_environment should succeed\n");
        return (FT_FAILURE);
    }
    if (transpiler_cli_parse(&options, 5, argv) != FT_SUCCESS)
    {
        test_unset_environment("CTOC_DEFAULT_DIRECTION");
        pf_printf("Assertion failed: transpiler_cli_parse should use CTOC_DEFAULT_DIRECTION\n");
        return (FT_FAILURE);
    }
    test_unset_environment("CTOC_DEFAULT_DIRECTION");
    if (test_expect_int_equal(options.source_language, TRANSPILE_LANGUAGE_COBOL, "environment should set source language") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.target_language, TRANSPILE_LANGUAGE_CBL_C, "environment should set target language") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_cli_help_short_circuits_validation(void)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--help"
    };
    t_transpiler_cli_options options;

    if (test_expect_success(transpiler_cli_parse(&options, 2, argv), "transpiler_cli_parse should succeed for --help") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.show_help, 1, "--help should enable usage flag") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_cli_optional_configuration(void)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cobol-to-cblc",
        "--input",
        "legacy.cob",
        "--output",
        "modern.cblc",
        "--output-dir",
        "out",
        "--format",
        "pretty",
        "--diagnostics",
        "verbose"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 13, argv), "transpiler_cli_parse should accept configuration flags") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.output_directory, "out", "output directory should be recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.format_mode, TRANSPILE_FORMAT_PRETTY, "format mode should map to enum") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.diagnostic_level, TRANSPILE_DIAGNOSTIC_VERBOSE, "diagnostic level should map to enum") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.output_directory, "out", "context should receive output directory") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.format_mode, TRANSPILE_FORMAT_PRETTY, "context should store format mode") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostic_level, TRANSPILE_DIAGNOSTIC_VERBOSE, "context should store diagnostic level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

const t_test_case *get_cli_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cli_parse_direction_flag", test_cli_parse_direction_flag},
        {"cli_direction_from_environment", test_cli_direction_from_environment},
        {"cli_help_short_circuits_validation", test_cli_help_short_circuits_validation},
        {"cli_optional_configuration", test_cli_optional_configuration}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

