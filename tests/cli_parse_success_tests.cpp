#include <cstdlib>

#include "test_suites.hpp"
#include "cli_test_registry.hpp"

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

FT_TEST(test_cli_parse_direction_flag)
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

    if (test_expect_success(transpiler_cli_parse(&options, 7, argv),
            "transpiler_cli_parse should accept explicit direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.source_language, TRANSPILE_LANGUAGE_CBL_C,
            "source language should match direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.target_language, TRANSPILE_LANGUAGE_COBOL,
            "target language should match direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(options.input_count), 1,
            "one input path should be recorded") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(options.input_paths[0], "input.cblc",
            "input path should be recorded") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(options.output_count), 1,
            "one output path should be recorded") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(options.output_paths[0], "output.cob",
            "output path should be recorded") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.show_help, 0,
            "show_help should remain disabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_direction_from_environment)
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
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    test_unset_environment("CTOC_DEFAULT_DIRECTION");
    if (test_expect_int_equal(options.source_language, TRANSPILE_LANGUAGE_COBOL,
            "environment should set source language") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.target_language, TRANSPILE_LANGUAGE_CBL_C,
            "environment should set target language") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_help_short_circuits_validation)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--help"
    };
    t_transpiler_cli_options options;

    if (test_expect_success(transpiler_cli_parse(&options, 2, argv),
            "transpiler_cli_parse should succeed for --help") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.show_help, 1,
            "--help should enable usage flag") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_optional_configuration)
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
        "verbose",
        "--layout",
        "preserve"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 15, argv),
            "transpiler_cli_parse should accept configuration flags") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(options.input_count), 1,
            "one input should remain recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.input_paths[0], "legacy.cob",
            "input path should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(options.output_count), 1,
            "one output should remain recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.output_paths[0], "modern.cblc",
            "output path should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.output_directory, "out",
            "output directory should be recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.format_mode, TRANSPILE_FORMAT_PRETTY,
            "format mode should map to enum") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.diagnostic_level, TRANSPILE_DIAGNOSTIC_VERBOSE,
            "diagnostic level should map to enum") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.layout_mode, TRANSPILE_LAYOUT_PRESERVE,
            "layout mode should map to enum") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.source_count), 1,
            "context should record one source path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_paths[0], "legacy.cob",
            "context should store input path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.target_count), 1,
            "context should record one output path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_paths[0], "modern.cblc",
            "context should store output path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.output_directory, "out",
            "context should receive output directory") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.format_mode, TRANSPILE_FORMAT_PRETTY,
            "context should store format mode") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostic_level, TRANSPILE_DIAGNOSTIC_VERBOSE,
            "context should store diagnostic level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.layout_mode, TRANSPILE_LAYOUT_PRESERVE,
            "context should store layout mode") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_dump_copybook_graph_option)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cobol-to-cblc",
        "--input",
        "input.cob",
        "--output",
        "output.cblc",
        "--dump-copybook-graph",
        "graphs"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 9, argv),
            "transpiler_cli_parse should accept --dump-copybook-graph") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.dump_copybook_graph, 1,
            "--dump-copybook-graph should enable dumping") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.dump_copybook_graph_directory, "graphs",
            "--dump-copybook-graph should record directory") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_get_copybook_graph_enabled(&context), 1,
            "context should receive copybook graph flag") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(transpiler_context_get_copybook_graph_directory(&context), "graphs",
            "context should retain copybook graph directory") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_dump_semantic_ir_option)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cobol-to-cblc",
        "--input",
        "input.cob",
        "--output",
        "output.cblc",
        "--dump-semantic-ir",
        "snapshots"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 9, argv),
            "transpiler_cli_parse should accept --dump-semantic-ir") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.dump_semantic_ir, 1,
            "--dump-semantic-ir should enable dumping") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.dump_semantic_ir_directory, "snapshots",
            "--dump-semantic-ir should record directory") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(transpiler_context_get_semantic_diff_enabled(&context), 1,
            "context should receive semantic diff flag") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(transpiler_context_get_semantic_diff_directory(&context), "snapshots",
            "context should retain semantic diff directory") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_standard_library_direction)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "standard-library",
        "--output-dir",
        "library"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 5, argv),
            "transpiler_cli_parse should accept standard-library direction") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.emit_standard_library, 1,
            "standard-library direction should enable library emission") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(options.input_count), 0,
            "standard-library direction should not record inputs") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(options.output_directory, "library",
            "output directory should be recorded for library builds") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.emit_standard_library, 1,
            "context should record standard-library emission request") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_enables_warning_escalation)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "input.cblc",
        "--output",
        "output.cob",
        "--warnings-as-errors"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 8, argv),
            "transpiler_cli_parse should accept --warnings-as-errors") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.warnings_as_errors, 1,
            "warnings-as-errors flag should be enabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_cli_apply(&options, &context),
            "transpiler_cli_apply should propagate warnings-as-errors") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.warnings_as_errors, 1,
            "context should enable warning escalation") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_warning_group_toggles)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "input.cblc",
        "--output",
        "output.cob",
        "-Wno-conversion",
        "-Wno-overflow",
        "-Wno-string-trunc",
        "-Wstring-trunc",
        "-Wno-shadow",
        "-Wno-unused",
        "-Werror"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 14, argv),
            "transpiler_cli_parse should accept -W flags") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(options.warning_settings.conversion, 0,
            "conversion warnings should be disabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.warning_settings.overflow, 0,
            "overflow warnings should be disabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.warning_settings.string_truncation, 1,
            "string truncation warnings should be re-enabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.warning_settings.shadow, 0,
            "shadow warnings should be disabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.warning_settings.unused, 0,
            "unused warnings should be disabled") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(options.warnings_as_errors, 1,
            "-Werror should enable warning escalation") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_cli_apply(&options, &context),
            "transpiler_cli_apply should propagate warning toggles") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.warning_settings.conversion, 0,
            "context should disable conversion warnings") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.warning_settings.overflow, 0,
            "context should disable overflow warnings") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.warning_settings.string_truncation, 1,
            "context should enable string truncation warnings") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.warning_settings.shadow, 0,
            "context should disable shadow warnings") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.warning_settings.unused, 0,
            "context should disable unused warnings") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_supports_multiple_inputs)
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
        "first.cob",
        "--output",
        "second.cob"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 11, argv),
            "transpiler_cli_parse should accept multi-file inputs") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_cli_apply(&options, &context),
            "transpiler_cli_apply should populate context paths") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.source_count), 2,
            "context should record two source paths") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.target_count), 2,
            "context should record two target paths") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_paths[0], "first.cblc",
            "first source path should remain accessible") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_paths[1], "second.cblc",
            "second source path should remain accessible") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_paths[0], "first.cob",
            "first target path should remain accessible") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_paths[1], "second.cob",
            "second target path should remain accessible") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_path, "first.cblc",
            "legacy single-source alias should point to first path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_path, "first.cob",
            "legacy single-target alias should point to first path") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_apply_propagates_multi_file_context)
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
        "first.cob",
        "--output",
        "second.cob"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 11, argv),
            "transpiler_cli_parse should accept multi-file inputs") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context),
            "context init should succeed") != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_cli_apply(&options, &context),
            "transpiler_cli_apply should populate context paths") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.source_paths[0], "first.cblc",
            "first source path should remain accessible") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.target_paths[1], "second.cob",
            "second target path should remain accessible") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

const t_test_case *get_cli_parse_success_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cli_parse_direction_flag", test_cli_parse_direction_flag},
        {"cli_direction_from_environment", test_cli_direction_from_environment},
        {"cli_help_short_circuits_validation", test_cli_help_short_circuits_validation},
        {"cli_optional_configuration", test_cli_optional_configuration},
        {"cli_enables_warning_escalation", test_cli_enables_warning_escalation},
        {"cli_supports_multiple_inputs", test_cli_supports_multiple_inputs},
        {"cli_apply_propagates_multi_file_context", test_cli_apply_propagates_multi_file_context},
        {"cli_dump_copybook_graph_option", test_cli_dump_copybook_graph_option},
        {"cli_dump_semantic_ir_option", test_cli_dump_semantic_ir_option}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
