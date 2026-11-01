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
        std::printf("Assertion failed: test_set_environment should succeed\n");
        return (FT_FAILURE);
    }
    if (transpiler_cli_parse(&options, 5, argv) != FT_SUCCESS)
    {
        test_unset_environment("CTOC_DEFAULT_DIRECTION");
        std::printf("Assertion failed: transpiler_cli_parse should use CTOC_DEFAULT_DIRECTION\n");
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

FT_TEST(test_cli_lifetime_edge_transfer_limit)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "input.cblc",
        "--output",
        "output.cob",
        "--max-lifetime-edge-transfers",
        "0"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 9, argv),
            "CLI should accept a zero lifetime edge-transfer limit") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(options.max_lifetime_edge_transfers, 0,
            "CLI should preserve an explicit zero lifetime edge-transfer limit")
        != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS
        || test_expect_size_t_equal(context.max_lifetime_edge_transfers, 0,
            "CLI apply should copy the lifetime edge-transfer limit into the invocation context")
            != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_template_resource_limits)
{
    const char *argv[] = {
        "ctoc_cobol_transpiler",
        "--direction",
        "cblc-to-cobol",
        "--input",
        "input.cblc",
        "--output",
        "output.cob",
        "--max-template-instantiations",
        "7",
        "--max-template-depth",
        "3",
        "--max-source-bytes",
        "4096"
    };
    t_transpiler_cli_options options;
    t_transpiler_context context;

    if (test_expect_success(transpiler_cli_parse(&options, 13, argv),
            "CLI should accept per-invocation template resource limits")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_size_t_equal(options.max_template_instantiations, 7,
            "CLI should preserve the template-instantiation work limit")
        != FT_SUCCESS
        || test_expect_size_t_equal(options.max_template_instantiation_depth, 3,
            "CLI should preserve the template nesting-depth limit")
            != FT_SUCCESS
        || test_expect_size_t_equal(options.max_source_bytes, 4096,
            "CLI should preserve the aggregate source-byte limit")
            != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS
        || test_expect_size_t_equal(context.max_template_instantiations, 7,
            "CLI apply should copy the template-instantiation limit") != FT_SUCCESS
        || test_expect_size_t_equal(context.max_template_instantiation_depth, 3,
            "CLI apply should copy the template nesting-depth limit") != FT_SUCCESS
        || test_expect_size_t_equal(context.max_source_bytes, 4096,
            "CLI apply should copy the aggregate source-byte limit") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_cli_options_dispose(&options);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_cli_options_dispose(&options);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_lifetime_edge_transfer_limit_reaches_translation_unit)
{
    char directory[256];
    char source_path[256];
    char output_path[256];
    char diagnostic_path[256];
    char command[1024];
    char diagnostic_text[1024];
    int command_length;
    int source_path_length;
    int output_path_length;
    int diagnostic_path_length;

    directory[0] = '\0';
    source_path[0] = '\0';
    output_path[0] = '\0';
    diagnostic_path[0] = '\0';
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    source_path_length = std::snprintf(source_path, sizeof(source_path),
        "%s/lifetime_budget.cblc", directory);
    output_path_length = std::snprintf(output_path, sizeof(output_path),
        "%s/lifetime_budget.cob", directory);
    diagnostic_path_length = std::snprintf(diagnostic_path,
        sizeof(diagnostic_path), "%s/lifetime_budget.diagnostics", directory);
    if (source_path_length < 0
        || static_cast<size_t>(source_path_length) >= sizeof(source_path)
        || output_path_length < 0
        || static_cast<size_t>(output_path_length) >= sizeof(output_path)
        || diagnostic_path_length < 0
        || static_cast<size_t>(diagnostic_path_length) >= sizeof(diagnostic_path)
        || test_write_text_file(source_path,
            "void main() { int value; value = 1; return; }\n") != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\"",
        source_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: ordinary CLI translation should accept the lifetime-budget fixture\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_file(output_path);
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\" --max-lifetime-edge-transfers 0 > \"%s\" 2>&1",
        source_path, output_path, diagnostic_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command_expect_failure(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: CLI zero lifetime budget should fail during translation\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(diagnostic_path, diagnostic_text,
            sizeof(diagnostic_text)) != FT_SUCCESS
        || !std::strstr(diagnostic_text, "configured_limit=0")
        || !std::strstr(diagnostic_text, "observed=1"))
    {
        std::printf("Assertion failed: CLI diagnostic should retain the configured and observed lifetime-analysis limits\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, diagnostic_text,
            sizeof(diagnostic_text)) == FT_SUCCESS)
    {
        std::printf("Assertion failed: failed lifetime analysis must not publish a COBOL output\n");
        test_remove_file(output_path);
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_template_limits_reach_monomorphization)
{
    char directory[256];
    char source_path[256];
    char output_path[256];
    char diagnostic_path[256];
    char command[1024];
    char diagnostic_text[1024];
    int source_path_length;
    int output_path_length;
    int diagnostic_path_length;
    int command_length;

    directory[0] = '\0';
    source_path[0] = '\0';
    output_path[0] = '\0';
    diagnostic_path[0] = '\0';
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    source_path_length = std::snprintf(source_path, sizeof(source_path),
        "%s/template_limits.cblc", directory);
    output_path_length = std::snprintf(output_path, sizeof(output_path),
        "%s/template_limits.cob", directory);
    diagnostic_path_length = std::snprintf(diagnostic_path,
        sizeof(diagnostic_path), "%s/template_limits.diagnostics", directory);
    if (source_path_length < 0
        || static_cast<size_t>(source_path_length) >= sizeof(source_path)
        || output_path_length < 0
        || static_cast<size_t>(output_path_length) >= sizeof(output_path)
        || diagnostic_path_length < 0
        || static_cast<size_t>(diagnostic_path_length) >= sizeof(diagnostic_path)
        || test_write_text_file(source_path,
            "template <typename T> struct box { T value; };\n"
            "box<int> first; box<char> second;\n"
            "void main() { return; }\n") != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\"",
        source_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: default template policy should accept two concrete specializations\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_file(output_path);
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\" --max-template-instantiations 1 > \"%s\" 2>&1",
        source_path, output_path, diagnostic_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command_expect_failure(command) != FT_SUCCESS
        || test_read_text_file(diagnostic_path, diagnostic_text,
            sizeof(diagnostic_text)) != FT_SUCCESS
        || !std::strstr(diagnostic_text, "template instantiation limit exceeded"))
    {
        std::printf("Assertion failed: invocation template-instantiation limit should stop monomorphization with a stable diagnostic\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path,
            "template <typename T> struct box { T value; };\n"
            "box<box<int>> nested;\n"
            "void main() { return; }\n") != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_file(output_path);
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\" --max-template-depth 1 > \"%s\" 2>&1",
        source_path, output_path, diagnostic_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command_expect_failure(command) != FT_SUCCESS
        || test_read_text_file(diagnostic_path, diagnostic_text,
            sizeof(diagnostic_text)) != FT_SUCCESS
        || !std::strstr(diagnostic_text, "template type nesting limit exceeded"))
    {
        std::printf("Assertion failed: invocation template-depth limit should stop nested type parsing with a stable diagnostic\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_source_byte_limit_stops_before_translation)
{
    char directory[256];
    char source_path[256];
    char output_path[256];
    char diagnostic_path[256];
    char command[1024];
    char diagnostic_text[1024];
    const char *source_text;
    int source_path_length;
    int output_path_length;
    int diagnostic_path_length;
    int command_length;
    size_t source_length;
    directory[0] = '\0';
    source_path[0] = '\0';
    output_path[0] = '\0';
    diagnostic_path[0] = '\0';
    source_text = "void main() { return; }\n";
    source_length = std::strlen(source_text);
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    source_path_length = std::snprintf(source_path, sizeof(source_path),
        "%s/source_budget.cblc", directory);
    output_path_length = std::snprintf(output_path, sizeof(output_path),
        "%s/source_budget.cob", directory);
    diagnostic_path_length = std::snprintf(diagnostic_path,
        sizeof(diagnostic_path), "%s/source_budget.diagnostics", directory);
    if (source_path_length < 0
        || static_cast<size_t>(source_path_length) >= sizeof(source_path)
        || output_path_length < 0
        || static_cast<size_t>(output_path_length) >= sizeof(output_path)
        || diagnostic_path_length < 0
        || static_cast<size_t>(diagnostic_path_length) >= sizeof(diagnostic_path)
        || test_write_text_file(source_path, source_text) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_file(output_path);
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\" --max-source-bytes %zu",
        source_path, output_path, source_length);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: source-byte budget should allow input exactly at the configured limit\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_file(output_path);
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\" --max-source-bytes 8 > \"%s\" 2>&1",
        source_path, output_path, diagnostic_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command_expect_failure(command) != FT_SUCCESS
        || test_read_text_file(diagnostic_path, diagnostic_text,
            sizeof(diagnostic_text)) != FT_SUCCESS
        || !std::strstr(diagnostic_text, "resource_limit_exceeded phase=source_read")
        || !std::strstr(diagnostic_text, "dimension=source_bytes configured_limit=8")
        || !std::strstr(diagnostic_text, "observed=")
        || test_read_text_file(output_path, diagnostic_text,
            sizeof(diagnostic_text)) == FT_SUCCESS)
    {
        std::printf("Assertion failed: source-byte exhaustion should emit a structured diagnostic and no COBOL output\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --output \"%s\" --max-source-bytes 0",
        source_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: zero source-byte limit should disable the configured cap\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_directory(directory);
    return (FT_SUCCESS);
}

FT_TEST(test_cli_source_byte_limit_is_aggregate_across_inputs)
{
    char directory[256];
    char first_source_path[256];
    char second_source_path[256];
    char first_output_path[256];
    char second_output_path[256];
    char diagnostic_path[256];
    char command[1536];
    char diagnostic_text[1024];
    char expected_limit[64];
    char expected_observed[64];
    const char *first_source;
    const char *second_source;
    size_t first_length;
    size_t second_length;
    size_t configured_limit;
    size_t observed_count;
    int first_source_path_length;
    int second_source_path_length;
    int first_output_path_length;
    int second_output_path_length;
    int diagnostic_path_length;
    int command_length;

    directory[0] = '\0';
    first_source_path[0] = '\0';
    second_source_path[0] = '\0';
    first_output_path[0] = '\0';
    second_output_path[0] = '\0';
    diagnostic_path[0] = '\0';
    first_source = "void main() { return; }\n";
    second_source = "void helper() { return; }\n";
    first_length = std::strlen(first_source);
    second_length = std::strlen(second_source);
    configured_limit = first_length + second_length - 1;
    observed_count = first_length + second_length;
    if (std::snprintf(expected_limit, sizeof(expected_limit),
            "dimension=source_bytes configured_limit=%zu", configured_limit) < 0
        || std::snprintf(expected_observed, sizeof(expected_observed),
            "observed=%zu", observed_count) < 0)
        return (FT_FAILURE);
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    first_source_path_length = std::snprintf(first_source_path,
        sizeof(first_source_path), "%s/first.cblc", directory);
    second_source_path_length = std::snprintf(second_source_path,
        sizeof(second_source_path), "%s/second.cblc", directory);
    first_output_path_length = std::snprintf(first_output_path,
        sizeof(first_output_path), "%s/first.cob", directory);
    second_output_path_length = std::snprintf(second_output_path,
        sizeof(second_output_path), "%s/second.cob", directory);
    diagnostic_path_length = std::snprintf(diagnostic_path,
        sizeof(diagnostic_path), "%s/aggregate.diagnostics", directory);
    if (first_source_path_length < 0
        || static_cast<size_t>(first_source_path_length) >= sizeof(first_source_path)
        || second_source_path_length < 0
        || static_cast<size_t>(second_source_path_length) >= sizeof(second_source_path)
        || first_output_path_length < 0
        || static_cast<size_t>(first_output_path_length) >= sizeof(first_output_path)
        || second_output_path_length < 0
        || static_cast<size_t>(second_output_path_length) >= sizeof(second_output_path)
        || diagnostic_path_length < 0
        || static_cast<size_t>(diagnostic_path_length) >= sizeof(diagnostic_path)
        || test_write_text_file(first_source_path, first_source) != FT_SUCCESS
        || test_write_text_file(second_source_path, second_source) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input \"%s\" --input \"%s\" --output \"%s\" --output \"%s\" --max-source-bytes %zu > \"%s\" 2>&1",
        first_source_path, second_source_path, first_output_path,
        second_output_path, configured_limit, diagnostic_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command_expect_failure(command) != FT_SUCCESS
        || test_read_text_file(diagnostic_path, diagnostic_text,
            sizeof(diagnostic_text)) != FT_SUCCESS
        || !std::strstr(diagnostic_text, expected_limit)
        || !std::strstr(diagnostic_text, expected_observed)
        || !std::strstr(diagnostic_text, second_source_path))
    {
        std::printf("Assertion failed: aggregate source-byte limit should reject the canonical input prefix that exceeds the invocation budget\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_read_text_file(first_output_path, diagnostic_text,
            sizeof(diagnostic_text)) == FT_SUCCESS)
    {
        std::printf("Assertion failed: aggregate source-byte exhaustion must not publish a partial module output\n");
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    test_remove_directory(directory);
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
        {"cli_dump_semantic_ir_option", test_cli_dump_semantic_ir_option},
        {"cli_lifetime_edge_transfer_limit", test_cli_lifetime_edge_transfer_limit},
        {"cli_template_resource_limits", test_cli_template_resource_limits},
        {"cli_lifetime_edge_transfer_limit_reaches_translation_unit",
            test_cli_lifetime_edge_transfer_limit_reaches_translation_unit},
        {"cli_template_limits_reach_monomorphization",
            test_cli_template_limits_reach_monomorphization},
        {"cli_source_byte_limit_stops_before_translation",
            test_cli_source_byte_limit_stops_before_translation},
        {"cli_source_byte_limit_is_aggregate_across_inputs",
            test_cli_source_byte_limit_is_aggregate_across_inputs}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
