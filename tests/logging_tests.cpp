#include "transpiler_logging.hpp"

#include "test_suites.hpp"

FT_TEST(test_logging_info_requires_verbose)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_NORMAL);
    if (test_expect_success(transpiler_logging_emit(&context, TRANSPILE_SEVERITY_INFO, 0, "info"),
        "info emit should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
        "info diagnostics should be suppressed for normal level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_VERBOSE);
    if (test_expect_success(transpiler_logging_emit(&context, TRANSPILE_SEVERITY_INFO, 0, "info"),
        "info emit should succeed for verbose level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
        "info diagnostics should be recorded for verbose level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.diagnostics.items[0].message, "info",
        "info message should match recorded text") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_logging_warning_respects_silent_level)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_SILENT);
    if (test_expect_success(transpiler_logging_emit(&context, TRANSPILE_SEVERITY_WARNING, 7, "warn"),
        "warning emit should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 0,
        "warnings should be suppressed for silent level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_NORMAL);
    if (test_expect_success(transpiler_logging_emit(&context, TRANSPILE_SEVERITY_WARNING, 7, "warn"),
        "warning emit should succeed for normal level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
        "warnings should be recorded for normal level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code, 7,
        "warning code should be preserved") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.items[0].severity),
        static_cast<int>(TRANSPILE_SEVERITY_WARNING),
        "warning severity should be recorded") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_logging_stage_failure_records_error)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_SILENT);
    transpiler_logging_stage_failure(&context, "unit-stage", 42);
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
        "stage failure should record diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.items[0].code, 42,
        "stage failure should preserve error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.items[0].severity),
        static_cast<int>(TRANSPILE_SEVERITY_ERROR),
        "stage failure should record error severity") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.diagnostics.items[0].message,
        "Stage 'unit-stage' failed with code 42",
        "stage failure message should include stage name and code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_logging_stage_progress_requires_verbose)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_VERBOSE);
    transpiler_logging_stage_start(&context, "progress-stage");
    transpiler_logging_stage_success(&context, "progress-stage");
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 2,
        "stage progress should produce two diagnostics") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.diagnostics.items[0].message,
        "Starting stage 'progress-stage'",
        "first diagnostic should report stage start") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(context.diagnostics.items[1].message,
        "Completed stage 'progress-stage'",
        "second diagnostic should report stage completion") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

FT_TEST(test_logging_escalates_warnings_when_enabled)
{
    t_transpiler_context context;

    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_NORMAL);
    if (test_expect_success(transpiler_logging_emit(&context, TRANSPILE_SEVERITY_WARNING, 11, "warn"),
        "warning emit should succeed for normal level") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
        "warning should record a diagnostic when escalation disabled") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.items[0].severity),
        static_cast<int>(TRANSPILE_SEVERITY_WARNING),
        "warning severity should remain warning when escalation disabled") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    if (test_expect_success(transpiler_context_init(&context), "context re-init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_set_diagnostic_level(&context, TRANSPILE_DIAGNOSTIC_NORMAL);
    transpiler_context_set_warnings_as_errors(&context, 1);
    if (test_expect_success(transpiler_logging_emit(&context, TRANSPILE_SEVERITY_WARNING, 13, "warn"),
        "warning emit should succeed with escalation") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.count), 1,
        "escalated warning should record a single diagnostic") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(context.diagnostics.items[0].severity),
        static_cast<int>(TRANSPILE_SEVERITY_ERROR),
        "escalated warning should be stored as error") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.last_error_code, 13,
        "escalated warning should record error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    return (FT_SUCCESS);
}

const t_test_case *get_logging_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_logging_info_requires_verbose", test_logging_info_requires_verbose},
        {"transpiler_logging_warning_respects_silent_level", test_logging_warning_respects_silent_level},
        {"transpiler_logging_escalates_warnings_when_enabled", test_logging_escalates_warnings_when_enabled},
        {"transpiler_logging_stage_failure_records_error", test_logging_stage_failure_records_error},
        {"transpiler_logging_stage_progress_requires_verbose", test_logging_stage_progress_requires_verbose}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
