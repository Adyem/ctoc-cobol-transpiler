#include "transpiler_context.hpp"
#include "transpiler_pipeline.hpp"

#include "test_suites.hpp"

static int test_stage_callback(t_transpiler_context *context, void *user_data)
{
    int *counter;

    (void)context;
    counter = static_cast<int *>(user_data);
    if (!counter)
        return (FT_FAILURE);
    *counter += 1;
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_pipeline_executes_stage)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_CBL_C, TRANSPILE_LANGUAGE_COBOL);
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "unit-stage", test_stage_callback, &counter),
        "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_execute(&pipeline, &context),
        "pipeline should execute successfully") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (test_expect_int_equal(counter, 1, "pipeline should execute the stage once") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int failing_stage_callback(t_transpiler_context *context, void *user_data)
{
    int *counter;

    counter = static_cast<int *>(user_data);
    if (!counter)
        return (FT_FAILURE);
    *counter += 1;
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int early_failing_stage_callback(t_transpiler_context *context, void *user_data)
{
    (void)user_data;
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

FT_TEST(test_transpiler_pipeline_reports_failure)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "failing-stage", failing_stage_callback, &counter),
        "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (transpiler_pipeline_execute(&pipeline, &context) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        pf_printf("Assertion failed: pipeline execute should fail when a stage fails\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (test_expect_int_equal(counter, 1, "pipeline should execute failing stage once") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_pipeline_stops_after_failure)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "early-failure", early_failing_stage_callback, &counter),
        "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "later-stage", test_stage_callback, &counter),
        "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (transpiler_pipeline_execute(&pipeline, &context) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        pf_printf("Assertion failed: pipeline execute should fail for early failure\n");
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (test_expect_int_equal(counter, 0, "pipeline should skip later stages after failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_pipeline_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"transpiler_pipeline_executes_stage", test_transpiler_pipeline_executes_stage},
        {"transpiler_pipeline_reports_failure", test_transpiler_pipeline_reports_failure},
        {"transpiler_pipeline_stops_after_failure", test_transpiler_pipeline_stops_after_failure}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
