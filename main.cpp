#include "runtime_scalar.hpp"
#include "transpiler_cli.hpp"
#include "transpiler_logging.hpp"
#include "transpiler_pipeline.hpp"

static int runtime_demo_stage(t_transpiler_context *context, void *user_data)
{
    (void)context;
    (void)user_data;
    runtime_demo();
    return (FT_SUCCESS);
}

int main(int argc, const char **argv)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    t_transpiler_cli_options options;
    int status;

    if (transpiler_cli_parse(&options, argc, argv) != FT_SUCCESS)
    {
        transpiler_cli_print_usage();
        return (1);
    }
    if (options.show_help)
    {
        transpiler_cli_print_usage();
        return (0);
    }
    if (transpiler_pipeline_init(&pipeline) != FT_SUCCESS)
        return (1);
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    if (transpiler_pipeline_add_stage(&pipeline, "runtime-scalar-demo", runtime_demo_stage, NULL) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    status = transpiler_pipeline_execute(&pipeline, &context);
    transpiler_logging_flush(&context);
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
