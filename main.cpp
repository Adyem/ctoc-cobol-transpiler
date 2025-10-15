#include "transpiler_cli.hpp"
#include "transpiler_logging.hpp"
#include "transpiler_pipeline.hpp"
#include "transpiler_stub_cblc.hpp"

static int transpiler_stage_cblc_to_cobol(t_transpiler_context *context, void *user_data)
{
    size_t index;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    if (context->source_count != context->target_count)
        return (FT_FAILURE);
    index = 0;
    while (index < context->source_count)
    {
        if (!context->source_paths || !context->target_paths)
            return (FT_FAILURE);
        if (transpiler_stub_cblc_to_cobol(context->source_paths[index], context->target_paths[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
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
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (options.show_help)
    {
        transpiler_cli_print_usage();
        transpiler_cli_options_dispose(&options);
        return (0);
    }
    if (transpiler_pipeline_init(&pipeline) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (context.source_language == TRANSPILE_LANGUAGE_CBL_C
        && context.target_language == TRANSPILE_LANGUAGE_COBOL)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "cblc-to-cobol", transpiler_stage_cblc_to_cobol, NULL) != FT_SUCCESS)
        {
            transpiler_context_dispose(&context);
            transpiler_pipeline_dispose(&pipeline);
            transpiler_cli_options_dispose(&options);
            return (1);
        }
    }
    status = transpiler_pipeline_execute(&pipeline, &context);
    transpiler_logging_flush(&context);
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    transpiler_cli_options_dispose(&options);
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
