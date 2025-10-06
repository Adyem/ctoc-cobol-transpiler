#include "libft/Printf/printf.hpp"
#include "runtime_scalar.hpp"
#include "transpiler_pipeline.hpp"

static int runtime_demo_stage(t_transpiler_context *context, void *user_data)
{
    (void)context;
    (void)user_data;
    runtime_demo();
    return (FT_SUCCESS);
}

static void transpiler_log_diagnostics(const t_transpiler_context *context)
{
    size_t index;

    if (!context)
        return ;
    index = 0;
    while (index < context->diagnostics.count)
    {
        pf_printf("[%d] %s\n", context->diagnostics.items[index].severity, context->diagnostics.items[index].message);
        index += 1;
    }
}

int main(void)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int status;

    if (transpiler_pipeline_init(&pipeline) != FT_SUCCESS)
        return (1);
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_CBL_C, TRANSPILE_LANGUAGE_COBOL);
    if (transpiler_pipeline_add_stage(&pipeline, "runtime-scalar-demo", runtime_demo_stage, NULL) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    status = transpiler_pipeline_execute(&pipeline, &context);
    if (status != FT_SUCCESS)
    {
        transpiler_log_diagnostics(&context);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
