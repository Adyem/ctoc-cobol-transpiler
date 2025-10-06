#include <cstdlib>

#include "libft/CMA/CMA.hpp"
#include "transpiler_pipeline.hpp"
#include "transpiler_diagnostics.hpp"
#include "libft/Printf/printf.hpp"

static int transpiler_pipeline_reserve(t_transpiler_pipeline *pipeline, size_t desired_capacity)
{
    t_transpiler_stage *new_stages;

    if (!pipeline)
        return (FT_FAILURE);
    if (pipeline->stage_capacity >= desired_capacity)
        return (FT_SUCCESS);
    new_stages = static_cast<t_transpiler_stage *>(cma_calloc(desired_capacity, sizeof(t_transpiler_stage)));
    if (!new_stages)
        return (FT_FAILURE);
    if (pipeline->stages)
    {
        ft_memcpy(new_stages, pipeline->stages, pipeline->stage_count * sizeof(t_transpiler_stage));
        cma_free(pipeline->stages);
    }
    pipeline->stages = new_stages;
    pipeline->stage_capacity = desired_capacity;
    return (FT_SUCCESS);
}

int transpiler_pipeline_init(t_transpiler_pipeline *pipeline)
{
    if (!pipeline)
        return (FT_FAILURE);
    pipeline->stages = NULL;
    pipeline->stage_count = 0;
    pipeline->stage_capacity = 0;
    pipeline->last_error = FT_SUCCESS;
    if (transpiler_pipeline_reserve(pipeline, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

void transpiler_pipeline_dispose(t_transpiler_pipeline *pipeline)
{
    if (!pipeline)
        return ;
    if (pipeline->stages)
        cma_free(pipeline->stages);
    pipeline->stages = NULL;
    pipeline->stage_count = 0;
    pipeline->stage_capacity = 0;
    pipeline->last_error = FT_SUCCESS;
}

void transpiler_pipeline_reset(t_transpiler_pipeline *pipeline)
{
    if (!pipeline)
        return ;
    pipeline->stage_count = 0;
    pipeline->last_error = FT_SUCCESS;
}

int transpiler_pipeline_add_stage(t_transpiler_pipeline *pipeline, const char *name, t_transpiler_stage_callback callback, void *
user_data)
{
    t_transpiler_stage *stage;

    if (!pipeline)
        return (FT_FAILURE);
    if (!callback)
        return (FT_FAILURE);
    if (pipeline->stage_count >= pipeline->stage_capacity)
    {
        if (transpiler_pipeline_reserve(pipeline, pipeline->stage_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    stage = &pipeline->stages[pipeline->stage_count];
    stage->callback = callback;
    stage->user_data = user_data;
    stage->name[0] = '\0';
    if (name)
        ft_strlcpy(stage->name, name, TRANSPILE_STAGE_NAME_MAX);
    pipeline->stage_count += 1;
    return (FT_SUCCESS);
}

static void transpiler_pipeline_report_failure(t_transpiler_context *context, const t_transpiler_stage *stage, int error_code)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return ;
    pf_snprintf(message, sizeof(message), "Stage '%s' failed with code %d", stage->name, error_code);
    transpiler_diagnostics_push(&context->diagnostics, TRANSPILE_SEVERITY_ERROR, error_code, message);
    transpiler_context_record_error(context, error_code);
}

int transpiler_pipeline_execute(t_transpiler_pipeline *pipeline, t_transpiler_context *context)
{
    size_t index;
    int status;

    if (!pipeline)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    pipeline->last_error = FT_SUCCESS;
    index = 0;
    while (index < pipeline->stage_count)
    {
        status = pipeline->stages[index].callback(context, pipeline->stages[index].user_data);
        if (status != FT_SUCCESS)
        {
            pipeline->last_error = status;
            transpiler_pipeline_report_failure(context, &pipeline->stages[index], status);
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}
