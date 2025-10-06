#ifndef TRANSPILER_PIPELINE_HPP
#define TRANSPILER_PIPELINE_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"
#include "transpiler_context.hpp"

#define TRANSPILE_STAGE_NAME_MAX 64

typedef int (*t_transpiler_stage_callback)(t_transpiler_context *context, void *user_data);

typedef struct s_transpiler_stage
{
    t_transpiler_stage_callback callback;
    void *user_data;
    char name[TRANSPILE_STAGE_NAME_MAX];
}   t_transpiler_stage;

typedef struct s_transpiler_pipeline
{
    t_transpiler_stage *stages;
    size_t stage_count;
    size_t stage_capacity;
    int last_error;
}   t_transpiler_pipeline;

int transpiler_pipeline_init(t_transpiler_pipeline *pipeline);
void transpiler_pipeline_dispose(t_transpiler_pipeline *pipeline);
int transpiler_pipeline_add_stage(t_transpiler_pipeline *pipeline, const char *name, t_transpiler_stage_callback callback, void *user_data);
int transpiler_pipeline_execute(t_transpiler_pipeline *pipeline, t_transpiler_context *context);
void transpiler_pipeline_reset(t_transpiler_pipeline *pipeline);

#endif
