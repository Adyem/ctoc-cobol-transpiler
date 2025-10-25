#include "transpiler_semantics_internal.hpp"

int transpiler_semantics_analyze_program(t_transpiler_context *context, const t_ast_node *program)
{
    t_transpiler_semantic_scope scope;
    int status;

    if (!context)
        return (FT_FAILURE);
    transpiler_semantics_scope_init(&scope);
    status = FT_SUCCESS;
    if (transpiler_semantics_collect_scope(program, &scope, context) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_semantics_collect_use_after_error(program, context) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_context_capture_semantic_snapshot_before(context) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_semantics_validate_statements(program, &scope, context) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_semantics_analyze_usage(&scope, context) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_context_capture_semantic_snapshot_after(context) != FT_SUCCESS)
        status = FT_FAILURE;
    transpiler_semantics_scope_dispose(&scope);
    if (transpiler_context_has_errors(context))
        status = FT_FAILURE;
    return (status);
}
