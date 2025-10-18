#include "transpiler_semantics_internal.hpp"

static int transpiler_semantics_validate_statement_sequence(const t_ast_node *sequence,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context);

static int transpiler_semantics_validate_statement(const t_ast_node *statement,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    size_t index;
    int status;

    if (!statement)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (statement->kind == AST_NODE_ASSIGNMENT_STATEMENT)
        return (transpiler_semantics_validate_assignment_statement(statement, scope, context));
    if (statement->kind == AST_NODE_MOVE_STATEMENT)
        return (transpiler_semantics_validate_move_statement(statement, scope, context));
    if (statement->kind == AST_NODE_STATEMENT_SEQUENCE)
        return (transpiler_semantics_validate_statement_sequence(statement, scope, context));
    if (statement->kind == AST_NODE_IF_STATEMENT
        || statement->kind == AST_NODE_PERFORM_UNTIL_STATEMENT
        || statement->kind == AST_NODE_PERFORM_VARYING_STATEMENT)
    {
        status = FT_SUCCESS;
        index = 0;
        while (index < ast_node_child_count(statement))
        {
            const t_ast_node *child;

            child = ast_node_get_child(statement, index);
            if (child && child->kind == AST_NODE_CONDITION)
            {
                if (transpiler_semantics_validate_condition(child, scope, context) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            else if (child && child->kind == AST_NODE_STATEMENT_SEQUENCE)
            {
                if (transpiler_semantics_validate_statement_sequence(child, scope, context) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            index += 1;
        }
        return (status);
    }
    return (FT_SUCCESS);
}

static int transpiler_semantics_validate_statement_sequence(const t_ast_node *sequence,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    size_t index;
    int status;

    if (!sequence)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(sequence))
    {
        const t_ast_node *statement;

        statement = ast_node_get_child(sequence, index);
        if (statement)
        {
            if (transpiler_semantics_validate_statement(statement, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}

int transpiler_semantics_validate_statements(const t_ast_node *program,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    size_t index;
    const t_ast_node *procedure_division;
    int status;

    if (!program)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    procedure_division = NULL;
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_PROCEDURE_DIVISION)
        {
            procedure_division = candidate;
            break ;
        }
        index += 1;
    }
    if (!procedure_division)
        return (FT_SUCCESS);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(procedure_division))
    {
        const t_ast_node *child;

        child = ast_node_get_child(procedure_division, index);
        if (child && child->kind == AST_NODE_STATEMENT_SEQUENCE)
        {
            if (transpiler_semantics_validate_statement_sequence(child, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}
