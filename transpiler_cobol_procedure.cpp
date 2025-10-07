#include "transpiler_cobol_procedure.hpp"

#include "libft/CMA/CMA.hpp"

static int transpiler_cobol_statement_block_reserve(t_transpiler_cobol_statement_block *block,
    size_t desired_capacity)
{
    t_transpiler_cobol_statement **new_statements;
    size_t index;

    if (!block)
        return (FT_FAILURE);
    if (block->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_statements = static_cast<t_transpiler_cobol_statement **>(cma_calloc(desired_capacity,
        sizeof(*new_statements)));
    if (!new_statements)
        return (FT_FAILURE);
    index = 0;
    while (index < block->count)
    {
        new_statements[index] = block->statements[index];
        index += 1;
    }
    if (block->statements)
        cma_free(block->statements);
    block->statements = new_statements;
    block->capacity = desired_capacity;
    return (FT_SUCCESS);
}

void transpiler_cobol_statement_block_init(t_transpiler_cobol_statement_block *block)
{
    if (!block)
        return ;
    block->statements = NULL;
    block->count = 0;
    block->capacity = 0;
}

void transpiler_cobol_statement_block_dispose(t_transpiler_cobol_statement_block *block)
{
    size_t index;

    if (!block)
        return ;
    index = 0;
    while (index < block->count)
    {
        if (block->statements && block->statements[index])
            transpiler_cobol_statement_destroy(block->statements[index]);
        index += 1;
    }
    if (block->statements)
        cma_free(block->statements);
    block->statements = NULL;
    block->count = 0;
    block->capacity = 0;
}

static t_transpiler_cobol_statement *transpiler_cobol_statement_allocate(void)
{
    t_transpiler_cobol_statement *statement;

    statement = static_cast<t_transpiler_cobol_statement *>(cma_calloc(1,
        sizeof(*statement)));
    if (!statement)
        return (NULL);
    transpiler_cobol_statement_block_init(&statement->if_statement.then_branch);
    transpiler_cobol_statement_block_init(&statement->if_statement.else_branch);
    transpiler_cobol_statement_block_init(&statement->perform_until.body);
    transpiler_cobol_statement_block_init(&statement->perform_varying.body);
    return (statement);
}

int transpiler_cobol_statement_block_append(t_transpiler_cobol_statement_block *block,
    t_transpiler_cobol_statement *statement)
{
    if (!block)
        return (FT_FAILURE);
    if (!statement)
        return (FT_FAILURE);
    if (block->count >= block->capacity)
    {
        if (transpiler_cobol_statement_block_reserve(block,
                block->capacity == 0 ? 4 : block->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    block->statements[block->count] = statement;
    block->count += 1;
    return (FT_SUCCESS);
}

t_transpiler_cobol_statement *transpiler_cobol_statement_create_move(const char *source, const char *target)
{
    t_transpiler_cobol_statement *statement;

    if (!source)
        return (NULL);
    if (!target)
        return (NULL);
    statement = transpiler_cobol_statement_allocate();
    if (!statement)
        return (NULL);
    statement->kind = TRANSPILE_COBOL_STATEMENT_MOVE;
    statement->move.source = source;
    statement->move.target = target;
    return (statement);
}

t_transpiler_cobol_statement *transpiler_cobol_statement_create_if(const t_transpiler_cobol_condition *condition)
{
    t_transpiler_cobol_statement *statement;

    if (!condition)
        return (NULL);
    if (!condition->left || !condition->right)
        return (NULL);
    statement = transpiler_cobol_statement_allocate();
    if (!statement)
        return (NULL);
    statement->kind = TRANSPILE_COBOL_STATEMENT_IF;
    statement->if_statement.condition = *condition;
    return (statement);
}

t_transpiler_cobol_statement *transpiler_cobol_statement_create_perform_until(
    const t_transpiler_cobol_condition *condition)
{
    t_transpiler_cobol_statement *statement;

    if (!condition)
        return (NULL);
    if (!condition->left || !condition->right)
        return (NULL);
    statement = transpiler_cobol_statement_allocate();
    if (!statement)
        return (NULL);
    statement->kind = TRANSPILE_COBOL_STATEMENT_PERFORM_UNTIL;
    statement->perform_until.condition = *condition;
    return (statement);
}

t_transpiler_cobol_statement *transpiler_cobol_statement_create_perform_varying(const char *counter,
    const char *initial, const char *step, const t_transpiler_cobol_condition *condition)
{
    t_transpiler_cobol_statement *statement;

    if (!counter || !counter[0])
        return (NULL);
    if (!initial || !initial[0])
        return (NULL);
    if (!step || !step[0])
        return (NULL);
    if (!condition)
        return (NULL);
    if (!condition->left || !condition->right)
        return (NULL);
    statement = transpiler_cobol_statement_allocate();
    if (!statement)
        return (NULL);
    statement->kind = TRANSPILE_COBOL_STATEMENT_PERFORM_VARYING;
    statement->perform_varying.counter = counter;
    statement->perform_varying.initial = initial;
    statement->perform_varying.step = step;
    statement->perform_varying.condition = *condition;
    return (statement);
}

void transpiler_cobol_statement_destroy(t_transpiler_cobol_statement *statement)
{
    if (!statement)
        return ;
    if (statement->kind == TRANSPILE_COBOL_STATEMENT_IF)
    {
        transpiler_cobol_statement_block_dispose(&statement->if_statement.then_branch);
        transpiler_cobol_statement_block_dispose(&statement->if_statement.else_branch);
    }
    else if (statement->kind == TRANSPILE_COBOL_STATEMENT_PERFORM_UNTIL)
    {
        transpiler_cobol_statement_block_dispose(&statement->perform_until.body);
    }
    else if (statement->kind == TRANSPILE_COBOL_STATEMENT_PERFORM_VARYING)
    {
        transpiler_cobol_statement_block_dispose(&statement->perform_varying.body);
    }
    cma_free(statement);
}

t_transpiler_cobol_statement_block *transpiler_cobol_if_get_then_branch(t_transpiler_cobol_statement *statement)
{
    if (!statement)
        return (NULL);
    if (statement->kind != TRANSPILE_COBOL_STATEMENT_IF)
        return (NULL);
    return (&statement->if_statement.then_branch);
}

t_transpiler_cobol_statement_block *transpiler_cobol_if_get_else_branch(t_transpiler_cobol_statement *statement)
{
    if (!statement)
        return (NULL);
    if (statement->kind != TRANSPILE_COBOL_STATEMENT_IF)
        return (NULL);
    return (&statement->if_statement.else_branch);
}

t_transpiler_cobol_statement_block *transpiler_cobol_perform_until_get_body(t_transpiler_cobol_statement *statement)
{
    if (!statement)
        return (NULL);
    if (statement->kind != TRANSPILE_COBOL_STATEMENT_PERFORM_UNTIL)
        return (NULL);
    return (&statement->perform_until.body);
}

t_transpiler_cobol_statement_block *transpiler_cobol_perform_varying_get_body(t_transpiler_cobol_statement *statement)
{
    if (!statement)
        return (NULL);
    if (statement->kind != TRANSPILE_COBOL_STATEMENT_PERFORM_VARYING)
        return (NULL);
    return (&statement->perform_varying.body);
}
