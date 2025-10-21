#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

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

static int transpiler_cobol_procedure_reserve(t_transpiler_cobol_procedure *procedure,
    size_t desired_capacity)
{
    t_transpiler_cobol_paragraph **new_paragraphs;
    size_t index;

    if (!procedure)
        return (FT_FAILURE);
    if (procedure->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_paragraphs = static_cast<t_transpiler_cobol_paragraph **>(cma_calloc(desired_capacity,
        sizeof(*new_paragraphs)));
    if (!new_paragraphs)
        return (FT_FAILURE);
    index = 0;
    while (index < procedure->count)
    {
        new_paragraphs[index] = procedure->paragraphs[index];
        index += 1;
    }
    if (procedure->paragraphs)
        cma_free(procedure->paragraphs);
    procedure->paragraphs = new_paragraphs;
    procedure->capacity = desired_capacity;
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
    statement->call.arguments = NULL;
    statement->call.argument_count = 0;
    statement->call.argument_capacity = 0;
    statement->call.return_slot = NULL;
    return (statement);
}

static int transpiler_cobol_call_reserve(t_transpiler_cobol_call_statement *call, size_t desired_capacity)
{
    const char **new_arguments;
    size_t index;

    if (!call)
        return (FT_FAILURE);
    if (call->argument_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_arguments = static_cast<const char **>(cma_calloc(desired_capacity,
        sizeof(*new_arguments)));
    if (!new_arguments)
        return (FT_FAILURE);
    index = 0;
    while (index < call->argument_count)
    {
        new_arguments[index] = call->arguments[index];
        index += 1;
    }
    if (call->arguments)
        cma_free(call->arguments);
    call->arguments = new_arguments;
    call->argument_capacity = desired_capacity;
    return (FT_SUCCESS);
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

t_transpiler_cobol_statement *transpiler_cobol_statement_create_compute(const char *target,
    const char *expression, int rounded)
{
    t_transpiler_cobol_statement *statement;

    if (!target)
        return (NULL);
    if (!expression)
        return (NULL);
    statement = transpiler_cobol_statement_allocate();
    if (!statement)
        return (NULL);
    statement->kind = TRANSPILE_COBOL_STATEMENT_COMPUTE;
    statement->compute.target = target;
    statement->compute.expression = expression;
    statement->compute.rounded = rounded ? 1 : 0;
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

t_transpiler_cobol_statement *transpiler_cobol_statement_create_call(const char *subprogram)
{
    t_transpiler_cobol_statement *statement;

    if (!subprogram)
        return (NULL);
    if (subprogram[0] == '\0')
        return (NULL);
    statement = transpiler_cobol_statement_allocate();
    if (!statement)
        return (NULL);
    statement->kind = TRANSPILE_COBOL_STATEMENT_CALL;
    statement->call.subprogram = subprogram;
    return (statement);
}

int transpiler_cobol_call_append_argument(t_transpiler_cobol_statement *statement, const char *argument)
{
    t_transpiler_cobol_call_statement *call;

    if (!statement || !argument)
        return (FT_FAILURE);
    if (statement->kind != TRANSPILE_COBOL_STATEMENT_CALL)
        return (FT_FAILURE);
    call = &statement->call;
    if (call->argument_count >= call->argument_capacity)
    {
        if (transpiler_cobol_call_reserve(call, call->argument_capacity == 0 ? 4 : call->argument_capacity * 2)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    call->arguments[call->argument_count] = argument;
    call->argument_count += 1;
    return (FT_SUCCESS);
}

int transpiler_cobol_call_set_return_slot(t_transpiler_cobol_statement *statement, const char *return_slot)
{
    if (!statement || !return_slot)
        return (FT_FAILURE);
    if (statement->kind != TRANSPILE_COBOL_STATEMENT_CALL)
        return (FT_FAILURE);
    statement->call.return_slot = return_slot;
    return (FT_SUCCESS);
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
    else if (statement->kind == TRANSPILE_COBOL_STATEMENT_CALL)
    {
        if (statement->call.arguments)
            cma_free(statement->call.arguments);
        statement->call.arguments = NULL;
        statement->call.argument_count = 0;
        statement->call.argument_capacity = 0;
        statement->call.return_slot = NULL;
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

t_transpiler_cobol_paragraph *transpiler_cobol_paragraph_create(const char *name)
{
    t_transpiler_cobol_paragraph *paragraph;
    size_t length;

    if (!name)
        return (NULL);
    if (name[0] == '\0')
        return (NULL);
    paragraph = static_cast<t_transpiler_cobol_paragraph *>(cma_calloc(1,
        sizeof(*paragraph)));
    if (!paragraph)
        return (NULL);
    length = ft_strlen(name);
    paragraph->name = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!paragraph->name)
    {
        cma_free(paragraph);
        return (NULL);
    }
    ft_strlcpy(paragraph->name, name, length + 1);
    transpiler_cobol_statement_block_init(&paragraph->statements);
    return (paragraph);
}

void transpiler_cobol_paragraph_destroy(t_transpiler_cobol_paragraph *paragraph)
{
    if (!paragraph)
        return ;
    if (paragraph->name)
        cma_free(paragraph->name);
    transpiler_cobol_statement_block_dispose(&paragraph->statements);
    paragraph->name = NULL;
    cma_free(paragraph);
}

t_transpiler_cobol_statement_block *transpiler_cobol_paragraph_get_statements(t_transpiler_cobol_paragraph *paragraph)
{
    if (!paragraph)
        return (NULL);
    return (&paragraph->statements);
}

void transpiler_cobol_procedure_init(t_transpiler_cobol_procedure *procedure)
{
    if (!procedure)
        return ;
    procedure->paragraphs = NULL;
    procedure->count = 0;
    procedure->capacity = 0;
}

void transpiler_cobol_procedure_dispose(t_transpiler_cobol_procedure *procedure)
{
    size_t index;

    if (!procedure)
        return ;
    index = 0;
    while (index < procedure->count)
    {
        if (procedure->paragraphs && procedure->paragraphs[index])
            transpiler_cobol_paragraph_destroy(procedure->paragraphs[index]);
        index += 1;
    }
    if (procedure->paragraphs)
        cma_free(procedure->paragraphs);
    procedure->paragraphs = NULL;
    procedure->count = 0;
    procedure->capacity = 0;
}

int transpiler_cobol_procedure_append(t_transpiler_cobol_procedure *procedure,
    t_transpiler_cobol_paragraph *paragraph)
{
    if (!procedure)
        return (FT_FAILURE);
    if (!paragraph)
        return (FT_FAILURE);
    if (procedure->count >= procedure->capacity)
    {
        if (transpiler_cobol_procedure_reserve(procedure,
                procedure->capacity == 0 ? 4 : procedure->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    procedure->paragraphs[procedure->count] = paragraph;
    procedure->count += 1;
    return (FT_SUCCESS);
}
