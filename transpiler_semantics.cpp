#include "transpiler_semantics.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "transpiler_logging.hpp"

typedef struct s_transpiler_semantic_scope
{
    char (*identifiers)[TRANSPILE_IDENTIFIER_MAX];
    size_t identifier_count;
    size_t identifier_capacity;
}   t_transpiler_semantic_scope;

static void transpiler_semantics_scope_init(t_transpiler_semantic_scope *scope)
{
    if (!scope)
        return ;
    scope->identifiers = NULL;
    scope->identifier_count = 0;
    scope->identifier_capacity = 0;
}

static void transpiler_semantics_scope_dispose(t_transpiler_semantic_scope *scope)
{
    if (!scope)
        return ;
    if (scope->identifiers)
        cma_free(scope->identifiers);
    scope->identifiers = NULL;
    scope->identifier_count = 0;
    scope->identifier_capacity = 0;
}

static int transpiler_semantics_scope_reserve(t_transpiler_semantic_scope *scope, size_t desired_capacity)
{
    char (*new_identifiers)[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!scope)
        return (FT_FAILURE);
    if (scope->identifier_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_identifiers = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(desired_capacity,
        sizeof(*new_identifiers)));
    if (!new_identifiers)
        return (FT_FAILURE);
    index = 0;
    while (index < scope->identifier_count)
    {
        ft_strlcpy(new_identifiers[index], scope->identifiers[index], TRANSPILE_IDENTIFIER_MAX);
        index += 1;
    }
    if (scope->identifiers)
        cma_free(scope->identifiers);
    scope->identifiers = new_identifiers;
    scope->identifier_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_semantics_scope_contains(const t_transpiler_semantic_scope *scope, const char *name)
{
    size_t index;

    if (!scope)
        return (0);
    if (!name)
        return (0);
    index = 0;
    while (index < scope->identifier_count)
    {
        if (ft_strncmp(scope->identifiers[index], name, TRANSPILE_IDENTIFIER_MAX) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int transpiler_semantics_emit_error(t_transpiler_context *context, int code, const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, code, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, code);
    return (FT_FAILURE);
}

static int transpiler_semantics_register_data_item(t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *name)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (!name || name[0] == '\0')
    {
        pf_snprintf(message, sizeof(message),
            "data item declaration is missing an identifier");
        return (transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message));
    }
    if (transpiler_semantics_scope_contains(scope, name))
    {
        pf_snprintf(message, sizeof(message),
            "data item '%s' declared multiple times in WORKING-STORAGE", name);
        return (transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM, message));
    }
    if (scope->identifier_count >= scope->identifier_capacity)
    {
        if (transpiler_semantics_scope_reserve(scope, scope->identifier_capacity == 0 ? 4 : scope->identifier_capacity * 2)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(scope->identifiers[scope->identifier_count], name, TRANSPILE_IDENTIFIER_MAX);
    scope->identifier_count += 1;
    return (FT_SUCCESS);
}

static int transpiler_semantics_collect_data_items(const t_ast_node *section, t_transpiler_semantic_scope *scope,
    t_transpiler_context *context)
{
    size_t index;
    const t_ast_node *child;
    int status;

    if (!section)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(section))
    {
        child = ast_node_get_child(section, index);
        if (child && child->kind == AST_NODE_DATA_ITEM)
        {
            const t_ast_node *name_node;
            size_t name_index;

            name_node = NULL;
            name_index = 0;
            while (name_index < ast_node_child_count(child))
            {
                const t_ast_node *candidate;

                candidate = ast_node_get_child(child, name_index);
                if (candidate && candidate->kind == AST_NODE_IDENTIFIER)
                {
                    name_node = candidate;
                    break ;
                }
                name_index += 1;
            }
            if (name_node && name_node->token.lexeme)
            {
                if (transpiler_semantics_register_data_item(scope, context, name_node->token.lexeme) != FT_SUCCESS)
                    status = FT_FAILURE;
            }
            else
            {
                char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                pf_snprintf(message, sizeof(message),
                    "data item at line %zu is missing a name", child->token.line);
                transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
                status = FT_FAILURE;
            }
        }
        index += 1;
    }
    return (status);
}

static int transpiler_semantics_collect_scope(const t_ast_node *program, t_transpiler_semantic_scope *scope,
    t_transpiler_context *context)
{
    size_t index;
    const t_ast_node *data_division;
    int status;

    if (!program)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    data_division = NULL;
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_DATA_DIVISION)
        {
            data_division = candidate;
            break ;
        }
        index += 1;
    }
    if (!data_division)
        return (FT_SUCCESS);
    status = FT_SUCCESS;
    index = 0;
    while (index < ast_node_child_count(data_division))
    {
        const t_ast_node *child;

        child = ast_node_get_child(data_division, index);
        if (child && child->kind == AST_NODE_WORKING_STORAGE_SECTION)
        {
            if (transpiler_semantics_collect_data_items(child, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
        }
        index += 1;
    }
    return (status);
}

static int transpiler_semantics_validate_identifier_use(const t_transpiler_semantic_scope *scope,
    t_transpiler_context *context, const t_ast_node *identifier, int is_target)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!identifier)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (!identifier->token.lexeme)
    {
        pf_snprintf(message, sizeof(message),
            "MOVE %s is missing an identifier", is_target ? "target" : "source");
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        return (FT_FAILURE);
    }
    if (!transpiler_semantics_scope_contains(scope, identifier->token.lexeme))
    {
        pf_snprintf(message, sizeof(message),
            "identifier '%s' referenced in MOVE is not declared in WORKING-STORAGE", identifier->token.lexeme);
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER, message);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int transpiler_semantics_validate_move_statement(const t_ast_node *move_node,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    const t_ast_node *source;
    const t_ast_node *target;
    int status;

    if (!move_node)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (ast_node_child_count(move_node) < 2)
        return (FT_FAILURE);
    source = ast_node_get_child(move_node, 0);
    target = ast_node_get_child(move_node, 1);
    status = FT_SUCCESS;
    if (!target || target->kind != AST_NODE_IDENTIFIER)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "MOVE statement is missing a valid target identifier");
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        status = FT_FAILURE;
    }
    else if (transpiler_semantics_validate_identifier_use(scope, context, target, 1) != FT_SUCCESS)
        status = FT_FAILURE;
    if (source && source->kind == AST_NODE_IDENTIFIER)
    {
        if (transpiler_semantics_validate_identifier_use(scope, context, source, 0) != FT_SUCCESS)
            status = FT_FAILURE;
    }
    else if (source && source->kind != AST_NODE_LITERAL)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "MOVE source must be an identifier or literal");
        transpiler_semantics_emit_error(context, TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE, message);
        status = FT_FAILURE;
    }
    return (status);
}

static int transpiler_semantics_validate_statements(const t_ast_node *program,
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
            size_t stmt_index;

            stmt_index = 0;
            while (stmt_index < ast_node_child_count(child))
            {
                const t_ast_node *statement;

                statement = ast_node_get_child(child, stmt_index);
                if (statement && statement->kind == AST_NODE_MOVE_STATEMENT)
                {
                    if (transpiler_semantics_validate_move_statement(statement, scope, context) != FT_SUCCESS)
                        status = FT_FAILURE;
                }
                stmt_index += 1;
            }
        }
        index += 1;
    }
    return (status);
}

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
    if (transpiler_semantics_validate_statements(program, &scope, context) != FT_SUCCESS)
        status = FT_FAILURE;
    transpiler_semantics_scope_dispose(&scope);
    if (transpiler_context_has_errors(context))
        status = FT_FAILURE;
    return (status);
}
