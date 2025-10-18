#include "libft/Printf/printf.hpp"

#include "transpiler_semantics_internal.hpp"

static int transpiler_semantics_validate_statement_sequence(const t_ast_node *sequence,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context);

static const char *transpiler_semantics_statement_name(const t_ast_node *statement)
{
    if (!statement)
        return ("statement");
    if (statement->kind == AST_NODE_MOVE_STATEMENT)
        return ("MOVE statement");
    if (statement->kind == AST_NODE_ASSIGNMENT_STATEMENT)
        return ("ASSIGNMENT statement");
    if (statement->kind == AST_NODE_COMPUTE_STATEMENT)
        return ("COMPUTE statement");
    if (statement->kind == AST_NODE_IF_STATEMENT)
        return ("IF statement");
    if (statement->kind == AST_NODE_PERFORM_UNTIL_STATEMENT)
        return ("PERFORM UNTIL statement");
    if (statement->kind == AST_NODE_PERFORM_VARYING_STATEMENT)
        return ("PERFORM VARYING statement");
    if (statement->kind == AST_NODE_STOP_STATEMENT)
        return ("STOP RUN statement");
    if (statement->token.lexeme)
        return (statement->token.lexeme);
    return ("statement");
}

static int transpiler_semantics_statement_is_terminator(const t_ast_node *statement)
{
    if (!statement)
        return (0);
    if (statement->kind == AST_NODE_STOP_STATEMENT)
        return (1);
    return (0);
}

static int transpiler_semantics_parse_integer_literal(const t_ast_node *literal,
    long long *out_value)
{
    const char *text;
    size_t length;
    size_t index;
    long long result;
    int sign;

    if (!literal)
        return (0);
    if (!out_value)
        return (0);
    if (literal->token.kind != LEXER_TOKEN_NUMERIC_LITERAL)
        return (0);
    if (!literal->token.lexeme)
        return (0);
    text = literal->token.lexeme;
    length = literal->token.length;
    if (length == 0)
        return (0);
    index = 0;
    sign = 1;
    if (text[0] == '-')
    {
        sign = -1;
        index = 1;
    }
    else if (text[0] == '+')
        index = 1;
    if (index >= length)
        return (0);
    result = 0;
    while (index < length)
    {
        char digit;

        digit = text[index];
        if (digit < '0' || digit > '9')
            return (0);
        result = (result * 10) + (digit - '0');
        index += 1;
    }
    *out_value = result * sign;
    return (1);
}

static int transpiler_semantics_literal_boolean_value(const t_ast_node *literal,
    int *out_value)
{
    if (!literal)
        return (0);
    if (!out_value)
        return (0);
    if (literal->token.kind == LEXER_TOKEN_KEYWORD_TRUE)
    {
        *out_value = 1;
        return (1);
    }
    if (literal->token.kind == LEXER_TOKEN_KEYWORD_FALSE)
    {
        *out_value = 0;
        return (1);
    }
    return (0);
}

static int transpiler_semantics_parse_string_literal(const t_ast_node *literal,
    const char **out_text, size_t *out_length)
{
    const char *text;
    size_t length;

    if (!literal)
        return (0);
    if (!out_text)
        return (0);
    if (!out_length)
        return (0);
    if (literal->token.kind != LEXER_TOKEN_STRING_LITERAL)
        return (0);
    if (!literal->token.lexeme)
        return (0);
    text = literal->token.lexeme;
    length = literal->token.length;
    if (length >= 2
        && (text[0] == '"' || text[0] == '\'')
        && text[0] == text[length - 1])
    {
        *out_text = text + 1;
        *out_length = length - 2;
        return (1);
    }
    *out_text = text;
    *out_length = length;
    return (1);
}

static int transpiler_semantics_condition_constant_result(const t_ast_node *condition,
    int *out_is_constant, int *out_value)
{
    const t_ast_node *left;
    const t_ast_node *right;
    const t_ast_node *operator_node;
    int has_not;
    int is_constant;
    int value;

    if (out_is_constant)
        *out_is_constant = 0;
    if (out_value)
        *out_value = 0;
    if (!condition)
        return (FT_SUCCESS);
    if (ast_node_child_count(condition) < 3)
        return (FT_SUCCESS);
    left = ast_node_get_child(condition, 0);
    operator_node = ast_node_get_child(condition, 1);
    right = ast_node_get_child(condition, 2);
    if (!left || !operator_node || !right)
        return (FT_SUCCESS);
    has_not = 0;
    if (condition->token.kind == LEXER_TOKEN_KEYWORD_NOT)
        has_not = 1;
    is_constant = 0;
    value = 0;
    if (left->kind == AST_NODE_LITERAL && right->kind == AST_NODE_LITERAL)
    {
        if (operator_node->token.kind == LEXER_TOKEN_EQUALS
            || operator_node->token.kind == LEXER_TOKEN_NOT_EQUALS)
        {
            int left_boolean;
            int right_boolean;

            if (transpiler_semantics_literal_boolean_value(left, &left_boolean)
                && transpiler_semantics_literal_boolean_value(right, &right_boolean))
            {
                is_constant = 1;
                value = (left_boolean == right_boolean);
                if (operator_node->token.kind == LEXER_TOKEN_NOT_EQUALS)
                    value = !value;
            }
            else
            {
                const char *left_text;
                const char *right_text;
                size_t left_length;
                size_t right_length;

                if (transpiler_semantics_parse_string_literal(left, &left_text, &left_length)
                    && transpiler_semantics_parse_string_literal(right, &right_text, &right_length))
                {
                    size_t index;

                    is_constant = 1;
                    value = 1;
                    if (left_length != right_length)
                        value = 0;
                    index = 0;
                    while (value && index < left_length)
                    {
                        if (left_text[index] != right_text[index])
                            value = 0;
                        index += 1;
                    }
                    if (operator_node->token.kind == LEXER_TOKEN_NOT_EQUALS)
                        value = !value;
                }
                else
                {
                    long long left_value;
                    long long right_value;

                    if (transpiler_semantics_parse_integer_literal(left, &left_value)
                        && transpiler_semantics_parse_integer_literal(right, &right_value))
                    {
                        is_constant = 1;
                        value = (left_value == right_value);
                        if (operator_node->token.kind == LEXER_TOKEN_NOT_EQUALS)
                            value = !value;
                    }
                }
            }
        }
        else
        {
            long long left_value;
            long long right_value;

            if (transpiler_semantics_parse_integer_literal(left, &left_value)
                && transpiler_semantics_parse_integer_literal(right, &right_value))
            {
                is_constant = 1;
                if (operator_node->token.kind == LEXER_TOKEN_LESS_THAN)
                    value = left_value < right_value;
                else if (operator_node->token.kind == LEXER_TOKEN_LESS_OR_EQUAL)
                    value = left_value <= right_value;
                else if (operator_node->token.kind == LEXER_TOKEN_GREATER_THAN)
                    value = left_value > right_value;
                else if (operator_node->token.kind == LEXER_TOKEN_GREATER_OR_EQUAL)
                    value = left_value >= right_value;
                else
                    is_constant = 0;
            }
        }
    }
    if (is_constant)
    {
        if (has_not)
            value = !value;
        if (out_is_constant)
            *out_is_constant = 1;
        if (out_value)
            *out_value = value;
    }
    return (FT_SUCCESS);
}

static void transpiler_semantics_emit_unreachable_warning(t_transpiler_context *context,
    const t_ast_node *statement, const char *reason)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context)
        return ;
    if (!reason)
        return ;
    pf_snprintf(message, sizeof(message),
        "%s is unreachable because %s",
        transpiler_semantics_statement_name(statement), reason);
    transpiler_semantics_emit_warning(context,
        TRANSPILE_WARNING_SEMANTIC_UNREACHABLE_CODE, message);
}

static void transpiler_semantics_mark_sequence_unreachable(const t_ast_node *sequence,
    t_transpiler_context *context, const char *reason)
{
    size_t index;

    if (!sequence)
        return ;
    if (!context)
        return ;
    index = 0;
    while (index < ast_node_child_count(sequence))
    {
        const t_ast_node *statement;

        statement = ast_node_get_child(sequence, index);
        if (statement)
            transpiler_semantics_emit_unreachable_warning(context, statement, reason);
        index += 1;
    }
}

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
        if (statement->kind == AST_NODE_IF_STATEMENT)
        {
            const t_ast_node *condition;
            int is_constant;
            int constant_value;

            condition = NULL;
            if (ast_node_child_count(statement) >= 1)
                condition = ast_node_get_child(statement, 0);
            is_constant = 0;
            constant_value = 0;
            if (transpiler_semantics_condition_constant_result(condition,
                    &is_constant, &constant_value) == FT_SUCCESS && is_constant)
            {
                if (constant_value == 0)
                {
                    const t_ast_node *branch;

                    branch = NULL;
                    if (ast_node_child_count(statement) >= 2)
                        branch = ast_node_get_child(statement, 1);
                    if (branch)
                        transpiler_semantics_mark_sequence_unreachable(branch, context,
                            "the IF condition is always false");
                }
                else
                {
                    const t_ast_node *branch;

                    branch = NULL;
                    if (ast_node_child_count(statement) >= 3)
                        branch = ast_node_get_child(statement, 2);
                    if (branch)
                        transpiler_semantics_mark_sequence_unreachable(branch, context,
                            "the IF condition is always true");
                }
            }
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
    int unreachable;
    const char *unreachable_reason;

    if (!sequence)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    status = FT_SUCCESS;
    index = 0;
    unreachable = 0;
    unreachable_reason = NULL;
    while (index < ast_node_child_count(sequence))
    {
        const t_ast_node *statement;

        statement = ast_node_get_child(sequence, index);
        if (statement)
        {
            if (unreachable && unreachable_reason)
                transpiler_semantics_emit_unreachable_warning(context, statement,
                    unreachable_reason);
            if (transpiler_semantics_validate_statement(statement, scope, context) != FT_SUCCESS)
                status = FT_FAILURE;
            if (!unreachable && transpiler_semantics_statement_is_terminator(statement))
            {
                unreachable = 1;
                if (statement->kind == AST_NODE_STOP_STATEMENT)
                    unreachable_reason = "a prior STOP RUN terminates control flow";
                else
                    unreachable_reason = "control flow does not continue past this statement";
            }
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
