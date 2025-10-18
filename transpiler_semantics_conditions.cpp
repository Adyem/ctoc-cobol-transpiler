#include "libft/Printf/printf.hpp"

#include "transpiler_semantics_internal.hpp"

static int transpiler_semantics_emit_invalid_condition(t_transpiler_context *context,
    const char *message)
{
    if (!context)
        return (FT_FAILURE);
    if (!message)
        return (FT_FAILURE);
    return (transpiler_semantics_emit_error(context,
        TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION, message));
}

static int transpiler_semantics_classify_condition_identifier(const t_ast_node *identifier,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
    size_t *out_scale, int *out_scale_known)
{
    const t_transpiler_semantic_data_item *item;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!identifier)
        return (FT_FAILURE);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (!role)
        return (FT_FAILURE);
    item = transpiler_semantics_scope_lookup(scope, identifier->token.lexeme);
    if (!item)
    {
        pf_snprintf(message, sizeof(message),
            "condition %s identifier '%s' is not declared in WORKING-STORAGE",
            role, identifier && identifier->token.lexeme ? identifier->token.lexeme : "<unknown>");
        if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_FAILURE);
    }
    if (out_kind)
        *out_kind = item->kind;
    if (out_length)
        *out_length = item->declared_length;
    if (out_scale)
        *out_scale = item->declared_scale;
    if (out_scale_known)
        *out_scale_known = item->has_declared_scale;
    return (FT_SUCCESS);
}

static int transpiler_semantics_classify_condition_value(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
    size_t *out_scale, int *out_scale_known)
{
    t_transpiler_semantic_data_kind kind;
    size_t length;
    size_t scale;
    int scale_known;

    if (!role)
        return (FT_FAILURE);
    kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    length = 0;
    scale = 0;
    scale_known = 0;
    if (!value)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "condition %s is missing", role);
        return (transpiler_semantics_emit_invalid_condition(context, message));
    }
    if (value->kind == AST_NODE_IDENTIFIER)
    {
        if (transpiler_semantics_classify_condition_identifier(value, scope, context,
                role, &kind, &length, &scale, &scale_known) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (value->kind == AST_NODE_LITERAL)
    {
        kind = transpiler_semantics_classify_literal(value);
        if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
        {
            length = transpiler_semantics_literal_alphanumeric_length(value);
            scale = 0;
            scale_known = 0;
        }
        else if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        {
            length = transpiler_semantics_literal_numeric_length(value);
            scale = transpiler_semantics_literal_decimal_scale(value);
            scale_known = 1;
        }
        else
        {
            length = 0;
            scale = 0;
            scale_known = 0;
        }
    }
    else if (value->kind == AST_NODE_UNARY_EXPRESSION)
    {
        return (transpiler_semantics_classify_unary_expression(value, scope,
            context, role, transpiler_semantics_classify_condition_value,
            out_kind, out_length, out_scale, out_scale_known));
    }
    else if (value->kind == AST_NODE_ARITHMETIC_EXPRESSION)
    {
        return (transpiler_semantics_classify_arithmetic_expression(value, scope,
            context, role, transpiler_semantics_classify_condition_value, out_kind,
            out_length, out_scale, out_scale_known));
    }
    else
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "condition %s must be an identifier, literal, unary expression, or arithmetic expression",
            role);
        if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_FAILURE);
    }
    if (out_kind)
        *out_kind = kind;
    if (out_length)
        *out_length = length;
    if (out_scale)
        *out_scale = scale;
    if (out_scale_known)
        *out_scale_known = scale_known;
    return (FT_SUCCESS);
}

int transpiler_semantics_validate_condition(const t_ast_node *condition,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    const t_ast_node *left;
    const t_ast_node *operator_node;
    const t_ast_node *right;
    t_transpiler_semantic_data_kind left_kind;
    t_transpiler_semantic_data_kind right_kind;
    size_t left_length;
    size_t right_length;
    size_t left_scale;
    size_t right_scale;
    int left_scale_known;
    int right_scale_known;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    int status;

    if (!condition)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    left = NULL;
    operator_node = NULL;
    right = NULL;
    left_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    right_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    left_length = 0;
    right_length = 0;
    left_scale = 0;
    right_scale = 0;
    left_scale_known = 0;
    right_scale_known = 0;
    status = FT_SUCCESS;
    if (ast_node_child_count(condition) >= 1)
        left = ast_node_get_child(condition, 0);
    if (ast_node_child_count(condition) >= 2)
        operator_node = ast_node_get_child(condition, 1);
    if (ast_node_child_count(condition) >= 3)
        right = ast_node_get_child(condition, 2);
    if (!left || !operator_node || !right)
    {
        pf_snprintf(message, sizeof(message),
            "condition is missing left operand, operator, or right operand");
        return (transpiler_semantics_emit_invalid_condition(context, message));
    }
    if (transpiler_semantics_classify_condition_value(left, scope, context,
            "left operand", &left_kind, &left_length, &left_scale,
            &left_scale_known) != FT_SUCCESS)
        status = FT_FAILURE;
    if (transpiler_semantics_classify_condition_value(right, scope, context,
            "right operand", &right_kind, &right_length, &right_scale,
            &right_scale_known) != FT_SUCCESS)
        status = FT_FAILURE;
    if (status != FT_SUCCESS)
        return (status);
    if (operator_node->token.kind == LEXER_TOKEN_EQUALS
        || operator_node->token.kind == LEXER_TOKEN_NOT_EQUALS)
    {
        int kinds_match;

        kinds_match = transpiler_semantics_kinds_compatible(left_kind, right_kind);
        if (!kinds_match)
        {
            const char *left_name;
            const char *right_name;

            left_name = left && left->token.lexeme ? left->token.lexeme : "<left>";
            right_name = right && right->token.lexeme ? right->token.lexeme : "<right>";
            if ((transpiler_semantics_is_floating_kind(left_kind)
                    && right_kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
                || (transpiler_semantics_is_floating_kind(right_kind)
                    && left_kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC))
            {
                kinds_match = 1;
            }
            if (!kinds_match)
            {
                pf_snprintf(message, sizeof(message),
                    "operands '%s' (%s) and '%s' (%s) are not compatible for comparison",
                    left_name, transpiler_semantics_kind_to_string(left_kind),
                    right_name, transpiler_semantics_kind_to_string(right_kind));
                if (transpiler_semantics_emit_error(context,
                        TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message) != FT_SUCCESS)
                    status = FT_FAILURE;
                else
                    status = FT_FAILURE;
            }
        }
        if (status == FT_SUCCESS
            && transpiler_semantics_is_numeric_kind(left_kind)
            && transpiler_semantics_is_numeric_kind(right_kind)
            && !transpiler_semantics_numeric_kinds_match(left_kind, right_kind)
            && !transpiler_semantics_is_floating_kind(left_kind)
            && !transpiler_semantics_is_floating_kind(right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "numeric comparison requires matching kinds but left operand is %s and right operand is %s",
                transpiler_semantics_kind_to_string(left_kind),
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_error(context,
                    TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
    }
    else
    {
        const char *operator_text;

        operator_text = operator_node->token.lexeme ? operator_node->token.lexeme : "<operator>";
        if (!transpiler_semantics_is_numeric_kind(left_kind))
        {
            pf_snprintf(message, sizeof(message),
                "relational operator '%s' requires numeric or floating operands but left operand is %s",
                operator_text, transpiler_semantics_kind_to_string(left_kind));
            if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (!transpiler_semantics_is_numeric_kind(right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "relational operator '%s' requires numeric or floating operands but right operand is %s",
                operator_text, transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS
            && !transpiler_semantics_numeric_kinds_match(left_kind, right_kind))
        {
            if (!(transpiler_semantics_is_numeric_kind(left_kind)
                    && transpiler_semantics_is_numeric_kind(right_kind)))
            {
                pf_snprintf(message, sizeof(message),
                    "relational operator '%s' requires compatible kinds but left operand is %s and right operand is %s",
                    operator_text, transpiler_semantics_kind_to_string(left_kind),
                    transpiler_semantics_kind_to_string(right_kind));
                if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
                    status = FT_FAILURE;
                else
                    status = FT_FAILURE;
            }
        }
    }
    if (status == FT_SUCCESS
        && transpiler_semantics_is_numeric_kind(left_kind)
        && transpiler_semantics_is_numeric_kind(right_kind)
        && left_scale_known && right_scale_known)
    {
        if (left_scale > left_length
            || right_scale > right_length)
        {
            pf_snprintf(message, sizeof(message),
                "operands have invalid decimal scale for comparison");
            if (transpiler_semantics_emit_invalid_condition(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
    }
    return (status);
}
