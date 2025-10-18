#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

#include "transpiler_semantics_internal.hpp"

int transpiler_semantics_classify_unary_expression(const t_ast_node *expression,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantics_value_classifier classifier,
    t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
    size_t *out_scale, int *out_scale_known)
{
    const t_ast_node *operator_node;
    const t_ast_node *operand;
    t_transpiler_semantic_data_kind operand_kind;
    size_t operand_length;
    const char *expression_role;
    char operand_role[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    t_lexer_token_kind operator_kind;
    const char *operator_text;
    int status;
    size_t operand_scale;
    int operand_scale_known;

    if (out_kind)
        *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    if (out_length)
        *out_length = 0;
    if (out_scale)
        *out_scale = 0;
    if (out_scale_known)
        *out_scale_known = 0;
    expression_role = (role && role[0] != '\0') ? role : "unary expression";
    if (!expression)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (!classifier)
        return (FT_FAILURE);
    if (ast_node_child_count(expression) < 2)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing an operator or operand", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    operator_node = ast_node_get_child(expression, 0);
    operand = ast_node_get_child(expression, 1);
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing an arithmetic operator", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (!operand)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing an operand", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    operator_kind = operator_node->token.kind;
    if (operator_kind == LEXER_TOKEN_MINUS)
        operator_text = "-";
    else if (operator_kind == LEXER_TOKEN_PLUS)
        operator_text = "+";
    else if (operator_kind == LEXER_TOKEN_KEYWORD_ABS)
    {
        if (operator_node->token.lexeme && operator_node->token.length > 0)
            operator_text = operator_node->token.lexeme;
        else
            operator_text = "ABS";
    }
    else
    {
        if (operator_node->token.lexeme && operator_node->token.length > 0)
            operator_text = operator_node->token.lexeme;
        else
            operator_text = "<operator>";
        pf_snprintf(message, sizeof(message),
            "unary operator '%s' is not supported in %s",
            operator_text, expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (role && role[0] != '\0')
        pf_snprintf(operand_role, sizeof(operand_role), "%s operand", role);
    else
        ft_strlcpy(operand_role, "operand", sizeof(operand_role));
    operand_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    operand_length = 0;
    operand_scale = 0;
    operand_scale_known = 0;
    status = classifier(operand, scope, context, operand_role,
        &operand_kind, &operand_length, &operand_scale, &operand_scale_known);
    if (status != FT_SUCCESS)
        return (FT_FAILURE);
    if (!transpiler_semantics_is_numeric_kind(operand_kind))
    {
        pf_snprintf(message, sizeof(message),
            "unary operator '%s' requires numeric or floating operands but %s is %s",
            operator_text, operand_role,
            transpiler_semantics_kind_to_string(operand_kind));
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (out_kind)
        *out_kind = operand_kind;
    if (out_length)
        *out_length = operand_length;
    if (out_scale)
        *out_scale = operand_scale;
    if (out_scale_known)
        *out_scale_known = operand_scale_known;
    return (FT_SUCCESS);
}

int transpiler_semantics_classify_arithmetic_expression(const t_ast_node *expression,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantics_value_classifier classifier,
    t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
    size_t *out_scale, int *out_scale_known)
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
    size_t result_scale;
    int result_scale_known;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    char left_role[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    char right_role[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    const char *expression_role;
    t_lexer_token_kind operator_kind;
    const char *operator_text;
    int status;

    if (out_kind)
        *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    if (out_length)
        *out_length = 0;
    if (out_scale)
        *out_scale = 0;
    if (out_scale_known)
        *out_scale_known = 0;
    expression_role = (role && role[0] != '\0') ? role : "arithmetic expression";
    if (!expression)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (!classifier)
        return (FT_FAILURE);
    if (ast_node_child_count(expression) < 3)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing operands or operator", expression_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    left = ast_node_get_child(expression, 0);
    operator_node = ast_node_get_child(expression, 1);
    right = ast_node_get_child(expression, 2);
    if (role && role[0] != '\0')
    {
        pf_snprintf(left_role, sizeof(left_role), "%s left operand", role);
        pf_snprintf(right_role, sizeof(right_role), "%s right operand", role);
    }
    else
    {
        ft_strlcpy(left_role, "left operand", sizeof(left_role));
        ft_strlcpy(right_role, "right operand", sizeof(right_role));
    }
    left_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    right_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    left_length = 0;
    right_length = 0;
    left_scale = 0;
    right_scale = 0;
    left_scale_known = 0;
    right_scale_known = 0;
    result_scale = 0;
    result_scale_known = 0;
    status = FT_SUCCESS;
    if (classifier(left, scope, context, left_role, &left_kind, &left_length,
            &left_scale, &left_scale_known) != FT_SUCCESS)
        status = FT_FAILURE;
    if (classifier(right, scope, context, right_role, &right_kind, &right_length,
            &right_scale, &right_scale_known) != FT_SUCCESS)
        status = FT_FAILURE;
    if (!operator_node || operator_node->kind != AST_NODE_ARITHMETIC_OPERATOR)
    {
        pf_snprintf(message, sizeof(message),
            "%s is missing an arithmetic operator", expression_role);
        if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
            status = FT_FAILURE;
        else
            status = FT_FAILURE;
        return (status);
    }
    operator_kind = operator_node->token.kind;
    if (operator_node->token.lexeme)
        operator_text = operator_node->token.lexeme;
    else if (operator_kind == LEXER_TOKEN_KEYWORD_MOD)
        operator_text = "MOD";
    else if (operator_kind == LEXER_TOKEN_MINUS)
        operator_text = "-";
    else if (operator_kind == LEXER_TOKEN_STAR)
        operator_text = "*";
    else if (operator_kind == LEXER_TOKEN_SLASH)
        operator_text = "/";
    else
        operator_text = "+";
    if (operator_kind == LEXER_TOKEN_PLUS
        || operator_kind == LEXER_TOKEN_MINUS)
    {
        if (left_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && !transpiler_semantics_is_numeric_kind(left_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires numeric or floating operands but %s is %s",
                operator_text, left_role,
                transpiler_semantics_kind_to_string(left_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (right_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && !transpiler_semantics_is_numeric_kind(right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires numeric or floating operands but %s is %s",
                operator_text, right_role,
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS
            && !transpiler_semantics_numeric_kinds_match(left_kind, right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires operands of the same type but %s is %s and %s is %s",
                operator_text, left_role,
                transpiler_semantics_kind_to_string(left_kind), right_role,
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS)
        {
            if (out_kind)
            {
                if (left_kind == TRANSPILE_SEMANTIC_DATA_FLOATING
                    || right_kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
                    *out_kind = TRANSPILE_SEMANTIC_DATA_FLOATING;
                else if (left_kind == TRANSPILE_SEMANTIC_DATA_UNKNOWN
                    || right_kind == TRANSPILE_SEMANTIC_DATA_UNKNOWN)
                    *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
                else
                    *out_kind = TRANSPILE_SEMANTIC_DATA_NUMERIC;
            }
            if (out_length)
            {
                if (left_length > right_length)
                    *out_length = left_length;
                else
                    *out_length = right_length;
            }
            if (out_scale || out_scale_known)
            {
                if ((left_kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
                    || (right_kind == TRANSPILE_SEMANTIC_DATA_FLOATING))
                {
                    if (left_scale_known && right_scale_known)
                    {
                        if (left_scale > right_scale)
                            result_scale = left_scale;
                        else
                            result_scale = right_scale;
                        result_scale_known = 1;
                    }
                    else if (left_scale_known)
                    {
                        result_scale = left_scale;
                        result_scale_known = 0;
                    }
                    else if (right_scale_known)
                    {
                        result_scale = right_scale;
                        result_scale_known = 0;
                    }
                    else
                    {
                        result_scale = 0;
                        result_scale_known = 0;
                    }
                }
                else
                {
                    result_scale = 0;
                    if (left_kind == TRANSPILE_SEMANTIC_DATA_UNKNOWN
                        && right_kind == TRANSPILE_SEMANTIC_DATA_UNKNOWN)
                        result_scale_known = 0;
                    else
                        result_scale_known = 1;
                }
            }
            if (out_scale)
                *out_scale = result_scale;
            if (out_scale_known)
                *out_scale_known = result_scale_known;
        }
        return (status);
    }
    if (operator_kind == LEXER_TOKEN_KEYWORD_MOD)
    {
        if (left_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && left_kind != TRANSPILE_SEMANTIC_DATA_NUMERIC)
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires integral operands but %s is %s",
                operator_text, left_role,
                transpiler_semantics_kind_to_string(left_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (right_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && right_kind != TRANSPILE_SEMANTIC_DATA_NUMERIC)
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires integral operands but %s is %s",
                operator_text, right_role,
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS)
        {
            if (out_kind)
                *out_kind = TRANSPILE_SEMANTIC_DATA_NUMERIC;
            if (out_length)
            {
                if (left_length > right_length)
                    *out_length = left_length;
                else
                    *out_length = right_length;
            }
            if (out_scale)
                *out_scale = 0;
            if (out_scale_known)
                *out_scale_known = 1;
        }
        return (status);
    }
    if (operator_kind == LEXER_TOKEN_STAR
        || operator_kind == LEXER_TOKEN_SLASH)
    {
        if (left_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && !transpiler_semantics_is_numeric_kind(left_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires numeric or floating operands but %s is %s",
                operator_text, left_role,
                transpiler_semantics_kind_to_string(left_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (right_kind != TRANSPILE_SEMANTIC_DATA_UNKNOWN
            && !transpiler_semantics_is_numeric_kind(right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires numeric or floating operands but %s is %s",
                operator_text, right_role,
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS
            && !transpiler_semantics_numeric_kinds_match(left_kind, right_kind))
        {
            pf_snprintf(message, sizeof(message),
                "arithmetic operator '%s' requires operands of the same type but %s is %s and %s is %s",
                operator_text, left_role,
                transpiler_semantics_kind_to_string(left_kind), right_role,
                transpiler_semantics_kind_to_string(right_kind));
            if (transpiler_semantics_emit_invalid_expression(context, message) != FT_SUCCESS)
                status = FT_FAILURE;
            else
                status = FT_FAILURE;
        }
        if (status == FT_SUCCESS)
        {
            if (out_kind)
                *out_kind = TRANSPILE_SEMANTIC_DATA_FLOATING;
            if (out_length)
            {
                if (left_length > right_length)
                    *out_length = left_length;
                else
                    *out_length = right_length;
            }
            if (out_scale || out_scale_known)
            {
                if (operator_kind == LEXER_TOKEN_STAR)
                {
                    if (left_scale_known && right_scale_known)
                    {
                        if (left_scale > SIZE_MAX - right_scale)
                            result_scale = SIZE_MAX;
                        else
                            result_scale = left_scale + right_scale;
                        result_scale_known = 1;
                    }
                    else
                    {
                        result_scale = 0;
                        result_scale_known = 0;
                    }
                }
                else
                {
                    result_scale = 0;
                    result_scale_known = 0;
                }
            }
            if (out_scale)
                *out_scale = result_scale;
            if (out_scale_known)
                *out_scale_known = result_scale_known;
        }
        return (status);
    }
    pf_snprintf(message, sizeof(message),
        "arithmetic operator '%s' is not supported in %s",
        operator_text, expression_role);
    transpiler_semantics_emit_invalid_expression(context, message);
    return (FT_FAILURE);
}
