#include "libft/Printf/printf.hpp"

#include "transpiler_semantics_internal.hpp"

static int transpiler_semantics_classify_move_value(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
    size_t *out_scale, int *out_scale_known)
{
    t_transpiler_semantic_data_kind kind;
    size_t length;
    const char *move_role;
    size_t scale;
    int scale_known;

    kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    length = 0;
    scale = 0;
    scale_known = 0;
    move_role = (role && role[0] != '\0') ? role : "MOVE source";
    if (out_kind)
        *out_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    if (out_length)
        *out_length = 0;
    if (out_scale)
        *out_scale = 0;
    if (out_scale_known)
        *out_scale_known = 0;
    if (!value)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "%s is missing", move_role);
        return (transpiler_semantics_emit_invalid_expression(context, message));
    }
    if (value->kind == AST_NODE_IDENTIFIER)
    {
        if (transpiler_semantics_validate_identifier_use(scope, context, value, 0,
                &kind, &length, &scale, &scale_known, NULL) != FT_SUCCESS)
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
            context, move_role, transpiler_semantics_classify_move_value, out_kind,
            out_length, out_scale, out_scale_known));
    }
    else if (value->kind == AST_NODE_ARITHMETIC_EXPRESSION)
    {
        return (transpiler_semantics_classify_arithmetic_expression(value, scope,
            context, move_role, transpiler_semantics_classify_move_value, out_kind,
            out_length, out_scale, out_scale_known));
    }
    else
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "%s must be an identifier, literal, unary expression, or arithmetic expression",
            move_role);
        transpiler_semantics_emit_invalid_expression(context, message);
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

static int transpiler_semantics_validate_assignment_like_statement(const t_ast_node *statement,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *statement_label, const char *role_prefix, int invalid_code)
{
    const t_ast_node *source;
    const t_ast_node *target;
    t_transpiler_semantic_data_kind target_kind;
    t_transpiler_semantic_data_kind source_kind;
    size_t target_length;
    size_t source_length;
    size_t target_scale;
    size_t source_scale;
    int target_scale_known;
    int source_scale_known;
    int target_is_read_only;
    char source_role[64];
    char target_role[64];
    const char *label;
    int status;

    if (!statement)
        return (FT_SUCCESS);
    if (!scope)
        return (FT_FAILURE);
    if (!context)
        return (FT_FAILURE);
    if (ast_node_child_count(statement) < 2)
        return (FT_FAILURE);
    source = ast_node_get_child(statement, 0);
    target = ast_node_get_child(statement, 1);
    target_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    source_kind = TRANSPILE_SEMANTIC_DATA_UNKNOWN;
    target_length = 0;
    source_length = 0;
    target_scale = 0;
    source_scale = 0;
    target_scale_known = 0;
    source_scale_known = 0;
    target_is_read_only = 0;
    status = FT_SUCCESS;
    if (role_prefix && role_prefix[0] != '\0')
    {
        pf_snprintf(source_role, sizeof(source_role), "%s source", role_prefix);
        pf_snprintf(target_role, sizeof(target_role), "%s target", role_prefix);
    }
    else
    {
        ft_strlcpy(source_role, "source", sizeof(source_role));
        ft_strlcpy(target_role, "target", sizeof(target_role));
    }
    label = (statement_label && statement_label[0] != '\0') ? statement_label : "assignment";
    if (!target || target->kind != AST_NODE_IDENTIFIER)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        pf_snprintf(message, sizeof(message),
            "%s statement is missing a valid target identifier", label);
        transpiler_semantics_emit_error(context, invalid_code, message);
        status = FT_FAILURE;
    }
    else if (transpiler_semantics_validate_identifier_use(scope, context, target, 1,
            &target_kind, &target_length, &target_scale,
            &target_scale_known, &target_is_read_only) != FT_SUCCESS)
        status = FT_FAILURE;
    if (source)
    {
        if (transpiler_semantics_classify_move_value(source, scope, context,
                source_role, &source_kind, &source_length,
                &source_scale, &source_scale_known) != FT_SUCCESS)
            status = FT_FAILURE;
    }
    if (target_is_read_only)
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        const char *target_name;

        target_name = (target && target->token.lexeme) ? target->token.lexeme : "<target>";
        pf_snprintf(message, sizeof(message),
            "%s '%s' is read-only and cannot be modified",
            target_role, target_name);
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_IMMUTABLE_TARGET, message);
        status = FT_FAILURE;
    }
    if (!transpiler_semantics_kinds_compatible(target_kind, source_kind))
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        const char *target_name;
        const char *source_name;

        target_name = (target && target->token.lexeme) ? target->token.lexeme : "<target>";
        if (source && source->token.lexeme)
            source_name = source->token.lexeme;
        else if (source && source->kind == AST_NODE_LITERAL && source->token.lexeme)
            source_name = source->token.lexeme;
        else
            source_name = "<source>";
        pf_snprintf(message, sizeof(message),
            "%s '%s' (%s) is incompatible with %s '%s' (%s)",
            source_role, source_name, transpiler_semantics_kind_to_string(source_kind),
            target_role, target_name, transpiler_semantics_kind_to_string(target_kind));
        transpiler_semantics_emit_error(context,
            TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message);
        status = FT_FAILURE;
    }
    if (status == FT_SUCCESS
        && (target_kind == TRANSPILE_SEMANTIC_DATA_NUMERIC
            || target_kind == TRANSPILE_SEMANTIC_DATA_FLOATING))
    {
        if (source_kind == TRANSPILE_SEMANTIC_DATA_FLOATING
            && target_kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            pf_snprintf(message, sizeof(message),
                "%s is floating but %s is numeric",
                source_role, target_role);
            transpiler_semantics_emit_error(context,
                TRANSPILE_ERROR_SEMANTIC_FLOATING_TRUNCATION, message);
            status = FT_FAILURE;
        }
        if (status == FT_SUCCESS
            && source_scale_known && target_scale_known
            && source_scale > target_scale)
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            pf_snprintf(message, sizeof(message),
                "%s has more fractional digits (%zu) than %s (%zu)",
                source_role, source_scale, target_role, target_scale);
            transpiler_semantics_emit_error(context,
                TRANSPILE_ERROR_SEMANTIC_DECIMAL_SCALE_MISMATCH, message);
            status = FT_FAILURE;
        }
        if (status == FT_SUCCESS && source_length > target_length)
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            pf_snprintf(message, sizeof(message),
                "%s has more digits (%zu) than %s (%zu)",
                source_role, source_length, target_role, target_length);
            transpiler_semantics_emit_error(context,
                TRANSPILE_ERROR_SEMANTIC_NUMERIC_OVERFLOW, message);
            status = FT_FAILURE;
        }
    }
    else if (status == FT_SUCCESS
        && target_kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC
        && target_length > 0)
    {
        size_t required_length;

        required_length = 0;
        if (source_kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
            required_length = source_length;
        if (source && source->kind == AST_NODE_LITERAL)
        {
            size_t literal_length;

            literal_length = transpiler_semantics_literal_alphanumeric_length(source);
            if (literal_length > required_length)
                required_length = literal_length;
        }
        if (required_length > target_length)
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
            const char *target_name;
            const char *source_name;

            target_name = (target && target->token.lexeme) ? target->token.lexeme : "<target>";
            if (source && source->token.lexeme)
                source_name = source->token.lexeme;
            else
                source_name = "<source>";
            pf_snprintf(message, sizeof(message),
                "%s '%s' (%zu characters) truncates into %s '%s' (%zu characters)",
                source_role, source_name, required_length, target_role,
                target_name, target_length);
            transpiler_semantics_emit_error(context,
                TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION, message);
            status = FT_FAILURE;
        }
    }
    return (status);
}

int transpiler_semantics_validate_move_statement(const t_ast_node *move_node,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    return (transpiler_semantics_validate_assignment_like_statement(move_node, scope, context,
        "MOVE", "MOVE", TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE));
}

int transpiler_semantics_validate_assignment_statement(const t_ast_node *assignment_node,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context)
{
    return (transpiler_semantics_validate_assignment_like_statement(assignment_node, scope, context,
        "'=' assignment", "assignment", TRANSPILE_ERROR_SEMANTIC_INVALID_ASSIGNMENT));
}
