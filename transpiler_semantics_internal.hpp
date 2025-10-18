#ifndef TRANSPILE_SEMANTICS_INTERNAL_HPP
#define TRANSPILE_SEMANTICS_INTERNAL_HPP

#include <cstddef>

#include "cblc_transpiler.hpp"

typedef enum e_transpiler_semantic_data_kind
{
    TRANSPILE_SEMANTIC_DATA_UNKNOWN = 0,
    TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC,
    TRANSPILE_SEMANTIC_DATA_BOOLEAN,
    TRANSPILE_SEMANTIC_DATA_NUMERIC,
    TRANSPILE_SEMANTIC_DATA_FLOATING
}   t_transpiler_semantic_data_kind;

typedef struct s_transpiler_semantic_data_item
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_semantic_data_kind kind;
    size_t declared_length;
    size_t declared_scale;
    int has_declared_scale;
    int is_read_only;
    size_t read_count;
    size_t write_count;
    int has_initial_value;
}   t_transpiler_semantic_data_item;

typedef struct s_transpiler_semantic_scope
{
    t_transpiler_semantic_data_item *items;
    size_t item_count;
    size_t item_capacity;
}   t_transpiler_semantic_scope;

typedef int (*t_transpiler_semantics_value_classifier)(const t_ast_node *value,
    const t_transpiler_semantic_scope *scope, t_transpiler_context *context,
    const char *role, t_transpiler_semantic_data_kind *out_kind,
    size_t *out_length, size_t *out_scale, int *out_scale_known);

void    transpiler_semantics_scope_init(t_transpiler_semantic_scope *scope);
void    transpiler_semantics_scope_dispose(t_transpiler_semantic_scope *scope);
const t_transpiler_semantic_data_item
        *transpiler_semantics_scope_lookup(const t_transpiler_semantic_scope *scope,
            const char *name);
int     transpiler_semantics_collect_scope(const t_ast_node *program,
            t_transpiler_semantic_scope *scope, t_transpiler_context *context);
int     transpiler_semantics_validate_identifier_use(const t_transpiler_semantic_scope *scope,
            t_transpiler_context *context, const t_ast_node *identifier,
            int is_target, t_transpiler_semantic_data_kind *out_kind,
            size_t *out_length, size_t *out_scale, int *out_scale_known,
            int *out_is_read_only);
int     transpiler_semantics_analyze_usage(const t_transpiler_semantic_scope *scope,
            t_transpiler_context *context);

int     transpiler_semantics_emit_error(t_transpiler_context *context, int code,
            const char *message);
int     transpiler_semantics_emit_error_at(t_transpiler_context *context, const t_ast_node *node,
            int code, const char *message, const char *suggestion);
int     transpiler_semantics_emit_warning(t_transpiler_context *context, int code,
            const char *message);
int     transpiler_semantics_emit_warning_at(t_transpiler_context *context, const t_ast_node *node,
            int code, const char *message, const char *suggestion);
int     transpiler_semantics_emit_invalid_expression(t_transpiler_context *context,
            const char *message);

const char  *transpiler_semantics_kind_to_string(t_transpiler_semantic_data_kind kind);
t_transpiler_semantic_data_kind
        transpiler_semantics_classify_picture(const char *text);
size_t  transpiler_semantics_picture_alphanumeric_length(const char *text);
size_t  transpiler_semantics_picture_numeric_length(const char *text);
size_t  transpiler_semantics_picture_decimal_scale(const char *text);
t_transpiler_semantic_data_kind
        transpiler_semantics_classify_literal(const t_ast_node *literal);
size_t  transpiler_semantics_literal_alphanumeric_length(const t_ast_node *literal);
size_t  transpiler_semantics_literal_numeric_length(const t_ast_node *literal);
size_t  transpiler_semantics_literal_decimal_scale(const t_ast_node *literal);
t_transpiler_data_item_kind
        transpiler_semantics_convert_kind(t_transpiler_semantic_data_kind kind);
t_transpiler_semantic_data_kind
        transpiler_semantics_kind_from_context(t_transpiler_data_item_kind kind);
int     transpiler_semantics_is_numeric_kind(t_transpiler_semantic_data_kind kind);
int     transpiler_semantics_is_floating_kind(t_transpiler_semantic_data_kind kind);
int     transpiler_semantics_numeric_kinds_match(t_transpiler_semantic_data_kind left,
            t_transpiler_semantic_data_kind right);
int     transpiler_semantics_kinds_compatible(t_transpiler_semantic_data_kind left,
            t_transpiler_semantic_data_kind right);

int     transpiler_semantics_classify_unary_expression(const t_ast_node *expression,
            const t_transpiler_semantic_scope *scope,
            t_transpiler_context *context, const char *role,
            t_transpiler_semantics_value_classifier classifier,
            t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
            size_t *out_scale, int *out_scale_known);
int     transpiler_semantics_classify_arithmetic_expression(const t_ast_node *expression,
            const t_transpiler_semantic_scope *scope,
            t_transpiler_context *context, const char *role,
            t_transpiler_semantics_value_classifier classifier,
            t_transpiler_semantic_data_kind *out_kind, size_t *out_length,
            size_t *out_scale, int *out_scale_known);

int     transpiler_semantics_validate_condition(const t_ast_node *condition,
            const t_transpiler_semantic_scope *scope, t_transpiler_context *context);
int     transpiler_semantics_validate_move_statement(const t_ast_node *move_node,
            const t_transpiler_semantic_scope *scope, t_transpiler_context *context);
int     transpiler_semantics_validate_assignment_statement(
            const t_ast_node *assignment_node,
            const t_transpiler_semantic_scope *scope, t_transpiler_context *context);
int     transpiler_semantics_validate_statements(const t_ast_node *program,
            const t_transpiler_semantic_scope *scope, t_transpiler_context *context);

#endif
