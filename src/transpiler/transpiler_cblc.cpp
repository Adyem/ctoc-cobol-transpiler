#include "cblc_transpiler.hpp"

#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

typedef struct s_cobol_text_builder
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cobol_text_builder;

typedef struct s_cblc_constructor_parse_state
{
    const t_cblc_struct_type *type;
    int *initialized_fields;
    int active;
}   t_cblc_constructor_parse_state;

static t_cblc_constructor_parse_state g_cblc_constructor_parse_state = {NULL, NULL, 0};
static const t_cblc_struct_type *g_cblc_member_access_type = NULL;

static int cblc_parse_std_strcpy(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity);
static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *constructor_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity);
static int cblc_parse_local_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_string_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_struct_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_bind_lifecycle_scope(t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t *saved_data_count);
static void cblc_unbind_lifecycle_scope(t_cblc_translation_unit *unit, size_t saved_data_count);
static int cblc_parse_statement_block(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, int allow_return);
static int cblc_parse_numeric_expression(const char **cursor, t_cblc_translation_unit *unit,
    char *buffer, size_t buffer_size);
static int cblc_parse_numeric_expression_until_paren(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size);
static int cblc_parse_call_argument_list(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size,
    size_t *out_count);
static int cblc_extract_call_argument(const t_cblc_statement *statement,
    size_t argument_index, char *buffer, size_t buffer_size);
static int cblc_argument_matches_parameter(const t_cblc_translation_unit *unit,
    const char *argument, const t_cblc_parameter *parameter);
static int cblc_normalize_call_arguments(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count, char *buffer,
    size_t buffer_size);
static int cblc_parse_string_constructor_clause(const char **cursor,
    t_cblc_translation_unit *unit, size_t *out_length, char *out_arguments,
    size_t out_arguments_size, size_t *out_argument_count);
static int cblc_expression_append(char *buffer, size_t buffer_size, const char *token);
static int cblc_starts_with_function_declaration(const t_cblc_translation_unit *unit,
    const char *cursor);
static int cblc_parse_parameter_list(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_parameter *parameters,
    size_t *parameter_count, const char *scope_source_name,
    const char *scope_cobol_name, const char *owner_function_name,
    const t_cblc_struct_type *self_type);
static int cblc_add_parameter_aliases(t_cblc_translation_unit *unit,
    const t_cblc_parameter *parameters, size_t parameter_count,
    const t_cblc_struct_type *self_type);
static int cblc_add_local_struct_alias_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *alias_source_name,
    const char *actual_cobol_name);
static void cblc_build_constructor_scope_name(const char *base, const char *separator,
    size_t arity, char *buffer, size_t buffer_size);
static int cblc_parse_function_return_type(const char **cursor,
    const t_cblc_translation_unit *unit, t_cblc_function_return_kind *out_kind,
    char *type_name, size_t type_name_size);
static const t_cblc_struct_type *cblc_find_struct_type(const t_cblc_translation_unit *unit,
    const char *identifier);
static const t_cblc_struct_type *cblc_find_receiver_type(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item);
static t_cblc_data_item *cblc_find_data_item(t_cblc_translation_unit *unit,
    const char *identifier);
static const t_cblc_data_item *cblc_find_data_item_by_cobol(
    const t_cblc_translation_unit *unit, const char *identifier);
static int cblc_parse_data_reference_text(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference, char *out_reference,
    size_t out_reference_size);
static int cblc_add_named_data_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, const char *owner_function_name, int is_function_local,
    int is_alias, int is_const);
static int cblc_add_temp_alias_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, int is_const);
static int cblc_constructor_signatures_match(const t_cblc_constructor *constructor,
    const t_cblc_parameter *parameters, size_t parameter_count);
static const t_cblc_constructor *cblc_find_constructor_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *call_arguments, size_t call_argument_count);
static int cblc_data_kind_is_pointer(t_cblc_data_kind kind);
static int cblc_pointer_assignment_is_compatible(t_cblc_data_kind target_kind,
    t_cblc_data_kind source_kind);
static int cblc_parse_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, int is_const);
static int cblc_parse_pointer_allocation_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item);
static int cblc_parse_std_free(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);

#include "src/cblc/transpiler_cblc_text_helpers.inc"
static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size);

#include "src/cblc/transpiler_cblc_storage.inc"
#include "src/cblc/transpiler_cblc_scalar_helpers.inc"

#include "src/cblc/transpiler_cblc_pointer_parse.inc"
#include "src/cblc/transpiler_cblc_declarations.inc"
#include "src/cblc/transpiler_cblc_data_call_helpers.inc"

#include "src/cblc/transpiler_cblc_lifecycle_parse.inc"

#include "src/cblc/transpiler_cblc_statement_parse.inc"

#include "src/cblc/transpiler_cblc_unit_parse.inc"

#include "src/cblc/transpiler_cblc_lifecycle_emit.inc"

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder);
static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder);
static int cblc_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_cobol_text_builder *builder);
static int cblc_build_external_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size);
static int cblc_emit_pointer_assignment_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line,
    size_t line_size);
static int cblc_emit_pointer_compute_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line,
    size_t line_size);
static int cblc_emit_prepare_struct_pointer_field(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *field_reference,
    size_t field_reference_size);

#include "src/cblc/transpiler_cblc_substituted_emit.inc"

#include "src/cblc/transpiler_cblc_call_match_helpers.inc"

#include "src/cblc/transpiler_cblc_cobol_emit.inc"
#include "src/cblc/transpiler_cblc_generate.inc"
