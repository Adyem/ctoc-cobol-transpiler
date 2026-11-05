#include "cblc_transpiler.hpp"

#include <chrono>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"
#include "backend_typed_ir.hpp"

typedef struct s_cblc_lifecycle_constructor_request
{
    t_cblc_data_item receiver;
    size_t constructor_index;
}   t_cblc_lifecycle_constructor_request;

typedef struct s_cobol_text_builder
{
    char *data;
    size_t length;
    size_t capacity;
    int free_format;
    int control_flow_depth;
    char **based_name_slots;
    size_t based_name_capacity;
    size_t based_name_count;
    char *deferred_procedure_text;
    size_t deferred_procedure_length;
    size_t deferred_procedure_capacity;
    std::unordered_set<std::string> *emitted_method_paragraphs;
    std::unordered_map<std::string, std::string> *method_paragraph_return_targets;
    std::unordered_set<std::string> *known_paragraph_definitions;
    std::vector<t_cblc_lifecycle_constructor_request> lifecycle_constructor_requests;
    const char *active_function_name;
    const t_cblc_method *active_method;
    const t_cblc_constructor *active_constructor;
    const t_cblc_statement *active_statement;
    size_t active_statement_index;
    int diagnostic_code;
    char diagnostic_message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
}   t_cobol_text_builder;

typedef struct s_cobol_text_builder_lifecycle_scope
{
    t_cobol_text_builder *builder;
    const t_cblc_method *saved_method;
    const t_cblc_constructor *saved_constructor;
    const char *saved_function_name;

    s_cobol_text_builder_lifecycle_scope(t_cobol_text_builder *value,
        const t_cblc_method *method, const t_cblc_constructor *constructor)
        : builder(value), saved_method(value ? value->active_method : NULL),
          saved_constructor(value ? value->active_constructor : NULL),
          saved_function_name(value ? value->active_function_name : NULL)
    {
        if (builder)
        {
            builder->active_method = method;
            builder->active_constructor = constructor;
            if (method && method->scope_source_name[0] != '\0')
                builder->active_function_name = method->scope_source_name;
            else if (constructor && constructor->scope_source_name[0] != '\0')
                builder->active_function_name = constructor->scope_source_name;
        }
    }

    ~s_cobol_text_builder_lifecycle_scope()
    {
        if (builder)
        {
            builder->active_method = saved_method;
            builder->active_constructor = saved_constructor;
            builder->active_function_name = saved_function_name;
        }
    }
}   t_cobol_text_builder_lifecycle_scope;

typedef struct s_cblc_constructor_parse_state
{
    const t_cblc_struct_type *type;
    int *initialized_fields;
    int active;
}   t_cblc_constructor_parse_state;

static t_cblc_constructor_parse_state g_cblc_constructor_parse_state = {NULL, NULL, 0};
static const t_cblc_struct_type *g_cblc_member_access_type = NULL;
static const char *g_cblc_active_template_parameter = NULL;
static char g_cblc_active_template_parameters[CBLC_TEMPLATE_PARAMETER_MAX]
    [TRANSPILE_IDENTIFIER_MAX];
static size_t g_cblc_active_template_parameter_count = 0;

static cblc::backend::lifetime_analysis_policy cblc_lifetime_policy_for_unit(
    const t_cblc_translation_unit *unit)
{
    cblc::backend::lifetime_analysis_policy policy;

    if (unit)
        policy.max_edge_transfers = unit->max_lifetime_edge_transfers;
    return (policy);
}

static int cblc_parse_std_strcpy(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_file_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_file_read_while(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_try_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_throw_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity);
static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *constructor_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity);
static int cblc_parse_local_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_scalar_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_struct_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_bind_lifecycle_scope(t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t *saved_data_count);
static void cblc_unbind_lifecycle_scope(t_cblc_translation_unit *unit, size_t saved_data_count);
static void cblc_unbind_method_scope_preserving_locals(
    t_cblc_translation_unit *unit, size_t saved_data_count,
    const char *method_function_name);
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
static int cblc_call_arguments_match_parameters(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count,
    const t_cblc_parameter *parameters, size_t parameter_count);
static int cblc_parameter_kind_from_data_kind(t_cblc_data_kind kind,
    t_transpiler_function_parameter_kind *out_kind);
static void cblc_replace_template_text(char *text, size_t text_size,
    const char *from, const char *to);
static int cblc_build_template_instance_names(const char *template_name,
    const char *argument, char *source_name, size_t source_size,
    char *cobol_name, size_t cobol_size);
static int cblc_normalize_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const char *call_arguments,
    size_t call_argument_count, char *buffer,
    size_t buffer_size);
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
static const t_cblc_function *cblc_find_template_function(
    const t_cblc_translation_unit *unit, const char *identifier);
static int cblc_parse_function(const char **cursor, t_cblc_translation_unit *unit);
static void cblc_record_template_parse_error(t_cblc_translation_unit *unit,
    int code, const char *message);
static size_t cblc_source_offset(const t_cblc_translation_unit *unit,
    const char *position);
static const t_cblc_struct_type *cblc_find_receiver_type(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item);
static t_cblc_data_item *cblc_find_data_item(t_cblc_translation_unit *unit,
    const char *identifier);
static t_cblc_file_declaration *cblc_find_file(t_cblc_translation_unit *unit,
    const char *source_name);
static const t_cblc_data_item *cblc_find_data_item_by_cobol(
    const t_cblc_translation_unit *unit, const char *identifier);
static int cblc_parse_data_reference_text(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference, char *out_reference,
    size_t out_reference_size);
static int cblc_add_named_data_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, const char *owner_function_name, int is_function_local,
    int is_alias, int is_const);
static int cblc_add_struct_instance_field_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *instance_source_name,
    const char *instance_cobol_name, const char *owner_function_name,
    int is_function_local, int is_alias);
static int cblc_register_imported_parameter_storage(t_cblc_translation_unit *unit,
    const t_cblc_parameter *parameters, size_t parameter_count, int is_imported);
static int cblc_add_temp_alias_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, int is_const);
static int cblc_data_kind_from_parameter_kind(t_transpiler_function_parameter_kind parameter_kind,
    t_cblc_data_kind *out_kind);
static int cblc_constructor_signatures_match(const t_cblc_constructor *constructor,
    const t_cblc_parameter *parameters, size_t parameter_count);
const t_cblc_constructor *cblc_find_constructor_for_arguments(
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
static int cblc_parse_delete_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_register_scope(t_cblc_translation_unit *unit, size_t parent_id,
    const char *owner_function_name, size_t *out_scope_id);
static int cblc_parse_template_argument_list(const char **cursor,
    t_cblc_translation_unit *unit,
    char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX],
    size_t *out_count, size_t initial_nesting_depth);
static const t_cblc_struct_type *cblc_instantiate_template_struct_arguments(
    t_cblc_translation_unit *unit, const char *template_name,
    const char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX],
    size_t argument_count);
static int cblc_template_argument_ref(t_cblc_translation_unit *unit,
    const char *argument, size_t *out_ref_id);
static void cblc_record_struct_template_use(t_cblc_translation_unit *unit,
    const char *template_name, size_t argument_ref_id, size_t use_offset);
static const t_cblc_method *cblc_find_method_on_type_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *identifier, const char *call_arguments,
    size_t call_argument_count);
static int cblc_propagate_local_exception_summaries(
    t_cblc_translation_unit *unit);

#include "src/cblc/transpiler_cblc_text_helpers.inc"
static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size);
static const t_cblc_data_item *cblc_find_data_item_const(
    const t_cblc_translation_unit *unit, const char *identifier);

#include "src/cblc/transpiler_cblc_storage.inc"
#include "src/cblc/transpiler_cblc_scalar_helpers.inc"

#include "src/cblc/transpiler_cblc_pointer_parse.inc"
#include "src/cblc/transpiler_cblc_declarations.inc"
static int cblc_register_parameter_destructors(
    const t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_append_parameter_destructor_statements(
    const t_cblc_translation_unit *unit, t_cblc_function *function);
#include "src/cblc/transpiler_cblc_data_call_helpers.inc"

#include "src/cblc/transpiler_cblc_lifecycle_parse.inc"

#include "src/cblc/transpiler_cblc_statement_parse.inc"

#include "src/cblc/transpiler_cblc_unit_parse.inc"

static int cblc_emit_substituted_statement_with_flow(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement, const char *replacement,
    t_cobol_text_builder *builder, int *control_flow_depth,
    const t_cblc_data_item *receiver_context = NULL);

#include "src/cblc/transpiler_cblc_lifecycle_emit.inc"

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_data_item *receiver_context = NULL);
static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_data_item *receiver_context = NULL);
static int cblc_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_cobol_text_builder *builder);
static int cblc_build_external_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size);
static int cblc_emit_pointer_assignment_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line,
    size_t line_size, const t_cblc_data_item *receiver_context = NULL,
    const t_cblc_function *function_context = NULL);
static int cblc_emit_struct_pointer_element_copy_assignment(
    const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_cobol_text_builder *builder,
    const t_cblc_data_item *receiver_context = NULL);
static const t_cblc_data_item *cblc_resolve_struct_pointer_item(
    const t_cblc_translation_unit *unit, const char *pointer_name,
    t_cblc_data_item *synthetic_item,
    const t_cblc_data_item *receiver_context = NULL);
static int cblc_emit_pointer_compute_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line,
    size_t line_size);
static int cblc_emit_prepare_struct_pointer_field(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *field_reference,
    size_t field_reference_size, int allow_pointer_member = 0);
static int cblc_emit_prepare_struct_pointer_value(
    const t_cblc_translation_unit *unit, const char *reference,
    t_cobol_text_builder *builder, char *based_name, size_t based_name_size,
    const t_cblc_data_item *receiver_context = NULL,
    const t_cblc_function *function_context = NULL);
static int cblc_parse_pointer_element_reference(const t_cblc_translation_unit *unit,
    const char *reference, const t_cblc_data_item **out_pointer_item,
    t_cblc_data_kind *out_element_kind, char *index_expression,
    size_t index_expression_size, int *out_has_index);
static int cblc_emit_prepare_pointer_element(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *based_name,
    size_t based_name_size, const t_cblc_data_item *receiver_context = NULL,
    const t_cblc_function *function_context = NULL);
static int cblc_function_return_paragraph_name(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, char *buffer, size_t buffer_size);
static int cblc_emit_parameter_argument_moves(
    const t_cblc_translation_unit *unit, const char *call_arguments,
    size_t call_argument_count, const t_cblc_parameter *parameters,
    size_t parameter_count, t_cobol_text_builder *builder,
    const t_cblc_parameter *source_parameters = NULL,
    size_t source_parameter_count = 0, int borrow_struct_arguments = 0);
static int cblc_method_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *owner_type, const t_cblc_method *method,
    size_t depth);
static int cblc_emit_method_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    const char *assign_target, t_cobol_text_builder *builder);
static int cblc_emit_method_exception_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    const char *assign_target, t_cobol_text_builder *builder, int emit_call);

#include "src/cblc/transpiler_cblc_substituted_emit.inc"

#include "src/cblc/transpiler_cblc_call_match_helpers.inc"

#include "src/cblc/transpiler_cblc_cobol_emit.inc"
#include "src/cblc/transpiler_cblc_generate.inc"
