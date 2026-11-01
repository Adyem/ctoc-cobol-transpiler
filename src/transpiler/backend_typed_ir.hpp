#ifndef CBLC_BACKEND_TYPED_IR_HPP
#define CBLC_BACKEND_TYPED_IR_HPP

#include <cstdint>
#include <string>
#include <vector>

#include "cblc_transpiler.hpp"

namespace cblc::backend
{

inline constexpr std::uint32_t typed_ir_schema_version = 13;

using value_id = std::uint32_t;
using place_id = std::uint32_t;
using operation_id = std::uint32_t;
using type_id = std::uint32_t;
using symbol_id = std::uint32_t;
using region_id = std::uint32_t;
using basic_block_id = std::uint32_t;

enum class value_category
{
    scalar,
    owned_object,
    borrow,
    pointer_or_handle,
    compiler_only
};

enum class access_mode
{
    read_only,
    read_write
};

enum class opcode
{
    literal,
    load,
    store,
    construct,
    copy_construct,
    move_construct,
    assign,
    copy_assign,
    move_assign,
    call,
    compare,
    add,
    subtract,
    checked_access,
    destroy,
    catch_bind,
    return_value,
    throw_value,
    scope_exit,
    numeric_convert
};

enum class comparison_kind
{
    none,
    equal,
    not_equal,
    less,
    less_equal,
    greater,
    greater_equal
};

enum class region_kind
{
    function_body,
    lexical_scope,
    conditional,
    loop,
    exception_try,
    exception_handler,
    cleanup
};

enum class exception_effect
{
    non_throwing,
    may_throw_known,
    unknown
};

enum class outcome_kind
{
    normal,
    thrown_exception,
    allocation_failure,
    checked_access_failure,
    fatal_termination
};

struct source_origin
{
    std::uint64_t source_unit_id = 0;
    std::uint64_t source_offset = 0;
    std::uint32_t source_length = 0;
    std::uint32_t line = 0;
    std::uint32_t column = 0;
    std::string role;
    bool compiler_generated = false;
};

struct typed_value
{
    value_id id = 0;
    type_id type = 0;
    value_category category = value_category::scalar;
    access_mode access = access_mode::read_only;
    std::uint64_t lifetime_token = 0;
    bool is_definition = true;
    std::vector<source_origin> provenance;
    std::string constant_value;
    /* Nonzero only for compiler-only aggregate storage values. */
    std::size_t array_extent = 0;
};

struct place_projection
{
    enum class kind { field, index, dereference } projection_kind = kind::field;
    std::uint32_t resolved_member = 0;
    type_id resulting_type = 0;
    /* Nonzero for a runtime-indexed projection; otherwise resolved_member is
     * the one-based static aggregate index. */
    value_id index_value = 0;
};

struct typed_symbol
{
    symbol_id id = 0;
    std::string canonical_name;
    type_id type = 0;
    value_category category = value_category::scalar;
    bool is_parameter = false;
};

struct typed_place
{
    place_id id = 0;
    value_id base_value = 0;
    type_id type = 0;
    access_mode access = access_mode::read_only;
    std::uint64_t owner_lifetime_token = 0;
    std::vector<place_projection> projections;
    /* Zero only for compiler-generated temporaries with no source binding. */
    symbol_id symbol = 0;
    /* Optional address root when base_value is a by-value owned subobject. */
    value_id storage_base_value = 0;
};

enum class typed_type_kind
{
    primitive,
    class_type,
    pointer
};

struct typed_field
{
    std::uint32_t member_id = 0;
    std::string name;
    type_id type = 0;
    std::size_t array_extent = 0;
    bool is_borrowed = false;
    bool is_dependent = false;
};

struct typed_type
{
    type_id id = 0;
    typed_type_kind kind = typed_type_kind::primitive;
    std::string canonical_name;
    std::vector<typed_field> fields;
    type_id pointee_type = 0;
};

enum class callable_kind
{
    method,
    copy_assignment,
    move_assignment,
    constructor,
    copy_constructor,
    move_constructor,
    destructor,
    function
};

/* Canonical semantic target for lifecycle and method dispatch. */
struct typed_callable
{
    symbol_id id = 0;
    callable_kind kind = callable_kind::method;
    type_id owner_type = 0;
    std::size_t overload_index = 0;
    std::string canonical_name;
    std::vector<type_id> parameter_types;
    type_id result_type = 0;
    exception_effect exceptions = exception_effect::unknown;
    bool has_definition = false;
    std::vector<std::uint64_t> known_exception_type_ids;
};

struct effect_summary
{
    std::vector<place_id> reads;
    std::vector<place_id> writes;
    bool allocates = false;
    bool deallocates = false;
    bool constructs = false;
    bool destroys = false;
    bool may_fail = false;
    exception_effect exceptions = exception_effect::unknown;
    std::vector<std::uint64_t> known_exception_type_ids;
};

struct operation_outcome
{
    outcome_kind kind = outcome_kind::normal;
    std::vector<value_id> initialized;
    std::vector<value_id> live;
    std::vector<value_id> moved_from;
    std::vector<value_id> uninitialized;
    std::vector<value_id> destroyed;
    /* Concrete type for thrown_exception outcomes; zero means unknown. */
    std::uint64_t exception_type_id = 0;
};

struct typed_operation
{
    operation_id id = 0;
    region_id region = 1;
    opcode operation = opcode::literal;
    comparison_kind comparison = comparison_kind::none;
    std::vector<value_id> operands;
    std::vector<place_id> places;
    std::vector<value_id> results;
    symbol_id callee = 0;
    std::uint64_t exception_type_id = 0;
    std::uint64_t source_exception_region_id = 0;
    bool has_dependent_lookup = false;
    effect_summary effects;
    std::vector<operation_outcome> outcomes;
    std::vector<operation_id> must_follow;
    std::vector<source_origin> provenance;
};

struct typed_region
{
    region_id id = 0;
    region_id parent = 0;
    region_kind kind = region_kind::function_body;
    std::vector<region_id> children;
    std::vector<operation_id> operations;
    /* Mixed source order for direct operations and structured child regions. */
    struct item
    {
        enum class kind { operation, child_region } item_kind = kind::operation;
        std::uint32_t id = 0;
    };
    std::vector<item> sequence;
    /* Loop pre-test operations execute on entry and again on every back-edge. */
    std::vector<operation_id> condition_operations;
    value_id condition = 0;
    std::uint64_t source_exception_region_id = 0;
    std::uint64_t exception_type_id = 0;
    bool catches_all = false;
};

struct typed_function
{
    std::uint32_t schema_version = typed_ir_schema_version;
    std::uint64_t source_unit_id = 0;
    std::uint32_t function_id = 0;
    /* Owned by-value parameters are live at function entry in declaration order. */
    std::vector<value_id> entry_live_values;
    std::vector<typed_symbol> symbols;
    std::vector<typed_type> types;
    std::vector<typed_callable> callables;
    std::vector<typed_value> values;
    std::vector<typed_place> places;
    std::vector<typed_region> regions;
    std::vector<typed_operation> operations;
};

enum class cfg_terminator_kind
{
    jump,
    branch,
    invoke,
    return_value,
    throw_value,
    handler_entry,
    normal_exit,
    uncaught_exception_exit,
    fatal_exit
};

struct cfg_edge
{
    basic_block_id target = 0;
    outcome_kind outcome = outcome_kind::normal;
    std::uint64_t exception_type_id = 0;
    value_id exception_payload = 0;
};

struct cfg_terminator
{
    cfg_terminator_kind kind = cfg_terminator_kind::normal_exit;
    operation_id operation = 0;
    value_id condition = 0;
    region_id control_region = 0;
    region_id exception_handler_region = 0;
    region_id handler_entry_region = 0;
    std::vector<cfg_edge> successors;
};

struct basic_block
{
    basic_block_id id = 0;
    std::vector<operation_id> operations;
    cfg_terminator terminator;
};

struct control_flow_graph
{
    std::uint32_t schema_version = typed_ir_schema_version;
    std::uint64_t source_unit_id = 0;
    std::uint32_t function_id = 0;
    basic_block_id entry = 0;
    std::vector<basic_block> blocks;
};

struct cleanup_site
{
    basic_block_id exit_block = 0;
    cfg_terminator_kind exit_kind = cfg_terminator_kind::normal_exit;
    /* Objects in the order their destroy operations must execute. */
    std::vector<value_id> destruction_order;
};

struct cleanup_edge
{
    basic_block_id source_block = 0;
    basic_block_id target_block = 0;
    operation_id operation = 0;
    outcome_kind outcome = outcome_kind::normal;
    /* Completed objects to destroy on this exact failure CFG edge. */
    std::vector<value_id> destruction_order;
};

struct lifecycle_cleanup_plan
{
    std::uint32_t schema_version = typed_ir_schema_version;
    std::uint64_t source_unit_id = 0;
    std::uint32_t function_id = 0;
    std::vector<cleanup_site> exits;
    std::vector<cleanup_edge> failure_edges;
};

/* Immutable per-pass work quota. One unit is charged for each CFG edge
 * propagated for one concrete incoming lifetime state. */
struct lifetime_analysis_policy
{
    std::size_t max_edge_transfers =
        TRANSPILE_DEFAULT_MAX_LIFETIME_EDGE_TRANSFERS;
};

enum class verification_error
{
    none,
    unsupported_schema,
    missing_function_identity,
    duplicate_or_zero_id,
    dangling_reference,
    invalid_type_or_projection,
    immutable_write,
    incomplete_call_target,
    use_before_definition,
    missing_provenance,
    missing_effect_or_outcome,
    invalid_outcome_state,
    cyclic_evaluation_order,
    invalid_region_tree,
    unsupported_cfg_region,
    invalid_cfg_block,
    invalid_cfg_edge,
    invalid_cfg_terminator,
    unreachable_cfg_block,
    invalid_lifetime_transition,
    inconsistent_lifetime_join,
    resource_limit_exceeded,
    live_object_at_exit,
    use_after_lifetime_end,
    unsupported_dynamic_initialization,
    unsupported_legacy_statement,
    unsupported_legacy_type
};

struct verification_diagnostic
{
    verification_error code = verification_error::none;
    operation_id operation = 0;
    std::string message;
};

bool verify_typed_function(const typed_function &function,
    verification_diagnostic *diagnostic);
bool verify_global_object_lifetimes(const t_cblc_translation_unit *unit,
    verification_diagnostic *diagnostic);
bool lower_scalar_function(const t_cblc_translation_unit *unit,
    std::size_t function_index, typed_function *result,
    verification_diagnostic *diagnostic);
symbol_id canonical_constructor_symbol_id(const t_cblc_struct_type *owner,
    const t_cblc_constructor *constructor);
bool lower_scalar_method(const t_cblc_translation_unit *unit,
    std::size_t type_index, std::size_t method_index, typed_function *result,
    verification_diagnostic *diagnostic);
bool build_linear_control_flow_graph(const typed_function &function,
    control_flow_graph *result, verification_diagnostic *diagnostic);
bool build_structured_control_flow_graph(const typed_function &function,
    control_flow_graph *result, verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy = lifetime_analysis_policy{});
bool build_structured_control_flow_graph_with_cleanup(
    const typed_function &function, control_flow_graph *result,
    lifecycle_cleanup_plan *cleanup_plan,
    verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy = lifetime_analysis_policy{});
bool verify_control_flow_graph(const typed_function &function,
    const control_flow_graph &graph, verification_diagnostic *diagnostic);
bool verify_lifetime_transitions(const typed_function &function,
    const control_flow_graph &graph, verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy = lifetime_analysis_policy{});
bool derive_lifecycle_cleanup_plan(const typed_function &function,
    const control_flow_graph &graph, lifecycle_cleanup_plan *result,
    verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy = lifetime_analysis_policy{});

}

#endif
