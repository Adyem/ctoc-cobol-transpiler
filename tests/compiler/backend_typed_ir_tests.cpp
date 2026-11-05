#include "../test_suites.hpp"

#include <algorithm>
#include <cstring>

#include "../../src/transpiler/backend_typed_ir.hpp"

using namespace cblc::backend;

static typed_function make_valid_function()
{
    typed_function function;
    function.source_unit_id = 17;
    function.function_id = 3;
    for (type_id id = 1; id <= 5; ++id)
        function.types.push_back({id, typed_type_kind::primitive,
            "primitive-" + std::to_string(id), {}});
    function.values.push_back({1, 4, value_category::scalar, access_mode::read_only,
        0, true, {{17, 0, 1, 1, 1, "literal", false}}, ""});
    function.values.push_back({2, 4, value_category::scalar, access_mode::read_only,
        0, true, {{17, 4, 1, 1, 5, "assignment-result", false}}, ""});
    function.places.push_back({1, 2, 4, access_mode::read_write, 0, {}});
    typed_region root_region;
    root_region.id = 1;
    root_region.kind = region_kind::function_body;
    root_region.operations = {1, 2};
    root_region.sequence = {{typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2}};
    function.regions.push_back(root_region);
    typed_operation literal;
    literal.id = 1;
    literal.operation = opcode::literal;
    literal.results.push_back(1);
    operation_outcome literal_completion;
    literal_completion.initialized.push_back(1);
    literal.outcomes.push_back(literal_completion);
    literal.provenance.push_back({17, 0, 1, 1, 1, "literal", false});
    literal.effects.exceptions = exception_effect::non_throwing;
    function.operations.push_back(literal);
    typed_operation store;
    store.id = 2;
    store.operation = opcode::store;
    store.operands.push_back(1);
    store.places.push_back(1);
    store.results.push_back(2);
    store.must_follow.push_back(1);
    operation_outcome store_completion;
    store_completion.initialized.push_back(2);
    store.outcomes.push_back(store_completion);
    store.provenance.push_back({17, 4, 1, 1, 5, "assignment", false});
    store.effects.writes.push_back(1);
    store.effects.exceptions = exception_effect::non_throwing;
    function.operations.push_back(store);
    return function;
}

static typed_function make_may_fail_construct_function()
{
    typed_function function;
    function.source_unit_id = 17;
    function.function_id = 9;
    for (type_id id = 1; id <= 5; ++id)
        function.types.push_back({id, typed_type_kind::primitive,
            "primitive-" + std::to_string(id), {}});
    function.values.push_back({1, 4, value_category::owned_object,
        access_mode::read_write, 1, true,
        {{17, 2, 8, 1, 3, "object-declaration", false}}, ""});
    function.places.push_back({1, 1, 4, access_mode::read_write, 1, {}});
    typed_region root;
    root.id = 1;
    root.kind = region_kind::function_body;
    root.operations = {1, 2};
    root.sequence = {{typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2}};
    function.regions.push_back(root);
    typed_operation construct;
    construct.id = 1;
    construct.operation = opcode::construct;
    construct.places.push_back(1);
    construct.results.push_back(1);
    construct.effects.constructs = true;
    construct.effects.may_fail = true;
    construct.effects.exceptions = exception_effect::may_throw_known;
    construct.effects.known_exception_type_ids.push_back(41);
    operation_outcome completed;
    completed.kind = outcome_kind::normal;
    completed.live.push_back(1);
    construct.outcomes.push_back(completed);
    operation_outcome allocation_failed;
    allocation_failed.kind = outcome_kind::allocation_failure;
    allocation_failed.uninitialized.push_back(1);
    construct.outcomes.push_back(allocation_failed);
    construct.provenance.push_back({17, 2, 8, 1, 3, "construction", false});
    function.operations.push_back(construct);
    typed_operation destroy;
    destroy.id = 2;
    destroy.operation = opcode::destroy;
    destroy.places.push_back(1);
    destroy.must_follow.push_back(1);
    destroy.effects.destroys = true;
    destroy.effects.exceptions = exception_effect::non_throwing;
    operation_outcome destroyed;
    destroyed.destroyed.push_back(1);
    destroy.outcomes.push_back(destroyed);
    destroy.provenance.push_back({17, 10, 4, 1, 11, "destruction", false});
    function.operations.push_back(destroy);
    return function;
}

static typed_function make_structured_function(region_kind control_kind)
{
    typed_function function = make_valid_function();
    typed_region root;
    typed_region control;
    typed_region body;

    function.regions.clear();
    root.id = 1;
    root.kind = region_kind::function_body;
    root.children.push_back(2);
    root.operations.push_back(1);
    root.sequence = {{typed_region::item::kind::operation, 1},
        {typed_region::item::kind::child_region, 2}};
    control.id = 2;
    control.parent = 1;
    control.kind = control_kind;
    control.condition = 1;
    control.children.push_back(3);
    if (control_kind == region_kind::conditional)
    {
        control.children.push_back(4);
        control.sequence = {{typed_region::item::kind::child_region, 3},
            {typed_region::item::kind::child_region, 4}};
    }
    else
        control.sequence = {{typed_region::item::kind::child_region, 3}};
    body.id = 3;
    body.parent = 2;
    body.kind = region_kind::lexical_scope;
    body.operations.push_back(2);
    body.sequence.push_back({typed_region::item::kind::operation, 2});
    function.operations[1].region = 3;
    function.regions.push_back(root);
    function.regions.push_back(control);
    function.regions.push_back(body);
    if (control_kind == region_kind::conditional)
    {
        typed_region else_body;
        else_body.id = 4;
        else_body.parent = 2;
        else_body.kind = region_kind::lexical_scope;
        function.regions.push_back(else_body);
    }
    return function;
}

FT_TEST(test_backend_typed_ir_verifies_ordered_well_typed_operations)
{
    const typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            1, "valid typed operations and evaluation order should verify") != FT_SUCCESS)
        return (FT_FAILURE);
    return (test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::none), "valid IR has no diagnostic"));
}

FT_TEST(test_backend_typed_ir_rejects_invalid_order_and_mutation)
{
    typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    function.operations[0].must_follow.push_back(2);
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "cyclic evaluation order should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::cyclic_evaluation_order),
            "cyclic order reports a stable verifier category") != FT_SUCCESS)
        return (FT_FAILURE);
    function = make_valid_function();
    function.places[0].access = access_mode::read_only;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "write through immutable place should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::immutable_write),
            "immutable write reports a stable verifier category") != FT_SUCCESS)
        return (FT_FAILURE);
    function = make_valid_function();
    function.places[0].symbol = 77;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "place binding to an undeclared source symbol must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::invalid_type_or_projection),
        "invalid symbol binding reports a typed storage diagnostic");
}

FT_TEST(test_backend_typed_ir_rejects_dangling_and_incomplete_calls)
{
    typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    function.operations[1].operands[0] = 99;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "dangling operand should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::dangling_reference),
            "dangling operand reports a stable verifier category") != FT_SUCCESS)
        return (FT_FAILURE);
    function = make_valid_function();
    function.operations[1].places[0] = 99;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "dangling assignment place must be rejected as data, not escape the verifier")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::dangling_reference),
            "dangling place reports a stable verifier category") != FT_SUCCESS)
        return (FT_FAILURE);
    function = make_valid_function();
    function.operations[1].operation = opcode::call;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "call without a resolved/dependent target should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    return (test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::incomplete_call_target),
        "incomplete call reports a stable verifier category"));
}

FT_TEST(test_backend_typed_ir_requires_well_formed_scalar_comparisons)
{
    typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    function.operations[0].operation = opcode::compare;
    function.operations[0].operands.push_back(1);
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "comparison with only one operand must be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::invalid_type_or_projection),
        "malformed comparison reports a typed operation diagnostic");
}

FT_TEST(test_backend_typed_ir_requires_complete_lifecycle_outcomes)
{
    typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    function = make_may_fail_construct_function();
    function.operations[0].effects.known_exception_type_ids.clear();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "known throwing operation without its exception set must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::missing_effect_or_outcome),
            "missing known exception set reports an effect diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[0].effects.known_exception_type_ids.push_back(41);
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "duplicate exception type IDs must be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::missing_effect_or_outcome),
            "duplicate exception type reports an effect diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[0].effects.exceptions = exception_effect::unknown;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            1, "unknown effects may retain known exception types as a partial set")
        != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[0].effects.constructs = false;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "construction opcode without a construction effect must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::missing_effect_or_outcome),
            "missing construction effect reports an effect diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[0].places.clear();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "construction without exactly one destination place must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_type_or_projection),
            "missing construction destination reports a lifecycle type diagnostic")
        != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[0].operation = opcode::copy_construct;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "copy construction without an explicit source value must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_type_or_projection),
            "missing copy source reports a lifecycle type diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[0].operation = opcode::move_construct;
    function.operations[0].operands.push_back(1);
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "move construction cannot consume the object it is constructing")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_type_or_projection),
            "self move construction reports a lifecycle type diagnostic")
        != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[1].effects.destroys = false;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "destroy opcode without a destruction effect must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::missing_effect_or_outcome),
            "missing destruction effect reports an effect diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    function.operations[1].operation = opcode::scope_exit;
    function.operations[1].effects.destroys = false;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "scope-exit cleanup without a destruction effect must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::missing_effect_or_outcome),
            "missing scope-exit effect reports an effect diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_valid_function();
    function.operations[0].outcomes[0].initialized.clear();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "result without a per-outcome completion state must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_outcome_state),
            "missing result state reports invalid outcome") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_may_fail_construct_function();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            1, "destroy outcome explicitly records object lifetime end") != FT_SUCCESS)
        return (FT_FAILURE);
    function.operations[1].operation = opcode::assign;
    return test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
        0, "non-destruction operation cannot claim to end object lifetime");
}

FT_TEST(test_backend_typed_ir_requires_calls_to_declare_owned_result_construction)
{
    typed_function function = make_may_fail_construct_function();
    verification_diagnostic diagnostic;
    typed_callable factory;

    function.types[3].kind = typed_type_kind::class_type;
    function.types[3].canonical_name = "OwnedResult";
    factory.id = 90;
    factory.kind = callable_kind::function;
    factory.canonical_name = "make_owned_result";
    factory.result_type = 4;
    factory.exceptions = exception_effect::may_throw_known;
    factory.has_definition = true;
    factory.known_exception_type_ids = {41};
    function.callables.push_back(factory);

    typed_operation &call = function.operations.front();
    call.operation = opcode::call;
    call.callee = factory.id;
    call.effects.constructs = false;

    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "a call cannot make an owned result live without a construction effect")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_outcome_state),
            "undeclared owned-result construction reports an outcome diagnostic")
        != FT_SUCCESS)
        return (FT_FAILURE);

    call.effects.constructs = true;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            1, "a class-returning call may construct its explicit owned result")
        != FT_SUCCESS)
    {
        std::printf("Owned-result call diagnostic: %s\n", diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_backend_typed_ir_accepts_pointer_based_move_constructor_dispatch)
{
    typed_function function = make_may_fail_construct_function();
    verification_diagnostic diagnostic;

    function.types[3].kind = typed_type_kind::class_type;
    function.types[3].canonical_name = "Tracked";
    function.types.push_back({6, typed_type_kind::pointer, "Tracked*", {}, 4});
    typed_value source;
    source.id = 2;
    source.type = 4;
    source.category = value_category::borrow;
    source.access = access_mode::read_write;
    source.lifetime_token = 2;
    source.provenance.push_back({17, 1, 1, 1, 2, "move-source", false});
    function.values.push_back(source);
    function.places.push_back({2, 2, 4, access_mode::read_write, 2, {}});

    typed_callable move_constructor;
    move_constructor.id = 71;
    move_constructor.kind = callable_kind::move_constructor;
    move_constructor.owner_type = 4;
    move_constructor.canonical_name = "Tracked::<move-constructor>";
    move_constructor.parameter_types = {6};
    move_constructor.exceptions = exception_effect::may_throw_known;
    move_constructor.known_exception_type_ids = {41};
    move_constructor.has_definition = true;
    function.callables.push_back(move_constructor);

    typed_operation &construction = function.operations[0];
    construction.operation = opcode::move_construct;
    construction.operands = {2};
    construction.callee = move_constructor.id;
    construction.effects.reads = {2};
    construction.outcomes[0].moved_from = {2};
    construction.outcomes[1].kind = outcome_kind::thrown_exception;
    construction.outcomes[1].uninitialized = {1};

    const bool valid_pointer_move = verify_typed_function(function, &diagnostic);
    if (!valid_pointer_move)
        std::printf("Pointer move verifier diagnostic: %s\n",
            diagnostic.message.c_str());
    if (test_expect_int_equal(valid_pointer_move ? 1 : 0,
            1, "typed move dispatch accepts the CBL-C pointer-based move signature")
        != FT_SUCCESS)
        return FT_FAILURE;

    function.callables[0].parameter_types = {5};
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "typed move dispatch rejects a pointer to a different class")
        != FT_SUCCESS)
        return FT_FAILURE;
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::incomplete_call_target),
        "incompatible move constructor pointer has a call-target diagnostic");
}

FT_TEST(test_backend_lifetime_rejects_live_object_at_function_exit)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;

    function.operations.pop_back();
    function.regions[0].operations.pop_back();
    function.regions[0].sequence.pop_back();
    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 0,
            "a successfully constructed object cannot escape function cleanup")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::live_object_at_exit),
        "missing cleanup reports a live object at exit");
}

FT_TEST(test_backend_lifetime_cleanup_plan_tracks_only_completed_objects)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph graph;
    lifecycle_cleanup_plan plan;
    verification_diagnostic diagnostic;
    const cleanup_site *normal_cleanup = nullptr;
    const cleanup_site *first_failure_cleanup = nullptr;
    const cleanup_site *second_failure_cleanup = nullptr;
    const cleanup_edge *first_failure_edge = nullptr;
    const cleanup_edge *second_failure_edge = nullptr;
    typed_operation second_construct;

    function.operations.pop_back();
    function.values.push_back({2, 4, value_category::owned_object,
        access_mode::read_write, 2, true,
        {{17, 30, 8, 1, 31, "second-object", false}}, ""});
    function.places.push_back({2, 2, 4, access_mode::read_write, 2, {}});
    second_construct = function.operations.front();
    second_construct.id = 2;
    second_construct.places = {2};
    second_construct.results = {2};
    second_construct.must_follow = {1};
    second_construct.outcomes[0].live = {2};
    second_construct.outcomes[1].uninitialized = {2};
    second_construct.provenance[0].source_offset = 30;
    function.operations.push_back(second_construct);
    function.regions[0].operations = {1, 2};
    function.regions[0].sequence = {
        {typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2}};

    if (test_expect_success(build_structured_control_flow_graph_with_cleanup(
                function, &graph, &plan, &diagnostic) ? FT_SUCCESS : FT_FAILURE,
            "structured CFG lowering should derive cleanup on each completed edge")
        != FT_SUCCESS)
    {
        std::printf("Cleanup-plan diagnostic %d: %s\n",
            static_cast<int>(diagnostic.code), diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    for (const cleanup_site &site : plan.exits)
    {
        if (site.exit_kind == cfg_terminator_kind::normal_exit)
            normal_cleanup = &site;
        if (site.exit_kind == cfg_terminator_kind::uncaught_exception_exit
            && site.destruction_order.empty())
            first_failure_cleanup = &site;
        if (site.exit_kind == cfg_terminator_kind::uncaught_exception_exit
            && site.destruction_order.size() == 1)
            second_failure_cleanup = &site;
    }
    for (const cleanup_edge &edge : plan.failure_edges)
    {
        if (edge.operation == 1
            && edge.outcome == outcome_kind::allocation_failure)
            first_failure_edge = &edge;
        if (edge.operation == 2
            && edge.outcome == outcome_kind::allocation_failure)
            second_failure_edge = &edge;
    }
    if (test_expect_success(normal_cleanup && first_failure_cleanup
                && second_failure_cleanup && first_failure_edge
                && second_failure_edge && plan.exits.size() == 3
                && plan.failure_edges.size() == 2
                ? FT_SUCCESS : FT_FAILURE,
            "cleanup plan records both exits and individual failure edges")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(normal_cleanup->destruction_order.size()),
            2, "normal exit cleans both fully constructed objects") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(normal_cleanup->destruction_order[0]),
            2, "cleanup starts with the most recently constructed object") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(normal_cleanup->destruction_order[1]),
            1, "cleanup ends with the earliest constructed object") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(first_failure_cleanup
                ->destruction_order.size()),
            0, "failure of the first construction destroys no incomplete object")
            != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(second_failure_cleanup
                ->destruction_order.size()),
            1, "failure of the second construction cleans the completed prefix")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(second_failure_cleanup
                ->destruction_order[0]),
            1, "failed construction cleans the completed prefix in reverse order")
        != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(first_failure_edge
                ->destruction_order.size()), 0,
            "first-construction failure edge requires no destruction")
            != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(second_failure_edge
                ->destruction_order.size()), 1,
            "second-construction failure edge carries cleanup for completed prefix")
            != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(second_failure_edge
            ->destruction_order[0]),
        1, "failure-edge cleanup is tied to its concrete operation and target");
}

FT_TEST(test_backend_lifetime_analysis_is_independent_of_cfg_storage_order)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph canonical_graph;
    control_flow_graph permuted_graph;
    lifecycle_cleanup_plan canonical_plan;
    lifecycle_cleanup_plan permuted_plan;
    verification_diagnostic diagnostic;

    if (!build_structured_control_flow_graph(function, &canonical_graph,
            &diagnostic)
        || !derive_lifecycle_cleanup_plan(function, canonical_graph,
            &canonical_plan, &diagnostic))
    {
        std::printf("Canonical lifetime analysis failed: %s\n",
            diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    permuted_graph = canonical_graph;
    std::reverse(permuted_graph.blocks.begin(), permuted_graph.blocks.end());
    for (basic_block &block : permuted_graph.blocks)
        std::reverse(block.terminator.successors.begin(),
            block.terminator.successors.end());
    if (!derive_lifecycle_cleanup_plan(function, permuted_graph,
            &permuted_plan, &diagnostic))
    {
        std::printf("Permuted lifetime analysis failed: %s\n",
            diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    if (canonical_plan.exits.size() != permuted_plan.exits.size()
        || canonical_plan.failure_edges.size()
            != permuted_plan.failure_edges.size())
        return (FT_FAILURE);
    for (std::size_t index = 0; index < canonical_plan.exits.size(); ++index)
    {
        const cleanup_site &left = canonical_plan.exits[index];
        const cleanup_site &right = permuted_plan.exits[index];

        if (left.exit_block != right.exit_block
            || left.exit_kind != right.exit_kind
            || left.destruction_order != right.destruction_order)
            return (FT_FAILURE);
    }
    for (std::size_t index = 0;
        index < canonical_plan.failure_edges.size(); ++index)
    {
        const cleanup_edge &left = canonical_plan.failure_edges[index];
        const cleanup_edge &right = permuted_plan.failure_edges[index];

        if (left.source_block != right.source_block
            || left.target_block != right.target_block
            || left.operation != right.operation
            || left.outcome != right.outcome
            || left.destruction_order != right.destruction_order)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_backend_lifetime_dataflow_enforces_exact_transfer_budget)
{
    const typed_function function = make_valid_function();
    control_flow_graph graph;
    lifecycle_cleanup_plan plan;
    verification_diagnostic diagnostic;
    lifetime_analysis_policy policy;
    std::size_t edge_count = 0;

    if (!build_linear_control_flow_graph(function, &graph, &diagnostic))
        return (FT_FAILURE);
    for (const basic_block &block : graph.blocks)
        edge_count += block.terminator.successors.size();
    if (edge_count < 2)
        return (FT_FAILURE);

    policy.max_edge_transfers = edge_count;
    if (!derive_lifecycle_cleanup_plan(function, graph, &plan, &diagnostic,
            policy))
        return (FT_FAILURE);
    policy.max_edge_transfers = edge_count + 1;
    if (!derive_lifecycle_cleanup_plan(function, graph, &plan, &diagnostic,
            policy))
        return (FT_FAILURE);

    plan.source_unit_id = 999;
    policy.max_edge_transfers = edge_count - 1;
    if (derive_lifecycle_cleanup_plan(function, graph, &plan, &diagnostic,
            policy))
        return (FT_FAILURE);
    if (diagnostic.code != verification_error::resource_limit_exceeded
        || diagnostic.message.find("phase=lifetime-dataflow") == std::string::npos
        || diagnostic.message.find("dimension=cfg_edge_transfers")
            == std::string::npos
        || diagnostic.message.find("configured_limit="
                + std::to_string(edge_count - 1)) == std::string::npos
        || diagnostic.message.find("observed=" + std::to_string(edge_count))
            == std::string::npos)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(plan.source_unit_id), 999,
        "resource exhaustion must not publish a partial cleanup plan");
}

FT_TEST(test_backend_lifetime_cleans_completed_prefix_for_checked_access_failure)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph graph;
    lifecycle_cleanup_plan plan;
    verification_diagnostic diagnostic;
    typed_operation second_construct;
    const cleanup_edge *checked_failure = nullptr;

    function.operations.pop_back();
    function.values.push_back({2, 4, value_category::owned_object,
        access_mode::read_write, 2, true,
        {{17, 30, 8, 1, 31, "second-object", false}}, ""});
    function.places.push_back({2, 2, 4, access_mode::read_write, 2, {}});
    second_construct = function.operations.front();
    second_construct.id = 2;
    second_construct.places = {2};
    second_construct.results = {2};
    second_construct.must_follow = {1};
    second_construct.outcomes[0].live = {2};
    second_construct.outcomes[1].kind = outcome_kind::checked_access_failure;
    second_construct.outcomes[1].uninitialized = {2};
    second_construct.provenance[0].source_offset = 30;
    function.operations.push_back(second_construct);
    function.regions[0].operations = {1, 2};
    function.regions[0].sequence = {
        {typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2}};

    if (test_expect_success(build_structured_control_flow_graph_with_cleanup(
                function, &graph, &plan, &diagnostic) ? FT_SUCCESS : FT_FAILURE,
            "checked-access failure should produce a cleanup plan") != FT_SUCCESS)
    {
        std::printf("Cleanup-plan diagnostic %d: %s\n",
            static_cast<int>(diagnostic.code), diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    for (const cleanup_edge &edge : plan.failure_edges)
        if (edge.operation == 2
            && edge.outcome == outcome_kind::checked_access_failure)
            checked_failure = &edge;
    if (test_expect_success(checked_failure ? FT_SUCCESS : FT_FAILURE,
            "checked-access failure edge is represented") != FT_SUCCESS)
    {
        for (const cleanup_edge &edge : plan.failure_edges)
            std::printf("cleanup edge op=%u outcome=%d destructions=%zu\n",
                edge.operation, static_cast<int>(edge.outcome),
                edge.destruction_order.size());
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(checked_failure
                ->destruction_order.size()), 1,
            "checked-access failure cleans the completed construction prefix")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(checked_failure
            ->destruction_order[0]), 1,
        "checked-access cleanup destroys the prior object");
}

FT_TEST(test_backend_lifetime_rejects_double_destruction)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    typed_operation second_destroy = function.operations.back();
    second_destroy.id = 3;
    second_destroy.must_follow.push_back(2);
    second_destroy.provenance[0].source_offset += 4;
    function.operations.push_back(second_destroy);
    function.regions[0].operations.push_back(3);
    function.regions[0].sequence.push_back(
        {typed_region::item::kind::operation, 3});

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 0,
            "an already destroyed object cannot be destroyed again")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::invalid_lifetime_transition),
        "double destruction reports an invalid lifecycle transition");
}

FT_TEST(test_backend_lifetime_requires_reverse_construction_order)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    typed_value second_object{2, 4, value_category::owned_object,
        access_mode::read_write, 2, true,
        {{17, 30, 8, 1, 31, "second-object", false}}, ""};
    typed_place second_place{2, 2, 4, access_mode::read_write, 2, {}};
    typed_operation second_construct = function.operations[0];
    typed_operation destroy_first = function.operations[1];
    typed_operation destroy_second = destroy_first;

    function.values.push_back(second_object);
    function.places.push_back(second_place);
    second_construct.id = 2;
    second_construct.places = {2};
    second_construct.results = {2};
    second_construct.must_follow = {1};
    second_construct.effects.may_fail = false;
    second_construct.effects.exceptions = exception_effect::non_throwing;
    second_construct.effects.known_exception_type_ids.clear();
    second_construct.outcomes.resize(1);
    second_construct.outcomes[0].kind = outcome_kind::normal;
    second_construct.outcomes[0].live = {2};
    second_construct.outcomes[0].uninitialized.clear();
    second_construct.provenance[0].source_offset += 20;
    destroy_first.id = 3;
    destroy_first.must_follow = {1, 2};
    destroy_first.provenance[0].source_offset += 40;
    destroy_second.id = 4;
    destroy_second.places = {2};
    destroy_second.must_follow = {1, 2, 3};
    destroy_second.outcomes[0].destroyed = {2};
    destroy_second.provenance[0].source_offset += 60;
    function.operations = {function.operations[0], second_construct,
        destroy_first, destroy_second};
    function.regions[0].operations = {1, 2, 3, 4};
    function.regions[0].sequence = {
        {typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2},
        {typed_region::item::kind::operation, 3},
        {typed_region::item::kind::operation, 4}};

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 0,
            "objects in one activation must be destroyed in reverse construction order")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::invalid_lifetime_transition),
        "out-of-order destruction reports an invalid lifecycle transition");
}

FT_TEST(test_backend_typed_ir_models_explicit_and_self_assignment)
{
    typed_function function = make_may_fail_construct_function();
    verification_diagnostic diagnostic;
    control_flow_graph cfg;
    typed_operation copy_assignment;
    typed_operation destruction = function.operations[1];

    function.types[3].kind = typed_type_kind::class_type;
    function.types[3].canonical_name = "Box";
    function.callables.push_back({77, callable_kind::copy_assignment, 4, 0,
        "Box::operator=", {4}, 0, exception_effect::non_throwing, true, {}});
    function.callables.push_back({78, callable_kind::move_assignment, 4, 0,
        "Box::operator=(Box&&)", {4}, 0, exception_effect::non_throwing, true, {}});

    copy_assignment.id = 2;
    copy_assignment.operation = opcode::copy_assign;
    copy_assignment.callee = 77;
    copy_assignment.operands = {1};
    copy_assignment.places = {1};
    copy_assignment.must_follow = {1};
    copy_assignment.effects.reads = {1};
    copy_assignment.effects.writes = {1};
    copy_assignment.effects.exceptions = exception_effect::non_throwing;
    copy_assignment.outcomes.push_back({});
    copy_assignment.provenance.push_back(
        {17, 20, 8, 1, 21, "self-copy-assignment", false});
    destruction.id = 3;
    destruction.must_follow = {1, 2};
    destruction.provenance[0].source_offset += 20;
    function.operations = {function.operations[0], copy_assignment, destruction};
    function.regions[0].operations = {1, 2, 3};
    function.regions[0].sequence = {
        {typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2},
        {typed_region::item::kind::operation, 3}};

    const bool valid_self_copy = build_linear_control_flow_graph(function, &cfg,
        &diagnostic);
    if (!valid_self_copy)
        std::printf("Self-assignment diagnostic: %s\n", diagnostic.message.c_str());
    if (test_expect_int_equal(valid_self_copy ? 1 : 0, 1,
            "self-copy assignment is an explicit valid operation") != FT_SUCCESS)
        return (FT_FAILURE);
    function.operations[1].callee = 78;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
                0, "copy assignment cannot reference an unregistered callable target")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::incomplete_call_target),
            "forged copy-assignment dispatch reports an incomplete target")
        != FT_SUCCESS)
        return (FT_FAILURE);
    function.operations[1].callee = 77;
    function.operations[1].operation = opcode::assign;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
                0, "generic assignment cannot stand in for owned-object copy/move dispatch")
        != FT_SUCCESS)
        return (FT_FAILURE);
    function.operations[1].operation = opcode::move_assign;
    function.operations[1].callee = 78;
    return test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
        "self-move assignment is accepted without spuriously moving-from its target");
}

FT_TEST(test_backend_typed_ir_allows_copy_from_live_borrow_only)
{
    typed_function function = make_may_fail_construct_function();
    verification_diagnostic diagnostic;
    control_flow_graph cfg;
    typed_operation copy_assignment;
    typed_operation destroy_target;
    typed_operation destroy_owner;

    function.types[3].kind = typed_type_kind::class_type;
    function.types[3].canonical_name = "Box";
    function.values[0].lifetime_token = 101;
    function.values.push_back({2, 4, value_category::owned_object,
        access_mode::read_write, 102, true,
        {{17, 20, 8, 1, 21, "assignment-target", false}}, ""});
    function.values.push_back({3, 4, value_category::borrow,
        access_mode::read_only, 101, true,
        {{17, 30, 8, 1, 31, "borrowed-member-source", false}}, ""});
    function.places.push_back({2, 2, 4, access_mode::read_write, 102, {}});
    function.entry_live_values = {2, 1};
    function.callables.push_back({77, callable_kind::copy_assignment, 4, 0,
        "Box::operator=", {4}, 0, exception_effect::non_throwing, true, {}});
    function.callables.push_back({78, callable_kind::move_assignment, 4, 0,
        "Box::operator=(Box&&)", {4}, 0, exception_effect::non_throwing, true, {}});

    copy_assignment.id = 1;
    copy_assignment.operation = opcode::copy_assign;
    copy_assignment.callee = 77;
    copy_assignment.operands = {3};
    copy_assignment.places = {2};
    copy_assignment.effects.reads = {2};
    copy_assignment.effects.writes = {2};
    copy_assignment.effects.exceptions = exception_effect::non_throwing;
    copy_assignment.outcomes.push_back({});
    copy_assignment.provenance.push_back(
        {17, 40, 8, 1, 41, "borrowed-copy-source", false});
    destroy_target.id = 3;
    destroy_target.operation = opcode::destroy;
    destroy_target.places = {2};
    destroy_target.must_follow = {1, 2};
    destroy_target.effects.destroys = true;
    destroy_target.effects.exceptions = exception_effect::non_throwing;
    operation_outcome target_destroyed;
    target_destroyed.destroyed = {2};
    destroy_target.outcomes.push_back(target_destroyed);
    destroy_target.provenance.push_back(
        {17, 50, 4, 1, 51, "target-destruction", false});
    destroy_owner = function.operations[1];
    destroy_owner.id = 2;
    destroy_owner.must_follow = {1};
    destroy_owner.provenance[0].source_offset += 60;
    function.operations = {copy_assignment, destroy_owner, destroy_target};
    function.regions[0].operations = {1, 2, 3};
    function.regions[0].sequence = {
        {typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2},
        {typed_region::item::kind::operation, 3}};

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "copy assignment may read a borrow whose owner is live") != FT_SUCCESS)
        return (FT_FAILURE);
    function.values[2].lifetime_token = 999;
    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 0,
            "copy assignment rejects a borrow with no live owner") != FT_SUCCESS)
        return (FT_FAILURE);
    function.values[2].lifetime_token = 101;
    {
        typed_operation destroy_owner_first = destroy_owner;
        typed_operation copy_after_owner = copy_assignment;
        typed_operation destroy_target_after_copy = destroy_target;

        destroy_owner_first.id = 1;
        destroy_owner_first.must_follow.clear();
        copy_after_owner.id = 2;
        copy_after_owner.must_follow = {1};
        destroy_target_after_copy.id = 3;
        destroy_target_after_copy.must_follow = {1, 2};
        function.operations = {destroy_owner_first, copy_after_owner,
            destroy_target_after_copy};
        if (test_expect_int_equal(build_linear_control_flow_graph(function,
                    &cfg, &diagnostic) ? 1 : 0, 0,
                "copy assignment rejects a borrow after its owner is destroyed")
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (test_expect_int_equal(static_cast<int>(diagnostic.code),
                static_cast<int>(verification_error::use_after_lifetime_end),
                "expired borrowed copy source reports a lifetime error")
            != FT_SUCCESS)
            return (FT_FAILURE);
        function.operations = {copy_assignment, destroy_owner, destroy_target};
    }
    function.operations[0].operation = opcode::move_assign;
    function.operations[0].callee = 78;
    return test_expect_int_equal(verify_typed_function(function, &diagnostic)
            ? 1 : 0, 0,
        "move assignment must not accept a borrowed source");
}

FT_TEST(test_backend_typed_ir_requires_moved_from_state_for_move_assignment)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    function.types[3].kind = typed_type_kind::class_type;
    function.types[3].canonical_name = "Box";
    function.callables.push_back({78, callable_kind::move_assignment, 4, 0,
        "Box::operator=(Box&&)", {4}, 0, exception_effect::non_throwing, true, {}});
    function.values.push_back({2, 4, value_category::owned_object,
        access_mode::read_write, 2, true,
        {{17, 30, 8, 1, 31, "move-source", false}}, ""});
    function.places.push_back({2, 2, 4, access_mode::read_write, 2, {}});

    typed_operation second_construct = function.operations[0];
    second_construct.id = 2;
    second_construct.places = {2};
    second_construct.results = {2};
    second_construct.must_follow = {1};
    second_construct.effects.may_fail = false;
    second_construct.effects.exceptions = exception_effect::non_throwing;
    second_construct.effects.known_exception_type_ids.clear();
    second_construct.outcomes.resize(1);
    second_construct.outcomes[0].kind = outcome_kind::normal;
    second_construct.outcomes[0].live = {2};
    second_construct.outcomes[0].uninitialized.clear();
    second_construct.provenance[0].source_offset += 20;

    typed_operation move_assignment;
    move_assignment.id = 3;
    move_assignment.operation = opcode::move_assign;
    move_assignment.callee = 78;
    move_assignment.operands = {2};
    move_assignment.places = {1};
    move_assignment.must_follow = {1, 2};
    move_assignment.effects.reads = {1};
    move_assignment.effects.writes = {1};
    move_assignment.effects.exceptions = exception_effect::non_throwing;
    operation_outcome moved_source;
    moved_source.moved_from = {2};
    move_assignment.outcomes.push_back(moved_source);
    move_assignment.provenance.push_back(
        {17, 50, 8, 1, 51, "move-assignment", false});

    typed_operation destroy_source = function.operations[1];
    destroy_source.id = 4;
    destroy_source.places = {2};
    destroy_source.must_follow = {1, 2, 3};
    destroy_source.outcomes[0].destroyed = {2};
    destroy_source.provenance[0].source_offset += 60;
    typed_operation destroy_target = function.operations[1];
    destroy_target.id = 5;
    destroy_target.must_follow = {1, 2, 3, 4};
    destroy_target.provenance[0].source_offset += 80;

    function.operations = {function.operations[0], second_construct,
        move_assignment, destroy_source, destroy_target};
    function.regions[0].operations = {1, 2, 3, 4, 5};
    function.regions[0].sequence = {
        {typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 2},
        {typed_region::item::kind::operation, 3},
        {typed_region::item::kind::operation, 4},
        {typed_region::item::kind::operation, 5}};

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "move assignment explicitly transitions the source to moved-from")
        != FT_SUCCESS)
        return (FT_FAILURE);
    function.operations[2].outcomes[0].moved_from.clear();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
                0, "move assignment without a moved-from completion state must fail")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::invalid_outcome_state),
        "missing moved-from state reports invalid outcome");
}

FT_TEST(test_backend_lifetime_rejects_branch_local_cleanup_leak)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    typed_region root;
    typed_region conditional;
    typed_region then_region;
    typed_region else_region;
    typed_operation condition_literal;

    function.values.push_back({2, 4, value_category::scalar, access_mode::read_only,
        0, true, {{17, 20, 1, 1, 21, "branch-condition", false}}, "1"});
    function.operations[1].region = 3;
    condition_literal.id = 3;
    condition_literal.operation = opcode::literal;
    condition_literal.results.push_back(2);
    condition_literal.provenance.push_back(
        {17, 20, 1, 1, 21, "branch-condition", false});
    condition_literal.effects.exceptions = exception_effect::non_throwing;
    operation_outcome condition_completed;
    condition_completed.initialized.push_back(2);
    condition_literal.outcomes.push_back(condition_completed);
    function.operations.push_back(condition_literal);

    root.id = 1;
    root.kind = region_kind::function_body;
    root.children.push_back(2);
    root.operations = {1, 3};
    root.sequence = {{typed_region::item::kind::operation, 1},
        {typed_region::item::kind::operation, 3},
        {typed_region::item::kind::child_region, 2}};
    conditional.id = 2;
    conditional.parent = 1;
    conditional.kind = region_kind::conditional;
    conditional.condition = 2;
    conditional.children = {3, 4};
    conditional.sequence = {{typed_region::item::kind::child_region, 3},
        {typed_region::item::kind::child_region, 4}};
    then_region.id = 3;
    then_region.parent = 2;
    then_region.kind = region_kind::lexical_scope;
    then_region.operations.push_back(2);
    then_region.sequence.push_back({typed_region::item::kind::operation, 2});
    else_region.id = 4;
    else_region.parent = 2;
    else_region.kind = region_kind::lexical_scope;
    function.regions = {root, conditional, then_region, else_region};

    if (test_expect_int_equal(build_structured_control_flow_graph(function,
                &cfg, &diagnostic) ? 1 : 0, 0,
            "cleanup that occurs on only one branch must not pass lifetime verification")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::live_object_at_exit),
        "branch leaving an outer object alive reports a live-object exit");
}

FT_TEST(test_backend_lifetime_accepts_path_dependent_state_before_scope_cleanup)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    typed_region root;
    typed_region conditional;
    typed_region then_region;
    typed_region else_region;
    typed_operation condition_literal;

    function.values.push_back({2, 4, value_category::scalar,
        access_mode::read_only, 0, true,
        {{17, 20, 1, 1, 21, "branch-condition", false}}, "1"});
    function.operations[0].region = 3;
    function.operations[1].region = 1;
    function.operations[1].operation = opcode::scope_exit;
    function.operations[1].must_follow.clear();
    function.operations[1].provenance[0].role = "conditional-scope-cleanup";
    function.operations[1].provenance[0].source_offset = 40;
    condition_literal.id = 3;
    condition_literal.operation = opcode::literal;
    condition_literal.region = 1;
    condition_literal.results.push_back(2);
    condition_literal.provenance.push_back(
        {17, 20, 1, 1, 21, "branch-condition", false});
    condition_literal.effects.exceptions = exception_effect::non_throwing;
    operation_outcome condition_completed;
    condition_completed.initialized.push_back(2);
    condition_literal.outcomes.push_back(condition_completed);
    function.operations.push_back(condition_literal);

    root.id = 1;
    root.kind = region_kind::function_body;
    root.children.push_back(2);
    root.operations = {3, 2};
    root.sequence = {{typed_region::item::kind::operation, 3},
        {typed_region::item::kind::child_region, 2},
        {typed_region::item::kind::operation, 2}};
    conditional.id = 2;
    conditional.parent = 1;
    conditional.kind = region_kind::conditional;
    conditional.condition = 2;
    conditional.children = {3, 4};
    conditional.sequence = {{typed_region::item::kind::child_region, 3},
        {typed_region::item::kind::child_region, 4}};
    then_region.id = 3;
    then_region.parent = 2;
    then_region.kind = region_kind::lexical_scope;
    then_region.operations.push_back(1);
    then_region.sequence.push_back({typed_region::item::kind::operation, 1});
    else_region.id = 4;
    else_region.parent = 2;
    else_region.kind = region_kind::lexical_scope;
    function.regions = {root, conditional, then_region, else_region};

    if (!build_structured_control_flow_graph_with_cleanup(function, &graph,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Path-dependent lifetime analysis failed: %s\n",
            diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    for (const cleanup_site &exit : cleanup_plan.exits)
        if (!exit.destruction_order.empty())
            return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_backend_typed_ir_lowers_scalar_translation_unit_slice)
{
    const char *source = "int main() {\n int value;\n int copied;\n"
        " value = 007;\n copied = value;\n return copied;\n}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    char *generated_cobol;
    char expected_copy[256];
    const t_cblc_data_item *value_item;
    const t_cblc_data_item *copied_item;
    int status;

    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    expected_copy[0] = '\0';
    value_item = NULL;
    copied_item = NULL;
    status = FT_FAILURE;
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        if (unit.parse_error_message[0] != '\0')
            std::printf("CBL-C diagnostic: %s\n", unit.parse_error_message);
        goto cleanup;
    }
    if (unit.function_count == 0
        || !lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Typed IR diagnostic: %s\n", diagnostic.message.c_str());
        goto cleanup;
    }
    {
        const auto value_symbol = std::find_if(function_ir.symbols.begin(),
            function_ir.symbols.end(), [](const typed_symbol &symbol)
            {
                return symbol.canonical_name == "value";
            });
        const auto value_place = std::find_if(function_ir.places.begin(),
            function_ir.places.end(), [&](const typed_place &place)
            {
                return value_symbol != function_ir.symbols.end()
                    && place.symbol == value_symbol->id;
            });
        if (value_symbol == function_ir.symbols.end()
            || value_place == function_ir.places.end()
            || value_symbol->type != value_place->type)
        {
            std::printf("Typed scalar storage binding missing: symbols=%zu places=%zu\n",
                function_ir.symbols.size(), function_ir.places.size());
            for (const typed_symbol &symbol : function_ir.symbols)
                std::printf("  symbol %u %s type=%u\n", symbol.id,
                    symbol.canonical_name.c_str(), symbol.type);
            goto cleanup;
        }
    }
    if (test_expect_int_equal(static_cast<int>(function_ir.operations.size()), 6,
            "scalar adapter emits literal, assignments, loads, and return operations")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(function_ir.operations[1].operation),
            static_cast<int>(opcode::assign), "scalar assignment is typed") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(function_ir.operations[3].provenance[0].line),
            5, "typed assignment retains its source line") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(function_ir.operations[3].provenance[0].source_offset),
            static_cast<int>(std::strstr(source, "copied = value;") - source),
            "typed assignment retains its source byte offset") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(function_ir.operations[3].provenance[0].source_length),
            static_cast<int>(std::strlen("copied = value;")),
            "typed assignment retains its exact source length") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(build_linear_control_flow_graph(function_ir, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "scalar IR should lower to a verified CFG") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(cfg.blocks.size()), 6,
            "scalar return CFG has one block per operation and no fallthrough block")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(cfg.blocks.back().terminator.kind),
            static_cast<int>(cfg_terminator_kind::return_value),
            "return operation becomes an explicit CFG terminator") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "verified scalar slice still reaches the current COBOL emitter") != FT_SUCCESS)
        goto cleanup;
    for (size_t index = 0; index < unit.data_count; ++index)
    {
        if (std::strcmp(unit.data_items[index].source_name, "value") == 0)
            value_item = &unit.data_items[index];
        else if (std::strcmp(unit.data_items[index].source_name, "copied") == 0)
            copied_item = &unit.data_items[index];
    }
    if (!value_item || !copied_item
        || std::snprintf(expected_copy, sizeof(expected_copy),
            "MOVE %s TO %s.", value_item->cobol_name,
            copied_item->cobol_name) < 0)
        goto cleanup;
    if (!generated_cobol || !std::strstr(generated_cobol, "MOVE 7 TO ")
        || std::strstr(generated_cobol, "MOVE 007 TO ")
        || !std::strstr(generated_cobol, expected_copy))
    {
        std::printf("Typed scalar emission should use canonical IR literal and source/destination bindings\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_lowers_resolved_instance_call_with_receiver_lifetime)
{
    const char *source =
        "class Counter { public: void touch() { return; } "
        "void adjust(int amount) { return; } "
        "int read() { return 5; } "
        "int return_amount(int amount) { return amount; } };\n"
        "void main() { Counter counter; int total; counter.touch(); "
        "counter.adjust(3); total = counter.read(); "
        "total = counter.return_amount(4); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    const typed_operation *zero_argument_call = nullptr;
    const typed_operation *scalar_argument_call = nullptr;
    const typed_operation *zero_argument_result = nullptr;
    const typed_operation *scalar_argument_result = nullptr;
    const typed_symbol *receiver_symbol = nullptr;
    const typed_value *receiver_value = nullptr;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0
        || !lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Instance-call typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_symbol &symbol : function_ir.symbols)
        if (symbol.canonical_name == "counter"
            || symbol.canonical_name == "main__counter")
            receiver_symbol = &symbol;
    if (receiver_symbol)
        for (const typed_place &place : function_ir.places)
            if (place.symbol == receiver_symbol->id)
                for (const typed_value &value : function_ir.values)
                    if (value.id == place.base_value)
                        receiver_value = &value;
    if (!receiver_value
        || receiver_value->category != value_category::scalar)
    {
        std::printf("A method-bearing class with only an implicit trivial constructor should not enter the non-trivial owned-lifetime set\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::call)
        {
            if (!operation.results.empty())
            {
                if (operation.operands.empty())
                    zero_argument_result = &operation;
                else
                    scalar_argument_result = &operation;
            }
            else if (operation.operands.empty())
                zero_argument_call = &operation;
            else
                scalar_argument_call = &operation;
        }
    if (!zero_argument_call || zero_argument_call->callee == 0
        || zero_argument_call->places.size() != 1
        || zero_argument_call->effects.reads != zero_argument_call->places
        || zero_argument_call->effects.writes != zero_argument_call->places
        || zero_argument_call->effects.may_fail
        || !scalar_argument_call || scalar_argument_call->callee == 0
        || scalar_argument_call->operands.size() != 1
        || scalar_argument_call->must_follow.size() != 1
        || scalar_argument_call->places.size() != 1
        || scalar_argument_call->effects.reads != scalar_argument_call->places
        || scalar_argument_call->effects.writes != scalar_argument_call->places
        || scalar_argument_call->effects.may_fail
        || !zero_argument_result || zero_argument_result->results.size() != 1
        || zero_argument_result->places.size() != 2
        || zero_argument_result->effects.writes.size() != 2
        || zero_argument_result->outcomes.empty()
        || zero_argument_result->outcomes[0].initialized.size() != 1
        || !scalar_argument_result
        || scalar_argument_result->operands.size() != 1
        || scalar_argument_result->results.size() != 1
        || scalar_argument_result->must_follow.size() != 1
        || scalar_argument_result->outcomes.empty()
        || scalar_argument_result->outcomes[0].initialized.size() != 1
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Instance-call typed CFG failed: %s\n",
            diagnostic.message.c_str());
        for (const typed_operation &operation : function_ir.operations)
        {
            std::printf("  op %u kind=%d places=%zu results=%zu outcomes=%zu\n",
                operation.id, static_cast<int>(operation.operation),
                operation.places.size(), operation.results.size(),
                operation.outcomes.size());
            for (const operation_outcome &outcome : operation.outcomes)
                std::printf("    outcome=%d live=%zu destroyed=%zu uninit=%zu\n",
                    static_cast<int>(outcome.kind), outcome.live.size(),
                    outcome.destroyed.size(), outcome.uninitialized.size());
        }
        for (const basic_block &block : graph.blocks)
            std::printf("  block %u ops=%zu term=%d edges=%zu\n", block.id,
                block.operations.size(), static_cast<int>(block.terminator.kind),
                block.terminator.successors.size());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (test_expect_int_equal(static_cast<int>(cleanup_plan.exits.size()), 1,
            "typed CFG records the single normal function exit after receiver calls")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_borrows_global_class_receiver_without_function_cleanup)
{
    const char *source =
        "class GlobalWorker { public: void touch() { return; } };\n"
        "GlobalWorker global_worker;\n"
        "void main() { global_worker.touch(); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    const typed_symbol *global_symbol = nullptr;
    const typed_value *global_value = nullptr;
    const typed_operation *call = nullptr;
    char *generated_cobol = nullptr;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count
        || !lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Global class receiver lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_symbol &symbol : function_ir.symbols)
        if (symbol.canonical_name == "global_worker")
            global_symbol = &symbol;
    for (const typed_place &place : function_ir.places)
        if (global_symbol && place.symbol == global_symbol->id)
            for (const typed_value &value : function_ir.values)
                if (value.id == place.base_value)
                    global_value = &value;
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::call)
            call = &operation;
    if (!global_symbol || global_symbol->category != value_category::borrow
        || !global_value || global_value->category != value_category::borrow
        || !call || call->places.size() != 1
        || call->effects.reads != call->places
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Global class storage was not modeled as an external-lifetime borrow: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (cleanup_plan.exits.size() != 1
        || !cleanup_plan.exits.front().destruction_order.empty())
    {
        std::printf("Function exit must not destroy program-scope class storage\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (!verify_global_object_lifetimes(&unit, &diagnostic)
        || cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("Trivial external-lifetime global class should remain lowerable: %s\n",
            diagnostic.message.c_str());
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_unwinds_partially_initialized_global_owned_objects)
{
    const char *source =
        "class ReadyGlobal { private: int value; public: "
        "ReadyGlobal() { value = 1; } "
        "~ReadyGlobal() { value = 0; } };\n"
        "class FailingGlobal { private: int value; public: "
        "FailingGlobal() { throw 1; } "
        "~FailingGlobal() { value = 0; } };\n"
        "ReadyGlobal ready_resource;\n"
        "FailingGlobal failing_resource;\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    verification_diagnostic diagnostic;
    char *generated = nullptr;
    const char *root;
    const char *root_init_call;
    const char *root_init_paragraph;
    const char *first_global_live_init;
    const char *first_drop;
    const char *second_drop;
    char expected_drop_line[TRANSPILE_IDENTIFIER_MAX + 16];
    const t_cblc_statement *expected_first_drop = nullptr;
    const t_cblc_statement *expected_second_drop = nullptr;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Global owner fixture parse failed: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (!verify_global_object_lifetimes(&unit, &diagnostic)
        || cblc_generate_cobol(&unit, &generated) != FT_SUCCESS
        || !generated)
    {
        std::printf("Global startup unwind fixture was not verified and emitted: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (unit.entry_function_index >= unit.function_count)
    {
        std::printf("Global startup unwind fixture lost its entry function\n");
        cma_free(generated);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const t_cblc_function &entry = unit.functions[unit.entry_function_index];
    for (size_t index = 0; index < entry.statement_count; ++index)
    {
        const t_cblc_statement &statement = entry.statements[index];

        if (statement.type != CBLC_STATEMENT_DESTRUCT
            || statement.lifecycle_cleanup_paragraph[0] == '\0')
            continue;
        if (!expected_first_drop)
            expected_first_drop = &statement;
        else if (std::strcmp(expected_first_drop->lifecycle_cleanup_paragraph,
                statement.lifecycle_cleanup_paragraph) != 0)
        {
            expected_second_drop = &statement;
            break;
        }
    }
    root = std::strstr(generated, "CBLC-ROOT-UNCAUGHT.");
    root_init_call = std::strstr(generated, "PERFORM CBLC-ROOT-INIT.");
    root_init_paragraph = std::strstr(generated, "       CBLC-ROOT-INIT.");
    first_global_live_init = std::strstr(generated,
        "MOVE 'N' TO CBLC-GLOBAL-LIVE-");
    first_drop = root ? std::strstr(root, "PERFORM CBLC-GLOBAL-DROP-") : nullptr;
    second_drop = first_drop
        ? std::strstr(first_drop + std::strlen("PERFORM CBLC-GLOBAL-DROP-"),
            "PERFORM CBLC-GLOBAL-DROP-") : nullptr;
    expected_drop_line[0] = '\0';
    if (expected_first_drop)
        std::snprintf(expected_drop_line, sizeof(expected_drop_line),
            "PERFORM %s", expected_first_drop->lifecycle_cleanup_paragraph);
    if (!expected_first_drop || !expected_second_drop
        || !std::strstr(generated, "CBLC-GLOBAL-LIVE-")
        || !std::strstr(generated, "GO TO CBLC-ROOT-UNCAUGHT")
        || !root_init_call || !root_init_paragraph
        || !first_global_live_init || root_init_call >= first_global_live_init
        || !std::strstr(root_init_paragraph,
            "MOVE 'N' TO CBLC-EX-ACTIVE")
        || !std::strstr(root_init_paragraph,
            "MOVE 'N' TO CBLC-EX-MATCHED")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-TYPE-ID")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-DYNAMIC-TYPE-ID")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-PAYLOAD-SIZE")
        || !std::strstr(root_init_paragraph,
            "MOVE 'N' TO CBLC-EX-PAYLOAD-OWNER")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-SOURCE-FILE-ID")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-SOURCE-LINE")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-SOURCE-COLUMN")
        || !std::strstr(root_init_paragraph,
            "MOVE 'N' TO CBLC-EX-RAISING")
        || !std::strstr(root_init_paragraph,
            "MOVE 'N' TO CBLC-EX-CLEANING")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-SECONDARY-TYPE-ID")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-SECONDARY-SOURCE-LINE")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-SECONDARY-SOURCE-COLUMN")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-INT-PAYLOAD")
        || !std::strstr(root_init_paragraph,
            "MOVE 0 TO CBLC-EX-MESSAGE-LEN")
        || !std::strstr(root_init_paragraph,
            "MOVE SPACES TO CBLC-EX-MESSAGE-BUF")
        || !root || !first_drop || !second_drop
        || std::strncmp(first_drop, expected_drop_line,
            std::strlen(expected_drop_line)) != 0)
    {
        std::printf("Global startup failure path omitted completion tracking, reverse-order cleanup, or payload release\n");
        cma_free(generated);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::snprintf(expected_drop_line, sizeof(expected_drop_line),
        "PERFORM %s", expected_second_drop->lifecycle_cleanup_paragraph);
    if (std::strncmp(second_drop, expected_drop_line,
            std::strlen(expected_drop_line)) != 0
        || !std::strstr(root, "PERFORM CBLC-EX-CLEAR-PAYLOAD"))
    {
        std::printf("Global startup failure path omitted completion tracking, reverse-order cleanup, or payload release\n");
        cma_free(generated);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_emits_nothrow_global_owned_lifecycle)
{
    const char *source =
        "class ManagedGlobal { private: int value; public: "
        "ManagedGlobal() { value = 1; } "
        "~ManagedGlobal() { value = 0; } };\n"
        "ManagedGlobal global_resource;\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    verification_diagnostic diagnostic;
    char *generated = nullptr;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Global owner fixture parse failed: code=%d message=%s\n",
            unit.parse_error_code, unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (!verify_global_object_lifetimes(&unit, &diagnostic)
        || cblc_generate_cobol(&unit, &generated) != FT_SUCCESS
        || !generated)
    {
        std::printf("Verified non-throwing global lifecycle was not emitted: %s\n",
            diagnostic.message.c_str());
        if (generated)
            cma_free(generated);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated);
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_records_int_to_double_method_argument_conversion)
{
    const char *source =
        "class Sink { public: void accept(double input) { return; } };\n"
        "void main() { Sink sink; sink.accept(7); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    const typed_operation *conversion = nullptr;
    const typed_operation *call = nullptr;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0
        || !lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Numeric-conversion typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::numeric_convert)
            conversion = &operation;
        else if (operation.operation == opcode::call)
            call = &operation;
    }
    const auto callable = call
        ? std::find_if(function_ir.callables.begin(), function_ir.callables.end(),
            [&](const typed_callable &candidate)
            {
                return candidate.id == call->callee;
            })
        : function_ir.callables.end();
    const auto int_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [](const typed_type &candidate)
        {
            return candidate.id == static_cast<type_id>(CBLC_DATA_KIND_INT) + 1U;
        });
    const auto double_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [](const typed_type &candidate)
        {
            return candidate.id == static_cast<type_id>(CBLC_DATA_KIND_DOUBLE) + 1U;
        });
    if (!conversion || conversion->operands.size() != 1
        || conversion->results.size() != 1 || !call
        || call->operands.size() != 1
        || call->operands.front() != conversion->results.front()
        || std::find(call->must_follow.begin(), call->must_follow.end(),
            conversion->id) == call->must_follow.end()
        || int_type == function_ir.types.end()
        || double_type == function_ir.types.end()
        || callable == function_ir.callables.end()
        || callable->parameter_types.size() != 1
        || callable->parameter_types.front() != double_type->id)
    {
        std::printf("Int-to-double argument conversion must be explicit and sequenced before the exact method call\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto conversion_source = std::find_if(function_ir.values.begin(),
        function_ir.values.end(), [&](const typed_value &candidate)
        {
            return candidate.id == conversion->operands.front();
        });
    const auto conversion_result = std::find_if(function_ir.values.begin(),
        function_ir.values.end(), [&](const typed_value &candidate)
        {
            return candidate.id == conversion->results.front();
        });
    const bool valid_types = conversion_source != function_ir.values.end()
        && conversion_result != function_ir.values.end()
        && conversion_source->type == int_type->id
        && conversion_result->type == double_type->id;
    if (!valid_types || !verify_typed_function(function_ir, &diagnostic))
    {
        std::printf("Numeric conversion failed typed verification: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    typed_function malformed = function_ir;
    auto malformed_conversion = std::find_if(malformed.operations.begin(),
        malformed.operations.end(), [](const typed_operation &operation)
        {
            return operation.operation == opcode::numeric_convert;
        });
    if (malformed_conversion == malformed.operations.end())
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    malformed_conversion->must_follow.clear();
    if (verify_typed_function(malformed, &diagnostic)
        || diagnostic.code != verification_error::invalid_type_or_projection)
    {
        std::printf("Typed numeric conversion without an explicit source-order dependency must be rejected\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_records_typed_direct_function_call)
{
    const char *source =
        "int combine(int first, int second) { return first; }\n"
        "void accept(double input) { return; }\n"
        "void main() { int result; result = combine(11, 12); "
        "accept(13); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    std::size_t main_index = 0;
    std::vector<const typed_operation *> calls;
    std::vector<const typed_operation *> literals;
    std::vector<const typed_operation *> conversions;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Direct-call conversion fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (main_index < unit.function_count
        && std::strcmp(unit.functions[main_index].source_name, "main") != 0)
        ++main_index;
    if (main_index == unit.function_count
        || !lower_scalar_function(&unit, main_index, &function_ir, &diagnostic))
    {
        std::printf("Direct-call typed lowering failed at function %zu/%zu: %s\n",
            main_index, unit.function_count, diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::call)
            calls.push_back(&operation);
        else if (operation.operation == opcode::numeric_convert)
            conversions.push_back(&operation);
        else if (operation.operation == opcode::literal)
            literals.push_back(&operation);
    }
    const auto combine = calls.size() == 2
        ? std::find_if(function_ir.callables.begin(), function_ir.callables.end(),
            [&](const typed_callable &candidate)
            {
                return candidate.id == calls[0]->callee;
            })
        : function_ir.callables.end();
    const auto accept = calls.size() == 2
        ? std::find_if(function_ir.callables.begin(), function_ir.callables.end(),
            [&](const typed_callable &candidate)
            {
                return candidate.id == calls[1]->callee;
            })
        : function_ir.callables.end();
    const type_id int_type = static_cast<type_id>(CBLC_DATA_KIND_INT) + 1U;
    const type_id double_type = static_cast<type_id>(CBLC_DATA_KIND_DOUBLE) + 1U;
    if (calls.size() != 2 || calls[0]->callee == 0 || calls[1]->callee == 0
        || calls[0]->operands.size() != 2 || calls[0]->results.size() != 1
        || calls[0]->places.size() != 1 || calls[0]->outcomes.size() != 1
        || calls[1]->operands.size() != 1 || !calls[1]->results.empty()
        || !calls[1]->places.empty() || literals.size() != 3
        || conversions.size() != 1 || conversions.front()->results.size() != 1
        || calls[1]->operands.front() != conversions.front()->results.front()
        || std::find(calls[1]->must_follow.begin(), calls[1]->must_follow.end(),
            conversions.front()->id) == calls[1]->must_follow.end()
        || std::find(literals[1]->must_follow.begin(),
            literals[1]->must_follow.end(), literals[0]->id)
            == literals[1]->must_follow.end()
        || combine == function_ir.callables.end()
        || combine->kind != callable_kind::function || combine->owner_type != 0
        || combine->canonical_name != "combine"
        || combine->parameter_types != std::vector<type_id>{int_type, int_type}
        || combine->result_type != int_type || !combine->has_definition
        || accept == function_ir.callables.end()
        || accept->kind != callable_kind::function || accept->owner_type != 0
        || accept->canonical_name != "accept"
        || accept->parameter_types != std::vector<type_id>{double_type}
        || accept->result_type != 0 || !accept->has_definition
        || !verify_typed_function(function_ir, &diagnostic))
    {
        std::printf("Direct function calls must carry ordered arguments, explicit conversion, and canonical signatures: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    if (!build_structured_control_flow_graph_with_cleanup(function_ir, &graph,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Direct-call CFG lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_propagates_direct_call_exception_summary)
{
    const char *source =
        "void first() { second(); return; }\n"
        "void second() { first(); throw 7; return; }\n"
        "void relay() { first(); return; }\n"
        "void main() {\n"
        "    try { relay(); }\n"
        "    catch (int error) { return; }\n"
        "    return;\n"
        "}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    std::size_t relay_index = 0;
    const typed_operation *call = nullptr;
    const typed_callable *target = nullptr;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Call-effect fixture failed to parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (relay_index < unit.function_count
        && std::strcmp(unit.functions[relay_index].source_name, "relay") != 0)
        ++relay_index;
    if (relay_index == unit.function_count
        || !lower_scalar_function(&unit, relay_index, &function_ir, &diagnostic))
    {
        std::printf("Relay call-effect lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::call)
            call = &operation;
    if (call)
        for (const typed_callable &callable : function_ir.callables)
            if (callable.id == call->callee)
                target = &callable;
    const bool propagated = call && target
        && call->effects.exceptions == exception_effect::may_throw_known
        && call->effects.known_exception_type_ids.size() == 1
        && target->known_exception_type_ids
            == call->effects.known_exception_type_ids;
    if (test_expect_int_equal(propagated ? 1 : 0, 1,
            "typed direct-call effect must preserve exceptions propagated through a recursive call component")
        != FT_SUCCESS)
    {
        std::printf("Relay call effect=%d, known exception IDs=%zu, target=%p\n",
            call ? static_cast<int>(call->effects.exceptions) : -1,
            call ? call->effects.known_exception_type_ids.size() : 0,
            static_cast<const void *>(target));
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_copies_and_destroys_direct_by_value_class_argument)
{
    const char *source =
        "class Box { private: int value; public: "
        "Box() { value = 0; } "
        "Box(Box source) { value = source.value; throw 1; } "
        "~Box() { value = 0; } };\n"
        "void consume(Box value) { return; }\n"
        "void main() { Box original; consume(original); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::size_t function_index = 0;
    const typed_operation *copy = nullptr;
    const typed_operation *call = nullptr;
    const typed_operation *temporary_destroy = nullptr;
    bool failed_copy_preserves_source = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Direct by-value fixture failed to parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name, "main") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Direct by-value typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::copy_construct)
            copy = &operation;
        else if (operation.operation == opcode::call)
            call = &operation;
    }
    if (!copy || !call || copy->results.size() != 1
        || copy->operands.size() != 1 || copy->callee == 0
        || call->operands.size() != 1
        || call->operands.front() != copy->results.front()
        || std::find(call->must_follow.begin(), call->must_follow.end(),
            copy->id) == call->must_follow.end()
        || !copy->effects.constructs || copy->effects.reads.empty()
        || copy->effects.writes.size() != 1
        || !copy->effects.may_fail
        || copy->effects.exceptions != exception_effect::unknown
        || copy->outcomes.size() != 2
        || copy->outcomes.back().kind != outcome_kind::thrown_exception
        || copy->outcomes.back().uninitialized.size() != 1
        || copy->outcomes.back().uninitialized.front() != copy->results.front())
    {
        std::printf("Direct by-value class call did not copy its argument before dispatch\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::scope_exit
            && !operation.outcomes.empty()
            && std::find(operation.outcomes.front().destroyed.begin(),
                operation.outcomes.front().destroyed.end(), copy->results.front())
                != operation.outcomes.front().destroyed.end())
            temporary_destroy = &operation;
    if (!temporary_destroy
        || !verify_typed_function(function_ir, &diagnostic)
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Direct by-value argument destruction/CFG verification failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const cleanup_site &exit : cleanup_plan.exits)
        if (exit.exit_kind == cfg_terminator_kind::uncaught_exception_exit
            && std::find(exit.destruction_order.begin(),
                exit.destruction_order.end(), copy->operands.front())
                != exit.destruction_order.end()
            && std::find(exit.destruction_order.begin(),
                exit.destruction_order.end(), copy->results.front())
                == exit.destruction_order.end())
            failed_copy_preserves_source = true;
    if (!failed_copy_preserves_source)
    {
        std::printf("Failed direct-call copy must retain the source and exclude the incomplete temporary from cleanup\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_borrows_class_pointer_for_direct_function_call)
{
    const char *source =
        "class Box { public: void touch() { return; } };\n"
        "void consume(Box *value) { return; }\n"
        "void route(Box *pointer) { consume(pointer); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    std::size_t function_index = 0;
    const typed_operation *pointer_load = nullptr;
    const typed_operation *call = nullptr;
    bool makes_no_owned_copy = true;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Direct pointer-call fixture failed to parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name, "route") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Direct pointer-call lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::copy_construct)
            makes_no_owned_copy = false;
        if (operation.operation == opcode::load
            && operation.results.size() == 1
            && std::any_of(function_ir.values.begin(), function_ir.values.end(),
                [&](const typed_value &value)
                {
                    return value.id == operation.results.front()
                        && value.category == value_category::pointer_or_handle;
                }))
            pointer_load = &operation;
        if (operation.operation == opcode::call)
            call = &operation;
    }
    if (!pointer_load || !call || call->operands.size() != 1
        || call->must_follow.size() != 1
        || call->must_follow.front() != pointer_load->id
        || !makes_no_owned_copy
        || !verify_typed_function(function_ir, &diagnostic))
    {
        std::printf("Direct class-pointer call must stage a typed borrow without copying its pointee: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto pointer_value = std::find_if(function_ir.values.begin(),
        function_ir.values.end(), [&](const typed_value &value)
        {
            return value.id == call->operands.front();
        });
    const auto pointer_type = pointer_value == function_ir.values.end()
        ? function_ir.types.end()
        : std::find_if(function_ir.types.begin(), function_ir.types.end(),
            [&](const typed_type &type)
            {
                return type.id == pointer_value->type;
            });
    const bool correct_pointer = pointer_value != function_ir.values.end()
        && pointer_value->category == value_category::pointer_or_handle
        && pointer_type != function_ir.types.end()
        && pointer_type->kind == typed_type_kind::pointer
        && pointer_type->canonical_name == "Box*";
    cblc_translation_unit_dispose(&unit);
    return test_expect_int_equal(correct_pointer ? 1 : 0, 1,
        "direct function pointer arguments preserve canonical pointee type and borrow category");
}

FT_TEST(test_backend_typed_ir_cleans_direct_by_value_argument_when_callee_throws)
{
    const char *source =
        "class Box { private: int value; public: "
        "Box() { value = 0; } "
        "Box(Box source) { value = source.value; } "
        "~Box() { value = 0; } };\n"
        "void consume(Box value) { throw 7; return; }\n"
        "void main() { Box original; consume(original); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::size_t function_index = 0;
    const typed_operation *copy = nullptr;
    const typed_operation *call = nullptr;
    const cleanup_site *uncaught_exit = nullptr;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Throwing direct-call fixture failed to parse: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name, "main") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Throwing direct-call lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::copy_construct)
            copy = &operation;
        else if (operation.operation == opcode::call)
            call = &operation;
    }
    if (!copy || !call || copy->results.size() != 1
        || call->effects.exceptions != exception_effect::may_throw_known
        || call->effects.known_exception_type_ids.size() != 1
        || call->operands.size() != 1
        || call->operands.front() != copy->results.front()
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Throwing direct-call typed effect/CFG failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const cleanup_site &exit : cleanup_plan.exits)
        if (exit.exit_kind == cfg_terminator_kind::uncaught_exception_exit)
            uncaught_exit = &exit;
    if (!uncaught_exit)
    {
        std::printf("Throwing direct call has no uncaught cleanup exit\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto temporary_position = std::find(
        uncaught_exit->destruction_order.begin(),
        uncaught_exit->destruction_order.end(), copy->results.front());
    const auto source_position = std::find(
        uncaught_exit->destruction_order.begin(),
        uncaught_exit->destruction_order.end(), copy->operands.front());
    const bool cleanup_is_exact = temporary_position
            != uncaught_exit->destruction_order.end()
        && source_position != uncaught_exit->destruction_order.end()
        && temporary_position < source_position
        && std::count(uncaught_exit->destruction_order.begin(),
            uncaught_exit->destruction_order.end(), copy->results.front()) == 1
        && std::count(uncaught_exit->destruction_order.begin(),
            uncaught_exit->destruction_order.end(), copy->operands.front()) == 1;
    cblc_translation_unit_dispose(&unit);
    return test_expect_int_equal(cleanup_is_exact ? 1 : 0, 1,
        "callee exception destroys the live by-value copy before its source exactly once");
}

FT_TEST(test_backend_typed_ir_lowers_class_pointer_method_argument_as_borrow)
{
    const char *source =
        "class Leaf { public: void touch() { return; } };\n"
        "class Owner { public: void attach(Leaf *child) { "
        "if (child != NULL) { child->touch(); } return; } };\n"
        "void route(Leaf *child, Owner *owner) { owner->attach(child); return; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    std::size_t function_index = 0;
    const typed_operation *pointer_argument_load = nullptr;
    const typed_operation *attach_call = nullptr;
    bool status = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Pointer-argument fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name, "route") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Pointer-argument typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::load
            && operation.results.size() == 1
            && std::any_of(function_ir.values.begin(), function_ir.values.end(),
                [&](const typed_value &value)
                {
                    return value.id == operation.results.front()
                        && value.category == value_category::pointer_or_handle;
                }))
            pointer_argument_load = &operation;
        if (operation.operation == opcode::call && !operation.operands.empty())
            attach_call = &operation;
    }
    if (!pointer_argument_load || !attach_call
        || attach_call->operands.size() != 1
        || attach_call->must_follow.size() != 1
        || attach_call->must_follow.front() != pointer_argument_load->id
        || attach_call->places.size() != 1
        || attach_call->callee == 0
        || !verify_typed_function(function_ir, &diagnostic))
    {
        std::printf("Pointer-argument IR verification failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto loaded_value = std::find_if(function_ir.values.begin(),
        function_ir.values.end(), [&](const typed_value &value)
        {
            return value.id == attach_call->operands.front();
        });
    const auto pointer_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [&](const typed_type &type)
        {
            return type.id == (loaded_value == function_ir.values.end()
                ? 0 : loaded_value->type);
        });
    const auto pointee_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Leaf";
        });
    status = loaded_value != function_ir.values.end()
        && loaded_value->category == value_category::pointer_or_handle
        && pointer_type != function_ir.types.end()
        && pointer_type->kind == typed_type_kind::pointer
        && pointer_type->canonical_name == "Leaf*"
        && pointee_type != function_ir.types.end()
        && pointer_type->pointee_type == pointee_type->id;
    cblc_translation_unit_dispose(&unit);
    return test_expect_int_equal(status ? 1 : 0, 1,
        "class pointer method arguments lower to canonical typed borrows with ordered loads");
}

FT_TEST(test_backend_typed_ir_copies_and_destroys_by_value_class_argument)
{
    const char *source =
        "class Inner { private: int value; public: "
        "Inner() { value = 0; } "
        "Inner(Inner source) { value = source.value; } "
        "~Inner() { value = 0; } };\n"
        "class Message { private: Inner inner; int payload; public: "
        "Message() { payload = 0; } "
        "Message(Message source) { payload = source.payload; throw 1; } "
        "~Message() { payload = 0; } };\n"
        "template <typename T> class Sink { public: "
        "void send(T message) { return; } };\n"
        "void relay(Sink<Message> *sink, Message message) { "
        "sink->send(message); return; }\n"
        "void main() { Message message; Sink<Message> sink; "
        "sink.send(message); return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::size_t function_index = 0;
    const typed_operation *copy = nullptr;
    const typed_operation *call = nullptr;
    const typed_operation *temporary_destroy = nullptr;
    bool resolved_inner_constructor = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("By-value class argument fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name, "main") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("By-value class argument typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::copy_construct)
            copy = &operation;
        else if (operation.operation == opcode::call
            && !operation.operands.empty())
            call = &operation;
        else if (operation.operation == opcode::construct
            && operation.callee != 0 && operation.results.size() == 1)
        {
            const auto constructed_value = std::find_if(function_ir.values.begin(),
                function_ir.values.end(), [&](const typed_value &value)
                {
                    return value.id == operation.results.front();
                });
            const auto constructed_type = constructed_value
                == function_ir.values.end() ? function_ir.types.end()
                : std::find_if(function_ir.types.begin(), function_ir.types.end(),
                    [&](const typed_type &type)
                    {
                        return type.id == constructed_value->type;
                    });
            if (constructed_type != function_ir.types.end()
                && constructed_type->canonical_name == "Inner")
                resolved_inner_constructor = true;
        }
    }
    if (!copy || !call || copy->results.size() != 1
        || copy->callee == 0 || copy->operands.size() != 1
        || copy->places.size() != 1 || !resolved_inner_constructor
        || !copy->effects.constructs || copy->effects.reads.empty()
        || copy->effects.writes.size() != 1
        || !copy->effects.may_fail
        || copy->effects.exceptions != exception_effect::unknown
        || copy->outcomes.size() != 2
        || copy->outcomes.front().live.size() != 1
        || copy->outcomes.back().kind != outcome_kind::thrown_exception
        || copy->outcomes.back().uninitialized.size() != 1
        || copy->outcomes.back().uninitialized.front() != copy->results.front()
        || !copy->effects.destroys
        || copy->outcomes.back().destroyed.size() != 1
        || call->operands.size() != 1
        || std::find(call->operands.begin(), call->operands.end(),
            copy->results.front()) == call->operands.end()
        || std::find(call->must_follow.begin(), call->must_follow.end(), copy->id)
            == call->must_follow.end())
    {
        std::printf("By-value class argument copy/call chain is incomplete: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::scope_exit
            && !operation.outcomes.empty()
            && std::find(operation.outcomes.front().destroyed.begin(),
                operation.outcomes.front().destroyed.end(), copy->results.front())
                != operation.outcomes.front().destroyed.end())
            temporary_destroy = &operation;
    if (!temporary_destroy
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("By-value class argument cleanup verification failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    bool found_failed_copy_exit = false;
    for (const cleanup_site &exit : cleanup_plan.exits)
        if (exit.exit_kind == cfg_terminator_kind::uncaught_exception_exit
            && std::find(exit.destruction_order.begin(),
                exit.destruction_order.end(), copy->operands.front())
                != exit.destruction_order.end()
            && std::find(exit.destruction_order.begin(),
                exit.destruction_order.end(), copy->results.front())
                == exit.destruction_order.end())
            found_failed_copy_exit = true;
    const value_id failed_subobject = copy->outcomes.back().destroyed.front();
    const auto failed_subobject_type = std::find_if(function_ir.values.begin(),
        function_ir.values.end(), [&](const typed_value &value)
        {
            return value.id == failed_subobject;
        });
    const auto inner_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Inner";
        });
    if (failed_subobject_type == function_ir.values.end()
        || inner_type == function_ir.types.end()
        || failed_subobject_type->type != inner_type->id
        || std::any_of(cleanup_plan.exits.begin(), cleanup_plan.exits.end(),
            [&](const cleanup_site &exit)
            {
                return exit.exit_kind
                        == cfg_terminator_kind::uncaught_exception_exit
                    && std::find(exit.destruction_order.begin(),
                        exit.destruction_order.end(), failed_subobject)
                        != exit.destruction_order.end();
            }))
    {
        std::printf("Throwing copy constructor did not destroy its completed class subobject exactly on the failure edge\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (!found_failed_copy_exit)
    {
        std::printf("Throwing copy constructor did not preserve the source and exclude the uninitialized temporary from cleanup\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }

    std::size_t relay_index = 0;
    while (relay_index < unit.function_count
        && std::strcmp(unit.functions[relay_index].source_name, "relay") != 0)
        ++relay_index;
    typed_function relay_ir;
    if (relay_index == unit.function_count)
    {
        std::printf("By-value parameter fixture has no relay function (functions=%zu)\n",
            unit.function_count);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const t_cblc_parameter *relay_message_parameter = nullptr;
    for (std::size_t parameter_index = 0;
        parameter_index < unit.functions[relay_index].parameter_count;
        ++parameter_index)
        if (unit.functions[relay_index].parameters[parameter_index].kind
                == TRANSPILE_FUNCTION_PARAMETER_STRUCT
            && std::strcmp(unit.functions[relay_index].parameters[parameter_index]
                    .type_name, "Message") == 0)
            relay_message_parameter =
                &unit.functions[relay_index].parameters[parameter_index];
    bool parameter_destroy_precedes_return = false;
    if (relay_message_parameter)
        for (std::size_t statement_index = 0;
            statement_index < unit.functions[relay_index].statement_count;
            ++statement_index)
            if (unit.functions[relay_index].statements[statement_index].type
                    == CBLC_STATEMENT_DESTRUCT
                && std::strcmp(unit.functions[relay_index].statements[
                        statement_index].target,
                    relay_message_parameter->cobol_name) == 0)
                for (std::size_t later_index = statement_index + 1;
                    later_index < unit.functions[relay_index].statement_count;
                    ++later_index)
                    if (unit.functions[relay_index].statements[later_index].type
                        == CBLC_STATEMENT_RETURN)
                        parameter_destroy_precedes_return = true;
    if (!parameter_destroy_precedes_return)
    {
        std::printf("Legacy COBOL lowering input omits destruction of the owned by-value Message parameter before return\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (!lower_scalar_function(&unit, relay_index, &relay_ir, &diagnostic))
    {
        std::printf("By-value parameter entry-lifetime lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (relay_ir.entry_live_values.size() != 2)
    {
        std::printf("By-value parameter entry-live set has %zu values; expected Inner and Message\n",
            relay_ir.entry_live_values.size());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto entry_inner = std::find_if(relay_ir.values.begin(),
        relay_ir.values.end(), [&](const typed_value &value)
        {
            return value.id == relay_ir.entry_live_values[0];
        });
    const auto entry_message = std::find_if(relay_ir.values.begin(),
        relay_ir.values.end(), [&](const typed_value &value)
        {
            return value.id == relay_ir.entry_live_values[1];
        });
    const auto named_type = [&](const char *name)
    {
        return std::find_if(relay_ir.types.begin(), relay_ir.types.end(),
            [&](const typed_type &type)
            {
                return type.kind == typed_type_kind::class_type
                    && type.canonical_name == name;
            });
    };
    const auto relay_inner_type = named_type("Inner");
    const auto relay_message_type = named_type("Message");
    bool relay_copies_live_parameter = false;
    for (const typed_operation &operation : relay_ir.operations)
        if (operation.operation == opcode::copy_construct
            && operation.operands.size() == 1
            && operation.operands.front() == relay_ir.entry_live_values[1])
            relay_copies_live_parameter = true;
    if (entry_inner == relay_ir.values.end()
        || entry_message == relay_ir.values.end()
        || relay_inner_type == relay_ir.types.end()
        || relay_message_type == relay_ir.types.end()
        || entry_inner->type != relay_inner_type->id
        || entry_message->type != relay_message_type->id
        || !relay_copies_live_parameter
        || !build_structured_control_flow_graph_with_cleanup(relay_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("By-value parameter lifetime/forwarding verification failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto cleanup_has_parameter_tree = [&](const cleanup_site &exit)
    {
        const auto message_position = std::find(exit.destruction_order.begin(),
            exit.destruction_order.end(), entry_message->id);
        return message_position != exit.destruction_order.end()
            && std::next(message_position) != exit.destruction_order.end()
            && *std::next(message_position) == entry_inner->id;
    };
    std::size_t message_cleanup_operation = relay_ir.operations.size();
    std::size_t inner_cleanup_operation = relay_ir.operations.size();
    std::size_t relay_return_operation = relay_ir.operations.size();
    for (std::size_t operation_index = 0;
        operation_index < relay_ir.operations.size(); ++operation_index)
    {
        const typed_operation &operation = relay_ir.operations[operation_index];
        if (operation.operation == opcode::scope_exit
            && !operation.outcomes.empty())
        {
            if (std::find(operation.outcomes.front().destroyed.begin(),
                    operation.outcomes.front().destroyed.end(), entry_message->id)
                != operation.outcomes.front().destroyed.end())
                message_cleanup_operation = operation_index;
            if (std::find(operation.outcomes.front().destroyed.begin(),
                    operation.outcomes.front().destroyed.end(), entry_inner->id)
                != operation.outcomes.front().destroyed.end())
                inner_cleanup_operation = operation_index;
        }
        if (operation.operation == opcode::return_value)
            relay_return_operation = operation_index;
    }
    const bool relay_has_normal_parameter_cleanup =
        message_cleanup_operation < inner_cleanup_operation
        && inner_cleanup_operation < relay_return_operation;
    const bool relay_has_exceptional_parameter_cleanup = std::any_of(
        cleanup_plan.exits.begin(), cleanup_plan.exits.end(),
        [&](const cleanup_site &exit)
        {
            return exit.exit_kind == cfg_terminator_kind::uncaught_exception_exit
                && cleanup_has_parameter_tree(exit);
        });
    if (!relay_has_normal_parameter_cleanup
        || !relay_has_exceptional_parameter_cleanup)
    {
        std::printf("By-value parameter cleanup is missing or not in reverse construction order (normal=%d, exceptional=%d)\n",
            relay_has_normal_parameter_cleanup ? 1 : 0,
            relay_has_exceptional_parameter_cleanup ? 1 : 0);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_routes_resolved_throwing_instance_call)
{
    const char *source =
        "class Counter { public: int fail() { throw 7; return 0; } };\n"
        "void main() {\n"
        "    Counter counter;\n"
        "    int result;\n"
        "    try { result = counter.fail(); }\n"
        "    catch (int error) { return; }\n"
        "    return;\n"
        "}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    const typed_operation *call = nullptr;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0
        || !lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Throwing instance-call lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::call)
            call = &operation;
    if (!call)
    {
        std::printf("Typed IR did not contain the resolved throwing call\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (call->effects.exceptions != exception_effect::may_throw_known
        || call->effects.known_exception_type_ids.size() != 1
        || call->results.size() != 1
        || call->outcomes.size() != 2
        || call->outcomes[0].initialized.size() != 1
        || call->outcomes[1].kind != outcome_kind::thrown_exception
        || call->outcomes[1].uninitialized.size() != 1)
    {
        std::printf("Throwing call effect mismatch: effect=%d known=%zu outcomes=%zu\n",
            static_cast<int>(call->effects.exceptions),
            call->effects.known_exception_type_ids.size(), call->outcomes.size());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto resolved_callable = std::find_if(function_ir.callables.begin(),
        function_ir.callables.end(), [&](const typed_callable &candidate)
        {
            return candidate.id == call->callee;
        });
    if (resolved_callable == function_ir.callables.end()
        || resolved_callable->known_exception_type_ids
            != call->effects.known_exception_type_ids)
    {
        std::printf("Resolved method descriptor did not preserve its concrete exception set\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    typed_function inconsistent_call_ir = function_ir;
    auto inconsistent_callable = std::find_if(
        inconsistent_call_ir.callables.begin(),
        inconsistent_call_ir.callables.end(),
        [&](const typed_callable &candidate)
        {
            return candidate.id == call->callee;
        });
    if (inconsistent_callable == inconsistent_call_ir.callables.end())
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    inconsistent_callable->known_exception_type_ids.clear();
    if (verify_typed_function(inconsistent_call_ir, &diagnostic))
    {
        std::printf("Typed IR verifier accepted a callable whose exception set disagreed with its call\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (!build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Throwing instance-call CFG failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto call_block = std::find_if(graph.blocks.begin(), graph.blocks.end(),
        [&](const basic_block &block)
        {
            return block.terminator.kind == cfg_terminator_kind::invoke
                && block.terminator.operation == call->id;
        });
    const cfg_edge *thrown_edge = nullptr;
    if (call_block != graph.blocks.end())
        for (const cfg_edge &edge : call_block->terminator.successors)
            if (edge.outcome == outcome_kind::thrown_exception)
            {
                thrown_edge = &edge;
                break;
            }
    const basic_block *handler_entry = nullptr;
    const typed_region *handler_region = nullptr;
    if (thrown_edge)
    {
        const auto target = std::find_if(graph.blocks.begin(), graph.blocks.end(),
            [&](const basic_block &block)
            {
                return block.id == thrown_edge->target;
            });
        if (target != graph.blocks.end())
        {
            handler_entry = &*target;
            const auto region = std::find_if(function_ir.regions.begin(),
                function_ir.regions.end(), [&](const typed_region &candidate)
                {
                    return candidate.id
                            == handler_entry->terminator.handler_entry_region
                        && candidate.kind == region_kind::exception_handler;
                });
            if (region != function_ir.regions.end())
                handler_region = &*region;
        }
    }
    const bool routed_to_matching_handler = thrown_edge && handler_entry
        && handler_region
        && handler_entry->terminator.kind == cfg_terminator_kind::handler_entry
        && handler_region->source_exception_region_id
            == call->source_exception_region_id
        && handler_region->exception_type_id
            == call->effects.known_exception_type_ids.front()
        && thrown_edge->exception_type_id
            == call->effects.known_exception_type_ids.front()
        && thrown_edge->exception_payload == 0;
    if (test_expect_int_equal(routed_to_matching_handler ? 1 : 0, 1,
            "typed method exception reaches its matching lexical catch handler")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_routes_resolved_throwing_direct_call)
{
    const char *source =
        "int fail() { throw 7; return 0; }\n"
        "void main() {\n"
        "    int result;\n"
        "    try { result = fail(); }\n"
        "    catch (int error) { return; }\n"
        "    return;\n"
        "}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    const typed_operation *call = nullptr;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count
        || !lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Throwing direct-call lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::call)
            call = &operation;
    if (!call || call->source_exception_region_id == 0
        || call->effects.known_exception_type_ids.size() != 1
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Throwing direct-call CFG failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto call_block = std::find_if(graph.blocks.begin(), graph.blocks.end(),
        [&](const basic_block &block)
        {
            return block.terminator.kind == cfg_terminator_kind::invoke
                && block.terminator.operation == call->id;
        });
    const cfg_edge *thrown_edge = nullptr;
    if (call_block != graph.blocks.end())
        for (const cfg_edge &edge : call_block->terminator.successors)
            if (edge.outcome == outcome_kind::thrown_exception)
            {
                thrown_edge = &edge;
                break;
            }
    const basic_block *handler_entry = nullptr;
    const typed_region *handler_region = nullptr;
    if (thrown_edge)
    {
        const auto target = std::find_if(graph.blocks.begin(), graph.blocks.end(),
            [&](const basic_block &block)
            {
                return block.id == thrown_edge->target;
            });
        if (target != graph.blocks.end())
        {
            handler_entry = &*target;
            const auto region = std::find_if(function_ir.regions.begin(),
                function_ir.regions.end(), [&](const typed_region &candidate)
                {
                    return candidate.id
                            == handler_entry->terminator.handler_entry_region
                        && candidate.kind == region_kind::exception_handler;
                });
            if (region != function_ir.regions.end())
                handler_region = &*region;
        }
    }
    const bool routed_to_matching_handler = thrown_edge && handler_entry
        && handler_region
        && handler_entry->terminator.kind == cfg_terminator_kind::handler_entry
        && handler_region->source_exception_region_id
            == call->source_exception_region_id
        && handler_region->exception_type_id
            == call->effects.known_exception_type_ids.front()
        && thrown_edge->exception_type_id
            == call->effects.known_exception_type_ids.front()
        && thrown_edge->exception_payload == 0;
    if (test_expect_int_equal(routed_to_matching_handler ? 1 : 0, 1,
            "typed direct-call exception reaches its matching lexical catch handler")
        != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_lowers_by_value_class_method_parameters)
{
    const char *source =
        "class Packet { private: int payload; public: "
        "Packet() { payload = 0; } ~Packet() { payload = 0; } };\n"
        "class Cell { public: int state; Cell() { state = 0; } };\n"
        "class Receiver { private: int state; Cell cell; public: "
        "int read() { return state; } "
        "int read_nested() { return cell.state; } "
        "void set_nested(int next) { cell.state = next; return; } "
        "void set(int next) { state = next; return; } "
        "void consume(Packet item) { return; } };\n"
        "void main() { Receiver receiver; return; }\n";
    t_cblc_translation_unit unit;
    typed_function method_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    const t_cblc_method *method = nullptr;
    std::size_t owner_index = 0;
    std::size_t method_index = 0;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Method lifecycle fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t read_owner = 0;
    std::size_t read_index = 0;
    bool found_read = false;
    while (read_owner < unit.struct_type_count && !found_read)
    {
        for (read_index = 0;
            read_index < unit.struct_types[read_owner].method_count;
            ++read_index)
            if (std::strcmp(unit.struct_types[read_owner].methods[read_index]
                    .source_name, "read") == 0)
            {
                found_read = true;
                break;
            }
        if (!found_read)
            ++read_owner;
    }
    typed_function read_ir;
    if (!found_read || !lower_scalar_method(&unit, read_owner, read_index,
            &read_ir, &diagnostic))
    {
        std::printf("Read method typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto read_receiver_type = std::find_if(read_ir.types.begin(),
        read_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Receiver";
        });
    const auto read_receiver_pointer_type = read_receiver_type
        == read_ir.types.end() ? read_ir.types.end()
        : std::find_if(read_ir.types.begin(), read_ir.types.end(),
            [&](const typed_type &type)
            {
                return type.kind == typed_type_kind::pointer
                    && type.pointee_type == read_receiver_type->id;
            });
    const auto read_receiver_value = read_receiver_pointer_type
        == read_ir.types.end() ? read_ir.values.end()
        : std::find_if(read_ir.values.begin(), read_ir.values.end(),
            [&](const typed_value &value)
            {
                return value.type == read_receiver_pointer_type->id
                    && value.category == value_category::pointer_or_handle;
            });
    const typed_place *state_place = nullptr;
    for (const typed_operation &operation : read_ir.operations)
        if (operation.operation == opcode::load && !operation.places.empty())
        {
            const auto place = std::find_if(read_ir.places.begin(),
                read_ir.places.end(), [&](const typed_place &candidate)
                {
                    return candidate.id == operation.places.front();
                });
            if (place != read_ir.places.end())
                state_place = &*place;
        }
    if (read_receiver_value == read_ir.values.end() || !state_place
        || state_place->base_value != read_receiver_value->id
        || state_place->projections.size() != 2
        || state_place->projections[0].projection_kind
            != place_projection::kind::dereference
        || state_place->projections[0].resolved_member != 0
        || state_place->projections[0].resulting_type != read_receiver_type->id
        || state_place->projections[1].projection_kind
            != place_projection::kind::field
        || state_place->projections[1].resolved_member != 1
        || state_place->owner_lifetime_token != read_receiver_value->id)
    {
        std::printf("Method field access must project through the borrowed receiver in typed IR\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t nested_read_index = 0;
    bool found_nested_read = false;
    for (nested_read_index = 0;
        nested_read_index < unit.struct_types[read_owner].method_count;
        ++nested_read_index)
        if (std::strcmp(unit.struct_types[read_owner].methods[nested_read_index]
                .source_name, "read_nested") == 0)
        {
            found_nested_read = true;
            break;
        }
    typed_function nested_read_ir;
    if (!found_nested_read || !lower_scalar_method(&unit, read_owner,
            nested_read_index, &nested_read_ir, &diagnostic))
    {
        std::printf("Nested receiver field lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto nested_state_place = std::find_if(nested_read_ir.places.begin(),
        nested_read_ir.places.end(), [](const typed_place &place)
        {
            return place.projections.size() == 3
                && place.projections[0].projection_kind
                    == place_projection::kind::dereference
                && place.projections[1].projection_kind
                    == place_projection::kind::field
                && place.projections[2].projection_kind
                    == place_projection::kind::field;
        });
    const auto nested_cell_type = std::find_if(nested_read_ir.types.begin(),
        nested_read_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Cell";
        });
    const auto nested_receiver_value = nested_state_place
        == nested_read_ir.places.end() ? nested_read_ir.values.end()
        : std::find_if(nested_read_ir.values.begin(), nested_read_ir.values.end(),
            [&](const typed_value &value)
            {
                return value.id == nested_state_place->base_value
                    && value.category == value_category::pointer_or_handle
                    && value.lifetime_token == nested_state_place->owner_lifetime_token;
            });
    if (nested_state_place == nested_read_ir.places.end()
        || nested_cell_type == nested_read_ir.types.end()
        || nested_receiver_value == nested_read_ir.values.end()
        || nested_state_place->projections[0].resolved_member != 0
        || nested_state_place->projections[0].resulting_type
            != read_receiver_type->id
        || nested_state_place->projections[1].resolved_member != 2
        || nested_state_place->projections[1].resulting_type != nested_cell_type->id
        || nested_state_place->projections[2].resolved_member != 1
        || nested_state_place->projections[2].resulting_type
            != static_cast<type_id>(CBLC_DATA_KIND_INT) + 1U)
    {
        std::printf("Nested method field access must retain the exact typed receiver/member/member path\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t nested_set_index = 0;
    bool found_nested_set = false;
    for (nested_set_index = 0;
        nested_set_index < unit.struct_types[read_owner].method_count;
        ++nested_set_index)
        if (std::strcmp(unit.struct_types[read_owner].methods[nested_set_index]
                .source_name, "set_nested") == 0)
        {
            found_nested_set = true;
            break;
        }
    typed_function nested_set_ir;
    if (!found_nested_set || !lower_scalar_method(&unit, read_owner,
            nested_set_index, &nested_set_ir, &diagnostic))
    {
        std::printf("Nested receiver field assignment lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const bool nested_receiver_write = std::any_of(nested_set_ir.operations.begin(),
        nested_set_ir.operations.end(), [&](const typed_operation &operation)
        {
            if ((operation.operation != opcode::store
                    && operation.operation != opcode::assign)
                || operation.places.empty())
                return false;
            const auto place = std::find_if(nested_set_ir.places.begin(),
                nested_set_ir.places.end(), [&](const typed_place &candidate)
                {
                    return candidate.id == operation.places.front();
                });
            return place != nested_set_ir.places.end()
                && place->projections.size() == 3
                && place->projections[0].projection_kind
                    == place_projection::kind::dereference
                && place->projections[1].projection_kind
                    == place_projection::kind::field
                && place->projections[2].projection_kind
                    == place_projection::kind::field
                && place->projections[1].resolved_member == 2
                && place->projections[2].resolved_member == 1
                && place->access == access_mode::read_write;
        });
    if (!nested_receiver_write)
    {
        std::printf("Nested method field assignments must retain the writable receiver/member/member path\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t set_index = 0;
    bool found_set = false;
    for (set_index = 0;
        set_index < unit.struct_types[read_owner].method_count; ++set_index)
        if (std::strcmp(unit.struct_types[read_owner].methods[set_index]
                .source_name, "set") == 0)
        {
            found_set = true;
            break;
        }
    typed_function set_ir;
    if (!found_set || !lower_scalar_method(&unit, read_owner, set_index,
            &set_ir, &diagnostic))
    {
        std::printf("Mutable method field lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    bool stores_through_receiver = false;
    for (const typed_operation &operation : set_ir.operations)
        if ((operation.operation == opcode::store
                || operation.operation == opcode::assign)
            && !operation.places.empty())
        {
            const auto place = std::find_if(set_ir.places.begin(),
                set_ir.places.end(), [&](const typed_place &candidate)
                {
                    return candidate.id == operation.places.front();
                });
            if (place != set_ir.places.end()
                && place->projections.size() == 2
                && place->projections[0].projection_kind
                    == place_projection::kind::dereference
                && place->projections[0].resolved_member == 0
                && place->projections[1].projection_kind
                    == place_projection::kind::field
                && place->projections[1].resolved_member == 1
                && place->access == access_mode::read_write)
                stores_through_receiver = true;
        }
    if (!stores_through_receiver)
    {
        std::printf("Mutable method field writes must retain the receiver projection and access mode\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (owner_index < unit.struct_type_count && !method)
    {
        method_index = 0;
        while (method_index < unit.struct_types[owner_index].method_count)
        {
            const t_cblc_method &candidate =
                unit.struct_types[owner_index].methods[method_index];
            if (std::strcmp(candidate.source_name, "consume") == 0)
            {
                method = &candidate;
                break;
            }
            ++method_index;
        }
        if (!method)
            ++owner_index;
    }
    if (!method || !lower_scalar_method(&unit, owner_index, method_index,
            &method_ir, &diagnostic))
    {
        std::printf("By-value method parameter typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto packet_type = std::find_if(method_ir.types.begin(),
        method_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Packet";
        });
    const auto packet_parameter = std::find_if(method_ir.values.begin(),
        method_ir.values.end(), [&](const typed_value &value)
        {
            return !method_ir.entry_live_values.empty()
                && value.id == method_ir.entry_live_values.front();
        });
    bool parameter_destroyed_before_return = false;
    std::size_t destroy_index = method_ir.operations.size();
    std::size_t return_index = method_ir.operations.size();
    for (std::size_t operation_index = 0;
        operation_index < method_ir.operations.size(); ++operation_index)
    {
        const typed_operation &operation = method_ir.operations[operation_index];
        if ((operation.operation == opcode::destroy
                || operation.operation == opcode::scope_exit)
            && !operation.outcomes.empty()
            && std::find(operation.outcomes.front().destroyed.begin(),
                operation.outcomes.front().destroyed.end(),
                method_ir.entry_live_values.empty() ? 0
                    : method_ir.entry_live_values.front())
                != operation.outcomes.front().destroyed.end()
            && !method_ir.entry_live_values.empty()
            )
            destroy_index = operation_index;
        if (operation.operation == opcode::return_value)
            return_index = operation_index;
    }
    parameter_destroyed_before_return = destroy_index < return_index;
    if (packet_type == method_ir.types.end()
        || method_ir.entry_live_values.size() != 1
        || packet_parameter == method_ir.values.end()
        || packet_parameter->type != packet_type->id
        || packet_parameter->category != value_category::owned_object
        || !parameter_destroyed_before_return
        || !build_structured_control_flow_graph_with_cleanup(method_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Typed method parameter ownership/cleanup was not proven: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto receiver_class_type = std::find_if(method_ir.types.begin(),
        method_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Receiver";
        });
    const auto receiver_pointer_type = receiver_class_type
        == method_ir.types.end() ? method_ir.types.end()
        : std::find_if(method_ir.types.begin(), method_ir.types.end(),
            [&](const typed_type &type)
            {
                return type.kind == typed_type_kind::pointer
                    && type.pointee_type == receiver_class_type->id;
            });
    const bool has_borrowed_receiver = receiver_pointer_type != method_ir.types.end()
        && std::any_of(method_ir.values.begin(), method_ir.values.end(),
            [&](const typed_value &value)
            {
                return value.type == receiver_pointer_type->id
                    && value.category == value_category::pointer_or_handle;
            });
    if (!has_borrowed_receiver)
    {
        std::printf("Instance method receiver must be typed as a borrow rather than an owned entry lifetime\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    char *generated_cobol = nullptr;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "nested receiver field methods pass typed CFG validation before COBOL emission")
        != FT_SUCCESS)
    {
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_lowers_nested_member_expression_from_function)
{
    const char *source =
        "class Cell { public: int state; };\n"
        "class Box { public: Cell cell; };\n"
        "class Sink { public: void accept(int value) { return; } };\n"
        "int read_member(Box *input) { return input->cell.state; }\n"
        "void write_member(Box *input, int next) { input->cell.state = next; return; }\n"
        "void condition_member(Box *input) { if (input->cell.state > 0) { return; } return; }\n"
        "void pass_member(Box *input, Sink *sink) { sink->accept(input->cell.state); return; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    verification_diagnostic diagnostic;
    typed_function function_ir;
    std::size_t function_index = 0;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Nested member function fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    bool found = false;
    for (; function_index < unit.function_count; ++function_index)
        if (std::strcmp(unit.functions[function_index].source_name,
                "read_member") == 0)
        {
            found = true;
            break;
        }
    if (!found || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Nested member function typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto box_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Box";
        });
    const auto cell_type = std::find_if(function_ir.types.begin(),
        function_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Cell";
        });
    const typed_place *member_place = nullptr;
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::load && !operation.places.empty())
            for (const typed_place &place : function_ir.places)
                if (place.id == operation.places.front())
                    member_place = &place;
    const auto input_symbol = std::find_if(function_ir.symbols.begin(),
        function_ir.symbols.end(), [](const typed_symbol &symbol)
        {
            return symbol.canonical_name == "read_member__input"
                && symbol.is_parameter;
        });
    const auto input_value = member_place == nullptr
        ? function_ir.values.end()
        : std::find_if(function_ir.values.begin(), function_ir.values.end(),
            [&](const typed_value &value)
            {
                return value.id == member_place->base_value
                    && value.category == value_category::pointer_or_handle;
            });
    const auto box_pointer = box_type == function_ir.types.end()
        ? function_ir.types.end()
        : std::find_if(function_ir.types.begin(), function_ir.types.end(),
            [&](const typed_type &type)
            {
                return type.kind == typed_type_kind::pointer
                    && type.pointee_type == box_type->id;
            });
    if (!member_place || box_type == function_ir.types.end()
        || cell_type == function_ir.types.end()
        || input_symbol == function_ir.symbols.end()
        || member_place->symbol != input_symbol->id
        || box_pointer == function_ir.types.end()
        || input_value == function_ir.values.end()
        || input_value->type != box_pointer->id
        || member_place->projections.size() != 3
        || member_place->projections[0].projection_kind
            != place_projection::kind::dereference
        || member_place->projections[0].resulting_type != box_type->id
        || member_place->projections[1].projection_kind
            != place_projection::kind::field
        || member_place->projections[1].resolved_member != 1
        || member_place->projections[1].resulting_type != cell_type->id
        || member_place->projections[2].projection_kind
            != place_projection::kind::field
        || member_place->projections[2].resolved_member != 1
        || member_place->projections[2].resulting_type
            != static_cast<type_id>(CBLC_DATA_KIND_INT) + 1U
        || member_place->owner_lifetime_token != input_value->id)
    {
        std::printf("Function member projection mismatch: symbol=%u expected=%u projections=%zu\n",
            member_place ? member_place->symbol : 0,
            input_symbol != function_ir.symbols.end() ? input_symbol->id : 0,
            member_place ? member_place->projections.size() : 0);
        for (const typed_symbol &symbol : function_ir.symbols)
            std::printf("  symbol %u %s parameter=%d type=%u\n", symbol.id,
                symbol.canonical_name.c_str(), symbol.is_parameter, symbol.type);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t write_index = 0;
    bool found_write = false;
    for (; write_index < unit.function_count; ++write_index)
        if (std::strcmp(unit.functions[write_index].source_name,
                "write_member") == 0)
        {
            found_write = true;
            break;
        }
    typed_function write_ir;
    if (!found_write || !lower_scalar_function(&unit, write_index, &write_ir,
            &diagnostic))
    {
        std::printf("Nested member assignment function lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const bool nested_member_write = std::any_of(write_ir.operations.begin(),
        write_ir.operations.end(), [&](const typed_operation &operation)
        {
            if (operation.operation != opcode::assign || operation.places.empty())
                return false;
            const auto place = std::find_if(write_ir.places.begin(),
                write_ir.places.end(), [&](const typed_place &candidate)
                {
                    return candidate.id == operation.places.front();
                });
            return place != write_ir.places.end()
                && place->access == access_mode::read_write
                && place->projections.size() == 3
                && place->projections[0].projection_kind
                    == place_projection::kind::dereference
                && place->projections[1].resolved_member == 1
                && place->projections[2].resolved_member == 1;
        });
    if (!nested_member_write)
    {
        std::printf("Function member assignment must preserve its writable nested projection path\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t condition_index = 0;
    bool found_condition = false;
    for (; condition_index < unit.function_count; ++condition_index)
        if (std::strcmp(unit.functions[condition_index].source_name,
                "condition_member") == 0)
        {
            found_condition = true;
            break;
        }
    typed_function condition_ir;
    if (!found_condition || !lower_scalar_function(&unit, condition_index,
            &condition_ir, &diagnostic)
        || std::none_of(condition_ir.operations.begin(), condition_ir.operations.end(),
            [](const typed_operation &operation)
            {
                return operation.operation == opcode::compare;
            }))
    {
        std::printf("Nested member expressions in conditions must lower to typed comparisons: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    std::size_t call_index = 0;
    bool found_call = false;
    for (; call_index < unit.function_count; ++call_index)
        if (std::strcmp(unit.functions[call_index].source_name,
                "pass_member") == 0)
        {
            found_call = true;
            break;
        }
    typed_function call_ir;
    if (!found_call || !lower_scalar_function(&unit, call_index, &call_ir,
            &diagnostic))
    {
        std::printf("Nested member expression method argument lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto member_argument_load = std::find_if(call_ir.operations.begin(),
        call_ir.operations.end(), [&](const typed_operation &operation)
        {
            if (operation.operation != opcode::load || operation.places.empty())
                return false;
            const auto place = std::find_if(call_ir.places.begin(),
                call_ir.places.end(), [&](const typed_place &candidate)
                {
                    return candidate.id == operation.places.front();
                });
            return place != call_ir.places.end()
                && place->projections.size() == 3
                && place->projections[0].projection_kind
                    == place_projection::kind::dereference
                && place->projections[1].resolved_member == 1
                && place->projections[2].resolved_member == 1;
        });
    const auto method_call = std::find_if(call_ir.operations.begin(),
        call_ir.operations.end(), [](const typed_operation &operation)
        {
            return operation.operation == opcode::call;
        });
    if (member_argument_load == call_ir.operations.end()
        || method_call == call_ir.operations.end()
        || method_call->operands.empty()
        || std::find(method_call->must_follow.begin(), method_call->must_follow.end(),
            member_argument_load->id) == method_call->must_follow.end())
    {
        std::printf("Resolved method call must depend on the nested member argument load\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const char *emission_source =
        "class Cell { public: int state; };\n"
        "class Box { public: Cell cell; };\n"
        "class Sink { public: void accept(int value) { return; } };\n"
        "void pass_member(Box *input, Sink *sink) { sink->accept(input->cell.state); return; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit emission_unit;
    char *generated_cobol = nullptr;
    int generation_diagnostic_code = 0;
    char generation_diagnostic[512] = {};
    bool emitted_member_argument = false;
    cblc_translation_unit_init(&emission_unit);
    if (cblc_parse_translation_unit(emission_source, &emission_unit) != FT_SUCCESS
        || cblc_generate_cobol_with_diagnostic(&emission_unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("Nested member source did not emit COBOL (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic);
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&emission_unit);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    emitted_member_argument = std::strstr(generated_cobol,
            "COMPUTE SINK-CBLC-USER-ACCEPT-VALUE") != NULL
        && std::strstr(generated_cobol,
            "CBLC-SPTR-PASS-MEMBER-INPUT-CELL-STATE") != NULL;
    if (!emitted_member_argument)
    {
        const char *argument_slot = std::strstr(generated_cobol,
            "SINK-CBLC-USER-ACCEPT-VALUE");
        const char *nested_field = std::strstr(generated_cobol, "CELL-STATE");

        std::printf("Generated ABI markers: argument-slot=%d nested-field=%d\n",
            argument_slot != NULL, nested_field != NULL);
        const char *compute = std::strstr(generated_cobol,
            "COMPUTE SINK-CBLC-USER-ACCEPT-VALUE");
        if (compute)
            std::printf("Generated argument assignment: %.360s\n", compute);
        for (const char *occurrence = argument_slot; occurrence;
            occurrence = std::strstr(occurrence + 1,
                "SINK-CBLC-USER-ACCEPT-VALUE"))
        {
            const char *line_start = occurrence;
            const char *line_end;

            while (line_start > generated_cobol && line_start[-1] != '\n')
                --line_start;
            line_end = std::strchr(occurrence, '\n');
            if (!line_end)
                line_end = occurrence + std::strlen(occurrence);

            std::printf("Generated method-call line: %.*s\n",
                static_cast<int>(line_end - line_start), line_start);
        }
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&emission_unit);
        std::printf("COBOL method-call ABI must stage the nested field value into its argument slot\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&emission_unit);
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_preserves_pointer_return_as_borrowed_receiver)
{
    const char *source =
        "class Leaf { public: void touch() { return; } };\n"
        "Leaf *shared_leaf;\n"
        "class Branch1 { public: Leaf *borrow() { return shared_leaf; } };\n"
        "Branch1 *shared_branch1;\n"
        "class Branch2 { public: Branch1 *borrow() { return shared_branch1; } };\n"
        "Branch2 *shared_branch2;\n"
        "class Branch3 { public: Branch2 *borrow() { return shared_branch2; } };\n"
        "void main() {\n"
        "    Branch3 root;\n"
        "    root.borrow()->borrow()->borrow()->touch();\n"
        "    return;\n"
        "}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph graph;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::vector<const typed_operation *> calls;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0
        || !lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Borrow-chain typed lowering failed (functions: %zu; parse: %s; typed IR: %s)\n",
            unit.function_count, unit.parse_error_message,
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::call)
            calls.push_back(&operation);
    if (calls.size() != 4 || calls[0]->results.size() != 1
        || calls[1]->results.size() != 1 || calls[2]->results.size() != 1
        || !calls[3]->results.empty()
        || calls[0]->effects.exceptions != exception_effect::non_throwing
        || function_ir.values.empty()
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &graph, &cleanup_plan, &diagnostic))
    {
        std::printf("Borrow-chain CFG failed: %s\n", diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (std::size_t call_index = 0; call_index < 3; ++call_index)
    {
        const value_id borrow_id = calls[call_index]->results.front();
        const auto borrowed_value = std::find_if(function_ir.values.begin(),
            function_ir.values.end(), [&](const typed_value &value)
            {
                return value.id == borrow_id;
            });
        if (borrowed_value == function_ir.values.end()
            || borrowed_value->category != value_category::borrow)
        {
            std::printf("Pointer-return call at depth %zu was not typed as a borrow\n",
                call_index + 1);
            cblc_translation_unit_dispose(&unit);
            return FT_FAILURE;
        }
        const typed_place *receiver_place = nullptr;
        for (const typed_place &place : function_ir.places)
            if (!calls[call_index + 1]->places.empty()
                && place.id == calls[call_index + 1]->places.front())
            {
                receiver_place = &place;
                break;
            }
        const auto pointer_type = std::find_if(function_ir.types.begin(),
            function_ir.types.end(), [&](const typed_type &type)
            {
                return type.id == borrowed_value->type;
            });
        if (!receiver_place || receiver_place->projections.size() != 1
            || receiver_place->projections.front().projection_kind
                != place_projection::kind::dereference
            || pointer_type == function_ir.types.end()
            || pointer_type->kind != typed_type_kind::pointer
            || pointer_type->pointee_type != receiver_place->type)
        {
            std::printf("Pointer-return call at depth %zu does not feed the next call through a pointee-typed borrow\n",
                call_index + 1);
            cblc_translation_unit_dispose(&unit);
            return FT_FAILURE;
        }
    }
    if (cleanup_plan.exits.size() != 1)
    {
        std::printf("Pointer return was not preserved as a typed borrow chain\n");
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_failed_lowering_discards_partial_output)
{
    const char *source = "int main() { int value; value = 7; return value * 2; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;

    cblc_translation_unit_init(&unit);
    function_ir.function_id = 99;
    function_ir.values.push_back(typed_value{});
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0)
    {
        std::printf("Unsupported typed-IR source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(lower_scalar_function(&unit, 0,
                &function_ir, &diagnostic) ? 1 : 0, 0,
            "unsupported multiplicative return expression must fail typed lowering") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(diagnostic.code),
                static_cast<int>(verification_error::unsupported_legacy_statement),
            "failed lowering reports the unsupported source operation") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(function_ir.function_id), 0,
            "failed pass does not publish partial function identity") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(function_ir.values.size()), 0,
            "failed pass discards values produced before its failure") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(function_ir.operations.size()), 0,
            "failed pass discards earlier lowered operations") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(function_ir.regions.size()), 0,
            "failed pass discards partial structured regions") != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

FT_TEST(test_backend_typed_ir_lowers_scalar_literal_return)
{
    const char *source = "int constant_value() { return 8; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    std::size_t function_index = 0;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Literal return source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name,
            "constant_value") != 0)
        ++function_index;
    if (function_index == unit.function_count
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Literal return typed-lowering diagnostic: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const typed_operation *literal = nullptr;
    const typed_operation *returned = nullptr;
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::literal)
            literal = &operation;
        else if (operation.operation == opcode::return_value)
            returned = &operation;
    }
    bool verified = false;
    if (literal && returned && returned->operands.size() == 1
        && returned->must_follow.size() == 1
        && returned->must_follow.front() == literal->id)
        verified = verify_typed_function(function_ir, &diagnostic);
    if (!verified
        || cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol
        || !ft_strnstr(generated_cobol, "MOVE 8 TO ",
            std::strlen(generated_cobol)))
    {
        std::printf("Literal return COBOL emission diagnostic: %s\n",
            diagnostic.message.c_str());
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return test_expect_success(verified ? FT_SUCCESS : FT_FAILURE,
        "scalar literal return is explicit in typed IR and sequenced before return");
}

FT_TEST(test_backend_typed_ir_emits_scalar_parameter_return_from_load)
{
    const char *source = "int echo_value(int input) { return input; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    const t_cblc_function *function = nullptr;
    const t_cblc_statement *return_statement = nullptr;
    const t_cblc_data_item *input_item = nullptr;
    const t_cblc_data_item *return_item = nullptr;
    const typed_operation *load = nullptr;
    const typed_operation *returned = nullptr;
    std::size_t function_index = 0;
    bool verified = false;
    std::string expected_move;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Parameter return source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name,
            "echo_value") != 0)
        ++function_index;
    if (function_index == unit.function_count)
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    function = &unit.functions[function_index];
    for (std::size_t statement_index = 0;
        statement_index < function->statement_count; ++statement_index)
        if (function->statements[statement_index].type
                == CBLC_STATEMENT_RETURN)
        {
            return_statement = &function->statements[statement_index];
            break;
        }
    for (std::size_t item_index = 0; item_index < unit.data_count; ++item_index)
    {
        const t_cblc_data_item *item = &unit.data_items[item_index];

        if (item->is_alias || item->is_shadowed
            || (item->is_function_local
                && std::strcmp(item->owner_function_name,
                    function->source_name) != 0))
            continue;
        if (return_statement && return_statement->target[0] != '\0'
            && std::strcmp(item->cobol_name,
                return_statement->target) == 0)
            return_item = item;
        if (std::strcmp(item->source_name, "echo_value__input") == 0
            && item->kind == CBLC_DATA_KIND_INT)
            input_item = item;
    }
    if (!return_statement
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Parameter return typed-lowering diagnostic: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::return_value)
            returned = &operation;
        if (operation.operation == opcode::load)
            load = &operation;
    }
    if (returned && returned->operands.size() == 1 && load
        && load->results.size() == 1
        && load->results.front() == returned->operands.front()
        && std::find(returned->must_follow.begin(), returned->must_follow.end(),
            load->id) != returned->must_follow.end())
        verified = verify_typed_function(function_ir, &diagnostic);
    if (input_item && return_item)
        expected_move = std::string("MOVE ") + input_item->cobol_name
            + " TO " + return_item->cobol_name;
    if (!verified || expected_move.empty()
        || cblc_generate_cobol(&unit, &generated_cobol) != FT_SUCCESS
        || !generated_cobol
        || !ft_strnstr(generated_cobol, expected_move.c_str(),
            std::strlen(generated_cobol)))
    {
        std::printf("Parameter return COBOL emission diagnostic: %s expected='%s' return-target='%s' input=%s output=%s\n",
            diagnostic.message.c_str(), expected_move.c_str(),
            return_statement ? return_statement->target : "<none>",
            input_item ? input_item->cobol_name : "<missing>",
            return_item ? return_item->cobol_name : "<missing>");
        if (returned)
            std::printf("  return op=%u operands=%zu must-follow=%zu load=%u\n",
                returned->id, returned->operands.size(),
                returned->must_follow.size(), load ? load->id : 0);
        if (generated_cobol)
            std::printf("  generated length=%zu\n", std::strlen(generated_cobol));
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return test_expect_success(verified ? FT_SUCCESS : FT_FAILURE,
        "scalar parameter return is emitted from its typed load before function exit");
}

FT_TEST(test_backend_typed_ir_emits_scalar_member_return_before_parameter_cleanup)
{
    const char *source = "class Cell { public: int state; };\n"
        "class Box { public: Cell cell; ~Box() { cell.state = 0; } };\n"
        "int read_member(Box input) { return input.cell.state; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    const t_cblc_function *function = nullptr;
    const t_cblc_statement *return_statement = nullptr;
    const t_cblc_data_item *member_item = nullptr;
    const t_cblc_data_item *return_item = nullptr;
    const typed_operation *member_load = nullptr;
    const typed_operation *store = nullptr;
    const typed_operation *return_load = nullptr;
    const typed_operation *returned = nullptr;
    const typed_operation *cleanup = nullptr;
    std::size_t function_index = 0;
    std::size_t member_load_index = static_cast<std::size_t>(-1);
    std::size_t store_index = static_cast<std::size_t>(-1);
    std::size_t return_load_index = static_cast<std::size_t>(-1);
    std::size_t return_index = static_cast<std::size_t>(-1);
    std::size_t last_cleanup_index = 0;
    bool verified = false;
    std::string expected_move;
    int generation_diagnostic_code = 0;
    char generation_diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX] = {};

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Member return source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name,
            "read_member") != 0)
        ++function_index;
    if (function_index == unit.function_count)
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    function = &unit.functions[function_index];
    for (std::size_t statement_index = 0;
        statement_index < function->statement_count; ++statement_index)
        if (function->statements[statement_index].type
                == CBLC_STATEMENT_RETURN)
        {
            return_statement = &function->statements[statement_index];
            break;
        }
    for (std::size_t item_index = 0; item_index < unit.data_count; ++item_index)
    {
        const t_cblc_data_item *item = &unit.data_items[item_index];

        if (item->is_alias || item->is_shadowed
            || (item->is_function_local
                && std::strcmp(item->owner_function_name,
                    function->source_name) != 0))
            continue;
        if (return_statement && return_statement->target[0] != '\0'
            && std::strcmp(item->cobol_name,
                return_statement->target) == 0)
            return_item = item;
        if (std::strcmp(item->source_name,
                "read_member__input.cell.state") == 0
            && item->kind == CBLC_DATA_KIND_INT)
            member_item = item;
    }
    if (!return_statement
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Member return typed-lowering diagnostic: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (std::size_t operation_index = 0;
        operation_index < function_ir.operations.size(); ++operation_index)
    {
        const typed_operation &operation = function_ir.operations[operation_index];

        if (operation.operation == opcode::return_value)
        {
            returned = &operation;
            return_index = operation_index;
        }
        if (operation.operation == opcode::store
            || operation.operation == opcode::assign)
        {
            store = &operation;
            store_index = operation_index;
        }
        if (operation.operation == opcode::load
            && !operation.results.empty() && !operation.places.empty())
        {
            const auto place = std::find_if(function_ir.places.begin(),
                function_ir.places.end(), [&](const typed_place &candidate)
                {
                    return candidate.id == operation.places.front();
                });
            const auto symbol = place == function_ir.places.end()
                ? function_ir.symbols.end()
                : std::find_if(function_ir.symbols.begin(),
                    function_ir.symbols.end(), [&](const typed_symbol &candidate)
                    {
                        return candidate.id == place->symbol;
                    });

            if (symbol != function_ir.symbols.end()
                && member_item
                && symbol->canonical_name == member_item->source_name)
            {
                member_load = &operation;
                member_load_index = operation_index;
            }
        }
        if (operation.operation == opcode::scope_exit
            && operation.effects.destroys)
        {
            cleanup = &operation;
            last_cleanup_index = operation_index;
        }
    }
    if (returned && !returned->operands.empty())
        for (std::size_t operation_index = 0;
            operation_index < function_ir.operations.size(); ++operation_index)
        {
            const typed_operation &operation = function_ir.operations[operation_index];

            if (operation.operation == opcode::load
                && operation.results.size() == 1
                && operation.results.front() == returned->operands.front())
            {
                return_load = &operation;
                return_load_index = operation_index;
                break;
            }
        }
    if (member_load && store && return_load && returned && cleanup
        && member_load->results.size() == 1
        && store->operands.size() == 1
        && store->operands.front() == member_load->results.front()
        && returned->operands.size() == 1
        && return_load->results.size() == 1
        && return_load->results.front() == returned->operands.front()
        && member_load_index < store_index
        && store_index < last_cleanup_index
        && last_cleanup_index < return_load_index
        && return_load_index < return_index
        && std::find(returned->must_follow.begin(), returned->must_follow.end(),
            return_load->id) != returned->must_follow.end())
        verified = verify_typed_function(function_ir, &diagnostic);
    if (member_item && return_item)
        expected_move = std::string("MOVE ") + member_item->cobol_name
            + " TO " + return_item->cobol_name;
    if (!verified || expected_move.empty()
        || cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic,
            sizeof(generation_diagnostic)) != FT_SUCCESS
        || !generated_cobol
        || !ft_strnstr(generated_cobol, expected_move.c_str(),
            std::strlen(generated_cobol)))
    {
        std::printf("Member return COBOL emission diagnostic: %s generated(%d): %s expected='%s'\n",
            diagnostic.message.c_str(), generation_diagnostic_code,
            generation_diagnostic, expected_move.c_str());
        if (generated_cobol)
            cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return test_expect_success(verified ? FT_SUCCESS : FT_FAILURE,
        "scalar member return loads before owned parameter cleanup and emits from the resolved field");
}

FT_TEST(test_backend_typed_ir_emits_compound_scalar_return_before_parameter_cleanup)
{
    const char *source = "class Cell { public: int state; };\n"
        "class Box { public: Cell cell; ~Box() { cell.state = 0; } };\n"
        "int read_member_plus_one(Box input) { return input.cell.state + 1; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    const t_cblc_function *function = nullptr;
    const t_cblc_statement *return_statement = nullptr;
    const t_cblc_data_item *member_item = nullptr;
    const t_cblc_data_item *return_item = nullptr;
    const typed_operation *addition = nullptr;
    const typed_operation *assignment = nullptr;
    std::size_t function_index = 0;
    std::size_t add_index = static_cast<std::size_t>(-1);
    std::size_t assign_index = static_cast<std::size_t>(-1);
    std::size_t cleanup_index = static_cast<std::size_t>(-1);
    std::string expected_compute;
    bool verified = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Compound member return source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    while (function_index < unit.function_count
        && std::strcmp(unit.functions[function_index].source_name,
            "read_member_plus_one") != 0)
        ++function_index;
    if (function_index == unit.function_count)
    {
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    function = &unit.functions[function_index];
    for (std::size_t statement_index = 0;
        statement_index < function->statement_count; ++statement_index)
        if (function->statements[statement_index].type
                == CBLC_STATEMENT_RETURN)
        {
            return_statement = &function->statements[statement_index];
            break;
        }
    for (std::size_t item_index = 0; item_index < unit.data_count; ++item_index)
    {
        const t_cblc_data_item *item = &unit.data_items[item_index];

        if (item->is_alias || item->is_shadowed
            || (item->is_function_local
                && std::strcmp(item->owner_function_name,
                    function->source_name) != 0))
            continue;
        if (return_statement && return_statement->target[0] != '\0'
            && std::strcmp(item->cobol_name,
                return_statement->target) == 0)
            return_item = item;
        if (std::strcmp(item->source_name,
                "read_member_plus_one__input.cell.state") == 0
            && item->kind == CBLC_DATA_KIND_INT)
            member_item = item;
    }
    if (!return_statement
        || !lower_scalar_function(&unit, function_index, &function_ir,
            &diagnostic))
    {
        std::printf("Compound member return typed-lowering diagnostic: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (std::size_t operation_index = 0;
        operation_index < function_ir.operations.size(); ++operation_index)
    {
        const typed_operation &operation = function_ir.operations[operation_index];

        if (operation.operation == opcode::add)
        {
            addition = &operation;
            add_index = operation_index;
        }
        if (operation.operation == opcode::assign
            && !operation.operands.empty())
        {
            const typed_value *value = nullptr;
            for (const typed_value &candidate : function_ir.values)
                if (candidate.id == operation.operands.front())
                {
                    value = &candidate;
                    break;
                }
            if (value && addition
                && value->id == addition->results.front())
            {
                assignment = &operation;
                assign_index = operation_index;
            }
        }
        if (operation.operation == opcode::scope_exit
            && operation.effects.destroys)
            cleanup_index = operation_index;
    }
    if (member_item && return_item)
        expected_compute = std::string("COMPUTE ") + return_item->cobol_name
            + " = (" + member_item->cobol_name + " + 1).";
    if (addition && assignment && add_index < assign_index
        && assign_index < cleanup_index
        && addition->results.size() == 1
        && assignment->operands.size() == 1
        && assignment->operands.front() == addition->results.front()
        && verify_typed_function(function_ir, &diagnostic)
        && !expected_compute.empty()
        && cblc_generate_cobol(&unit, &generated_cobol) == FT_SUCCESS
        && generated_cobol
        && ft_strnstr(generated_cobol, expected_compute.c_str(),
            std::strlen(generated_cobol)))
        verified = true;
    else
        std::printf("Compound member return diagnostic: %s expected='%s'\n",
            diagnostic.message.c_str(), expected_compute.c_str());
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return test_expect_success(verified ? FT_SUCCESS : FT_FAILURE,
        "compound scalar return is computed into its return slot before owned parameter cleanup");
}

FT_TEST(test_backend_typed_ir_lowers_pointer_move_and_relocation_intrinsics)
{
    const char *source =
        "class Tracker { public: Tracker(Tracker *source) {} "
        "Tracker(Tracker source) {} ~Tracker() {} };\n"
        "void move_to(Tracker *destination, Tracker *source) { "
        "cblc_move_construct_at(destination, source); "
        "cblc_destroy_at(destination); return; }\n"
        "void relocate_to(Tracker *destination, Tracker *source) { "
        "cblc_relocate_at(destination, source); "
        "cblc_destroy_at(destination); return; }\n"
        "void indexed_relocate(Tracker *destination, int destination_slot, "
        "Tracker *source, int source_slot) { "
        "cblc_relocate_at(destination[destination_slot], "
        "source[source_slot]); "
        "cblc_destroy_at(destination[destination_slot]); return; }\n"
        "class Fallback { public: Fallback(Fallback *source) { throw 1; } "
        "Fallback(Fallback source) {} ~Fallback() {} };\n"
        "void relocate_copy(Fallback *destination, Fallback *source) { "
        "cblc_relocate_at(destination, source); "
        "cblc_destroy_at(destination); return; }\n"
        "void move_may_throw(Fallback *destination, Fallback *source) { "
        "cblc_move_construct_at(destination, source); "
        "cblc_destroy_at(destination); return; }\n"
        "void main() { return; }\n";
    t_cblc_translation_unit unit;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Lifecycle intrinsic source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    struct lifecycle_expectation
    {
        const char *function_name;
        opcode operation;
        bool source_moves;
        bool may_throw;
    };
    const lifecycle_expectation expectations[] = {
        {"move_to", opcode::move_construct, true, false},
        {"relocate_to", opcode::move_construct, true, false},
        {"indexed_relocate", opcode::move_construct, true, false},
        {"relocate_copy", opcode::copy_construct, false, false},
        {"move_may_throw", opcode::move_construct, true, true}
    };
    for (const lifecycle_expectation &expectation : expectations)
    {
        const char *function_name = expectation.function_name;
        std::size_t function_index = 0;
        while (function_index < unit.function_count
            && std::strcmp(unit.functions[function_index].source_name,
                function_name) != 0)
            ++function_index;
        typed_function function_ir;
        verification_diagnostic diagnostic;
        if (function_index == unit.function_count
            || !lower_scalar_function(&unit, function_index, &function_ir,
                &diagnostic))
        {
            std::printf("Typed lifecycle lowering failed for %s: %s\n",
                function_name, diagnostic.message.c_str());
            for (std::size_t item_index = 0; item_index < unit.data_count;
                ++item_index)
                if (std::strcmp(unit.data_items[item_index].owner_function_name,
                        function_name) == 0)
                    std::printf("  item %s kind=%d class=%s active=%d alias=%d\n",
                        unit.data_items[item_index].source_name,
                        static_cast<int>(unit.data_items[item_index].kind),
                        unit.data_items[item_index].struct_type_name,
                        unit.data_items[item_index].is_active,
                        unit.data_items[item_index].is_alias);
            cblc_translation_unit_dispose(&unit);
            return FT_FAILURE;
        }
        const auto construction = std::find_if(function_ir.operations.begin(),
            function_ir.operations.end(), [&](const typed_operation &operation)
            {
                return operation.operation == expectation.operation;
            });
        const bool has_destroy = std::any_of(function_ir.operations.begin(),
            function_ir.operations.end(), [](const typed_operation &operation)
            {
                return operation.operation == opcode::destroy;
            });
        bool has_dynamic_destination_index = false;
        bool has_dynamic_source_index = false;
        if (construction != function_ir.operations.end())
        {
            for (const typed_place &place : function_ir.places)
            {
                if (place.id == construction->places.front())
                    has_dynamic_destination_index = std::any_of(
                        place.projections.begin(), place.projections.end(),
                        [](const place_projection &projection)
                        {
                            return projection.projection_kind
                                    == place_projection::kind::index
                                && projection.index_value != 0;
                        });
                for (place_id read_place : construction->effects.reads)
                    if (place.id == read_place)
                        has_dynamic_source_index = std::any_of(
                            place.projections.begin(), place.projections.end(),
                            [](const place_projection &projection)
                            {
                                return projection.projection_kind
                                        == place_projection::kind::index
                                    && projection.index_value != 0;
                            });
            }
        }
        if (construction == function_ir.operations.end()
            || construction->operands.size() != 1
            || construction->results.size() != 1
            || construction->callee == 0
            || construction->outcomes.empty()
            || (expectation.source_moves
                != (construction->outcomes.front().moved_from.size() == 1))
            || (expectation.may_throw
                != (construction->outcomes.size() == 2
                    && construction->outcomes[1].kind
                        == outcome_kind::thrown_exception))
            || (std::strcmp(function_name, "indexed_relocate") == 0
                && (!has_dynamic_destination_index
                    || !has_dynamic_source_index))
            || !has_destroy
            || !verify_typed_function(function_ir, &diagnostic))
        {
            std::printf("Typed lifecycle operation mismatch for %s: %s\n",
                function_name, diagnostic.message.c_str());
            cblc_translation_unit_dispose(&unit);
            return FT_FAILURE;
        }
        if (std::strcmp(function_name, "indexed_relocate") == 0)
        {
            const typed_place *destination_place = nullptr;
            for (const typed_place &place : function_ir.places)
                if (place.id == construction->places.front())
                    destination_place = &place;
            const auto dynamic_index = destination_place
                ? std::find_if(destination_place->projections.begin(),
                    destination_place->projections.end(),
                    [](const place_projection &projection)
                    {
                        return projection.projection_kind
                                == place_projection::kind::index
                            && projection.index_value != 0;
                    }) : std::vector<place_projection>::const_iterator{};
            if (!destination_place
                || dynamic_index == destination_place->projections.end())
            {
                cblc_translation_unit_dispose(&unit);
                return FT_FAILURE;
            }
            typed_function malformed_index = function_ir;
            const value_id index_value = dynamic_index->index_value;
            for (typed_value &value : malformed_index.values)
                if (value.id == index_value)
                    value.type = static_cast<type_id>(
                        CBLC_DATA_KIND_DOUBLE) + 1U;
            if (test_expect_int_equal(verify_typed_function(malformed_index,
                        &diagnostic) ? 1 : 0, 0,
                    "runtime pointer index must remain a canonical int") != FT_SUCCESS
                || test_expect_int_equal(static_cast<int>(diagnostic.code),
                    static_cast<int>(verification_error::invalid_type_or_projection),
                    "wrong dynamic index type is rejected by place verification")
                != FT_SUCCESS)
            {
                cblc_translation_unit_dispose(&unit);
                return FT_FAILURE;
            }
            typed_function unordered_index = function_ir;
            operation_id index_definition = 0;
            for (const typed_operation &operation : unordered_index.operations)
                if (std::find(operation.results.begin(), operation.results.end(),
                        index_value) != operation.results.end())
                    index_definition = operation.id;
            auto move_operation = std::find_if(unordered_index.operations.begin(),
                unordered_index.operations.end(), [&](const typed_operation &operation)
                {
                    return operation.id == construction->id;
                });
            if (move_operation == unordered_index.operations.end()
                || index_definition == 0)
            {
                cblc_translation_unit_dispose(&unit);
                return FT_FAILURE;
            }
            move_operation->must_follow.erase(std::remove(
                move_operation->must_follow.begin(),
                move_operation->must_follow.end(), index_definition),
                move_operation->must_follow.end());
            if (test_expect_int_equal(verify_typed_function(unordered_index,
                        &diagnostic) ? 1 : 0, 0,
                    "construction must be sequenced after index evaluation")
                    != FT_SUCCESS
                || test_expect_int_equal(static_cast<int>(diagnostic.code),
                    static_cast<int>(verification_error::use_before_definition),
                    "missing dynamic-index sequencing is rejected") != FT_SUCCESS)
            {
                cblc_translation_unit_dispose(&unit);
                return FT_FAILURE;
            }
        }
    }
    cblc_translation_unit_dispose(&unit);
    return FT_SUCCESS;
}

FT_TEST(test_backend_typed_ir_lowers_integer_throw_to_uncaught_cfg_exit)
{
    const char *source = "void main() { throw 7; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    control_flow_graph malformed_cfg;
    lifecycle_cleanup_plan cleanup_plan;
    typed_function malformed_function;
    verification_diagnostic diagnostic;
    bool saw_throw = false;
    bool saw_uncaught_exit = false;
    bool saw_typed_exception_edge = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Throw source parse diagnostic: %s\n", unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Throw typed-IR diagnostic: %s\n", diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!build_structured_control_flow_graph_with_cleanup(function_ir, &cfg,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Throw CFG diagnostic: %s\n", diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    malformed_function = function_ir;
    for (typed_operation &operation : malformed_function.operations)
        if (operation.operation == opcode::throw_value)
            operation.exception_type_id = 0;
    if (test_expect_int_equal(verify_typed_function(malformed_function,
                &diagnostic) ? 1 : 0, 0,
            "typed throw without an exception identity must be rejected") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::missing_effect_or_outcome),
            "missing throw identity reports incomplete exception semantics") != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    malformed_cfg = cfg;
    for (basic_block &block : malformed_cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::throw_value
            && !block.terminator.successors.empty())
            block.terminator.successors.front().exception_type_id = 0;
    if (test_expect_int_equal(verify_control_flow_graph(function_ir,
                malformed_cfg, &diagnostic) ? 1 : 0, 0,
            "CFG edge that drops the exception identity must be rejected") != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    for (const basic_block &block : cfg.blocks)
    {
        saw_throw = saw_throw
            || block.terminator.kind == cfg_terminator_kind::throw_value;
        saw_uncaught_exit = saw_uncaught_exit
            || block.terminator.kind == cfg_terminator_kind::uncaught_exception_exit;
        if (block.terminator.kind == cfg_terminator_kind::throw_value
            && !block.operations.empty() && block.terminator.successors.size() == 1)
        {
            const auto operation = std::find_if(function_ir.operations.begin(),
                function_ir.operations.end(), [&](const typed_operation &candidate)
                {
                    return candidate.id == block.operations.front();
                });
            saw_typed_exception_edge = operation != function_ir.operations.end()
                && operation->exception_type_id != 0
                && block.terminator.successors.front().exception_type_id
                    == operation->exception_type_id;
        }
    }
    cblc_translation_unit_dispose(&unit);
    return test_expect_int_equal(saw_throw && saw_uncaught_exit
                && saw_typed_exception_edge ? 1 : 0, 1,
        "integer throw preserves its exception type ID through the uncaught CFG edge");
}

FT_TEST(test_backend_typed_ir_routes_integer_throw_to_typed_int_handler)
{
    const char *source = "int main() { int result; result = 1; try { "
        "if (result > 0) { throw 7; } else { throw 8; } } "
        "catch (int caught_value) { result = caught_value; } "
        "catch (...) { result = 2; } return result; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    control_flow_graph malformed_cfg;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    int generation_diagnostic_code = 0;
    char generation_diagnostic_message[512] = {};
    bool saw_handled_throw = false;
    bool saw_exact_typed_match = false;
    bool saw_payload_binding = false;
    bool saw_handler_store = false;
    bool saw_uncaught_exit = false;
    std::size_t routed_throw_count = 0;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Catch-all source parse diagnostic: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Catch-all typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!build_structured_control_flow_graph(function_ir, &cfg, &diagnostic))
    {
        std::printf("Catch-all CFG diagnostic: %s\n", diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (cblc_generate_cobol_with_diagnostic(&unit, &generated_cobol,
            &generation_diagnostic_code, generation_diagnostic_message,
            sizeof(generation_diagnostic_message)) != FT_SUCCESS)
    {
        std::printf("Catch binding COBOL generation failed (%d): %s\n",
            generation_diagnostic_code, generation_diagnostic_message);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    malformed_cfg = cfg;
    for (basic_block &block : malformed_cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::throw_value)
        {
            block.terminator.exception_handler_region = 0;
            break;
        }
    if (test_expect_int_equal(verify_control_flow_graph(function_ir,
                malformed_cfg, &diagnostic) ? 1 : 0, 0,
            "CFG mutation that removes a catch-all destination must be rejected")
        != FT_SUCCESS)
    {
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    malformed_cfg = cfg;
    region_id catch_all_region = 0;
    basic_block_id catch_all_entry = 0;
    for (const typed_region &region : function_ir.regions)
        if (region.kind == region_kind::exception_handler && region.catches_all)
            catch_all_region = region.id;
    for (const basic_block &block : cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::handler_entry
            && block.terminator.handler_entry_region == catch_all_region)
            catch_all_entry = block.id;
    for (basic_block &block : malformed_cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::throw_value)
        {
            block.terminator.exception_handler_region = catch_all_region;
            block.terminator.successors.front().target = catch_all_entry;
        }
    if (test_expect_int_equal(verify_control_flow_graph(function_ir,
                malformed_cfg, &diagnostic) ? 1 : 0, 0,
            "CFG mutation that skips the first matching typed catch must be rejected")
        != FT_SUCCESS)
    {
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    malformed_cfg = cfg;
    for (basic_block &block : malformed_cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::throw_value)
        {
            block.terminator.successors.front().exception_payload = 0;
            break;
        }
    if (test_expect_int_equal(verify_control_flow_graph(function_ir,
                malformed_cfg, &diagnostic) ? 1 : 0, 0,
            "CFG mutation that discards the thrown value must be rejected")
        != FT_SUCCESS)
    {
        cma_free(generated_cobol);
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    for (const basic_block &block : cfg.blocks)
    {
        saw_uncaught_exit = saw_uncaught_exit
            || block.terminator.kind == cfg_terminator_kind::uncaught_exception_exit;
        if (block.terminator.kind == cfg_terminator_kind::throw_value
            && block.terminator.exception_handler_region != 0
            && block.terminator.successors.size() == 1)
        {
            const auto target = std::find_if(function_ir.regions.begin(),
                function_ir.regions.end(),
                [&](const typed_region &candidate)
                {
                    return candidate.id
                        == block.terminator.exception_handler_region;
                });
            const auto operation = std::find_if(function_ir.operations.begin(),
                function_ir.operations.end(), [&](const typed_operation &candidate)
                {
                    return !block.operations.empty()
                        && candidate.id == block.operations.front();
                });
            saw_handled_throw = target != function_ir.regions.end()
                && operation != function_ir.operations.end()
                && block.terminator.successors.front().outcome
                    == outcome_kind::thrown_exception;
            saw_exact_typed_match = saw_handled_throw
                && !target->catches_all
                && target->exception_type_id == operation->exception_type_id;
            saw_handled_throw = saw_handled_throw
                && block.terminator.successors.front().exception_payload
                    == operation->operands.front();
            if (saw_handled_throw)
                routed_throw_count += 1;
        }
        if (!block.operations.empty())
        {
            const auto operation = std::find_if(function_ir.operations.begin(),
                function_ir.operations.end(), [&](const typed_operation &candidate)
                {
                    return candidate.id == block.operations.front();
                });
            saw_handler_store = saw_handler_store
                || (operation != function_ir.operations.end()
                    && operation->operation == opcode::assign);
            saw_payload_binding = saw_payload_binding
                || (operation != function_ir.operations.end()
                    && operation->operation == opcode::catch_bind
                    && operation->places.size() == 1
                    && operation->exception_type_id != 0);
        }
    }
    const int test_result = test_expect_int_equal(saw_handled_throw && saw_exact_typed_match
            && saw_payload_binding
            && saw_handler_store
            && routed_throw_count == 2
            && !saw_uncaught_exit
            && ft_strnstr(generated_cobol,
                "MOVE CBLC-EX-INT-PAYLOAD TO CAUGHT-VALUE",
                std::strlen(generated_cobol)) != nullptr ? 1 : 0, 1,
        "typed catch binds the thrown int and later source reads that binding");
    cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return test_result;
}

FT_TEST(test_backend_typed_ir_routes_unmatched_throw_to_catch_all_handler)
{
    const char *source = "void main() { try { throw 7; } catch (...) { } }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    bool routed_to_catch_all = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count
        || !lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic)
        || !build_structured_control_flow_graph(function_ir, &cfg, &diagnostic))
    {
        std::printf("Catch-all typed CFG diagnostic: %s %s\n",
            unit.parse_error_message, diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    for (const basic_block &block : cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::throw_value
            && block.terminator.exception_handler_region != 0)
            for (const typed_region &region : function_ir.regions)
                if (region.id == block.terminator.exception_handler_region
                    && region.kind == region_kind::exception_handler
                    && region.catches_all
                    && region.exception_type_id == 0)
                    routed_to_catch_all = true;
    cblc_translation_unit_dispose(&unit);
    return test_expect_int_equal(routed_to_catch_all ? 1 : 0, 1,
        "catch-all remains the fallback for a known exception type");
}

FT_TEST(test_backend_typed_ir_applies_throw_cleanup_before_uncaught_exit)
{
    const char *source = "class Tracker { public: Tracker() { } ~Tracker() { } };\n"
        "void main() { Tracker first; Tracker second; Tracker third; throw 7; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    const cleanup_edge *throw_cleanup_edge = nullptr;
    operation_id throw_operation_id = 0;
    std::vector<value_id> constructed_values;
    bool found_destroy_before_throw = false;
    std::size_t destroy_count = 0;
    bool saw_scope_exit = false;
    bool saw_throw = false;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count
        || !lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Throw cleanup lowering diagnostic: %s %s\n",
            unit.parse_error_message, diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!build_structured_control_flow_graph_with_cleanup(function_ir, &cfg,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Throw cleanup CFG diagnostic: operation=%llu %s\n",
            static_cast<unsigned long long>(diagnostic.operation),
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    for (std::size_t operation_index = 0;
        operation_index < function_ir.operations.size(); ++operation_index)
    {
        const typed_operation &operation = function_ir.operations[operation_index];
        if (operation.operation == opcode::construct
            && operation.results.size() == 1)
            constructed_values.push_back(operation.results.front());
        if (operation.operation == opcode::scope_exit)
        {
            destroy_count += 1;
            saw_scope_exit = true;
            found_destroy_before_throw = !saw_throw;
        }
        else if (operation.operation == opcode::destroy)
            destroy_count += 1;
        if (operation.operation == opcode::throw_value)
        {
            saw_throw = true;
            throw_operation_id = operation.id;
        }
    }
    for (const cleanup_edge &edge : cleanup_plan.failure_edges)
        if (edge.operation == throw_operation_id
            && edge.outcome == outcome_kind::thrown_exception)
            throw_cleanup_edge = &edge;
    cblc_translation_unit_dispose(&unit);
    if (test_expect_int_equal(destroy_count == 3 && saw_scope_exit && saw_throw
                && found_destroy_before_throw && throw_cleanup_edge
                && constructed_values.size() == 3
                && throw_cleanup_edge->destruction_order.size() == 3
                ? 1 : 0,
            1, "typed failure edge carries every live object's scope cleanup")
        != FT_SUCCESS)
        return (FT_FAILURE);
    for (std::size_t index = 0; index < constructed_values.size(); ++index)
        if (test_expect_int_equal(
                static_cast<int>(throw_cleanup_edge->destruction_order[index]),
                static_cast<int>(constructed_values[
                    constructed_values.size() - index - 1]),
                "throw edge destroys completed objects in exact reverse construction order")
            != FT_SUCCESS)
            return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_backend_typed_ir_lowers_local_class_lifecycle_to_cleanup_verified_cfg)
{
    const char *source = "class Tracker { public: int value;\n"
        "    Tracker(int seed) { value = seed; }\n"
        "    Tracker() { value = 7; }\n"
        "    ~Tracker() { value = 0; }\n"
        "};\n"
        "void main() { Tracker tracker; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    std::size_t owned_value_count = 0;
    std::size_t construction_count = 0;
    std::size_t destruction_count = 0;
    const typed_operation *constructor_operation = nullptr;
    const typed_operation *destructor_operation = nullptr;
    typed_function mutated_dispatch;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Class lifecycle parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Class lifecycle typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_value &value : function_ir.values)
        if (value.category == value_category::owned_object)
            owned_value_count += 1;
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::construct)
        {
            construction_count += 1;
            constructor_operation = &operation;
        }
        if (operation.operation == opcode::destroy
            || operation.operation == opcode::scope_exit)
        {
            destruction_count += 1;
            destructor_operation = &operation;
        }
    }
    if (test_expect_int_equal(static_cast<int>(owned_value_count), 1,
            "class declaration receives an owned-object type") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(construction_count), 1,
            "class declaration produces one typed construct")
        != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(destruction_count), 1,
            "scope cleanup becomes a typed destruction") != FT_SUCCESS)
        goto cleanup;
    if (!constructor_operation || constructor_operation->callee == 0
        || !destructor_operation || destructor_operation->callee == 0
        || std::none_of(function_ir.callables.begin(), function_ir.callables.end(),
            [&](const typed_callable &callable)
            {
                return callable.id == constructor_operation->callee
                    && callable.kind == callable_kind::constructor;
            })
        || std::none_of(function_ir.callables.begin(), function_ir.callables.end(),
            [&](const typed_callable &callable)
            {
                return callable.id == destructor_operation->callee
                    && callable.kind == callable_kind::destructor;
            }))
    {
        std::printf("Constructor/destructor lifecycle operations lack canonical dispatch metadata\n");
        goto cleanup;
    }
    mutated_dispatch = function_ir;
    for (typed_operation &operation : mutated_dispatch.operations)
        if (operation.operation == opcode::construct)
            operation.callee += 1;
    if (verify_typed_function(mutated_dispatch, &diagnostic)
        || diagnostic.code != verification_error::incomplete_call_target)
    {
        std::printf("Forged constructor target was not rejected by typed IR verification\n");
        goto cleanup;
    }
    mutated_dispatch = function_ir;
    for (typed_operation &operation : mutated_dispatch.operations)
        if (operation.operation == opcode::scope_exit)
            operation.callee += 1;
    if (verify_typed_function(mutated_dispatch, &diagnostic)
        || diagnostic.code != verification_error::incomplete_call_target)
    {
        std::printf("Forged destructor target was not rejected by typed IR verification\n");
        goto cleanup;
    }
    if (test_expect_success(build_structured_control_flow_graph_with_cleanup(
                function_ir, &cfg, &cleanup_plan, &diagnostic)
                ? FT_SUCCESS : FT_FAILURE,
            "parsed class lifecycle should pass typed CFG and cleanup verification")
        != FT_SUCCESS)
    {
        std::printf("Class lifecycle CFG diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "verified class lifecycle should lower through the COBOL emitter")
            != FT_SUCCESS
        || !generated_cobol
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-CONSTRUCTOR-MAIN-TRACKER-1 THRU")
        || !std::strstr(generated_cobol,
            "PERFORM CBLC-DESTRUCTOR-MAIN-TRACKER."))
    {
        std::printf("Typed lifecycle lowering omitted the class constructor or destructor call\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_emits_local_class_array_lifecycle)
{
    const char *source = "class Tracker { public: int value;\n"
        "    Tracker() { value = 7; }\n"
        "    ~Tracker() { value = 0; }\n"
        "};\n"
        "void main() { Tracker trackers[3]; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    std::size_t construction_count = 0;
    std::size_t destruction_count = 0;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count
        || !lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Local class-array typed lowering failed: %s\n",
            unit.parse_error_message[0] ? unit.parse_error_message
                : diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::construct)
            construction_count += 1;
        else if (operation.operation == opcode::scope_exit)
            destruction_count += 1;
    }
    if (test_expect_int_equal(static_cast<int>(construction_count), 3,
            "each local class-array element has a typed construct") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(destruction_count), 3,
            "each local class-array element has a typed scope exit") != FT_SUCCESS
        || !build_structured_control_flow_graph_with_cleanup(function_ir, &cfg,
            &cleanup_plan, &diagnostic))
    {
        if (diagnostic.code != verification_error::none)
            std::printf("Local class-array CFG failed: %s\n",
                diagnostic.message.c_str());
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "local class-array lifecycle should reach COBOL emission") != FT_SUCCESS
        || !generated_cobol || !std::strstr(generated_cobol, "MAIN-TRACKERS")
        || !std::strstr(generated_cobol, "(3)"))
    {
        std::printf("Local class-array COBOL layout or lifecycle emission is missing\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_lowers_class_copy_assignment_to_emitter)
{
    const char *source = "class Box { public: int value;\n"
        "    Box() { value = 0; }\n"
        "    void operator=(int source) { value = source; }\n"
        "    void operator=(Box source) { value = source.value; }\n"
        "    ~Box() { value = 0; }\n"
        "};\n"
        "void main() { Box first; Box second; second = first; first = first; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    const typed_operation *copy_assignment = nullptr;
    std::string expected_source_stage;
    std::size_t copy_assignment_count = 0;
    bool saw_self_assignment = false;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count
        || !lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Typed class-copy lowering failed: %s\n",
            unit.parse_error_message[0] ? unit.parse_error_message
                : diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::copy_assign)
        {
            copy_assignment = &operation;
            copy_assignment_count += 1;
            if (operation.operands.size() == 1 && operation.places.size() == 1)
                for (const typed_place &place : function_ir.places)
                    if (place.id == operation.places.front()
                        && operation.operands.front() == place.base_value)
                        saw_self_assignment = true;
        }
    if (!copy_assignment || copy_assignment_count != 2 || !saw_self_assignment
        || copy_assignment->callee == 0
        || copy_assignment->operands.size() != 1
        || copy_assignment->places.size() != 1
        || !build_structured_control_flow_graph_with_cleanup(function_ir,
            &cfg, &cleanup_plan, &diagnostic))
    {
        std::printf("Typed class-copy CFG failed: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_callable &callable : function_ir.callables)
        if (callable.id == copy_assignment->callee
            && callable.kind == callable_kind::copy_assignment)
            for (std::size_t type_index = 0; type_index < unit.struct_type_count;
                ++type_index)
                if (std::strcmp(unit.struct_types[type_index].source_name, "Box") == 0)
                    for (std::size_t method_index = 0;
                        method_index < unit.struct_types[type_index].method_count;
                        ++method_index)
                    {
                        const t_cblc_method &method =
                            unit.struct_types[type_index].methods[method_index];
                        if (method.overload_index == callable.overload_index
                            && method.parameter_count == 1)
                            expected_source_stage = std::string("MOVE MAIN-FIRST TO ")
                                + method.parameters[0].cobol_name + ".";
                    }
    if (expected_source_stage.empty())
    {
        std::printf("Typed copy-assignment overload has no exact source signature\n");
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "typed class-copy operation should reach COBOL emission") != FT_SUCCESS
        || !generated_cobol)
    {
        std::printf("Typed copy-assignment dispatch is missing from generated COBOL\n");
        goto cleanup;
    }
    {
        const char *move = generated_cobol;
        std::size_t staged_arguments = 0;
        while ((move = std::strstr(move, expected_source_stage.c_str())) != nullptr)
        {
            staged_arguments += 1;
            move += expected_source_stage.size();
        }
        if (staged_arguments != 2)
        {
            std::printf("Expected both ordinary and self-copy assignments to stage their by-value argument\n");
            goto cleanup;
        }
    }
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_tracks_each_class_array_element_lifetime)
{
    const char *source = "class Tracker { public:\n"
        "    int value;\n"
        "    Tracker() { throw 3; }\n"
        "    ~Tracker() { }\n"
        "};\n"
        "void main() { Tracker trackers[3]; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::vector<value_id> constructed_elements;
    std::vector<value_id> destroyed_elements;
    std::vector<place_id> element_places;
    std::vector<bool> observed_failure_prefixes(3, false);
    std::vector<std::pair<operation_id, std::size_t>> observed_failure_operations;
    const cleanup_site *normal_exit = nullptr;
    std::size_t destruction_count = 0;
    std::size_t exceptional_exit_count = 0;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Class-array lifecycle parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Class-array lifecycle typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::construct)
        {
            constructed_elements.push_back(operation.results.front());
            element_places.push_back(operation.places.front());
        }
        else if (operation.operation == opcode::destroy
            || operation.operation == opcode::scope_exit)
        {
            destruction_count += 1;
            destroyed_elements.push_back(operation.outcomes.front().destroyed.front());
        }
    }
    if (test_expect_int_equal(static_cast<int>(constructed_elements.size()), 3,
            "each class-array element has an independent typed construction") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(destruction_count), 3,
            "each class-array element has an independent typed destruction") != FT_SUCCESS)
        goto cleanup;
    for (std::size_t element = 0; element < element_places.size(); ++element)
    {
        const typed_place *place = nullptr;
        for (const typed_place &candidate : function_ir.places)
            if (candidate.id == element_places[element])
                place = &candidate;
        const auto storage = place
            ? std::find_if(function_ir.values.begin(), function_ir.values.end(),
                [&](const typed_value &value)
                {
                    return value.id == place->base_value;
                })
            : function_ir.values.end();
        if (test_expect_success(place && place->projections.size() == 1
                    && place->projections.front().projection_kind
                        == place_projection::kind::index
                    && place->projections.front().resolved_member == element + 1
                    && storage != function_ir.values.end()
                    && storage->category == value_category::compiler_only
                    && storage->array_extent == 3
                    ? FT_SUCCESS : FT_FAILURE,
                "element place retains its checked index within aggregate array storage")
            != FT_SUCCESS)
            goto cleanup;
    }
    {
        typed_function member_projection = function_ir;
        verification_diagnostic member_diagnostic;
        const auto tracker_type = std::find_if(member_projection.types.begin(),
            member_projection.types.end(), [](const typed_type &type)
            {
                return type.canonical_name == "Tracker";
            });
        const auto root_value = std::find_if(member_projection.values.begin(),
            member_projection.values.end(), [&](const typed_value &value)
            {
                return value.id == constructed_elements.front();
            });
        if (tracker_type == member_projection.types.end()
            || tracker_type->fields.size() != 1
            || root_value == member_projection.values.end())
            goto cleanup;
        typed_place field_place;
        field_place.id = static_cast<place_id>(member_projection.places.size() + 1);
        field_place.base_value = root_value->id;
        field_place.type = tracker_type->fields.front().type;
        field_place.projections.push_back({place_projection::kind::field,
            tracker_type->fields.front().member_id, field_place.type});
        member_projection.places.push_back(field_place);
        if (test_expect_success(verify_typed_function(member_projection,
                    &member_diagnostic) ? FT_SUCCESS : FT_FAILURE,
                "field projection resolves through canonical class member metadata")
            != FT_SUCCESS)
            goto cleanup;
        member_projection.places.back().projections.front().resolved_member = 2;
        if (test_expect_success(verify_typed_function(member_projection,
                    &member_diagnostic) ? FT_FAILURE : FT_SUCCESS,
                "typed verifier rejects a missing class member projection")
            != FT_SUCCESS
            || test_expect_int_equal(static_cast<int>(member_diagnostic.code),
                static_cast<int>(verification_error::invalid_type_or_projection),
                "unresolved class member has a stable typed diagnostic") != FT_SUCCESS)
            goto cleanup;
    }
    {
        typed_function invalid_array_projection = function_ir;
        verification_diagnostic projection_diagnostic;
        const auto element_place = std::find_if(invalid_array_projection.places.begin(),
            invalid_array_projection.places.end(), [](const typed_place &candidate)
            {
                return !candidate.projections.empty()
                    && candidate.projections.front().projection_kind
                        == place_projection::kind::index;
            });
        if (element_place == invalid_array_projection.places.end())
            goto cleanup;
        element_place->projections.front().resolved_member = 4;
        if (test_expect_success(verify_typed_function(invalid_array_projection,
                    &projection_diagnostic) ? FT_FAILURE : FT_SUCCESS,
                "typed verifier rejects an array element projection beyond its extent")
            != FT_SUCCESS
            || test_expect_int_equal(static_cast<int>(projection_diagnostic.code),
                static_cast<int>(verification_error::invalid_type_or_projection),
                "out-of-range array projection has a stable typed diagnostic")
                != FT_SUCCESS)
            goto cleanup;
    }
    if (!build_structured_control_flow_graph_with_cleanup(function_ir, &cfg,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Class-array lifecycle CFG diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const cleanup_site &site : cleanup_plan.exits)
    {
        if (site.exit_kind == cfg_terminator_kind::normal_exit)
            normal_exit = &site;
    }
    for (const cleanup_edge &edge : cleanup_plan.failure_edges)
    {
        if (edge.outcome != outcome_kind::thrown_exception)
            continue;
        const std::size_t completed_count = edge.destruction_order.size();
        const auto prior_operation = std::find_if(
            observed_failure_operations.begin(), observed_failure_operations.end(),
            [&](const std::pair<operation_id, std::size_t> &entry)
            {
                return entry.first == edge.operation;
            });
        if (prior_operation != observed_failure_operations.end())
        {
            if (prior_operation->second != completed_count)
            {
                std::printf("Assertion failed: one array construction operation has inconsistent failure cleanup\n");
                goto cleanup;
            }
            continue;
        }
        if (completed_count >= observed_failure_prefixes.size()
            || observed_failure_prefixes[completed_count])
        {
            std::printf("Assertion failed: each array construction failure must have a distinct completed-prefix cleanup\n");
            goto cleanup;
        }
        for (std::size_t element = 0; element < completed_count; ++element)
            if (edge.destruction_order[element]
                != constructed_elements[completed_count - element - 1])
            {
                std::printf("Assertion failed: failed array element cleanup must run in reverse completion order\n");
                goto cleanup;
            }
        observed_failure_prefixes[completed_count] = true;
        observed_failure_operations.emplace_back(edge.operation, completed_count);
        exceptional_exit_count += 1;
    }
    if (test_expect_success(normal_exit
                && normal_exit->destruction_order.empty()
                && exceptional_exit_count == 3
                && observed_failure_prefixes[0]
                && observed_failure_prefixes[1]
                && observed_failure_prefixes[2]
                && destroyed_elements.size() == 3
                ? FT_SUCCESS : FT_FAILURE,
            "normal cleanup is explicit and each constructor failure gets an edge-specific prefix plan") != FT_SUCCESS)
        goto cleanup;
    for (std::size_t element = 0; element < constructed_elements.size(); ++element)
        if (test_expect_int_equal(static_cast<int>(destroyed_elements[element]),
                static_cast<int>(constructed_elements[constructed_elements.size() - element - 1]),
                "explicit array destruction runs in reverse construction order")
            != FT_SUCCESS)
            goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_composes_nested_member_lifetimes)
{
    const char *source = "class Leaf { public:\n"
        "    Leaf() { throw 8; }\n"
        "    ~Leaf() { }\n"
        "};\n"
        "class Aggregate { public:\n"
        "    Leaf first;\n"
        "    Leaf rest[2];\n"
        "    Aggregate() { }\n"
        "    ~Aggregate() { }\n"
        "};\n"
        "void main() { Aggregate value; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::vector<value_id> leaf_constructions;
    std::vector<value_id> leaf_destructions;
    value_id aggregate_value = 0;
    std::vector<bool> failed_prefixes(3, false);
    std::size_t exceptional_exits = 0;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Nested member lifecycle parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Nested member typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_operation &operation : function_ir.operations)
    {
        if (operation.operation == opcode::construct)
        {
            const auto value = std::find_if(function_ir.values.begin(),
                function_ir.values.end(), [&](const typed_value &candidate)
                {
                    return candidate.id == operation.results.front();
                });
            if (value == function_ir.values.end())
                goto cleanup;
            const auto type = std::find_if(function_ir.types.begin(),
                function_ir.types.end(), [&](const typed_type &candidate)
                {
                    return candidate.id == value->type;
                });
            if (type != function_ir.types.end() && type->canonical_name == "Leaf")
                leaf_constructions.push_back(value->id);
            else if (type != function_ir.types.end()
                && type->canonical_name == "Aggregate")
                aggregate_value = value->id;
        }
        else if (operation.operation == opcode::destroy
            || operation.operation == opcode::scope_exit)
        {
            const value_id destroyed = operation.outcomes.front().destroyed.front();
            const auto value = std::find_if(function_ir.values.begin(),
                function_ir.values.end(), [&](const typed_value &candidate)
                {
                    return candidate.id == destroyed;
                });
            if (value == function_ir.values.end())
                goto cleanup;
            const auto type = std::find_if(function_ir.types.begin(),
                function_ir.types.end(), [&](const typed_type &candidate)
                {
                    return candidate.id == value->type;
                });
            if (type != function_ir.types.end() && type->canonical_name == "Leaf")
                leaf_destructions.push_back(destroyed);
        }
    }
    if (test_expect_int_equal(static_cast<int>(leaf_constructions.size()), 3,
            "each class field and class-array element receives its own construct operation")
            != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(leaf_destructions.size()), 3,
            "each class field and class-array element receives its own destroy operation")
            != FT_SUCCESS
        || test_expect_success(aggregate_value != 0 ? FT_SUCCESS : FT_FAILURE,
            "aggregate itself remains a distinct completed lifetime") != FT_SUCCESS)
        goto cleanup;
    if (!build_structured_control_flow_graph_with_cleanup(function_ir, &cfg,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Nested member cleanup CFG diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const cleanup_site &site : cleanup_plan.exits)
        if (site.exit_kind == cfg_terminator_kind::uncaught_exception_exit)
        {
            const std::size_t completed = site.destruction_order.size();
            if (completed >= failed_prefixes.size() || failed_prefixes[completed])
                goto cleanup;
            for (std::size_t element = 0; element < completed; ++element)
                if (site.destruction_order[element]
                    != leaf_constructions[completed - element - 1])
                    goto cleanup;
            failed_prefixes[completed] = true;
            exceptional_exits += 1;
        }
    if (test_expect_success(exceptional_exits == 3 && failed_prefixes[0]
                && failed_prefixes[1] && failed_prefixes[2]
                ? FT_SUCCESS : FT_FAILURE,
            "each nested member constructor failure cleans only completed subobjects")
        != FT_SUCCESS)
        goto cleanup;
    for (std::size_t index = 0; index < leaf_constructions.size(); ++index)
        if (test_expect_int_equal(static_cast<int>(leaf_destructions[index]),
                static_cast<int>(leaf_constructions[leaf_constructions.size() - index - 1]),
                "nested members and member arrays destroy in reverse completion order")
            != FT_SUCCESS)
            goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_plans_cleanup_after_later_constructor_failure)
{
    const char *source = "class Worker {\n"
        "    Worker() { throw 15; }\n"
        "    ~Worker() { }\n"
        "};\n"
        "void main() { Worker first; Worker second; }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    std::size_t constructor_failure_edges = 0;
    std::size_t empty_constructor_cleanup_count = 0;
    std::size_t partial_constructor_cleanup_count = 0;
    char *generated_cobol = nullptr;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Throwing class lifecycle parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Throwing class typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    if (test_expect_success(build_structured_control_flow_graph_with_cleanup(
                function_ir, &cfg, &cleanup_plan, &diagnostic)
                ? FT_SUCCESS : FT_FAILURE,
            "throwing constructor CFG should have edge-specific cleanup")
        != FT_SUCCESS)
    {
        std::printf("Throwing class CFG diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::construct
            && operation.outcomes.size() == 2
            && operation.outcomes[1].kind == outcome_kind::thrown_exception)
        {
            constructor_failure_edges += 1;
            if (operation.outcomes[1].destroyed.empty())
                empty_constructor_cleanup_count += 1;
            else if (operation.outcomes[1].destroyed.size() == 1)
                partial_constructor_cleanup_count += 1;
        }
    if (test_expect_success(constructor_failure_edges == 2
                && empty_constructor_cleanup_count == 1
                && partial_constructor_cleanup_count == 1
                ? FT_SUCCESS : FT_FAILURE,
            "constructor failure outcomes distinguish empty and partial cleanup")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "production COBOL generation should pass through the typed cleanup analysis")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_uses_resolved_constructor_throw_effect)
{
    const char *source = "class Probe { public: "
        "Probe(int value) { throw value; } "
        "Probe(double value) { } ~Probe() { } }; "
        "void main() { double value; Probe probe(value); }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    verification_diagnostic diagnostic;
    bool saw_class_construction = false;
    bool class_construction_may_throw = true;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Overloaded constructor parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Overloaded constructor typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::construct
            && !operation.results.empty())
        {
            const auto value = std::find_if(function_ir.values.begin(),
                function_ir.values.end(), [&](const typed_value &candidate)
                {
                    return candidate.id == operation.results.front();
                });
            const auto type = value == function_ir.values.end()
                ? function_ir.types.end()
                : std::find_if(function_ir.types.begin(), function_ir.types.end(),
                    [&](const typed_type &candidate)
                    {
                        return candidate.id == value->type;
                    });
            if (value != function_ir.values.end()
                && value->category == value_category::owned_object
                && type != function_ir.types.end()
                && type->canonical_name == "Probe")
            {
                saw_class_construction = true;
                class_construction_may_throw = operation.effects.may_fail;
            }
        }
    if (test_expect_int_equal(saw_class_construction ? 1 : 0, 1,
            "resolved char overload must produce a typed class construction")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(class_construction_may_throw ? 1 : 0, 0,
            "non-throwing char overload must not inherit the int overload's throw effect")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_lowers_source_if_else_to_structured_cfg)
{
    const char *source = "int main() {\n int value;\n value = 1;\n"
        " if (value > 0) { value = 2; } else { value = 3; }\n"
        " return value;\n}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    std::size_t conditional_count = 0;
    char *generated_cobol = nullptr;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0)
    {
        std::printf("CBL-C parse diagnostic: %s\n", unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Typed IR diagnostic: %s\n", diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_region &region : function_ir.regions)
        if (region.kind == region_kind::conditional)
            conditional_count += 1;
    if (test_expect_int_equal(static_cast<int>(conditional_count), 1,
            "parsed if/else becomes one typed conditional region") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(build_structured_control_flow_graph(function_ir,
                &cfg, &diagnostic) ? 1 : 0, 1,
            "source-derived conditional IR builds a verified CFG") != FT_SUCCESS)
    {
        std::printf("Structured CFG diagnostic: %s\n", diagnostic.message.c_str());
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "verified source conditional still reaches COBOL generation") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_lowers_source_while_with_retested_condition)
{
    const char *source = "int main() {\n int value;\n value = 1;\n"
        " while (value > 0) { value = 0; }\n return value;\n}\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    const typed_region *loop = nullptr;
    const typed_region *body_region = nullptr;
    const basic_block *branch = nullptr;
    const basic_block *body_block = nullptr;
    const basic_block *back_edge_block = nullptr;
    basic_block_id condition_entry;
    basic_block_id body_entry;
    char *generated_cobol = nullptr;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.function_count == 0)
    {
        std::printf("CBL-C parse diagnostic: %s\n", unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, 0, &function_ir, &diagnostic))
    {
        std::printf("Typed IR diagnostic: %s\n", diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_region &region : function_ir.regions)
    {
        if (region.kind == region_kind::loop)
            loop = &region;
        if (loop && region.parent == loop->id
            && region.kind == region_kind::lexical_scope)
            body_region = &region;
    }
    if (test_expect_success(loop != nullptr ? FT_SUCCESS : FT_FAILURE,
            "source while statement becomes a loop region") != FT_SUCCESS
        || test_expect_int_equal(loop
                ? static_cast<int>(loop->condition_operations.size()) : 0,
            3, "loop owns typed load, literal, and comparison pre-test operations")
            != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(build_structured_control_flow_graph(function_ir,
                &cfg, &diagnostic) ? 1 : 0, 1,
            "source-derived loop IR builds a verified CFG") != FT_SUCCESS)
    {
        std::printf("Structured CFG diagnostic at operation %u: %s\n",
            diagnostic.operation, diagnostic.message.c_str());
        goto cleanup;
    }
    condition_entry = 0;
    body_entry = 0;
    for (const basic_block &block : cfg.blocks)
    {
        if (block.terminator.kind == cfg_terminator_kind::branch)
            branch = &block;
        if (!block.operations.empty()
            && block.operations.front() == loop->condition_operations.front())
            condition_entry = block.id;
    }
    if (!branch)
        goto cleanup;
    body_entry = branch->terminator.successors[0].target;
    if (test_expect_int_equal(branch->terminator.successors[1].target != 0
                ? 1 : 0, 1,
            "loop false edge exits to its continuation") != FT_SUCCESS)
        goto cleanup;
    for (const basic_block &block : cfg.blocks)
    {
        if (block.id == body_entry && body_region
            && !body_region->sequence.empty()
            && !block.operations.empty()
            && block.operations.front() == body_region->sequence.front().id)
            body_block = &block;
        if (body_region && !body_region->operations.empty()
            && !block.operations.empty()
            && block.operations.front() == body_region->operations.back())
            back_edge_block = &block;
    }
    if (test_expect_int_equal(condition_entry != 0 && body_block && back_edge_block
                && !back_edge_block->terminator.successors.empty()
                && back_edge_block->terminator.successors[0].target == condition_entry
                ? 1 : 0, 1,
            "loop body exit back-edge re-evaluates the condition prologue") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "verified source loop still reaches COBOL generation") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_typed_ir_destroys_loop_local_on_each_back_edge)
{
    const char *source = "class Iteration { public: int value; "
        "Iteration() { value = 0; } ~Iteration() { display(value); } };\n"
        "void main() { int index; index = 0; "
        "while (index < 2) { Iteration current; index = 2; } }\n";
    t_cblc_translation_unit unit;
    typed_function function_ir;
    control_flow_graph cfg;
    lifecycle_cleanup_plan cleanup_plan;
    verification_diagnostic diagnostic;
    char *generated_cobol = nullptr;
    std::size_t loop_count = 0;
    std::size_t loop_scope_exit_count = 0;
    bool scope_exit_in_loop_region = false;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS
        || unit.entry_function_index >= unit.function_count)
    {
        std::printf("Loop-local lifecycle parse diagnostic: %s\n",
            unit.parse_error_message);
        goto cleanup;
    }
    if (!lower_scalar_function(&unit, unit.entry_function_index,
            &function_ir, &diagnostic))
    {
        std::printf("Loop-local lifecycle typed-IR diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_region &region : function_ir.regions)
        if (region.kind == region_kind::loop)
            loop_count += 1;
    for (const typed_operation &operation : function_ir.operations)
        if (operation.operation == opcode::scope_exit)
        {
            loop_scope_exit_count += 1;
            for (const typed_region &region : function_ir.regions)
                if (region.kind == region_kind::lexical_scope
                    && region.id == operation.region)
                    for (const typed_region &candidate : function_ir.regions)
                        if (candidate.kind == region_kind::loop
                            && candidate.id == region.parent)
                            scope_exit_in_loop_region = true;
        }
    if (test_expect_int_equal(static_cast<int>(loop_count), 1,
            "loop-local object is enclosed by one typed loop region") != FT_SUCCESS
        || test_expect_int_equal(static_cast<int>(loop_scope_exit_count), 1,
            "loop-local object has one guarded scope-exit operation") != FT_SUCCESS
        || test_expect_int_equal(scope_exit_in_loop_region ? 1 : 0, 1,
            "loop-local cleanup belongs to the iteration scope") != FT_SUCCESS)
        goto cleanup;
    if (!build_structured_control_flow_graph_with_cleanup(function_ir, &cfg,
            &cleanup_plan, &diagnostic))
    {
        std::printf("Loop-local lifecycle CFG diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    if (test_expect_success(verify_lifetime_transitions(function_ir, cfg,
                &diagnostic) ? FT_SUCCESS : FT_FAILURE,
            "loop back-edge re-enters with the iteration object destroyed")
        != FT_SUCCESS)
    {
        std::printf("Loop-local lifecycle verification diagnostic: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "verified loop-local lifecycle reaches COBOL generation") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(ft_strnstr(generated_cobol,
                "PERFORM CBLC-DESTRUCTOR-MAIN-CURRENT",
                std::strlen(generated_cobol)) != nullptr
                ? FT_SUCCESS : FT_FAILURE,
            "generated loop body invokes the local object's destructor")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return status;
}

FT_TEST(test_backend_cfg_routes_normal_and_failure_outcomes)
{
    typed_function function = make_may_fail_construct_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    const basic_block *invoke_block;
    std::size_t index;
    bool found_normal;
    bool found_allocation_failure;

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "may-fail construction should lower to an invoke CFG") != FT_SUCCESS)
        return (FT_FAILURE);
    invoke_block = &cfg.blocks[0];
    if (test_expect_int_equal(static_cast<int>(invoke_block->terminator.kind),
            static_cast<int>(cfg_terminator_kind::invoke),
            "may-fail operation ends its block with invoke") != FT_SUCCESS)
        return (FT_FAILURE);
    found_normal = false;
    found_allocation_failure = false;
    index = 0;
    while (index < invoke_block->terminator.successors.size())
    {
        const cfg_edge &edge = invoke_block->terminator.successors[index];
        if (edge.outcome == outcome_kind::normal)
            found_normal = true;
        if (edge.outcome == outcome_kind::allocation_failure)
            found_allocation_failure = true;
        index += 1;
    }
    if (test_expect_int_equal(found_normal ? 1 : 0, 1,
            "invoke preserves the normal-success edge") != FT_SUCCESS
        || test_expect_int_equal(found_allocation_failure ? 1 : 0, 1,
            "invoke preserves the allocation-failure edge") != FT_SUCCESS)
        return (FT_FAILURE);
    cfg.blocks[0].terminator.successors.pop_back();
    return test_expect_int_equal(verify_control_flow_graph(function, cfg, &diagnostic) ? 1 : 0,
        0, "CFG verifier rejects omitted failure outcome edges");
}

FT_TEST(test_backend_cfg_rejects_skipped_or_reordered_operations)
{
    typed_function function = make_valid_function();
    control_flow_graph cfg;
    verification_diagnostic diagnostic;

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "valid linear function should build a CFG") != FT_SUCCESS)
        return (FT_FAILURE);
    cfg.blocks[0].terminator.successors[0].target = cfg.blocks[2].id;
    if (test_expect_int_equal(verify_control_flow_graph(function, cfg,
                &diagnostic) ? 1 : 0, 0,
            "CFG edge that skips a source operation must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_cfg_edge),
            "skipped source operation reports an invalid CFG edge") != FT_SUCCESS)
        return (FT_FAILURE);

    if (test_expect_int_equal(build_linear_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "valid linear function should rebuild after graph mutation")
        != FT_SUCCESS)
        return (FT_FAILURE);
    cfg.entry = cfg.blocks[1].id;
    return test_expect_int_equal(verify_control_flow_graph(function, cfg,
            &diagnostic) ? 1 : 0, 0,
        "entry that starts after the first source operation must fail");
}

FT_TEST(test_backend_cfg_builds_structured_if_and_loop_regions)
{
    typed_function function = make_structured_function(region_kind::conditional);
    control_flow_graph cfg;
    verification_diagnostic diagnostic;
    basic_block *branch = nullptr;
    std::size_t branch_count = 0;

    if (test_expect_int_equal(build_structured_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "structured conditional regions should lower to a verified CFG")
        != FT_SUCCESS)
    {
        std::printf("Structured CFG diagnostic: %s\n", diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    for (basic_block &block : cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::branch)
        {
            branch = &block;
            branch_count += 1;
        }
    if (test_expect_int_equal(static_cast<int>(branch_count), 1,
            "if/else lowers to one conditional branch block") != FT_SUCCESS
        || test_expect_int_equal(branch && branch->terminator.control_region == 2
                ? 1 : 0, 1,
            "branch retains its controlling typed region") != FT_SUCCESS
        || test_expect_int_equal(branch
                ? static_cast<int>(branch->terminator.successors.size()) : 0,
            2, "if/else branch has true and false successors") != FT_SUCCESS)
        return (FT_FAILURE);
    branch->terminator.successors[0].target = branch->terminator.successors[1].target;
    if (test_expect_int_equal(verify_control_flow_graph(function, cfg,
                &diagnostic) ? 1 : 0, 0,
            "branch that bypasses its nonempty then region must be rejected")
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_cfg_edge),
            "bypassed branch body reports an invalid CFG edge") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_structured_function(region_kind::loop);
    if (test_expect_int_equal(build_structured_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 1,
            "structured loop regions should lower to a verified CFG") != FT_SUCCESS)
    {
        std::printf("Loop CFG diagnostic: %s\n", diagnostic.message.c_str());
        return (FT_FAILURE);
    }
    branch = nullptr;
    for (basic_block &block : cfg.blocks)
        if (block.terminator.kind == cfg_terminator_kind::branch)
            branch = &block;
    if (test_expect_success(branch != nullptr ? FT_SUCCESS : FT_FAILURE,
            "loop has a condition branch") != FT_SUCCESS)
        return (FT_FAILURE);
    const basic_block_id header_id = branch->id;
    const basic_block_id body_id = branch->terminator.successors[0].target;
    const basic_block *body_block = nullptr;
    for (const basic_block &block : cfg.blocks)
        if (block.id == body_id)
            body_block = &block;
    return test_expect_int_equal(body_block
            && body_block->terminator.successors.size() == 1
            && body_block->terminator.successors[0].target == header_id ? 1 : 0,
        1, "loop body back-edge returns to its condition header");
}

FT_TEST(test_backend_cfg_requires_branch_values_to_dominate)
{
    typed_function function = make_structured_function(region_kind::conditional);
    control_flow_graph cfg;
    verification_diagnostic diagnostic;

    function.regions[1].condition = 2;
    if (test_expect_int_equal(build_structured_control_flow_graph(function, &cfg,
                &diagnostic) ? 1 : 0, 0,
            "condition produced only in the then arm must not control the branch")
        != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::use_before_definition),
        "non-dominating branch condition reports use-before-definition");
}

FT_TEST(test_backend_typed_ir_class_type_ids_ignore_declaration_order)
{
    const char *first_source = "class Anchor { public: int value; };\n"
        "void main() { Anchor item; }\n";
    const char *reordered_source = "class Interloper { public: int other; };\n"
        "class Anchor { public: int value; };\n"
        "void main() { Anchor item; }\n";
    t_cblc_translation_unit first_unit;
    t_cblc_translation_unit reordered_unit;
    typed_function first_ir;
    typed_function reordered_ir;
    verification_diagnostic diagnostic;
    type_id first_anchor_id = 0;
    type_id reordered_anchor_id = 0;
    type_id interloper_id = 0;
    int status = FT_FAILURE;

    cblc_translation_unit_init(&first_unit);
    cblc_translation_unit_init(&reordered_unit);
    if (cblc_parse_translation_unit(first_source, &first_unit) != FT_SUCCESS
        || cblc_parse_translation_unit(reordered_source, &reordered_unit) != FT_SUCCESS)
    {
        std::printf("Stable type-ID fixture parse failed\n");
        goto cleanup;
    }
    if (first_unit.entry_function_index >= first_unit.function_count
        || reordered_unit.entry_function_index >= reordered_unit.function_count
        || !lower_scalar_function(&first_unit, first_unit.entry_function_index,
            &first_ir, &diagnostic)
        || !lower_scalar_function(&reordered_unit,
            reordered_unit.entry_function_index, &reordered_ir, &diagnostic))
    {
        std::printf("Stable type-ID typed lowering failed: %s\n",
            diagnostic.message.c_str());
        goto cleanup;
    }
    for (const typed_type &type : first_ir.types)
        if (type.kind == typed_type_kind::class_type
            && type.canonical_name == "Anchor")
            first_anchor_id = type.id;
    for (const typed_type &type : reordered_ir.types)
        if (type.kind == typed_type_kind::class_type)
        {
            if (type.canonical_name == "Anchor")
                reordered_anchor_id = type.id;
            else if (type.canonical_name == "Interloper")
                interloper_id = type.id;
        }
    if (test_expect_success(first_anchor_id != 0
                && first_anchor_id == reordered_anchor_id
                && reordered_anchor_id != interloper_id
                ? FT_SUCCESS : FT_FAILURE,
            "canonical class type IDs must not shift when another declaration is inserted first")
        != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    cblc_translation_unit_dispose(&first_unit);
    cblc_translation_unit_dispose(&reordered_unit);
    return status;
}

FT_TEST(test_backend_typed_ir_rejects_canonical_type_id_collision)
{
    const char *source =
        "class CollisionType296528 { public: int value; };\n"
        "class CollisionType757342 { public: int value; };\n"
        "void main() { CollisionType296528 left; "
        "CollisionType757342 right; return; }\n";
    t_cblc_translation_unit unit;
    typed_function function;
    verification_diagnostic diagnostic;

    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Type-ID collision fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const bool lowered = lower_scalar_function(&unit,
        unit.entry_function_index, &function, &diagnostic);
    const bool rejected = !lowered
        && diagnostic.code == verification_error::invalid_type_or_projection
        && diagnostic.message.find("canonical typed type ID collides")
            != std::string::npos;
    cblc_translation_unit_dispose(&unit);
    return test_expect_success(rejected ? FT_SUCCESS : FT_FAILURE,
        "distinct canonical class names with the same compact ID must be rejected, not aliased");
}

FT_TEST(test_backend_typed_ir_lowers_concrete_template_class_method)
{
    const char *source =
        "template <typename T> class Box { private: T value; public: "
        "void set(T next) { value = next; return; } };\n"
        "void main() { Box<int> box; box.set(7); return; }\n";
    t_cblc_translation_unit unit;
    typed_function method_ir;
    verification_diagnostic diagnostic;
    std::size_t owner_index = 0;
    std::size_t method_index = 0;
    bool found = false;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Concrete template method fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (; owner_index < unit.struct_type_count && !found; ++owner_index)
    {
        const t_cblc_struct_type &owner = unit.struct_types[owner_index];
        if (!owner.is_template_instantiation
            || std::strcmp(owner.template_source_name, "Box") != 0)
            continue;
        for (method_index = 0; method_index < owner.method_count; ++method_index)
            if (std::strcmp(owner.methods[method_index].source_name, "set") == 0
                && owner.methods[method_index].has_definition)
            {
                found = true;
                break;
            }
    }
    if (!found || owner_index == 0
        || !lower_scalar_method(&unit, owner_index - 1, method_index,
            &method_ir, &diagnostic))
    {
        std::printf("Concrete template method typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto concrete_owner_type = std::find_if(method_ir.types.begin(),
        method_ir.types.end(), [&](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == unit.struct_types[owner_index - 1].source_name;
        });
    const auto concrete_field = concrete_owner_type == method_ir.types.end()
        ? std::vector<typed_field>::const_iterator{}
        : std::find_if(concrete_owner_type->fields.begin(),
            concrete_owner_type->fields.end(), [](const typed_field &field)
            {
                return field.name == "value" && !field.is_dependent
                    && field.type != 0;
            });
    const bool has_concrete_field = concrete_owner_type != method_ir.types.end()
        && concrete_field != concrete_owner_type->fields.end();
    const bool has_typed_assignment = std::any_of(method_ir.operations.begin(),
        method_ir.operations.end(), [&](const typed_operation &operation)
        {
            if (operation.operation != opcode::assign || operation.places.size() != 1
                || operation.operands.size() != 1)
                return false;
            const auto destination = std::find_if(method_ir.places.begin(),
                method_ir.places.end(), [&](const typed_place &place)
                {
                    return place.id == operation.places.front();
                });
            const auto source_value = std::find_if(method_ir.values.begin(),
                method_ir.values.end(), [&](const typed_value &value)
                {
                    return value.id == operation.operands.front();
                });
            return destination != method_ir.places.end()
                && source_value != method_ir.values.end()
                && destination->type == concrete_field->type
                && source_value->type == concrete_field->type;
        });
    const bool verified = verify_typed_function(method_ir, &diagnostic);
    std::size_t primary_index = 0;
    std::size_t primary_method_index = 0;
    bool found_primary = false;
    for (; primary_index < unit.struct_type_count && !found_primary;
        ++primary_index)
    {
        const t_cblc_struct_type &candidate = unit.struct_types[primary_index];
        if (!candidate.is_template || candidate.is_template_instantiation
            || std::strcmp(candidate.source_name, "Box") != 0)
            continue;
        for (primary_method_index = 0;
            primary_method_index < candidate.method_count; ++primary_method_index)
            if (std::strcmp(candidate.methods[primary_method_index].source_name,
                    "set") == 0)
            {
                found_primary = true;
                break;
            }
    }
    typed_function rejected_primary_ir;
    const bool rejected_unspecialized_primary = found_primary
        && !lower_scalar_method(&unit, primary_index - 1, primary_method_index,
            &rejected_primary_ir, &diagnostic)
        && diagnostic.code == verification_error::unsupported_legacy_type;
    cblc_translation_unit_dispose(&unit);
    return test_expect_success(has_concrete_field && has_typed_assignment && verified
            && rejected_unspecialized_primary
            ? FT_SUCCESS : FT_FAILURE,
        "concrete template method lowers substituted fields and parameters into verified typed IR");
}

FT_TEST(test_backend_typed_ir_dispatches_class_lifecycle_in_template_method)
{
    const char *source =
        "class Payload { private: int value; public: "
        "Payload() { value = 0; } "
        "Payload(Payload source) { value = source.value; } "
        "void operator=(Payload source) { value = source.value; return; } "
        "~Payload() { value = 0; } };\n"
        "template <typename T> class Box { private: T value; public: "
        "void set(T next) { value = next; return; } };\n"
        "void main() { Box<Payload> box; Payload item; box.set(item); return; }\n";
    t_cblc_translation_unit unit;
    typed_function method_ir;
    verification_diagnostic diagnostic;
    std::size_t owner_index = 0;
    std::size_t method_index = 0;
    bool found = false;
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(source, &unit) != FT_SUCCESS)
    {
        std::printf("Class-valued template method fixture parse failed: %s\n",
            unit.parse_error_message);
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    for (; owner_index < unit.struct_type_count && !found; ++owner_index)
    {
        const t_cblc_struct_type &owner = unit.struct_types[owner_index];
        if (!owner.is_template_instantiation
            || std::strcmp(owner.template_source_name, "Box") != 0
            || std::strcmp(owner.template_argument_name, "Payload") != 0)
            continue;
        for (method_index = 0; method_index < owner.method_count; ++method_index)
            if (std::strcmp(owner.methods[method_index].source_name, "set") == 0
                && owner.methods[method_index].has_definition)
            {
                found = true;
                break;
            }
    }
    if (!found || owner_index == 0
        || !lower_scalar_method(&unit, owner_index - 1, method_index,
            &method_ir, &diagnostic))
    {
        std::printf("Class-valued template method typed lowering failed: %s\n",
            diagnostic.message.c_str());
        cblc_translation_unit_dispose(&unit);
        return FT_FAILURE;
    }
    const auto payload_type = std::find_if(method_ir.types.begin(),
        method_ir.types.end(), [](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == "Payload";
        });
    const auto box_type = std::find_if(method_ir.types.begin(),
        method_ir.types.end(), [&](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == unit.struct_types[owner_index - 1].source_name;
        });
    const auto payload_field = box_type == method_ir.types.end()
        ? std::vector<typed_field>::const_iterator{}
        : std::find_if(box_type->fields.begin(), box_type->fields.end(),
            [&](const typed_field &field)
            {
                return field.name == "value" && !field.is_dependent
                    && payload_type != method_ir.types.end()
                    && field.type == payload_type->id;
            });
    const bool has_concrete_payload_field = payload_field
        != std::vector<typed_field>::const_iterator{};
    const bool calls_payload_assignment = std::any_of(
        method_ir.operations.begin(), method_ir.operations.end(),
        [&](const typed_operation &operation)
        {
            if (operation.operation != opcode::copy_assign
                || operation.callee == 0 || operation.places.size() != 1
                || operation.operands.size() != 1)
                return false;
            const auto callable = std::find_if(method_ir.callables.begin(),
                method_ir.callables.end(), [&](const typed_callable &candidate)
                {
                    return candidate.id == operation.callee;
                });
            const auto destination = std::find_if(method_ir.places.begin(),
                method_ir.places.end(), [&](const typed_place &place)
                {
                    return place.id == operation.places.front();
                });
            const auto destination_value = destination == method_ir.places.end()
                ? method_ir.values.end()
                : std::find_if(method_ir.values.begin(), method_ir.values.end(),
                    [&](const typed_value &value)
                    {
                        return value.id == destination->base_value;
                    });
            return callable != method_ir.callables.end()
                && callable->owner_type == payload_type->id
                && destination != method_ir.places.end()
                && destination->type == payload_type->id
                && destination->storage_base_value != 0
                && destination->projections.size() >= 2
                && destination_value != method_ir.values.end()
                && destination_value->category == value_category::borrow
                && destination_value->lifetime_token != 0
                && destination->owner_lifetime_token
                    == destination_value->lifetime_token;
        });
    value_id by_value_parameter = 0;
    for (const typed_operation &operation : method_ir.operations)
        if (operation.operation == opcode::copy_assign
            && !operation.operands.empty())
            by_value_parameter = operation.operands.front();
    std::size_t parameter_destruction_count = 0;
    for (const typed_operation &operation : method_ir.operations)
        if (operation.operation == opcode::destroy
            || operation.operation == opcode::scope_exit)
            for (const operation_outcome &outcome : operation.outcomes)
                if (std::find(outcome.destroyed.begin(), outcome.destroyed.end(),
                        by_value_parameter) != outcome.destroyed.end())
                    ++parameter_destruction_count;
    bool borrowed_field_destroyed = false;
    for (const typed_operation &operation : method_ir.operations)
        if (operation.operation == opcode::construct
            || operation.operation == opcode::destroy
            || operation.operation == opcode::scope_exit)
        {
            for (value_id result : operation.results)
            {
                const auto value = std::find_if(method_ir.values.begin(),
                    method_ir.values.end(), [&](const typed_value &candidate)
                    {
                        return candidate.id == result;
                    });
                if (value != method_ir.values.end()
                    && value->category == value_category::borrow)
                    borrowed_field_destroyed = true;
            }
            for (const operation_outcome &outcome : operation.outcomes)
                for (value_id destroyed : outcome.destroyed)
                {
                    const auto value = std::find_if(method_ir.values.begin(),
                        method_ir.values.end(), [&](const typed_value &candidate)
                        {
                            return candidate.id == destroyed;
                        });
                    if (value != method_ir.values.end()
                        && value->category == value_category::borrow)
                        borrowed_field_destroyed = true;
                }
        }
    const bool verified = verify_typed_function(method_ir, &diagnostic);
    cblc_translation_unit_dispose(&unit);
    if (!(has_concrete_payload_field && calls_payload_assignment
            && by_value_parameter != 0 && parameter_destruction_count == 1
            && !borrowed_field_destroyed && verified))
    {
        std::printf("Template method lifecycle details: field=%d assignment=%d parameter=%u parameter-destroys=%zu borrowed-destroyed=%d verified=%d diagnostic=%s\n",
            has_concrete_payload_field ? 1 : 0,
            calls_payload_assignment ? 1 : 0,
            static_cast<unsigned int>(by_value_parameter),
            parameter_destruction_count,
            borrowed_field_destroyed ? 1 : 0,
            verified ? 1 : 0, diagnostic.message.c_str());
        for (const typed_operation &operation : method_ir.operations)
            for (const operation_outcome &outcome : operation.outcomes)
                if (std::find(outcome.destroyed.begin(), outcome.destroyed.end(),
                        by_value_parameter) != outcome.destroyed.end())
                    std::printf("  destruction record: operation=%u region=%u opcode=%u outcome=%u role=%s\n",
                        static_cast<unsigned int>(operation.id),
                        static_cast<unsigned int>(operation.region),
                        static_cast<unsigned int>(operation.operation),
                        static_cast<unsigned int>(outcome.kind),
                        operation.provenance.empty() ? "<none>"
                            : operation.provenance.front().role.c_str());
    }
    return test_expect_success(has_concrete_payload_field
            && calls_payload_assignment && by_value_parameter != 0
            && parameter_destruction_count == 1 && !borrowed_field_destroyed
            && verified
            ? FT_SUCCESS : FT_FAILURE,
        "generic class-valued receiver fields dispatch assignment without owning their lifetime; by-value parameters are destroyed once");
}

FT_TEST(test_backend_typed_ir_rejects_inconsistent_region_membership)
{
    typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    function.regions[0].operations.pop_back();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "operation omitted from its region should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::invalid_region_tree),
            "region membership failure has a stable category") != FT_SUCCESS)
        return (FT_FAILURE);

    function = make_valid_function();
    typed_region child;
    child.id = 2;
    child.parent = 1;
    child.kind = region_kind::lexical_scope;
    child.operations.push_back(2);
    function.regions[0].children.push_back(2);
    function.regions[0].operations.pop_back();
    function.regions[0].sequence = {{typed_region::item::kind::operation, 1},
        {typed_region::item::kind::child_region, 2}};
    child.sequence = {{typed_region::item::kind::operation, 2}};
    function.regions.push_back(child);
    function.operations[1].region = 2;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            1, "well-formed nested region tree should verify") != FT_SUCCESS)
        return (FT_FAILURE);
    function.regions[1].children.push_back(1);
    return test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
        0, "region cycle should be rejected");
}

FT_TEST(test_backend_typed_ir_requires_typed_dataflow_and_explicit_order)
{
    typed_function function = make_valid_function();
    verification_diagnostic diagnostic;
    function.operations[1].must_follow.clear();
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "consumer without a dependency edge should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(static_cast<int>(diagnostic.code),
            static_cast<int>(verification_error::use_before_definition),
            "missing def-use order has a stable diagnostic") != FT_SUCCESS)
        return (FT_FAILURE);
    function = make_valid_function();
    function.values[0].type = 5;
    if (test_expect_int_equal(verify_typed_function(function, &diagnostic) ? 1 : 0,
            0, "assignment across incompatible types should be rejected") != FT_SUCCESS)
        return (FT_FAILURE);
    return test_expect_int_equal(static_cast<int>(diagnostic.code),
        static_cast<int>(verification_error::invalid_type_or_projection),
        "type mismatch has a stable diagnostic");
}

const t_test_case *get_compiler_backend_typed_ir_tests(size_t *count);

static const t_test_case backend_typed_ir_test_cases[] = {
    {"backend_typed_ir_verifies_ordered_well_typed_operations",
        test_backend_typed_ir_verifies_ordered_well_typed_operations},
    {"backend_typed_ir_rejects_invalid_order_and_mutation",
        test_backend_typed_ir_rejects_invalid_order_and_mutation},
    {"backend_typed_ir_rejects_dangling_and_incomplete_calls",
        test_backend_typed_ir_rejects_dangling_and_incomplete_calls},
    {"backend_typed_ir_requires_well_formed_scalar_comparisons",
        test_backend_typed_ir_requires_well_formed_scalar_comparisons},
    {"backend_typed_ir_requires_complete_lifecycle_outcomes",
        test_backend_typed_ir_requires_complete_lifecycle_outcomes},
    {"backend_typed_ir_accepts_pointer_based_move_constructor_dispatch",
        test_backend_typed_ir_accepts_pointer_based_move_constructor_dispatch},
    {"backend_lifetime_rejects_live_object_at_function_exit",
        test_backend_lifetime_rejects_live_object_at_function_exit},
    {"backend_lifetime_cleanup_plan_tracks_only_completed_objects",
        test_backend_lifetime_cleanup_plan_tracks_only_completed_objects},
    {"backend_lifetime_analysis_is_independent_of_cfg_storage_order",
        test_backend_lifetime_analysis_is_independent_of_cfg_storage_order},
    {"backend_lifetime_dataflow_enforces_exact_transfer_budget",
        test_backend_lifetime_dataflow_enforces_exact_transfer_budget},
    {"backend_lifetime_cleans_completed_prefix_for_checked_access_failure",
        test_backend_lifetime_cleans_completed_prefix_for_checked_access_failure},
    {"backend_lifetime_rejects_double_destruction",
        test_backend_lifetime_rejects_double_destruction},
    {"backend_lifetime_requires_reverse_construction_order",
        test_backend_lifetime_requires_reverse_construction_order},
    {"backend_typed_ir_models_explicit_and_self_assignment",
        test_backend_typed_ir_models_explicit_and_self_assignment},
    {"backend_typed_ir_allows_copy_from_live_borrow_only",
        test_backend_typed_ir_allows_copy_from_live_borrow_only},
    {"backend_typed_ir_requires_moved_from_state_for_move_assignment",
        test_backend_typed_ir_requires_moved_from_state_for_move_assignment},
    {"backend_lifetime_rejects_branch_local_cleanup_leak",
        test_backend_lifetime_rejects_branch_local_cleanup_leak},
    {"backend_lifetime_accepts_path_dependent_state_before_scope_cleanup",
        test_backend_lifetime_accepts_path_dependent_state_before_scope_cleanup},
    {"backend_typed_ir_lowers_scalar_translation_unit_slice",
        test_backend_typed_ir_lowers_scalar_translation_unit_slice},
    {"backend_typed_ir_lowers_resolved_instance_call_with_receiver_lifetime",
        test_backend_typed_ir_lowers_resolved_instance_call_with_receiver_lifetime},
    {"backend_typed_ir_borrows_global_class_receiver_without_function_cleanup",
        test_backend_typed_ir_borrows_global_class_receiver_without_function_cleanup},
    {"backend_typed_ir_unwinds_partially_initialized_global_owned_objects",
        test_backend_typed_ir_unwinds_partially_initialized_global_owned_objects},
    {"backend_typed_ir_emits_nothrow_global_owned_lifecycle",
        test_backend_typed_ir_emits_nothrow_global_owned_lifecycle},
    {"backend_typed_ir_records_int_to_double_method_argument_conversion",
        test_backend_typed_ir_records_int_to_double_method_argument_conversion},
    {"backend_typed_ir_records_typed_direct_function_call",
        test_backend_typed_ir_records_typed_direct_function_call},
    {"backend_typed_ir_lowers_class_pointer_method_argument_as_borrow",
        test_backend_typed_ir_lowers_class_pointer_method_argument_as_borrow},
    {"backend_typed_ir_copies_and_destroys_by_value_class_argument",
        test_backend_typed_ir_copies_and_destroys_by_value_class_argument},
    {"backend_typed_ir_routes_resolved_throwing_instance_call",
        test_backend_typed_ir_routes_resolved_throwing_instance_call},
    {"backend_typed_ir_routes_resolved_throwing_direct_call",
        test_backend_typed_ir_routes_resolved_throwing_direct_call},
    {"backend_typed_ir_lowers_by_value_class_method_parameters",
        test_backend_typed_ir_lowers_by_value_class_method_parameters},
    {"backend_typed_ir_preserves_pointer_return_as_borrowed_receiver",
        test_backend_typed_ir_preserves_pointer_return_as_borrowed_receiver},
    {"backend_typed_ir_lowers_nested_member_expression_from_function",
        test_backend_typed_ir_lowers_nested_member_expression_from_function},
    {"backend_typed_ir_failed_lowering_discards_partial_output",
        test_backend_typed_ir_failed_lowering_discards_partial_output},
    {"backend_typed_ir_lowers_scalar_literal_return",
        test_backend_typed_ir_lowers_scalar_literal_return},
    {"backend_typed_ir_emits_scalar_parameter_return_from_load",
        test_backend_typed_ir_emits_scalar_parameter_return_from_load},
    {"backend_typed_ir_emits_scalar_member_return_before_parameter_cleanup",
        test_backend_typed_ir_emits_scalar_member_return_before_parameter_cleanup},
    {"backend_typed_ir_emits_compound_scalar_return_before_parameter_cleanup",
        test_backend_typed_ir_emits_compound_scalar_return_before_parameter_cleanup},
    {"backend_typed_ir_lowers_pointer_move_and_relocation_intrinsics",
        test_backend_typed_ir_lowers_pointer_move_and_relocation_intrinsics},
    {"backend_typed_ir_lowers_integer_throw_to_uncaught_cfg_exit",
        test_backend_typed_ir_lowers_integer_throw_to_uncaught_cfg_exit},
    {"backend_typed_ir_routes_integer_throw_to_typed_int_handler",
        test_backend_typed_ir_routes_integer_throw_to_typed_int_handler},
    {"backend_typed_ir_routes_unmatched_throw_to_catch_all_handler",
        test_backend_typed_ir_routes_unmatched_throw_to_catch_all_handler},
    {"backend_typed_ir_applies_throw_cleanup_before_uncaught_exit",
        test_backend_typed_ir_applies_throw_cleanup_before_uncaught_exit},
    {"backend_typed_ir_lowers_local_class_lifecycle_to_cleanup_verified_cfg",
        test_backend_typed_ir_lowers_local_class_lifecycle_to_cleanup_verified_cfg},
    {"backend_typed_ir_emits_local_class_array_lifecycle",
        test_backend_typed_ir_emits_local_class_array_lifecycle},
    {"backend_typed_ir_lowers_class_copy_assignment_to_emitter",
        test_backend_typed_ir_lowers_class_copy_assignment_to_emitter},
    {"backend_typed_ir_tracks_each_class_array_element_lifetime",
        test_backend_typed_ir_tracks_each_class_array_element_lifetime},
    {"backend_typed_ir_composes_nested_member_lifetimes",
        test_backend_typed_ir_composes_nested_member_lifetimes},
    {"backend_typed_ir_plans_cleanup_after_later_constructor_failure",
        test_backend_typed_ir_plans_cleanup_after_later_constructor_failure},
    {"backend_typed_ir_uses_resolved_constructor_throw_effect",
        test_backend_typed_ir_uses_resolved_constructor_throw_effect},
    {"backend_typed_ir_lowers_source_if_else_to_structured_cfg",
        test_backend_typed_ir_lowers_source_if_else_to_structured_cfg},
    {"backend_typed_ir_lowers_source_while_with_retested_condition",
        test_backend_typed_ir_lowers_source_while_with_retested_condition},
    {"backend_typed_ir_destroys_loop_local_on_each_back_edge",
        test_backend_typed_ir_destroys_loop_local_on_each_back_edge},
    {"backend_typed_ir_rejects_inconsistent_region_membership",
        test_backend_typed_ir_rejects_inconsistent_region_membership},
    {"backend_typed_ir_requires_typed_dataflow_and_explicit_order",
        test_backend_typed_ir_requires_typed_dataflow_and_explicit_order},
    {"backend_cfg_routes_normal_and_failure_outcomes",
        test_backend_cfg_routes_normal_and_failure_outcomes},
    {"backend_cfg_rejects_skipped_or_reordered_operations",
        test_backend_cfg_rejects_skipped_or_reordered_operations},
    {"backend_cfg_builds_structured_if_and_loop_regions",
        test_backend_cfg_builds_structured_if_and_loop_regions},
    {"backend_cfg_requires_branch_values_to_dominate",
        test_backend_cfg_requires_branch_values_to_dominate},
    {"backend_typed_ir_class_type_ids_ignore_declaration_order",
        test_backend_typed_ir_class_type_ids_ignore_declaration_order},
    {"backend_typed_ir_rejects_canonical_type_id_collision",
        test_backend_typed_ir_rejects_canonical_type_id_collision},
    {"backend_typed_ir_lowers_concrete_template_class_method",
        test_backend_typed_ir_lowers_concrete_template_class_method},
    {"backend_typed_ir_dispatches_class_lifecycle_in_template_method",
        test_backend_typed_ir_dispatches_class_lifecycle_in_template_method}
};

const t_test_case *get_compiler_backend_typed_ir_tests(size_t *count)
{
    if (count)
        *count = sizeof(backend_typed_ir_test_cases) / sizeof(backend_typed_ir_test_cases[0]);
    return backend_typed_ir_test_cases;
}
