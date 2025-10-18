#include "libft/Libft/libft.hpp"

#include "semantics_test_support.hpp"

#include "semantics_test_groups.hpp"

static int semantics_context_has_unreachable_warning(const t_transpiler_context *context)
{
    size_t index;

    if (!context)
        return (0);
    index = 0;
    while (index < context->diagnostics.count)
    {
        if (context->diagnostics.items[index].code == TRANSPILE_WARNING_SEMANTIC_UNREACHABLE_CODE
            && context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_WARNING)
            return (1);
        index += 1;
    }
    return (0);
}

FT_TEST(test_semantics_warns_on_unreachable_after_stop)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *statements[2];
    int status;

    status = FT_FAILURE;
    statements[0] = NULL;
    statements[1] = NULL;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TARGET", "PIC 9(4).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "SOURCE", "PIC 9(4).") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    statements[0] = semantics_create_stop_run_statement();
    statements[1] = semantics_create_move_statement("SOURCE", "TARGET");
    if (!statements[0] || !statements[1])
    {
        if (statements[0])
            ast_node_destroy(statements[0]);
        if (statements[1])
            ast_node_destroy(statements[1]);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_statements(program, statements, 2) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_has_unreachable_warning(&context))
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_unreachable_if_branch)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *if_statement;
    t_ast_node *condition;
    t_ast_node *left_literal;
    t_ast_node *operator_node;
    t_ast_node *right_literal;
    t_ast_node *then_sequence;
    t_ast_node *move_statement;
    t_ast_node *statements[1];
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TARGET", "PIC 9(4).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "SOURCE", "PIC 9(4).") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if_statement = ast_node_create(AST_NODE_IF_STATEMENT);
    if (!if_statement)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    condition = ast_node_create(AST_NODE_CONDITION);
    if (!condition)
    {
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    left_literal = semantics_create_literal_node("1", LEXER_TOKEN_NUMERIC_LITERAL);
    if (!left_literal)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(condition, left_literal) != FT_SUCCESS)
    {
        ast_node_destroy(left_literal);
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    operator_node = semantics_create_comparison_operator_node(LEXER_TOKEN_EQUALS, "=");
    if (!operator_node)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(condition, operator_node) != FT_SUCCESS)
    {
        ast_node_destroy(operator_node);
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    right_literal = semantics_create_literal_node("0", LEXER_TOKEN_NUMERIC_LITERAL);
    if (!right_literal)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(condition, right_literal) != FT_SUCCESS)
    {
        ast_node_destroy(right_literal);
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(if_statement, condition) != FT_SUCCESS)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    then_sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!then_sequence)
    {
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(if_statement, then_sequence) != FT_SUCCESS)
    {
        ast_node_destroy(then_sequence);
        ast_node_destroy(if_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    move_statement = semantics_create_move_statement("SOURCE", "TARGET");
    if (!move_statement)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(then_sequence, move_statement) != FT_SUCCESS)
    {
        ast_node_destroy(move_statement);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    statements[0] = if_statement;
    if (semantics_attach_procedure_with_statements(program, statements, 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_has_unreachable_warning(&context))
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

const t_test_case *get_semantics_control_flow_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_warns_on_unreachable_after_stop", test_semantics_warns_on_unreachable_after_stop},
        {"semantics_warns_on_unreachable_if_branch", test_semantics_warns_on_unreachable_if_branch}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
