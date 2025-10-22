#include "libft/Libft/libft.hpp"

#include "semantics_test_support.hpp"

#include "semantics_test_groups.hpp"

FT_TEST(test_semantics_accepts_declared_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TARGET", NULL);
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "SOURCE", "TARGET", 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_undeclared_identifier)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("PRIMARY", NULL);
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "SOURCE", "MISSING", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1 &&
                context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER &&
                context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_detects_duplicate_data_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *duplicate_item;
    t_ast_node *duplicate_level;
    t_ast_node *duplicate_name;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SHARED", NULL);
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    data_division = ast_node_get_child(program, 0);
    if (!data_division)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    working_storage = ast_node_get_child(data_division, 0);
    if (!working_storage)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    duplicate_item = ast_node_create(AST_NODE_DATA_ITEM);
    if (!duplicate_item)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    duplicate_level = semantics_create_literal_node("01", LEXER_TOKEN_NUMERIC_LITERAL);
    if (!duplicate_level)
    {
        ast_node_destroy(duplicate_item);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(duplicate_item, duplicate_level) != FT_SUCCESS)
    {
        ast_node_destroy(duplicate_level);
        ast_node_destroy(duplicate_item);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    duplicate_name = semantics_create_identifier_node("SHARED");
    if (!duplicate_name)
    {
        ast_node_destroy(duplicate_item);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(duplicate_item, duplicate_name) != FT_SUCCESS)
    {
        ast_node_destroy(duplicate_name);
        ast_node_destroy(duplicate_item);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(working_storage, duplicate_item) != FT_SUCCESS)
    {
        ast_node_destroy(duplicate_item);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1 &&
                context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM &&
                context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_registers_copybook_items)
{
    t_transpiler_context context;
    t_transpiler_copybook_item copy_items[1];
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(copy_items, sizeof(copy_items));
    ft_strlcpy(copy_items[0].name, "COPY-TARGET", sizeof(copy_items[0].name));
    copy_items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    copy_items[0].declared_length = 8;
    copy_items[0].is_read_only = 0;
    if (transpiler_context_register_copybook(&context, "CUSTOM-COPY", copy_items, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    program = semantics_build_program_with_storage("LOCAL-SOURCE", "PIC X(8)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_copybook_include(program, "CUSTOM-COPY") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "LOCAL-SOURCE", "COPY-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS
        && transpiler_context_has_errors(&context) == 0)
        status = FT_SUCCESS;
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_reports_unknown_copybook)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LOCAL-SOURCE", "PIC X(8)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_copybook_include(program, "MISSING-COPY") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1
            && context.diagnostics.count >= 1
            && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_UNKNOWN_COPYBOOK)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_detects_copybook_duplicate_data_item)
{
    t_transpiler_context context;
    t_transpiler_copybook_item copy_items[1];
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(copy_items, sizeof(copy_items));
    ft_strlcpy(copy_items[0].name, "SHARED", sizeof(copy_items[0].name));
    copy_items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    copy_items[0].declared_length = 4;
    copy_items[0].is_read_only = 0;
    if (transpiler_context_register_copybook(&context, "SHARED-COPY", copy_items, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    program = semantics_build_program_with_storage("SHARED", "PIC X(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_copybook_include(program, "SHARED-COPY") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1
            && context.diagnostics.count >= 1
            && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_skips_duplicate_copybook_expansion)
{
    t_transpiler_context context;
    t_transpiler_copybook_item copy_items[1];
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_bzero(copy_items, sizeof(copy_items));
    ft_strlcpy(copy_items[0].name, "CHAINED-FIELD", sizeof(copy_items[0].name));
    copy_items[0].kind = TRANSPILE_DATA_ITEM_ALPHANUMERIC;
    copy_items[0].declared_length = 8;
    copy_items[0].is_read_only = 0;
    if (transpiler_context_register_copybook(&context, "CHAINED-COPY", copy_items, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    program = semantics_build_program_with_storage("LOCAL-SOURCE", "PIC X(8)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_copybook_include(program, "CHAINED-COPY") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_copybook_include(program, "CHAINED-COPY") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "CHAINED-FIELD", "LOCAL-SOURCE", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS
        && transpiler_context_has_errors(&context) == 0)
    {
        size_t diagnostic_index;
        int found_warning;

        diagnostic_index = 0;
        found_warning = 0;
        while (diagnostic_index < context.diagnostics.count)
        {
            if (context.diagnostics.items[diagnostic_index].code == TRANSPILE_WARNING_SEMANTIC_DUPLICATE_COPYBOOK_INCLUDE)
                found_warning = 1;
            diagnostic_index += 1;
        }
        if (found_warning == 1)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_move_into_read_only_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage_level("CONST-TARGET", "PIC X(5)", "78");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "IGNORED", "CONST-TARGET", 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1
            && context.last_error_code == TRANSPILE_ERROR_SEMANTIC_IMMUTABLE_TARGET)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_assignment_into_read_only_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage_level("CONST-TARGET", "PIC X(5)", "78");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment(program, "IGNORED", "CONST-TARGET", 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1
            && context.last_error_code == TRANSPILE_ERROR_SEMANTIC_IMMUTABLE_TARGET)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_allows_move_from_read_only_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage_level("CONST-SOURCE", "PIC X(5)", "78");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "TARGET-FIELD", "PIC X(10)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "CONST-SOURCE", "TARGET-FIELD", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_records_alphanumeric_length_in_context)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("ALPHA-FIELD", "PIC X(5)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "COMBINED-FIELD", "PIC X(2)X(3)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-FIELD", "PIC 9(3)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        const t_transpiler_data_item *alpha_item;
        const t_transpiler_data_item *combined_item;
        const t_transpiler_data_item *numeric_item;

        alpha_item = transpiler_context_find_data_item(&context, "ALPHA-FIELD");
        combined_item = transpiler_context_find_data_item(&context, "COMBINED-FIELD");
        numeric_item = transpiler_context_find_data_item(&context, "NUMERIC-FIELD");
        if (alpha_item && combined_item && numeric_item)
        {
            if (alpha_item->kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && alpha_item->declared_length == 5
                && combined_item->kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC
                && combined_item->declared_length == 5
                && numeric_item->kind == TRANSPILE_DATA_ITEM_NUMERIC
                && numeric_item->declared_length == 3)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_type_mismatch_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-TARGET", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-SOURCE", "PIC X(8)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "ALPHA-SOURCE", "NUMERIC-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_widening_numeric_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("WIDE-TARGET", "PIC 9(9)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NARROW-SOURCE", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "NARROW-SOURCE", "WIDE-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_numeric_overflow_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NARROW-TARGET", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "WIDE-SOURCE", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "WIDE-SOURCE", "NARROW-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_NUMERIC_OVERFLOW
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_floating_to_numeric_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-TARGET", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "NUMERIC-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_FLOATING_TRUNCATION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_matching_floating_scale_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(2)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "FLOAT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_floating_scale_widening_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 9V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "FLOAT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
            status = FT_SUCCESS;
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_float_to_double_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("DOUBLE-TARGET", "PIC 9V9(18)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "DOUBLE-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_FLOAT_TO_DOUBLE
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_integral_to_floating_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "INT-SOURCE", "PIC 9(9)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "INT-SOURCE", "FLOAT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_INTEGRAL_TO_FLOATING
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_integral_to_alphanumeric_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TEXT-TARGET", "PIC X(8)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "INT-SOURCE", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "INT-SOURCE", "TEXT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 2
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_INTEGRAL_TO_ALPHANUMERIC
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING
                && context.diagnostics.items[1].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[1].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_boolean_to_numeric_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-TARGET", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "STATUS-FLAG", "PIC X") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "STATUS-FLAG",
            "NUMERIC-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 2
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_BOOLEAN_TO_NUMERIC
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING
                && context.diagnostics.items[1].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[1].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_numeric_to_boolean_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("RESULT-FLAG", "PIC X");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-SOURCE", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "NUMERIC-SOURCE",
            "RESULT-FLAG", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 2
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_NUMERIC_TO_BOOLEAN
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING
                && context.diagnostics.items[1].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[1].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_boolean_to_alphanumeric_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TEXT-TARGET", "PIC X(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "CONTROL-FLAG", "PIC X") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "CONTROL-FLAG",
            "TEXT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 2
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_BOOLEAN_TO_ALPHANUMERIC
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING
                && context.diagnostics.items[1].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[1].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_alphanumeric_to_boolean_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("STATE-FLAG", "PIC X");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "TEXT-SOURCE", "PIC X(8)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "TEXT-SOURCE",
            "STATE-FLAG", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 2
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_ALPHANUMERIC_TO_BOOLEAN
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING
                && context.diagnostics.items[1].code == TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH
                && context.diagnostics.items[1].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_suppresses_conversion_warnings_when_disabled)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_transpiler_warning_settings settings;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    settings.conversion = 0;
    settings.overflow = 1;
    settings.string_truncation = 1;
    settings.shadow = 1;
    settings.unused = 1;
    transpiler_context_set_warning_settings(&context, &settings);
    program = semantics_build_program_with_storage("DOUBLE-TARGET", "PIC 9V9(18)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "DOUBLE-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (context.diagnostics.count == 0)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_floating_scale_mismatch_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(1)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 9V9(3)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "FLOAT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_DECIMAL_SCALE_MISMATCH
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_double_to_float_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "DOUBLE-SOURCE", "PIC 999999999999999V999") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "DOUBLE-SOURCE", "FLOAT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 2
                && context.diagnostics.items[0].code == TRANSPILE_WARNING_SEMANTIC_DOUBLE_TO_FLOAT
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_WARNING
                && context.diagnostics.items[1].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_floating_integer_overflow_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-TARGET", "PIC 9V9(2)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-SOURCE", "PIC 99V9(2)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "FLOAT-SOURCE", "FLOAT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_NUMERIC_OVERFLOW
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_truncating_identifier_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    const char *source_text;
    size_t target_length;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    source_text = "MOVE LONG-SOURCE TO SHORT-TARGET.";
    target_length = ft_strlen("SHORT-TARGET");
    context.source_path = "test.cob";
    context.active_source_text = source_text;
    context.active_source_length = ft_strlen(source_text);
    program = semantics_build_program_with_storage("SHORT-TARGET", "PIC X(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LONG-SOURCE", "PIC X(8)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "LONG-SOURCE", "SHORT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
            {
                if (test_expect_cstring_equal(context.diagnostics.items[0].snippet,
                        source_text, "string truncation diagnostic should include source line") == FT_SUCCESS
                    && test_expect_cstring_equal(context.diagnostics.items[0].suggestion,
                        "Increase the target length or truncate the source explicitly.",
                        "string truncation diagnostic should include suggestion") == FT_SUCCESS
                    && test_expect_cstring_equal(context.diagnostics.items[0].span.path,
                        context.source_path, "string truncation diagnostic should include path") == FT_SUCCESS)
                {
                    if (test_expect_size_t_equal(context.diagnostics.items[0].span.start_line, 1,
                            "string truncation diagnostic should start on line 1") == FT_SUCCESS
                        && test_expect_size_t_equal(context.diagnostics.items[0].span.start_column, 1,
                            "string truncation diagnostic should start at column 1") == FT_SUCCESS
                        && test_expect_size_t_equal(context.diagnostics.items[0].span.end_column, target_length,
                            "string truncation diagnostic should span the target name") == FT_SUCCESS)
                        status = FT_SUCCESS;
                }
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_truncating_identifier_assignment)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SHORT-TARGET", "PIC X(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LONG-SOURCE", "PIC X(8)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment(program, "LONG-SOURCE", "SHORT-TARGET", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_truncating_literal_move)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SHORT-LITERAL", "PIC X(3)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, "IGNORED", "SHORT-LITERAL", 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

const t_test_case *get_semantics_assignment_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_accepts_declared_move", test_semantics_accepts_declared_move},
        {"semantics_rejects_undeclared_identifier", test_semantics_rejects_undeclared_identifier},
        {"semantics_detects_duplicate_data_item", test_semantics_detects_duplicate_data_item},
        {"semantics_registers_copybook_items", test_semantics_registers_copybook_items},
        {"semantics_reports_unknown_copybook", test_semantics_reports_unknown_copybook},
        {"semantics_detects_copybook_duplicate_data_item", test_semantics_detects_copybook_duplicate_data_item},
        {"semantics_skips_duplicate_copybook_expansion", test_semantics_skips_duplicate_copybook_expansion},
        {"semantics_rejects_move_into_read_only_item", test_semantics_rejects_move_into_read_only_item},
        {"semantics_rejects_assignment_into_read_only_item", test_semantics_rejects_assignment_into_read_only_item},
        {"semantics_allows_move_from_read_only_item", test_semantics_allows_move_from_read_only_item},
        {"semantics_records_alphanumeric_length_in_context", test_semantics_records_alphanumeric_length_in_context},
        {"semantics_rejects_type_mismatch_move", test_semantics_rejects_type_mismatch_move},
        {"semantics_accepts_widening_numeric_move", test_semantics_accepts_widening_numeric_move},
        {"semantics_rejects_numeric_overflow_move", test_semantics_rejects_numeric_overflow_move},
        {"semantics_rejects_floating_to_numeric_move", test_semantics_rejects_floating_to_numeric_move},
        {"semantics_accepts_matching_floating_scale_move", test_semantics_accepts_matching_floating_scale_move},
        {"semantics_accepts_floating_scale_widening_move", test_semantics_accepts_floating_scale_widening_move},
        {"semantics_warns_on_float_to_double_assignment", test_semantics_warns_on_float_to_double_assignment},
        {"semantics_rejects_floating_scale_mismatch_move", test_semantics_rejects_floating_scale_mismatch_move},
        {"semantics_rejects_floating_integer_overflow_move", test_semantics_rejects_floating_integer_overflow_move},
        {"semantics_warns_on_double_to_float_assignment", test_semantics_warns_on_double_to_float_assignment},
        {"semantics_rejects_truncating_identifier_move", test_semantics_rejects_truncating_identifier_move},
        {"semantics_rejects_truncating_identifier_assignment", test_semantics_rejects_truncating_identifier_assignment},
        {"semantics_rejects_truncating_literal_move", test_semantics_rejects_truncating_literal_move}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}

