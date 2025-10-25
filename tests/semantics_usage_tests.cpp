#include "libft/Libft/libft.hpp"

#include "semantics_test_support.hpp"
#include "transpiler_semantics_internal.hpp"

#include "semantics_test_groups.hpp"

static size_t semantics_context_warning_count(const t_transpiler_context *context)
{
    size_t index;
    size_t total;

    total = 0;
    if (!context)
        return (0);
    index = 0;
    while (index < context->diagnostics.count)
    {
        if (context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_WARNING)
            total += 1;
        index += 1;
    }
    return (total);
}

static int semantics_context_contains_warning(const t_transpiler_context *context,
    int code)
{
    size_t index;

    if (!context)
        return (0);
    index = 0;
    while (index < context->diagnostics.count)
    {
        if (context->diagnostics.items[index].code == code
            && context->diagnostics.items[index].severity == TRANSPILE_SEVERITY_WARNING)
            return (1);
        index += 1;
    }
    return (0);
}

static int semantics_attach_use_after_error_declaratives(t_ast_node *program,
    const char *section_name, const char **file_names, size_t file_count,
    t_ast_node **out_procedure_division)
{
    t_ast_node *procedure_division;
    t_ast_node *declaratives;
    t_ast_node *section;
    t_ast_node *use_node;
    t_lexer_token token;
    size_t index;

    if (!program)
        return (FT_FAILURE);
    if (!section_name)
        return (FT_FAILURE);
    if (file_count == 0)
        return (FT_FAILURE);
    if (!file_names)
        return (FT_FAILURE);
    procedure_division = ast_node_create(AST_NODE_PROCEDURE_DIVISION);
    if (!procedure_division)
        return (FT_FAILURE);
    declaratives = ast_node_create(AST_NODE_DECLARATIVES);
    if (!declaratives)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(procedure_division, declaratives) != FT_SUCCESS)
    {
        ast_node_destroy(declaratives);
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    section = ast_node_create(AST_NODE_DECLARATIVE_SECTION);
    if (!section)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = section_name;
    token.length = ft_strlen(section_name);
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(section, &token) != FT_SUCCESS)
    {
        ast_node_destroy(section);
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(declaratives, section) != FT_SUCCESS)
    {
        ast_node_destroy(section);
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    use_node = ast_node_create(AST_NODE_USE_AFTER_ERROR_PROCEDURE);
    if (!use_node)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    token.kind = LEXER_TOKEN_KEYWORD_USE;
    token.lexeme = "USE";
    token.length = ft_strlen(token.lexeme);
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(use_node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(use_node);
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(section, use_node) != FT_SUCCESS)
    {
        ast_node_destroy(use_node);
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < file_count)
    {
        t_ast_node *identifier;

        identifier = semantics_create_identifier_node(file_names[index]);
        if (!identifier)
        {
            ast_node_destroy(procedure_division);
            return (FT_FAILURE);
        }
        if (ast_node_add_child(use_node, identifier) != FT_SUCCESS)
        {
            ast_node_destroy(identifier);
            ast_node_destroy(procedure_division);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (ast_node_add_child(program, procedure_division) != FT_SUCCESS)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    if (out_procedure_division)
        *out_procedure_division = procedure_division;
    return (FT_SUCCESS);
}

static int semantics_attach_empty_main_paragraph(t_ast_node *procedure_division)
{
    t_ast_node *paragraph;
    t_ast_node *sequence;
    t_lexer_token token;

    if (!procedure_division)
        return (FT_FAILURE);
    paragraph = ast_node_create(AST_NODE_PARAGRAPH);
    if (!paragraph)
        return (FT_FAILURE);
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = "MAIN";
    token.length = ft_strlen(token.lexeme);
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(paragraph, &token) != FT_SUCCESS)
    {
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
    {
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(paragraph, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(procedure_division, paragraph) != FT_SUCCESS)
    {
        ast_node_destroy(paragraph);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_semantics_warns_on_unused_data_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("UNUSED", "PIC 9(4).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_warning_count(&context) >= 1)
            {
                if (semantics_context_contains_warning(&context,
                        TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM))
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_write_only_data_item)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("TARGET", "PIC X(8).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move(program, NULL, "TARGET", 1) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_warning_count(&context) >= 1)
            {
                if (semantics_context_contains_warning(&context,
                        TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM))
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_warns_on_read_without_write)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SOURCE", "PIC 9(4).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "SOURCE",
            LEXER_TOKEN_EQUALS, "=", "SOURCE") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            if (semantics_context_warning_count(&context) >= 1)
            {
                if (semantics_context_contains_warning(&context,
                        TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE))
                    status = FT_SUCCESS;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_registers_use_after_error_binding)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *procedure_division;
    const t_transpiler_use_after_error_binding *bindings;
    size_t binding_count;
    const char *file_names[1];
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("DUMMY", "PIC X(1).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_file(&context, "INPUT-FILE", TRANSPILE_FILE_ROLE_INPUT,
            "input.dat", 0) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    file_names[0] = "INPUT-FILE";
    if (semantics_attach_use_after_error_declaratives(program, "ERR-SECTION",
            file_names, 1, &procedure_division) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_empty_main_paragraph(procedure_division) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) == FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 0)
        {
            bindings = transpiler_context_get_use_after_error_bindings(&context,
                &binding_count);
            if (bindings && binding_count == 1)
            {
                if (ft_strncmp(bindings[0].section_name, "ERR-SECTION",
                        TRANSPILE_IDENTIFIER_MAX) == 0)
                {
                    if (ft_strncmp(bindings[0].file_name, "INPUT-FILE",
                            TRANSPILE_IDENTIFIER_MAX) == 0)
                        status = FT_SUCCESS;
                }
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_reports_missing_use_after_error_file)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *procedure_division;
    const char *file_names[1];
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("DUMMY", "PIC X(1).");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    file_names[0] = "MISSING-FILE";
    if (semantics_attach_use_after_error_declaratives(program, "ERR-SECTION",
            file_names, 1, &procedure_division) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_empty_main_paragraph(procedure_division) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context))
        {
            size_t index;

            index = 0;
            while (index < context.diagnostics.count)
            {
                if (context.diagnostics.items[index].code == TRANSPILE_ERROR_FILE_UNKNOWN)
                {
                    status = FT_SUCCESS;
                    break ;
                }
                index += 1;
            }
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_collects_occurs_metadata)
{
    t_transpiler_context context;
    t_transpiler_semantic_scope scope;
    t_ast_node *program;
    t_ast_node *table_item;
    const t_transpiler_semantic_data_item *registered;
    const t_transpiler_data_item *context_item;
    int result;
    int status;

    result = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_semantics_scope_init(&scope);
    program = NULL;
    table_item = NULL;
    program = semantics_build_program_with_storage_level("ENTRY-COUNT",
            "PIC 9(4)", "01");
    if (!program)
        goto cleanup;
    if (semantics_add_data_item_with_level(program, "TABLE-FIELD",
            "PIC X(2)", "01") != FT_SUCCESS)
        goto cleanup;
    table_item = semantics_find_data_item(program, "TABLE-FIELD");
    if (!table_item)
        goto cleanup;
    if (semantics_attach_occurs_clause(table_item, "1", "10", "ENTRY-COUNT")
        != FT_SUCCESS)
        goto cleanup;
    status = transpiler_semantics_collect_scope(program, &scope, &context);
    if (test_expect_int_equal(status, FT_SUCCESS,
            "scope collection should succeed for OCCURS metadata") != FT_SUCCESS)
        goto cleanup;
    registered = transpiler_semantics_scope_lookup(&scope, "TABLE-FIELD");
    if (!registered)
    {
        pf_printf("Assertion failed: OCCURS data item missing from scope\n");
        goto cleanup;
    }
    if (test_expect_int_equal(registered->occurs.present, 1,
            "scope should record OCCURS presence") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(registered->occurs.minimum), 1,
            "scope should track OCCURS minimum") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(registered->occurs.maximum), 10,
            "scope should track OCCURS maximum") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(registered->occurs.has_depending_on, 1,
            "scope should record DEPENDING ON metadata") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(registered->occurs.depending_on,
            "ENTRY-COUNT",
            "scope should capture OCCURS controller name") != FT_SUCCESS)
        goto cleanup;
    context_item = transpiler_context_find_data_item(&context, "TABLE-FIELD");
    if (!context_item)
    {
        pf_printf("Assertion failed: OCCURS data item missing from context registry\n");
        goto cleanup;
    }
    if (test_expect_int_equal(context_item->occurs.present, 1,
            "context should record OCCURS presence") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(context_item->occurs.minimum), 1,
            "context should track OCCURS minimum") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(static_cast<int>(context_item->occurs.maximum), 10,
            "context should track OCCURS maximum") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(context_item->occurs.has_depending_on, 1,
            "context should record OCCURS controller flag") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_cstring_equal(context_item->occurs.depending_on,
            "ENTRY-COUNT",
            "context should capture OCCURS controller name") != FT_SUCCESS)
        goto cleanup;
    result = FT_SUCCESS;
cleanup:
    semantics_destroy_program(program);
    transpiler_semantics_scope_dispose(&scope);
    transpiler_context_dispose(&context);
    return (result);
}

FT_TEST(test_semantics_reports_missing_occurs_controller)
{
    t_transpiler_context context;
    t_transpiler_semantic_scope scope;
    t_ast_node *program;
    t_ast_node *table_item;
    int result;
    int status;

    result = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_semantics_scope_init(&scope);
    program = NULL;
    table_item = NULL;
    program = semantics_build_program_with_storage("TABLE-FIELD",
            "PIC X(2)");
    if (!program)
        goto cleanup;
    table_item = semantics_find_data_item(program, "TABLE-FIELD");
    if (!table_item)
        goto cleanup;
    if (semantics_attach_occurs_clause(table_item, "1", "5", "MISSING-COUNT")
        != FT_SUCCESS)
        goto cleanup;
    status = transpiler_semantics_collect_scope(program, &scope, &context);
    if (test_expect_int_equal(status, FT_FAILURE,
            "scope collection should fail for missing OCCURS controller")
        != FT_SUCCESS)
        goto cleanup;
    if (test_expect_int_equal(transpiler_context_has_errors(&context), 1,
            "missing OCCURS controller should set error flag") != FT_SUCCESS)
        goto cleanup;
    if (context.diagnostics.count > 0)
    {
        if (test_expect_int_equal(context.diagnostics.items[0].code,
                TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE,
                "missing OCCURS controller should emit invalid-move diagnostic")
            != FT_SUCCESS)
            goto cleanup;
    }
    result = FT_SUCCESS;
cleanup:
    semantics_destroy_program(program);
    transpiler_semantics_scope_dispose(&scope);
    transpiler_context_dispose(&context);
    return (result);
}

const t_test_case *get_semantics_usage_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_warns_on_unused_data_item", test_semantics_warns_on_unused_data_item},
        {"semantics_warns_on_write_only_data_item", test_semantics_warns_on_write_only_data_item},
        {"semantics_warns_on_read_without_write", test_semantics_warns_on_read_without_write},
        {"semantics_registers_use_after_error_binding", test_semantics_registers_use_after_error_binding},
        {"semantics_reports_missing_use_after_error_file", test_semantics_reports_missing_use_after_error_file},
        {"semantics_collects_occurs_metadata", test_semantics_collects_occurs_metadata},
        {"semantics_reports_missing_occurs_controller", test_semantics_reports_missing_occurs_controller}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
