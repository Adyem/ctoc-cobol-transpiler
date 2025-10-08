#include "transpiler_semantics.hpp"

#include "libft/Libft/libft.hpp"

#include "test_suites.hpp"

static void semantics_destroy_program(t_ast_node *program)
{
    if (!program)
        return ;
    ast_node_destroy(program);
}

static t_ast_node *semantics_create_identifier_node(const char *name)
{
    t_ast_node *node;
    t_lexer_token token;

    node = ast_node_create(AST_NODE_IDENTIFIER);
    if (!node)
        return (NULL);
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = name;
    token.length = ft_strlen(name);
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (NULL);
    }
    return (node);
}

static t_ast_node *semantics_create_literal_node(const char *lexeme, t_lexer_token_kind kind)
{
    t_ast_node *node;
    t_lexer_token token;

    node = ast_node_create(AST_NODE_LITERAL);
    if (!node)
        return (NULL);
    token.kind = kind;
    token.lexeme = lexeme;
    token.length = ft_strlen(lexeme);
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (NULL);
    }
    return (node);
}

static t_ast_node *semantics_create_picture_node(const char *text)
{
    t_ast_node *node;
    t_lexer_token token;

    if (!text)
        return (NULL);
    node = ast_node_create(AST_NODE_PICTURE_CLAUSE);
    if (!node)
        return (NULL);
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = text;
    token.length = ft_strlen(text);
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (NULL);
    }
    return (node);
}

static t_ast_node *semantics_build_program_with_storage(const char *storage_name, const char *picture_text)
{
    t_ast_node *program;
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *data_item;
    t_ast_node *level_literal;
    t_ast_node *name_node;

    program = ast_node_create(AST_NODE_PROGRAM);
    if (!program)
        return (NULL);
    data_division = ast_node_create(AST_NODE_DATA_DIVISION);
    if (!data_division)
    {
        semantics_destroy_program(program);
        return (NULL);
    }
    if (ast_node_add_child(program, data_division) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        ast_node_destroy(data_division);
        return (NULL);
    }
    working_storage = ast_node_create(AST_NODE_WORKING_STORAGE_SECTION);
    if (!working_storage)
    {
        semantics_destroy_program(program);
        return (NULL);
    }
    if (ast_node_add_child(data_division, working_storage) != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        ast_node_destroy(working_storage);
        return (NULL);
    }
    data_item = ast_node_create(AST_NODE_DATA_ITEM);
    if (!data_item)
    {
        semantics_destroy_program(program);
        return (NULL);
    }
    level_literal = semantics_create_literal_node("01", LEXER_TOKEN_NUMERIC_LITERAL);
    if (!level_literal)
    {
        ast_node_destroy(data_item);
        semantics_destroy_program(program);
        return (NULL);
    }
    if (ast_node_add_child(data_item, level_literal) != FT_SUCCESS)
    {
        ast_node_destroy(level_literal);
        ast_node_destroy(data_item);
        semantics_destroy_program(program);
        return (NULL);
    }
    name_node = semantics_create_identifier_node(storage_name);
    if (!name_node)
    {
        ast_node_destroy(data_item);
        semantics_destroy_program(program);
        return (NULL);
    }
    if (ast_node_add_child(data_item, name_node) != FT_SUCCESS)
    {
        ast_node_destroy(name_node);
        ast_node_destroy(data_item);
        semantics_destroy_program(program);
        return (NULL);
    }
    if (picture_text)
    {
        t_ast_node *picture_node;

        picture_node = semantics_create_picture_node(picture_text);
        if (!picture_node)
        {
            ast_node_destroy(data_item);
            semantics_destroy_program(program);
            return (NULL);
        }
        if (ast_node_add_child(data_item, picture_node) != FT_SUCCESS)
        {
            ast_node_destroy(picture_node);
            ast_node_destroy(data_item);
            semantics_destroy_program(program);
            return (NULL);
        }
    }
    if (ast_node_add_child(working_storage, data_item) != FT_SUCCESS)
    {
        ast_node_destroy(data_item);
        semantics_destroy_program(program);
        return (NULL);
    }
    return (program);
}

static int semantics_add_data_item(t_ast_node *program, const char *name, const char *picture_text)
{
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *data_item;
    t_ast_node *level_literal;
    t_ast_node *name_node;
    size_t index;

    if (!program)
        return (FT_FAILURE);
    data_division = NULL;
    index = 0;
    while (index < ast_node_child_count(program))
    {
        t_ast_node *candidate;

        candidate = ast_node_get_child(program, index);
        if (candidate && candidate->kind == AST_NODE_DATA_DIVISION)
        {
            data_division = candidate;
            break ;
        }
        index += 1;
    }
    if (!data_division)
        return (FT_FAILURE);
    working_storage = NULL;
    index = 0;
    while (index < ast_node_child_count(data_division))
    {
        t_ast_node *candidate;

        candidate = ast_node_get_child(data_division, index);
        if (candidate && candidate->kind == AST_NODE_WORKING_STORAGE_SECTION)
        {
            working_storage = candidate;
            break ;
        }
        index += 1;
    }
    if (!working_storage)
        return (FT_FAILURE);
    data_item = ast_node_create(AST_NODE_DATA_ITEM);
    if (!data_item)
        return (FT_FAILURE);
    level_literal = semantics_create_literal_node("01", LEXER_TOKEN_NUMERIC_LITERAL);
    if (!level_literal)
    {
        ast_node_destroy(data_item);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(data_item, level_literal) != FT_SUCCESS)
    {
        ast_node_destroy(level_literal);
        ast_node_destroy(data_item);
        return (FT_FAILURE);
    }
    name_node = semantics_create_identifier_node(name);
    if (!name_node)
    {
        ast_node_destroy(data_item);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(data_item, name_node) != FT_SUCCESS)
    {
        ast_node_destroy(name_node);
        ast_node_destroy(data_item);
        return (FT_FAILURE);
    }
    if (picture_text)
    {
        t_ast_node *picture_node;

        picture_node = semantics_create_picture_node(picture_text);
        if (!picture_node)
        {
            ast_node_destroy(data_item);
            return (FT_FAILURE);
        }
        if (ast_node_add_child(data_item, picture_node) != FT_SUCCESS)
        {
            ast_node_destroy(picture_node);
            ast_node_destroy(data_item);
            return (FT_FAILURE);
        }
    }
    if (ast_node_add_child(working_storage, data_item) != FT_SUCCESS)
    {
        ast_node_destroy(data_item);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int semantics_attach_procedure_with_move(t_ast_node *program, const char *source_name, const char *target_name,
    int use_literal_source)
{
    t_ast_node *procedure_division;
    t_ast_node *sequence;
    t_ast_node *move_statement;
    t_ast_node *source_node;
    t_ast_node *target_node;

    if (!program)
        return (FT_FAILURE);
    procedure_division = ast_node_create(AST_NODE_PROCEDURE_DIVISION);
    if (!procedure_division)
        return (FT_FAILURE);
    if (ast_node_add_child(program, procedure_division) != FT_SUCCESS)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
        return (FT_FAILURE);
    if (ast_node_add_child(procedure_division, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        return (FT_FAILURE);
    }
    move_statement = ast_node_create(AST_NODE_MOVE_STATEMENT);
    if (!move_statement)
        return (FT_FAILURE);
    if (use_literal_source)
    {
        source_node = semantics_create_literal_node("'value'", LEXER_TOKEN_STRING_LITERAL);
    }
    else
    {
        source_node = semantics_create_identifier_node(source_name);
    }
    if (!source_node)
    {
        ast_node_destroy(move_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(move_statement, source_node) != FT_SUCCESS)
    {
        ast_node_destroy(source_node);
        ast_node_destroy(move_statement);
        return (FT_FAILURE);
    }
    target_node = semantics_create_identifier_node(target_name);
    if (!target_node)
    {
        ast_node_destroy(move_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(move_statement, target_node) != FT_SUCCESS)
    {
        ast_node_destroy(target_node);
        ast_node_destroy(move_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(sequence, move_statement) != FT_SUCCESS)
    {
        ast_node_destroy(move_statement);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

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
                && numeric_item->declared_length == 0)
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

FT_TEST(test_semantics_rejects_truncating_identifier_move)
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

const t_test_case *get_semantics_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"semantics_accepts_declared_move", test_semantics_accepts_declared_move},
        {"semantics_rejects_undeclared_identifier", test_semantics_rejects_undeclared_identifier},
        {"semantics_detects_duplicate_data_item", test_semantics_detects_duplicate_data_item},
        {"semantics_records_alphanumeric_length_in_context", test_semantics_records_alphanumeric_length_in_context},
        {"semantics_rejects_type_mismatch_move", test_semantics_rejects_type_mismatch_move},
        {"semantics_rejects_truncating_identifier_move", test_semantics_rejects_truncating_identifier_move},
        {"semantics_rejects_truncating_literal_move", test_semantics_rejects_truncating_literal_move}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
