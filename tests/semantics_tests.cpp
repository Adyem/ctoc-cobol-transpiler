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

static t_ast_node *semantics_create_comparison_operator_node(t_lexer_token_kind kind,
    const char *lexeme)
{
    t_ast_node *node;
    t_lexer_token token;

    node = ast_node_create(AST_NODE_COMPARISON_OPERATOR);
    if (!node)
        return (NULL);
    token.kind = kind;
    token.lexeme = lexeme;
    token.length = lexeme ? ft_strlen(lexeme) : 0;
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (NULL);
    }
    return (node);
}

static t_ast_node *semantics_create_arithmetic_operator_node(t_lexer_token_kind kind,
    const char *lexeme)
{
    t_ast_node *node;
    t_lexer_token token;

    node = ast_node_create(AST_NODE_ARITHMETIC_OPERATOR);
    if (!node)
        return (NULL);
    token.kind = kind;
    token.lexeme = lexeme;
    token.length = lexeme ? ft_strlen(lexeme) : 0;
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (NULL);
    }
    return (node);
}

static t_ast_node *semantics_create_arithmetic_expression_node(const char *left_name,
    const char *right_name)
{
    t_ast_node *expression;
    t_ast_node *left_operand;
    t_ast_node *operator_node;
    t_ast_node *right_operand;

    if (!left_name || !right_name)
        return (NULL);
    expression = ast_node_create(AST_NODE_ARITHMETIC_EXPRESSION);
    if (!expression)
        return (NULL);
    left_operand = semantics_create_identifier_node(left_name);
    if (!left_operand)
    {
        ast_node_destroy(expression);
        return (NULL);
    }
    if (ast_node_add_child(expression, left_operand) != FT_SUCCESS)
    {
        ast_node_destroy(left_operand);
        ast_node_destroy(expression);
        return (NULL);
    }
    operator_node = semantics_create_arithmetic_operator_node(LEXER_TOKEN_PLUS, "+");
    if (!operator_node)
    {
        ast_node_destroy(expression);
        return (NULL);
    }
    if (ast_node_add_child(expression, operator_node) != FT_SUCCESS)
    {
        ast_node_destroy(operator_node);
        ast_node_destroy(expression);
        return (NULL);
    }
    right_operand = semantics_create_identifier_node(right_name);
    if (!right_operand)
    {
        ast_node_destroy(expression);
        return (NULL);
    }
    if (ast_node_add_child(expression, right_operand) != FT_SUCCESS)
    {
        ast_node_destroy(right_operand);
        ast_node_destroy(expression);
        return (NULL);
    }
    return (expression);
}

static t_ast_node *semantics_build_program_with_storage_level(const char *storage_name,
    const char *picture_text, const char *level_text)
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
    if (!level_text)
        level_text = "01";
    level_literal = semantics_create_literal_node(level_text, LEXER_TOKEN_NUMERIC_LITERAL);
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

static t_ast_node *semantics_build_program_with_storage(const char *storage_name, const char *picture_text)
{
    return (semantics_build_program_with_storage_level(storage_name, picture_text, NULL));
}

static int semantics_add_data_item_with_level(t_ast_node *program, const char *name,
    const char *picture_text, const char *level_text)
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
    if (!level_text)
        level_text = "01";
    level_literal = semantics_create_literal_node(level_text, LEXER_TOKEN_NUMERIC_LITERAL);
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

static int semantics_add_data_item(t_ast_node *program, const char *name, const char *picture_text)
{
    return (semantics_add_data_item_with_level(program, name, picture_text, NULL));
}

static int semantics_add_copybook_include(t_ast_node *program, const char *copybook_name)
{
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *include_node;
    t_ast_node *name_node;

    if (!program)
        return (FT_FAILURE);
    data_division = ast_node_get_child(program, 0);
    if (!data_division)
        return (FT_FAILURE);
    working_storage = ast_node_get_child(data_division, 0);
    if (!working_storage)
        return (FT_FAILURE);
    include_node = ast_node_create(AST_NODE_COPYBOOK_INCLUDE);
    if (!include_node)
        return (FT_FAILURE);
    name_node = semantics_create_identifier_node(copybook_name);
    if (!name_node)
    {
        ast_node_destroy(include_node);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(include_node, name_node) != FT_SUCCESS)
    {
        ast_node_destroy(name_node);
        ast_node_destroy(include_node);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(working_storage, include_node) != FT_SUCCESS)
    {
        ast_node_destroy(include_node);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int semantics_attach_procedure_with_assignment_like_node(t_ast_node *program,
    t_ast_node *source_node, const char *target_name, t_ast_node_kind statement_kind)
{
    t_ast_node *procedure_division;
    t_ast_node *sequence;
    t_ast_node *statement;
    t_ast_node *target_node;

    if (!program)
        return (FT_FAILURE);
    if (!source_node)
        return (FT_FAILURE);
    procedure_division = ast_node_create(AST_NODE_PROCEDURE_DIVISION);
    if (!procedure_division)
    {
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, procedure_division) != FT_SUCCESS)
    {
        ast_node_destroy(procedure_division);
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
    {
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(procedure_division, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    statement = ast_node_create(statement_kind);
    if (!statement)
    {
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(statement, source_node) != FT_SUCCESS)
    {
        ast_node_destroy(source_node);
        ast_node_destroy(statement);
        return (FT_FAILURE);
    }
    target_node = semantics_create_identifier_node(target_name);
    if (!target_node)
    {
        ast_node_destroy(statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(statement, target_node) != FT_SUCCESS)
    {
        ast_node_destroy(target_node);
        ast_node_destroy(statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(sequence, statement) != FT_SUCCESS)
    {
        ast_node_destroy(statement);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int semantics_attach_procedure_with_move_node(t_ast_node *program, t_ast_node *source_node,
    const char *target_name)
{
    return (semantics_attach_procedure_with_assignment_like_node(program, source_node,
            target_name, AST_NODE_MOVE_STATEMENT));
}

static int semantics_attach_procedure_with_assignment_node(t_ast_node *program,
    t_ast_node *source_node, const char *target_name)
{
    return (semantics_attach_procedure_with_assignment_like_node(program, source_node,
            target_name, AST_NODE_ASSIGNMENT_STATEMENT));
}

static int semantics_attach_procedure_with_move(t_ast_node *program, const char *source_name, const char *target_name,
    int use_literal_source)
{
    t_ast_node *source_node;

    if (!program)
        return (FT_FAILURE);
    if (use_literal_source)
        source_node = semantics_create_literal_node("'value'", LEXER_TOKEN_STRING_LITERAL);
    else
        source_node = semantics_create_identifier_node(source_name);
    if (!source_node)
        return (FT_FAILURE);
    if (semantics_attach_procedure_with_move_node(program, source_node, target_name) != FT_SUCCESS)
    {
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int semantics_attach_procedure_with_assignment(t_ast_node *program, const char *source_name,
    const char *target_name, int use_literal_source)
{
    t_ast_node *source_node;

    if (!program)
        return (FT_FAILURE);
    if (use_literal_source)
        source_node = semantics_create_literal_node("'value'", LEXER_TOKEN_STRING_LITERAL);
    else
        source_node = semantics_create_identifier_node(source_name);
    if (!source_node)
        return (FT_FAILURE);
    if (semantics_attach_procedure_with_assignment_node(program, source_node, target_name) != FT_SUCCESS)
    {
        ast_node_destroy(source_node);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int semantics_attach_procedure_with_if_comparison(t_ast_node *program,
    const char *left_name, t_lexer_token_kind operator_kind, const char *operator_lexeme,
    const char *right_name)
{
    t_ast_node *procedure_division;
    t_ast_node *sequence;
    t_ast_node *if_statement;
    t_ast_node *condition;
    t_ast_node *left_operand;
    t_ast_node *operator_node;
    t_ast_node *right_operand;
    t_ast_node *then_sequence;

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
    if_statement = ast_node_create(AST_NODE_IF_STATEMENT);
    if (!if_statement)
        return (FT_FAILURE);
    condition = ast_node_create(AST_NODE_CONDITION);
    if (!condition)
    {
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    left_operand = semantics_create_identifier_node(left_name);
    if (!left_operand)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(condition, left_operand) != FT_SUCCESS)
    {
        ast_node_destroy(left_operand);
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    operator_node = semantics_create_comparison_operator_node(operator_kind, operator_lexeme);
    if (!operator_node)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(condition, operator_node) != FT_SUCCESS)
    {
        ast_node_destroy(operator_node);
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    right_operand = semantics_create_identifier_node(right_name);
    if (!right_operand)
    {
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(condition, right_operand) != FT_SUCCESS)
    {
        ast_node_destroy(right_operand);
        ast_node_destroy(condition);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(if_statement, condition) != FT_SUCCESS)
    {
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    then_sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!then_sequence)
    {
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(if_statement, then_sequence) != FT_SUCCESS)
    {
        ast_node_destroy(then_sequence);
        ast_node_destroy(if_statement);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(sequence, if_statement) != FT_SUCCESS)
    {
        ast_node_destroy(if_statement);
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

FT_TEST(test_semantics_accepts_numeric_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SUM-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("LEFT-TERM", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "SUM-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
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

FT_TEST(test_semantics_accepts_numeric_assignment_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("SUM-TARGET", "PIC 9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "LEFT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("LEFT-TERM", "RIGHT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_assignment_node(program, expression, "SUM-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
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

FT_TEST(test_semantics_accepts_floating_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
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
    if (semantics_add_data_item(program, "LEFT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-FLOAT", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("LEFT-FLOAT", "RIGHT-FLOAT");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
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

FT_TEST(test_semantics_accepts_mixed_floating_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("FLOAT-SUM", "PIC 9V9(6)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("FLOAT-TERM", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-SUM") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
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

FT_TEST(test_semantics_rejects_alphanumeric_move_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
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
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "NUMERIC-TERM", "PIC 9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("ALPHA-TERM", "NUMERIC-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "NUMERIC-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_rejects_alphanumeric_floating_addition)
{
    t_transpiler_context context;
    t_ast_node *program;
    t_ast_node *expression;
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
    if (semantics_add_data_item(program, "ALPHA-TERM", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "FLOAT-TERM", "PIC 9V9(4)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    expression = semantics_create_arithmetic_expression_node("ALPHA-TERM", "FLOAT-TERM");
    if (!expression)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_move_node(program, expression, "FLOAT-TARGET") != FT_SUCCESS)
    {
        ast_node_destroy(expression);
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_semantics_analyze_program(&context, program) != FT_SUCCESS)
    {
        if (transpiler_context_has_errors(&context) == 1)
        {
            if (context.diagnostics.count >= 1
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION
                && context.diagnostics.items[0].severity == TRANSPILE_SEVERITY_ERROR)
                status = FT_SUCCESS;
        }
    }
    semantics_destroy_program(program);
    transpiler_context_dispose(&context);
    return (status);
}

FT_TEST(test_semantics_accepts_numeric_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-NUMERIC", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-NUMERIC", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-NUMERIC",
            LEXER_TOKEN_EQUALS, "==", "RIGHT-NUMERIC") != FT_SUCCESS)
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

FT_TEST(test_semantics_rejects_mixed_condition_equality)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("NUMERIC-FIELD", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "ALPHA-FIELD", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "NUMERIC-FIELD",
            LEXER_TOKEN_EQUALS, "==", "ALPHA-FIELD") != FT_SUCCESS)
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

FT_TEST(test_semantics_accepts_numeric_condition_less_than)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-NUMERIC", "PIC 9(4)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-NUMERIC", "PIC 9(6)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-NUMERIC",
            LEXER_TOKEN_LESS_THAN, "<", "RIGHT-NUMERIC") != FT_SUCCESS)
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

FT_TEST(test_semantics_rejects_alphanumeric_condition_less_than)
{
    t_transpiler_context context;
    t_ast_node *program;
    int status;

    status = FT_FAILURE;
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    program = semantics_build_program_with_storage("LEFT-ALPHA", "PIC X(10)");
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_add_data_item(program, "RIGHT-ALPHA", "PIC X(5)") != FT_SUCCESS)
    {
        semantics_destroy_program(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (semantics_attach_procedure_with_if_comparison(program, "LEFT-ALPHA",
            LEXER_TOKEN_LESS_THAN, "<", "RIGHT-ALPHA") != FT_SUCCESS)
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
                && context.diagnostics.items[0].code == TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION
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
        {"semantics_rejects_move_into_read_only_item", test_semantics_rejects_move_into_read_only_item},
        {"semantics_rejects_assignment_into_read_only_item", test_semantics_rejects_assignment_into_read_only_item},
        {"semantics_allows_move_from_read_only_item", test_semantics_allows_move_from_read_only_item},
        {"semantics_records_alphanumeric_length_in_context", test_semantics_records_alphanumeric_length_in_context},
        {"semantics_rejects_type_mismatch_move", test_semantics_rejects_type_mismatch_move},
        {"semantics_rejects_truncating_identifier_move", test_semantics_rejects_truncating_identifier_move},
        {"semantics_rejects_truncating_identifier_assignment", test_semantics_rejects_truncating_identifier_assignment},
        {"semantics_rejects_truncating_literal_move", test_semantics_rejects_truncating_literal_move},
        {"semantics_accepts_numeric_move_addition", test_semantics_accepts_numeric_move_addition},
        {"semantics_accepts_numeric_assignment_addition", test_semantics_accepts_numeric_assignment_addition},
        {"semantics_rejects_alphanumeric_move_addition", test_semantics_rejects_alphanumeric_move_addition},
        {"semantics_accepts_numeric_condition_equality", test_semantics_accepts_numeric_condition_equality},
        {"semantics_rejects_mixed_condition_equality", test_semantics_rejects_mixed_condition_equality},
        {"semantics_accepts_numeric_condition_less_than", test_semantics_accepts_numeric_condition_less_than},
        {"semantics_rejects_alphanumeric_condition_less_than", test_semantics_rejects_alphanumeric_condition_less_than},
        {"semantics_registers_copybook_items", test_semantics_registers_copybook_items},
        {"semantics_reports_unknown_copybook", test_semantics_reports_unknown_copybook},
        {"semantics_detects_copybook_duplicate_data_item", test_semantics_detects_copybook_duplicate_data_item}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
