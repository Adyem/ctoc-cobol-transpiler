#include "semantics_test_support.hpp"

void semantics_destroy_program(t_ast_node *program)
{
    if (!program)
        return ;
    ast_node_destroy(program);
}

t_ast_node *semantics_create_identifier_node(const char *name)
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

t_ast_node *semantics_create_literal_node(const char *lexeme,
    t_lexer_token_kind kind)
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

t_ast_node *semantics_create_picture_node(const char *text)
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

t_ast_node *semantics_create_comparison_operator_node(
    t_lexer_token_kind kind, const char *lexeme)
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

t_ast_node *semantics_create_arithmetic_operator_node(
    t_lexer_token_kind kind, const char *lexeme)
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

t_ast_node *semantics_create_unary_expression_node(const char *operand_name,
    t_lexer_token_kind operator_kind, const char *operator_lexeme)
{
    t_ast_node *expression;
    t_ast_node *operator_node;
    t_ast_node *operand;

    if (!operand_name)
        return (NULL);
    expression = ast_node_create(AST_NODE_UNARY_EXPRESSION);
    if (!expression)
        return (NULL);
    operator_node = semantics_create_arithmetic_operator_node(operator_kind,
            operator_lexeme);
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
    operand = semantics_create_identifier_node(operand_name);
    if (!operand)
    {
        ast_node_destroy(expression);
        return (NULL);
    }
    if (ast_node_add_child(expression, operand) != FT_SUCCESS)
    {
        ast_node_destroy(operand);
        ast_node_destroy(expression);
        return (NULL);
    }
    return (expression);
}

t_ast_node *semantics_create_arithmetic_expression_node_with_operator(
    const char *left_name, t_lexer_token_kind operator_kind,
    const char *operator_lexeme, const char *right_name)
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
    operator_node = semantics_create_arithmetic_operator_node(operator_kind,
            operator_lexeme);
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

t_ast_node *semantics_create_arithmetic_expression_node(
    const char *left_name, const char *right_name)
{
    return (semantics_create_arithmetic_expression_node_with_operator(left_name,
        LEXER_TOKEN_PLUS, "+", right_name));
}

t_ast_node *semantics_create_move_statement(const char *source_name,
    const char *target_name)
{
    t_ast_node *statement;
    t_ast_node *source_node;
    t_ast_node *target_node;

    if (!source_name)
        return (NULL);
    if (!target_name)
        return (NULL);
    statement = ast_node_create(AST_NODE_MOVE_STATEMENT);
    if (!statement)
        return (NULL);
    source_node = semantics_create_identifier_node(source_name);
    if (!source_node)
    {
        ast_node_destroy(statement);
        return (NULL);
    }
    if (ast_node_add_child(statement, source_node) != FT_SUCCESS)
    {
        ast_node_destroy(source_node);
        ast_node_destroy(statement);
        return (NULL);
    }
    target_node = semantics_create_identifier_node(target_name);
    if (!target_node)
    {
        ast_node_destroy(statement);
        return (NULL);
    }
    if (ast_node_add_child(statement, target_node) != FT_SUCCESS)
    {
        ast_node_destroy(target_node);
        ast_node_destroy(statement);
        return (NULL);
    }
    return (statement);
}

t_ast_node *semantics_create_stop_run_statement(void)
{
    t_ast_node *statement;
    t_lexer_token token;

    statement = ast_node_create(AST_NODE_STOP_STATEMENT);
    if (!statement)
        return (NULL);
    token.kind = LEXER_TOKEN_KEYWORD_STOP;
    token.lexeme = "STOP";
    token.length = 4;
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(statement, &token) != FT_SUCCESS)
    {
        ast_node_destroy(statement);
        return (NULL);
    }
    return (statement);
}

t_ast_node *semantics_build_program_with_storage_level(
    const char *storage_name, const char *picture_text,
    const char *level_text)
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
        ast_node_destroy(data_division);
        semantics_destroy_program(program);
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
        ast_node_destroy(working_storage);
        semantics_destroy_program(program);
        return (NULL);
    }
    data_item = ast_node_create(AST_NODE_DATA_ITEM);
    if (!data_item)
    {
        semantics_destroy_program(program);
        return (NULL);
    }
    if (ast_node_add_child(working_storage, data_item) != FT_SUCCESS)
    {
        ast_node_destroy(data_item);
        semantics_destroy_program(program);
        return (NULL);
    }
    if (level_text)
    {
        level_literal = semantics_create_literal_node(level_text,
                LEXER_TOKEN_NUMERIC_LITERAL);
        if (!level_literal)
        {
            semantics_destroy_program(program);
            return (NULL);
        }
        if (ast_node_add_child(data_item, level_literal) != FT_SUCCESS)
        {
            ast_node_destroy(level_literal);
            semantics_destroy_program(program);
            return (NULL);
        }
    }
    name_node = semantics_create_identifier_node(storage_name);
    if (!name_node)
    {
        semantics_destroy_program(program);
        return (NULL);
    }
    if (ast_node_add_child(data_item, name_node) != FT_SUCCESS)
    {
        ast_node_destroy(name_node);
        semantics_destroy_program(program);
        return (NULL);
    }
    if (picture_text)
    {
        t_ast_node *picture_node;

        picture_node = semantics_create_picture_node(picture_text);
        if (!picture_node)
        {
            semantics_destroy_program(program);
            return (NULL);
        }
        if (ast_node_add_child(data_item, picture_node) != FT_SUCCESS)
        {
            ast_node_destroy(picture_node);
            semantics_destroy_program(program);
            return (NULL);
        }
    }
    return (program);
}

t_ast_node *semantics_build_program_with_storage(const char *storage_name,
    const char *picture_text)
{
    return (semantics_build_program_with_storage_level(storage_name,
        picture_text, NULL));
}

int semantics_add_data_item_with_level(t_ast_node *program,
    const char *name, const char *picture_text, const char *level_text)
{
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *data_item;
    t_ast_node *level_literal;
    t_ast_node *name_node;

    if (!program)
        return (FT_FAILURE);
    if (program->child_count < 1)
        return (FT_FAILURE);
    data_division = program->children[0];
    if (data_division->child_count < 1)
        return (FT_FAILURE);
    working_storage = data_division->children[0];
    data_item = ast_node_create(AST_NODE_DATA_ITEM);
    if (!data_item)
        return (FT_FAILURE);
    if (ast_node_add_child(working_storage, data_item) != FT_SUCCESS)
    {
        ast_node_destroy(data_item);
        return (FT_FAILURE);
    }
    if (level_text)
    {
        level_literal = semantics_create_literal_node(level_text,
                LEXER_TOKEN_NUMERIC_LITERAL);
        if (!level_literal)
            return (FT_FAILURE);
        if (ast_node_add_child(data_item, level_literal) != FT_SUCCESS)
        {
            ast_node_destroy(level_literal);
            return (FT_FAILURE);
        }
    }
    name_node = semantics_create_identifier_node(name);
    if (!name_node)
        return (FT_FAILURE);
    if (ast_node_add_child(data_item, name_node) != FT_SUCCESS)
    {
        ast_node_destroy(name_node);
        return (FT_FAILURE);
    }
    if (picture_text)
    {
        t_ast_node *picture_node;

        picture_node = semantics_create_picture_node(picture_text);
        if (!picture_node)
            return (FT_FAILURE);
        if (ast_node_add_child(data_item, picture_node) != FT_SUCCESS)
        {
            ast_node_destroy(picture_node);
            return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

int semantics_add_data_item(t_ast_node *program, const char *name,
    const char *picture_text)
{
    return (semantics_add_data_item_with_level(program, name, picture_text,
        NULL));
}

int semantics_add_copybook_include(t_ast_node *program,
    const char *copybook_name)
{
    t_ast_node *data_division;
    t_ast_node *working_storage;
    t_ast_node *include_node;
    t_ast_node *name_node;

    if (!program)
        return (FT_FAILURE);
    if (program->child_count < 1)
        return (FT_FAILURE);
    data_division = program->children[0];
    if (data_division->child_count < 1)
        return (FT_FAILURE);
    working_storage = data_division->children[0];
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

int semantics_attach_procedure_with_assignment_like_node(t_ast_node *program,
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

int semantics_attach_procedure_with_move_node(t_ast_node *program,
    t_ast_node *source_node, const char *target_name)
{
    return (semantics_attach_procedure_with_assignment_like_node(program,
        source_node, target_name, AST_NODE_MOVE_STATEMENT));
}

int semantics_attach_procedure_with_assignment_node(t_ast_node *program,
    t_ast_node *source_node, const char *target_name)
{
    return (semantics_attach_procedure_with_assignment_like_node(program,
        source_node, target_name, AST_NODE_ASSIGNMENT_STATEMENT));
}

int semantics_attach_procedure_with_move(t_ast_node *program,
    const char *source_name, const char *target_name, int use_literal_source)
{
    t_ast_node *source_node;

    if (use_literal_source)
        source_node = semantics_create_literal_node("'value'",
                LEXER_TOKEN_STRING_LITERAL);
    else
        source_node = semantics_create_identifier_node(source_name);
    if (!source_node)
        return (FT_FAILURE);
    if (semantics_attach_procedure_with_move_node(program, source_node,
            target_name) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int semantics_attach_procedure_with_assignment(t_ast_node *program,
    const char *source_name, const char *target_name, int use_literal_source)
{
    t_ast_node *source_node;

    if (use_literal_source)
        source_node = semantics_create_literal_node("'value'",
                LEXER_TOKEN_STRING_LITERAL);
    else
        source_node = semantics_create_identifier_node(source_name);
    if (!source_node)
        return (FT_FAILURE);
    if (semantics_attach_procedure_with_assignment_node(program,
            source_node, target_name) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int semantics_attach_procedure_with_statements(t_ast_node *program,
    t_ast_node **statements, size_t statement_count)
{
    t_ast_node *procedure_division;
    t_ast_node *sequence;
    size_t index;

    if (!program)
        return (FT_FAILURE);
    procedure_division = ast_node_create(AST_NODE_PROCEDURE_DIVISION);
    if (!procedure_division)
        return (FT_FAILURE);
    sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(procedure_division, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, procedure_division) != FT_SUCCESS)
    {
        ast_node_destroy(procedure_division);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < statement_count)
    {
        if (statements && statements[index])
        {
            if (ast_node_add_child(sequence, statements[index]) != FT_SUCCESS)
            {
                ast_node_destroy(statements[index]);
                return (FT_FAILURE);
            }
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

int semantics_attach_procedure_with_if_comparison(t_ast_node *program,
    const char *left_name, t_lexer_token_kind operator_kind,
    const char *operator_lexeme, const char *right_name)
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
    operator_node = semantics_create_comparison_operator_node(operator_kind,
            operator_lexeme);
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
        ast_node_destroy(condition);
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
