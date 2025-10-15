#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

FT_TEST(test_ast_node_add_child_preserves_order)
{
    t_ast_node *program;
    t_ast_node *division;
    t_ast_node *sequence;
    int status;

    program = ast_node_create(AST_NODE_PROGRAM);
    if (!program)
        return (FT_FAILURE);
    division = ast_node_create(AST_NODE_IDENTIFICATION_DIVISION);
    if (!division)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, division) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    status = FT_SUCCESS;
    if (ast_node_child_count(program) != 2)
        status = FT_FAILURE;
    else if (ast_node_get_child(program, 0) != division)
        status = FT_FAILURE;
    else if (ast_node_get_child(program, 1) != sequence)
        status = FT_FAILURE;
    else if (ast_node_get_child(program, 1)->kind != AST_NODE_STATEMENT_SEQUENCE)
        status = FT_FAILURE;
    ast_node_destroy(program);
    return (status);
}

FT_TEST(test_ast_node_set_token_copies_lexeme)
{
    t_ast_node *node;
    t_lexer_token token;
    char buffer[16];
    const char *source;
    size_t length;
    size_t index;

    node = ast_node_create(AST_NODE_IDENTIFIER);
    if (!node)
        return (FT_FAILURE);
    source = "program";
    length = 0;
    while (source[length] != '\0')
    {
        buffer[length] = source[length];
        length += 1;
    }
    buffer[length] = '\0';
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = buffer;
    token.length = length;
    token.line = 4;
    token.column = 2;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    buffer[0] = 'X';
    if (!node->token.lexeme)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.kind != LEXER_TOKEN_IDENTIFIER)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.length != length)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.lexeme[0] != 'p')
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < node->token.length)
    {
        if (node->token.lexeme[index] != source[index])
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (node->token.lexeme[node->token.length] != '\0')
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.lexeme != NULL)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.length != 0)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.kind != LEXER_TOKEN_UNKNOWN)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    ast_node_destroy(node);
    return (FT_SUCCESS);
}

const t_test_case *get_ast_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"ast_node_add_child_preserves_order", test_ast_node_add_child_preserves_order},
        {"ast_node_set_token_copies_lexeme", test_ast_node_set_token_copies_lexeme}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
