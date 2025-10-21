#include "cblc_transpiler.hpp"

#include <cstdio>

#include "test_suites.hpp"

static void ast_visualizer_remove_file(const char *path)
{
    if (!path)
        return ;
    std::remove(path);
}

FT_TEST(test_ast_visualizer_emits_graphviz_with_tokens)
{
    const char *output_path;
    t_ast_node *program;
    t_ast_node *identifier;
    t_ast_node *literal;
    t_lexer_token token;
    t_runtime_file file;
    char buffer[512];
    size_t total;
    size_t bytes_read;
    int status;

    output_path = "ast_visualizer_test_output.dot";
    program = ast_node_create(AST_NODE_PROGRAM);
    if (!program)
        return (FT_FAILURE);
    identifier = ast_node_create(AST_NODE_IDENTIFIER);
    if (!identifier)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = "VALUE";
    token.length = 5;
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(identifier, &token) != FT_SUCCESS)
    {
        ast_node_destroy(identifier);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, identifier) != FT_SUCCESS)
    {
        ast_node_destroy(identifier);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    literal = ast_node_create(AST_NODE_LITERAL);
    if (!literal)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    token.kind = LEXER_TOKEN_NUMERIC_LITERAL;
    token.lexeme = "42";
    token.length = 2;
    token.line = 2;
    token.column = 4;
    if (ast_node_set_token(literal, &token) != FT_SUCCESS)
    {
        ast_node_destroy(literal);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(identifier, literal) != FT_SUCCESS)
    {
        ast_node_destroy(literal);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    status = transpiler_ast_visualize_program(program, output_path);
    ast_node_destroy(program);
    if (status != FT_SUCCESS)
    {
        ast_visualizer_remove_file(output_path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (runtime_file_open_read(&file, output_path) != FT_SUCCESS)
    {
        ast_visualizer_remove_file(output_path);
        return (FT_FAILURE);
    }
    total = 0;
    while (1)
    {
        if (total + 1 >= sizeof(buffer))
        {
            runtime_file_close(&file);
            ast_visualizer_remove_file(output_path);
            return (FT_FAILURE);
        }
        if (runtime_file_read(&file, buffer + total, sizeof(buffer) - total - 1,
                &bytes_read) != FT_SUCCESS)
        {
            runtime_file_close(&file);
            ast_visualizer_remove_file(output_path);
            return (FT_FAILURE);
        }
        if (bytes_read == 0)
            break ;
        total += bytes_read;
    }
    buffer[total] = '\0';
    if (runtime_file_close(&file) != FT_SUCCESS)
    {
        ast_visualizer_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "digraph AST", total))
    {
        ast_visualizer_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "Identifier\\nVALUE", total))
    {
        ast_visualizer_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "Literal\\n42", total))
    {
        ast_visualizer_remove_file(output_path);
        return (FT_FAILURE);
    }
    ast_visualizer_remove_file(output_path);
    return (FT_SUCCESS);
}
