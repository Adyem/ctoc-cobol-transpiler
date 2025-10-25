#include "cblc_transpiler.hpp"

#include <cstdio>

#include "test_suites.hpp"

static void copybook_graph_remove_file(const char *path)
{
    if (!path)
        return ;
    std::remove(path);
}

FT_TEST(test_copybook_graph_emits_dependencies)
{
    const char *output_path;
    t_ast_node *program;
    t_ast_node *include_node;
    t_ast_node *identifier;
    t_lexer_token token;
    t_runtime_file file;
    char buffer[512];
    size_t total;
    size_t bytes_read;
    int status;
    t_transpiler_context context;

    output_path = "copybook_graph_test_output.dot";
    program = ast_node_create(AST_NODE_PROGRAM);
    if (!program)
        return (FT_FAILURE);
    include_node = ast_node_create(AST_NODE_COPYBOOK_INCLUDE);
    if (!include_node)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    identifier = ast_node_create(AST_NODE_IDENTIFIER);
    if (!identifier)
    {
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = "CUSTOM-COPY";
    token.length = ft_strlen("CUSTOM-COPY");
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(identifier, &token) != FT_SUCCESS)
    {
        ast_node_destroy(identifier);
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(include_node, identifier) != FT_SUCCESS)
    {
        ast_node_destroy(identifier);
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, include_node) != FT_SUCCESS)
    {
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    status = transpiler_copybook_graph_emit(&context, program, "sample.cob", output_path);
    transpiler_context_dispose(&context);
    ast_node_destroy(program);
    if (status != FT_SUCCESS)
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (runtime_file_open_read(&file, output_path) != FT_SUCCESS)
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    total = 0;
    while (1)
    {
        if (total + 1 >= sizeof(buffer))
        {
            runtime_file_close(&file);
            copybook_graph_remove_file(output_path);
            return (FT_FAILURE);
        }
        if (runtime_file_read(&file, buffer + total, sizeof(buffer) - total - 1,
                &bytes_read) != FT_SUCCESS)
        {
            runtime_file_close(&file);
            copybook_graph_remove_file(output_path);
            return (FT_FAILURE);
        }
        if (bytes_read == 0)
            break ;
        total += bytes_read;
    }
    buffer[total] = '\0';
    if (runtime_file_close(&file) != FT_SUCCESS)
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "digraph Copybooks", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "root [label=\"sample.cob\"]", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "copybook_0 [label=\"CUSTOM-COPY\"]", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "root -> copybook_0", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    copybook_graph_remove_file(output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_copybook_graph_handles_cycles)
{
    const char *output_path;
    t_transpiler_context context;
    const char *alpha_dependencies[1];
    const char *beta_dependencies[1];
    t_ast_node *program;
    t_ast_node *include_node;
    t_ast_node *identifier;
    t_lexer_token token;
    t_runtime_file file;
    char buffer[512];
    size_t total;
    size_t bytes_read;
    int status;

    output_path = "copybook_graph_cycle_output.dot";
    if (transpiler_context_init(&context) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_context_register_copybook(&context, "ALPHA", NULL, 0) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (transpiler_context_register_copybook(&context, "BETA", NULL, 0) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    alpha_dependencies[0] = "./beta";
    if (transpiler_context_register_copybook_dependencies(&context, "ALPHA", alpha_dependencies, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    beta_dependencies[0] = "././alpha";
    if (transpiler_context_register_copybook_dependencies(&context, "BETA", beta_dependencies, 1) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    program = ast_node_create(AST_NODE_PROGRAM);
    if (!program)
    {
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    include_node = ast_node_create(AST_NODE_COPYBOOK_INCLUDE);
    if (!include_node)
    {
        ast_node_destroy(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    identifier = ast_node_create(AST_NODE_IDENTIFIER);
    if (!identifier)
    {
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = "alpha";
    token.length = ft_strlen("alpha");
    token.line = 1;
    token.column = 1;
    if (ast_node_set_token(identifier, &token) != FT_SUCCESS)
    {
        ast_node_destroy(identifier);
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(include_node, identifier) != FT_SUCCESS)
    {
        ast_node_destroy(identifier);
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, include_node) != FT_SUCCESS)
    {
        ast_node_destroy(include_node);
        ast_node_destroy(program);
        transpiler_context_dispose(&context);
        return (FT_FAILURE);
    }
    status = transpiler_copybook_graph_emit(&context, program, "cycle_root.cob", output_path);
    transpiler_context_dispose(&context);
    ast_node_destroy(program);
    if (status != FT_SUCCESS)
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (runtime_file_open_read(&file, output_path) != FT_SUCCESS)
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    total = 0;
    while (1)
    {
        if (total + 1 >= sizeof(buffer))
        {
            runtime_file_close(&file);
            copybook_graph_remove_file(output_path);
            return (FT_FAILURE);
        }
        if (runtime_file_read(&file, buffer + total, sizeof(buffer) - total - 1,
                &bytes_read) != FT_SUCCESS)
        {
            runtime_file_close(&file);
            copybook_graph_remove_file(output_path);
            return (FT_FAILURE);
        }
        if (bytes_read == 0)
            break ;
        total += bytes_read;
    }
    buffer[total] = '\0';
    if (runtime_file_close(&file) != FT_SUCCESS)
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "copybook_0 [label=\"ALPHA\"]", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "copybook_1 [label=\"BETA\"]", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "copybook_0 -> copybook_1", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "copybook_1 -> copybook_0", total))
    {
        copybook_graph_remove_file(output_path);
        return (FT_FAILURE);
    }
    copybook_graph_remove_file(output_path);
    return (FT_SUCCESS);
}
