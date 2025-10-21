#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

static void ast_node_reset_token(t_ast_node *node)
{
    if (!node)
        return ;
    node->token.kind = LEXER_TOKEN_UNKNOWN;
    node->token.lexeme = NULL;
    node->token.length = 0;
    node->token.line = 0;
    node->token.column = 0;
}

static int ast_node_reserve_children(t_ast_node *node, size_t desired_capacity)
{
    t_ast_node **new_children;
    size_t index;

    if (!node)
        return (FT_FAILURE);
    if (node->child_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 1)
        desired_capacity = 1;
    new_children = static_cast<t_ast_node **>(cma_calloc(desired_capacity, sizeof(t_ast_node *)));
    if (!new_children)
        return (FT_FAILURE);
    index = 0;
    while (index < node->child_count)
    {
        new_children[index] = node->children[index];
        index += 1;
    }
    if (node->children)
        cma_free(node->children);
    node->children = new_children;
    node->child_capacity = desired_capacity;
    return (FT_SUCCESS);
}

int ast_node_init(t_ast_node *node, t_ast_node_kind kind)
{
    if (!node)
        return (FT_FAILURE);
    node->kind = kind;
    node->lexeme_storage = NULL;
    node->children = NULL;
    node->child_count = 0;
    node->child_capacity = 0;
    node->flags = 0;
    ast_node_reset_token(node);
    return (FT_SUCCESS);
}

static void ast_node_release_lexeme(t_ast_node *node)
{
    if (!node)
        return ;
    if (node->lexeme_storage)
        cma_free(node->lexeme_storage);
    node->lexeme_storage = NULL;
    ast_node_reset_token(node);
}

static void ast_node_release_children(t_ast_node *node)
{
    size_t index;

    if (!node)
        return ;
    if (!node->children)
        return ;
    index = 0;
    while (index < node->child_count)
    {
        if (node->children[index])
            ast_node_destroy(node->children[index]);
        index += 1;
    }
    cma_free(node->children);
    node->children = NULL;
    node->child_count = 0;
    node->child_capacity = 0;
}

void ast_node_dispose(t_ast_node *node)
{
    if (!node)
        return ;
    ast_node_release_children(node);
    ast_node_release_lexeme(node);
}

t_ast_node *ast_node_create(t_ast_node_kind kind)
{
    t_ast_node *node;

    node = static_cast<t_ast_node *>(cma_calloc(1, sizeof(t_ast_node)));
    if (!node)
        return (NULL);
    if (ast_node_init(node, kind) != FT_SUCCESS)
    {
        cma_free(node);
        return (NULL);
    }
    return (node);
}

void ast_node_destroy(t_ast_node *node)
{
    if (!node)
        return ;
    ast_node_dispose(node);
    cma_free(node);
}

int ast_node_set_token(t_ast_node *node, const t_lexer_token *token)
{
    size_t index;

    if (!node)
        return (FT_FAILURE);
    ast_node_release_lexeme(node);
    if (!token)
        return (FT_SUCCESS);
    node->token.kind = token->kind;
    node->token.line = token->line;
    node->token.column = token->column;
    if (!token->lexeme || token->length == 0)
    {
        node->token.lexeme = NULL;
        node->token.length = 0;
        return (FT_SUCCESS);
    }
    node->lexeme_storage = static_cast<char *>(cma_calloc(token->length + 1, sizeof(char)));
    if (!node->lexeme_storage)
    {
        ast_node_reset_token(node);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < token->length)
    {
        node->lexeme_storage[index] = token->lexeme[index];
        index += 1;
    }
    node->lexeme_storage[token->length] = '\0';
    node->token.lexeme = node->lexeme_storage;
    node->token.length = token->length;
    return (FT_SUCCESS);
}

int ast_node_add_child(t_ast_node *parent, t_ast_node *child)
{
    if (!parent || !child)
        return (FT_FAILURE);
    if (parent->child_count >= parent->child_capacity)
    {
        if (ast_node_reserve_children(parent, parent->child_capacity == 0 ? 4 : parent->child_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    parent->children[parent->child_count] = child;
    parent->child_count += 1;
    return (FT_SUCCESS);
}

size_t ast_node_child_count(const t_ast_node *node)
{
    if (!node)
        return (0);
    return (node->child_count);
}

t_ast_node *ast_node_get_child(const t_ast_node *node, size_t index)
{
    if (!node)
        return (NULL);
    if (index >= node->child_count)
        return (NULL);
    return (node->children[index]);
}
