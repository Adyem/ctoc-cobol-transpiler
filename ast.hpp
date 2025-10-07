#ifndef AST_HPP
#define AST_HPP

#include <cstddef>

#include "lexer_token.hpp"

typedef enum e_ast_node_kind
{
    AST_NODE_UNKNOWN = 0,
    AST_NODE_PROGRAM,
    AST_NODE_IDENTIFICATION_DIVISION,
    AST_NODE_ENVIRONMENT_DIVISION,
    AST_NODE_DATA_DIVISION,
    AST_NODE_PROCEDURE_DIVISION,
    AST_NODE_PROGRAM_ID,
    AST_NODE_WORKING_STORAGE_SECTION,
    AST_NODE_DATA_ITEM,
    AST_NODE_STATEMENT_SEQUENCE,
    AST_NODE_PARAGRAPH,
    AST_NODE_MOVE_STATEMENT,
    AST_NODE_IF_STATEMENT,
    AST_NODE_PERFORM_UNTIL_STATEMENT,
    AST_NODE_PERFORM_VARYING_STATEMENT,
    AST_NODE_STOP_STATEMENT,
    AST_NODE_CONDITION,
    AST_NODE_COMPARISON_OPERATOR,
    AST_NODE_IDENTIFIER,
    AST_NODE_LITERAL
}   t_ast_node_kind;

typedef struct s_ast_node
{
    t_ast_node_kind kind;
    t_lexer_token token;
    char *lexeme_storage;
    struct s_ast_node **children;
    size_t child_count;
    size_t child_capacity;
}   t_ast_node;

int ast_node_init(t_ast_node *node, t_ast_node_kind kind);
void ast_node_dispose(t_ast_node *node);

t_ast_node *ast_node_create(t_ast_node_kind kind);
void ast_node_destroy(t_ast_node *node);

int ast_node_set_token(t_ast_node *node, const t_lexer_token *token);
int ast_node_add_child(t_ast_node *parent, t_ast_node *child);

size_t ast_node_child_count(const t_ast_node *node);
t_ast_node *ast_node_get_child(const t_ast_node *node, size_t index);

#endif
