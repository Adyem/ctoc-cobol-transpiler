#ifndef TRANSPILER_COBOL_PROCEDURE_HPP
#define TRANSPILER_COBOL_PROCEDURE_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"

typedef enum e_transpiler_cobol_comparison_operator
{
    TRANSPILE_COBOL_COMPARISON_EQUALS = 0,
    TRANSPILE_COBOL_COMPARISON_NOT_EQUALS,
    TRANSPILE_COBOL_COMPARISON_LESS_THAN,
    TRANSPILE_COBOL_COMPARISON_LESS_OR_EQUAL,
    TRANSPILE_COBOL_COMPARISON_GREATER_THAN,
    TRANSPILE_COBOL_COMPARISON_GREATER_OR_EQUAL
}   t_transpiler_cobol_comparison_operator;

typedef struct s_transpiler_cobol_condition
{
    const char *left;
    const char *right;
    t_transpiler_cobol_comparison_operator op;
    int negated;
}   t_transpiler_cobol_condition;

typedef struct s_transpiler_cobol_statement t_transpiler_cobol_statement;

typedef struct s_transpiler_cobol_statement_block
{
    t_transpiler_cobol_statement **statements;
    size_t count;
    size_t capacity;
}   t_transpiler_cobol_statement_block;

typedef enum e_transpiler_cobol_statement_kind
{
    TRANSPILE_COBOL_STATEMENT_MOVE = 0,
    TRANSPILE_COBOL_STATEMENT_IF,
    TRANSPILE_COBOL_STATEMENT_PERFORM_UNTIL,
    TRANSPILE_COBOL_STATEMENT_PERFORM_VARYING
}   t_transpiler_cobol_statement_kind;

typedef struct s_transpiler_cobol_move_statement
{
    const char *source;
    const char *target;
}   t_transpiler_cobol_move_statement;

typedef struct s_transpiler_cobol_if_statement
{
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement_block then_branch;
    t_transpiler_cobol_statement_block else_branch;
}   t_transpiler_cobol_if_statement;

typedef struct s_transpiler_cobol_perform_until
{
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement_block body;
}   t_transpiler_cobol_perform_until;

typedef struct s_transpiler_cobol_perform_varying
{
    const char *counter;
    const char *initial;
    const char *step;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement_block body;
}   t_transpiler_cobol_perform_varying;

typedef struct s_transpiler_cobol_statement
{
    t_transpiler_cobol_statement_kind kind;
    t_transpiler_cobol_move_statement move;
    t_transpiler_cobol_if_statement if_statement;
    t_transpiler_cobol_perform_until perform_until;
    t_transpiler_cobol_perform_varying perform_varying;
}   t_transpiler_cobol_statement;

typedef struct s_transpiler_cobol_paragraph
{
    char *name;
    t_transpiler_cobol_statement_block statements;
}   t_transpiler_cobol_paragraph;

typedef struct s_transpiler_cobol_procedure
{
    t_transpiler_cobol_paragraph **paragraphs;
    size_t count;
    size_t capacity;
}   t_transpiler_cobol_procedure;

void transpiler_cobol_statement_block_init(t_transpiler_cobol_statement_block *block);
void transpiler_cobol_statement_block_dispose(t_transpiler_cobol_statement_block *block);
int transpiler_cobol_statement_block_append(t_transpiler_cobol_statement_block *block,
    t_transpiler_cobol_statement *statement);

t_transpiler_cobol_statement *transpiler_cobol_statement_create_move(const char *source, const char *target);

t_transpiler_cobol_statement *transpiler_cobol_statement_create_if(const t_transpiler_cobol_condition *condition);

t_transpiler_cobol_statement *transpiler_cobol_statement_create_perform_until(
    const t_transpiler_cobol_condition *condition);

t_transpiler_cobol_statement *transpiler_cobol_statement_create_perform_varying(const char *counter,
    const char *initial, const char *step, const t_transpiler_cobol_condition *condition);

void transpiler_cobol_statement_destroy(t_transpiler_cobol_statement *statement);

t_transpiler_cobol_statement_block *transpiler_cobol_if_get_then_branch(t_transpiler_cobol_statement *statement);

t_transpiler_cobol_statement_block *transpiler_cobol_if_get_else_branch(t_transpiler_cobol_statement *statement);

t_transpiler_cobol_statement_block *transpiler_cobol_perform_until_get_body(t_transpiler_cobol_statement *statement);

t_transpiler_cobol_statement_block *transpiler_cobol_perform_varying_get_body(t_transpiler_cobol_statement *statement);

t_transpiler_cobol_paragraph *transpiler_cobol_paragraph_create(const char *name);
void transpiler_cobol_paragraph_destroy(t_transpiler_cobol_paragraph *paragraph);
t_transpiler_cobol_statement_block *transpiler_cobol_paragraph_get_statements(t_transpiler_cobol_paragraph *paragraph);

void transpiler_cobol_procedure_init(t_transpiler_cobol_procedure *procedure);
void transpiler_cobol_procedure_dispose(t_transpiler_cobol_procedure *procedure);
int transpiler_cobol_procedure_append(t_transpiler_cobol_procedure *procedure,
    t_transpiler_cobol_paragraph *paragraph);

#endif
