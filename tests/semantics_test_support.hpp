#ifndef SEMANTICS_TEST_SUPPORT_HPP
#define SEMANTICS_TEST_SUPPORT_HPP

#include "transpiler_semantics.hpp"

#include "libft/Libft/libft.hpp"

void semantics_destroy_program(t_ast_node *program);
t_ast_node *semantics_create_identifier_node(const char *name);
t_ast_node *semantics_create_literal_node(const char *lexeme,
    t_lexer_token_kind kind);
t_ast_node *semantics_create_picture_node(const char *text);
t_ast_node *semantics_create_comparison_operator_node(
    t_lexer_token_kind kind, const char *lexeme);
t_ast_node *semantics_create_arithmetic_operator_node(
    t_lexer_token_kind kind, const char *lexeme);
t_ast_node *semantics_create_arithmetic_expression_node_with_operator(
    const char *left_name, t_lexer_token_kind operator_kind,
    const char *operator_lexeme, const char *right_name);
t_ast_node *semantics_create_arithmetic_expression_node(
    const char *left_name, const char *right_name);
t_ast_node *semantics_build_program_with_storage_level(
    const char *storage_name, const char *picture_text,
    const char *level_text);
t_ast_node *semantics_build_program_with_storage(const char *storage_name,
    const char *picture_text);
int semantics_add_data_item_with_level(t_ast_node *program,
    const char *name, const char *picture_text, const char *level_text);
int semantics_add_data_item(t_ast_node *program, const char *name,
    const char *picture_text);
int semantics_add_copybook_include(t_ast_node *program,
    const char *copybook_name);
int semantics_attach_procedure_with_assignment_like_node(
    t_ast_node *program, t_ast_node *source_node, const char *target_name,
    t_ast_node_kind statement_kind);
int semantics_attach_procedure_with_move_node(t_ast_node *program,
    t_ast_node *source_node, const char *target_name);
int semantics_attach_procedure_with_assignment_node(t_ast_node *program,
    t_ast_node *source_node, const char *target_name);
int semantics_attach_procedure_with_move(t_ast_node *program,
    const char *source_name, const char *target_name, int use_literal_source);
int semantics_attach_procedure_with_assignment(t_ast_node *program,
    const char *source_name, const char *target_name,
    int use_literal_source);
int semantics_attach_procedure_with_if_comparison(t_ast_node *program,
    const char *left_name, t_lexer_token_kind operator_kind,
    const char *operator_lexeme, const char *right_name);

#endif
