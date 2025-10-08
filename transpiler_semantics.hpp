#ifndef TRANSPILER_SEMANTICS_HPP
#define TRANSPILER_SEMANTICS_HPP

#include "ast.hpp"
#include "transpiler_context.hpp"

#define TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER 2001
#define TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM 2002
#define TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE 2003
#define TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH 2004
#define TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION 2005

int transpiler_semantics_analyze_program(t_transpiler_context *context, const t_ast_node *program);

#endif
