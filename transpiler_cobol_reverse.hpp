#ifndef TRANSPILER_COBOL_REVERSE_HPP
#define TRANSPILER_COBOL_REVERSE_HPP

#include "ast.hpp"
#include "transpiler_context.hpp"

int transpiler_cobol_program_to_cblc(t_transpiler_context *context, const t_ast_node *program,
    char **out_text);

#endif
