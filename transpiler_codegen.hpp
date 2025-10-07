#ifndef TRANSPILER_CODEGEN_HPP
#define TRANSPILER_CODEGEN_HPP

#include "transpiler_context.hpp"
#include "transpiler_cobol_procedure.hpp"

typedef struct s_transpiler_cobol_file_sections
{
    char *environment_division;
    char *data_division;
}   t_transpiler_cobol_file_sections;

void transpiler_codegen_file_sections_init(t_transpiler_cobol_file_sections *sections);
void transpiler_codegen_file_sections_dispose(t_transpiler_cobol_file_sections *sections);
int transpiler_codegen_build_file_sections(const t_transpiler_context *context,
    t_transpiler_cobol_file_sections *sections);

int transpiler_codegen_build_procedure_division(const t_transpiler_cobol_procedure *procedure,
    char **out);

#endif
