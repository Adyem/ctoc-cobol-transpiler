#ifndef TRANSPILER_CBLC_HPP
#define TRANSPILER_CBLC_HPP

#include <cstddef>

#include "transpiler_context.hpp"

typedef struct s_cblc_data_item
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    size_t length;
}   t_cblc_data_item;

typedef enum e_cblc_statement_type
{
    CBLC_STATEMENT_ASSIGNMENT,
    CBLC_STATEMENT_DISPLAY
}   t_cblc_statement_type;

typedef struct s_cblc_statement
{
    t_cblc_statement_type type;
    char target[TRANSPILE_IDENTIFIER_MAX];
    char source[TRANSPILE_IDENTIFIER_MAX];
    int is_literal;
}   t_cblc_statement;

typedef struct s_cblc_translation_unit
{
    t_cblc_data_item *data_items;
    size_t data_count;
    size_t data_capacity;
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;
    char program_name[TRANSPILE_IDENTIFIER_MAX];
    int saw_return;
}   t_cblc_translation_unit;

void cblc_translation_unit_init(t_cblc_translation_unit *unit);
void cblc_translation_unit_dispose(t_cblc_translation_unit *unit);
int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit);
int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text);

#endif
