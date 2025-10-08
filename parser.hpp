#ifndef PARSER_HPP
#define PARSER_HPP

#include "lexer.hpp"
#include "ast.hpp"

typedef struct s_parser
{
    t_lexer lexer;
    t_lexer_token current;
    int has_current;
    int last_error;
    size_t error_count;
}   t_parser;

void parser_init(t_parser *parser, const char *text);
void parser_dispose(t_parser *parser);
int parser_parse_program(t_parser *parser, t_ast_node **out_program);

#endif
