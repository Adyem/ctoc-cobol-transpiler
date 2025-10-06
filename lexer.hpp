#ifndef LEXER_HPP
#define LEXER_HPP

#include <cstddef>

#include "lexer_token.hpp"

typedef struct s_lexer
{
    const char *text;
    size_t length;
    size_t offset;
    size_t line;
    size_t column;
}   t_lexer;

void lexer_init(t_lexer *lexer, const char *text);
int lexer_next_token(t_lexer *lexer, t_lexer_token *token);

#endif
