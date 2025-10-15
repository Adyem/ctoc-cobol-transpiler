#include "lexer.hpp"

#include "libft/Libft/libft.hpp"

static const char g_lexer_single_quote = '\'';

static void lexer_reset(t_lexer *lexer)
{
    if (!lexer)
        return ;
    lexer->text = NULL;
    lexer->length = 0;
    lexer->offset = 0;
    lexer->line = 1;
    lexer->column = 1;
}

void lexer_init(t_lexer *lexer, const char *text)
{
    if (!lexer)
        return ;
    lexer_reset(lexer);
    if (!text)
        return ;
    lexer->text = text;
    lexer->length = ft_strlen(text);
}

static int lexer_is_at_end(const t_lexer *lexer)
{
    if (!lexer)
        return (1);
    if (lexer->offset >= lexer->length)
        return (1);
    return (0);
}

static char lexer_peek(const t_lexer *lexer)
{
    if (lexer_is_at_end(lexer))
        return ('\0');
    return (lexer->text[lexer->offset]);
}

static char lexer_peek_next(const t_lexer *lexer)
{
    if (!lexer)
        return ('\0');
    if (lexer->offset + 1 >= lexer->length)
        return ('\0');
    return (lexer->text[lexer->offset + 1]);
}

static void lexer_advance(t_lexer *lexer)
{
    char value;

    if (!lexer)
        return ;
    if (lexer_is_at_end(lexer))
        return ;
    value = lexer->text[lexer->offset];
    lexer->offset += 1;
    if (value == '\n')
    {
        lexer->line += 1;
        lexer->column = 1;
    }
    else
        lexer->column += 1;
}

static int lexer_is_whitespace(char value)
{
    if (value == ' ')
        return (1);
    if (value == '\t')
        return (1);
    if (value == '\n')
        return (1);
    if (value == '\r')
        return (1);
    if (value == '\f')
        return (1);
    if (value == '\v')
        return (1);
    return (0);
}

static int lexer_starts_comment(const t_lexer *lexer)
{
    if (!lexer)
        return (0);
    if (lexer_peek(lexer) != '*')
        return (0);
    if (lexer_peek_next(lexer) != '>')
        return (0);
    return (1);
}

static void lexer_skip_comment(t_lexer *lexer)
{
    if (!lexer)
        return ;
    lexer_advance(lexer);
    if (!lexer_is_at_end(lexer) && lexer_peek(lexer) == '>')
        lexer_advance(lexer);
    while (!lexer_is_at_end(lexer))
    {
        char value;

        value = lexer_peek(lexer);
        lexer_advance(lexer);
        if (value == '\n')
            break ;
    }
}

static void lexer_skip_trivia(t_lexer *lexer)
{
    int progress;

    if (!lexer)
        return ;
    progress = 1;
    while (progress)
    {
        progress = 0;
        while (!lexer_is_at_end(lexer) && lexer_is_whitespace(lexer_peek(lexer)))
        {
            lexer_advance(lexer);
            progress = 1;
        }
        if (lexer_starts_comment(lexer))
        {
            lexer_skip_comment(lexer);
            progress = 1;
        }
    }
}

static int lexer_is_identifier_start(char value)
{
    if (ft_isalpha(static_cast<unsigned char>(value)))
        return (1);
    return (0);
}

static int lexer_is_identifier_continue(char value)
{
    if (lexer_is_identifier_start(value))
        return (1);
    if (ft_isdigit(static_cast<unsigned char>(value)))
        return (1);
    if (value == '-')
        return (1);
    if (value == '_')
        return (1);
    return (0);
}

static int lexer_is_digit(char value)
{
    if (ft_isdigit(static_cast<unsigned char>(value)))
        return (1);
    return (0);
}

static void lexer_build_token(t_lexer *lexer, t_lexer_token *token, t_lexer_token_kind kind,
    size_t start_offset, size_t start_line, size_t start_column)
{
    if (!token)
        return ;
    token->kind = kind;
    token->lexeme = NULL;
    token->length = 0;
    token->line = start_line;
    token->column = start_column;
    if (!lexer)
        return ;
    token->lexeme = lexer->text + start_offset;
    token->length = lexer->offset - start_offset;
}

static int lexer_collect_identifier(t_lexer *lexer, t_lexer_token *token, size_t start_offset,
    size_t start_line, size_t start_column)
{
    t_lexer_token_kind kind;

    if (!lexer)
        return (FT_FAILURE);
    while (!lexer_is_at_end(lexer) && lexer_is_identifier_continue(lexer_peek(lexer)))
        lexer_advance(lexer);
    lexer_build_token(lexer, token, LEXER_TOKEN_IDENTIFIER, start_offset, start_line, start_column);
    kind = lexer_token_lookup_keyword(token->lexeme, token->length);
    token->kind = kind;
    return (FT_SUCCESS);
}

static int lexer_collect_numeric_literal(t_lexer *lexer, t_lexer_token *token, size_t start_offset,
    size_t start_line, size_t start_column)
{
    if (!lexer)
        return (FT_FAILURE);
    while (!lexer_is_at_end(lexer) && lexer_is_digit(lexer_peek(lexer)))
        lexer_advance(lexer);
    if (!lexer_is_at_end(lexer) && lexer_peek(lexer) == '.'
        && lexer_is_digit(lexer_peek_next(lexer)))
    {
        lexer_advance(lexer);
        while (!lexer_is_at_end(lexer) && lexer_is_digit(lexer_peek(lexer)))
            lexer_advance(lexer);
    }
    lexer_build_token(lexer, token, LEXER_TOKEN_NUMERIC_LITERAL, start_offset, start_line, start_column);
    return (FT_SUCCESS);
}

static int lexer_collect_string_literal(t_lexer *lexer, t_lexer_token *token, size_t start_offset,
    size_t start_line, size_t start_column)
{
    char terminator;

    if (!lexer)
        return (FT_FAILURE);
    terminator = lexer_peek(lexer);
    lexer_advance(lexer);
    while (!lexer_is_at_end(lexer))
    {
        char value;

        value = lexer_peek(lexer);
        if (value == terminator)
        {
            lexer_advance(lexer);
            lexer_build_token(lexer, token, LEXER_TOKEN_STRING_LITERAL, start_offset, start_line, start_column);
            return (FT_SUCCESS);
        }
        lexer_advance(lexer);
    }
    lexer_build_token(lexer, token, LEXER_TOKEN_UNKNOWN, start_offset, start_line, start_column);
    return (FT_FAILURE);
}

static int lexer_collect_punctuation(t_lexer *lexer, t_lexer_token *token, size_t start_offset,
    size_t start_line, size_t start_column)
{
    char value;
    t_lexer_token_kind kind;

    if (!lexer)
        return (FT_FAILURE);
    value = lexer_peek(lexer);
    lexer_advance(lexer);
    kind = LEXER_TOKEN_UNKNOWN;
    if (value == '.')
        kind = LEXER_TOKEN_PERIOD;
    else if (value == ',')
        kind = LEXER_TOKEN_COMMA;
    else if (value == ':')
        kind = LEXER_TOKEN_COLON;
    else if (value == ';')
        kind = LEXER_TOKEN_SEMICOLON;
    else if (value == '(')
        kind = LEXER_TOKEN_LEFT_PAREN;
    else if (value == ')')
        kind = LEXER_TOKEN_RIGHT_PAREN;
    else if (value == '+')
        kind = LEXER_TOKEN_PLUS;
    else if (value == '-')
        kind = LEXER_TOKEN_MINUS;
    else if (value == '*')
        kind = LEXER_TOKEN_STAR;
    else if (value == '/')
        kind = LEXER_TOKEN_SLASH;
    lexer_build_token(lexer, token, kind, start_offset, start_line, start_column);
    if (kind == LEXER_TOKEN_UNKNOWN)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int lexer_collect_unknown(t_lexer *lexer, t_lexer_token *token, size_t start_offset,
    size_t start_line, size_t start_column)
{
    if (!lexer)
        return (FT_FAILURE);
    lexer_advance(lexer);
    lexer_build_token(lexer, token, LEXER_TOKEN_UNKNOWN, start_offset, start_line, start_column);
    return (FT_FAILURE);
}

int lexer_next_token(t_lexer *lexer, t_lexer_token *token)
{
    size_t start_offset;
    size_t start_line;
    size_t start_column;
    char value;

    if (!lexer)
        return (FT_FAILURE);
    if (!token)
        return (FT_FAILURE);
    lexer_skip_trivia(lexer);
    if (lexer_is_at_end(lexer))
    {
        lexer_build_token(lexer, token, LEXER_TOKEN_END_OF_FILE, lexer->offset, lexer->line, lexer->column);
        return (FT_SUCCESS);
    }
    start_offset = lexer->offset;
    start_line = lexer->line;
    start_column = lexer->column;
    value = lexer_peek(lexer);
    if (lexer_is_identifier_start(value))
        return (lexer_collect_identifier(lexer, token, start_offset, start_line, start_column));
    if (lexer_is_digit(value))
        return (lexer_collect_numeric_literal(lexer, token, start_offset, start_line, start_column));
    if (value == '"' || value == g_lexer_single_quote)
        return (lexer_collect_string_literal(lexer, token, start_offset, start_line, start_column));
    if (value == '=')
    {
        t_lexer_token_kind kind;

        lexer_advance(lexer);
        kind = LEXER_TOKEN_ASSIGN;
        if (!lexer_is_at_end(lexer) && lexer_peek(lexer) == '=')
        {
            lexer_advance(lexer);
            kind = LEXER_TOKEN_EQUALS;
        }
        lexer_build_token(lexer, token, kind, start_offset, start_line, start_column);
        return (FT_SUCCESS);
    }
    if (value == '<')
    {
        t_lexer_token_kind kind;

        lexer_advance(lexer);
        kind = LEXER_TOKEN_LESS_THAN;
        if (!lexer_is_at_end(lexer) && lexer_peek(lexer) == '=')
        {
            lexer_advance(lexer);
            kind = LEXER_TOKEN_LESS_OR_EQUAL;
        }
        else if (!lexer_is_at_end(lexer) && lexer_peek(lexer) == '>')
        {
            lexer_advance(lexer);
            kind = LEXER_TOKEN_NOT_EQUALS;
        }
        lexer_build_token(lexer, token, kind, start_offset, start_line, start_column);
        return (FT_SUCCESS);
    }
    if (value == '>')
    {
        t_lexer_token_kind kind;

        lexer_advance(lexer);
        kind = LEXER_TOKEN_GREATER_THAN;
        if (!lexer_is_at_end(lexer) && lexer_peek(lexer) == '=')
        {
            lexer_advance(lexer);
            kind = LEXER_TOKEN_GREATER_OR_EQUAL;
        }
        lexer_build_token(lexer, token, kind, start_offset, start_line, start_column);
        return (FT_SUCCESS);
    }
    if (value == '.' || value == ',' || value == ':' || value == ';'
        || value == '(' || value == ')' || value == '+' || value == '-'
        || value == '*' || value == '/')
        return (lexer_collect_punctuation(lexer, token, start_offset, start_line, start_column));
    return (lexer_collect_unknown(lexer, token, start_offset, start_line, start_column));
}
