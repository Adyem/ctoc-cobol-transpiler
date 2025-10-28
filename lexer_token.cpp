#include "cblc_transpiler.hpp"

#include "libft/Libft/libft.hpp"

typedef struct s_keyword_entry
{
    const char *text;
    t_lexer_token_kind kind;
}   t_keyword_entry;

static char lexer_to_upper(char value)
{
    if (value >= 'a' && value <= 'z')
        return (static_cast<char>(value - ('a' - 'A')));
    return (value);
}

static int lexer_keyword_equals(const char *text, size_t length, const char *keyword)
{
    size_t index;
    size_t keyword_length;

    if (!text)
        return (0);
    if (!keyword)
        return (0);
    keyword_length = static_cast<size_t>(ft_strlen(keyword));
    if (keyword_length != length)
        return (0);
    index = 0;
    while (index < length)
    {
        if (lexer_to_upper(text[index]) != keyword[index])
            return (0);
        index += 1;
    }
    return (1);
}

static const t_keyword_entry g_keyword_table[] = {
    {"IDENTIFICATION", LEXER_TOKEN_KEYWORD_IDENTIFICATION},
    {"DIVISION", LEXER_TOKEN_KEYWORD_DIVISION},
    {"PROGRAM-ID", LEXER_TOKEN_KEYWORD_PROGRAM_ID},
    {"ENVIRONMENT", LEXER_TOKEN_KEYWORD_ENVIRONMENT},
    {"DATA", LEXER_TOKEN_KEYWORD_DATA},
    {"PROCEDURE", LEXER_TOKEN_KEYWORD_PROCEDURE},
    {"WORKING-STORAGE", LEXER_TOKEN_KEYWORD_WORKING_STORAGE},
    {"SECTION", LEXER_TOKEN_KEYWORD_SECTION},
    {"FILE", LEXER_TOKEN_KEYWORD_FILE},
    {"SELECT", LEXER_TOKEN_KEYWORD_SELECT},
    {"ASSIGN", LEXER_TOKEN_KEYWORD_ASSIGN},
    {"TO", LEXER_TOKEN_KEYWORD_TO},
    {"FD", LEXER_TOKEN_KEYWORD_FD},
    {"COPY", LEXER_TOKEN_KEYWORD_COPY},
    {"REPLACING", LEXER_TOKEN_KEYWORD_REPLACING},
    {"LEADING", LEXER_TOKEN_KEYWORD_LEADING},
    {"TRAILING", LEXER_TOKEN_KEYWORD_TRAILING},
    {"WORD", LEXER_TOKEN_KEYWORD_WORD},
    {"PIC", LEXER_TOKEN_KEYWORD_PIC},
    {"VALUE", LEXER_TOKEN_KEYWORD_VALUE},
    {"IF", LEXER_TOKEN_KEYWORD_IF},
    {"ELSE", LEXER_TOKEN_KEYWORD_ELSE},
    {"PERFORM", LEXER_TOKEN_KEYWORD_PERFORM},
    {"UNTIL", LEXER_TOKEN_KEYWORD_UNTIL},
    {"MOVE", LEXER_TOKEN_KEYWORD_MOVE},
    {"COMPUTE", LEXER_TOKEN_KEYWORD_COMPUTE},
    {"ABS", LEXER_TOKEN_KEYWORD_ABS},
    {"MOD", LEXER_TOKEN_KEYWORD_MOD},
    {"OPEN", LEXER_TOKEN_KEYWORD_OPEN},
    {"CLOSE", LEXER_TOKEN_KEYWORD_CLOSE},
    {"READ", LEXER_TOKEN_KEYWORD_READ},
    {"WRITE", LEXER_TOKEN_KEYWORD_WRITE},
    {"CALL", LEXER_TOKEN_KEYWORD_CALL},
    {"DISPLAY", LEXER_TOKEN_KEYWORD_DISPLAY},
    {"VARYING", LEXER_TOKEN_KEYWORD_VARYING},
    {"FROM", LEXER_TOKEN_KEYWORD_FROM},
    {"BY", LEXER_TOKEN_KEYWORD_BY},
    {"OCCURS", LEXER_TOKEN_KEYWORD_OCCURS},
    {"TIMES", LEXER_TOKEN_KEYWORD_TIMES},
    {"DEPENDING", LEXER_TOKEN_KEYWORD_DEPENDING},
    {"USING", LEXER_TOKEN_KEYWORD_USING},
    {"REFERENCE", LEXER_TOKEN_KEYWORD_REFERENCE},
    {"CONTENT", LEXER_TOKEN_KEYWORD_CONTENT},
    {"LENGTH", LEXER_TOKEN_KEYWORD_LENGTH},
    {"OF", LEXER_TOKEN_KEYWORD_OF},
    {"END-IF", LEXER_TOKEN_KEYWORD_END_IF},
    {"END-PERFORM", LEXER_TOKEN_KEYWORD_END_PERFORM},
    {"NOT", LEXER_TOKEN_KEYWORD_NOT},
    {"TRUE", LEXER_TOKEN_KEYWORD_TRUE},
    {"FALSE", LEXER_TOKEN_KEYWORD_FALSE},
    {"STOP", LEXER_TOKEN_KEYWORD_STOP},
    {"RUN", LEXER_TOKEN_KEYWORD_RUN},
    {"USE", LEXER_TOKEN_KEYWORD_USE},
    {"AFTER", LEXER_TOKEN_KEYWORD_AFTER},
    {"ERROR", LEXER_TOKEN_KEYWORD_ERROR},
    {"ON", LEXER_TOKEN_KEYWORD_ON},
    {"DECLARATIVES", LEXER_TOKEN_KEYWORD_DECLARATIVES},
    {NULL, LEXER_TOKEN_IDENTIFIER}
};

t_lexer_token_kind lexer_token_lookup_keyword(const char *text, size_t length)
{
    size_t index;

    if (!text)
        return (LEXER_TOKEN_IDENTIFIER);
    if (length == 0)
        return (LEXER_TOKEN_IDENTIFIER);
    index = 0;
    while (g_keyword_table[index].text)
    {
        if (lexer_keyword_equals(text, length, g_keyword_table[index].text))
            return (g_keyword_table[index].kind);
        index += 1;
    }
    return (LEXER_TOKEN_IDENTIFIER);
}

t_lexer_trivia_kind lexer_classify_trivia(const char *text, size_t length)
{
    size_t index;
    char value;

    if (!text)
        return (LEXER_TRIVIA_NONE);
    if (length == 0)
        return (LEXER_TRIVIA_NONE);
    if (length >= 2 && text[0] == '*' && text[1] == '>')
        return (LEXER_TRIVIA_COMMENT);
    index = 0;
    while (index < length)
    {
        value = text[index];
        if (value != ' ' && value != '\t' && value != '\n' && value != '\r'
            && value != '\f' && value != '\v')
            return (LEXER_TRIVIA_NONE);
        index += 1;
    }
    return (LEXER_TRIVIA_WHITESPACE);
}
