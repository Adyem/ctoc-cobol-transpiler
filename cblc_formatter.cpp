#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"

#define CBLC_FORMATTER_KEYWORD_COUNT 14

typedef enum e_cblc_formatter_token_kind
{
    CBLC_FORMATTER_TOKEN_NONE = 0,
    CBLC_FORMATTER_TOKEN_IDENTIFIER,
    CBLC_FORMATTER_TOKEN_KEYWORD,
    CBLC_FORMATTER_TOKEN_NUMBER,
    CBLC_FORMATTER_TOKEN_STRING,
    CBLC_FORMATTER_TOKEN_OPERATOR,
    CBLC_FORMATTER_TOKEN_OPEN_PAREN,
    CBLC_FORMATTER_TOKEN_CLOSE_PAREN,
    CBLC_FORMATTER_TOKEN_OPEN_BRACE,
    CBLC_FORMATTER_TOKEN_CLOSE_BRACE,
    CBLC_FORMATTER_TOKEN_COMMA,
    CBLC_FORMATTER_TOKEN_SEMICOLON,
    CBLC_FORMATTER_TOKEN_OPEN_BRACKET,
    CBLC_FORMATTER_TOKEN_CLOSE_BRACKET
}   t_cblc_formatter_token_kind;

typedef struct s_cblc_formatter_buffer
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cblc_formatter_buffer;

static const char *g_cblc_keywords[CBLC_FORMATTER_KEYWORD_COUNT] = {
    "function",
    "void",
    "char",
    "int",
    "long",
    "float",
    "double",
    "return",
    "if",
    "else",
    "while",
    "do",
    "open",
    "close"
};

static int cblc_formatter_buffer_reserve(t_cblc_formatter_buffer *buffer, size_t desired_capacity)
{
    char *new_data;
    size_t index;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity > (SIZE_MAX / 2))
        desired_capacity = SIZE_MAX / 2;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    index = 0;
    while (index < buffer->length)
    {
        new_data[index] = buffer->data[index];
        index += 1;
    }
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = new_data;
    buffer->capacity = desired_capacity;
    buffer->data[buffer->length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_formatter_buffer_append_char(t_cblc_formatter_buffer *buffer, char value)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer->length + 1 >= buffer->capacity)
    {
        size_t new_capacity;

        if (buffer->capacity == 0)
            new_capacity = 64;
        else
            new_capacity = buffer->capacity * 2;
        if (cblc_formatter_buffer_reserve(buffer, new_capacity) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    buffer->data[buffer->length] = value;
    buffer->length += 1;
    buffer->data[buffer->length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_formatter_buffer_append_string(t_cblc_formatter_buffer *buffer, const char *text, size_t length)
{
    size_t index;

    if (!buffer)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    index = 0;
    while (index < length)
    {
        if (cblc_formatter_buffer_append_char(buffer, text[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_formatter_is_identifier_start(char ch)
{
    if (ft_isalpha(ch))
        return (1);
    if (ch == '_')
        return (1);
    return (0);
}

static int cblc_formatter_is_identifier_part(char ch)
{
    if (ft_isalnum(ch))
        return (1);
    if (ch == '_')
        return (1);
    return (0);
}

static int cblc_formatter_is_keyword(const char *text, size_t length)
{
    size_t index;
    size_t keyword_length;

    if (!text)
        return (0);
    index = 0;
    while (index < CBLC_FORMATTER_KEYWORD_COUNT)
    {
        keyword_length = ft_strlen(g_cblc_keywords[index]);
        if (keyword_length == length)
        {
            if (ft_strncmp(text, g_cblc_keywords[index], length) == 0)
                return (1);
        }
        index += 1;
    }
    return (0);
}

static int cblc_formatter_keyword_needs_paren_space(const char *text, size_t length)
{
    if (!text)
        return (0);
    if (length == 2 && ft_strncmp(text, "if", 2) == 0)
        return (1);
    if (length == 5 && ft_strncmp(text, "while", 5) == 0)
        return (1);
    if (length == 2 && ft_strncmp(text, "do", 2) == 0)
        return (1);
    return (0);
}

static int cblc_formatter_flush_whitespace(t_cblc_formatter_buffer *buffer,
    int *newline_pending, int *space_pending, size_t indent_level, int *at_line_start)
{
    size_t indent_index;

    if (!buffer || !newline_pending || !space_pending || !at_line_start)
        return (FT_FAILURE);
    if (*newline_pending)
    {
        if (cblc_formatter_buffer_append_char(buffer, '\n') != FT_SUCCESS)
            return (FT_FAILURE);
        indent_index = 0;
        while (indent_index < indent_level)
        {
            if (cblc_formatter_buffer_append_string(buffer, "    ", 4) != FT_SUCCESS)
                return (FT_FAILURE);
            indent_index += 1;
        }
        *at_line_start = 0;
        *newline_pending = 0;
        *space_pending = 0;
        return (FT_SUCCESS);
    }
    if (*space_pending && !*at_line_start)
    {
        if (cblc_formatter_buffer_append_char(buffer, ' ') != FT_SUCCESS)
            return (FT_FAILURE);
        *space_pending = 0;
    }
    return (FT_SUCCESS);
}

static int cblc_formatter_append_identifier(t_cblc_formatter_buffer *buffer,
    const char *start, size_t length, t_cblc_formatter_token_kind *last_token,
    int *last_keyword_needs_paren_space, int *space_pending, int *newline_pending,
    size_t indent_level, int *at_line_start)
{
    int is_keyword;

    if (!buffer || !start || !last_token || !last_keyword_needs_paren_space
        || !space_pending || !newline_pending || !at_line_start)
        return (FT_FAILURE);
    if (*last_token == CBLC_FORMATTER_TOKEN_IDENTIFIER
        || *last_token == CBLC_FORMATTER_TOKEN_KEYWORD
        || *last_token == CBLC_FORMATTER_TOKEN_NUMBER
        || *last_token == CBLC_FORMATTER_TOKEN_STRING
        || *last_token == CBLC_FORMATTER_TOKEN_CLOSE_PAREN
        || *last_token == CBLC_FORMATTER_TOKEN_CLOSE_BRACKET)
    {
        *space_pending = 1;
    }
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_string(buffer, start, length) != FT_SUCCESS)
        return (FT_FAILURE);
    *at_line_start = 0;
    is_keyword = cblc_formatter_is_keyword(start, length);
    if (is_keyword)
        *last_token = CBLC_FORMATTER_TOKEN_KEYWORD;
    else
        *last_token = CBLC_FORMATTER_TOKEN_IDENTIFIER;
    *last_keyword_needs_paren_space = 0;
    if (is_keyword)
    {
        *last_keyword_needs_paren_space = cblc_formatter_keyword_needs_paren_space(start, length);
        *space_pending = 1;
    }
    else
    {
        *space_pending = 1;
    }
    return (FT_SUCCESS);
}

static int cblc_formatter_append_number(t_cblc_formatter_buffer *buffer,
    const char *start, size_t length, t_cblc_formatter_token_kind *last_token,
    int *space_pending, int *newline_pending, size_t indent_level, int *at_line_start)
{
    if (!buffer || !start || !last_token || !space_pending || !newline_pending || !at_line_start)
        return (FT_FAILURE);
    if (*last_token == CBLC_FORMATTER_TOKEN_IDENTIFIER
        || *last_token == CBLC_FORMATTER_TOKEN_KEYWORD
        || *last_token == CBLC_FORMATTER_TOKEN_NUMBER
        || *last_token == CBLC_FORMATTER_TOKEN_STRING
        || *last_token == CBLC_FORMATTER_TOKEN_CLOSE_PAREN
        || *last_token == CBLC_FORMATTER_TOKEN_CLOSE_BRACKET)
    {
        *space_pending = 1;
    }
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_string(buffer, start, length) != FT_SUCCESS)
        return (FT_FAILURE);
    *at_line_start = 0;
    *last_token = CBLC_FORMATTER_TOKEN_NUMBER;
    *space_pending = 1;
    return (FT_SUCCESS);
}

static int cblc_formatter_append_string_literal(t_cblc_formatter_buffer *buffer,
    const char *input, size_t *index, t_cblc_formatter_token_kind *last_token,
    int *space_pending, int *newline_pending, size_t indent_level, int *at_line_start)
{
    char ch;

    if (!buffer || !input || !index || !last_token || !space_pending
        || !newline_pending || !at_line_start)
        return (FT_FAILURE);
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_char(buffer, '"') != FT_SUCCESS)
        return (FT_FAILURE);
    *index += 1;
    while (input[*index] != '\0')
    {
        ch = input[*index];
        if (ch == '\\')
        {
            if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
                return (FT_FAILURE);
            *index += 1;
            if (input[*index] != '\0')
            {
                ch = input[*index];
                if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
                    return (FT_FAILURE);
                *index += 1;
                continue ;
            }
            break ;
        }
        if (ch == '"')
        {
            if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
                return (FT_FAILURE);
            *index += 1;
            *last_token = CBLC_FORMATTER_TOKEN_STRING;
            *space_pending = 1;
            *at_line_start = 0;
            return (FT_SUCCESS);
        }
        if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
            return (FT_FAILURE);
        *index += 1;
    }
    *last_token = CBLC_FORMATTER_TOKEN_STRING;
    *space_pending = 1;
    *at_line_start = 0;
    return (FT_SUCCESS);
}

static int cblc_formatter_append_char_literal(t_cblc_formatter_buffer *buffer,
    const char *input, size_t *index, t_cblc_formatter_token_kind *last_token,
    int *space_pending, int *newline_pending, size_t indent_level, int *at_line_start)
{
    char ch;

    if (!buffer || !input || !index || !last_token || !space_pending
        || !newline_pending || !at_line_start)
        return (FT_FAILURE);
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_char(buffer, '\'') != FT_SUCCESS)
        return (FT_FAILURE);
    *index += 1;
    while (input[*index] != '\0')
    {
        ch = input[*index];
        if (ch == '\\')
        {
            if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
                return (FT_FAILURE);
            *index += 1;
            if (input[*index] != '\0')
            {
                ch = input[*index];
                if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
                    return (FT_FAILURE);
                *index += 1;
                continue ;
            }
            break ;
        }
        if (ch == '\'')
        {
            if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
                return (FT_FAILURE);
            *index += 1;
            *last_token = CBLC_FORMATTER_TOKEN_STRING;
            *space_pending = 1;
            *at_line_start = 0;
            return (FT_SUCCESS);
        }
        if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
            return (FT_FAILURE);
        *index += 1;
    }
    *last_token = CBLC_FORMATTER_TOKEN_STRING;
    *space_pending = 1;
    *at_line_start = 0;
    return (FT_SUCCESS);
}

static int cblc_formatter_append_operator(t_cblc_formatter_buffer *buffer,
    const char *text, size_t length, t_cblc_formatter_token_kind *last_token,
    int *space_pending, int *newline_pending, size_t indent_level, int *at_line_start)
{
    if (!buffer || !text || !last_token || !space_pending || !newline_pending || !at_line_start)
        return (FT_FAILURE);
    if (*last_token != CBLC_FORMATTER_TOKEN_NONE
        && *last_token != CBLC_FORMATTER_TOKEN_OPEN_BRACE
        && *last_token != CBLC_FORMATTER_TOKEN_OPERATOR)
        *space_pending = 1;
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_string(buffer, text, length) != FT_SUCCESS)
        return (FT_FAILURE);
    *last_token = CBLC_FORMATTER_TOKEN_OPERATOR;
    *space_pending = 1;
    *at_line_start = 0;
    return (FT_SUCCESS);
}

static int cblc_formatter_append_comment(t_cblc_formatter_buffer *buffer,
    const char *input, size_t *index, size_t indent_level, int *newline_pending,
    int *space_pending, int *at_line_start)
{
    if (!buffer || !input || !index || !newline_pending || !space_pending || !at_line_start)
        return (FT_FAILURE);
    if (!*at_line_start)
        *newline_pending = 1;
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_char(buffer, '/') != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_char(buffer, '/') != FT_SUCCESS)
        return (FT_FAILURE);
    *index += 2;
    while (input[*index] != '\0' && input[*index] != '\n')
    {
        if (cblc_formatter_buffer_append_char(buffer, input[*index]) != FT_SUCCESS)
            return (FT_FAILURE);
        *index += 1;
    }
    *newline_pending = 1;
    *space_pending = 0;
    *at_line_start = 0;
    return (FT_SUCCESS);
}

static int cblc_formatter_process_token(const char *input, size_t *index,
    t_cblc_formatter_buffer *buffer, size_t *indent_level,
    t_cblc_formatter_token_kind *last_token, int *last_keyword_needs_paren_space,
    int *space_pending, int *newline_pending, int *at_line_start)
{
    char ch;
    size_t start_index;

    ch = input[*index];
    if (ft_isspace(ch))
    {
        if (ch == '\n')
            *newline_pending = 1;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == '/' && input[*index + 1] == '/')
        return (cblc_formatter_append_comment(buffer, input, index, *indent_level,
            newline_pending, space_pending, at_line_start));
    if (cblc_formatter_is_identifier_start(ch))
    {
        start_index = *index;
        *index += 1;
        while (cblc_formatter_is_identifier_part(input[*index]))
            *index += 1;
        return (cblc_formatter_append_identifier(buffer, input + start_index,
            *index - start_index, last_token, last_keyword_needs_paren_space,
            space_pending, newline_pending, *indent_level, at_line_start));
    }
    if (ft_isdigit(ch))
    {
        start_index = *index;
        *index += 1;
        while (ft_isdigit(input[*index]))
            *index += 1;
        if (input[*index] == '.' && ft_isdigit(input[*index + 1]))
        {
            *index += 1;
            while (ft_isdigit(input[*index]))
                *index += 1;
        }
        return (cblc_formatter_append_number(buffer, input + start_index,
            *index - start_index, last_token, space_pending,
            newline_pending, *indent_level, at_line_start));
    }
    if (ch == '"')
        return (cblc_formatter_append_string_literal(buffer, input, index, last_token,
            space_pending, newline_pending, *indent_level, at_line_start));
    if (ch == '\'')
        return (cblc_formatter_append_char_literal(buffer, input, index, last_token,
            space_pending, newline_pending, *indent_level, at_line_start));
    if (ch == '{')
    {
        *space_pending = 0;
        if (!*at_line_start)
            *newline_pending = 1;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, '{') != FT_SUCCESS)
            return (FT_FAILURE);
        *indent_level += 1;
        *newline_pending = 1;
        *last_token = CBLC_FORMATTER_TOKEN_OPEN_BRACE;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == '}')
    {
        if (*indent_level > 0)
            *indent_level -= 1;
        *space_pending = 0;
        if (!*at_line_start)
            *newline_pending = 1;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, '}') != FT_SUCCESS)
            return (FT_FAILURE);
        *newline_pending = 1;
        *last_token = CBLC_FORMATTER_TOKEN_CLOSE_BRACE;
        *index += 1;
        *at_line_start = 0;
        return (FT_SUCCESS);
    }
    if (ch == '(')
    {
        if (*last_token == CBLC_FORMATTER_TOKEN_KEYWORD && *last_keyword_needs_paren_space)
            *space_pending = 1;
        else
            *space_pending = 0;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, '(') != FT_SUCCESS)
            return (FT_FAILURE);
        *last_token = CBLC_FORMATTER_TOKEN_OPEN_PAREN;
        *space_pending = 0;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == ')')
    {
        *space_pending = 0;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, ')') != FT_SUCCESS)
            return (FT_FAILURE);
        *last_token = CBLC_FORMATTER_TOKEN_CLOSE_PAREN;
        *space_pending = 1;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == '[')
    {
        *space_pending = 0;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, '[') != FT_SUCCESS)
            return (FT_FAILURE);
        *last_token = CBLC_FORMATTER_TOKEN_OPEN_BRACKET;
        *space_pending = 0;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == ']')
    {
        *space_pending = 0;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, ']') != FT_SUCCESS)
            return (FT_FAILURE);
        *last_token = CBLC_FORMATTER_TOKEN_CLOSE_BRACKET;
        *space_pending = 1;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == ',')
    {
        *space_pending = 0;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, ',') != FT_SUCCESS)
            return (FT_FAILURE);
        *space_pending = 1;
        *last_token = CBLC_FORMATTER_TOKEN_COMMA;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == ';')
    {
        *space_pending = 0;
        if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
                *indent_level, at_line_start) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_formatter_buffer_append_char(buffer, ';') != FT_SUCCESS)
            return (FT_FAILURE);
        *newline_pending = 1;
        *space_pending = 0;
        *last_token = CBLC_FORMATTER_TOKEN_SEMICOLON;
        *at_line_start = 0;
        *index += 1;
        return (FT_SUCCESS);
    }
    if (ch == '\n')
    {
        *newline_pending = 1;
        *index += 1;
        return (FT_SUCCESS);
    }
    if ((ch == '=' && input[*index + 1] == '=')
        || (ch == '!' && input[*index + 1] == '=')
        || (ch == '<' && input[*index + 1] == '=')
        || (ch == '>' && input[*index + 1] == '=')
        || (ch == '&' && input[*index + 1] == '&')
        || (ch == '|' && input[*index + 1] == '|'))
    {
        start_index = *index;
        *index += 2;
        return (cblc_formatter_append_operator(buffer, input + start_index, 2,
            last_token, space_pending, newline_pending, *indent_level, at_line_start));
    }
    if (ch == '=' || ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '<'
        || ch == '>' || ch == '%' || ch == '!')
    {
        start_index = *index;
        *index += 1;
        return (cblc_formatter_append_operator(buffer, input + start_index, 1,
            last_token, space_pending, newline_pending, *indent_level, at_line_start));
    }
    if (cblc_formatter_flush_whitespace(buffer, newline_pending, space_pending,
            *indent_level, at_line_start) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_formatter_buffer_append_char(buffer, ch) != FT_SUCCESS)
        return (FT_FAILURE);
    *last_token = CBLC_FORMATTER_TOKEN_OPERATOR;
    *space_pending = 1;
    *at_line_start = 0;
    *index += 1;
    return (FT_SUCCESS);
}

static int cblc_formatter_format_pretty(const char *input, char **output)
{
    t_cblc_formatter_buffer buffer;
    size_t index;
    size_t indent_level;
    t_cblc_formatter_token_kind last_token;
    int last_keyword_needs_paren_space;
    int space_pending;
    int newline_pending;
    int at_line_start;

    if (!output)
        return (FT_FAILURE);
    *output = NULL;
    if (!input)
        return (FT_FAILURE);
    buffer.data = NULL;
    buffer.length = 0;
    buffer.capacity = 0;
    if (cblc_formatter_buffer_reserve(&buffer, 128) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    indent_level = 0;
    last_token = CBLC_FORMATTER_TOKEN_NONE;
    last_keyword_needs_paren_space = 0;
    space_pending = 0;
    newline_pending = 0;
    at_line_start = 1;
    while (input[index] != '\0')
    {
        if (cblc_formatter_process_token(input, &index, &buffer, &indent_level,
                &last_token, &last_keyword_needs_paren_space, &space_pending,
                &newline_pending, &at_line_start) != FT_SUCCESS)
        {
            cma_free(buffer.data);
            return (FT_FAILURE);
        }
    }
    if (newline_pending)
    {
        if (cblc_formatter_flush_whitespace(&buffer, &newline_pending, &space_pending,
                indent_level, &at_line_start) != FT_SUCCESS)
        {
            cma_free(buffer.data);
            return (FT_FAILURE);
        }
    }
    if (buffer.length == 0 || buffer.data[buffer.length - 1] != '\n')
    {
        if (cblc_formatter_buffer_append_char(&buffer, '\n') != FT_SUCCESS)
        {
            cma_free(buffer.data);
            return (FT_FAILURE);
        }
    }
    *output = buffer.data;
    return (FT_SUCCESS);
}

static int cblc_formatter_format_passthrough(const char *input, char **output)
{
    size_t length;
    size_t index;
    char *result;

    if (!input || !output)
        return (FT_FAILURE);
    length = ft_strlen(input);
    result = static_cast<char *>(cma_calloc(length + 1, sizeof(char)));
    if (!result)
        return (FT_FAILURE);
    index = 0;
    while (index < length)
    {
        result[index] = input[index];
        index += 1;
    }
    result[length] = '\0';
    *output = result;
    return (FT_SUCCESS);
}

int cblc_formatter_format(const char *input, t_transpiler_format_mode mode, char **output)
{
    if (!output)
        return (FT_FAILURE);
    if (!input)
        return (FT_FAILURE);
    if (mode == TRANSPILE_FORMAT_MINIMAL)
        return (cblc_formatter_format_passthrough(input, output));
    return (cblc_formatter_format_pretty(input, output));
}
