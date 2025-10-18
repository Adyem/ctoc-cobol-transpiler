#include <cstddef>

#include "libft/Libft/libft.hpp"

#include "transpiler_semantics_internal.hpp"

const char *transpiler_semantics_kind_to_string(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
        return ("alphanumeric");
    if (kind == TRANSPILE_SEMANTIC_DATA_BOOLEAN)
        return ("boolean");
    if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        return ("numeric");
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return ("floating");
    return ("unknown");
}

t_transpiler_semantic_data_kind transpiler_semantics_classify_picture(const char *text)
{
    size_t index;
    int has_alpha;
    int has_numeric;
    int has_decimal;

    if (!text)
        return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
    has_alpha = 0;
    has_numeric = 0;
    has_decimal = 0;
    index = 0;
    while (text[index] != '\0')
    {
        char value;

        value = text[index];
        if (value >= 'a' && value <= 'z')
            value = static_cast<char>(value - ('a' - 'A'));
        if (value == 'X' || value == 'A' || value == 'Z')
            has_alpha = 1;
        if (value == '9' || value == 'S' || value == 'V')
        {
            has_numeric = 1;
            if (value == 'V')
                has_decimal = 1;
        }
        index += 1;
    }
    if (has_decimal && !has_alpha)
        return (TRANSPILE_SEMANTIC_DATA_FLOATING);
    if (has_numeric && !has_alpha)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (has_alpha && !has_numeric)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    if (has_decimal)
        return (TRANSPILE_SEMANTIC_DATA_FLOATING);
    if (has_numeric)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (has_alpha)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
}

t_transpiler_semantic_data_kind transpiler_semantics_classify_literal(const t_ast_node *literal)
{
    if (!literal)
        return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
    if (literal->token.kind == LEXER_TOKEN_NUMERIC_LITERAL)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (literal->token.kind == LEXER_TOKEN_STRING_LITERAL)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
}

size_t transpiler_semantics_picture_numeric_length(const char *text)
{
    size_t index;
    size_t length;
    char last_symbol;

    if (!text)
        return (0);
    index = 0;
    length = 0;
    last_symbol = '\0';
    while (text[index] != '\0')
    {
        char value;

        value = text[index];
        if (value >= 'a' && value <= 'z')
            value = static_cast<char>(value - ('a' - 'A'));
        if (value == '9')
        {
            if (length < SIZE_MAX)
                length += 1;
            last_symbol = value;
            index += 1;
            continue ;
        }
        if (value == '(' && last_symbol == '9')
        {
            size_t repeat_index;
            size_t repeat_value;
            int has_digits;

            repeat_index = index + 1;
            repeat_value = 0;
            has_digits = 0;
            while (text[repeat_index] >= '0' && text[repeat_index] <= '9')
            {
                size_t digit;

                digit = static_cast<size_t>(text[repeat_index] - '0');
                if (repeat_value > (SIZE_MAX - digit) / 10)
                    repeat_value = SIZE_MAX;
                else
                    repeat_value = repeat_value * 10 + digit;
                has_digits = 1;
                repeat_index += 1;
            }
            if (!has_digits || text[repeat_index] != ')')
            {
                index += 1;
                last_symbol = '\0';
                continue ;
            }
            if (repeat_value == SIZE_MAX)
                return (SIZE_MAX);
            if (repeat_value > 0)
            {
                size_t increment;

                increment = repeat_value - 1;
                if (increment > SIZE_MAX - length)
                    return (SIZE_MAX);
                length += increment;
            }
            index = repeat_index + 1;
            continue ;
        }
        last_symbol = '\0';
        index += 1;
    }
    return (length);
}

size_t transpiler_semantics_picture_decimal_scale(const char *text)
{
    size_t index;
    size_t scale;
    int in_scale;
    char last_symbol;

    if (!text)
        return (0);
    index = 0;
    scale = 0;
    in_scale = 0;
    last_symbol = '\0';
    while (text[index] != '\0')
    {
        char value;

        value = text[index];
        if (value >= 'a' && value <= 'z')
            value = static_cast<char>(value - ('a' - 'A'));
        if (value == 'V')
        {
            in_scale = 1;
            last_symbol = value;
            index += 1;
            continue ;
        }
        if (value == '9')
        {
            if (in_scale && scale < SIZE_MAX)
                scale += 1;
            last_symbol = value;
            index += 1;
            continue ;
        }
        if (value == '(' && last_symbol == '9')
        {
            size_t repeat_index;
            size_t repeat_value;
            int has_digits;

            repeat_index = index + 1;
            repeat_value = 0;
            has_digits = 0;
            while (text[repeat_index] >= '0' && text[repeat_index] <= '9')
            {
                size_t digit;

                digit = static_cast<size_t>(text[repeat_index] - '0');
                if (repeat_value > (SIZE_MAX - digit) / 10)
                    repeat_value = SIZE_MAX;
                else
                    repeat_value = repeat_value * 10 + digit;
                has_digits = 1;
                repeat_index += 1;
            }
            if (!has_digits || text[repeat_index] != ')')
            {
                index += 1;
                last_symbol = '\0';
                continue ;
            }
            if (repeat_value == SIZE_MAX)
                return (SIZE_MAX);
            if (repeat_value > 0 && in_scale)
            {
                size_t increment;

                increment = repeat_value - 1;
                if (increment > SIZE_MAX - scale)
                    return (SIZE_MAX);
                scale += increment;
            }
            index = repeat_index + 1;
            last_symbol = '\0';
            continue ;
        }
        last_symbol = value;
        index += 1;
    }
    return (scale);
}

size_t transpiler_semantics_picture_alphanumeric_length(const char *text)
{
    size_t index;
    size_t length;
    char last_symbol;

    if (!text)
        return (0);
    index = 0;
    length = 0;
    last_symbol = '\0';
    while (text[index] != '\0')
    {
        char value;

        value = text[index];
        if (value >= 'a' && value <= 'z')
            value = static_cast<char>(value - ('a' - 'A'));
        if (value == 'X' || value == 'A' || value == 'Z')
        {
            if (length < SIZE_MAX)
                length += 1;
            last_symbol = value;
            index += 1;
            continue ;
        }
        if (value == '(' && last_symbol != '\0')
        {
            size_t repeat_index;
            size_t repeat_value;
            int has_digits;

            repeat_index = index + 1;
            repeat_value = 0;
            has_digits = 0;
            while (text[repeat_index] >= '0' && text[repeat_index] <= '9')
            {
                size_t digit;

                digit = static_cast<size_t>(text[repeat_index] - '0');
                if (repeat_value > (SIZE_MAX - digit) / 10)
                    repeat_value = SIZE_MAX;
                else
                    repeat_value = repeat_value * 10 + digit;
                has_digits = 1;
                repeat_index += 1;
            }
            if (!has_digits || text[repeat_index] != ')')
            {
                index += 1;
                last_symbol = '\0';
                continue ;
            }
            if (repeat_value == SIZE_MAX)
                return (SIZE_MAX);
            if (repeat_value > 0)
            {
                size_t increment;

                increment = repeat_value - 1;
                if (increment > SIZE_MAX - length)
                    return (SIZE_MAX);
                length += increment;
            }
            index = repeat_index + 1;
            continue ;
        }
        last_symbol = '\0';
        index += 1;
    }
    return (length);
}

size_t transpiler_semantics_literal_alphanumeric_length(const t_ast_node *literal)
{
    size_t length;

    if (!literal)
        return (0);
    if (literal->kind != AST_NODE_LITERAL)
        return (0);
    if (literal->token.kind != LEXER_TOKEN_STRING_LITERAL)
        return (0);
    if (!literal->token.lexeme)
        return (0);
    length = ft_strlen(literal->token.lexeme);
    if (length >= 2)
    {
        char first;
        char last;

        first = literal->token.lexeme[0];
        last = literal->token.lexeme[length - 1];
        if ((first == '"' && last == '"') || (first == '\'' && last == '\''))
        {
            if (length >= 2)
                length -= 2;
        }
    }
    return (length);
}

size_t transpiler_semantics_literal_numeric_length(const t_ast_node *literal)
{
    size_t index;
    size_t length;

    if (!literal)
        return (0);
    if (literal->kind != AST_NODE_LITERAL)
        return (0);
    if (literal->token.kind != LEXER_TOKEN_NUMERIC_LITERAL)
        return (0);
    if (!literal->token.lexeme)
        return (0);
    index = 0;
    length = 0;
    while (literal->token.lexeme[index] != '\0')
    {
        char value;

        value = literal->token.lexeme[index];
        if (value >= '0' && value <= '9')
        {
            if (length < SIZE_MAX)
                length += 1;
        }
        index += 1;
    }
    return (length);
}

size_t transpiler_semantics_literal_decimal_scale(const t_ast_node *literal)
{
    size_t index;
    size_t scale;
    int in_scale;

    if (!literal)
        return (0);
    if (literal->kind != AST_NODE_LITERAL)
        return (0);
    if (literal->token.kind != LEXER_TOKEN_NUMERIC_LITERAL)
        return (0);
    if (!literal->token.lexeme)
        return (0);
    index = 0;
    scale = 0;
    in_scale = 0;
    while (literal->token.lexeme[index] != '\0')
    {
        char value;

        value = literal->token.lexeme[index];
        if (value == '.' || value == 'V' || value == 'v')
        {
            in_scale = 1;
            index += 1;
            continue ;
        }
        if (value >= '0' && value <= '9')
        {
            if (in_scale && scale < SIZE_MAX)
                scale += 1;
        }
        index += 1;
    }
    return (scale);
}

t_transpiler_data_item_kind transpiler_semantics_convert_kind(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC)
        return (TRANSPILE_DATA_ITEM_ALPHANUMERIC);
    if (kind == TRANSPILE_SEMANTIC_DATA_BOOLEAN)
        return (TRANSPILE_DATA_ITEM_ALPHANUMERIC);
    if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        return (TRANSPILE_DATA_ITEM_NUMERIC);
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return (TRANSPILE_DATA_ITEM_FLOATING);
    return (TRANSPILE_DATA_ITEM_UNKNOWN);
}

t_transpiler_semantic_data_kind transpiler_semantics_kind_from_context(t_transpiler_data_item_kind kind)
{
    if (kind == TRANSPILE_DATA_ITEM_ALPHANUMERIC)
        return (TRANSPILE_SEMANTIC_DATA_ALPHANUMERIC);
    if (kind == TRANSPILE_DATA_ITEM_NUMERIC)
        return (TRANSPILE_SEMANTIC_DATA_NUMERIC);
    if (kind == TRANSPILE_DATA_ITEM_FLOATING)
        return (TRANSPILE_SEMANTIC_DATA_FLOATING);
    return (TRANSPILE_SEMANTIC_DATA_UNKNOWN);
}

int transpiler_semantics_is_numeric_kind(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_NUMERIC)
        return (1);
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return (1);
    return (0);
}

int transpiler_semantics_is_floating_kind(t_transpiler_semantic_data_kind kind)
{
    if (kind == TRANSPILE_SEMANTIC_DATA_FLOATING)
        return (1);
    return (0);
}

int transpiler_semantics_numeric_kinds_match(t_transpiler_semantic_data_kind left,
    t_transpiler_semantic_data_kind right)
{
    if (left == TRANSPILE_SEMANTIC_DATA_UNKNOWN
        || right == TRANSPILE_SEMANTIC_DATA_UNKNOWN)
        return (1);
    if (!transpiler_semantics_is_numeric_kind(left)
        || !transpiler_semantics_is_numeric_kind(right))
        return (left == right);
    if (left == right)
        return (1);
    return (0);
}

int transpiler_semantics_kinds_compatible(t_transpiler_semantic_data_kind left,
    t_transpiler_semantic_data_kind right)
{
    if (left == TRANSPILE_SEMANTIC_DATA_UNKNOWN || right == TRANSPILE_SEMANTIC_DATA_UNKNOWN)
        return (1);
    if (left == right)
        return (1);
    if ((left == TRANSPILE_SEMANTIC_DATA_NUMERIC && right == TRANSPILE_SEMANTIC_DATA_FLOATING)
        || (left == TRANSPILE_SEMANTIC_DATA_FLOATING && right == TRANSPILE_SEMANTIC_DATA_NUMERIC))
        return (1);
    return (0);
}
