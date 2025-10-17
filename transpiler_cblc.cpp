#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

typedef struct s_cobol_text_builder
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cobol_text_builder;

static void cobol_text_builder_init(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static void cobol_text_builder_dispose(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    if (builder->data)
        cma_free(builder->data);
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static int cobol_text_builder_reserve(t_cobol_text_builder *builder, size_t desired_capacity)
{
    char *new_data;

    if (!builder)
        return (FT_FAILURE);
    if (builder->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 128)
        desired_capacity = 128;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (builder->data && builder->length > 0)
        ft_memcpy(new_data, builder->data, builder->length);
    if (builder->data)
        cma_free(builder->data);
    builder->data = new_data;
    builder->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_span(t_cobol_text_builder *builder, const char *text, size_t length)
{
    if (!builder)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (cobol_text_builder_reserve(builder, builder->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_memcpy(builder->data + builder->length, text, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_string(t_cobol_text_builder *builder, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (cobol_text_builder_append_span(builder, text, ft_strlen(text)));
}

static int cobol_text_builder_append_line(t_cobol_text_builder *builder, const char *text)
{
    if (cobol_text_builder_append_string(builder, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_match_keyword(const char **cursor, const char *keyword)
{
    size_t length;

    if (!cursor || !*cursor || !keyword)
        return (0);
    length = ft_strlen(keyword);
    if (ft_strncmp(*cursor, keyword, length) != 0)
        return (0);
    if ((*cursor)[length] != '\0' && ((*cursor)[length] == '_' || ((*cursor)[length] >= 'a'
                && (*cursor)[length] <= 'z') || ((*cursor)[length] >= 'A'
                && (*cursor)[length] <= 'Z') || ((*cursor)[length] >= '0'
                && (*cursor)[length] <= '9')))
        return (0);
    *cursor += length;
    return (1);
}

static void cblc_skip_whitespace(const char **cursor)
{
    if (!cursor || !*cursor)
        return ;
    while (**cursor == ' ' || **cursor == '\t' || **cursor == '\n' || **cursor == '\r')
        *cursor += 1;
}

static int cblc_parse_identifier(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    length = 0;
    while ((*cursor)[length] == '_' || ((*cursor)[length] >= 'a'
            && (*cursor)[length] <= 'z') || ((*cursor)[length] >= 'A'
            && (*cursor)[length] <= 'Z') || ((*cursor)[length] >= '0'
            && (*cursor)[length] <= '9'))
        length += 1;
    if (length == 0)
        return (FT_FAILURE);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    ft_memcpy(buffer, *cursor, length);
    buffer[length] = '\0';
    *cursor += length;
    return (FT_SUCCESS);
}

static void cblc_identifier_to_cobol(const char *identifier, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!identifier || !buffer || buffer_size == 0)
        return ;
    index = 0;
    while (identifier[index] != '\0' && index + 1 < buffer_size)
    {
        char character;

        character = identifier[index];
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - 'a' + 'A');
        else if (character == '_')
            character = '-';
        buffer[index] = character;
        index += 1;
    }
    buffer[index] = '\0';
}

static int cblc_parse_numeric_literal(const char **cursor, size_t *out_value)
{
    size_t value;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    value = 0;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
    }
    *out_value = value;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_data_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_data_item *new_items;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->data_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_items = static_cast<t_cblc_data_item *>(cma_calloc(desired_capacity,
            sizeof(*new_items)));
    if (!new_items)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->data_count)
    {
        new_items[index] = unit->data_items[index];
        index += 1;
    }
    if (unit->data_items)
        cma_free(unit->data_items);
    unit->data_items = new_items;
    unit->data_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_statement_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_statement *new_statements;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->statement_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_statements = static_cast<t_cblc_statement *>(cma_calloc(desired_capacity,
            sizeof(*new_statements)));
    if (!new_statements)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->statement_count)
    {
        new_statements[index] = unit->statements[index];
        index += 1;
    }
    if (unit->statements)
        cma_free(unit->statements);
    unit->statements = new_statements;
    unit->statement_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_parse_char_literal(const char **cursor, char *out_value)
{
    char value;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    if (**cursor != '\'')
        return (FT_FAILURE);
    *cursor += 1;
    if (**cursor == '\0')
        return (FT_FAILURE);
    if (**cursor == '\\')
    {
        *cursor += 1;
        if (**cursor == 'n')
            value = '\n';
        else if (**cursor == 'r')
            value = '\r';
        else if (**cursor == 't')
            value = '\t';
        else if (**cursor == '\\')
            value = '\\';
        else if (**cursor == '\'')
            value = '\'';
        else if (**cursor == '"')
            value = '"';
        else
            return (FT_FAILURE);
    }
    else
    {
        value = **cursor;
    }
    if (**cursor == '\0')
        return (FT_FAILURE);
    *cursor += 1;
    if (**cursor != '\'')
        return (FT_FAILURE);
    *cursor += 1;
    *out_value = value;
    return (FT_SUCCESS);
}

static int cblc_parse_char_declaration(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t length;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "char"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    length = 1;
    if (**cursor == '[')
    {
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_numeric_literal(cursor, &length) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != ']')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(unit->data_items[unit->data_count].source_name, identifier,
        sizeof(unit->data_items[unit->data_count].source_name));
    cblc_identifier_to_cobol(identifier, unit->data_items[unit->data_count].cobol_name,
        sizeof(unit->data_items[unit->data_count].cobol_name));
    unit->data_items[unit->data_count].length = length;
    unit->data_items[unit->data_count].kind = CBLC_DATA_KIND_CHAR;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_int_declaration(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "int"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(unit->data_items[unit->data_count].source_name, identifier,
        sizeof(unit->data_items[unit->data_count].source_name));
    cblc_identifier_to_cobol(identifier, unit->data_items[unit->data_count].cobol_name,
        sizeof(unit->data_items[unit->data_count].cobol_name));
    unit->data_items[unit->data_count].length = 0;
    unit->data_items[unit->data_count].kind = CBLC_DATA_KIND_INT;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (**cursor != '"')
        return (FT_FAILURE);
    *cursor += 1;
    index = 0;
    while (**cursor != '\0' && **cursor != '"')
    {
        if (index + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[index] = **cursor;
        index += 1;
        *cursor += 1;
    }
    if (**cursor != '"')
        return (FT_FAILURE);
    buffer[index] = '\0';
    *cursor += 1;
    return (FT_SUCCESS);
}

static t_cblc_data_item *cblc_find_data_item(t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->data_count)
    {
        if (ft_strncmp(unit->data_items[index].source_name, identifier,
                sizeof(unit->data_items[index].source_name)) == 0)
            return (&unit->data_items[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_append_statement(t_cblc_translation_unit *unit, t_cblc_statement_type type,
    const char *target, const char *source, int is_literal)
{
    t_cblc_statement *statement;

    if (!unit)
        return (FT_FAILURE);
    if (unit->statement_count >= unit->statement_capacity)
    {
        if (cblc_translation_unit_ensure_statement_capacity(unit,
                unit->statement_capacity == 0 ? 4 : unit->statement_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    statement = &unit->statements[unit->statement_count];
    statement->type = type;
    statement->is_literal = is_literal;
    statement->target[0] = '\0';
    statement->source[0] = '\0';
    if (target)
        ft_strlcpy(statement->target, target, sizeof(statement->target));
    if (source)
        ft_strlcpy(statement->source, source, sizeof(statement->source));
    unit->statement_count += 1;
    return (FT_SUCCESS);
}

static int cblc_expression_append(char *buffer, size_t buffer_size, const char *token)
{
    size_t length;
    size_t token_length;

    if (!buffer || !token)
        return (FT_FAILURE);
    length = ft_strlen(buffer);
    if (length > 0)
    {
        if (length + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[length] = ' ';
        length += 1;
        buffer[length] = '\0';
    }
    token_length = ft_strlen(token);
    if (length + token_length >= buffer_size)
        return (FT_FAILURE);
    ft_memcpy(buffer + length, token, token_length);
    buffer[length + token_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_parse_numeric_expression(const char **cursor, t_cblc_translation_unit *unit,
    char *buffer, size_t buffer_size)
{
    int expect_operand;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    expect_operand = 1;
    while (**cursor != '\0' && **cursor != ';')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '\0' || **cursor == ';')
            break ;
        if (expect_operand)
        {
            if (**cursor >= '0' && **cursor <= '9')
            {
                char number_token[32];
                size_t length;

                length = 0;
                while ((*cursor)[length] >= '0' && (*cursor)[length] <= '9'
                    && length + 1 < sizeof(number_token))
                {
                    number_token[length] = (*cursor)[length];
                    length += 1;
                }
                if (length == 0)
                    return (FT_FAILURE);
                number_token[length] = '\0';
                if (cblc_expression_append(buffer, buffer_size, number_token) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += length;
                expect_operand = 0;
                continue ;
            }
            if ((**cursor >= 'a' && **cursor <= 'z') || (**cursor >= 'A' && **cursor <= 'Z')
                || **cursor == '_')
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];
                t_cblc_data_item *item;

                if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
                    return (FT_FAILURE);
                item = cblc_find_data_item(unit, identifier);
                if (!item || item->kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, item->cobol_name) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            return (FT_FAILURE);
        }
        if (**cursor == '+' || **cursor == '-' || **cursor == '*' || **cursor == '/')
        {
            char operator_buffer[2];

            operator_buffer[0] = **cursor;
            operator_buffer[1] = '\0';
            if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            *cursor += 1;
            expect_operand = 1;
            continue ;
        }
        return (FT_FAILURE);
    }
    if (expect_operand)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit)
{
    char target_identifier[TRANSPILE_IDENTIFIER_MAX];
    char source_identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char expression_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *target_item;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, target_identifier, sizeof(target_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '=')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    target_item = cblc_find_data_item(unit, target_identifier);
    if (!target_item)
        return (FT_FAILURE);
    if (target_item->kind == CBLC_DATA_KIND_CHAR)
    {
        t_cblc_data_item *source_item;

        if (**cursor == '"')
        {
            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            cobol_source[0] = '\"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(unit, CBLC_STATEMENT_ASSIGNMENT,
                    target_item->cobol_name, cobol_source, 1));
        }
        if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            cobol_source[0] = '\"';
            cobol_source[1] = '\0';
            if (literal_value == '"')
                ft_strlcat(cobol_source, "\"\"", sizeof(cobol_source));
            else
                ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(unit, CBLC_STATEMENT_ASSIGNMENT,
                    target_item->cobol_name, cobol_source, 1));
        }
        if (cblc_parse_identifier(cursor, source_identifier, sizeof(source_identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        source_item = cblc_find_data_item(unit, source_identifier);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_item->cobol_name, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(unit, CBLC_STATEMENT_ASSIGNMENT,
                target_item->cobol_name, cobol_source, 0));
    }
    if (target_item->kind == CBLC_DATA_KIND_INT)
    {
        if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                sizeof(expression_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(unit, CBLC_STATEMENT_COMPUTE,
                target_item->cobol_name, expression_buffer, 0));
    }
    return (FT_FAILURE);
}

static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char cobol_argument[TRANSPILE_IDENTIFIER_MAX];
    int is_literal;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "display"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cobol_argument[0] = '\"';
        cobol_argument[1] = '\0';
        ft_strlcat(cobol_argument, literal_buffer, sizeof(cobol_argument));
        ft_strlcat(cobol_argument, "\"", sizeof(cobol_argument));
        is_literal = 1;
    }
    else
    {
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_identifier_to_cobol(identifier, cobol_argument, sizeof(cobol_argument));
        is_literal = 0;
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_append_statement(unit, CBLC_STATEMENT_DISPLAY, NULL, cobol_argument, is_literal));
}

static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit)
{
    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "return"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    unit->saw_return = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_function(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "function"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (!cblc_match_keyword(cursor, "void"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (unit->program_name[0] == '\0')
        cblc_identifier_to_cobol(identifier, unit->program_name, sizeof(unit->program_name));
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    *cursor += 1;
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '}')
        {
            *cursor += 1;
            return (FT_SUCCESS);
        }
        if (cblc_match_keyword(cursor, "return"))
        {
            (*cursor) -= ft_strlen("return");
            if (cblc_parse_return(cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(cursor, "display"))
        {
            (*cursor) -= ft_strlen("display");
            if (cblc_parse_display(cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_assignment(cursor, unit) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_FAILURE);
}

void cblc_translation_unit_init(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->statements = NULL;
    unit->statement_count = 0;
    unit->statement_capacity = 0;
    unit->program_name[0] = '\0';
    unit->saw_return = 0;
}

void cblc_translation_unit_dispose(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    if (unit->data_items)
        cma_free(unit->data_items);
    if (unit->statements)
        cma_free(unit->statements);
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->statements = NULL;
    unit->statement_count = 0;
    unit->statement_capacity = 0;
    unit->program_name[0] = '\0';
    unit->saw_return = 0;
}

int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit)
{
    const char *cursor;

    if (!text || !unit)
        return (FT_FAILURE);
    cursor = text;
    while (cursor && *cursor != '\0')
    {
        cblc_skip_whitespace(&cursor);
        if (*cursor == '\0')
            break ;
        if (cblc_match_keyword(&cursor, "function"))
        {
            cursor -= ft_strlen("function");
            if (cblc_parse_function(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "char"))
        {
            cursor -= ft_strlen("char");
            if (cblc_parse_char_declaration(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "int"))
        {
            cursor -= ft_strlen("int");
            if (cblc_parse_int_declaration(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        return (FT_FAILURE);
    }
    if (unit->program_name[0] == '\0')
        ft_strlcpy(unit->program_name, "MAIN", sizeof(unit->program_name));
    return (FT_SUCCESS);
}

int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    t_cobol_text_builder builder;
    char line[256];
    size_t index;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    cobol_text_builder_init(&builder);
    if (cobol_text_builder_append_line(&builder, "       IDENTIFICATION DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (pf_snprintf(line, sizeof(line), "       PROGRAM-ID. %s.", unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       ENVIRONMENT DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       DATA DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       WORKING-STORAGE SECTION.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->data_count)
    {
        if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (pf_snprintf(line, sizeof(line), "       01 %s PIC X(%zu).",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].length) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_INT)
        {
            if (pf_snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (cobol_text_builder_append_line(&builder, "       PROCEDURE DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "MAIN.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->statement_count)
    {
        const t_cblc_statement *statement;

        statement = &unit->statements[index];
        if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            if (pf_snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    statement->source, statement->target) < 0)
                goto cleanup;
        }
        else if (statement->type == CBLC_STATEMENT_DISPLAY)
        {
            if (pf_snprintf(line, sizeof(line), "           DISPLAY %s",
                    statement->source) < 0)
                goto cleanup;
        }
        else if (statement->type == CBLC_STATEMENT_COMPUTE)
        {
            if (pf_snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                    statement->target, statement->source) < 0)
                goto cleanup;
        }
        else
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
        goto cleanup;
    *out_text = builder.data;
    builder.data = NULL;
cleanup:
    cobol_text_builder_dispose(&builder);
    if (!*out_text)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
