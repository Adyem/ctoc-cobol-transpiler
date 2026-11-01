static int cblc_expression_append(char *buffer, size_t buffer_size, const char *token)
{
    size_t length;
    size_t token_length;

    if (!buffer || !token)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length > 0)
    {
        if (length + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[length] = ' ';
        length += 1;
        buffer[length] = '\0';
    }
    token_length = std::strlen(token);
    if (length + token_length >= buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer + length, token, token_length);
    buffer[length + token_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_sizeof_data_kind(const t_cblc_translation_unit *unit, t_cblc_data_kind kind,
    size_t length, size_t array_count, const char *struct_type_name, size_t *out_size);

static int cblc_sizeof_struct_type(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, size_t *out_size)
{
    size_t index;
    size_t total_size;

    if (!unit || !type || !out_size)
        return (FT_FAILURE);
    index = 0;
    total_size = 0;
    while (index < type->field_count)
    {
        size_t field_size;

        if (cblc_sizeof_data_kind(unit, type->fields[index].kind,
                type->fields[index].length, type->fields[index].array_count,
                type->fields[index].struct_type_name, &field_size) != FT_SUCCESS)
            return (FT_FAILURE);
        total_size += field_size;
        index += 1;
    }
    *out_size = total_size;
    return (FT_SUCCESS);
}

static int cblc_sizeof_data_kind(const t_cblc_translation_unit *unit, t_cblc_data_kind kind,
    size_t length, size_t array_count, const char *struct_type_name, size_t *out_size)
{
    size_t base_size;

    if (!out_size)
        return (FT_FAILURE);
    base_size = 0;
    if (kind == CBLC_DATA_KIND_CHAR)
    {
        if (length == 0)
            base_size = 1;
        else
            base_size = length;
    }
    else if (kind == CBLC_DATA_KIND_INT)
        base_size = 4;
    else if (kind == CBLC_DATA_KIND_CHAR_BUFFER)
    {
        if (length == 0)
            base_size = 5;
        else
            base_size = length + 4;
    }
    else if (cblc_data_kind_is_pointer(kind))
        base_size = 8;
    else if (kind == CBLC_DATA_KIND_STRUCT)
    {
        const t_cblc_struct_type *type;

        if (!unit || !struct_type_name || struct_type_name[0] == '\0')
            return (FT_FAILURE);
        type = cblc_find_struct_type(unit, struct_type_name);
        if (!type)
        {
            size_t template_parameter_index;

            if (cblc_active_template_parameter_index(struct_type_name,
                    &template_parameter_index) != FT_SUCCESS)
                return (FT_FAILURE);
            base_size = 4;
        }
        else if (cblc_sizeof_struct_type(unit, type, &base_size) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
        return (FT_FAILURE);
    if (array_count > 0)
        base_size *= array_count;
    *out_size = base_size;
    return (FT_SUCCESS);
}

static int cblc_parse_sizeof_expression(const char **cursor, t_cblc_translation_unit *unit,
    char *buffer, size_t buffer_size)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t size_value;
    t_cblc_data_kind pointer_kind;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "sizeof"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    size_value = 0;
    if (cblc_parse_pointer_type(cursor, &pointer_kind) == FT_SUCCESS)
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '*')
        {
            *cursor += 1;
            if (cblc_sizeof_data_kind(unit, pointer_kind, 0, 0, NULL,
                    &size_value) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
        else
        {
            if (pointer_kind == CBLC_DATA_KIND_CHAR_POINTER)
                size_value = 1;
            else if (pointer_kind == CBLC_DATA_KIND_INT_POINTER)
                size_value = 4;
            else
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
    }
    else
    {
        const t_cblc_data_item *item;
        const t_cblc_struct_type *type;

        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        {
            size_t template_parameter_index;

            if (cblc_active_template_parameter_index(identifier,
                    &template_parameter_index) == FT_SUCCESS)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor != ')'
                    || std::snprintf(buffer, buffer_size, "CBLC-TPL-SIZE-%s",
                        identifier) < 0)
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
                *cursor += 1;
                return (FT_SUCCESS);
            }
        }
        item = cblc_find_data_item_const(unit, identifier);
        if (item)
        {
            if (cblc_sizeof_data_kind(unit, item->kind, item->length, item->array_count,
                    item->struct_type_name, &size_value) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
        else
        {
            type = cblc_find_struct_type(unit, identifier);
            if (!type || cblc_sizeof_struct_type(unit, type, &size_value) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (std::snprintf(buffer, buffer_size, "%zu", size_value) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
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
            if (std::strncmp(*cursor, "std::", 5) == 0
                && (std::strncmp(*cursor, "std::fabs", 9) == 0
                    || std::strncmp(*cursor, "std::floor", 10) == 0
                    || std::strncmp(*cursor, "std::ceil", 9) == 0
                    || std::strncmp(*cursor, "std::exp", 8) == 0
                    || std::strncmp(*cursor, "std::log", 8) == 0
                    || std::strncmp(*cursor, "std::sin", 8) == 0
                    || std::strncmp(*cursor, "std::cos", 8) == 0
                    || std::strncmp(*cursor, "std::tan", 8) == 0
                    || std::strncmp(*cursor, "std::sqrt", 9) == 0
                    || std::strncmp(*cursor, "std::pow", 8) == 0
                    || std::strncmp(*cursor, "std::ord", 8) == 0
                    || std::strncmp(*cursor, "std::odd", 8) == 0
                    || std::strncmp(*cursor, "std::date_serial", 16) == 0))
            {
                char intrinsic[TRANSPILE_IDENTIFIER_MAX];
                char argument[TRANSPILE_IDENTIFIER_MAX];
                char second_argument[TRANSPILE_IDENTIFIER_MAX];
                char mapped[TRANSPILE_STATEMENT_TEXT_MAX];
                const t_cblc_data_item *argument_item;
                const char *cobol_name;

                if (cblc_parse_identifier(cursor, intrinsic, sizeof(intrinsic)) != FT_SUCCESS)
                    return (FT_FAILURE);
                cblc_skip_whitespace(cursor);
                if (**cursor != '(')
                    return (FT_FAILURE);
                *cursor += 1;
                cblc_skip_whitespace(cursor);
                if (std::strcmp(intrinsic, "std::ord") == 0)
                {
                    if (**cursor == '\'')
                    {
                        char literal_value;

                        if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                            return (FT_FAILURE);
                        cblc_skip_whitespace(cursor);
                        if (**cursor != ')')
                            return (FT_FAILURE);
                        *cursor += 1;
                        if (std::snprintf(mapped, sizeof(mapped), "FUNCTION ORD('%c')",
                                literal_value) < 0
                            || cblc_expression_append(buffer, buffer_size, mapped)
                                != FT_SUCCESS)
                            return (FT_FAILURE);
                        expect_operand = 0;
                        continue ;
                    }
                    t_cblc_data_item *ord_item;
                    int ord_is_length;
                    char ord_reference[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_ord_reference[TRANSPILE_STATEMENT_TEXT_MAX];
                    char ord_base[TRANSPILE_IDENTIFIER_MAX];
                    char ord_index[TRANSPILE_STATEMENT_TEXT_MAX];
                    char *ord_open;
                    char *ord_close;
                    size_t ord_base_length;
                    size_t ord_index_length;

                    if (cblc_parse_data_reference_text(cursor, unit, &ord_item,
                            &ord_is_length, ord_reference, sizeof(ord_reference)) != FT_SUCCESS
                        || ord_is_length
                        || (ord_item->kind != CBLC_DATA_KIND_CHAR
                            && ord_item->kind != CBLC_DATA_KIND_CHAR_POINTER
                            && ord_item->kind != CBLC_DATA_KIND_CHAR_BUFFER))
                        return (FT_FAILURE);
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ')')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (cblc_translate_expression_for_cobol(ord_reference,
                            translated_ord_reference, sizeof(translated_ord_reference))
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                    ord_open = std::strchr(translated_ord_reference, '(');
                    ord_close = std::strrchr(translated_ord_reference, ')');
                    if (!ord_open)
                    {
                        if (std::snprintf(mapped, sizeof(mapped), "FUNCTION ORD(%s)",
                                translated_ord_reference) < 0
                            || cblc_expression_append(buffer, buffer_size, mapped)
                                != FT_SUCCESS)
                            return (FT_FAILURE);
                        expect_operand = 0;
                        continue ;
                    }
                    if (!ord_open || !ord_close || ord_close <= ord_open
                        || static_cast<size_t>(ord_open - translated_ord_reference)
                            >= sizeof(ord_base)
                        || static_cast<size_t>(ord_close - ord_open - 1)
                            >= sizeof(ord_index))
                        return (FT_FAILURE);
                    ord_base_length = static_cast<size_t>(ord_open - translated_ord_reference);
                    std::memcpy(ord_base, translated_ord_reference, ord_base_length);
                    ord_base[ord_base_length] = '\0';
                    if (ord_item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
                        ft_strlcat(ord_base, "-BUF", sizeof(ord_base));
                    ord_index_length = static_cast<size_t>(ord_close - ord_open - 1);
                    std::memcpy(ord_index, ord_open + 1, ord_index_length);
                    ord_index[ord_index_length] = '\0';
                    if (std::snprintf(mapped, sizeof(mapped), "FUNCTION ORD(%s(%s:1))",
                            ord_base, ord_index) < 0
                        || cblc_expression_append(buffer, buffer_size, mapped) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (cblc_parse_identifier(cursor, argument, sizeof(argument)) != FT_SUCCESS)
                    return (FT_FAILURE);
                argument_item = cblc_find_data_item(unit, argument);
                if (!argument_item || (argument_item->kind != CBLC_DATA_KIND_INT
                        && argument_item->kind != CBLC_DATA_KIND_DOUBLE))
                    return (FT_FAILURE);
                cobol_name = argument_item->cobol_name;
                cblc_skip_whitespace(cursor);
                if (std::strcmp(intrinsic, "std::pow") == 0)
                {
                    const t_cblc_data_item *second_item;

                    if (**cursor != ',')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_identifier(cursor, second_argument,
                            sizeof(second_argument)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    second_item = cblc_find_data_item(unit, second_argument);
                    if (!second_item || (second_item->kind != CBLC_DATA_KIND_INT
                            && second_item->kind != CBLC_DATA_KIND_DOUBLE))
                        return (FT_FAILURE);
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ')')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (std::snprintf(mapped, sizeof(mapped), "%s ** %s", cobol_name,
                            second_item->cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cblc_expression_append(buffer, buffer_size, mapped) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (**cursor != ')')
                    return (FT_FAILURE);
                *cursor += 1;
                if (std::strcmp(intrinsic, "std::fabs") == 0)
                    ft_strlcpy(mapped, "FUNCTION ABS(", sizeof(mapped));
                else if (std::strcmp(intrinsic, "std::floor") == 0)
                    ft_strlcpy(mapped, "FUNCTION INTEGER-PART(", sizeof(mapped));
                else if (std::strcmp(intrinsic, "std::ceil") == 0)
                    ft_strlcpy(mapped, "FUNCTION INTEGER-PART(", sizeof(mapped));
                else if (std::strcmp(intrinsic, "std::date_serial") == 0)
                    ft_strlcpy(mapped, "FUNCTION INTEGER-OF-DATE(", sizeof(mapped));
                else if (std::strcmp(intrinsic, "std::odd") == 0)
                {
                    if (std::snprintf(mapped, sizeof(mapped), "FUNCTION MOD(%s, 2)",
                            cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cblc_expression_append(buffer, buffer_size, mapped) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                else
                {
                    const char *function_name;

                    function_name = std::strcmp(intrinsic, "std::exp") == 0 ? "EXP"
                        : (std::strcmp(intrinsic, "std::log") == 0 ? "LOG"
                            : (std::strcmp(intrinsic, "std::sin") == 0 ? "SIN"
                                : (std::strcmp(intrinsic, "std::cos") == 0 ? "COS"
                                    : (std::strcmp(intrinsic, "std::tan") == 0 ? "TAN" : "SQRT"))));
                    if (std::snprintf(mapped, sizeof(mapped), "FUNCTION %s(", function_name) < 0)
                        return (FT_FAILURE);
                }
                ft_strlcat(mapped, cobol_name, sizeof(mapped));
                ft_strlcat(mapped, ")", sizeof(mapped));
                if (cblc_expression_append(buffer, buffer_size, mapped) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            if (std::strncmp(*cursor, "sizeof", std::strlen("sizeof")) == 0)
            {
                char sizeof_buffer[32];

                if (cblc_parse_sizeof_expression(cursor, unit, sizeof_buffer,
                        sizeof(sizeof_buffer)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, sizeof_buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            if (std::strncmp(*cursor, "std::abs", std::strlen("std::abs")) == 0)
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::strncmp(identifier, "std::abs", sizeof(identifier)) == 0)
                {
                    char function_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char argument_identifier[TRANSPILE_IDENTIFIER_MAX];
                    char cobol_argument[TRANSPILE_IDENTIFIER_MAX];
                    t_cblc_data_item *argument_item;

                    cblc_skip_whitespace(cursor);
                    if (**cursor != '(')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_identifier(cursor, argument_identifier,
                            sizeof(argument_identifier)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    argument_item = cblc_find_data_item(unit, argument_identifier);
                    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_INT)
                        return (FT_FAILURE);
                    ft_strlcpy(cobol_argument, argument_item->cobol_name,
                        sizeof(cobol_argument));
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ')')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (std::snprintf(function_buffer, sizeof(function_buffer),
                            "FUNCTION ABS(%s)", cobol_argument) < 0)
                        return (FT_FAILURE);
                    if (cblc_expression_append(buffer, buffer_size, function_buffer) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                return (FT_FAILURE);
            }
            if (**cursor >= '0' && **cursor <= '9')
            {
                char number_token[32];
                size_t length;

                length = 0;
                while (((((*cursor)[length] >= '0' && (*cursor)[length] <= '9')
                    || (*cursor)[length] == '.')
                    && length + 1 < sizeof(number_token)))
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
            if (**cursor == '+' || **cursor == '-')
            {
                char operator_buffer[2];

                operator_buffer[0] = **cursor;
                operator_buffer[1] = '\0';
                if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += 1;
                continue ;
            }
            if ((**cursor >= 'a' && **cursor <= 'z') || (**cursor >= 'A' && **cursor <= 'Z')
                || **cursor == '_')
            {
                t_cblc_data_item *item;
                int is_length_reference;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
                        reference, sizeof(reference))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_length_reference)
                {
                    if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (item->kind != CBLC_DATA_KIND_INT
                    && item->kind != CBLC_DATA_KIND_DOUBLE
                    && item->kind != CBLC_DATA_KIND_CHAR
                    && !(item->kind == CBLC_DATA_KIND_INT_POINTER
                        && (std::strchr(reference, '[') || std::strchr(reference, '(')))
                    && !(item->kind == CBLC_DATA_KIND_CHAR_POINTER
                        && (std::strchr(reference, '[') || std::strchr(reference, '(')))
                    && !(item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                        && (std::strchr(reference, '[') || std::strchr(reference, '('))))
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            if (**cursor == '*')
            {
                t_cblc_data_item *item;
                t_cblc_data_kind dereference_kind;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_pointer_dereference_reference(cursor, unit, &item,
                        &dereference_kind, reference, sizeof(reference)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (dereference_kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
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

static int cblc_parse_numeric_expression_until_paren(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size)
{
    int expect_operand;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    expect_operand = 1;
    while (**cursor != '\0' && **cursor != ')' && **cursor != ',')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '\0' || **cursor == ')' || **cursor == ',')
            break ;
        if (expect_operand)
        {
            if (std::strncmp(*cursor, "sizeof", std::strlen("sizeof")) == 0)
            {
                char sizeof_buffer[32];

                if (cblc_parse_sizeof_expression(cursor, unit, sizeof_buffer,
                        sizeof(sizeof_buffer)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, sizeof_buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            if (std::strncmp(*cursor, "std::abs", std::strlen("std::abs")) == 0)
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::strncmp(identifier, "std::abs", sizeof(identifier)) == 0)
                {
                    char function_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char argument_identifier[TRANSPILE_IDENTIFIER_MAX];
                    char cobol_argument[TRANSPILE_IDENTIFIER_MAX];
                    t_cblc_data_item *argument_item;

                    cblc_skip_whitespace(cursor);
                    if (**cursor != '(')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_identifier(cursor, argument_identifier,
                            sizeof(argument_identifier)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    argument_item = cblc_find_data_item(unit, argument_identifier);
                    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_INT)
                        return (FT_FAILURE);
                    ft_strlcpy(cobol_argument, argument_item->cobol_name,
                        sizeof(cobol_argument));
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ')')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (std::snprintf(function_buffer, sizeof(function_buffer),
                            "FUNCTION ABS(%s)", cobol_argument) < 0)
                        return (FT_FAILURE);
                    if (cblc_expression_append(buffer, buffer_size, function_buffer) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                return (FT_FAILURE);
            }
            if (**cursor >= '0' && **cursor <= '9')
            {
                char number_token[32];
                size_t length;

                length = 0;
                while (((((*cursor)[length] >= '0' && (*cursor)[length] <= '9')
                    || (*cursor)[length] == '.')
                    && length + 1 < sizeof(number_token)))
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
            if (**cursor == '+' || **cursor == '-')
            {
                char operator_buffer[2];

                operator_buffer[0] = **cursor;
                operator_buffer[1] = '\0';
                if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += 1;
                continue ;
            }
            if ((**cursor >= 'a' && **cursor <= 'z') || (**cursor >= 'A' && **cursor <= 'Z')
                || **cursor == '_')
            {
                t_cblc_data_item *item;
                int is_length_reference;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
                        reference, sizeof(reference))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_length_reference)
                {
                    if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (item->kind != CBLC_DATA_KIND_INT
                    && item->kind != CBLC_DATA_KIND_DOUBLE
                    && item->kind != CBLC_DATA_KIND_CHAR
                    && !(item->kind == CBLC_DATA_KIND_INT_POINTER
                        && (std::strchr(reference, '[') || std::strchr(reference, '(')))
                    && !(item->kind == CBLC_DATA_KIND_CHAR_POINTER
                        && (std::strchr(reference, '[') || std::strchr(reference, '(')))
                    && !(item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                        && (std::strchr(reference, '[') || std::strchr(reference, '('))))
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            if (**cursor == '*')
            {
                t_cblc_data_item *item;
                t_cblc_data_kind dereference_kind;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_pointer_dereference_reference(cursor, unit, &item,
                        &dereference_kind, reference, sizeof(reference)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (dereference_kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
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

static int cblc_parse_method_call_argument_list(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    char *buffer, size_t buffer_size, size_t *out_count);
static int cblc_destroy_method_argument_temporaries(t_cblc_function *function,
    size_t cleanup_base);

static int cblc_parse_function_call_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    const t_cblc_function *called_function;
    t_cblc_statement *statement;
    size_t argument_count;
    size_t argument_cleanup_base;
    char target_source_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    ft_strlcpy(target_source_name, target_item->source_name, sizeof(target_source_name));
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_parse_explicit_function_application(cursor, unit, identifier,
            sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_item = cblc_find_data_item(unit, target_source_name);
    if (!target_item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    argument_cleanup_base = function->local_destructor_count;
    if (cblc_parse_method_call_argument_list(cursor, unit, function,
            call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    called_function = cblc_find_function_in_unit(unit, identifier);
    if (!called_function)
    {
        const t_cblc_function *template_function;
        char inferred_argument[TRANSPILE_IDENTIFIER_MAX];

        template_function = cblc_find_template_function(unit, identifier);
        if (template_function
            && cblc_infer_template_function_argument(unit, template_function,
                call_arguments, argument_count, inferred_argument,
                sizeof(inferred_argument)) == FT_SUCCESS)
        {
            size_t argument_ref_id;

            called_function = cblc_instantiate_template_function(unit, identifier,
                inferred_argument);
            if (!called_function)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            if (cblc_template_argument_ref(unit, inferred_argument, &argument_ref_id)
                == FT_SUCCESS)
                cblc_record_function_template_use(unit, identifier, argument_ref_id,
                    cblc_source_offset(unit, start));
            ft_strlcpy(identifier, called_function->source_name, sizeof(identifier));
            target_item = cblc_find_data_item(unit, target_source_name);
            if (!target_item)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
    }
    if (called_function && !cblc_function_return_matches_target(called_function, target_item))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (called_function
        && !cblc_call_arguments_match_parameters(unit, call_arguments, argument_count,
            called_function->parameters, called_function->parameter_count))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (called_function)
    {
        if (called_function->return_cobol_name[0] == '\0')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (called_function->parameter_count != argument_count)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
            target_item->cobol_name,
            called_function ? called_function->return_cobol_name : NULL, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (function->statement_count == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier,
        sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    statement->call_is_external = called_function ? 0 : 1;
    if (cblc_destroy_method_argument_temporaries(function,
            argument_cleanup_base) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    return (FT_SUCCESS);
}

static int cblc_destroy_method_argument_temporaries(t_cblc_function *function,
    size_t cleanup_base)
{
    if (!function || cleanup_base > function->local_destructor_count)
        return (FT_FAILURE);
    while (function->local_destructor_count > cleanup_base)
    {
        size_t cleanup_index;

        cleanup_index = function->local_destructor_count - 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                function->local_destructor_targets[cleanup_index], NULL, 0)
            != FT_SUCCESS)
            return (FT_FAILURE);
        function->local_destructor_count = cleanup_index;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_class_addition_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    const char *operator_start;
    t_cblc_data_item *left_item;
    t_cblc_data_item *right_item;
    t_cblc_data_item *result_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    t_cblc_statement *statement;
    char left_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    char right_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char normalized_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t argument_count;
    size_t original_data_count;
    size_t original_statement_count;
    size_t cleanup_base;
    size_t chain_index;

    if (!cursor || !*cursor || !unit || !function || !target_item
        || target_item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_FAILURE);
    start = *cursor;
    original_data_count = unit->data_count;
    original_statement_count = function->statement_count;
    cleanup_base = function->local_destructor_count;
    chain_index = 0;
    if (cblc_parse_data_reference_text(cursor, unit, &left_item, NULL,
            left_reference, sizeof(left_reference)) != FT_SUCCESS
        || !left_item || left_item->kind != CBLC_DATA_KIND_STRUCT)
        goto fail;
    if (std::strcmp(target_item->struct_type_name,
            left_item->struct_type_name) != 0)
        goto fail;
    for (;;)
    {
        char temporary_base[TRANSPILE_IDENTIFIER_MAX];
        char temporary_source[TRANSPILE_IDENTIFIER_MAX];
        char temporary_cobol[TRANSPILE_IDENTIFIER_MAX];
        const t_cblc_struct_type *result_type;
        int needs_destructor;

        cblc_skip_whitespace(cursor);
        if (**cursor != '+' || (*cursor)[1] == '=')
            goto fail;
        operator_start = *cursor;
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_data_reference_text(cursor, unit, &right_item, NULL,
                right_reference, sizeof(right_reference)) != FT_SUCCESS
            || !right_item || right_item->kind != CBLC_DATA_KIND_STRUCT
            || std::strcmp(left_item->struct_type_name,
                right_item->struct_type_name) != 0)
            goto fail;
        cblc_skip_whitespace(cursor);
        if (**cursor != '+' && **cursor != ';')
            goto fail;
        if (chain_index == static_cast<size_t>(-1))
            goto fail;
        if (std::snprintf(temporary_base, sizeof(temporary_base),
                "CBLC_OPERATOR_PLUS_TEMP_%zu_%zu", function->statement_count,
                chain_index) < 0
            || cblc_build_unique_local_storage_names(unit, function,
                temporary_base, temporary_source, sizeof(temporary_source),
                temporary_cobol, sizeof(temporary_cobol)) != FT_SUCCESS
            || cblc_add_named_data_item(unit, temporary_source, temporary_cobol,
                CBLC_DATA_KIND_STRUCT, 0, 0, left_item->struct_type_name,
                function->source_name, 1, 0, 0) != FT_SUCCESS)
            goto fail;
        result_item = cblc_find_data_item(unit, temporary_source);
        if (!result_item)
            goto fail;
        receiver_type = cblc_find_receiver_type(unit, left_item);
        if (!receiver_type)
            goto fail;
        ft_strlcpy(call_arguments, right_item->source_name,
            sizeof(call_arguments));
        argument_count = 1;
        method = cblc_find_method_on_type_for_arguments(unit, receiver_type,
            "operator+", call_arguments, argument_count);
        if (!method || method->return_kind != CBLC_FUNCTION_RETURN_STRUCT
            || std::strcmp(method->return_type_name,
                result_item->struct_type_name) != 0
            || method->parameter_count != 1
            || !cblc_argument_matches_parameter(unit, right_item->source_name,
                &method->parameters[0])
            || cblc_normalize_call_arguments(unit, call_arguments,
                argument_count, normalized_arguments,
                sizeof(normalized_arguments)) != FT_SUCCESS
            || cblc_append_statement(function,
                CBLC_STATEMENT_METHOD_CALL_ASSIGN, result_item->cobol_name,
                left_item->cobol_name, 0) != FT_SUCCESS)
            goto fail;
        statement = &function->statements[function->statement_count - 1];
        statement->source_offset = cblc_source_offset(unit, operator_start);
        statement->source_length = 1;
        cblc_exception_source_position(unit, operator_start,
            &statement->exception_source_line,
            &statement->exception_source_column);
        cblc_exception_source_position(unit, operator_start,
            &statement->source_line, &statement->source_column);
        ft_strlcpy(statement->call_identifier, "operator+",
            sizeof(statement->call_identifier));
        ft_strlcpy(statement->call_arguments, normalized_arguments,
            sizeof(statement->call_arguments));
        statement->call_argument_count = argument_count;

        result_type = cblc_find_struct_type(unit,
            result_item->struct_type_name);
        if (!result_type)
            goto fail;
        needs_destructor = cblc_type_requires_destruct(unit, result_type);
        if (needs_destructor)
        {
            if (function->local_destructor_count
                    >= function->local_destructor_capacity
                && cblc_function_ensure_local_destructor_capacity(function,
                    function->local_destructor_capacity == 0 ? 4
                        : function->local_destructor_capacity * 2) != FT_SUCCESS)
                goto fail;
            ft_strlcpy(function->local_destructor_targets[
                    function->local_destructor_count], result_item->cobol_name,
                TRANSPILE_IDENTIFIER_MAX);
            function->local_destructor_count += 1;
        }
        left_item = result_item;
        chain_index += 1;
        if (**cursor == ';')
        {
            *cursor += 1;
            break ;
        }
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
            target_item->cobol_name, left_item->cobol_name, 0) != FT_SUCCESS)
        goto fail;
    while (function->local_destructor_count > cleanup_base)
    {
        char cleanup_target[TRANSPILE_IDENTIFIER_MAX];
        size_t cleanup_index;

        cleanup_index = function->local_destructor_count - 1;
        ft_strlcpy(cleanup_target,
            function->local_destructor_targets[cleanup_index],
            sizeof(cleanup_target));
        function->local_destructor_count = cleanup_index;
        if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                cleanup_target, NULL, 0) != FT_SUCCESS)
            goto fail;
    }
    return (FT_SUCCESS);
fail:
    *cursor = start;
    unit->data_count = original_data_count;
    function->statement_count = original_statement_count;
    function->local_destructor_count = cleanup_base;
    return (FT_FAILURE);
}

static int cblc_parse_method_call_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    const char *method_source_start;
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char normalized_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    char original_target_source_name[TRANSPILE_IDENTIFIER_MAX];
    char original_target_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    size_t argument_count;
    size_t chain_index;
    size_t argument_cleanup_base;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    ft_strlcpy(original_target_source_name, target_item->source_name,
        sizeof(original_target_source_name));
    ft_strlcpy(original_target_cobol_name, target_item->cobol_name,
        sizeof(original_target_cobol_name));
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    method_source_start = *cursor - std::strlen(method_name);
    chain_index = 0;
    while (1)
    {
        t_cblc_data_item receiver_snapshot;
        char receiver_cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char target_cobol_name[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_statement *statement;
        int has_next_member;

        receiver_type = cblc_find_receiver_type(unit, receiver_item);
        cblc_skip_whitespace(cursor);
        if (**cursor != '(')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        call_arguments[0] = '\0';
        normalized_call_arguments[0] = '\0';
        argument_count = 0;
        argument_cleanup_base = function->local_destructor_count;
        if (cblc_parse_method_call_argument_list(cursor, unit, function,
                call_arguments,
                sizeof(call_arguments), &argument_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        method = cblc_find_method_on_type_for_arguments(unit, receiver_type,
            method_name, call_arguments, argument_count);
        if (!method)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        {
            size_t argument_index;
            t_cblc_statement probe_statement;

            std::memset(&probe_statement, 0, sizeof(probe_statement));
            ft_strlcpy(probe_statement.call_arguments, call_arguments,
                sizeof(probe_statement.call_arguments));
            probe_statement.call_argument_count = argument_count;
            argument_index = 0;
            while (argument_index < argument_count)
            {
                char argument[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_extract_call_argument(&probe_statement, argument_index,
                        argument, sizeof(argument)) != FT_SUCCESS
                    || !cblc_argument_matches_parameter(unit, argument,
                        &method->parameters[argument_index]))
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
                argument_index += 1;
            }
        }
        if (cblc_normalize_call_arguments(unit, call_arguments, argument_count,
                normalized_call_arguments, sizeof(normalized_call_arguments))
            != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ')')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        has_next_member = ((*cursor)[0] == '-' && (*cursor)[1] == '>');
        if (has_next_member)
        {
            char base_source_name[TRANSPILE_IDENTIFIER_MAX];
            char generated_source_name[TRANSPILE_IDENTIFIER_MAX];
            char generated_cobol_name[TRANSPILE_IDENTIFIER_MAX];
            const char *temporary_base_name;

            if (method->return_kind != CBLC_FUNCTION_RETURN_STRUCT_POINTER
                || method->return_type_name[0] == '\0')
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            ft_strlcpy(receiver_snapshot.source_name, receiver_item->source_name,
                sizeof(receiver_snapshot.source_name));
            ft_strlcpy(receiver_snapshot.cobol_name, receiver_item->cobol_name,
                sizeof(receiver_snapshot.cobol_name));
            ft_strlcpy(receiver_cobol_name, receiver_snapshot.cobol_name,
                sizeof(receiver_cobol_name));
            temporary_base_name = "CBLC_CHAIN_VALUE";
            if (std::snprintf(base_source_name, sizeof(base_source_name),
                    "%s_%zu", temporary_base_name,
                    function->statement_count + chain_index) < 0
                || cblc_build_unique_local_storage_names(unit, function,
                    base_source_name, generated_source_name,
                    sizeof(generated_source_name), generated_cobol_name,
                    sizeof(generated_cobol_name)) != FT_SUCCESS
                || cblc_add_named_data_item(unit, generated_source_name,
                    generated_cobol_name, CBLC_DATA_KIND_STRUCT_POINTER, 0, 0,
                    method->return_type_name, function->source_name, 1, 0, 0)
                    != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            ft_strlcpy(target_cobol_name, generated_cobol_name,
                sizeof(target_cobol_name));
            if (cblc_append_statement(function,
                    CBLC_STATEMENT_METHOD_CALL_ASSIGN, target_cobol_name,
                    receiver_cobol_name, 0) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            statement = &function->statements[function->statement_count - 1];
            statement->source_offset = cblc_source_offset(unit,
                method_source_start);
            statement->source_length = std::strlen(method_name);
            cblc_exception_source_position(unit, method_source_start,
                &statement->exception_source_line,
                &statement->exception_source_column);
            cblc_exception_source_position(unit, method_source_start,
                &statement->source_line, &statement->source_column);
            ft_strlcpy(statement->call_identifier, method_name,
                sizeof(statement->call_identifier));
            ft_strlcpy(statement->call_arguments, normalized_call_arguments,
                sizeof(statement->call_arguments));
            statement->call_argument_count = argument_count;
            if (cblc_destroy_method_argument_temporaries(function,
                    argument_cleanup_base) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            receiver_item = cblc_find_data_item(unit, generated_source_name);
            if (!receiver_item)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            *cursor += 2;
            method_source_start = *cursor;
            if (cblc_parse_identifier(cursor, method_name,
                    sizeof(method_name)) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            chain_index += 1;
            continue ;
        }

        target_item = cblc_find_data_item(unit, original_target_source_name);
        if (!target_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR)
            has_next_member = target_item->kind != CBLC_DATA_KIND_CHAR;
        else if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
            has_next_member = target_item->kind != CBLC_DATA_KIND_INT;
        else if (method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
            has_next_member = target_item->kind != CBLC_DATA_KIND_DOUBLE;
        else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
            has_next_member = target_item->kind != CBLC_DATA_KIND_STRUCT
                || std::strncmp(target_item->struct_type_name,
                    method->return_type_name,
                    sizeof(target_item->struct_type_name)) != 0;
        else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
            has_next_member = !cblc_pointer_assignment_is_compatible(
                target_item->kind, CBLC_DATA_KIND_VOID_POINTER);
        else if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
            has_next_member = !cblc_pointer_assignment_is_compatible(
                target_item->kind, CBLC_DATA_KIND_CHAR_POINTER);
        else if (method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
            has_next_member = !cblc_pointer_assignment_is_compatible(
                target_item->kind, CBLC_DATA_KIND_INT_POINTER);
        else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            has_next_member = target_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                || std::strncmp(target_item->struct_type_name,
                    method->return_type_name,
                    sizeof(target_item->struct_type_name)) != 0;
        else
            has_next_member = 1;
        if (has_next_member || **cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(receiver_snapshot.cobol_name, receiver_item->cobol_name,
            sizeof(receiver_snapshot.cobol_name));
        ft_strlcpy(target_cobol_name, original_target_cobol_name,
            sizeof(target_cobol_name));
        *cursor += 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL_ASSIGN,
                target_cobol_name, receiver_snapshot.cobol_name, 0) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        statement = &function->statements[function->statement_count - 1];
        statement->source_offset = cblc_source_offset(unit,
            method_source_start);
        statement->source_length = std::strlen(method_name);
        cblc_exception_source_position(unit, method_source_start,
            &statement->exception_source_line,
            &statement->exception_source_column);
        cblc_exception_source_position(unit, method_source_start,
            &statement->source_line, &statement->source_column);
        ft_strlcpy(statement->call_identifier, method_name,
            sizeof(statement->call_identifier));
        ft_strlcpy(statement->call_arguments, normalized_call_arguments,
            sizeof(statement->call_arguments));
        statement->call_argument_count = argument_count;
        if (cblc_destroy_method_argument_temporaries(function,
                argument_cleanup_base) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
}

/* Lower a method-valued argument into a typed local before its parent call.
 * This preserves source evaluation order and gives owned class results normal
 * lifecycle ownership instead of treating call syntax as text substitution. */
static int cblc_parse_method_call_argument_list(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    char *buffer, size_t buffer_size, size_t *out_count)
{
    const char *start;
    char simple_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t simple_count;

    if (!cursor || !*cursor || !unit || !function || !buffer
        || buffer_size == 0 || !out_count)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_call_argument_list(cursor, unit, simple_arguments,
            sizeof(simple_arguments), &simple_count) == FT_SUCCESS)
    {
        if (std::strlen(simple_arguments) >= buffer_size)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(buffer, simple_arguments, buffer_size);
        *out_count = simple_count;
        return (FT_SUCCESS);
    }
    *cursor = start;
    buffer[0] = '\0';
    *out_count = 0;
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
        return (FT_SUCCESS);
    while (1)
    {
        const char *argument_start;
        const char *argument_end;
        const char *scan;
        int nesting;
        int in_string;
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *dot;
        const char *open_paren;

        argument_start = *cursor;
        scan = argument_start;
        nesting = 0;
        in_string = 0;
        while (*scan != '\0')
        {
            if (in_string)
            {
                if (*scan == '\\' && scan[1] != '\0')
                    scan += 2;
                else
                {
                    if (*scan == '"')
                        in_string = 0;
                    scan += 1;
                }
                continue ;
            }
            if (*scan == '"')
                in_string = 1;
            else if (*scan == '(')
                nesting += 1;
            else if (*scan == ')')
            {
                if (nesting == 0)
                    break ;
                nesting -= 1;
            }
            else if (*scan == ',' && nesting == 0)
                break ;
            scan += 1;
        }
        if (in_string || nesting != 0 || scan == argument_start)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        argument_end = scan;
        while (argument_start < argument_end
            && std::isspace(static_cast<unsigned char>(*argument_start)))
            argument_start += 1;
        while (argument_end > argument_start
            && std::isspace(static_cast<unsigned char>(argument_end[-1])))
            argument_end -= 1;
        if (argument_end <= argument_start
            || static_cast<size_t>(argument_end - argument_start) >= sizeof(argument))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        std::memcpy(argument, argument_start,
            static_cast<size_t>(argument_end - argument_start));
        argument[argument_end - argument_start] = '\0';
        dot = std::strchr(argument, '.');
        open_paren = std::strchr(argument, '(');
        if (dot && open_paren && dot < open_paren)
        {
            char nested_source[TRANSPILE_STATEMENT_TEXT_MAX];
            char method_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
            char generated_source_name[TRANSPILE_IDENTIFIER_MAX];
            char generated_cobol_name[TRANSPILE_IDENTIFIER_MAX];
            char temporary_base[TRANSPILE_IDENTIFIER_MAX];
            char method_name[TRANSPILE_IDENTIFIER_MAX];
            t_cblc_data_item *receiver_item;
            t_cblc_data_item *result_item;
            const t_cblc_struct_type *receiver_type;
            const t_cblc_struct_type *result_type;
            const t_cblc_method *method;
            t_cblc_data_kind temporary_kind;
            const char *temporary_type_name;
            size_t method_argument_count;
            size_t nested_statement_start;
            const char *nested_cursor;
            size_t argument_length;

            argument_length = std::strlen(argument);
            if (argument_length + 2 >= sizeof(nested_source))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            std::memcpy(nested_source, argument, argument_length);
            nested_source[argument_length] = ';';
            nested_source[argument_length + 1] = '\0';
            nested_cursor = nested_source;
            if (cblc_parse_method_receiver(&nested_cursor, unit, &receiver_item,
                    method_name, sizeof(method_name)) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            receiver_type = cblc_find_receiver_type(unit, receiver_item);
            cblc_skip_whitespace(&nested_cursor);
            if (!receiver_type || *nested_cursor != '(')
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            nested_cursor += 1;
            if (cblc_parse_call_argument_list(&nested_cursor, unit,
                    method_arguments, sizeof(method_arguments),
                    &method_argument_count) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            method = cblc_find_method_on_type_for_arguments(unit, receiver_type,
                method_name, method_arguments, method_argument_count);
            if (!method || method->return_kind == CBLC_FUNCTION_RETURN_VOID)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            temporary_kind = CBLC_DATA_KIND_INT;
            temporary_type_name = NULL;
            if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR)
                temporary_kind = CBLC_DATA_KIND_CHAR;
            else if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
                temporary_kind = CBLC_DATA_KIND_INT;
            else if (method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
                temporary_kind = CBLC_DATA_KIND_DOUBLE;
            else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
            {
                temporary_kind = CBLC_DATA_KIND_STRUCT;
                temporary_type_name = method->return_type_name;
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
                temporary_kind = CBLC_DATA_KIND_VOID_POINTER;
            else if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
                temporary_kind = CBLC_DATA_KIND_CHAR_POINTER;
            else if (method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
                temporary_kind = CBLC_DATA_KIND_INT_POINTER;
            else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            {
                temporary_kind = CBLC_DATA_KIND_STRUCT_POINTER;
                temporary_type_name = method->return_type_name;
            }
            else
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            if ((temporary_kind == CBLC_DATA_KIND_STRUCT
                    || temporary_kind == CBLC_DATA_KIND_STRUCT_POINTER)
                && (!temporary_type_name || temporary_type_name[0] == '\0'))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            if (std::snprintf(temporary_base, sizeof(temporary_base),
                    "CBLC_CHAIN_ARG_%zu", function->statement_count) < 0
                || cblc_build_unique_local_storage_names(unit, function,
                    temporary_base, generated_source_name,
                    sizeof(generated_source_name), generated_cobol_name,
                    sizeof(generated_cobol_name)) != FT_SUCCESS
                || cblc_add_named_data_item(unit, generated_source_name,
                    generated_cobol_name, temporary_kind, 0, 0,
                    temporary_type_name, function->source_name, 1, 0, 0)
                    != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            result_item = cblc_find_data_item(unit, generated_source_name);
            if (!result_item)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            nested_statement_start = function->statement_count;
            nested_cursor = nested_source;
            if (cblc_parse_method_call_assignment(&nested_cursor, unit,
                    function, result_item) != FT_SUCCESS
                || *nested_cursor != '\0')
            {
                function->statement_count = nested_statement_start;
                *cursor = start;
                return (FT_FAILURE);
            }
            for (size_t statement_index = nested_statement_start;
                statement_index < function->statement_count; ++statement_index)
            {
                t_cblc_statement *nested_statement;

                nested_statement = &function->statements[statement_index];
                nested_statement->source_offset = cblc_source_offset(unit,
                    argument_start);
                cblc_exception_source_position(unit, argument_start,
                    &nested_statement->source_line,
                    &nested_statement->source_column);
                cblc_exception_source_position(unit, argument_start,
                    &nested_statement->exception_source_line,
                    &nested_statement->exception_source_column);
            }
            {
                result_type = temporary_kind == CBLC_DATA_KIND_STRUCT
                    ? cblc_find_struct_type(unit, temporary_type_name) : NULL;
                if (temporary_kind == CBLC_DATA_KIND_STRUCT && !result_type)
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
                if (result_type && cblc_type_requires_destruct(unit, result_type))
                {
                    if (function->local_destructor_count
                            >= function->local_destructor_capacity
                        && cblc_function_ensure_local_destructor_capacity(function,
                            function->local_destructor_capacity == 0 ? 4
                                : function->local_destructor_capacity * 2)
                            != FT_SUCCESS)
                    {
                        *cursor = start;
                        return (FT_FAILURE);
                    }
                    ft_strlcpy(function->local_destructor_targets[
                            function->local_destructor_count],
                        result_item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
                    function->local_destructor_count += 1;
                }
            }
            if (*out_count > 0)
                ft_strlcat(buffer, ",", buffer_size);
            if (std::strlen(buffer) + std::strlen(generated_source_name)
                    >= buffer_size)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            ft_strlcat(buffer, generated_source_name, buffer_size);
        }
        else
        {
            if (*out_count > 0)
                ft_strlcat(buffer, ",", buffer_size);
            if (std::strlen(buffer) + std::strlen(argument) >= buffer_size)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            ft_strlcat(buffer, argument, buffer_size);
        }
        *out_count += 1;
        *cursor = scan;
        cblc_skip_whitespace(cursor);
        if (**cursor == ',')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            continue ;
        }
        if (**cursor == ')')
            return (FT_SUCCESS);
        *cursor = start;
        return (FT_FAILURE);
    }
}

static void cblc_function_add_exception_type(t_cblc_function *function,
    size_t type_id);
static size_t cblc_exception_type_id(const char *type_name);

static int cblc_parse_pointer_allocation_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    if (!cblc_data_kind_is_pointer(target_item->kind))
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strcmp(identifier, "new") == 0)
    {
        const t_cblc_struct_type *allocated_type;
        char type_identifier[TRANSPILE_IDENTIFIER_MAX];

        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor == '[')
        {
            const char *count_start;
            const char *count_cursor;
            size_t bracket_depth;
            size_t count_length;

            if (!cblc_data_kind_is_pointer(target_item->kind))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            *cursor += 1;
            count_start = *cursor;
            count_cursor = count_start;
            bracket_depth = 0;
            while (*count_cursor != '\0')
            {
                if (*count_cursor == '[')
                    bracket_depth += 1;
                else if (*count_cursor == ']')
                {
                    if (bracket_depth == 0)
                        break ;
                    bracket_depth -= 1;
                }
                count_cursor += 1;
            }
            if (*count_cursor != ']' || bracket_depth != 0)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            count_length = static_cast<size_t>(count_cursor - count_start);
            if (count_length == 0 || count_length >= sizeof(call_arguments))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            std::memcpy(call_arguments, count_start, count_length);
            call_arguments[count_length] = '\0';
            {
                size_t template_parameter_index;
                char byte_expression[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t element_size;

                if (cblc_active_template_parameter_index(type_identifier,
                        &template_parameter_index) == FT_SUCCESS)
                {
                    if (std::snprintf(byte_expression, sizeof(byte_expression),
                            "%s * CBLC-TPL-SIZE-%s", call_arguments,
                            type_identifier) < 0
                        || std::strlen(byte_expression) >= sizeof(call_arguments))
                    {
                        *cursor = start;
                        return (FT_FAILURE);
                    }
                    ft_strlcpy(call_arguments, byte_expression, sizeof(call_arguments));
                }
                else
                {
                    t_cblc_data_kind element_kind;

                    element_kind = CBLC_DATA_KIND_STRUCT;
                    if (std::strcmp(type_identifier, "char") == 0)
                        element_kind = CBLC_DATA_KIND_CHAR;
                    else if (std::strcmp(type_identifier, "int") == 0)
                        element_kind = CBLC_DATA_KIND_INT;
                    else if (std::strcmp(type_identifier, "double") == 0)
                        element_kind = CBLC_DATA_KIND_DOUBLE;
                    if (cblc_sizeof_data_kind(unit, element_kind,
                            0,
                            0, element_kind == CBLC_DATA_KIND_STRUCT
                                ? type_identifier : NULL, &element_size) != FT_SUCCESS)
                    {
                        *cursor = start;
                        return (FT_FAILURE);
                    }
                    if (element_size == 1)
                        ft_strlcpy(byte_expression, call_arguments, sizeof(byte_expression));
                    else if (std::snprintf(byte_expression, sizeof(byte_expression),
                            "%s * %zu", call_arguments, element_size) < 0
                        || std::strlen(byte_expression) >= sizeof(call_arguments))
                    {
                        *cursor = start;
                        return (FT_FAILURE);
                    }
                    ft_strlcpy(call_arguments, byte_expression, sizeof(call_arguments));
                }
            }
            *cursor = count_cursor + 1;
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            *cursor += 1;
            if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
                    target_item->cobol_name, type_identifier, 0) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            statement = &function->statements[function->statement_count - 1];
            ft_strlcpy(statement->call_identifier, "CBLC-NEW-ARRAY",
                sizeof(statement->call_identifier));
            ft_strlcpy(statement->call_arguments, call_arguments,
                sizeof(statement->call_arguments));
            statement->call_argument_count = 1;
            statement->exception_may_throw = 1;
            cblc_function_add_exception_type(function,
                cblc_exception_type_id("bad_alloc"));
            return (FT_SUCCESS);
        }
        if (target_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        allocated_type = cblc_find_struct_type(unit, type_identifier);
        if (!allocated_type
            || std::strncmp(allocated_type->source_name, target_item->struct_type_name,
                sizeof(target_item->struct_type_name)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != '(')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
                sizeof(call_arguments), &argument_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (**cursor != ')')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
                target_item->cobol_name, type_identifier, 0) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        statement = &function->statements[function->statement_count - 1];
        ft_strlcpy(statement->call_identifier, "CBLC-NEW", sizeof(statement->call_identifier));
        ft_strlcpy(statement->call_arguments, call_arguments,
            sizeof(statement->call_arguments));
        statement->call_argument_count = argument_count;
        statement->exception_may_throw = 1;
        cblc_function_add_exception_type(function,
            cblc_exception_type_id("bad_alloc"));
        return (FT_SUCCESS);
    }
    if (std::strncmp(identifier, "std::malloc", sizeof(identifier)) != 0
        && std::strncmp(identifier, "std::realloc", sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    {
        size_t template_parameter_index;

        if (target_item->kind == CBLC_DATA_KIND_STRUCT_POINTER
            && cblc_active_template_parameter_index(target_item->struct_type_name,
                &template_parameter_index) == FT_SUCCESS)
    {
        const char *argument_start;
        const char *argument_cursor;
        size_t parenthesis_depth;
        size_t argument_length;

        cblc_skip_whitespace(cursor);
        if (**cursor != '(')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        argument_start = *cursor;
        argument_cursor = argument_start;
        parenthesis_depth = 0;
        while (*argument_cursor != '\0')
        {
            if (*argument_cursor == '(')
                parenthesis_depth += 1;
            else if (*argument_cursor == ')')
            {
                if (parenthesis_depth == 0)
                    break ;
                parenthesis_depth -= 1;
            }
            argument_cursor += 1;
        }
        if (*argument_cursor != ')' || parenthesis_depth != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        argument_length = static_cast<size_t>(argument_cursor - argument_start);
        if (argument_length + 1 > sizeof(call_arguments))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        std::memcpy(call_arguments, argument_start, argument_length);
        call_arguments[argument_length] = '\0';
        {
            char sizeof_text[TRANSPILE_IDENTIFIER_MAX];
            char sizeof_marker[TRANSPILE_IDENTIFIER_MAX];

            if (std::snprintf(sizeof_text, sizeof(sizeof_text), "sizeof(%s)",
                    target_item->struct_type_name) < 0
                || std::snprintf(sizeof_marker, sizeof(sizeof_marker),
                    "CBLC-TPL-SIZE-%s", target_item->struct_type_name) < 0)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            cblc_replace_template_text(call_arguments, sizeof(call_arguments),
                sizeof_text, sizeof_marker);
        }
        *cursor = argument_cursor + 1;
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
                target_item->cobol_name, NULL, 0) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        statement = &function->statements[function->statement_count - 1];
        ft_strlcpy(statement->call_identifier, identifier,
            sizeof(statement->call_identifier));
        ft_strlcpy(statement->call_arguments, call_arguments,
            sizeof(statement->call_arguments));
        statement->call_argument_count = std::strcmp(identifier, "std::malloc") == 0 ? 1 : 2;
        return (FT_SUCCESS);
    }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, "std::malloc", sizeof(identifier)) == 0)
    {
        if (argument_count != 1)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (argument_count != 2)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
            target_item->cobol_name, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (function->statement_count == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier,
        sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_std_free(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *item;
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::free", sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (argument_count != 1)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    std::memset(argument, 0, sizeof(argument));
    item = NULL;
    {
        t_cblc_statement probe_statement;

        std::memset(&probe_statement, 0, sizeof(probe_statement));
        ft_strlcpy(probe_statement.call_arguments, call_arguments,
            sizeof(probe_statement.call_arguments));
        probe_statement.call_argument_count = argument_count;
        if (cblc_extract_call_argument(&probe_statement, 0, argument,
                sizeof(argument)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    item = cblc_find_data_item(unit, argument);
    if (!item || !cblc_data_kind_is_pointer(item->kind))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL, NULL, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (function->statement_count == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier,
        sizeof(statement->call_identifier));
    if (item->is_alias && std::strncmp(item->cobol_name, "CBLC-THIS-",
            std::strlen("CBLC-THIS-")) == 0)
        ft_strlcpy(statement->call_arguments, item->cobol_name,
            sizeof(statement->call_arguments));
    else
        ft_strlcpy(statement->call_arguments, call_arguments,
            sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_delete_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    t_cblc_data_item *item;
    char reference[TRANSPILE_STATEMENT_TEXT_MAX];
    int is_array;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "delete"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    is_array = 0;
    if (**cursor == '[')
    {
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor != ']')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        is_array = 1;
        cblc_skip_whitespace(cursor);
    }
    if (cblc_parse_data_reference_text(cursor, unit, &item, NULL, reference,
            sizeof(reference)) != FT_SUCCESS
        || !item
        || (is_array ? !cblc_data_kind_is_pointer(item->kind)
            : item->kind != CBLC_DATA_KIND_STRUCT_POINTER))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL, NULL, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    {
        t_cblc_statement *statement;

        statement = &function->statements[function->statement_count - 1];
        ft_strlcpy(statement->call_identifier,
            is_array ? "CBLC-DELETE-ARRAY" : "CBLC-DELETE",
            sizeof(statement->call_identifier));
        ft_strlcpy(statement->call_arguments, reference, sizeof(statement->call_arguments));
        statement->call_argument_count = 1;
    }
    return (FT_SUCCESS);
}
