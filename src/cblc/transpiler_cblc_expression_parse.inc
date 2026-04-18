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
    else if (kind == CBLC_DATA_KIND_STRING)
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
        if (!type || cblc_sizeof_struct_type(unit, type, &base_size) != FT_SUCCESS)
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
                    && !(item->kind == CBLC_DATA_KIND_INT_POINTER
                        && std::strchr(reference, '[')))
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
                    && !(item->kind == CBLC_DATA_KIND_INT_POINTER
                        && std::strchr(reference, '[')))
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

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
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
    called_function = cblc_find_function_in_unit(unit, identifier);
    if (called_function && !cblc_function_return_matches_target(called_function, target_item))
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
    *cursor += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_method_call_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char normalized_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    call_arguments[0] = '\0';
    normalized_call_arguments[0] = '\0';
    argument_count = 0;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    method = cblc_find_method_on_type(receiver_type, method_name);
    if (!method)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
    {
        if (target_item->kind != CBLC_DATA_KIND_INT)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (target_item->kind != CBLC_DATA_KIND_STRUCT
            || std::strncmp(target_item->struct_type_name, method->return_type_name,
                sizeof(target_item->struct_type_name)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
    {
        if (!cblc_pointer_assignment_is_compatible(target_item->kind,
                CBLC_DATA_KIND_VOID_POINTER))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
    {
        if (!cblc_pointer_assignment_is_compatible(target_item->kind,
                CBLC_DATA_KIND_CHAR_POINTER))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
    {
        if (!cblc_pointer_assignment_is_compatible(target_item->kind,
                CBLC_DATA_KIND_INT_POINTER))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        if (target_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
            || std::strncmp(target_item->struct_type_name, method->return_type_name,
                sizeof(target_item->struct_type_name)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else
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
    if (method->parameter_count != argument_count)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    {
        size_t index;
        t_cblc_statement probe_statement;

        std::memset(&probe_statement, 0, sizeof(probe_statement));
        ft_strlcpy(probe_statement.call_arguments, call_arguments,
            sizeof(probe_statement.call_arguments));
        probe_statement.call_argument_count = argument_count;
        index = 0;
        while (index < argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(&probe_statement, index, argument,
                    sizeof(argument)) != FT_SUCCESS
                || !cblc_argument_matches_parameter(unit, argument, &method->parameters[index]))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            index += 1;
        }
    }
    if (cblc_normalize_call_arguments(unit, call_arguments, argument_count,
            normalized_call_arguments, sizeof(normalized_call_arguments)) != FT_SUCCESS)
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
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL_ASSIGN,
            target_item->cobol_name, receiver_item->cobol_name, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, normalized_call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

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
    if (std::strncmp(identifier, "std::malloc", sizeof(identifier)) != 0
        && std::strncmp(identifier, "std::realloc", sizeof(identifier)) != 0)
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
    ft_strlcpy(statement->call_arguments, call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}
