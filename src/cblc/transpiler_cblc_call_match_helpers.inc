static int cblc_extract_call_argument(const t_cblc_statement *statement,
    size_t argument_index, char *buffer, size_t buffer_size)
{
    const char *cursor;
    size_t current_index;
    size_t length;

    if (!statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    cursor = statement->call_arguments;
    current_index = 0;
    while (*cursor != '\0' && current_index < argument_index)
    {
        while (*cursor != '\0' && *cursor != ',')
            cursor += 1;
        if (*cursor == ',')
        {
            cursor += 1;
            current_index += 1;
        }
    }
    if (current_index != argument_index)
        return (FT_FAILURE);
    while (*cursor == ' ' || *cursor == '\t')
        cursor += 1;
    length = 0;
    while (cursor[length] != '\0' && cursor[length] != ',')
        length += 1;
    while (length > 0
        && (cursor[length - 1] == ' ' || cursor[length - 1] == '\t'))
        length -= 1;
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, cursor, length);
    buffer[length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_cast_argument_kind(const t_cblc_translation_unit *unit,
    const char *argument, t_cblc_data_kind *out_kind, char *out_struct_type_name,
    size_t out_struct_type_name_size)
{
    const char *cursor;
    t_cblc_data_kind cast_kind;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_data_item *item;

    if (!unit || !argument || !out_kind || !out_struct_type_name
        || out_struct_type_name_size == 0)
        return (FT_FAILURE);
    out_struct_type_name[0] = '\0';
    cursor = argument;
    cblc_skip_whitespace(&cursor);
    if (*cursor != '(')
        return (FT_FAILURE);
    cursor += 1;
    cblc_skip_whitespace(&cursor);
    if (cblc_parse_pointer_cast_type(&cursor, unit, &cast_kind, cast_struct_type_name,
            sizeof(cast_struct_type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(&cursor);
    if (*cursor != '*')
        return (FT_FAILURE);
    cursor += 1;
    cblc_skip_whitespace(&cursor);
    if (*cursor != ')')
        return (FT_FAILURE);
    cursor += 1;
    cblc_skip_whitespace(&cursor);
    if (cblc_parse_identifier(&cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(&cursor);
    if (*cursor != '\0')
        return (FT_FAILURE);
    item = cblc_find_data_item_const(unit, identifier);
    if (!item)
        item = cblc_find_data_item_by_cobol(unit, identifier);
    if (!item || !cblc_data_kind_is_pointer(item->kind))
        return (FT_FAILURE);
    *out_kind = cast_kind;
    ft_strlcpy(out_struct_type_name, cast_struct_type_name, out_struct_type_name_size);
    return (FT_SUCCESS);
}

static int cblc_argument_matches_parameter(const t_cblc_translation_unit *unit,
    const char *argument, const t_cblc_parameter *parameter)
{
    const t_cblc_data_item *item;
    t_cblc_data_kind cast_kind;
    char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !argument || !parameter)
        return (0);
    item = cblc_find_data_item_const(unit, argument);
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
    {
        if (item && item->kind == CBLC_DATA_KIND_STRUCT)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
    {
        if (argument[0] == '"')
            return (1);
        if (!item || item->kind != CBLC_DATA_KIND_STRING)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
    {
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
            return (0);
        if (std::strncmp(item->struct_type_name, parameter->type_name,
                sizeof(item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
            return (cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_VOID_POINTER,
                    cast_kind));
        if (!item || !cblc_data_kind_is_pointer(item->kind))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
            return (cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_CHAR_POINTER,
                    cast_kind));
        if (!item || !cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_CHAR_POINTER,
                item->kind))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
            return (cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_INT_POINTER,
                    cast_kind));
        if (!item || !cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_INT_POINTER,
                item->kind))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
        {
            if (cast_kind != CBLC_DATA_KIND_STRUCT_POINTER)
                return (0);
            if (std::strncmp(cast_struct_type_name, parameter->type_name,
                    sizeof(cast_struct_type_name)) != 0)
                return (0);
            return (1);
        }
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
            return (0);
        if (std::strncmp(item->struct_type_name, parameter->type_name,
                sizeof(item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    return (0);
}

static const t_cblc_constructor *cblc_find_constructor_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *call_arguments, size_t call_argument_count)
{
    size_t constructor_index;

    if (!unit || !type)
        return (NULL);
    constructor_index = 0;
    while (constructor_index < type->constructor_count)
    {
        const t_cblc_constructor *constructor;
        t_cblc_statement statement;
        size_t parameter_index;
        int matches;

        constructor = &type->constructors[constructor_index];
        if (constructor->parameter_count != call_argument_count)
        {
            constructor_index += 1;
            continue ;
        }
        std::memset(&statement, 0, sizeof(statement));
        if (call_arguments)
            ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
        statement.call_argument_count = call_argument_count;
        parameter_index = 0;
        matches = 1;
        while (parameter_index < call_argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(&statement, parameter_index, argument,
                    sizeof(argument)) != FT_SUCCESS
                || !cblc_argument_matches_parameter(unit, argument,
                    &constructor->parameters[parameter_index]))
            {
                matches = 0;
                break ;
            }
            parameter_index += 1;
        }
        if (matches)
            return (constructor);
        constructor_index += 1;
    }
    return (NULL);
}

static int cblc_emit_parameter_argument_moves(const t_cblc_translation_unit *unit,
    const char *call_arguments,
    size_t call_argument_count, const t_cblc_parameter *parameters,
    size_t parameter_count, t_cobol_text_builder *builder)
{
    size_t index;

    if (!unit || !call_arguments || !parameters || !builder)
        return (FT_FAILURE);
    if (parameter_count != call_argument_count)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[256];
        t_cblc_statement statement;

        std::memset(&statement, 0, sizeof(statement));
        ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
        statement.call_argument_count = call_argument_count;
        if (cblc_extract_call_argument(&statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        {
            const t_cblc_data_item *item;

            item = cblc_find_data_item_const(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                    item->cobol_name, parameters[index].cobol_name) < 0)
                return (FT_FAILURE);
        }
        else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
        {
            const t_cblc_data_item *item;
            char line_length[256];
            char line_buffer[256];

            item = cblc_find_data_item_const(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (std::snprintf(line_buffer, sizeof(line_buffer), "           MOVE %s-BUF TO %s-BUF.",
                    item->cobol_name, parameters[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (std::snprintf(line_length, sizeof(line_length), "           COMPUTE %s-LEN = %s-LEN.",
                    parameters[index].cobol_name, item->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(line, line_length, sizeof(line));
        }
        else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER
            || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
            || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
            || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
        {
            const t_cblc_data_item *item;

            item = cblc_find_data_item_const(unit, argument);
            if (!item || !cblc_data_kind_is_pointer(item->kind))
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           SET %s TO %s.",
                    parameters[index].cobol_name, item->cobol_name) < 0)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_translate_expression_for_cobol(argument, expression,
                    sizeof(expression)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    parameters[index].cobol_name, expression) < 0)
                return (FT_FAILURE);
        }
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_cobol_text_builder *builder)
{
    if (!unit || !target_function || !statement || !builder)
        return (FT_FAILURE);
    return (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
            statement->call_argument_count, target_function->parameters,
            target_function->parameter_count, builder));
}
