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
    if (parameter->array_count > 0)
    {
        if (!item || item->array_count != parameter->array_count)
            return (0);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
            return (item->kind == CBLC_DATA_KIND_INT);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
            return (item->kind == CBLC_DATA_KIND_STRING
                && parameter->length > 0 && item->length == parameter->length);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        {
            const t_cblc_struct_type *source_type;
            const t_cblc_struct_type *target_type;

            source_type = item->kind == CBLC_DATA_KIND_STRUCT
                ? cblc_find_struct_type(unit, item->struct_type_name) : NULL;
            target_type = cblc_find_struct_type(unit, parameter->type_name);
            return (source_type && target_type
                && std::strncmp(item->struct_type_name, parameter->type_name,
                    sizeof(item->struct_type_name)) == 0
                && cblc_struct_layout_fingerprint(unit, source_type, 0)
                    == cblc_struct_layout_fingerprint(unit, target_type, 0));
        }
        return (0);
    }
    if (item && item->array_count > 0
        && (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT
            || parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRING
            || parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT))
        return (0);
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
        const t_cblc_struct_type *source_type;
        const t_cblc_struct_type *target_type;

        if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
            return (0);
        if (std::strncmp(item->struct_type_name, parameter->type_name,
                sizeof(item->struct_type_name)) != 0)
            return (0);
        source_type = cblc_find_struct_type(unit, item->struct_type_name);
        target_type = cblc_find_struct_type(unit, parameter->type_name);
        return (source_type && target_type
            && cblc_struct_layout_fingerprint(unit, source_type, 0)
                == cblc_struct_layout_fingerprint(unit, target_type, 0));
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

static int cblc_call_arguments_match_parameters(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count,
    const t_cblc_parameter *parameters, size_t parameter_count)
{
    t_cblc_statement statement;
    size_t index;

    if (!unit || !call_arguments || !parameters
        || call_argument_count != parameter_count)
        return (0);
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = call_argument_count;
    index = 0;
    while (index < parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_extract_call_argument(&statement, index, argument, sizeof(argument)) != FT_SUCCESS
            || !cblc_argument_matches_parameter(unit, argument, &parameters[index]))
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_emit_struct_array_dynamic_string_copy(
    const char *source_pointer, const char *source_capacity, const char *source_buffer,
    const char *source_length, const char *destination_pointer,
    const char *destination_capacity, const char *destination_buffer,
    const char *destination_length, t_cobol_text_builder *builder)
{
    char line[256];

    if (!source_pointer || !source_capacity || !source_buffer || !source_length
        || !destination_pointer || !destination_capacity || !destination_buffer
        || !destination_length || !builder)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "           IF %s NOT = NULL", source_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s < %s", destination_capacity,
            source_capacity) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   IF %s NOT = NULL",
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                       FREE %s",
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   ALLOCATE %s CHARACTERS RETURNING %s", source_capacity,
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   MOVE %s TO %s",
            source_capacity, destination_capacity) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            source_buffer, source_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            destination_buffer, destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %s TO %s", source_buffer,
            destination_buffer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %s TO %s",
            source_length, destination_length) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           ELSE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s NOT = NULL",
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   FREE %s", destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET %s TO NULL", destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE 0 TO %s", destination_capacity) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE 0 TO %s", destination_length) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_struct_array_field_copies(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *source_prefix,
    const char *destination_prefix, size_t element_count, size_t depth,
    t_cobol_text_builder *builder)
{
    size_t field_index;

    if (!unit || !type || !source_prefix || !destination_prefix || !builder
        || depth > CBLC_TEMPLATE_PARAMETER_MAX)
        return (FT_FAILURE);
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field;
        char source_field[TRANSPILE_IDENTIFIER_MAX];
        char destination_field[TRANSPILE_IDENTIFIER_MAX];
        size_t element_index;

        field = &type->fields[field_index];
        if (field->array_count > 0)
            return (FT_FAILURE);
        if (std::snprintf(source_field, sizeof(source_field), "%s-%s", source_prefix,
                field->cobol_name) < 0
            || std::snprintf(destination_field, sizeof(destination_field), "%s-%s",
                destination_prefix, field->cobol_name) < 0)
            return (FT_FAILURE);
        if (cblc_data_kind_is_pointer(field->kind))
        {
            if (!field->is_borrowed)
                return (FT_FAILURE);
            element_index = 0;
            while (element_index < element_count)
            {
                char line[256];

                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s(%zu) TO %s(%zu).", source_field,
                        element_index + 1, destination_field, element_index + 1) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                element_index += 1;
            }
            field_index += 1;
            continue ;
        }
        if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *nested_type;

            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!nested_type
                || cblc_emit_struct_array_field_copies(unit, nested_type, source_field,
                    destination_field, element_count, depth + 1, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            field_index += 1;
            continue ;
        }
        if (field->kind != CBLC_DATA_KIND_STRING
            && field->kind != CBLC_DATA_KIND_CHAR
            && field->kind != CBLC_DATA_KIND_INT)
            return (FT_FAILURE);
        element_index = 0;
        while (element_index < element_count)
        {
            size_t cobol_index;
            char line[256];

            cobol_index = element_index + 1;
            if (field->kind == CBLC_DATA_KIND_STRING)
            {
                char source_buffer[TRANSPILE_IDENTIFIER_MAX];
                char destination_buffer[TRANSPILE_IDENTIFIER_MAX];
                char source_pointer[TRANSPILE_IDENTIFIER_MAX];
                char source_capacity[TRANSPILE_IDENTIFIER_MAX];
                char destination_pointer[TRANSPILE_IDENTIFIER_MAX];
                char destination_capacity[TRANSPILE_IDENTIFIER_MAX];
                char source_length[TRANSPILE_IDENTIFIER_MAX];
                char destination_length[TRANSPILE_IDENTIFIER_MAX];

                if (std::snprintf(source_buffer, sizeof(source_buffer), "%s-BUF",
                        source_field) < 0
                    || std::snprintf(destination_buffer, sizeof(destination_buffer), "%s-BUF",
                        destination_field) < 0
                    || std::snprintf(source_pointer, sizeof(source_pointer), "%s-PTR(%zu)",
                        source_field, cobol_index) < 0
                    || std::snprintf(source_capacity, sizeof(source_capacity), "%s-CAP(%zu)",
                        source_field, cobol_index) < 0
                    || std::snprintf(destination_pointer, sizeof(destination_pointer),
                        "%s-PTR(%zu)", destination_field, cobol_index) < 0
                    || std::snprintf(destination_capacity, sizeof(destination_capacity),
                        "%s-CAP(%zu)", destination_field, cobol_index) < 0
                    || std::snprintf(source_length, sizeof(source_length), "%s-LEN(%zu)",
                        source_field, cobol_index) < 0
                    || std::snprintf(destination_length, sizeof(destination_length),
                        "%s-LEN(%zu)", destination_field, cobol_index) < 0)
                    return (FT_FAILURE);
                if (cblc_string_field_uses_dynamic_storage(field))
                {
                    if (cblc_emit_struct_array_dynamic_string_copy(source_pointer,
                            source_capacity, source_buffer, source_length,
                            destination_pointer, destination_capacity, destination_buffer,
                            destination_length, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(source_buffer, sizeof(source_buffer), "%s-BUF(%zu)",
                            source_field, cobol_index) < 0
                        || std::snprintf(destination_buffer, sizeof(destination_buffer),
                            "%s-BUF(%zu)", destination_field, cobol_index) < 0)
                        return (FT_FAILURE);
                }
                if (!cblc_string_field_uses_dynamic_storage(field)
                    && (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                            source_buffer, destination_buffer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                            destination_length, source_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS))
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           MOVE %s(%zu) TO %s(%zu).",
                    source_field, cobol_index, destination_field, cobol_index) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            element_index += 1;
        }
        field_index += 1;
    }
    return (FT_SUCCESS);
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
        if (parameters[index].array_count > 0)
        {
            const t_cblc_data_item *item;
            size_t element_index;

            item = cblc_find_data_item_const(unit, argument);
            if (!item || item->array_count != parameters[index].array_count)
                return (FT_FAILURE);
            if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
            {
                if (item->kind != CBLC_DATA_KIND_STRING
                    || item->length != parameters[index].length)
                    return (FT_FAILURE);
                element_index = 0;
                while (element_index < parameters[index].array_count)
                {
                    size_t cobol_index;
                    char source_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char source_pointer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_pointer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_capacity[TRANSPILE_STATEMENT_TEXT_MAX];
                    char source_length[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_length[TRANSPILE_STATEMENT_TEXT_MAX];

                    cobol_index = element_index + 1;
                    if (std::snprintf(source_buffer, sizeof(source_buffer), "%s-BUF",
                            item->cobol_name) < 0
                        || std::snprintf(destination_buffer, sizeof(destination_buffer),
                            "%s-BUF", parameters[index].cobol_name) < 0
                        || std::snprintf(source_pointer, sizeof(source_pointer),
                            "%s-PTR(%zu)", item->cobol_name, cobol_index) < 0
                        || std::snprintf(destination_pointer, sizeof(destination_pointer),
                            "%s-PTR(%zu)", parameters[index].cobol_name, cobol_index) < 0
                        || std::snprintf(destination_capacity, sizeof(destination_capacity),
                            "%s-CAP(%zu)", parameters[index].cobol_name, cobol_index) < 0
                        || std::snprintf(source_length, sizeof(source_length),
                            "%s-LEN(%zu)", item->cobol_name, cobol_index) < 0
                        || std::snprintf(destination_length, sizeof(destination_length),
                            "%s-LEN(%zu)", parameters[index].cobol_name, cobol_index) < 0)
                        return (FT_FAILURE);
                    if (cblc_string_item_array_uses_dynamic_storage(item))
                    {
                        if (std::snprintf(line, sizeof(line),
                                "           SET ADDRESS OF %s TO %s.",
                                source_buffer, source_pointer) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else if (std::snprintf(source_buffer, sizeof(source_buffer),
                            "%s-BUF(%zu)", item->cobol_name, cobol_index) < 0)
                        return (FT_FAILURE);
                    if (cblc_emit_dynamic_string_ensure_capacity_for_references(
                            destination_capacity, destination_pointer, destination_buffer,
                            parameters[index].length, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           SET ADDRESS OF %s TO %s.",
                            destination_buffer, destination_pointer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                            source_buffer, destination_buffer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                            destination_length, source_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    element_index += 1;
                }
                index += 1;
                continue ;
            }
            if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            {
                const t_cblc_struct_type *struct_type;

                if (item->kind != CBLC_DATA_KIND_STRUCT
                    || std::strncmp(item->struct_type_name, parameters[index].type_name,
                        sizeof(item->struct_type_name)) != 0)
                    return (FT_FAILURE);
                struct_type = cblc_find_struct_type(unit, parameters[index].type_name);
                if (!struct_type)
                    return (FT_FAILURE);
                if (cblc_emit_struct_array_field_copies(unit, struct_type,
                        item->cobol_name, parameters[index].cobol_name,
                        parameters[index].array_count, 0, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
                continue ;
            }
            if (parameters[index].kind != TRANSPILE_FUNCTION_PARAMETER_INT
                || item->kind != CBLC_DATA_KIND_INT)
                return (FT_FAILURE);
            element_index = 0;
            while (element_index < parameters[index].array_count)
            {
                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s(%zu) TO %s(%zu).",
                        item->cobol_name, element_index + 1,
                        parameters[index].cobol_name, element_index + 1) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                element_index += 1;
            }
            index += 1;
            continue ;
        }
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
