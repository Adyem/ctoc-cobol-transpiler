static void cblc_exception_source_position(const t_cblc_translation_unit *unit,
    const char *position, size_t *line, size_t *column);

static int cblc_parse_file_read_while(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char keyword[TRANSPILE_IDENTIFIER_MAX];
    char file_name[TRANSPILE_IDENTIFIER_MAX];
    char record_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_file_declaration *file;
    t_cblc_data_item *record;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, keyword, sizeof(keyword)) != FT_SUCCESS
        || std::strncmp(keyword, "while", sizeof(keyword)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, keyword, sizeof(keyword)) != FT_SUCCESS
        || std::strncmp(keyword, "read", sizeof(keyword)) != 0)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, file_name, sizeof(file_name)) != FT_SUCCESS)
        goto fail;
    file = cblc_find_file(unit, file_name);
    if (!file)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (**cursor != ',')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, record_name, sizeof(record_name)) != FT_SUCCESS)
        goto fail;
    record = cblc_find_data_item(unit, record_name);
    if (!record || record->kind != CBLC_DATA_KIND_CHAR || record->array_count != 0)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        goto fail;
    file->record_length = record->length;
    ft_strlcpy(file->record_source_name, record_name, sizeof(file->record_source_name));
    if (file->organization == TRANSPILE_FILE_ORGANIZATION_INDEXED
        && file->record_key_source_name[0] == '\0')
        ft_strlcpy(file->record_key_source_name, record_name,
            sizeof(file->record_key_source_name));
    if (cblc_append_statement(function, CBLC_STATEMENT_WHILE_BEGIN,
            file_name, NULL, 0) != FT_SUCCESS
        || cblc_append_statement(function, CBLC_STATEMENT_FILE_READ,
            file_name, record_name, 0) != FT_SUCCESS)
        goto fail;
    cblc_exception_source_position(unit, start,
        &function->statements[function->statement_count - 1].exception_source_line,
        &function->statements[function->statement_count - 1].exception_source_column);
    if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS
        || cblc_append_statement(function, CBLC_STATEMENT_WHILE_END,
            file_name, NULL, 0) != FT_SUCCESS)
        goto fail;
    return (FT_SUCCESS);
fail:
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_parse_file_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char operation[TRANSPILE_IDENTIFIER_MAX];
    char file_name[TRANSPILE_IDENTIFIER_MAX];
    char record_name[TRANSPILE_IDENTIFIER_MAX];
    char mode[8];
    t_cblc_file_declaration *file;
    t_cblc_data_item *record;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, operation, sizeof(operation)) != FT_SUCCESS
        || (std::strncmp(operation, "open", sizeof(operation)) != 0
            && std::strncmp(operation, "read", sizeof(operation)) != 0
            && std::strncmp(operation, "write", sizeof(operation)) != 0
            && std::strncmp(operation, "close", sizeof(operation)) != 0))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, file_name, sizeof(file_name)) != FT_SUCCESS)
        goto fail;
    file = cblc_find_file(unit, file_name);
    if (!file)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (std::strncmp(operation, "open", sizeof(operation)) == 0)
    {
        if (**cursor != ',')
            goto fail;
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_string_literal(cursor, mode, sizeof(mode)) != FT_SUCCESS
            || (std::strncmp(mode, "r", sizeof(mode)) != 0
                && std::strncmp(mode, "w", sizeof(mode)) != 0
                && std::strncmp(mode, "a", sizeof(mode)) != 0))
            goto fail;
        if (std::strncmp(mode, "r", sizeof(mode)) == 0)
            file->role = TRANSPILE_FILE_ROLE_INPUT;
        else if (std::strncmp(mode, "w", sizeof(mode)) == 0)
            file->role = TRANSPILE_FILE_ROLE_OUTPUT;
        else
            file->role = TRANSPILE_FILE_ROLE_OUTPUT;
        cblc_skip_whitespace(cursor);
        if (**cursor != ')' || ( ++(*cursor), cblc_skip_whitespace(cursor), **cursor != ';'))
            goto fail;
        *cursor += 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_FILE_OPEN,
                file_name, mode, 1) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_exception_source_position(unit, start,
            &function->statements[function->statement_count - 1].exception_source_line,
            &function->statements[function->statement_count - 1].exception_source_column);
        return (FT_SUCCESS);
    }
    if (std::strncmp(operation, "close", sizeof(operation)) == 0)
    {
        cblc_skip_whitespace(cursor);
        if (**cursor != ')' || ( ++(*cursor), cblc_skip_whitespace(cursor), **cursor != ';'))
            goto fail;
        *cursor += 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_FILE_CLOSE,
                file_name, NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_exception_source_position(unit, start,
            &function->statements[function->statement_count - 1].exception_source_line,
            &function->statements[function->statement_count - 1].exception_source_column);
        return (FT_SUCCESS);
    }
    if (**cursor != ',')
        goto fail;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, record_name, sizeof(record_name)) != FT_SUCCESS)
        goto fail;
    record = cblc_find_data_item(unit, record_name);
    if (!record || record->kind != CBLC_DATA_KIND_CHAR || record->array_count != 0)
        goto fail;
    file->record_length = record->length;
    ft_strlcpy(file->record_source_name, record_name, sizeof(file->record_source_name));
    cblc_skip_whitespace(cursor);
    if (**cursor != ')' || ( ++(*cursor), cblc_skip_whitespace(cursor), **cursor != ';'))
        goto fail;
    *cursor += 1;
    if (cblc_append_statement(function,
        std::strncmp(operation, "read", sizeof(operation)) == 0
            ? CBLC_STATEMENT_FILE_READ : CBLC_STATEMENT_FILE_WRITE,
        file_name, record_name, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_exception_source_position(unit, start,
        &function->statements[function->statement_count - 1].exception_source_line,
        &function->statements[function->statement_count - 1].exception_source_column);
    return (FT_SUCCESS);
fail:
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_parse_std_strcpy(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char destination_identifier[TRANSPILE_IDENTIFIER_MAX];
    char source_identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
    char destination_length_literal[32];
    char source_length_literal[32];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_source_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *target_item;
    t_cblc_data_item *source_item;
    t_cblc_data_item *status_item;
    size_t source_length;
    size_t target_index;
    size_t source_index;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::strcpy", sizeof(identifier)) != 0)
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
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, destination_identifier,
            sizeof(destination_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_item = cblc_find_data_item(unit, destination_identifier);
    if (!target_item || target_item->kind != CBLC_DATA_KIND_CHAR
        || target_item->is_const)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_index = static_cast<size_t>(target_item - unit->data_items);
    cblc_skip_whitespace(cursor);
    if (**cursor != ',')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    source_item = NULL;
    literal_text[0] = '\0';
    source_length = 0;
    source_index = 0;
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
            != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_length = std::strlen(literal_buffer);
        literal_text[0] = '\"';
        literal_text[1] = '\0';
        ft_strlcat(literal_text, literal_buffer, sizeof(literal_text));
        ft_strlcat(literal_text, "\"", sizeof(literal_text));
        source_item = cblc_create_literal_buffer(unit, source_length);
        if (!source_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                source_item->cobol_name, literal_text, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_index = static_cast<size_t>(source_item - unit->data_items);
        target_item = &unit->data_items[target_index];
    }
    else if (**cursor == '\'')
    {
        char literal_value;
        char literal_token[4];

        if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        literal_token[0] = literal_value;
        literal_token[1] = '\0';
        literal_text[0] = '\"';
        literal_text[1] = '\0';
        if (literal_value == '"')
            ft_strlcat(literal_text, "\"\"", sizeof(literal_text));
        else
            ft_strlcat(literal_text, literal_token, sizeof(literal_text));
        ft_strlcat(literal_text, "\"", sizeof(literal_text));
        source_length = 1;
        source_item = cblc_create_literal_buffer(unit, source_length);
        if (!source_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                source_item->cobol_name, literal_text, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_index = static_cast<size_t>(source_item - unit->data_items);
        target_item = &unit->data_items[target_index];
    }
    else
    {
        if (cblc_parse_identifier(cursor, source_identifier,
                sizeof(source_identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_item = cblc_find_data_item(unit, source_identifier);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_length = source_item->length;
        source_index = static_cast<size_t>(source_item - unit->data_items);
    }
    cblc_skip_whitespace(cursor);
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
    if (!source_item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(destination_length_literal, sizeof(destination_length_literal),
            "%zu", target_item->length) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(source_length_literal, sizeof(source_length_literal), "%zu",
            source_length) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    status_item = cblc_ensure_helper_status(unit);
    if (!status_item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_item = &unit->data_items[target_index];
    source_item = &unit->data_items[source_index];
    ft_strlcpy(cobol_source_name, source_item->cobol_name,
        sizeof(cobol_source_name));
    if (std::snprintf(call_arguments, sizeof(call_arguments),
            "BY REFERENCE %s BY VALUE %s BY REFERENCE %s BY VALUE %s BY REFERENCE %s",
            target_item->cobol_name, destination_length_literal, cobol_source_name,
            source_length_literal, status_item->cobol_name) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_append_call_statement(function, "CBLC-STRCPY", call_arguments) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

#include "src/cblc/transpiler_cblc_expression_parse.inc"
static int cblc_parse_builtin_string_append_call(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    t_cblc_data_item *argument_item;
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    char receiver_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char source_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_literal[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement *statement;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    if (!receiver_type || !receiver_type->is_builtin
        || std::strncmp(receiver_type->source_name, "string", TRANSPILE_IDENTIFIER_MAX) != 0
        || std::strncmp(method_name, "append", sizeof(method_name)) != 0)
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
    cblc_skip_whitespace(cursor);
    ft_strlcpy(receiver_cobol_name, receiver_item->cobol_name, sizeof(receiver_cobol_name));
    if (**cursor == '"')
    {
        t_cblc_data_item *source_item;
        size_t literal_length;

        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        literal_length = std::strlen(literal_buffer);
        cobol_literal[0] = '"';
        cobol_literal[1] = '\0';
        ft_strlcat(cobol_literal, literal_buffer, sizeof(cobol_literal));
        ft_strlcat(cobol_literal, "\"", sizeof(cobol_literal));
        source_item = cblc_create_literal_buffer(unit, literal_length);
        if (!source_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                source_item->cobol_name, cobol_literal, 1) != FT_SUCCESS)
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
        if (**cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        if (!cblc_ensure_helper_status(unit))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_update_string_capacity(unit, receiver_item, receiver_item->length + literal_length);
        if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
                receiver_cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        statement = &function->statements[function->statement_count - 1];
        ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
        ft_strlcpy(statement->call_arguments, source_item->cobol_name,
            sizeof(statement->call_arguments));
        statement->call_argument_count = 1;
        return (FT_SUCCESS);
    }
    if (cblc_parse_data_reference(cursor, unit, &argument_item, NULL) != FT_SUCCESS
        || !argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(source_cobol_name, argument_item->cobol_name, sizeof(source_cobol_name));
    cblc_skip_whitespace(cursor);
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
    if (!cblc_ensure_helper_status(unit))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_update_string_capacity(unit, receiver_item, receiver_item->length + argument_item->length);
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
            receiver_cobol_name, source_cobol_name, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, source_cobol_name,
        sizeof(statement->call_arguments));
    statement->call_argument_count = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_std_strlen_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, t_cblc_data_item *target_item)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char argument_identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char declared_length_literal[32];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *argument_item;
    size_t literal_length;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::strlen", sizeof(identifier)) != 0)
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
    cblc_skip_whitespace(cursor);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
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
        if (**cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        literal_length = std::strlen(literal_buffer);
        if (std::snprintf(declared_length_literal, sizeof(declared_length_literal),
                "%zu", literal_length) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                target_item->cobol_name, declared_length_literal, 1));
    }
    if (cblc_parse_identifier(cursor, argument_identifier,
            sizeof(argument_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    argument_item = cblc_find_data_item(unit, argument_identifier);
    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_CHAR)
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
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (std::snprintf(declared_length_literal, sizeof(declared_length_literal), "%zu",
            argument_item->length) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(call_arguments, sizeof(call_arguments),
            "BY REFERENCE %s BY VALUE %s BY REFERENCE %s", argument_item->cobol_name,
            declared_length_literal, target_item->cobol_name) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_append_call_statement(function, "CBLC-STRLEN", call_arguments) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char expression_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *target_item;
    t_cblc_data_kind effective_target_kind;
    int target_is_length_reference;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    target_is_length_reference = 0;
    if (**cursor == '*')
    {
        if (cblc_parse_pointer_dereference_reference(cursor, unit, &target_item,
                &effective_target_kind, target_reference, sizeof(target_reference))
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        if (cblc_parse_data_reference_text(cursor, unit, &target_item, &target_is_length_reference,
                target_reference, sizeof(target_reference))
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (target_is_length_reference)
            return (FT_FAILURE);
        effective_target_kind = target_item->kind;
    }
    if (std::strchr(target_reference, '[')
        && cblc_data_kind_is_pointer(target_item->kind))
    {
        if (target_item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
            effective_target_kind = CBLC_DATA_KIND_STRUCT;
        else if (cblc_pointer_element_kind(target_item->kind, &effective_target_kind)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '=')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (std::strchr(target_reference, '[')
        && target_item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        size_t template_parameter_index;

        if (cblc_active_template_parameter_index(target_item->struct_type_name,
                &template_parameter_index) == FT_SUCCESS)
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL,
                    source_reference, sizeof(source_reference)) != FT_SUCCESS
                || !source_item
                || (source_item->kind != CBLC_DATA_KIND_STRUCT
                    && source_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
                || std::strncmp(source_item->struct_type_name,
                    target_item->struct_type_name,
                    sizeof(source_item->struct_type_name)) != 0)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, source_reference, 0));
        }
    }
    if (target_item->is_const)
    {
        size_t field_index;

        if (cblc_find_current_constructor_field_index(target_item, &field_index) != FT_SUCCESS
            || !g_cblc_constructor_parse_state.initialized_fields)
            return (FT_FAILURE);
        if (g_cblc_constructor_parse_state.initialized_fields[field_index])
            return (FT_FAILURE);
        g_cblc_constructor_parse_state.initialized_fields[field_index] = 1;
    }
    if (effective_target_kind == CBLC_DATA_KIND_STRING)
    {
        t_cblc_data_item *source_item;
        char buffer_target[TRANSPILE_IDENTIFIER_MAX];
        char length_target[TRANSPILE_IDENTIFIER_MAX];
        char length_literal[32];
        size_t assigned_length;
        size_t target_index;

        if (std::strncmp(target_reference, target_item->cobol_name,
                sizeof(target_item->cobol_name)) == 0)
        {
            cblc_build_string_component_name(target_item->cobol_name, "-BUF", buffer_target,
                sizeof(buffer_target));
            cblc_build_string_component_name(target_item->cobol_name, "-LEN", length_target,
                sizeof(length_target));
        }
        else
        {
            if (std::strchr(target_reference, '['))
            {
                char base_name[TRANSPILE_IDENTIFIER_MAX];
                char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_cobol_reference_base(target_reference, base_name, sizeof(base_name))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::strncmp(base_name, target_item->cobol_name,
                        sizeof(target_item->cobol_name)) != 0)
                    return (FT_FAILURE);
                {
                    const char *open_bracket;
                    size_t index_length;

                    open_bracket = std::strchr(target_reference, '[');
                    if (!open_bracket)
                        return (FT_FAILURE);
                    open_bracket += 1;
                    index_length = std::strcspn(open_bracket, "]");
                    if (index_length + 1 >= sizeof(index_expression) || open_bracket[index_length] != ']')
                        return (FT_FAILURE);
                    std::memcpy(index_expression, open_bracket, index_length);
                    index_expression[index_length] = '\0';
                }
                if (cblc_format_string_component_expression_reference(target_item->cobol_name,
                        index_expression, "-BUF", buffer_target, sizeof(buffer_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_format_string_component_expression_reference(target_item->cobol_name,
                        index_expression, "-LEN", length_target, sizeof(length_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (cblc_parse_occurs_reference_index(target_reference, &target_index) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_format_string_component_reference(target_item->cobol_name, 1, target_index,
                        "-BUF", buffer_target, sizeof(buffer_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_format_string_component_reference(target_item->cobol_name, 1, target_index,
                        "-LEN", length_target, sizeof(length_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        if (**cursor == '"')
        {
            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
                != FT_SUCCESS)
                return (FT_FAILURE);
            assigned_length = std::strlen(literal_buffer);
            cblc_update_string_capacity(unit, target_item, assigned_length);
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
                    cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
                    length_literal, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            if (literal_value == '"')
                ft_strlcat(cobol_source, "\"\"", sizeof(cobol_source));
            else
                ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            assigned_length = 1;
            cblc_update_string_capacity(unit, target_item, assigned_length);
            if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
                    cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
                    length_literal, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        {
            const char *expression_start;

            expression_start = *cursor;
            if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                    sizeof(expression_buffer)) == FT_SUCCESS)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor == ';')
                {
                    *cursor += 1;
                    return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                        buffer_target, expression_buffer, 0));
                }
            }
            *cursor = expression_start;
        }
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (std::strchr(source_reference, '['))
        {
            char source_base[TRANSPILE_IDENTIFIER_MAX];
            const char *open_bracket;
            size_t index_length;
            char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_cobol_reference_base(source_reference, source_base,
                    sizeof(source_base)) != FT_SUCCESS)
                return (FT_FAILURE);
            open_bracket = std::strchr(source_reference, '[');
            if (!open_bracket)
                return (FT_FAILURE);
            open_bracket += 1;
            index_length = std::strcspn(open_bracket, "]");
            if (index_length + 1 >= sizeof(index_expression)
                || open_bracket[index_length] != ']')
                return (FT_FAILURE);
            std::memcpy(index_expression, open_bracket, index_length);
            index_expression[index_length] = '\0';
            if (cblc_format_string_component_expression_reference(source_base,
                    index_expression, "-BUF", cobol_source, sizeof(cobol_source)) != FT_SUCCESS)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                std::strchr(target_reference, '[') ? buffer_target : target_reference,
                cobol_source, 0));
        }
        cblc_update_string_capacity(unit, target_item, source_item->length);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    if (effective_target_kind == CBLC_DATA_KIND_CHAR)
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
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, cobol_source, 1));
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
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, cobol_source, 1));
        }
        if (**cursor == '*')
        {
            t_cblc_data_kind dereference_kind;

            if (cblc_parse_pointer_dereference_reference(cursor, unit, &source_item,
                    &dereference_kind, source_reference, sizeof(source_reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (dereference_kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, source_reference, 0));
        }
        {
            const char *expression_start;

            expression_start = *cursor;
            if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL,
                    source_reference, sizeof(source_reference)) == FT_SUCCESS)
            {
                if (target_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                    && std::strchr(target_reference, '[')
                    && source_item
                    && source_item->kind != CBLC_DATA_KIND_CHAR
                    && !(source_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                        && std::strchr(source_reference, '[')))
                    return (FT_FAILURE);
                *cursor = expression_start;
            }
            if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                    sizeof(expression_buffer)) == FT_SUCCESS)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor == ';')
                {
                    *cursor += 1;
                    return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                            target_reference, expression_buffer, 0));
                }
            }
            *cursor = expression_start;
        }
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item
            || (source_item->kind != CBLC_DATA_KIND_CHAR
                && !(source_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                    && std::strchr(source_reference, '['))))
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    if (effective_target_kind == CBLC_DATA_KIND_DOUBLE)
    {
        const char *call_start;

        call_start = *cursor;
        if (cblc_parse_method_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                sizeof(expression_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
            target_reference, expression_buffer, 0));
    }
    if (effective_target_kind == CBLC_DATA_KIND_INT)
    {
        const char *call_start;
        const char *reference_start;
        t_cblc_data_item *source_item;
        int source_is_length_reference;

        call_start = *cursor;
        if (cblc_parse_method_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_std_strlen_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_function_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        reference_start = *cursor;
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, &source_is_length_reference,
                source_reference, sizeof(source_reference)) == FT_SUCCESS)
        {
            if (!source_is_length_reference && source_item
                && source_item->kind != CBLC_DATA_KIND_INT
                && source_item->kind != CBLC_DATA_KIND_DOUBLE
                && !(source_item->kind == CBLC_DATA_KIND_INT_POINTER
                    && std::strchr(source_reference, '[')))
                return (FT_FAILURE);
            if (!source_is_length_reference && source_item
                && (source_item->kind == CBLC_DATA_KIND_INT
                    || (source_item->kind == CBLC_DATA_KIND_INT_POINTER
                        && std::strchr(source_reference, '['))))
            {
                cblc_skip_whitespace(cursor);
                if (**cursor == ';')
                {
                    *cursor += 1;
                    return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                            target_reference, source_reference, 0));
                }
            }
        }
        *cursor = reference_start;
        if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                sizeof(expression_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                target_reference, expression_buffer, 0));
    }
    if (cblc_data_kind_is_pointer(target_item->kind))
    {
        t_cblc_data_item *source_item;
        const char *call_start;

        call_start = *cursor;
        if (cblc_parse_pointer_allocation_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_match_keyword(cursor, "NULL"))
        {
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, "NULL", 1));
        }
        if (cblc_parse_pointer_address_of_reference(cursor, unit, effective_target_kind,
                &source_item, source_reference, sizeof(source_reference)) == FT_SUCCESS)
        {
            if (effective_target_kind == CBLC_DATA_KIND_STRUCT_POINTER
                && std::strncmp(target_item->struct_type_name, source_item->struct_type_name,
                    sizeof(target_item->struct_type_name)) != 0)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, source_reference, 0));
        }
        *cursor = call_start;
        if (cblc_parse_pointer_arithmetic_reference(cursor, unit, effective_target_kind,
                &source_item, source_reference, sizeof(source_reference)) == FT_SUCCESS)
        {
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, source_reference, 0));
        }
        *cursor = call_start;
        {
            t_cblc_data_kind cast_kind;
            char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_parse_pointer_cast_reference(cursor, unit, effective_target_kind,
                    target_item->struct_type_name, 1, &source_item, &cast_kind,
                    cast_struct_type_name, sizeof(cast_struct_type_name), source_reference,
                    sizeof(source_reference)) == FT_SUCCESS)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor != ';')
                    return (FT_FAILURE);
                *cursor += 1;
                return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                        target_reference, source_reference, 0));
            }
        }
        *cursor = call_start;
        if (target_item->kind == effective_target_kind
            && cblc_parse_method_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (target_item->kind == effective_target_kind
            && cblc_parse_function_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (**cursor == '*')
        {
            t_cblc_data_kind source_dereference_kind;

            if (cblc_parse_pointer_dereference_reference(cursor, unit, &source_item,
                    &source_dereference_kind, source_reference, sizeof(source_reference))
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (!cblc_pointer_assignment_is_compatible(effective_target_kind,
                    source_dereference_kind))
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, source_reference, 0));
        }
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item
            || !cblc_pointer_assignment_is_compatible(effective_target_kind, source_item->kind))
            return (FT_FAILURE);
        if (effective_target_kind == CBLC_DATA_KIND_STRUCT_POINTER
            && source_item->kind == CBLC_DATA_KIND_STRUCT_POINTER
            && std::strncmp(target_item->struct_type_name, source_item->struct_type_name,
                sizeof(target_item->struct_type_name)) != 0)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    if (effective_target_kind == CBLC_DATA_KIND_STRUCT)
    {
        t_cblc_data_item *source_item;
        const char *call_start;
        const char *target_struct_type_name;

        target_struct_type_name = target_item->struct_type_name;
        call_start = *cursor;
        if (target_item->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_parse_method_call_assignment(cursor, unit, function, target_item)
                == FT_SUCCESS)
                return (FT_SUCCESS);
            *cursor = call_start;
            if (cblc_parse_function_call_assignment(cursor, unit, function, target_item)
                == FT_SUCCESS)
                return (FT_SUCCESS);
            *cursor = call_start;
        }
        if (**cursor == '*')
        {
            t_cblc_data_kind source_dereference_kind;

            if (cblc_parse_pointer_dereference_reference(cursor, unit, &source_item,
                    &source_dereference_kind, source_reference, sizeof(source_reference))
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (source_dereference_kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
        }
        else if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL,
                source_reference, sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || (source_item->kind != CBLC_DATA_KIND_STRUCT
                && source_item->kind != CBLC_DATA_KIND_STRUCT_POINTER))
            return (FT_FAILURE);
        if (std::strncmp(source_item->struct_type_name, target_struct_type_name,
                sizeof(source_item->struct_type_name)) != 0)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    return (FT_FAILURE);
}

static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char cobol_argument[TRANSPILE_STATEMENT_TEXT_MAX];
    int is_literal;

    if (!cursor || !*cursor || !unit || !function)
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
        t_cblc_data_item *item;
        int is_length_reference;
        char reference[TRANSPILE_STATEMENT_TEXT_MAX];

        if (**cursor == '*')
        {
            t_cblc_data_kind dereference_kind;

            if (cblc_parse_pointer_dereference_reference(cursor, unit, &item,
                    &dereference_kind, reference, sizeof(reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (dereference_kind != CBLC_DATA_KIND_INT
                && dereference_kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
            is_literal = 0;
        }
        else
        {
            if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
                    reference, sizeof(reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (is_length_reference)
                ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
            else
            {
                if (item->kind == CBLC_DATA_KIND_STRING)
                    ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
                else
                    ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
            }
            is_literal = 0;
        }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_append_statement(function, CBLC_STATEMENT_DISPLAY, NULL, cobol_argument, is_literal));
}

static int cblc_parse_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::", std::strlen("std::")) == 0)
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
    {
        const t_cblc_function *local_target;

        local_target = cblc_find_function_in_unit(unit, identifier);
        if (!local_target)
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

                local_target = cblc_instantiate_template_function(unit, identifier,
                    inferred_argument);
                if (!local_target)
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
                if (cblc_template_argument_ref(unit, inferred_argument,
                        &argument_ref_id) == FT_SUCCESS)
                    cblc_record_function_template_use(unit, identifier,
                        argument_ref_id, cblc_source_offset(unit, start));
                ft_strlcpy(identifier, local_target->source_name,
                    sizeof(identifier));
            }
        }
        if (local_target)
        {
            size_t argument_index;
            t_cblc_statement probe_statement;

            if (local_target->parameter_count == argument_count)
            {
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
                            &local_target->parameters[argument_index]))
                    {
                        if (local_target->is_template_instantiation)
                            cblc_record_template_instantiation_error(unit,
                                TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED,
                                "template function instantiation failed",
                                local_target->template_source_name,
                                local_target->template_argument_name,
                                cblc_template_definition_offset(unit,
                                    local_target->template_source_name, 2),
                                cblc_source_offset(unit, start));
                        *cursor = start;
                        return (FT_FAILURE);
                    }
                    argument_index += 1;
                }
            }
        }
    }
    *cursor += 1;
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
    if (function->statement_count == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    cblc_exception_source_position(unit, start, &statement->exception_source_line,
        &statement->exception_source_column);
    ft_strlcpy(statement->call_identifier, identifier, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char normalized_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    call_arguments[0] = '\0';
    normalized_call_arguments[0] = '\0';
    argument_count = 0;
    if (cblc_parse_builtin_string_append_call(cursor, unit, function) == FT_SUCCESS)
        return (FT_SUCCESS);
    *cursor = start;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    method = cblc_find_method_on_type(receiver_type, method_name);
    if (!method || method->return_kind != CBLC_FUNCTION_RETURN_VOID)
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
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
            receiver_item->cobol_name, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    cblc_exception_source_position(unit, start, &statement->exception_source_line,
        &statement->exception_source_column);
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, normalized_call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static size_t cblc_exception_type_id(const char *type_name)
{
    size_t index;
    std::uint32_t hash;
    size_t abi_version;

    if (!type_name || type_name[0] == '\0')
        return (0);
    hash = 2166136261u;
    index = 0;
    while (type_name[index] != '\0')
    {
        hash ^= static_cast<unsigned char>(type_name[index]);
        hash *= 16777619u;
        index += 1;
    }
    hash ^= static_cast<unsigned char>('@');
    hash *= 16777619u;
    abi_version = CBLC_EXCEPTION_ABI_VERSION;
    while (abi_version > 0)
    {
        hash ^= static_cast<unsigned char>(abi_version & 0xffu);
        hash *= 16777619u;
        abi_version >>= 8;
    }
    hash %= 999999937u;
    return (hash == 0 ? 1 : static_cast<size_t>(hash));
}

static void cblc_function_mark_unknown_exception(t_cblc_function *function)
{
    if (function)
        function->exception_types_unknown = 1;
}

static void cblc_function_add_exception_type(t_cblc_function *function, size_t type_id)
{
    size_t index;

    if (!function || type_id == 0)
        return ;
    index = 0;
    while (index < function->exception_type_count)
    {
        if (function->exception_type_ids[index] == type_id)
            return ;
        index += 1;
    }
    if (function->exception_type_count >= CBLC_EXCEPTION_THROW_SET_MAX)
    {
        function->exception_types_unknown = 1;
        return ;
    }
    function->exception_type_ids[function->exception_type_count] = type_id;
    function->exception_type_count += 1;
}

static void cblc_function_merge_exception_summary(t_cblc_function *function,
    size_t type_count, const size_t *type_ids, int types_unknown)
{
    size_t index;

    if (!function)
        return ;
    if (types_unknown)
        function->exception_types_unknown = 1;
    index = 0;
    while (type_ids && index < type_count)
    {
        cblc_function_add_exception_type(function, type_ids[index]);
        index += 1;
    }
}

static void cblc_collect_statement_exception_summary(const t_cblc_statement *statements,
    size_t statement_count, t_cblc_function *summary)
{
    size_t index;

    if (!summary)
        return ;
    index = 0;
    while (index < statement_count)
    {
        const t_cblc_statement *statement;

        statement = &statements[index];
        if (statement->type == CBLC_STATEMENT_THROW)
        {
            if (statement->exception_is_rethrow)
                cblc_function_mark_unknown_exception(summary);
            else
                cblc_function_add_exception_type(summary, statement->exception_type_id);
        }
        else if (statement->type == CBLC_STATEMENT_FILE_OPEN
            || statement->type == CBLC_STATEMENT_FILE_READ
            || statement->type == CBLC_STATEMENT_FILE_WRITE
            || statement->type == CBLC_STATEMENT_FILE_CLOSE
            || ((statement->type == CBLC_STATEMENT_CALL
                || statement->type == CBLC_STATEMENT_CALL_ASSIGN
                || statement->type == CBLC_STATEMENT_METHOD_CALL
                || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                && statement->exception_may_throw))
            cblc_function_mark_unknown_exception(summary);
        index += 1;
    }
}

static int cblc_exception_type_derives_from(const t_cblc_translation_unit *unit,
    const char *derived_type_name, const char *base_type_name, size_t depth)
{
    const t_cblc_struct_type *derived_type;

    if (!unit || !derived_type_name || !base_type_name || depth > unit->struct_type_count)
        return (0);
    if (std::strcmp(derived_type_name, base_type_name) == 0)
        return (1);
    derived_type = cblc_find_struct_type(unit, derived_type_name);
    if (!derived_type || !derived_type->has_base_type)
        return (0);
    if (!derived_type->base_is_public)
        return (0);
    return (cblc_exception_type_derives_from(unit, derived_type->base_type_name,
        base_type_name, depth + 1));
}

static int cblc_parse_exception_type(const char **cursor, char *type_name,
    size_t type_name_size, int *is_reference)
{
    char first[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !type_name || type_name_size == 0 || !is_reference)
        return (FT_FAILURE);
    type_name[0] = '\0';
    *is_reference = 0;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, first, sizeof(first)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(first, "const", sizeof(first)) == 0)
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, first, sizeof(first)) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(type_name, first, type_name_size);
    cblc_skip_whitespace(cursor);
    if (**cursor == '&')
    {
        *cursor += 1;
        *is_reference = 1;
        cblc_skip_whitespace(cursor);
    }
    return (FT_SUCCESS);
}

static int cblc_exception_struct_is_trivial(const t_cblc_translation_unit *unit,
    const char *type_name, size_t depth)
{
    const t_cblc_struct_type *type;
    size_t index;

    if (!unit || !type_name || type_name[0] == '\0' || depth > unit->struct_type_count)
        return (0);
    type = cblc_find_struct_type(unit, type_name);
    if (!type || type->has_destructor || type->destructor_statement_count > 0)
        return (0);
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;

        field = &type->fields[index];
        if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (!cblc_exception_struct_is_trivial(unit, field->struct_type_name,
                    depth + 1))
                return (0);
        }
        else if (field->kind != CBLC_DATA_KIND_INT && field->kind != CBLC_DATA_KIND_CHAR)
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_exception_struct_is_copyable(const t_cblc_translation_unit *unit,
    const char *type_name, size_t depth)
{
    const t_cblc_struct_type *type;
    size_t index;

    if (!unit || !type_name || type_name[0] == '\0' || depth > unit->struct_type_count)
        return (0);
    type = cblc_find_struct_type(unit, type_name);
    if (!type || type->has_destructor || type->destructor_statement_count > 0)
        return (0);
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;

        field = &type->fields[index];
        if (field->array_count > 0)
            return (0);
        if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (!cblc_exception_struct_is_copyable(unit, field->struct_type_name,
                    depth + 1))
                return (0);
        }
        else if (field->kind == CBLC_DATA_KIND_STRING)
        {
            /* Dynamic strings are copied through their owned allocation below;
             * arrays remain rejected because their element lifetime metadata is
             * not yet represented by the exception payload ABI. */
        }
        else if (field->kind != CBLC_DATA_KIND_INT && field->kind != CBLC_DATA_KIND_CHAR)
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_exception_payload_name(const char *type_name, char *buffer,
    size_t buffer_size)
{
    char normalized[TRANSPILE_IDENTIFIER_MAX];

    if (!type_name || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    cblc_identifier_to_cobol(type_name, normalized, sizeof(normalized));
    if (std::snprintf(buffer, buffer_size, "CBLC-EX-PAYLOAD-%s", normalized) < 0)
        return (FT_FAILURE);
    return (std::strlen(buffer) < buffer_size
        ? FT_SUCCESS : FT_FAILURE);
}

static int cblc_ensure_exception_payload_item(t_cblc_translation_unit *unit,
    const char *type_name, char *payload_name, size_t payload_name_size)
{
    char source_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !type_name || !payload_name || payload_name_size == 0
        || (!cblc_exception_struct_is_trivial(unit, type_name, 0)
            && !cblc_exception_struct_is_copyable(unit, type_name, 0))
        || cblc_exception_payload_name(type_name, payload_name, payload_name_size) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(source_name, sizeof(source_name), "__cblc_exception_payload_%s",
            type_name) < 0)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].source_name, source_name,
                sizeof(unit->data_items[index].source_name)) == 0)
            return (FT_SUCCESS);
        index += 1;
    }
    return (cblc_add_named_data_item(unit, source_name, payload_name,
        CBLC_DATA_KIND_STRUCT, 0, 0, type_name, NULL, 0, 0, 0));
}

static void cblc_exception_source_position(const t_cblc_translation_unit *unit,
    const char *position, size_t *line, size_t *column)
{
    const char *cursor;

    if (!line || !column)
        return ;
    *line = 0;
    *column = 0;
    if (!unit || !unit->source_text || !position || position < unit->source_text)
        return ;
    *line = 1;
    *column = 1;
    cursor = unit->source_text;
    while (cursor < position && *cursor != '\0')
    {
        if (*cursor == '\n')
        {
            *line += 1;
            *column = 1;
        }
        else
            *column += 1;
        cursor += 1;
    }
}

static int cblc_parse_throw_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal[TRANSPILE_STATEMENT_TEXT_MAX];
    char type_name[TRANSPILE_IDENTIFIER_MAX];
    char payload_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;
    t_cblc_statement *statement;
    int is_rethrow;
    int is_struct_payload;
    int is_string_payload;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "throw"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    is_rethrow = (**cursor == ';');
    is_struct_payload = 0;
    is_string_payload = 0;
    payload_name[0] = '\0';
    item = NULL;
    expression[0] = '\0';
    if (is_rethrow)
    {
        if (!function->exception_in_catch)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(type_name, "exception", sizeof(type_name));
    }
    else if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal, sizeof(literal)) != FT_SUCCESS)
            goto fail;
        ft_strlcpy(expression, "\"", sizeof(expression));
        if (std::strlen(literal) + 2 >= sizeof(expression))
            goto fail;
        ft_strlcat(expression, literal, sizeof(expression));
        ft_strlcat(expression, "\"", sizeof(expression));
        ft_strlcpy(type_name, "string", sizeof(type_name));
        is_string_payload = 1;
    }
    else
    {
        const char *value_start;
        int is_length_reference;

        value_start = *cursor;
        if (cblc_parse_numeric_expression(cursor, unit, expression, sizeof(expression))
            == FT_SUCCESS)
        {
            item = cblc_find_data_item(unit, expression);
            if (item)
            {
                if (item->kind == CBLC_DATA_KIND_INT)
                    ft_strlcpy(type_name, "int", sizeof(type_name));
                else
                    goto fail;
            }
            else
                ft_strlcpy(type_name, "int", sizeof(type_name));
        }
        else
        {
            *cursor = value_start;
            if (cblc_parse_data_reference_text(cursor, unit, &item,
                    &is_length_reference, expression, sizeof(expression)) != FT_SUCCESS
                || is_length_reference || !item)
                goto fail;
            if (item->kind == CBLC_DATA_KIND_STRING)
            {
                ft_strlcpy(type_name, "string", sizeof(type_name));
                is_string_payload = 1;
            }
            else if (item->kind == CBLC_DATA_KIND_STRUCT)
            {
                ft_strlcpy(type_name, item->struct_type_name, sizeof(type_name));
                is_struct_payload = 1;
            }
            else if (item->kind == CBLC_DATA_KIND_INT)
                ft_strlcpy(type_name, "int", sizeof(type_name));
            else
                goto fail;
        }
        if (item && item->kind == CBLC_DATA_KIND_STRUCT)
            is_struct_payload = 1;
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        goto fail;
    *cursor += 1;
    if (is_struct_payload
        && cblc_ensure_exception_payload_item(unit, type_name, payload_name,
            sizeof(payload_name)) != FT_SUCCESS)
        goto fail;
    if (cblc_append_statement(function, CBLC_STATEMENT_THROW, NULL, expression,
            !is_rethrow && expression[0] == '"') != FT_SUCCESS)
        goto fail;
    statement = &function->statements[function->statement_count - 1];
    cblc_exception_source_position(unit, start, &statement->exception_source_line,
        &statement->exception_source_column);
    statement->exception_type_id = cblc_exception_type_id(type_name);
    ft_strlcpy(statement->exception_type_name, type_name,
        sizeof(statement->exception_type_name));
    statement->exception_is_rethrow = is_rethrow;
    if (is_rethrow)
        cblc_function_mark_unknown_exception(function);
    else
        cblc_function_add_exception_type(function, statement->exception_type_id);
    statement->exception_from_catch = function->exception_in_catch;
    statement->exception_to_outer_region = function->exception_in_catch
        && function->exception_region_depth > 0;
    statement->exception_payload_is_struct = is_struct_payload;
    statement->exception_payload_is_string = is_string_payload;
    if (is_struct_payload)
    {
        ft_strlcpy(statement->exception_payload_struct_type, type_name,
            sizeof(statement->exception_payload_struct_type));
        ft_strlcpy(statement->exception_payload_cobol_name, payload_name,
            sizeof(statement->exception_payload_cobol_name));
        statement->exception_payload_requires_copy =
            !cblc_exception_struct_is_trivial(unit, type_name, 0);
    }
    statement->exception_has_region = function->exception_region_depth > 0
        || function->exception_in_catch;
    if (function->exception_region_depth > 0)
    {
        statement->exception_region_id = function->exception_region_stack[
            function->exception_region_depth - 1];
        statement->exception_cleanup_base = function->exception_cleanup_base_stack[
            function->exception_region_depth - 1];
    }
    else
    {
        statement->exception_region_id = function->active_catch_region_id;
        statement->exception_cleanup_base = function->exception_region_depth > 0
            ? function->active_catch_cleanup_base : 0;
    }
    while (function->local_destructor_count > statement->exception_cleanup_base
        && statement->exception_cleanup_count < function->local_destructor_count
            - statement->exception_cleanup_base
        && statement->exception_cleanup_count < CBLC_EXCEPTION_CLEANUP_MAX)
    {
        size_t cleanup_index;

        cleanup_index = function->local_destructor_count
            - statement->exception_cleanup_count - 1;
        ft_strlcpy(statement->exception_cleanup_targets[statement->exception_cleanup_count],
            function->local_destructor_targets[cleanup_index],
            sizeof(statement->exception_cleanup_targets[statement->exception_cleanup_count]));
        statement->exception_cleanup_count += 1;
    }
    if (statement->exception_cleanup_count < function->local_destructor_count
            - statement->exception_cleanup_base)
        goto fail;
    return (FT_SUCCESS);
fail:
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_parse_try_statement(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    size_t region_id;
    size_t saved_catch_region;
    size_t saved_catch_cleanup_base;
    int saved_in_catch;
    int catch_count;
    int seen_catch_all;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "try"))
        return (FT_FAILURE);
    if (function->exception_region_depth >= CBLC_EXCEPTION_REGION_MAX)
        goto fail;
    region_id = function->next_exception_region_id++;
    function->exception_cleanup_base_stack[function->exception_region_depth]
        = function->local_destructor_count;
    if (cblc_append_statement(function, CBLC_STATEMENT_TRY_BEGIN, NULL, NULL, 0)
        != FT_SUCCESS)
        goto fail;
    function->statements[function->statement_count - 1].exception_region_id = region_id;
    cblc_exception_source_position(unit, start,
        &function->statements[function->statement_count - 1].exception_source_line,
        &function->statements[function->statement_count - 1].exception_source_column);
    function->exception_region_stack[function->exception_region_depth++] = region_id;
    if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS)
        goto fail;
    function->exception_region_depth -= 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_TRY_BODY_END, NULL, NULL, 0)
        != FT_SUCCESS)
        goto fail;
    function->statements[function->statement_count - 1].exception_region_id = region_id;
    catch_count = 0;
    saved_catch_region = function->active_catch_region_id;
    saved_catch_cleanup_base = function->active_catch_cleanup_base;
    saved_in_catch = function->exception_in_catch;
    seen_catch_all = 0;
    while (1)
    {
        const char *catch_start;
        char type_name[TRANSPILE_IDENTIFIER_MAX];
        char catch_name[TRANSPILE_IDENTIFIER_MAX];
        char catch_cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char catch_payload_name[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_statement *statement;
        size_t catch_data_index;
        int is_catch_all;
        int is_struct_catch;
        int is_reference;

        cblc_skip_whitespace(cursor);
        catch_start = *cursor;
        if (!cblc_match_keyword(cursor, "catch"))
            break;
        cblc_skip_whitespace(cursor);
        if (**cursor != '(')
            goto fail_catch;
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        is_catch_all = (std::strncmp(*cursor, "...", 3) == 0);
        type_name[0] = '\0';
        catch_name[0] = '\0';
        catch_payload_name[0] = '\0';
        catch_data_index = static_cast<size_t>(-1);
        is_struct_catch = 0;
        is_reference = 0;
        if (is_catch_all)
        {
            *cursor += 3;
        }
        else
        {
            if (cblc_parse_exception_type(cursor, type_name, sizeof(type_name),
                    &is_reference) != FT_SUCCESS)
                goto fail_catch;
            cblc_skip_whitespace(cursor);
            if (**cursor != ')'
                && cblc_parse_identifier(cursor, catch_name, sizeof(catch_name)) != FT_SUCCESS)
                goto fail_catch;
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ')')
            goto fail_catch;
        *cursor += 1;
        if (!is_catch_all
            && std::strncmp(type_name, "int", sizeof(type_name)) != 0
            && std::strncmp(type_name, "string", sizeof(type_name)) != 0)
        {
            if ((!cblc_exception_struct_is_trivial(unit, type_name, 0)
                    && !cblc_exception_struct_is_copyable(unit, type_name, 0))
                || cblc_ensure_exception_payload_item(unit, type_name,
                    catch_payload_name, sizeof(catch_payload_name)) != FT_SUCCESS)
                goto fail_catch;
            is_struct_catch = 1;
        }
        if (!is_catch_all)
        {
            size_t body_index;
            int saw_known_throw;
            int saw_matching_throw;
            int saw_unknown_throw;

            body_index = 0;
            saw_known_throw = 0;
            saw_matching_throw = 0;
            saw_unknown_throw = 0;
            while (body_index < function->statement_count)
            {
                const t_cblc_statement *body_statement;

                body_statement = &function->statements[body_index];
                if (body_statement->type == CBLC_STATEMENT_TRY_BODY_END
                    && body_statement->exception_region_id == region_id)
                    break ;
                if (body_statement->type == CBLC_STATEMENT_THROW
                    && body_statement->exception_region_id == region_id
                    && !body_statement->exception_is_rethrow)
                {
                    saw_known_throw = 1;
                    if (std::strncmp(body_statement->exception_type_name, type_name,
                            sizeof(body_statement->exception_type_name)) == 0
                        || cblc_exception_type_derives_from(unit,
                            body_statement->exception_type_name, type_name, 0))
                        saw_matching_throw = 1;
                }
                else if ((body_statement->type == CBLC_STATEMENT_CALL
                        || body_statement->type == CBLC_STATEMENT_CALL_ASSIGN
                        || body_statement->type == CBLC_STATEMENT_METHOD_CALL
                        || body_statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                        || body_statement->type == CBLC_STATEMENT_FILE_OPEN
                        || body_statement->type == CBLC_STATEMENT_FILE_READ
                        || body_statement->type == CBLC_STATEMENT_FILE_WRITE
                        || body_statement->type == CBLC_STATEMENT_FILE_CLOSE))
                    saw_unknown_throw = 1;
                body_index += 1;
            }
            if (saw_known_throw && !saw_matching_throw && !saw_unknown_throw)
                goto fail_catch;
        }
        {
            size_t prior_index;
            size_t current_type_id;

            current_type_id = is_catch_all ? 0 : cblc_exception_type_id(type_name);
            prior_index = 0;
            while (prior_index < function->statement_count)
            {
                const t_cblc_statement *prior;

                prior = &function->statements[prior_index];
                if (prior->type == CBLC_STATEMENT_CATCH_BEGIN
                    && prior->exception_region_id == region_id)
                {
                    if (prior->exception_is_catch_all
                        || (is_catch_all ? 0 : prior->exception_type_id == current_type_id)
                        || (!is_catch_all && prior->exception_type_name[0] != '\0'
                            && cblc_exception_type_derives_from(unit, type_name,
                                prior->exception_type_name, 0)))
                        goto fail_catch;
                }
                prior_index += 1;
            }
            if (seen_catch_all)
                goto fail_catch;
        }
        if (catch_name[0] != '\0')
        {
            cblc_identifier_to_cobol(catch_name, catch_cobol_name, sizeof(catch_cobol_name));
            if (is_reference)
            {
                if (!is_struct_catch)
                {
                    if (std::strncmp(type_name, "string", sizeof(type_name)) == 0)
                        ft_strlcpy(catch_payload_name, "CBLC-EX-STRING",
                            sizeof(catch_payload_name));
                    else
                        ft_strlcpy(catch_payload_name, "CBLC-EX-INT-PAYLOAD",
                            sizeof(catch_payload_name));
                }
                if (cblc_add_named_data_item(unit, catch_name, catch_payload_name,
                        is_struct_catch ? CBLC_DATA_KIND_STRUCT
                            : (std::strncmp(type_name, "string", sizeof(type_name)) == 0
                                ? CBLC_DATA_KIND_STRING : CBLC_DATA_KIND_INT),
                        std::strncmp(type_name, "string", sizeof(type_name)) == 0 ? 256 : 0,
                        0, is_struct_catch ? type_name : NULL, function->source_name,
                        1, 1, 0) != FT_SUCCESS)
                    goto fail_catch;
                catch_data_index = unit->data_count - 1;
            }
            else if (std::strncmp(type_name, "string", sizeof(type_name)) == 0)
            {
                if (cblc_add_named_data_item(unit, catch_name, catch_cobol_name,
                        CBLC_DATA_KIND_STRING, 256, 0, NULL, function->source_name,
                        1, 0, 1) != FT_SUCCESS)
                    goto fail_catch;
                catch_data_index = unit->data_count - 1;
            }
            else if (is_struct_catch
                ? cblc_add_named_data_item(unit, catch_name, catch_cobol_name,
                    CBLC_DATA_KIND_STRUCT, 0, 0, type_name, function->source_name,
                    1, 0, 0) != FT_SUCCESS
                : cblc_add_named_data_item(unit, catch_name, catch_cobol_name,
                    CBLC_DATA_KIND_INT, 0, 0, NULL, function->source_name,
                    1, 0, 0) != FT_SUCCESS)
                goto fail_catch;
            else
                catch_data_index = unit->data_count - 1;
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_CATCH_BEGIN,
                catch_name, NULL, 0) != FT_SUCCESS)
            goto fail_catch;
        statement = &function->statements[function->statement_count - 1];
        statement->exception_region_id = region_id;
        statement->exception_type_id = is_catch_all ? 0 : cblc_exception_type_id(type_name);
        if (!is_catch_all)
            ft_strlcpy(statement->exception_type_name, type_name,
                sizeof(statement->exception_type_name));
        statement->exception_is_catch_all = is_catch_all;
        statement->exception_catch_is_reference = is_reference;
        cblc_exception_source_position(unit, catch_start,
            &statement->exception_source_line, &statement->exception_source_column);
        if (is_struct_catch)
            ft_strlcpy(statement->exception_payload_cobol_name, catch_payload_name,
                sizeof(statement->exception_payload_cobol_name));
        seen_catch_all = seen_catch_all || is_catch_all;
        function->active_catch_region_id = region_id;
        function->active_catch_cleanup_base = function->local_destructor_count;
        function->exception_in_catch = 1;
        if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS)
            goto fail_catch;
        if (catch_data_index != static_cast<size_t>(-1))
            unit->data_items[catch_data_index].is_active = 0;
        function->active_catch_region_id = saved_catch_region;
        function->active_catch_cleanup_base = saved_catch_cleanup_base;
        function->exception_in_catch = saved_in_catch;
        if (cblc_append_statement(function, CBLC_STATEMENT_CATCH_END, NULL, NULL, 0)
            != FT_SUCCESS)
            goto fail_catch;
        function->statements[function->statement_count - 1].exception_region_id = region_id;
        catch_count += 1;
    }
    if (catch_count == 0)
        goto fail;
    if (cblc_append_statement(function, CBLC_STATEMENT_TRY_END, NULL, NULL, 0)
        != FT_SUCCESS)
        goto fail;
    function->statements[function->statement_count - 1].exception_region_id = region_id;
    function->active_catch_region_id = saved_catch_region;
    function->active_catch_cleanup_base = saved_catch_cleanup_base;
    function->exception_in_catch = saved_in_catch;
    return (FT_SUCCESS);
fail_catch:
    function->active_catch_region_id = saved_catch_region;
    function->active_catch_cleanup_base = saved_catch_cleanup_base;
    function->exception_in_catch = saved_in_catch;
fail:
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "return"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (function->return_kind == CBLC_FUNCTION_RETURN_VOID)
    {
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        function->saw_return = 1;
        if (cblc_append_local_destructor_statements(function) != FT_SUCCESS)
            return (FT_FAILURE);
        return (cblc_append_statement(function, CBLC_STATEMENT_RETURN, NULL, NULL, 0));
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT
        || function->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
    {
        if (cblc_parse_numeric_expression(cursor, unit, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (cblc_parse_data_reference_text(cursor, unit, &item, NULL, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!item)
            return (FT_FAILURE);
        if (item->kind == CBLC_DATA_KIND_STRUCT
            && std::strncmp(item->struct_type_name, function->return_type_name,
                sizeof(item->struct_type_name)) == 0)
            ;
        else
        {
            size_t template_parameter_index;

            if (item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                || std::strchr(expression, '[') == NULL
                || cblc_active_template_parameter_index(item->struct_type_name,
                    &template_parameter_index) != FT_SUCCESS
                || std::strncmp(item->struct_type_name, function->return_type_name,
                    sizeof(item->struct_type_name)) != 0)
                return (FT_FAILURE);
        }
    }
    else if (function->return_reference_kind != CBLC_REFERENCE_NONE)
    {
        t_cblc_data_kind expected_kind;
        t_cblc_data_item *referent_item;
        int referent_is_function_local;
        int referent_is_indexed_pointer;

        if (function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
            expected_kind = CBLC_DATA_KIND_INT;
        else if (function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
            expected_kind = CBLC_DATA_KIND_CHAR;
        else
            expected_kind = std::strncmp(function->return_type_name, "string",
                sizeof(function->return_type_name)) == 0
                ? CBLC_DATA_KIND_STRING : CBLC_DATA_KIND_STRUCT;
        if (cblc_parse_data_reference_text(cursor, unit, &referent_item, NULL,
                expression, sizeof(expression)) != FT_SUCCESS || !referent_item)
            return (FT_FAILURE);
        referent_is_indexed_pointer =
            (expected_kind == CBLC_DATA_KIND_INT
                && referent_item->kind == CBLC_DATA_KIND_INT_POINTER)
            || (expected_kind == CBLC_DATA_KIND_CHAR
                && referent_item->kind == CBLC_DATA_KIND_CHAR_POINTER)
            || (expected_kind == CBLC_DATA_KIND_STRUCT
                && referent_item->kind == CBLC_DATA_KIND_STRUCT_POINTER);
        if (referent_item->kind != expected_kind && !referent_is_indexed_pointer)
            return (FT_FAILURE);
        if (expected_kind == CBLC_DATA_KIND_STRUCT
            && std::strncmp(referent_item->struct_type_name, function->return_type_name,
                sizeof(referent_item->struct_type_name)) != 0)
            return (FT_FAILURE);
        referent_is_function_local = referent_item->is_function_local;
        if (!referent_is_function_local && referent_item->is_alias)
        {
            size_t data_index;

            data_index = 0;
            while (data_index < unit->data_count)
            {
                if (unit->data_items[data_index].is_function_local
                    && std::strncmp(unit->data_items[data_index].cobol_name,
                        referent_item->cobol_name,
                        sizeof(referent_item->cobol_name)) == 0)
                {
                    referent_is_function_local = 1;
                    break;
                }
                if (unit->data_items[data_index].reference_kind == CBLC_REFERENCE_NONE
                    && std::strncmp(unit->data_items[data_index].owner_function_name,
                        function->source_name,
                        sizeof(function->source_name)) == 0
                    && std::strncmp(unit->data_items[data_index].cobol_name,
                        referent_item->cobol_name,
                        sizeof(referent_item->cobol_name)) == 0)
                {
                    referent_is_function_local = 1;
                    break;
                }
                data_index += 1;
            }
        }
        if (referent_is_function_local
            || (referent_item->reference_kind != CBLC_REFERENCE_NONE
                && referent_item->reference_target_index < unit->data_count
                && unit->data_items[referent_item->reference_target_index].is_function_local))
        {
            unit->parse_error_code = TRANSPILE_ERROR_SEMANTIC_REFERENCE_BINDING;
            std::snprintf(unit->parse_error_message, sizeof(unit->parse_error_message),
                "reference return from '%s' would outlive its local target",
                function->source_name);
            return (FT_FAILURE);
        }
    }
    else if (function->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
        || function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
        || function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
        || function->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        t_cblc_data_kind return_pointer_kind;

        if (function->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
            return_pointer_kind = CBLC_DATA_KIND_VOID_POINTER;
        else if (function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
            return_pointer_kind = CBLC_DATA_KIND_CHAR_POINTER;
        else if (function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
            return_pointer_kind = CBLC_DATA_KIND_INT_POINTER;
        else
            return_pointer_kind = CBLC_DATA_KIND_STRUCT_POINTER;
        {
            char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_parse_pointer_cast_reference(cursor, unit, return_pointer_kind,
                    function->return_type_name, 1, &item, &return_pointer_kind,
                    cast_struct_type_name, sizeof(cast_struct_type_name),
                    expression, sizeof(expression)) != FT_SUCCESS)
            {
                if (function->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
                    return_pointer_kind = CBLC_DATA_KIND_VOID_POINTER;
                else if (function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
                    return_pointer_kind = CBLC_DATA_KIND_CHAR_POINTER;
                else if (function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
                    return_pointer_kind = CBLC_DATA_KIND_INT_POINTER;
                else
                    return_pointer_kind = CBLC_DATA_KIND_STRUCT_POINTER;
                if (cblc_parse_pointer_address_of_reference(cursor, unit, return_pointer_kind,
                        &item, expression, sizeof(expression)) != FT_SUCCESS)
                {
                    if (cblc_parse_pointer_arithmetic_reference(cursor, unit,
                            return_pointer_kind, &item, expression,
                            sizeof(expression)) != FT_SUCCESS)
                    {
                        if (cblc_parse_data_reference_text(cursor, unit, &item, NULL,
                                expression, sizeof(expression)) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    if (!item || !cblc_pointer_assignment_is_compatible(return_pointer_kind,
                            item->kind))
                        return (FT_FAILURE);
                }
                if (return_pointer_kind == CBLC_DATA_KIND_STRUCT_POINTER
                    && std::strncmp(item->struct_type_name, function->return_type_name,
                        sizeof(item->struct_type_name)) != 0)
                    return (FT_FAILURE);
            }
        }
    }
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    function->saw_return = 1;
    if (function->return_cobol_name[0] == '\0')
        return (FT_FAILURE);
    if (cblc_append_local_destructor_statements(function) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_RETURN,
            function->return_cobol_name, expression, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
