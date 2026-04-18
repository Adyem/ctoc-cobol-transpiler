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
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
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
        if (target_item->kind == effective_target_kind
            && cblc_parse_pointer_allocation_assignment(cursor, unit, function, target_item)
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
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, normalized_call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
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
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
    {
        if (cblc_parse_numeric_expression(cursor, unit, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (cblc_parse_identifier(cursor, expression, sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        item = cblc_find_data_item(unit, expression);
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT
            || std::strncmp(item->struct_type_name, function->return_type_name,
                sizeof(item->struct_type_name)) != 0)
            return (FT_FAILURE);
        ft_strlcpy(expression, item->cobol_name, sizeof(expression));
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
                    if (cblc_parse_data_reference_text(cursor, unit, &item, NULL,
                            expression, sizeof(expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
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
