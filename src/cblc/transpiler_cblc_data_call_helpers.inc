static t_cblc_data_item *cblc_find_data_item(t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = unit->data_count;
    while (index > 0)
    {
        index -= 1;
        if (!unit->data_items[index].is_active)
            continue ;
        if (std::strncmp(unit->data_items[index].source_name, identifier,
                sizeof(unit->data_items[index].source_name)) == 0)
            return (&unit->data_items[index]);
    }
    return (NULL);
}

static const t_cblc_data_item *cblc_find_data_item_const(
    const t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = unit->data_count;
    while (index > 0)
    {
        index -= 1;
        if (!unit->data_items[index].is_active)
            continue ;
        if (std::strncmp(unit->data_items[index].source_name, identifier,
                sizeof(unit->data_items[index].source_name)) == 0)
            return (&unit->data_items[index]);
    }
    return (NULL);
}

static int cblc_parse_string_constructor_clause(const char **cursor,
    t_cblc_translation_unit *unit, size_t *out_length, char *out_arguments,
    size_t out_arguments_size, size_t *out_argument_count)
{
    const t_cblc_data_item *actual_item;
    const t_cblc_data_item *item;
    const char *start;
    const char *literal_cursor;
    t_cblc_statement statement;
    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!cursor || !*cursor || !unit || !out_length || !out_arguments
        || out_arguments_size == 0 || !out_argument_count)
        return (FT_FAILURE);
    start = *cursor;
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, out_arguments, out_arguments_size,
            out_argument_count) != FT_SUCCESS)
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
    if (*out_argument_count != 1)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, out_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = *out_argument_count;
    if (cblc_extract_call_argument(&statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (argument[0] == '"')
    {
        literal_cursor = argument;
        if (cblc_parse_string_literal(&literal_cursor, literal_buffer, sizeof(literal_buffer))
            != FT_SUCCESS || *literal_cursor != '\0')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *out_length = std::strlen(literal_buffer);
        if (*out_length == 0)
            *out_length = 1;
        return (FT_SUCCESS);
    }
    item = cblc_find_data_item_const(unit, argument);
    if (!item || item->kind != CBLC_DATA_KIND_STRING || item->array_count > 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    actual_item = cblc_find_data_item_by_cobol(unit, item->cobol_name);
    if (actual_item && !actual_item->is_alias)
        ft_strlcpy(out_arguments, actual_item->source_name, out_arguments_size);
    *out_length = item->length;
    if (*out_length == 0)
        *out_length = 1;
    return (FT_SUCCESS);
}

#include "src/cblc/transpiler_cblc_references.inc"
#include "src/cblc/transpiler_cblc_struct_parse.inc"

static int cblc_parse_call_argument_list(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size,
    size_t *out_count)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t count;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0 || !out_count)
        return (FT_FAILURE);
    buffer[0] = '\0';
    count = 0;
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
    {
        *out_count = 0;
        return (FT_SUCCESS);
    }
    while (1)
    {
        {
            const char *argument_start;
            const char *argument_end;
            t_cblc_data_item *argument_item;
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
            int is_length_reference;

            argument_start = *cursor;
            if (**cursor == '"')
            {
                if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                expression[0] = '"';
                expression[1] = '\0';
                ft_strlcat(expression, literal_buffer, sizeof(expression));
                ft_strlcat(expression, "\"", sizeof(expression));
            }
            else if (**cursor == '(')
            {
                t_cblc_data_item *cast_item;
                t_cblc_data_kind cast_kind;
                char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_parse_pointer_cast_reference(cursor, unit,
                        CBLC_DATA_KIND_VOID_POINTER, NULL, 0, &cast_item, &cast_kind,
                        cast_struct_type_name, sizeof(cast_struct_type_name),
                        expression, sizeof(expression)) != FT_SUCCESS)
                {
                    *cursor = argument_start;
                    if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                            sizeof(expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else if (cblc_parse_data_reference_text(cursor, unit, &argument_item, &is_length_reference,
                    expression, sizeof(expression)) == FT_SUCCESS)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor != ')' && **cursor != ',')
                {
                    *cursor = argument_start;
                    if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                            sizeof(expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    size_t argument_length;

                    argument_end = *cursor;
                    while (argument_end > argument_start
                        && std::isspace(static_cast<unsigned char>(argument_end[-1])))
                        argument_end -= 1;
                    argument_length = static_cast<size_t>(argument_end - argument_start);
                    if (argument_length + 1 > sizeof(expression))
                        return (FT_FAILURE);
                    std::memcpy(expression, argument_start, argument_length);
                    expression[argument_length] = '\0';
                }
            }
            else
            {
                *cursor = argument_start;
                if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                        sizeof(expression)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        if (count > 0)
            ft_strlcat(buffer, ",", buffer_size);
        ft_strlcat(buffer, expression, buffer_size);
        count += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor == ',')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            continue ;
        }
        if (**cursor == ')')
        {
            *out_count = count;
            return (FT_SUCCESS);
        }
        return (FT_FAILURE);
    }
}

static t_cblc_data_item *cblc_ensure_helper_status(t_cblc_translation_unit *unit)
{
    t_cblc_data_item *item;

    if (!unit)
        return (NULL);
    if (unit->helper_status_index >= 0
        && static_cast<size_t>(unit->helper_status_index) < unit->data_count)
        return (&unit->data_items[unit->helper_status_index]);
    item = cblc_add_generated_int_item(unit, "cblc_helper_status");
    if (!item)
        return (NULL);
    unit->helper_status_index = static_cast<int>(item - unit->data_items);
    return (item);
}

static t_cblc_data_item *cblc_create_literal_buffer(t_cblc_translation_unit *unit,
    size_t literal_length)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit)
        return (NULL);
    unit->helper_literal_counter += 1;
    index = unit->helper_literal_counter;
    if (std::snprintf(identifier, sizeof(identifier), "cblc_literal_%zu", index) < 0)
        return (NULL);
    return (cblc_add_generated_char_item(unit, identifier,
        literal_length > 0 ? literal_length : 1));
}

static const t_cblc_function *cblc_find_function_in_unit(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->function_count)
    {
        if (std::strncmp(unit->functions[index].source_name, identifier,
                sizeof(unit->functions[index].source_name)) == 0)
            return (&unit->functions[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_append_statement(t_cblc_function *function, t_cblc_statement_type type,
    const char *target, const char *source, int is_literal)
{
    t_cblc_statement *statement;

    if (!function)
        return (FT_FAILURE);
    if (function->statement_count >= function->statement_capacity)
    {
        if (cblc_function_ensure_statement_capacity(function,
                function->statement_capacity == 0 ? 4 : function->statement_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count];
    statement->type = type;
    statement->is_literal = is_literal;
    statement->target[0] = '\0';
    statement->source[0] = '\0';
    statement->call_identifier[0] = '\0';
    statement->call_is_external = 0;
    statement->call_arguments[0] = '\0';
    statement->call_argument_count = 0;
    if (target)
        ft_strlcpy(statement->target, target, sizeof(statement->target));
    if (source)
        ft_strlcpy(statement->source, source, sizeof(statement->source));
    function->statement_count += 1;
    return (FT_SUCCESS);
}

static int cblc_append_existing_statement(t_cblc_function *function,
    const t_cblc_statement *statement)
{
    t_cblc_statement *appended;

    if (!function || !statement)
        return (FT_FAILURE);
    if (cblc_append_statement(function, statement->type, statement->target,
            statement->source, statement->is_literal) != FT_SUCCESS)
        return (FT_FAILURE);
    appended = &function->statements[function->statement_count - 1];
    ft_strlcpy(appended->call_identifier, statement->call_identifier,
        sizeof(appended->call_identifier));
    appended->call_is_external = statement->call_is_external;
    ft_strlcpy(appended->call_arguments, statement->call_arguments,
        sizeof(appended->call_arguments));
    appended->call_argument_count = statement->call_argument_count;
    return (FT_SUCCESS);
}

static int cblc_append_call_statement(t_cblc_function *function, const char *identifier,
    const char *arguments)
{
    t_cblc_statement *statement;

    if (!function || !identifier)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL, NULL, NULL, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (function->statement_count == 0)
        return (FT_FAILURE);
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier, sizeof(statement->call_identifier));
    statement->call_is_external = 1;
    if (arguments)
        ft_strlcpy(statement->call_arguments, arguments, sizeof(statement->call_arguments));
    return (FT_SUCCESS);
}

static int cblc_add_temp_alias_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, int is_const)
{
    t_cblc_data_item *item;

    if (!unit || !source_name || !cobol_name)
        return (FT_FAILURE);
    if (cblc_find_data_item(unit, source_name))
        return (FT_SUCCESS);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, source_name, sizeof(item->source_name));
    ft_strlcpy(item->cobol_name, cobol_name, sizeof(item->cobol_name));
    if (kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(item->declared_type_name, "string", sizeof(item->declared_type_name));
    else if (struct_type_name)
        ft_strlcpy(item->declared_type_name, struct_type_name, sizeof(item->declared_type_name));
    if (struct_type_name)
        ft_strlcpy(item->struct_type_name, struct_type_name, sizeof(item->struct_type_name));
    item->length = length;
    item->array_count = array_count;
    item->kind = kind;
    item->is_const = is_const;
    item->is_active = 1;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_add_named_data_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, const char *owner_function_name, int is_function_local,
    int is_alias, int is_const)
{
    t_cblc_data_item *item;

    if (!unit || !source_name || !cobol_name)
        return (FT_FAILURE);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, source_name, sizeof(item->source_name));
    ft_strlcpy(item->cobol_name, cobol_name, sizeof(item->cobol_name));
    if (kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(item->declared_type_name, "string", sizeof(item->declared_type_name));
    else if (struct_type_name)
        ft_strlcpy(item->declared_type_name, struct_type_name, sizeof(item->declared_type_name));
    if (struct_type_name)
        ft_strlcpy(item->struct_type_name, struct_type_name, sizeof(item->struct_type_name));
    if (owner_function_name)
        ft_strlcpy(item->owner_function_name, owner_function_name, sizeof(item->owner_function_name));
    item->length = length;
    item->array_count = array_count;
    item->kind = kind;
    item->is_const = is_const;
    item->is_function_local = is_function_local;
    item->is_alias = is_alias;
    item->is_active = 1;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static const t_cblc_struct_type *cblc_find_receiver_type(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    if (!unit || !item)
        return (NULL);
    if (item->declared_type_name[0] != '\0')
    {
        if (g_cblc_member_access_type
            && std::strncmp(item->declared_type_name, g_cblc_member_access_type->source_name,
                sizeof(item->declared_type_name)) == 0)
            return (g_cblc_member_access_type);
        return (cblc_find_struct_type(unit, item->declared_type_name));
    }
    if (item->kind == CBLC_DATA_KIND_STRUCT)
    {
        if (g_cblc_member_access_type
            && std::strncmp(item->struct_type_name, g_cblc_member_access_type->source_name,
                sizeof(item->struct_type_name)) == 0)
            return (g_cblc_member_access_type);
        return (cblc_find_struct_type(unit, item->struct_type_name));
    }
    return (NULL);
}

static int cblc_parse_method_receiver(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_receiver, char *method_name, size_t method_name_size)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !out_receiver || !method_name || method_name_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    if (!item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if ((*cursor)[0] == '-' && (*cursor)[1] == '>')
    {
        char member_identifier[TRANSPILE_IDENTIFIER_MAX];
        const t_cblc_struct_type *receiver_type;
        const t_cblc_method *method;
        const char *after_member;
        const char *probe;

        if (item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 2;
        if (cblc_parse_identifier(cursor, member_identifier, sizeof(member_identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        after_member = *cursor;
        receiver_type = cblc_find_receiver_type(unit, item);
        method = cblc_find_method_on_type(receiver_type, member_identifier);
        if (!receiver_type || !method
            || !cblc_member_visibility_is_allowed(receiver_type, method->visibility))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        probe = after_member;
        cblc_skip_whitespace(&probe);
        if (*probe != '(')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(method_name, member_identifier, method_name_size);
        *out_receiver = item;
        return (FT_SUCCESS);
    }
    while (**cursor == '.')
    {
        const char *member_start;
        char member_identifier[TRANSPILE_IDENTIFIER_MAX];
        const t_cblc_struct_type *receiver_type;
        const t_cblc_struct_field *field;
        const t_cblc_method *method;
        char field_source_name[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_item *next_item;
        const char *after_member;

        *cursor += 1;
        member_start = *cursor;
        if (cblc_parse_identifier(cursor, member_identifier, sizeof(member_identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        after_member = *cursor;
        receiver_type = cblc_find_receiver_type(unit, item);
        method = cblc_find_method_on_type(receiver_type, member_identifier);
        if (receiver_type && method
            && cblc_member_visibility_is_allowed(receiver_type, method->visibility))
        {
            const char *probe;

            probe = after_member;
            cblc_skip_whitespace(&probe);
            if (*probe == '(')
            {
                ft_strlcpy(method_name, member_identifier, method_name_size);
                *out_receiver = item;
                return (FT_SUCCESS);
            }
        }
        if (item->kind == CBLC_DATA_KIND_STRING
            && std::strncmp(member_identifier, "len", sizeof(member_identifier)) == 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (item->kind != CBLC_DATA_KIND_STRUCT)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        field = cblc_find_field_on_type(receiver_type, member_identifier);
        if (!field || !cblc_member_visibility_is_allowed(receiver_type, field->visibility))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_build_field_source_name(item->source_name, member_identifier, field_source_name,
            sizeof(field_source_name));
        next_item = cblc_find_data_item(unit, field_source_name);
        if (!next_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        item = next_item;
        (void)member_start;
    }
    *cursor = start;
    return (FT_FAILURE);
}
