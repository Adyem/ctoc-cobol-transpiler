static int cblc_bind_lifecycle_scope(t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t *saved_data_count)
{
    const char *root_source;
    const char *root_cobol;
    size_t alias_start;
    size_t alias_end;
    size_t index;

    if (!unit || !type || !saved_data_count)
        return (FT_FAILURE);
    *saved_data_count = unit->data_count;
    root_source = "this";
    root_cobol = "CBLC-THIS";
    if (cblc_add_temp_alias_item(unit, root_source, root_cobol, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_add_struct_instance_field_items(unit, type, root_source, root_cobol,
            NULL, 0, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    alias_start = *saved_data_count + 1;
    alias_end = unit->data_count;
    index = alias_start;
    while (index < alias_end)
    {
        char source_name[TRANSPILE_IDENTIFIER_MAX];
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char struct_type_name[TRANSPILE_IDENTIFIER_MAX];
        const char *declared_struct_type;
        t_cblc_data_kind kind;
        size_t length;
        size_t array_count;
        int is_const;

        ft_strlcpy(source_name, unit->data_items[index].source_name, sizeof(source_name));
        ft_strlcpy(cobol_name, unit->data_items[index].cobol_name, sizeof(cobol_name));
        ft_strlcpy(struct_type_name, unit->data_items[index].struct_type_name,
            sizeof(struct_type_name));
        kind = unit->data_items[index].kind;
        length = unit->data_items[index].length;
        array_count = unit->data_items[index].array_count;
        is_const = unit->data_items[index].is_const;
        if (std::strncmp(source_name, "this.", 5) == 0)
        {
            declared_struct_type = NULL;
            if (struct_type_name[0] != '\0')
                declared_struct_type = struct_type_name;
            if (cblc_add_temp_alias_item(unit, source_name + 5, cobol_name,
                    kind, length, array_count, declared_struct_type, is_const) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static void cblc_unbind_lifecycle_scope(t_cblc_translation_unit *unit, size_t saved_data_count)
{
    if (!unit)
        return ;
    if (saved_data_count <= unit->data_count)
        unit->data_count = saved_data_count;
}

static int cblc_parse_statement_block(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, int allow_return)
{
    size_t scope_destructor_base;
    size_t scope_data_base;
    int saw_return_in_block;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    scope_destructor_base = function->local_destructor_count;
    scope_data_base = unit->data_count;
    saw_return_in_block = 0;
    *cursor += 1;
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '}')
        {
            if (!saw_return_in_block)
            {
                while (function->local_destructor_count > scope_destructor_base)
                {
                    function->local_destructor_count -= 1;
                    if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                            function->local_destructor_targets[function->local_destructor_count],
                            NULL, 0) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else
                function->local_destructor_count = scope_destructor_base;
            while (scope_data_base < unit->data_count)
            {
                if (unit->data_items[scope_data_base].is_alias)
                    unit->data_items[scope_data_base].is_active = 0;
                scope_data_base += 1;
            }
            *cursor += 1;
            return (FT_SUCCESS);
        }
        if (**cursor == '{')
        {
            if (cblc_parse_statement_block(cursor, unit, function, allow_return) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(cursor, "return"))
        {
            (*cursor) -= std::strlen("return");
            if (!allow_return)
                return (FT_FAILURE);
            if (cblc_parse_return(cursor, unit, function) != FT_SUCCESS)
                return (FT_FAILURE);
            saw_return_in_block = 1;
            continue ;
        }
        if (cblc_match_keyword(cursor, "display"))
        {
            (*cursor) -= std::strlen("display");
            if (cblc_parse_display(cursor, unit, function) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_local_string_declaration(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_local_pointer_declaration(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_local_struct_pointer_declaration(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_local_struct_instance_declaration(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_std_strcpy(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_std_free(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_method_call(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_call(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_assignment(cursor, unit, function) == FT_SUCCESS)
            continue ;
        return (FT_FAILURE);
    }
    return (FT_FAILURE);
}

static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity)
{
    t_cblc_function lifecycle_function;
    size_t saved_data_count;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    std::memset(&lifecycle_function, 0, sizeof(lifecycle_function));
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
        return (FT_FAILURE);
    g_cblc_member_access_type = type;
    status = cblc_parse_statement_block(cursor, unit, &lifecycle_function, 0);
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    g_cblc_member_access_type = saved_access_type;
    if (status != FT_SUCCESS)
    {
        if (lifecycle_function.statements)
            cma_free(lifecycle_function.statements);
        return (FT_FAILURE);
    }
    *out_statements = lifecycle_function.statements;
    *out_count = lifecycle_function.statement_count;
    *out_capacity = lifecycle_function.statement_capacity;
    return (FT_SUCCESS);
}

static int cblc_find_field_index(const t_cblc_struct_type *type, const char *identifier,
    size_t *out_index)
{
    size_t index;

    if (!type || !identifier || !out_index)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (std::strncmp(type->fields[index].source_name, identifier,
                sizeof(type->fields[index].source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_find_current_constructor_field_index(const t_cblc_data_item *item,
    size_t *out_index)
{
    const t_cblc_struct_type *type;
    size_t index;
    char expected_cobol[TRANSPILE_IDENTIFIER_MAX];
    char expected_source[TRANSPILE_IDENTIFIER_MAX];

    if (!item || !out_index || !g_cblc_constructor_parse_state.active
        || !g_cblc_constructor_parse_state.type)
        return (FT_FAILURE);
    type = g_cblc_constructor_parse_state.type;
    index = 0;
    while (index < type->field_count)
    {
        if (std::snprintf(expected_cobol, sizeof(expected_cobol), "CBLC-THIS-%s",
                type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (std::strncmp(item->cobol_name, expected_cobol, sizeof(item->cobol_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        if (std::strncmp(item->source_name, type->fields[index].source_name,
                sizeof(item->source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        if (std::snprintf(expected_source, sizeof(expected_source), "this.%s",
                type->fields[index].source_name) < 0)
            return (FT_FAILURE);
        if (std::strncmp(item->source_name, expected_source, sizeof(item->source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_append_string_initializer_literal(t_cblc_function *function,
    const t_cblc_data_item *item, const char *literal_text)
{
    char buffer_target[TRANSPILE_IDENTIFIER_MAX];
    char length_target[TRANSPILE_IDENTIFIER_MAX];
    char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char length_literal[32];
    size_t assigned_length;

    if (!function || !item || !literal_text)
        return (FT_FAILURE);
    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_target,
        sizeof(buffer_target));
    cblc_build_string_component_name(item->cobol_name, "-LEN", length_target,
        sizeof(length_target));
    assigned_length = std::strlen(literal_text);
    if (assigned_length > item->length)
        assigned_length = item->length;
    cobol_source[0] = '"';
    cobol_source[1] = '\0';
    ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
    ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
    if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
            cobol_source, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
            length_literal, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_constructor_member_initializer(const char **cursor,
    t_cblc_translation_unit *unit, const t_cblc_struct_type *type, int *seen_fields,
    t_cblc_function *field_functions)
{
    char member_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;
    size_t field_index;

    if (!cursor || !*cursor || !unit || !type || !seen_fields || !field_functions)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, member_name, sizeof(member_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_find_field_index(type, member_name, &field_index) != FT_SUCCESS)
        return (FT_FAILURE);
    if (seen_fields[field_index])
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, member_name);
    if (!item)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
    {
        if (item->kind == CBLC_DATA_KIND_INT)
        {
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_COMPUTE,
                    item->cobol_name, "0", 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (item->kind == CBLC_DATA_KIND_STRING || item->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                    item->cobol_name, NULL, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
    }
    else if (item->kind == CBLC_DATA_KIND_INT)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_COMPUTE,
                item->cobol_name, expression, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (item->kind == CBLC_DATA_KIND_STRING)
    {
        if (**cursor == '"')
        {
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_string_initializer_literal(&field_functions[field_index], item,
                    literal_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[2];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            if (cblc_append_string_initializer_literal(&field_functions[field_index], item,
                    literal_text) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else if (item->kind == CBLC_DATA_KIND_CHAR)
    {
        char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];

        if (**cursor == '"')
        {
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = '"';
            literal_text[1] = literal_value;
            literal_text[2] = '"';
            literal_text[3] = '\0';
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, literal_text, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    seen_fields[field_index] = 1;
    return (FT_SUCCESS);
}

static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *constructor_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity)
{
    t_cblc_function *field_functions;
    int *seen_fields;
    size_t saved_data_count;
    size_t index;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !constructor_function
        || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    field_functions = static_cast<t_cblc_function *>(cma_calloc(type->field_count == 0 ? 1 : type->field_count,
            sizeof(*field_functions)));
    seen_fields = static_cast<int *>(cma_calloc(type->field_count == 0 ? 1 : type->field_count,
            sizeof(*seen_fields)));
    if (!field_functions || !seen_fields)
    {
        if (field_functions)
            cma_free(field_functions);
        if (seen_fields)
            cma_free(seen_fields);
        return (FT_FAILURE);
    }
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
    {
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    if (cblc_add_parameter_aliases(unit, constructor_function->parameters,
            constructor_function->parameter_count, type) != FT_SUCCESS)
    {
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    g_cblc_constructor_parse_state.type = type;
    g_cblc_constructor_parse_state.initialized_fields = seen_fields;
    g_cblc_constructor_parse_state.active = 1;
    g_cblc_member_access_type = type;
    cblc_skip_whitespace(cursor);
    if (**cursor == ':')
    {
        *cursor += 1;
        while (**cursor != '\0')
        {
            cblc_skip_whitespace(cursor);
            if (cblc_parse_constructor_member_initializer(cursor, unit, type, seen_fields,
                    field_functions) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            cblc_skip_whitespace(cursor);
            if (**cursor == ',')
            {
                *cursor += 1;
                continue ;
            }
            break ;
        }
    }
    index = 0;
    while (index < type->field_count)
    {
        t_cblc_data_item *field_item;
        size_t statement_index;

        field_item = cblc_find_data_item(unit, type->fields[index].source_name);
        if (!field_item)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        if (!seen_fields[index])
        {
            if (field_item->kind == CBLC_DATA_KIND_STRING || field_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_append_statement(constructor_function, CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                        field_item->cobol_name, NULL, 0) != FT_SUCCESS)
                {
                    status = FT_FAILURE;
                    goto cleanup;
                }
            }
        }
        statement_index = 0;
        while (statement_index < field_functions[index].statement_count)
        {
            if (cblc_append_existing_statement(constructor_function,
                    &field_functions[index].statements[statement_index]) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            statement_index += 1;
        }
        index += 1;
    }
    status = cblc_parse_statement_block(cursor, unit, constructor_function, 0);
    if (status != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].is_const && !seen_fields[index])
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    *out_statements = constructor_function->statements;
    *out_count = constructor_function->statement_count;
    *out_capacity = constructor_function->statement_capacity;
    constructor_function->statements = NULL;
    constructor_function->statement_count = 0;
    constructor_function->statement_capacity = 0;
    status = FT_SUCCESS;
cleanup:
    g_cblc_constructor_parse_state.type = NULL;
    g_cblc_constructor_parse_state.initialized_fields = NULL;
    g_cblc_constructor_parse_state.active = 0;
    g_cblc_member_access_type = saved_access_type;
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    index = 0;
    while (index < type->field_count)
    {
        if (field_functions[index].statements)
            cma_free(field_functions[index].statements);
        index += 1;
    }
    cma_free(field_functions);
    cma_free(seen_fields);
    return (status);
}

static int cblc_inject_entry_lifecycle(t_cblc_translation_unit *unit)
{
    t_cblc_function *function;
    t_cblc_statement *new_statements;
    char (*construct_targets)[TRANSPILE_IDENTIFIER_MAX];
    char (*construct_arguments)[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t *construct_argument_counts;
    char (*destruct_targets)[TRANSPILE_IDENTIFIER_MAX];
    size_t construct_count;
    size_t destruct_count;
    size_t return_count;
    size_t new_count;
    size_t index;
    size_t write_index;

    if (!unit || unit->function_count == 0)
        return (FT_SUCCESS);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (FT_SUCCESS);
    function = &unit->functions[unit->entry_function_index];
    index = unit->data_count;
    if (index == 0)
        index = 1;
    construct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*construct_targets)));
    construct_arguments = static_cast<char (*)[TRANSPILE_STATEMENT_TEXT_MAX]>(cma_calloc(index,
            sizeof(*construct_arguments)));
    construct_argument_counts = static_cast<size_t *>(cma_calloc(index,
            sizeof(*construct_argument_counts)));
    destruct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*destruct_targets)));
    if (!construct_targets || !construct_arguments || !construct_argument_counts || !destruct_targets)
    {
        if (construct_targets)
            cma_free(construct_targets);
        if (construct_arguments)
            cma_free(construct_arguments);
        if (construct_argument_counts)
            cma_free(construct_argument_counts);
        if (destruct_targets)
            cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    construct_count = 0;
    destruct_count = 0;
    index = 0;
    while (index < unit->data_count)
    {
        const t_cblc_data_item *item;
        const t_cblc_struct_type *type;

        item = &unit->data_items[index];
        if (item->is_alias || item->is_function_local || std::strchr(item->source_name, '.'))
        {
            index += 1;
            continue ;
        }
        type = cblc_find_declared_type_for_item(unit, item);
        if (item->kind == CBLC_DATA_KIND_STRUCT && !type)
            type = cblc_find_struct_type(unit, item->struct_type_name);
        if ((item->kind == CBLC_DATA_KIND_STRING || item->kind == CBLC_DATA_KIND_STRUCT) && !type)
        {
            cma_free(construct_targets);
            cma_free(construct_arguments);
            cma_free(construct_argument_counts);
            cma_free(destruct_targets);
            return (FT_FAILURE);
        }
        if (cblc_item_requires_default_construct(unit, item))
        {
            ft_strlcpy(construct_targets[construct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            ft_strlcpy(construct_arguments[construct_count], item->constructor_arguments,
                TRANSPILE_STATEMENT_TEXT_MAX);
            construct_argument_counts[construct_count] = item->constructor_argument_count;
            construct_count += 1;
        }
        if (cblc_item_requires_destruct(unit, item))
        {
            ft_strlcpy(destruct_targets[destruct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            destruct_count += 1;
        }
        index += 1;
    }
    return_count = 0;
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
            return_count += 1;
        index += 1;
    }
    new_count = function->statement_count + construct_count + (destruct_count * return_count);
    if (return_count == 0)
        new_count += destruct_count;
    if (new_count == 0)
    {
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        return (FT_SUCCESS);
    }
    new_statements = static_cast<t_cblc_statement *>(cma_calloc(new_count, sizeof(*new_statements)));
    if (!new_statements)
    {
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    write_index = 0;
    index = 0;
    while (index < construct_count)
    {
        new_statements[write_index].type = CBLC_STATEMENT_DEFAULT_CONSTRUCT;
        ft_strlcpy(new_statements[write_index].target, construct_targets[index],
            sizeof(new_statements[write_index].target));
        ft_strlcpy(new_statements[write_index].call_arguments, construct_arguments[index],
            sizeof(new_statements[write_index].call_arguments));
        new_statements[write_index].call_argument_count = construct_argument_counts[index];
        write_index += 1;
        index += 1;
    }
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
        {
            size_t destruct_index;

            destruct_index = destruct_count;
            while (destruct_index > 0)
            {
                destruct_index -= 1;
                new_statements[write_index].type = CBLC_STATEMENT_DESTRUCT;
                ft_strlcpy(new_statements[write_index].target, destruct_targets[destruct_index],
                    sizeof(new_statements[write_index].target));
                write_index += 1;
            }
        }
        new_statements[write_index] = function->statements[index];
        write_index += 1;
        index += 1;
    }
    if (return_count == 0)
    {
        index = destruct_count;
        while (index > 0)
        {
            index -= 1;
            new_statements[write_index].type = CBLC_STATEMENT_DESTRUCT;
            ft_strlcpy(new_statements[write_index].target, destruct_targets[index],
                sizeof(new_statements[write_index].target));
            write_index += 1;
        }
    }
    if (write_index != new_count)
    {
        cma_free(new_statements);
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    if (function->statements)
        cma_free(function->statements);
    function->statements = new_statements;
    function->statement_count = new_count;
    function->statement_capacity = new_count;
    cma_free(construct_targets);
    cma_free(construct_arguments);
    cma_free(construct_argument_counts);
    cma_free(destruct_targets);
    return (FT_SUCCESS);
}

static int cblc_append_local_destructor_statements(t_cblc_function *function)
{
    size_t index;

    if (!function)
        return (FT_FAILURE);
    index = function->local_destructor_count;
    while (index > 0)
    {
        index -= 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                function->local_destructor_targets[index], NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_add_local_struct_alias_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *alias_source_name, const char *actual_cobol_name)
{
    size_t index;

    if (!unit || !type || !alias_source_name || !actual_cobol_name)
        return (FT_FAILURE);
    if (cblc_add_named_data_item(unit, alias_source_name, actual_cobol_name, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, NULL, 0, 1, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char alias_field_source[TRANSPILE_IDENTIFIER_MAX];
        char actual_field_cobol[TRANSPILE_IDENTIFIER_MAX];

        cblc_build_field_source_name(alias_source_name, type->fields[index].source_name,
            alias_field_source, sizeof(alias_field_source));
        if (std::snprintf(actual_field_cobol, sizeof(actual_field_cobol), "%s-%s",
                actual_cobol_name, type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_add_local_struct_alias_items(unit, field_type, alias_field_source,
                    actual_field_cobol) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (cblc_add_named_data_item(unit, alias_field_source, actual_field_cobol,
                type->fields[index].kind, type->fields[index].length,
                type->fields[index].array_count,
                type->fields[index].struct_type_name[0] != '\0'
                    ? type->fields[index].struct_type_name : NULL,
                NULL, 0, 1, type->fields[index].is_const) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_local_string_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;
    size_t array_count;
    size_t constructor_argument_count;
    const char *constructor_start;
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "string"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    length = 1;
    array_count = 0;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (**cursor == '[')
    {
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (**cursor != '(')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (**cursor == '(')
    {
        constructor_start = *cursor;
        if (cblc_parse_string_capacity_clause(cursor, &length) == FT_SUCCESS)
        {
            if (std::snprintf(constructor_arguments, sizeof(constructor_arguments),
                    "%zu", length) < 0)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            constructor_argument_count = 1;
        }
        else
        {
            *cursor = constructor_start;
            if (cblc_parse_string_constructor_clause(cursor, unit, &length,
                    constructor_arguments, sizeof(constructor_arguments),
                    &constructor_argument_count) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
    }
    if (array_count > 0 || (array_count > 0 && constructor_argument_count > 0))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            function->source_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            function->cobol_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name, sizeof(actual_cobol_name));
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, CBLC_DATA_KIND_STRING,
            length, 0, NULL, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count - 1];
    ft_strlcpy(item->constructor_arguments, constructor_arguments,
        sizeof(item->constructor_arguments));
    item->constructor_argument_count = constructor_argument_count;
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name, CBLC_DATA_KIND_STRING,
            length, 0, NULL, NULL, 0, 1, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
            NULL, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_strlcpy(function->statements[function->statement_count - 1].call_arguments,
        constructor_arguments, sizeof(function->statements[function->statement_count - 1].call_arguments));
    function->statements[function->statement_count - 1].call_argument_count = constructor_argument_count;
    if (function->local_destructor_count >= function->local_destructor_capacity)
    {
        if (cblc_function_ensure_local_destructor_capacity(function,
                function->local_destructor_capacity == 0 ? 4
                : function->local_destructor_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(function->local_destructor_targets[function->local_destructor_count],
        actual_cobol_name, TRANSPILE_IDENTIFIER_MAX);
    function->local_destructor_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_local_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    t_cblc_data_kind kind;
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_pointer_type(cursor, &kind) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '*')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
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
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            function->source_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            function->cobol_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name,
        sizeof(actual_cobol_name));
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
            NULL, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name, kind, 0, 0,
            NULL, NULL, 0, 1, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_local_struct_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    const t_cblc_struct_type *type;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
    if (!type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '*')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
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
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            function->source_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            function->cobol_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name,
        sizeof(actual_cobol_name));
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name,
            CBLC_DATA_KIND_STRUCT_POINTER, 0, 0, type->source_name,
            function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name,
            CBLC_DATA_KIND_STRUCT_POINTER, 0, 0, type->source_name, NULL, 0, 1, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_local_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    const t_cblc_struct_type *type;
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t constructor_argument_count;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
    if (!type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '(')
    {
        *cursor += 1;
        if (cblc_parse_call_argument_list(cursor, unit, constructor_arguments,
                sizeof(constructor_arguments), &constructor_argument_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count > 0
        && !cblc_find_constructor_for_arguments(unit, type, constructor_arguments,
            constructor_argument_count))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count == 0
        && type->constructor_count > 0
        && !type->has_default_constructor)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            function->source_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            function->cobol_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name, sizeof(actual_cobol_name));
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_struct_instance_field_items(unit, type, actual_source_name, actual_cobol_name,
            function->source_name, 1, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_local_struct_alias_items(unit, type, local_identifier, actual_cobol_name)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_type_requires_default_construct(unit, type) || constructor_argument_count > 0)
    {
        if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
                NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_strlcpy(function->statements[function->statement_count - 1].call_arguments,
            constructor_arguments, sizeof(function->statements[function->statement_count - 1].call_arguments));
        function->statements[function->statement_count - 1].call_argument_count = constructor_argument_count;
    }
    if (cblc_type_requires_destruct(unit, type))
    {
        if (function->local_destructor_count >= function->local_destructor_capacity)
        {
            if (cblc_function_ensure_local_destructor_capacity(function,
                    function->local_destructor_capacity == 0 ? 4
                    : function->local_destructor_capacity * 2) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        ft_strlcpy(function->local_destructor_targets[function->local_destructor_count],
            actual_cobol_name, TRANSPILE_IDENTIFIER_MAX);
        function->local_destructor_count += 1;
    }
    return (FT_SUCCESS);
}
