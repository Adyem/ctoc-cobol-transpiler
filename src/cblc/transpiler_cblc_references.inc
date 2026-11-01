static const t_cblc_data_item *cblc_find_data_item_by_cobol(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;
    const t_cblc_data_item *alias_match;

    if (!unit || !identifier)
        return (NULL);
    alias_match = NULL;
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].cobol_name, identifier,
                sizeof(unit->data_items[index].cobol_name)) == 0)
        {
            if (!unit->data_items[index].is_active
                && (!unit->data_items[index].is_function_local
                    || unit->data_items[index].is_alias))
            {
                index += 1;
                continue ;
            }
            if (!unit->data_items[index].is_alias)
                return (&unit->data_items[index]);
            if (!alias_match)
                alias_match = &unit->data_items[index];
        }
        index += 1;
    }
    return (alias_match);
}

static const t_cblc_data_item *cblc_find_method_parameter_storage_by_cobol(
    const t_cblc_translation_unit *unit, const char *cobol_name)
{
    size_t data_index;

    if (!unit || !cobol_name || cobol_name[0] == '\0')
        return (NULL);
    for (data_index = 0; data_index < unit->data_count; ++data_index)
    {
        const t_cblc_data_item *item = &unit->data_items[data_index];
        size_t type_index;

        if (item->is_alias || item->kind != CBLC_DATA_KIND_STRUCT
            || std::strcmp(item->cobol_name, cobol_name) != 0)
            continue;
        for (type_index = 0; type_index < unit->struct_type_count; ++type_index)
        {
            const t_cblc_struct_type *type = &unit->struct_types[type_index];
            size_t method_index;

            for (method_index = 0; method_index < type->method_count;
                ++method_index)
            {
                const t_cblc_method *method = &type->methods[method_index];
                size_t parameter_index;

                if (!method->has_definition)
                    continue;
                for (parameter_index = 0;
                    parameter_index < method->parameter_count;
                    ++parameter_index)
                {
                    const t_cblc_parameter *parameter =
                        &method->parameters[parameter_index];
                    if (parameter->kind
                            == TRANSPILE_FUNCTION_PARAMETER_STRUCT
                        && std::strcmp(parameter->cobol_name, cobol_name) == 0
                        && std::strcmp(parameter->type_name,
                            item->struct_type_name) == 0
                        && (parameter->actual_source_name[0] == '\0'
                            || std::strcmp(parameter->actual_source_name,
                                item->source_name) == 0))
                        return (item);
                }
            }
        }
    }
    return (NULL);
}

static const t_cblc_data_item *cblc_find_data_item_for_emit_by_source(
    const t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;
    const t_cblc_data_item *inactive_alias;

    if (!unit || !identifier)
        return (NULL);
    inactive_alias = NULL;
    index = unit->data_count;
    while (index > 0)
    {
        index -= 1;
        if (std::strncmp(unit->data_items[index].source_name, identifier,
                sizeof(unit->data_items[index].source_name)) != 0)
            continue ;
        if (unit->data_items[index].is_active
            || (unit->data_items[index].is_function_local
                && !unit->data_items[index].is_alias))
            return (&unit->data_items[index]);
        if (!inactive_alias)
            inactive_alias = &unit->data_items[index];
    }
    if (inactive_alias)
    {
        index = unit->data_count;
        while (index > 0)
        {
            index -= 1;
            if ((unit->data_items[index].is_active
                    || (unit->data_items[index].is_function_local
                        && !unit->data_items[index].is_alias))
                && std::strncmp(unit->data_items[index].cobol_name,
                    inactive_alias->cobol_name,
                    sizeof(unit->data_items[index].cobol_name)) == 0)
                return (&unit->data_items[index]);
        }
    }
    return (NULL);
}

static const t_cblc_struct_type *cblc_find_struct_type(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (std::strncmp(unit->struct_types[index].source_name, identifier,
                sizeof(unit->struct_types[index].source_name)) == 0
            && !unit->struct_types[index].is_template)
            return (&unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_struct_type *cblc_find_template_struct(
    const t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (unit->struct_types[index].is_template
            && std::strncmp(unit->struct_types[index].source_name, identifier,
                sizeof(unit->struct_types[index].source_name)) == 0)
            return (&unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_method *cblc_find_method_on_type(const t_cblc_struct_type *type,
    const char *identifier)
{
    size_t index;

    if (!type || !identifier)
        return (NULL);
    index = 0;
    while (index < type->method_count)
    {
        if (std::strncmp(type->methods[index].source_name, identifier,
                sizeof(type->methods[index].source_name)) == 0)
            return (&type->methods[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_method *cblc_find_unique_method_on_type_for_arity(
    const t_cblc_struct_type *type, const char *identifier,
    size_t argument_count)
{
    const t_cblc_method *match;
    size_t index;

    if (!type || !identifier)
        return (NULL);
    match = NULL;
    index = 0;
    while (index < type->method_count)
    {
        const t_cblc_method *candidate = &type->methods[index];

        if (std::strcmp(candidate->source_name, identifier) == 0
            && candidate->parameter_count == argument_count)
        {
            if (match)
                return (NULL);
            match = candidate;
        }
        index += 1;
    }
    return (match);
}

static size_t cblc_method_overload_count(const t_cblc_struct_type *type,
    const char *identifier)
{
    size_t index;
    size_t count;

    if (!type || !identifier)
        return (0);
    index = 0;
    count = 0;
    while (index < type->method_count)
    {
        if (std::strcmp(type->methods[index].source_name, identifier) == 0)
            count += 1;
        index += 1;
    }
    return (count);
}

static const t_cblc_struct_field *cblc_find_field_on_type(const t_cblc_struct_type *type,
    const char *identifier)
{
    size_t index;

    if (!type || !identifier)
        return (NULL);
    index = 0;
    while (index < type->field_count)
    {
        if (std::strncmp(type->fields[index].source_name, identifier,
                sizeof(type->fields[index].source_name)) == 0)
            return (&type->fields[index]);
        index += 1;
    }
    return (NULL);
}

static t_cblc_data_item *cblc_resolve_implicit_member_item(
    t_cblc_translation_unit *unit, const char *identifier,
    t_cblc_data_item *lexical_item)
{
    const t_cblc_struct_field *member_field;
    char member_source_name[TRANSPILE_IDENTIFIER_MAX + 8];
    t_cblc_data_item *member_item;
    int current_function_local;

    if (!unit || !identifier || !g_cblc_member_access_type)
        return (lexical_item);
    member_field = cblc_find_field_on_type(g_cblc_member_access_type, identifier);
    if (!member_field
        || std::snprintf(member_source_name, sizeof(member_source_name),
            "this.%s", identifier) < 0)
        return (lexical_item);
    member_item = cblc_find_data_item(unit, member_source_name);
    if (!member_item)
        return (lexical_item);
    current_function_local = lexical_item && lexical_item->is_function_local
        && unit->active_scope_id < unit->scope_count
        && std::strcmp(lexical_item->owner_function_name,
            unit->scopes[unit->active_scope_id].owner_function_name) == 0;
    return (current_function_local ? lexical_item : member_item);
}

static int cblc_member_visibility_is_allowed(const t_cblc_struct_type *owner_type,
    t_cblc_member_visibility visibility)
{
    if (visibility == CBLC_MEMBER_VISIBILITY_PUBLIC)
        return (1);
    if (!owner_type || !g_cblc_member_access_type)
        return (0);
    if (std::strncmp(owner_type->source_name, g_cblc_member_access_type->source_name,
            sizeof(owner_type->source_name)) == 0)
        return (1);
    return (0);
}

static int cblc_parse_member_visibility_label(const char **cursor,
    t_cblc_member_visibility *current_visibility)
{
    const char *start;
    t_cblc_member_visibility visibility;

    if (!cursor || !*cursor || !current_visibility)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_match_keyword(cursor, "private"))
        visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
    else if (cblc_match_keyword(cursor, "public"))
        visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ':')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    *current_visibility = visibility;
    return (FT_SUCCESS);
}

static const t_cblc_struct_type *cblc_find_declared_type_for_item(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *item)
{
    if (!unit || !item || item->declared_type_name[0] == '\0')
        return (NULL);
    if (g_cblc_member_access_type
        && std::strncmp(item->declared_type_name, g_cblc_member_access_type->source_name,
            sizeof(item->declared_type_name)) == 0)
        return (g_cblc_member_access_type);
    return (cblc_find_struct_type(unit, item->declared_type_name));
}

static int cblc_type_requires_default_construct(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!unit || !type)
        return (0);
    /* An implicit default constructor establishes availability, but needs no
     * emitted constructor call unless one of its members does real work.
     * Explicit constructors always have a descriptor and must be dispatched. */
    if (type->has_default_constructor && type->constructor_count > 0)
        return (1);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_CHAR_BUFFER
            && !type->fields[index].is_const)
            return (1);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (field_type && cblc_type_requires_default_construct(unit, field_type))
                return (1);
        }
        index += 1;
    }
    return (0);
}

static int cblc_type_has_usable_default_constructor_impl(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t depth)
{
    size_t index;

    if (!unit || !type || depth > unit->struct_type_count)
        return (0);
    if (type->constructor_count > 0)
        return (type->has_default_constructor != 0);
    if (type->has_base_type)
    {
        const t_cblc_struct_type *base_type = cblc_find_struct_type(unit,
            type->base_type_name);
        if (!base_type
            || !cblc_type_has_usable_default_constructor_impl(unit,
                base_type, depth + 1))
            return (0);
    }
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type = cblc_find_struct_type(unit,
                type->fields[index].struct_type_name);
            if (!field_type
                || !cblc_type_has_usable_default_constructor_impl(unit,
                    field_type, depth + 1))
                return (0);
        }
        else if (type->fields[index].is_const)
            return (0);
        ++index;
    }
    return (1);
}

static int cblc_type_has_usable_default_constructor(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type)
{
    return (cblc_type_has_usable_default_constructor_impl(unit, type, 0));
}

static int cblc_type_requires_destruct(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!unit || !type)
        return (0);
    if (type->has_destructor)
        return (1);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_CHAR_BUFFER
            && !type->fields[index].is_const)
            return (1);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (field_type && cblc_type_requires_destruct(unit, field_type))
                return (1);
        }
        index += 1;
    }
    return (0);
}

static int cblc_item_requires_default_construct(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_struct_type *type;

    if (!unit || !item)
        return (0);
    if (item->constructor_argument_count > 0)
        return (1);
    if (item->kind == CBLC_DATA_KIND_STRUCT)
    {
        type = cblc_find_struct_type(unit, item->struct_type_name);
        return (type && cblc_type_requires_default_construct(unit, type));
    }
    type = cblc_find_declared_type_for_item(unit, item);
    return (type && cblc_type_requires_default_construct(unit, type));
}

static int cblc_item_requires_destruct(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_struct_type *type;

    if (!unit || !item)
        return (0);
    if (item->kind == CBLC_DATA_KIND_STRUCT)
    {
        type = cblc_find_struct_type(unit, item->struct_type_name);
        return (type && cblc_type_requires_destruct(unit, type));
    }
    type = cblc_find_declared_type_for_item(unit, item);
    return (type && cblc_type_requires_destruct(unit, type));
}

static void cblc_compact_source_name(const char *full_name, char *buffer,
    size_t buffer_size)
{
    size_t maximum_name_size;
    int full_name_length;

    if (!full_name || !buffer || buffer_size == 0)
        return ;
    full_name_length = static_cast<int>(std::strlen(full_name));
    if (buffer_size < TRANSPILE_IDENTIFIER_MAX)
    {
        buffer[0] = '\0';
        return ;
    }
    maximum_name_size = TRANSPILE_IDENTIFIER_MAX;
    if (static_cast<size_t>(full_name_length) < maximum_name_size)
    {
        ft_strlcpy(buffer, full_name, maximum_name_size);
        return ;
    }
    /* Deeply nested template instance names can exhaust the fixed source-name
     * slot before their member path is appended. Preserve a stable identity
     * for that complete path instead of silently truncating it to its parent
     * (which aliases distinct members and can emit duplicate COBOL storage). */
    {
        const size_t hash_suffix_length = sizeof("_H0000000000000000") - 1;
        size_t prefix_length;
        unsigned long long hash;
        size_t index;

        if (maximum_name_size <= hash_suffix_length + 1)
        {
            buffer[0] = '\0';
            return ;
        }
        prefix_length = maximum_name_size - hash_suffix_length - 1;
        if (prefix_length > static_cast<size_t>(full_name_length))
            prefix_length = static_cast<size_t>(full_name_length);
        hash = 1469598103934665603ULL;
        index = 0;
        while (index < static_cast<size_t>(full_name_length))
        {
            hash ^= static_cast<unsigned char>(full_name[index]);
            hash *= 1099511628211ULL;
            index += 1;
        }
        std::snprintf(buffer, maximum_name_size, "%.*s_H%016llX",
            static_cast<int>(prefix_length), full_name, hash);
    }
}

static void cblc_build_field_source_name(const char *base, const char *field, char *buffer,
    size_t buffer_size)
{
    char full_name[TRANSPILE_STATEMENT_TEXT_MAX];
    int written;

    if (!buffer || buffer_size == 0)
        return ;
    if (!base || !field)
    {
        buffer[0] = '\0';
        return ;
    }
    written = std::snprintf(full_name, sizeof(full_name), "%s.%s", base, field);
    if (written < 0 || static_cast<size_t>(written) >= sizeof(full_name))
    {
        buffer[0] = '\0';
        return ;
    }
    cblc_compact_source_name(full_name, buffer, buffer_size);
}

static int cblc_add_struct_pointer_nested_alias_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *alias_source_name, const char *alias_cobol_name)
{
    size_t index;

    if (!unit || !type || !alias_source_name || !alias_cobol_name)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char field_source_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char field_cobol_name[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *field_struct_type_name;

        cblc_build_field_source_name(alias_source_name, type->fields[index].source_name,
            field_source_name, sizeof(field_source_name));
        if (field_source_name[0] == '\0')
            return (FT_FAILURE);
        if (std::snprintf(field_cobol_name, sizeof(field_cobol_name), "%s.%s",
                alias_cobol_name, type->fields[index].source_name) < 0)
            return (FT_FAILURE);
        field_struct_type_name = NULL;
        if (type->fields[index].struct_type_name[0] != '\0')
            field_struct_type_name = type->fields[index].struct_type_name;
        if (cblc_add_temp_alias_item(unit, field_source_name, field_cobol_name,
                type->fields[index].kind, type->fields[index].length,
                type->fields[index].array_count, field_struct_type_name,
                type->fields[index].is_const) != FT_SUCCESS)
            return (FT_FAILURE);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_add_struct_pointer_nested_alias_items(unit, field_type,
                    field_source_name, field_cobol_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_array_literal_index(const char **cursor, size_t *out_index)
{
    size_t value;

    if (!cursor || !*cursor || !out_index)
        return (FT_FAILURE);
    if (**cursor != '[')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor < '0' || **cursor > '9')
        return (FT_FAILURE);
    value = 0;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ']')
        return (FT_FAILURE);
    *cursor += 1;
    *out_index = value;
    return (FT_SUCCESS);
}

static int cblc_parse_array_variable_index(const char **cursor, t_cblc_translation_unit *unit,
    char *out_expression, size_t out_expression_size)
{
    const char *start;
    const char *scan;
    size_t length;

    if (!cursor || !*cursor || !unit || !out_expression || out_expression_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    scan = start;
    length = 0;
    while (*scan != '\0' && *scan != ']')
    {
        if (length + 1 >= out_expression_size)
            return (FT_FAILURE);
        out_expression[length++] = *scan++;
    }
    if (*scan != ']' || length == 0)
        return (FT_FAILURE);
    out_expression[length] = '\0';
    {
        size_t index;
        int saw_operand;

        index = 0;
        saw_operand = 0;
        while (index < length)
        {
            if (out_expression[index] == ' ' || out_expression[index] == '\t'
                || out_expression[index] == '+' || out_expression[index] == '-'
                || out_expression[index] == '*' || out_expression[index] == '/'
                || out_expression[index] == '(' || out_expression[index] == ')')
            {
                index += 1;
                continue ;
            }
            if ((out_expression[index] >= '0' && out_expression[index] <= '9'))
            {
                saw_operand = 1;
                while (index < length && out_expression[index] >= '0'
                    && out_expression[index] <= '9')
                    index += 1;
                continue ;
            }
            if ((out_expression[index] >= 'A' && out_expression[index] <= 'Z')
                || (out_expression[index] >= 'a' && out_expression[index] <= 'z')
                || out_expression[index] == '_')
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];
                size_t identifier_length;
                t_cblc_data_item *item;

                identifier_length = 0;
                while (index < length
                    && ((out_expression[index] >= 'A' && out_expression[index] <= 'Z')
                        || (out_expression[index] >= 'a' && out_expression[index] <= 'z')
                        || (out_expression[index] >= '0' && out_expression[index] <= '9')
                        || out_expression[index] == '_'))
                {
                    if (identifier_length + 1 >= sizeof(identifier))
                        return (FT_FAILURE);
                    identifier[identifier_length++] = out_expression[index++];
                }
                identifier[identifier_length] = '\0';
                item = cblc_find_data_item(unit, identifier);
                item = cblc_resolve_implicit_member_item(unit, identifier, item);
                if (!item || item->kind != CBLC_DATA_KIND_INT || item->array_count > 0)
                    return (FT_FAILURE);
                saw_operand = 1;
                continue ;
            }
            return (FT_FAILURE);
        }
        if (!saw_operand)
            return (FT_FAILURE);
    }
    {
        char translated_expression[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t source_index;

        translated_expression[0] = '\0';
        source_index = 0;
        while (source_index < length)
        {
            if ((out_expression[source_index] >= 'A'
                    && out_expression[source_index] <= 'Z')
                || (out_expression[source_index] >= 'a'
                    && out_expression[source_index] <= 'z')
                || out_expression[source_index] == '_')
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];
                t_cblc_data_item *item;
                size_t identifier_length;

                identifier_length = 0;
                while (source_index < length
                    && ((out_expression[source_index] >= 'A'
                            && out_expression[source_index] <= 'Z')
                        || (out_expression[source_index] >= 'a'
                            && out_expression[source_index] <= 'z')
                        || (out_expression[source_index] >= '0'
                            && out_expression[source_index] <= '9')
                        || out_expression[source_index] == '_'))
                {
                    if (identifier_length + 1 >= sizeof(identifier))
                        return (FT_FAILURE);
                    identifier[identifier_length++] = out_expression[source_index++];
                }
                identifier[identifier_length] = '\0';
                item = cblc_find_data_item(unit, identifier);
                item = cblc_resolve_implicit_member_item(unit, identifier, item);
                if (!item || ft_strlcat(translated_expression, item->cobol_name,
                        sizeof(translated_expression)) >= sizeof(translated_expression))
                    return (FT_FAILURE);
            }
            else
            {
                char character[2];

                character[0] = out_expression[source_index++];
                character[1] = '\0';
                if (ft_strlcat(translated_expression, character,
                        sizeof(translated_expression)) >= sizeof(translated_expression))
                    return (FT_FAILURE);
            }
        }
        ft_strlcpy(out_expression, translated_expression, out_expression_size);
    }
    *cursor = scan;
    return (FT_SUCCESS);
}

static int cblc_format_occurs_reference(const char *base_name, size_t zero_based_index,
    char *buffer, size_t buffer_size)
{
    if (!base_name || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%s(%zu)", base_name, zero_based_index + 1) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_format_expression_indexed_reference(const char *base_name, const char *expression,
    char *buffer, size_t buffer_size)
{
    if (!base_name || !expression || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%s[%s]", base_name, expression) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_format_string_component_reference(const char *base_name, size_t has_index,
    size_t zero_based_index, const char *component_suffix, char *buffer, size_t buffer_size)
{
    if (!base_name || !component_suffix || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (has_index)
    {
        if (std::snprintf(buffer, buffer_size, "%s%s(%zu)", base_name, component_suffix,
                zero_based_index + 1) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(buffer, buffer_size, "%s%s", base_name, component_suffix) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_format_string_component_expression_reference(const char *base_name,
    const char *expression, const char *component_suffix, char *buffer, size_t buffer_size)
{
    if (!base_name || !expression || !component_suffix || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%s%s[%s]", base_name, component_suffix,
            expression) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_occurs_reference_index(const char *reference, size_t *out_zero_based_index)
{
    const char *open_paren;
    size_t value;

    if (!reference || !out_zero_based_index)
        return (FT_FAILURE);
    open_paren = std::strchr(reference, '(');
    if (!open_paren)
        return (FT_FAILURE);
    open_paren += 1;
    if (*open_paren < '0' || *open_paren > '9')
        return (FT_FAILURE);
    value = 0;
    while (*open_paren >= '0' && *open_paren <= '9')
    {
        value = value * 10 + static_cast<size_t>(*open_paren - '0');
        open_paren += 1;
    }
    if (*open_paren != ')' || value == 0)
        return (FT_FAILURE);
    *out_zero_based_index = value - 1;
    return (FT_SUCCESS);
}

static int cblc_parse_cobol_reference_base(const char *reference, char *base_name,
    size_t base_name_size)
{
    const char *open_paren;
    const char *open_bracket;
    size_t length;

    if (!reference || !base_name || base_name_size == 0)
        return (FT_FAILURE);
    open_paren = std::strchr(reference, '(');
    open_bracket = std::strchr(reference, '[');
    if (open_paren && (!open_bracket || open_paren < open_bracket))
        length = static_cast<size_t>(open_paren - reference);
    else if (open_bracket)
        length = static_cast<size_t>(open_bracket - reference);
    else
        length = std::strlen(reference);
    if (length + 1 > base_name_size)
        return (FT_FAILURE);
    if (length > 0)
        std::memcpy(base_name, reference, length);
    base_name[length] = '\0';
    return (FT_SUCCESS);
}

static thread_local int g_cblc_native_standard_library_expression_mode = 0;

void cblc_set_native_standard_library_expression_mode(int enabled)
{
    g_cblc_native_standard_library_expression_mode = enabled ? 1 : 0;
}

int cblc_native_standard_library_expression_mode_enabled(void)
{
    return (g_cblc_native_standard_library_expression_mode);
}

static int cblc_translate_expression_for_cobol(const char *expression, char *buffer,
    size_t buffer_size)
{
    size_t index;

    if (!expression || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    index = 0;
    while (expression[index] != '\0')
    {
        char character;

        character = expression[index];
        if (expression[index] != '\0' && expression[index + 1] != '\0'
            && expression[index + 2] != '\0'
            && std::tolower(static_cast<unsigned char>(expression[index])) == 's'
            && std::tolower(static_cast<unsigned char>(expression[index + 1])) == 't'
            && std::tolower(static_cast<unsigned char>(expression[index + 2])) == 'd')
        {
            size_t intrinsic_cursor;
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char mapped_function[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t argument_start;
            size_t argument_end;
            size_t scan;
            size_t depth;
            char quote;

            intrinsic_cursor = index + 3;
            while (std::isspace(static_cast<unsigned char>(expression[intrinsic_cursor])))
                intrinsic_cursor += 1;
            if (expression[intrinsic_cursor] != ':')
                goto append_expression_character;
            intrinsic_cursor += 1;
            while (std::isspace(static_cast<unsigned char>(expression[intrinsic_cursor])))
                intrinsic_cursor += 1;
            if (expression[intrinsic_cursor] != ':')
                goto append_expression_character;
            intrinsic_cursor += 1;
            while (std::isspace(static_cast<unsigned char>(expression[intrinsic_cursor])))
                intrinsic_cursor += 1;
            if (expression[intrinsic_cursor] == '\0'
                || expression[intrinsic_cursor + 1] == '\0'
                || expression[intrinsic_cursor + 2] == '\0'
                || std::tolower(static_cast<unsigned char>(expression[intrinsic_cursor])) != 'o'
                || std::tolower(static_cast<unsigned char>(expression[intrinsic_cursor + 1])) != 'r'
                || std::tolower(static_cast<unsigned char>(expression[intrinsic_cursor + 2])) != 'd')
                goto append_expression_character;
            intrinsic_cursor += 3;
            while (std::isspace(static_cast<unsigned char>(expression[intrinsic_cursor])))
                intrinsic_cursor += 1;
            if (expression[intrinsic_cursor] != '(')
                goto append_expression_character;
            argument_start = intrinsic_cursor + 1;
            scan = argument_start;
            depth = 1;
            quote = '\0';
            while (expression[scan] != '\0' && depth > 0)
            {
                if (quote != '\0')
                {
                    if (expression[scan] == quote
                        && (scan == 0 || expression[scan - 1] != '\\'))
                        quote = '\0';
                }
                else if (expression[scan] == '\'' || expression[scan] == '"')
                    quote = expression[scan];
                else if (expression[scan] == '(')
                    depth += 1;
                else if (expression[scan] == ')')
                    depth -= 1;
                if (depth > 0)
                    scan += 1;
            }
            if (depth != 0 || scan == argument_start
                || scan - argument_start >= sizeof(argument))
                return (FT_FAILURE);
            argument_end = scan;
            std::memcpy(argument, expression + argument_start,
                argument_end - argument_start);
            argument[argument_end - argument_start] = '\0';
            if (cblc_translate_expression_for_cobol(argument,
                    translated_argument, sizeof(translated_argument)) != FT_SUCCESS
                || std::snprintf(mapped_function, sizeof(mapped_function),
                    "FUNCTION ORD(%s)", translated_argument) < 0
                || cblc_expression_append(buffer, buffer_size, mapped_function)
                    != FT_SUCCESS)
                return (FT_FAILURE);
            index = scan + 1;
            continue ;
        }
        if (std::isspace(static_cast<unsigned char>(character)))
        {
            index += 1;
            continue ;
        }
        if (character == '\'' || character == '"')
        {
            char literal[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t literal_length;
            char quote;

            quote = character;
            literal_length = 0;
            while (expression[index] != '\0')
            {
                if (literal_length + 1 >= sizeof(literal))
                    return (FT_FAILURE);
                literal[literal_length++] = expression[index];
                if (expression[index] == quote && literal_length > 1
                    && expression[index - 1] != '\\')
                {
                    index += 1;
                    break ;
                }
                index += 1;
            }
            if (literal_length == 0 || literal[literal_length - 1] != quote)
                return (FT_FAILURE);
            literal[literal_length] = '\0';
            if (std::strcmp(literal, "'\\0'") == 0
                || std::strcmp(literal, "\"\\0\"") == 0)
                ft_strlcpy(literal, "LOW-VALUE", sizeof(literal));
            if (cblc_expression_append(buffer, buffer_size, literal) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (character == '(' || character == ')' || character == ':')
        {
            char parenthesis[2];

            parenthesis[0] = character;
            parenthesis[1] = '\0';
            if (cblc_expression_append(buffer, buffer_size, parenthesis) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (character == '<' || character == '>' || character == '='
            || character == '!')
        {
            char operator_buffer[8];
            size_t operator_length;

            operator_length = 1;
            operator_buffer[0] = character;
            if ((expression[index + 1] == '=' && (character == '<'
                    || character == '>' || character == '=' || character == '!')))
            {
                operator_buffer[operator_length++] = '=';
                index += 1;
            }
            operator_buffer[operator_length] = '\0';
            if (std::strcmp(operator_buffer, "==") == 0)
                operator_buffer[0] = '=', operator_buffer[1] = '\0';
            else if (std::strcmp(operator_buffer, "!=") == 0)
                ft_strlcpy(operator_buffer, "NOT =", sizeof(operator_buffer));
            else if (std::strcmp(operator_buffer, "!") == 0)
                ft_strlcpy(operator_buffer, "NOT", sizeof(operator_buffer));
            if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (character == '&' && expression[index + 1] == '&')
        {
            if (cblc_expression_append(buffer, buffer_size, "AND") != FT_SUCCESS)
                return (FT_FAILURE);
            index += 2;
            continue ;
        }
        if (character == '|' && expression[index + 1] == '|')
        {
            if (cblc_expression_append(buffer, buffer_size, "OR") != FT_SUCCESS)
                return (FT_FAILURE);
            index += 2;
            continue ;
        }
        if (character == '*' && expression[index + 1] == '*')
        {
            if (cblc_expression_append(buffer, buffer_size, "**") != FT_SUCCESS)
                return (FT_FAILURE);
            index += 2;
            continue ;
        }
        if (character == '+' || character == '-' || character == '*' || character == '/')
        {
            char operator_buffer[2];

            operator_buffer[0] = character;
            operator_buffer[1] = '\0';
            if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
append_expression_character:
        if ((character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')
            || character == '_')
        {
            char token[TRANSPILE_STATEMENT_TEXT_MAX];
            char mapped[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t token_index;
            size_t length;

            token_index = index;
            length = 0;
            while ((expression[token_index] >= 'A' && expression[token_index] <= 'Z')
                || (expression[token_index] >= 'a' && expression[token_index] <= 'z')
                || (expression[token_index] >= '0' && expression[token_index] <= '9')
                || expression[token_index] == '-' || expression[token_index] == '_')
            {
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[token_index];
                length += 1;
                token_index += 1;
            }
            token[length] = '\0';
            if (expression[token_index] == '[')
            {
                const char *expr_start;
                size_t expr_length;
                char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];

                expr_start = &expression[token_index + 1];
                while (expression[token_index] != '\0' && expression[token_index] != ']')
                    token_index += 1;
                if (expression[token_index] != ']')
                    return (FT_FAILURE);
                expr_length = static_cast<size_t>(&expression[token_index] - expr_start);
                if (expr_length + 1 >= sizeof(index_expression))
                    return (FT_FAILURE);
                std::memcpy(index_expression, expr_start, expr_length);
                index_expression[expr_length] = '\0';
                if (cblc_translate_expression_for_cobol(index_expression, translated_index,
                        sizeof(translated_index)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (g_cblc_native_standard_library_expression_mode
                    && (std::strcmp(token, "first") == 0
                        || std::strcmp(token, "second") == 0
                        || std::strcmp(token, "F-FIRST") == 0
                        || std::strcmp(token, "F-SECOND") == 0
                        || std::strcmp(token, "F-SOURCE") == 0
                        || std::strcmp(token, "F-TARGET") == 0
                        || std::strcmp(token, "F-DESTINATION") == 0
                        || std::strcmp(token, "F-LEFT") == 0
                        || std::strcmp(token, "F-RIGHT") == 0
                        || std::strcmp(token, "F-X") == 0
                        || std::strcmp(token, "source") == 0
                        || std::strcmp(token, "target") == 0
                        || std::strcmp(token, "destination") == 0
                        || std::strcmp(token, "left") == 0
                        || std::strcmp(token, "right") == 0
                        || std::strcmp(token, "x") == 0
                        || std::strcmp(token, "source_string") == 0
                        || std::strcmp(token, "target_string") == 0
                        || std::strcmp(token, "destination_string") == 0
                        || std::strcmp(token, "left_string") == 0
                        || std::strcmp(token, "right_string") == 0))
                {
                    if (std::strcmp(token, "F-FIRST") == 0
                        || std::strcmp(token, "F-SECOND") == 0)
                    {
                        if (std::snprintf(mapped, sizeof(mapped), "%s-BUF(%s + 1:1)",
                            token, translated_index) < 0)
                            return (FT_FAILURE);
                    }
                    else if (std::strcmp(token, "F-SOURCE") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-SOURCE-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "F-TARGET") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-TARGET-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "F-DESTINATION") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-DESTINATION-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "F-LEFT") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-LEFT-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "F-RIGHT") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-RIGHT-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "F-X") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-X-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "source_string") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-SOURCE-STRING-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "source") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-SOURCE-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "target") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-TARGET-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "destination") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-DESTINATION-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "left") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-LEFT-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "right") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-RIGHT-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "x") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-X-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "target_string") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-TARGET-STRING-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "destination_string") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-DESTINATION-STRING-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "left_string") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-LEFT-STRING-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                    else if (std::strcmp(token, "right_string") == 0
                        && std::snprintf(mapped, sizeof(mapped), "F-RIGHT-STRING-BUF(%s + 1:1)",
                            translated_index) < 0)
                        return (FT_FAILURE);
                }
                else
                if (std::snprintf(mapped, sizeof(mapped),
                        g_cblc_native_standard_library_expression_mode
                            ? "%s(%s + 1:1)" : "%s(%s + 1)", token,
                        translated_index) < 0)
                    return (FT_FAILURE);
                token_index += 1;
            }
            else if (expression[token_index] == '(')
            {
                size_t paren_depth;

                paren_depth = 0;
                while (expression[token_index] != '\0')
                {
                    if (length + 1 >= sizeof(token))
                        return (FT_FAILURE);
                    token[length] = expression[token_index];
                    length += 1;
                    if (expression[token_index] == '(')
                        paren_depth += 1;
                    else if (expression[token_index] == ')')
                    {
                        paren_depth -= 1;
                        if (paren_depth == 0)
                        {
                            token_index += 1;
                            break ;
                        }
                    }
                    token_index += 1;
                }
                if (paren_depth != 0)
                    return (FT_FAILURE);
                token[length] = '\0';
                ft_strlcpy(mapped, token, sizeof(mapped));
            }
            else
            {
                size_t mapped_index;

                mapped_index = 0;
                while (token[mapped_index] != '\0'
                    && mapped_index + 1 < sizeof(mapped))
                {
                    mapped[mapped_index] = token[mapped_index] == '_'
                        ? '-' : token[mapped_index];
                    mapped_index += 1;
                }
                mapped[mapped_index] = '\0';
            }
            if (cblc_expression_append(buffer, buffer_size, mapped) != FT_SUCCESS)
                return (FT_FAILURE);
            index = token_index;
            continue ;
        }
        if (character >= '0' && character <= '9')
        {
            char number_token[32];
            size_t length;

            length = 0;
            while (expression[index] >= '0' && expression[index] <= '9')
            {
                if (length + 1 >= sizeof(number_token))
                    return (FT_FAILURE);
                number_token[length] = expression[index];
                length += 1;
                index += 1;
            }
            number_token[length] = '\0';
            if (cblc_expression_append(buffer, buffer_size, number_token) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_data_reference_text(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference, char *out_reference,
    size_t out_reference_size)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char reference_text[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *item;
    size_t has_index;
    size_t array_index;
    int has_expression_index;
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!cursor || !*cursor || !unit || !out_item)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    item = cblc_resolve_implicit_member_item(unit, identifier, item);
    if (!item)
    {
        cblc_record_scope_violation(unit, identifier);
        return (FT_FAILURE);
    }
    ft_strlcpy(reference_text, item->cobol_name, sizeof(reference_text));
    has_index = 0;
    array_index = 0;
    has_expression_index = 0;
    index_expression[0] = '\0';
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '[')
        {
            t_cblc_data_kind pointer_element_kind;

            if (has_index)
                return (FT_FAILURE);
            if (item->array_count == 0 && item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                && item->kind != CBLC_DATA_KIND_CHAR_BUFFER
                && !(item->kind == CBLC_DATA_KIND_CHAR && item->length > 1)
                && cblc_pointer_element_kind(item->kind, &pointer_element_kind) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            {
                const char *index_start;

                index_start = *cursor;
                if (cblc_parse_array_literal_index(cursor, &array_index) == FT_SUCCESS)
                {
                    if (cblc_data_kind_is_pointer(item->kind))
                    {
                        char literal_index[32];

                        if (std::snprintf(literal_index, sizeof(literal_index), "%zu",
                                array_index) < 0)
                            return (FT_FAILURE);
                        ft_strlcpy(index_expression, literal_index, sizeof(index_expression));
                        if (cblc_format_expression_indexed_reference(item->cobol_name,
                                literal_index, reference_text, sizeof(reference_text))
                            != FT_SUCCESS)
                            return (FT_FAILURE);
                        has_index = 0;
                        has_expression_index = 1;
                    }
                    else if (item->kind == CBLC_DATA_KIND_CHAR
                        && item->array_count == 0 && item->length > 1)
                    {
                        if (array_index >= item->length
                            || std::snprintf(reference_text, sizeof(reference_text),
                                "%s(%zu:1)", item->cobol_name, array_index + 1) < 0)
                            return (FT_FAILURE);
                        has_index = 0;
                        has_expression_index = 0;
                    }
                    else
                    {
                        if (cblc_format_occurs_reference(item->cobol_name, array_index,
                                reference_text, sizeof(reference_text)) != FT_SUCCESS)
                            return (FT_FAILURE);
                        has_index = 1;
                        has_expression_index = 0;
                    }
                }
                else
                {
                    *cursor = index_start;
                    if (**cursor != '[')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_array_variable_index(cursor, unit, index_expression,
                            sizeof(index_expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ']')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (item->kind == CBLC_DATA_KIND_CHAR
                        && item->array_count == 0 && item->length > 1)
                    {
                        char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (cblc_translate_expression_for_cobol(index_expression,
                                translated_index, sizeof(translated_index)) != FT_SUCCESS
                            || std::snprintf(reference_text, sizeof(reference_text),
                                "%s(%s + 1:1)", item->cobol_name,
                                translated_index) < 0
                            || std::strlen(reference_text) >= sizeof(reference_text))
                            return (FT_FAILURE);
                    }
                    else if (cblc_format_expression_indexed_reference(item->cobol_name,
                            index_expression, reference_text, sizeof(reference_text))
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    has_index = 0;
                    has_expression_index = 1;
                }
            }
            continue ;
        }
        if ((*cursor)[0] == '-' && (*cursor)[1] == '>')
        {
            char member_identifier[TRANSPILE_IDENTIFIER_MAX];
            char field_source_name[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_struct_type *owner_type;
            const t_cblc_struct_field *field;
            const t_cblc_data_item *actual_item;
            const char *base_source_name;

            if (item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
                return (FT_FAILURE);
            *cursor += 2;
            if (cblc_parse_identifier(cursor, member_identifier,
                    sizeof(member_identifier)) != FT_SUCCESS)
                return (FT_FAILURE);
            owner_type = cblc_find_receiver_type(unit, item);
            field = cblc_find_field_on_type(owner_type, member_identifier);
            if (!field || !cblc_member_visibility_is_allowed(owner_type, field->visibility))
                return (FT_FAILURE);
            base_source_name = item->source_name;
            if (item->is_alias)
            {
                actual_item = cblc_find_data_item_by_cobol(unit, item->cobol_name);
                if (actual_item && !actual_item->is_alias)
                    base_source_name = actual_item->source_name;
            }
            if (std::snprintf(field_source_name, sizeof(field_source_name), "%s->%s",
                    base_source_name, member_identifier) < 0)
                return (FT_FAILURE);
            if (std::snprintf(reference_text, sizeof(reference_text), "%s->%s",
                    item->cobol_name, member_identifier) < 0)
                return (FT_FAILURE);
            if (!cblc_find_data_item(unit, field_source_name))
            {
                const char *field_struct_type;
                const t_cblc_struct_type *nested_type;

                field_struct_type = NULL;
                if (field->struct_type_name[0] != '\0')
                    field_struct_type = field->struct_type_name;
                if (cblc_add_temp_alias_item(unit, field_source_name, reference_text,
                        field->kind, field->length, field->array_count,
                        field_struct_type, field->is_const) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (field->kind == CBLC_DATA_KIND_STRUCT)
                {
                    nested_type = cblc_find_struct_type(unit, field->struct_type_name);
                    if (!nested_type)
                        return (FT_FAILURE);
                    if (cblc_add_struct_pointer_nested_alias_items(unit, nested_type,
                            field_source_name, reference_text) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            item = cblc_find_data_item(unit, field_source_name);
            if (!item)
                return (FT_FAILURE);
            has_index = 0;
            array_index = 0;
            has_expression_index = 0;
            index_expression[0] = '\0';
            continue ;
        }
            if (**cursor != '.')
                break ;
        {
            char member_identifier[TRANSPILE_IDENTIFIER_MAX];
            char field_source_name[TRANSPILE_STATEMENT_TEXT_MAX];
            char field_cobol_name[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_struct_type *owner_type;
            const t_cblc_struct_field *field;

            *cursor += 1;
            if (cblc_parse_identifier(cursor, member_identifier,
                    sizeof(member_identifier)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                && std::strncmp(member_identifier, "len", sizeof(member_identifier)) == 0)
            {
                if (out_len_reference)
                    *out_len_reference = 1;
                if (out_reference)
                {
                    if (has_expression_index)
                    {
                        if (cblc_format_string_component_expression_reference(item->cobol_name,
                                index_expression, "-LEN", out_reference, out_reference_size)
                            != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else if (cblc_format_string_component_reference(item->cobol_name, has_index,
                            array_index, "-LEN", out_reference, out_reference_size)
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                *out_item = item;
                return (FT_SUCCESS);
            }
            if (item->kind == CBLC_DATA_KIND_STRUCT_POINTER && has_expression_index)
            {
                const char *field_struct_type;
                const t_cblc_struct_type *nested_type;

                owner_type = cblc_find_receiver_type(unit, item);
                field = cblc_find_field_on_type(owner_type, member_identifier);
                if (!field || !cblc_member_visibility_is_allowed(owner_type, field->visibility))
                    return (FT_FAILURE);
                if (std::snprintf(field_source_name, sizeof(field_source_name), "%s[%s].%s",
                        item->source_name, index_expression, member_identifier) < 0)
                    return (FT_FAILURE);
                if (std::snprintf(field_cobol_name, sizeof(field_cobol_name), "%s.%s",
                        reference_text, member_identifier) < 0)
                    return (FT_FAILURE);
                if (!cblc_find_data_item(unit, field_source_name))
                {
                    field_struct_type = NULL;
                    if (field->struct_type_name[0] != '\0')
                        field_struct_type = field->struct_type_name;
                    if (cblc_add_temp_alias_item(unit, field_source_name, field_cobol_name,
                            field->kind, field->length, field->array_count,
                            field_struct_type, field->is_const) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (field->kind == CBLC_DATA_KIND_STRUCT)
                    {
                        nested_type = cblc_find_struct_type(unit, field->struct_type_name);
                        if (!nested_type)
                            return (FT_FAILURE);
                        if (cblc_add_struct_pointer_nested_alias_items(unit, nested_type,
                                field_source_name, field_cobol_name) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                }
                item = cblc_find_data_item(unit, field_source_name);
                if (!item)
                    return (FT_FAILURE);
                ft_strlcpy(reference_text, item->cobol_name, sizeof(reference_text));
                has_index = 0;
                array_index = 0;
                has_expression_index = 0;
                index_expression[0] = '\0';
                continue ;
            }
            if (item->kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
            owner_type = cblc_find_receiver_type(unit, item);
            field = cblc_find_field_on_type(owner_type, member_identifier);
            if (!field || !cblc_member_visibility_is_allowed(owner_type, field->visibility))
                return (FT_FAILURE);
            cblc_build_field_source_name(item->source_name, member_identifier, field_source_name,
                sizeof(field_source_name));
            item = cblc_find_data_item(unit, field_source_name);
            if (!item)
                return (FT_FAILURE);
            ft_strlcpy(reference_text, item->cobol_name, sizeof(reference_text));
            has_index = 0;
            array_index = 0;
            has_expression_index = 0;
            index_expression[0] = '\0';
        }
    }
    if (item->array_count > 0 && !has_index && !has_expression_index)
        return (FT_FAILURE);
    if (out_len_reference)
        *out_len_reference = 0;
    if (out_reference)
        ft_strlcpy(out_reference, reference_text, out_reference_size);
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_parse_data_reference(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference)
{
    return (cblc_parse_data_reference_text(cursor, unit, out_item, out_len_reference,
            NULL, 0));
}

static int cblc_struct_type_append_field(t_cblc_struct_type *type, const char *identifier,
    size_t length, size_t array_count, t_cblc_data_kind kind, const char *struct_type_name,
    int is_const, t_cblc_member_visibility visibility)
{
    t_cblc_struct_field *field;

    if (!type || !identifier)
        return (FT_FAILURE);
    if (type->field_count >= type->field_capacity)
    {
        if (cblc_struct_type_ensure_field_capacity(type,
                type->field_capacity == 0 ? 4 : type->field_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    field = &type->fields[type->field_count];
    ft_strlcpy(field->source_name, identifier, sizeof(field->source_name));
    cblc_identifier_to_cobol(identifier, field->cobol_name, sizeof(field->cobol_name));
    if (kind == CBLC_DATA_KIND_CHAR_BUFFER)
        ft_strlcpy(field->declared_type_name, "char[]", sizeof(field->declared_type_name));
    else if (struct_type_name)
        ft_strlcpy(field->declared_type_name, struct_type_name, sizeof(field->declared_type_name));
    if (struct_type_name)
        ft_strlcpy(field->struct_type_name, struct_type_name, sizeof(field->struct_type_name));
    else
        field->struct_type_name[0] = '\0';
    field->length = length;
    field->array_count = array_count;
    field->kind = kind;
    field->is_const = is_const;
    field->visibility = visibility;
    type->field_count += 1;
    return (FT_SUCCESS);
}
