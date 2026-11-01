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
        else if (**cursor == '0')
            value = '\0';
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

static int cblc_parse_char_intrinsic_expression(const char **cursor,
    const t_cblc_translation_unit *unit, char *out_expression,
    size_t expression_size)
{
    const char *start;
    char intrinsic[TRANSPILE_IDENTIFIER_MAX];
    char argument[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_data_item *argument_item;
    int expression_length;

    if (!cursor || !*cursor || !unit || !out_expression || expression_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, intrinsic, sizeof(intrinsic)) != FT_SUCCESS
        || std::strcmp(intrinsic, "std::chr") != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        goto parse_failure;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, argument, sizeof(argument)) != FT_SUCCESS)
        goto parse_failure;
    argument_item = cblc_find_data_item_const(unit, argument);
    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_INT)
        goto parse_failure;
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        goto parse_failure;
    *cursor += 1;
    expression_length = std::snprintf(out_expression, expression_size,
        "FUNCTION CHAR(%s + 1)", argument_item->cobol_name);
    if (expression_length < 0
        || static_cast<size_t>(expression_length) >= expression_size)
        goto parse_failure;
    return (FT_SUCCESS);
parse_failure:
    *cursor = start;
    out_expression[0] = '\0';
    return (FT_FAILURE);
}

static int cblc_parse_char_const_initializer(const char **cursor, char *buffer, size_t buffer_size,
    size_t length, size_t *out_initializer_length)
{
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_value;
    size_t literal_length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (!out_initializer_length)
        return (FT_FAILURE);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        literal_length = std::strlen(literal_buffer);
        if (length != 0 && literal_length > length)
            return (FT_FAILURE);
        buffer[0] = '"';
        buffer[1] = '\0';
        ft_strlcat(buffer, literal_buffer, buffer_size);
        ft_strlcat(buffer, "\"", buffer_size);
        *out_initializer_length = literal_length;
        return (FT_SUCCESS);
    }
    if (**cursor != '\'')
        return (FT_FAILURE);
    if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
        return (FT_FAILURE);
    if (length != 0 && length < 1)
        return (FT_FAILURE);
    buffer[0] = '\'';
    buffer[1] = literal_value;
    buffer[2] = '\'';
    buffer[3] = '\0';
    *out_initializer_length = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_int_const_initializer(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t literal_value;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_numeric_literal(cursor, &literal_value) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%zu", literal_value) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_array_count_clause(const char **cursor, size_t *out_count)
{
    if (!cursor || !*cursor || !out_count)
        return (FT_FAILURE);
    if (**cursor != '[')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_numeric_literal(cursor, out_count) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ']')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    return (FT_SUCCESS);
}


static int cblc_finalize_data_item(t_cblc_translation_unit *unit, const char *identifier,
    size_t length, size_t array_count, t_cblc_data_kind kind, int is_const, int has_initializer,
    const char *initializer_text, size_t initializer_length)
{
    t_cblc_data_item *item;

    if (!unit || !identifier)
        return (FT_FAILURE);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(identifier, item->cobol_name, sizeof(item->cobol_name));
    if (kind == CBLC_DATA_KIND_CHAR_BUFFER)
        ft_strlcpy(item->declared_type_name, "char[]", sizeof(item->declared_type_name));
    item->length = length;
    item->initial_capacity = length;
    item->array_count = array_count;
    item->kind = kind;
    item->is_active = 1;
    item->is_const = is_const;
    item->has_initializer = has_initializer;
    item->initializer_length = initializer_length;
    item->initializer_text[0] = '\0';
    if (initializer_text)
        ft_strlcpy(item->initializer_text, initializer_text, sizeof(item->initializer_text));
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static void cblc_update_char_buffer_capacity_by_cobol(t_cblc_translation_unit *unit,
    const char *cobol_name, size_t desired_capacity)
{
    size_t index;

    if (!unit || !cobol_name)
        return ;
    if (desired_capacity == 0)
        desired_capacity = 1;
    index = 0;
    while (index < unit->data_count)
    {
        if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR_BUFFER
            && std::strncmp(unit->data_items[index].cobol_name, cobol_name,
                sizeof(unit->data_items[index].cobol_name)) == 0
            && unit->data_items[index].length < desired_capacity)
            unit->data_items[index].length = desired_capacity;
        index += 1;
    }
    return ;
}

static void cblc_update_char_buffer_capacity(t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, size_t desired_capacity)
{
    if (!unit || !item)
        return ;
    cblc_update_char_buffer_capacity_by_cobol(unit, item->cobol_name, desired_capacity);
    return ;
}
