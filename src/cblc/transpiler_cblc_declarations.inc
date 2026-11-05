static int cblc_parse_char_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;
    size_t initializer_length;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "char"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    length = 1;
    if (**cursor == '[')
    {
        size_t candidate_length;
        int saw_dynamic_dimension;

        *cursor += 1;
        cblc_skip_whitespace(cursor);
        candidate_length = 1;
        saw_dynamic_dimension = 0;
        if (cblc_parse_numeric_literal(cursor, &candidate_length) == FT_SUCCESS)
        {
            length = candidate_length;
        }
        else
        {
            char dimension_identifier[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_parse_identifier(cursor, dimension_identifier,
                    sizeof(dimension_identifier)) != FT_SUCCESS)
                return (FT_FAILURE);
            saw_dynamic_dimension = 1;
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ']')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor == '[')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            if (cblc_parse_numeric_literal(cursor, &candidate_length) != FT_SUCCESS)
                return (FT_FAILURE);
            length = candidate_length;
            cblc_skip_whitespace(cursor);
            if (**cursor != ']')
                return (FT_FAILURE);
            *cursor += 1;
            cblc_skip_whitespace(cursor);
        }
        else if (saw_dynamic_dimension)
            length = 0;
    }
    initializer_text[0] = '\0';
    initializer_length = 0;
    if (is_const)
    {
        if (length == 0)
            return (FT_FAILURE);
        if (**cursor != '=')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_char_const_initializer(cursor, initializer_text,
                sizeof(initializer_text), length, &initializer_length) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_finalize_data_item(unit, identifier, length, 0, CBLC_DATA_KIND_CHAR,
            is_const, is_const, initializer_text, initializer_length));
}

static int cblc_parse_int_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t array_count;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "int"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    array_count = 0;
    if (**cursor == '[')
    {
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    initializer_text[0] = '\0';
    if (is_const)
    {
        if (array_count > 0)
            return (FT_FAILURE);
        if (**cursor != '=')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_int_const_initializer(cursor, initializer_text,
                sizeof(initializer_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_finalize_data_item(unit, identifier, 0, array_count, CBLC_DATA_KIND_INT,
            is_const, is_const, initializer_text, 0));
}

static int cblc_parse_bool_declaration(const char **cursor,
    t_cblc_translation_unit *unit, int is_const)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[8];
    int boolean_value;
    int has_initializer;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "bool"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    initializer_text[0] = '\0';
    has_initializer = 0;
    if (**cursor == '=')
    {
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_boolean_literal(cursor, &boolean_value) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (std::snprintf(initializer_text, sizeof(initializer_text), "%d",
                boolean_value) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        has_initializer = 1;
        cblc_skip_whitespace(cursor);
    }
    else if (is_const)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_finalize_data_item(unit, identifier, 0, 0, CBLC_DATA_KIND_INT,
            is_const, has_initializer, initializer_text, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (**cursor != '"')
        return (FT_FAILURE);
    *cursor += 1;
    index = 0;
    while (**cursor != '\0' && **cursor != '"')
    {
        if (index + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[index] = **cursor;
        index += 1;
        *cursor += 1;
    }
    if (**cursor != '"')
        return (FT_FAILURE);
    buffer[index] = '\0';
    *cursor += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_import(const char **cursor, t_cblc_translation_unit *unit)
{
    char path[TRANSPILE_FILE_PATH_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "import"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_string_literal(cursor, path, sizeof(path)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->import_count >= unit->import_capacity)
    {
        if (cblc_translation_unit_ensure_import_capacity(unit,
                unit->import_capacity == 0 ? 4 : unit->import_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(unit->imports[unit->import_count].path, path,
        sizeof(unit->imports[unit->import_count].path));
    unit->import_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_copy(const char **cursor, t_cblc_translation_unit *unit)
{
    char name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "copy"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_string_literal(cursor, name, sizeof(name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->copy_include_count >= unit->copy_include_capacity)
    {
        if (cblc_translation_unit_ensure_copy_capacity(unit,
                unit->copy_include_capacity == 0 ? 4 : unit->copy_include_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(unit->copy_includes[unit->copy_include_count].name, name,
        sizeof(unit->copy_includes[unit->copy_include_count].name));
    unit->copy_include_count += 1;
    return (FT_SUCCESS);
}
