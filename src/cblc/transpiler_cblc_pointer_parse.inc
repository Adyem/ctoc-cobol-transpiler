static int cblc_data_kind_is_pointer(t_cblc_data_kind kind)
{
    if (kind == CBLC_DATA_KIND_VOID_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_INT_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_STRUCT_POINTER)
        return (1);
    if (kind == CBLC_DATA_KIND_INT_POINTER_POINTER)
        return (1);
    return (0);
}

static int cblc_pointer_element_kind(t_cblc_data_kind kind, t_cblc_data_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_CHAR;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_INT_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_INT;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_INT_POINTER_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_INT_POINTER;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_pointer_assignment_is_compatible(t_cblc_data_kind target_kind,
    t_cblc_data_kind source_kind)
{
    if (!cblc_data_kind_is_pointer(target_kind)
        || !cblc_data_kind_is_pointer(source_kind))
        return (0);
    if (target_kind == source_kind)
        return (1);
    if (target_kind == CBLC_DATA_KIND_VOID_POINTER
        || source_kind == CBLC_DATA_KIND_VOID_POINTER)
        return (1);
    return (0);
}

static int cblc_pointer_address_of_is_compatible(t_cblc_data_kind target_kind,
    t_cblc_data_kind source_kind)
{
    if (target_kind == CBLC_DATA_KIND_VOID_POINTER)
    {
        if (!cblc_data_kind_is_pointer(source_kind))
            return (1);
        return (0);
    }
    if (target_kind == CBLC_DATA_KIND_CHAR_POINTER && source_kind == CBLC_DATA_KIND_CHAR)
        return (1);
    if (target_kind == CBLC_DATA_KIND_INT_POINTER && source_kind == CBLC_DATA_KIND_INT)
        return (1);
    if (target_kind == CBLC_DATA_KIND_STRUCT_POINTER && source_kind == CBLC_DATA_KIND_STRUCT)
        return (1);
    if (target_kind == CBLC_DATA_KIND_INT_POINTER_POINTER
        && source_kind == CBLC_DATA_KIND_INT_POINTER)
        return (1);
    return (0);
}

static int cblc_parse_pointer_address_of_reference(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_data_kind target_kind,
    t_cblc_data_item **out_item, char *out_reference, size_t out_reference_size)
{
    char reference[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *item;
    int is_length_reference;

    if (!cursor || !*cursor || !unit || !out_item || !out_reference
        || out_reference_size == 0)
        return (FT_FAILURE);
    if (**cursor != '&')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
            reference, sizeof(reference)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!item || is_length_reference || item->kind == CBLC_DATA_KIND_STRING
        || (item->array_count > 0
            && std::strncmp(reference, item->cobol_name, sizeof(item->cobol_name)) == 0)
        || !cblc_pointer_address_of_is_compatible(target_kind, item->kind))
        return (FT_FAILURE);
    if (std::snprintf(out_reference, out_reference_size, "&%s", reference) < 0)
        return (FT_FAILURE);
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_arithmetic_reference(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_data_kind target_kind,
    t_cblc_data_item **out_item, char *out_reference, size_t out_reference_size)
{
    char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    char operand_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *source_item;
    t_cblc_data_item *operand_item;
    char operator_char;

    if (!cursor || !*cursor || !unit || !out_item || !out_reference
        || out_reference_size == 0)
        return (FT_FAILURE);
    if (target_kind == CBLC_DATA_KIND_VOID_POINTER)
        return (FT_FAILURE);
    if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL,
            source_reference, sizeof(source_reference)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!source_item || source_item->kind != target_kind
        || source_item->kind == CBLC_DATA_KIND_VOID_POINTER)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '+' && **cursor != '-')
        return (FT_FAILURE);
    operator_char = **cursor;
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor >= '0' && **cursor <= '9')
    {
        size_t length;

        length = 0;
        while (**cursor >= '0' && **cursor <= '9')
        {
            if (length + 1 >= sizeof(operand_reference))
                return (FT_FAILURE);
            operand_reference[length] = **cursor;
            length += 1;
            *cursor += 1;
        }
        operand_reference[length] = '\0';
    }
    else
    {
        if (cblc_parse_data_reference_text(cursor, unit, &operand_item, NULL,
                operand_reference, sizeof(operand_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!operand_item || operand_item->kind != CBLC_DATA_KIND_INT)
            return (FT_FAILURE);
    }
    if (std::snprintf(out_reference, out_reference_size, "%s %c %s", source_reference,
            operator_char, operand_reference) < 0)
        return (FT_FAILURE);
    *out_item = source_item;
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_dereference_reference(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_data_item **out_item,
    t_cblc_data_kind *out_kind, char *out_reference, size_t out_reference_size)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !out_item || !out_kind || !out_reference
        || out_reference_size == 0)
        return (FT_FAILURE);
    if (**cursor != '*')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    if (!item)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        *out_kind = CBLC_DATA_KIND_STRUCT;
    else if (cblc_pointer_element_kind(item->kind, out_kind) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(out_reference, out_reference_size, "*%s", item->cobol_name) < 0)
        return (FT_FAILURE);
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_type(const char **cursor, t_cblc_data_kind *out_kind)
{
    if (!cursor || !*cursor || !out_kind)
        return (FT_FAILURE);
    if (cblc_match_keyword(cursor, "void"))
    {
        *out_kind = CBLC_DATA_KIND_VOID_POINTER;
        return (FT_SUCCESS);
    }
    if (cblc_match_keyword(cursor, "char"))
    {
        *out_kind = CBLC_DATA_KIND_CHAR_POINTER;
        return (FT_SUCCESS);
    }
    if (cblc_match_keyword(cursor, "int"))
    {
        *out_kind = CBLC_DATA_KIND_INT_POINTER;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_parse_pointer_cast_type(const char **cursor, const t_cblc_translation_unit *unit,
    t_cblc_data_kind *out_kind, char *out_struct_type_name, size_t out_struct_type_name_size)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_struct_type *type;

    if (!cursor || !*cursor || !unit || !out_kind || !out_struct_type_name
        || out_struct_type_name_size == 0)
        return (FT_FAILURE);
    out_struct_type_name[0] = '\0';
    start = *cursor;
    if (cblc_parse_pointer_type(cursor, out_kind) == FT_SUCCESS)
        return (FT_SUCCESS);
    *cursor = start;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, identifier);
    if (!type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(out_struct_type_name, type->source_name, out_struct_type_name_size);
    *out_kind = CBLC_DATA_KIND_STRUCT_POINTER;
    return (FT_SUCCESS);
}

static const char *cblc_pointer_type_text(t_cblc_data_kind kind)
{
    if (kind == CBLC_DATA_KIND_VOID_POINTER)
        return ("void *");
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
        return ("char *");
    if (kind == CBLC_DATA_KIND_INT_POINTER)
        return ("int *");
    if (kind == CBLC_DATA_KIND_INT_POINTER_POINTER)
        return ("int **");
    return (NULL);
}

static int cblc_parse_pointer_cast_reference(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_data_kind expected_kind,
    const char *expected_struct_type_name, int enforce_expected_kind,
    t_cblc_data_item **out_item, t_cblc_data_kind *out_kind,
    char *out_struct_type_name, size_t out_struct_type_name_size,
    char *out_reference, size_t out_reference_size)
{
    const char *start;
    const char *type_text;
    t_cblc_data_kind cast_kind;
    t_cblc_data_item *source_item;
    char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];
    char cast_type_text[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !out_item || !out_kind || !out_struct_type_name
        || out_struct_type_name_size == 0 || !out_reference || out_reference_size == 0)
        return (FT_FAILURE);
    out_struct_type_name[0] = '\0';
    start = *cursor;
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_pointer_cast_type(cursor, unit, &cast_kind, cast_struct_type_name,
            sizeof(cast_struct_type_name)) != FT_SUCCESS)
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
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (enforce_expected_kind && cast_kind != expected_kind)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (enforce_expected_kind && cast_kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        if (!expected_struct_type_name || expected_struct_type_name[0] == '\0'
            || std::strncmp(cast_struct_type_name, expected_struct_type_name,
                sizeof(cast_struct_type_name)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL,
            source_reference, sizeof(source_reference)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (!source_item || !cblc_data_kind_is_pointer(source_item->kind)
        || std::strchr(source_reference, '['))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cast_kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        if (std::snprintf(cast_type_text, sizeof(cast_type_text), "%s *",
                cast_struct_type_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        type_text = cast_type_text;
    }
    else
    {
        type_text = cblc_pointer_type_text(cast_kind);
        if (!type_text)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    if (std::snprintf(out_reference, out_reference_size, "(%s)%s", type_text,
            source_reference) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(out_struct_type_name, cast_struct_type_name, out_struct_type_name_size);
    *out_item = source_item;
    *out_kind = cast_kind;
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, int is_const)
{
    const char *start;
    t_cblc_data_kind kind;
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || is_const)
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
    if (kind == CBLC_DATA_KIND_INT_POINTER && **cursor == '*')
    {
        kind = CBLC_DATA_KIND_INT_POINTER_POINTER;
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
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
    return (cblc_finalize_data_item(unit, identifier, 0, 0, kind, 0, 0, NULL, 0));
}

static int cblc_parse_struct_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, int is_const)
{
    const char *start;
    const t_cblc_struct_type *type;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || is_const)
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
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
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
    cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
    return (cblc_add_named_data_item(unit, identifier, cobol_name,
            CBLC_DATA_KIND_STRUCT_POINTER, 0, 0, type->source_name, NULL, 0, 0, 0));
}
