static int cblc_append_external_call_using_argument(char *buffer, size_t buffer_size,
    const char *argument_text, int by_value)
{
    if (!buffer || buffer_size == 0 || !argument_text || argument_text[0] == '\0')
        return (FT_FAILURE);
    if (buffer[0] != '\0')
        ft_strlcat(buffer, " ", buffer_size);
    if (by_value)
        ft_strlcat(buffer, "BY VALUE ", buffer_size);
    else
        ft_strlcat(buffer, "BY REFERENCE ", buffer_size);
    ft_strlcat(buffer, argument_text, buffer_size);
    return (FT_SUCCESS);
}

static int cblc_argument_is_numeric_literal(const char *argument)
{
    size_t index;

    if (!argument || argument[0] == '\0')
        return (0);
    index = 0;
    if (argument[index] == '+' || argument[index] == '-')
        index += 1;
    if (argument[index] == '\0')
        return (0);
    while (argument[index] != '\0')
    {
        if (argument[index] < '0' || argument[index] > '9')
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_expression_is_simple_cobol_reference(const char *expression)
{
    size_t index;

    if (!expression || expression[0] == '\0')
        return (0);
    index = 0;
    while (expression[index] != '\0')
    {
        if ((expression[index] >= 'A' && expression[index] <= 'Z')
            || (expression[index] >= 'a' && expression[index] <= 'z')
            || (expression[index] >= '0' && expression[index] <= '9')
            || expression[index] == '-'
            || expression[index] == '_'
            || expression[index] == '('
            || expression[index] == ')'
            || expression[index] == ':')
        {
            index += 1;
            continue ;
        }
        return (0);
    }
    return (1);
}

static int cblc_build_external_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!unit || !statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    index = 0;
    while (index < statement->call_argument_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_extract_call_argument(statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (index > 0)
            ft_strlcat(buffer, " ", buffer_size);
        if (cblc_argument_is_numeric_literal(argument))
        {
            ft_strlcat(buffer, "BY VALUE ", buffer_size);
            ft_strlcat(buffer, argument, buffer_size);
        }
        else
        {
            char cobol_reference[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_data_item *argument_item;

            argument_item = cblc_find_data_item_const(unit, argument);
            if (argument_item)
                ft_strlcpy(cobol_reference, argument_item->cobol_name,
                    sizeof(cobol_reference));
            else if (cblc_translate_expression_for_cobol(argument, cobol_reference,
                        sizeof(cobol_reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!cblc_expression_is_simple_cobol_reference(cobol_reference))
                return (FT_FAILURE);
            ft_strlcat(buffer, "BY REFERENCE ", buffer_size);
            ft_strlcat(buffer, cobol_reference, buffer_size);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_is_entry_parameter_item(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_function *entry_function;
    size_t index;

    if (!unit || !item || item->is_alias || item->is_function_local)
        return (0);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (0);
    entry_function = &unit->functions[unit->entry_function_index];
    if (std::strncmp(item->owner_function_name, entry_function->source_name,
            sizeof(item->owner_function_name)) != 0)
        return (0);
    index = 0;
    while (index < entry_function->parameter_count)
    {
        if (std::strncmp(item->source_name,
                entry_function->parameters[index].actual_source_name,
                sizeof(item->source_name)) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_is_external_entry_function(const t_cblc_translation_unit *unit)
{
    const t_cblc_function *entry_function;

    if (!unit || unit->function_count == 0)
        return (0);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (0);
    entry_function = &unit->functions[unit->entry_function_index];
    if (std::strncmp(entry_function->source_name, "main",
            sizeof(entry_function->source_name)) == 0)
        return (0);
    return (1);
}

static int cblc_is_entry_return_item(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_function *entry_function;

    if (!unit || !item)
        return (0);
    if (!cblc_is_external_entry_function(unit))
        return (0);
    entry_function = &unit->functions[unit->entry_function_index];
    if (entry_function->return_kind == CBLC_FUNCTION_RETURN_VOID)
        return (0);
    if (std::strncmp(item->source_name, entry_function->return_source_name,
            sizeof(item->source_name)) == 0)
        return (1);
    return (0);
}

static int cblc_unit_has_pointer_data(const t_cblc_translation_unit *unit)
{
    size_t index;

    if (!unit)
        return (0);
    index = 0;
    while (index < unit->data_count)
    {
        if (cblc_data_kind_is_pointer(unit->data_items[index].kind))
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_pointer_element_size(t_cblc_data_kind kind, size_t *out_size)
{
    if (!out_size)
        return (FT_FAILURE);
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
    {
        *out_size = 1;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_INT_POINTER)
    {
        *out_size = 4;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_INT_POINTER_POINTER)
    {
        *out_size = 8;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_pointer_item_element_size(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, size_t *out_size)
{
    if (!item || !out_size)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        return (cblc_sizeof_data_kind(unit, CBLC_DATA_KIND_STRUCT, 0, 0,
                item->struct_type_name, out_size));
    return (cblc_pointer_element_size(item->kind, out_size));
}

static const char *cblc_pointer_based_name(t_cblc_data_kind element_kind)
{
    if (element_kind == CBLC_DATA_KIND_CHAR)
        return ("CBLC-PTR-CHAR");
    if (element_kind == CBLC_DATA_KIND_INT)
        return ("CBLC-PTR-INT");
    if (element_kind == CBLC_DATA_KIND_INT_POINTER)
        return ("CBLC-PTR-INT-PTR");
    return (NULL);
}

static int cblc_build_struct_pointer_based_name(const char *pointer_cobol_name,
    char *buffer, size_t buffer_size)
{
    if (!pointer_cobol_name || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "CBLC-SPTR-%s", pointer_cobol_name) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_prepare_struct_pointer_field(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *field_reference,
    size_t field_reference_size)
{
    char pointer_name[TRANSPILE_IDENTIFIER_MAX];
    char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char field_identifier[TRANSPILE_IDENTIFIER_MAX];
    char field_cobol_path[TRANSPILE_STATEMENT_TEXT_MAX];
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];
    const char *component_suffix;
    char line[256];
    const char *arrow;
    const char *open_bracket;
    const char *close_bracket;
    const char *field_cursor;
    const t_cblc_data_item *pointer_item;
    const t_cblc_struct_type *type;
    const t_cblc_struct_field *field;
    size_t pointer_name_length;
    size_t index_length;
    size_t element_size;
    int has_index;

    if (!unit || !reference || !builder || !field_reference || field_reference_size == 0)
        return (FT_FAILURE);
    arrow = std::strstr(reference, "->");
    has_index = 0;
    index_expression[0] = '\0';
    if (arrow)
    {
        pointer_name_length = static_cast<size_t>(arrow - reference);
        field_cursor = arrow + 2;
    }
    else
    {
        open_bracket = std::strchr(reference, '[');
        if (!open_bracket)
            return (FT_FAILURE);
        close_bracket = std::strchr(open_bracket, ']');
        if (!close_bracket || close_bracket[1] != '.')
            return (FT_FAILURE);
        pointer_name_length = static_cast<size_t>(open_bracket - reference);
        index_length = static_cast<size_t>(close_bracket - open_bracket - 1);
        if (index_length == 0 || index_length + 1 > sizeof(index_expression))
            return (FT_FAILURE);
        std::memcpy(index_expression, open_bracket + 1, index_length);
        index_expression[index_length] = '\0';
        field_cursor = close_bracket + 2;
        has_index = 1;
    }
    if (pointer_name_length == 0 || pointer_name_length + 1 > sizeof(pointer_name))
        return (FT_FAILURE);
    std::memcpy(pointer_name, reference, pointer_name_length);
    pointer_name[pointer_name_length] = '\0';
    if (cblc_parse_identifier(&field_cursor, field_identifier, sizeof(field_identifier))
        != FT_SUCCESS)
        return (FT_FAILURE);
    pointer_item = cblc_find_data_item_by_cobol(unit, pointer_name);
    if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, pointer_item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    field_cobol_path[0] = '\0';
    component_suffix = NULL;
    field = NULL;
    while (1)
    {
        char next_field_cobol_path[TRANSPILE_STATEMENT_TEXT_MAX];

        field = cblc_find_field_on_type(type, field_identifier);
        if (!field)
            return (FT_FAILURE);
        if (field_cobol_path[0] == '\0')
        {
            ft_strlcpy(field_cobol_path, field->cobol_name, sizeof(field_cobol_path));
        }
        else
        {
            if (std::snprintf(next_field_cobol_path, sizeof(next_field_cobol_path),
                    "%s-%s", field_cobol_path, field->cobol_name) < 0)
                return (FT_FAILURE);
            ft_strlcpy(field_cobol_path, next_field_cobol_path, sizeof(field_cobol_path));
        }
        if (*field_cursor != '.')
            break ;
        if (field->kind != CBLC_DATA_KIND_STRUCT)
            return (FT_FAILURE);
        type = cblc_find_struct_type(unit, field->struct_type_name);
        if (!type)
            return (FT_FAILURE);
        field_cursor += 1;
        if (cblc_parse_identifier(&field_cursor, field_identifier,
                sizeof(field_identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    component_suffix = field_cursor;
    if (*component_suffix != '\0'
        && std::strncmp(component_suffix, "-BUF", sizeof("-BUF")) != 0
        && std::strncmp(component_suffix, "-LEN", sizeof("-LEN")) != 0)
        return (FT_FAILURE);
    if ((field->kind == CBLC_DATA_KIND_INT || field->kind == CBLC_DATA_KIND_CHAR)
        && *component_suffix != '\0')
        return (FT_FAILURE);
    if (field->kind == CBLC_DATA_KIND_STRING
        && (*component_suffix == '\0'
            || (std::strncmp(component_suffix, "-BUF", sizeof("-BUF")) != 0
                && std::strncmp(component_suffix, "-LEN", sizeof("-LEN")) != 0)))
        return (FT_FAILURE);
    if (field->kind != CBLC_DATA_KIND_INT && field->kind != CBLC_DATA_KIND_CHAR
        && field->kind != CBLC_DATA_KIND_STRING)
        return (FT_FAILURE);
    if (cblc_build_struct_pointer_based_name(pointer_item->cobol_name, based_name,
            sizeof(based_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (has_index)
    {
        if (std::snprintf(line, sizeof(line), "           SET CBLC-PTR-CURSOR TO %s.",
                pointer_item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(index_expression, "0", sizeof(index_expression)) != 0)
        {
            if (cblc_pointer_item_element_size(unit, pointer_item, &element_size) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(index_expression, translated_index,
                    sizeof(translated_index)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE CBLC-PTR-OFFSET = %s * %zu.",
                    translated_index, element_size) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder,
                    "           SET CBLC-PTR-CURSOR UP BY CBLC-PTR-OFFSET.") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO CBLC-PTR-CURSOR.",
                based_name) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO %s.",
            based_name, pointer_item->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(field_reference, field_reference_size, "%s-%s%s",
            based_name, field_cobol_path, component_suffix) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_element_reference(const t_cblc_translation_unit *unit,
    const char *reference, const t_cblc_data_item **out_pointer_item,
    t_cblc_data_kind *out_element_kind, char *index_expression, size_t index_expression_size,
    int *out_has_index)
{
    char base_name[TRANSPILE_IDENTIFIER_MAX];
    const char *open_bracket;
    const t_cblc_data_item *item;
    size_t base_length;
    size_t index_length;

    if (!unit || !reference || !out_pointer_item || !out_element_kind
        || !index_expression || index_expression_size == 0 || !out_has_index)
        return (FT_FAILURE);
    index_expression[0] = '\0';
    *out_has_index = 0;
    if (reference[0] == '*')
    {
        item = cblc_find_data_item_by_cobol(unit, reference + 1);
        if (!item || cblc_pointer_element_kind(item->kind, out_element_kind) != FT_SUCCESS)
            return (FT_FAILURE);
        *out_pointer_item = item;
        return (FT_SUCCESS);
    }
    open_bracket = std::strchr(reference, '[');
    if (!open_bracket)
        return (FT_FAILURE);
    base_length = static_cast<size_t>(open_bracket - reference);
    if (base_length == 0 || base_length + 1 > sizeof(base_name))
        return (FT_FAILURE);
    std::memcpy(base_name, reference, base_length);
    base_name[base_length] = '\0';
    item = cblc_find_data_item_by_cobol(unit, base_name);
    if (!item || cblc_pointer_element_kind(item->kind, out_element_kind) != FT_SUCCESS)
        return (FT_FAILURE);
    open_bracket += 1;
    index_length = std::strcspn(open_bracket, "]");
    if (open_bracket[index_length] != ']' || open_bracket[index_length + 1] != '\0')
        return (FT_FAILURE);
    if (index_length + 1 > index_expression_size)
        return (FT_FAILURE);
    std::memcpy(index_expression, open_bracket, index_length);
    index_expression[index_length] = '\0';
    *out_pointer_item = item;
    *out_has_index = 1;
    return (FT_SUCCESS);
}

static int cblc_strip_pointer_cast_reference(const char *reference, char *buffer,
    size_t buffer_size)
{
    const char *close_paren;

    if (!reference || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (reference[0] != '(')
        return (FT_FAILURE);
    close_paren = std::strchr(reference, ')');
    if (!close_paren || close_paren[1] == '\0')
        return (FT_FAILURE);
    ft_strlcpy(buffer, close_paren + 1, buffer_size);
    return (FT_SUCCESS);
}

static int cblc_emit_prepare_struct_pointer_value(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *based_name, size_t based_name_size)
{
    const t_cblc_data_item *pointer_item;
    char pointer_name[TRANSPILE_IDENTIFIER_MAX];
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[256];
    const char *open_bracket;
    const char *close_bracket;
    const char *pointer_reference;
    size_t pointer_name_length;
    size_t index_length;
    size_t element_size;
    int has_index;

    if (!unit || !reference || !builder || !based_name || based_name_size == 0)
        return (FT_FAILURE);
    has_index = 0;
    pointer_reference = reference;
    index_expression[0] = '\0';
    if (reference[0] == '*')
        pointer_reference = reference + 1;
    else
    {
        open_bracket = std::strchr(reference, '[');
        if (!open_bracket)
            return (FT_FAILURE);
        close_bracket = std::strchr(open_bracket, ']');
        if (!close_bracket || close_bracket[1] != '\0')
            return (FT_FAILURE);
        pointer_name_length = static_cast<size_t>(open_bracket - reference);
        if (pointer_name_length == 0 || pointer_name_length + 1 > sizeof(pointer_name))
            return (FT_FAILURE);
        std::memcpy(pointer_name, reference, pointer_name_length);
        pointer_name[pointer_name_length] = '\0';
        index_length = static_cast<size_t>(close_bracket - open_bracket - 1);
        if (index_length == 0 || index_length + 1 > sizeof(index_expression))
            return (FT_FAILURE);
        std::memcpy(index_expression, open_bracket + 1, index_length);
        index_expression[index_length] = '\0';
        pointer_reference = pointer_name;
        has_index = 1;
    }
    pointer_item = cblc_find_data_item_by_cobol(unit, pointer_reference);
    if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        return (FT_FAILURE);
    if (cblc_build_struct_pointer_based_name(pointer_item->cobol_name, based_name,
            based_name_size) != FT_SUCCESS)
        return (FT_FAILURE);
    if (has_index)
    {
        if (std::snprintf(line, sizeof(line), "           SET CBLC-PTR-CURSOR TO %s.",
                pointer_item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(index_expression, "0", sizeof(index_expression)) != 0)
        {
            if (cblc_pointer_item_element_size(unit, pointer_item, &element_size) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(index_expression, translated_index,
                    sizeof(translated_index)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE CBLC-PTR-OFFSET = %s * %zu.",
                    translated_index, element_size) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder,
                    "           SET CBLC-PTR-CURSOR UP BY CBLC-PTR-OFFSET.") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO CBLC-PTR-CURSOR.",
                based_name) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO %s.",
            based_name, pointer_item->cobol_name) < 0)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line));
}

static int cblc_emit_prepare_pointer_element(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *based_name, size_t based_name_size)
{
    const t_cblc_data_item *pointer_item;
    char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[256];
    t_cblc_data_kind element_kind;
    int has_index;
    size_t element_size;
    const char *name;

    if (!unit || !reference || !builder || !based_name || based_name_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_pointer_element_reference(unit, reference, &pointer_item, &element_kind,
            index_expression, sizeof(index_expression), &has_index) != FT_SUCCESS)
        return (FT_FAILURE);
    name = cblc_pointer_based_name(element_kind);
    if (!name)
        return (FT_FAILURE);
    ft_strlcpy(based_name, name, based_name_size);
    if (std::snprintf(line, sizeof(line), "           SET CBLC-PTR-CURSOR TO %s.",
            pointer_item->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (has_index && std::strncmp(index_expression, "0", sizeof(index_expression)) != 0)
    {
        if (cblc_pointer_item_element_size(unit, pointer_item, &element_size) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_expression_for_cobol(index_expression, translated_index,
                sizeof(translated_index)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           COMPUTE CBLC-PTR-OFFSET = %s * %zu.",
                translated_index, element_size) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder,
                "           SET CBLC-PTR-CURSOR UP BY CBLC-PTR-OFFSET.") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO CBLC-PTR-CURSOR.",
            based_name) < 0)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line));
}

static int cblc_emit_pointer_assignment_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line, size_t line_size)
{
    const t_cblc_data_item *target_item;
    const t_cblc_data_item *source_item;
    char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_based[TRANSPILE_STATEMENT_TEXT_MAX];
    char source_based[TRANSPILE_STATEMENT_TEXT_MAX];
    char stripped_source[TRANSPILE_STATEMENT_TEXT_MAX];
    const char *assignment_source;

    if (!unit || !statement || !builder || !line || line_size == 0)
        return (FT_FAILURE);
    assignment_source = statement->source;
    if (!statement->is_literal
        && cblc_strip_pointer_cast_reference(statement->source, stripped_source,
            sizeof(stripped_source)) == FT_SUCCESS)
        assignment_source = stripped_source;
    if (cblc_emit_prepare_struct_pointer_value(unit, statement->target, builder,
            target_based, sizeof(target_based)) == FT_SUCCESS)
    {
        if (statement->is_literal)
            return (FT_FAILURE);
        if (cblc_emit_prepare_struct_pointer_value(unit, assignment_source, builder,
                source_based, sizeof(source_based)) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(assignment_source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(target_based, "CBLC-PTR-INT-PTR",
                std::strlen("CBLC-PTR-INT-PTR") + 1) == 0)
        {
            if (std::snprintf(line, line_size, "           SET %s TO %s",
                    target_based, source_text) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_text, target_based) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (cblc_emit_prepare_struct_pointer_field(unit, statement->target, builder,
            target_based, sizeof(target_based)) == FT_SUCCESS)
    {
        if (statement->is_literal)
            ft_strlcpy(source_text, statement->source, sizeof(source_text));
        else if (cblc_emit_prepare_pointer_element(unit, assignment_source, builder,
                source_based, sizeof(source_based)) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_emit_prepare_struct_pointer_field(unit, assignment_source, builder,
                source_based, sizeof(source_based)) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(assignment_source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_text, target_based) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (cblc_emit_prepare_pointer_element(unit, statement->target, builder, target_based,
            sizeof(target_based)) == FT_SUCCESS)
    {
        if (statement->is_literal)
            ft_strlcpy(source_text, statement->source, sizeof(source_text));
        else if (cblc_emit_prepare_pointer_element(unit, assignment_source, builder,
                source_based, sizeof(source_based)) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(assignment_source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_text, target_based) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!statement->is_literal && cblc_emit_prepare_struct_pointer_field(unit,
            assignment_source, builder, source_based, sizeof(source_based)) == FT_SUCCESS)
    {
        if (cblc_translate_expression_for_cobol(statement->target, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(source_based, "CBLC-PTR-INT-PTR",
                std::strlen("CBLC-PTR-INT-PTR") + 1) == 0)
        {
            if (std::snprintf(line, line_size, "           SET %s TO %s",
                    source_text, source_based) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_based, source_text) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!statement->is_literal && cblc_emit_prepare_struct_pointer_value(unit,
            assignment_source, builder, source_based, sizeof(source_based)) == FT_SUCCESS)
    {
        if (cblc_translate_expression_for_cobol(statement->target, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_based, source_text) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (!statement->is_literal && cblc_emit_prepare_pointer_element(unit, assignment_source,
            builder, source_based, sizeof(source_based)) == FT_SUCCESS)
    {
        if (cblc_translate_expression_for_cobol(statement->target, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_based, source_text) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    target_item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!target_item || !cblc_data_kind_is_pointer(target_item->kind))
        return (FT_FAILURE);
    if (statement->is_literal
        && std::strncmp(statement->source, "NULL", sizeof(statement->source)) == 0)
    {
        if (std::snprintf(line, line_size, "           SET %s TO NULL",
                target_item->cobol_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (assignment_source[0] == '&')
    {
        if (std::snprintf(line, line_size, "           SET %s TO ADDRESS OF %s",
                target_item->cobol_name, assignment_source + 1) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    source_item = cblc_find_data_item_by_cobol(unit, assignment_source);
    if (source_item && cblc_data_kind_is_pointer(source_item->kind))
    {
        if (std::snprintf(line, line_size, "           SET %s TO %s",
                target_item->cobol_name, source_item->cobol_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (std::strchr(assignment_source, '+') || std::strchr(assignment_source, '-'))
    {
        char base_name[TRANSPILE_IDENTIFIER_MAX];
        char operand[TRANSPILE_STATEMENT_TEXT_MAX];
        char translated_operand[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *operator_position;
        const t_cblc_data_item *base_item;
        char operator_char;
        size_t base_length;
        size_t element_size;

        operator_position = std::strchr(assignment_source, '+');
        if (!operator_position)
            operator_position = std::strchr(assignment_source, '-');
        if (!operator_position)
            return (FT_FAILURE);
        operator_char = *operator_position;
        base_length = static_cast<size_t>(operator_position - assignment_source);
        while (base_length > 0 && assignment_source[base_length - 1] == ' ')
            base_length -= 1;
        if (base_length == 0 || base_length + 1 > sizeof(base_name))
            return (FT_FAILURE);
        std::memcpy(base_name, assignment_source, base_length);
        base_name[base_length] = '\0';
        operator_position += 1;
        while (*operator_position == ' ')
            operator_position += 1;
        ft_strlcpy(operand, operator_position, sizeof(operand));
        base_item = cblc_find_data_item_by_cobol(unit, base_name);
        if (!base_item || !cblc_data_kind_is_pointer(base_item->kind)
            || cblc_pointer_item_element_size(unit, base_item, &element_size) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           SET %s TO %s.",
                target_item->cobol_name, base_item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_expression_for_cobol(operand, translated_operand,
                sizeof(translated_operand)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           COMPUTE CBLC-PTR-OFFSET = %s * %zu.",
                translated_operand, element_size) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (operator_char == '+')
        {
            if (std::snprintf(line, line_size, "           SET %s UP BY CBLC-PTR-OFFSET",
                    target_item->cobol_name) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, line_size, "           SET %s DOWN BY CBLC-PTR-OFFSET",
                target_item->cobol_name) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_emit_pointer_compute_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line, size_t line_size)
{
    char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_text[TRANSPILE_STATEMENT_TEXT_MAX];
    char based_name[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !statement || !builder || !line || line_size == 0)
        return (FT_FAILURE);
    if (cblc_emit_prepare_pointer_element(unit, statement->source, builder,
            based_name, sizeof(based_name)) == FT_SUCCESS)
        ft_strlcpy(source_text, based_name, sizeof(source_text));
    else if (cblc_emit_prepare_struct_pointer_field(unit, statement->source, builder,
            based_name, sizeof(based_name)) == FT_SUCCESS)
        ft_strlcpy(source_text, based_name, sizeof(source_text));
    else if (cblc_translate_expression_for_cobol(statement->source, source_text,
            sizeof(source_text)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_emit_prepare_pointer_element(unit, statement->target, builder,
            based_name, sizeof(based_name)) == FT_SUCCESS)
        ft_strlcpy(target_text, based_name, sizeof(target_text));
    else if (cblc_emit_prepare_struct_pointer_field(unit, statement->target, builder,
            based_name, sizeof(based_name)) == FT_SUCCESS)
        ft_strlcpy(target_text, based_name, sizeof(target_text));
    else if (cblc_translate_expression_for_cobol(statement->target, target_text,
            sizeof(target_text)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, line_size, "           COMPUTE %s = %s",
            target_text, source_text) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_method_body(const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_method *method, const char *assign_target, t_cobol_text_builder *builder)
{
    size_t index;

    if (!unit || !receiver || !method || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < method->statement_count)
    {
        const t_cblc_statement *body_statement;
        t_cblc_statement substituted;
        char line[256];

        body_statement = &method->statements[index];
        substituted = *body_statement;
        if (cblc_replace_this_prefix(body_statement->target, receiver->cobol_name, substituted.target,
                sizeof(substituted.target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_replace_this_prefix(body_statement->source, receiver->cobol_name, substituted.source,
                sizeof(substituted.source)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (substituted.type == CBLC_STATEMENT_RETURN)
        {
            if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
            {
                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                        assign_target, substituted.source) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
            {
                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                        substituted.source, assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            return (FT_SUCCESS);
        }
        if (cblc_emit_substituted_statement(unit, NULL, &substituted, receiver->cobol_name, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    const char *receiver_name;
    const char *assign_target;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_METHOD_CALL)
    {
        receiver_name = statement->target;
        assign_target = NULL;
    }
    else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
    {
        receiver_name = statement->source;
        assign_target = statement->target;
    }
    else
        return (FT_FAILURE);
    receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver);
    if (!receiver_type)
        return (FT_FAILURE);
    method = cblc_find_method_on_type(receiver_type, statement->call_identifier);
    if (!method)
        return (FT_FAILURE);
    if (receiver_type->is_builtin
        && std::strncmp(receiver_type->source_name, "string", TRANSPILE_IDENTIFIER_MAX) == 0)
    {
        char line[512];
        const t_cblc_data_item *status_item;

        if (std::strncmp(statement->call_identifier, "append",
                sizeof(statement->call_identifier)) == 0)
        {
            if (unit->helper_status_index < 0
                || static_cast<size_t>(unit->helper_status_index) >= unit->data_count)
                return (FT_FAILURE);
            status_item = &unit->data_items[unit->helper_status_index];
            if (!status_item)
                return (FT_FAILURE);
            if (cblc_string_item_uses_dynamic_storage(receiver))
            {
                if (cblc_emit_dynamic_string_ensure_capacity(receiver, receiver->length, builder)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (statement->is_literal)
                {
                    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    const char *literal_cursor;
                    size_t literal_length;

                    literal_cursor = statement->source;
                    if (cblc_parse_string_literal(&literal_cursor, literal_buffer,
                            sizeof(literal_buffer)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    literal_length = std::strlen(literal_buffer);
                    if (std::snprintf(line, sizeof(line),
                            "           MOVE %s TO %s-BUF(%s-LEN + 1:%zu).",
                            statement->source, receiver->cobol_name, receiver->cobol_name,
                            literal_length) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = %s-LEN + %zu.",
                            receiver->cobol_name, receiver->cobol_name, literal_length) < 0)
                        return (FT_FAILURE);
                    return (cobol_text_builder_append_line(builder, line));
                }
                else
                {
                    const t_cblc_data_item *source_item;

                    source_item = cblc_find_data_item_by_cobol(unit, statement->source);
                    if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           MOVE %s-BUF(1:%s-LEN) TO %s-BUF(%s-LEN + 1:%s-LEN).",
                            source_item->cobol_name, source_item->cobol_name,
                            receiver->cobol_name, receiver->cobol_name,
                            source_item->cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           COMPUTE %s-LEN = %s-LEN + %s-LEN.",
                            receiver->cobol_name, receiver->cobol_name,
                            source_item->cobol_name) < 0)
                        return (FT_FAILURE);
                    return (cobol_text_builder_append_line(builder, line));
                }
            }
            if (statement->is_literal)
            {
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE 0 %s BY REFERENCE %s.",
                        receiver->cobol_name, receiver->cobol_name, statement->source,
                        status_item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, receiver->cobol_name, statement->source,
                        status_item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "len",
                sizeof(statement->call_identifier)) == 0)
        {
            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s-LEN.",
                    assign_target, receiver->cobol_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "capacity",
                sizeof(statement->call_identifier)) == 0)
        {
            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %zu.",
                    assign_target, receiver->length) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "clear",
                sizeof(statement->call_identifier)) == 0)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                    receiver->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = 0.",
                    receiver->cobol_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "empty",
                sizeof(statement->call_identifier)) == 0)
        {
            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           IF %s-LEN = 0",
                    receiver->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
        if (std::strncmp(statement->call_identifier, "equals",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                char literal_cobol[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text,
                        sizeof(literal_text)) != FT_SUCCESS)
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                literal_cobol[0] = '"';
                literal_cobol[1] = '\0';
                ft_strlcat(literal_cobol, literal_text, sizeof(literal_cobol));
                ft_strlcat(literal_cobol, "\"", sizeof(literal_cobol));
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN = %zu AND %s-BUF(1:%s-LEN) = %s",
                        receiver->cobol_name, literal_length, receiver->cobol_name,
                        receiver->cobol_name, literal_cobol) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN = %s-LEN",
                        receiver->cobol_name, argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "               IF %s-BUF(1:%s-LEN) = %s-BUF(1:%s-LEN)",
                        receiver->cobol_name, receiver->cobol_name,
                        argument_item->cobol_name, argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] != '"')
            {
                if (cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
        if (std::strncmp(statement->call_identifier, "compare",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                char literal_cobol[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text, sizeof(literal_text))
                    != FT_SUCCESS || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                literal_cobol[0] = '"';
                literal_cobol[1] = '\0';
                ft_strlcat(literal_cobol, literal_text, sizeof(literal_cobol));
                ft_strlcat(literal_cobol, "\"", sizeof(literal_cobol));
                if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN < %zu",
                        receiver->cobol_name, literal_length) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = -1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           ELSE IF %s-LEN > %zu",
                        receiver->cobol_name, literal_length) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           ELSE IF %s-BUF(1:%s-LEN) < %s",
                        receiver->cobol_name, receiver->cobol_name, literal_cobol) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = -1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           ELSE IF %s-BUF(1:%s-LEN) > %s",
                        receiver->cobol_name, receiver->cobol_name, literal_cobol) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (cobol_text_builder_append_line(builder, "           END-IF."));
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCMP-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, argument_item->cobol_name, assign_target) < 0)
                    return (FT_FAILURE);
                return (cobol_text_builder_append_line(builder, line));
            }
        }
        if (std::strncmp(statement->call_identifier, "contains",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text, sizeof(literal_text))
                    != FT_SUCCESS || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                if (literal_length == 0)
                {
                    if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 1.",
                            assign_target) < 0)
                        return (FT_FAILURE);
                    return (cobol_text_builder_append_line(builder, line));
                }
                if (std::snprintf(line, sizeof(line),
                        "           INSPECT %s-BUF(1:%s-LEN) TALLYING %s FOR ALL %s",
                        receiver->cobol_name, receiver->cobol_name, assign_target, argument) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN = 0",
                        argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           ELSE") < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "               INSPECT %s-BUF(1:%s-LEN) TALLYING %s FOR ALL %s-BUF(1:%s-LEN)",
                        receiver->cobol_name, receiver->cobol_name, assign_target,
                        argument_item->cobol_name, argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           END-IF.") < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           IF %s > 0",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (cobol_text_builder_append_line(builder, "           END-IF."));
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           IF %s > 0",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
        if (std::strncmp(statement->call_identifier, "starts_with",
                sizeof(statement->call_identifier)) == 0
            || std::strncmp(statement->call_identifier, "ends_with",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text, sizeof(literal_text))
                    != FT_SUCCESS || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                if (std::strncmp(statement->call_identifier, "starts_with",
                        sizeof(statement->call_identifier)) == 0)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %zu AND %s-BUF(1:%zu) = %s",
                            receiver->cobol_name, literal_length, receiver->cobol_name,
                            literal_length, argument) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %zu AND %s-BUF(%s-LEN - %zu + 1:%zu) = %s",
                            receiver->cobol_name, literal_length, receiver->cobol_name,
                            receiver->cobol_name, literal_length, literal_length, argument) < 0)
                        return (FT_FAILURE);
                }
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::strncmp(statement->call_identifier, "starts_with",
                        sizeof(statement->call_identifier)) == 0)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %s-LEN AND %s-BUF(1:%s-LEN) = %s-BUF(1:%s-LEN)",
                            receiver->cobol_name, argument_item->cobol_name,
                            receiver->cobol_name, argument_item->cobol_name,
                            argument_item->cobol_name, argument_item->cobol_name) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %s-LEN AND %s-BUF(%s-LEN - %s-LEN + 1:%s-LEN) = %s-BUF(1:%s-LEN)",
                            receiver->cobol_name, argument_item->cobol_name,
                            receiver->cobol_name, receiver->cobol_name,
                            argument_item->cobol_name, argument_item->cobol_name,
                            argument_item->cobol_name, argument_item->cobol_name) < 0)
                        return (FT_FAILURE);
                }
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
    }
    if (method->parameter_count > 0)
    {
        if (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count, method->parameters,
                method->parameter_count, builder) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (cblc_emit_method_body(unit, receiver, method, assign_target, builder));
}

static int cblc_emit_dynamic_string_array_lifecycle(const t_cblc_data_item *item,
    t_cobol_text_builder *builder, int is_destruct)
{
    char line[256];
    size_t element_index;
    size_t initial_capacity;

    if (!item || !builder || !cblc_string_item_array_uses_dynamic_storage(item))
        return (FT_FAILURE);
    initial_capacity = item->initial_capacity;
    if (initial_capacity == 0)
        initial_capacity = item->length;
    if (initial_capacity == 0)
        initial_capacity = 1;
    element_index = 0;
    while (element_index < item->array_count)
    {
        size_t cobol_index;

        cobol_index = element_index + 1;
        if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-LEN(%zu).",
                item->cobol_name, cobol_index) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (is_destruct)
        {
            if (std::snprintf(line, sizeof(line), "           IF %s-PTR(%zu) NOT = NULL",
                    item->cobol_name, cobol_index) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               FREE %s-PTR(%zu)",
                    item->cobol_name, cobol_index) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           SET %s-PTR(%zu) TO NULL.",
                    item->cobol_name, cobol_index) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-CAP(%zu).",
                    item->cobol_name, cobol_index) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            char cap_name[TRANSPILE_STATEMENT_TEXT_MAX];
            char ptr_name[TRANSPILE_STATEMENT_TEXT_MAX];
            char buffer_name[TRANSPILE_IDENTIFIER_MAX];

            if (std::snprintf(cap_name, sizeof(cap_name), "%s-CAP(%zu)",
                    item->cobol_name, cobol_index) < 0)
                return (FT_FAILURE);
            if (std::snprintf(ptr_name, sizeof(ptr_name), "%s-PTR(%zu)",
                    item->cobol_name, cobol_index) < 0)
                return (FT_FAILURE);
            if (std::snprintf(buffer_name, sizeof(buffer_name), "%s-BUF",
                    item->cobol_name) < 0)
                return (FT_FAILURE);
            if (cblc_emit_dynamic_string_ensure_capacity_for_references(cap_name,
                    ptr_name, buffer_name, initial_capacity, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO %s",
                    buffer_name, ptr_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s.",
                    buffer_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        element_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder)
{
    const t_cblc_constructor *constructor;
    const t_cblc_data_item *item;
    const t_cblc_struct_type *type;
    const t_cblc_statement *body;
    size_t count;
    size_t index;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!item)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_STRING)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[256];
        const t_cblc_data_item *source_item;
        const char *literal_cursor;
        char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t assigned_length;
        size_t constructor_capacity;

        if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-LEN.",
                item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cblc_string_item_array_uses_dynamic_storage(item))
            return (cblc_emit_dynamic_string_array_lifecycle(item, builder,
                    statement->type == CBLC_STATEMENT_DESTRUCT));
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (statement->type == CBLC_STATEMENT_DESTRUCT
            && cblc_string_item_uses_dynamic_storage(item))
        {
            char cap_name[TRANSPILE_IDENTIFIER_MAX];
            char ptr_name[TRANSPILE_IDENTIFIER_MAX];

            cblc_build_string_component_name(item->cobol_name, "-CAP", cap_name, sizeof(cap_name));
            cblc_build_string_component_name(item->cobol_name, "-PTR", ptr_name, sizeof(ptr_name));
            if (std::snprintf(line, sizeof(line), "           IF %s NOT = NULL", ptr_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               FREE %s", ptr_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           SET %s TO NULL.", ptr_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.", cap_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        constructor_capacity = item->initial_capacity;
        if (constructor_capacity == 0)
            constructor_capacity = 1;
        if (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->call_argument_count == 0)
        {
            if (cblc_emit_dynamic_string_ensure_capacity(item, constructor_capacity, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                    item->cobol_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (statement->call_argument_count != 1)
            return (FT_FAILURE);
        if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (argument[0] >= '0' && argument[0] <= '9')
        {
            literal_cursor = argument;
            if (cblc_parse_numeric_literal(&literal_cursor, &constructor_capacity) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_emit_dynamic_string_ensure_capacity(item, constructor_capacity, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                    item->cobol_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (argument[0] == '"')
        {
            literal_cursor = argument;
            if (cblc_parse_string_literal(&literal_cursor, literal_buffer, sizeof(literal_buffer))
                != FT_SUCCESS || *literal_cursor != '\0')
                return (FT_FAILURE);
            assigned_length = std::strlen(literal_buffer);
            if (cblc_emit_dynamic_string_ensure_capacity(item, assigned_length, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s-BUF.",
                    argument, item->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = %zu.",
                    item->cobol_name, assigned_length) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        source_item = cblc_find_data_item_const(unit, argument);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (cblc_emit_dynamic_string_ensure_capacity(item, source_item->length, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE %s-BUF TO %s-BUF.",
                source_item->cobol_name, item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = %s-LEN.",
                item->cobol_name, source_item->cobol_name) < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && ((statement->call_argument_count == 0 && type->has_default_constructor)
            || statement->call_argument_count > 0))
    {
        constructor = cblc_find_constructor_for_arguments(unit, type,
                statement->call_arguments, statement->call_argument_count);
        if (!constructor)
        {
            if (statement->call_argument_count == 0)
                return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder, 0));
            return (FT_FAILURE);
        }
        if (constructor->statement_count == 0)
            return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder, 0));
        if (constructor->parameter_count > 0)
        {
            if (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                    statement->call_argument_count, constructor->parameters,
                    constructor->parameter_count, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        body = constructor->statements;
        count = constructor->statement_count;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement(unit, NULL, &body[index], item->cobol_name, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    if (statement->type == CBLC_STATEMENT_DESTRUCT && type->destructor_statement_count > 0)
    {
        body = type->destructor_statements;
        count = type->destructor_statement_count;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement(unit, NULL, &body[index], item->cobol_name, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder,
            statement->type == CBLC_STATEMENT_DESTRUCT));
}

static int cblc_emit_function(const t_cblc_translation_unit *unit, const t_cblc_function *function,
    t_cobol_text_builder *builder, int append_stop_run)
{
    char line[256];
    size_t index;

    if (!unit || !function || !builder)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "       %s.", function->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement;
        int skip_append;

        statement = &function->statements[index];
        skip_append = 0;
        if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_emit_pointer_assignment_statement(unit, statement, builder,
                    line, sizeof(line)) == FT_SUCCESS)
            {
                skip_append = 0;
            }
            else
            {
            if (statement->is_literal)
                ft_strlcpy(source_text, statement->source, sizeof(source_text));
            else if (cblc_translate_expression_for_cobol(statement->source, source_text,
                        sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(statement->target, target_text,
                    sizeof(target_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (statement->is_literal)
            {
                if (source_text[0] == '"')
                {
                    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    const char *literal_cursor;

                    literal_cursor = source_text;
                    if (cblc_parse_string_literal(&literal_cursor, literal_buffer,
                            sizeof(literal_buffer)) == FT_SUCCESS)
                    {
                        char prepared_target[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (cblc_prepare_dynamic_string_buffer_reference(unit, target_text,
                                std::strlen(literal_buffer), builder, prepared_target,
                                sizeof(prepared_target)) == FT_SUCCESS)
                            ft_strlcpy(target_text, prepared_target, sizeof(target_text));
                    }
                }
            }
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    source_text, target_text) < 0)
                return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_DISPLAY)
        {
            char base_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_data_item *item;

            if (statement->is_literal)
            {
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s",
                        statement->source) < 0)
                    return (FT_FAILURE);
            }
            else
            {
            item = NULL;
            if (cblc_parse_cobol_reference_base(statement->source, base_name, sizeof(base_name))
                == FT_SUCCESS)
                item = cblc_find_data_item_by_cobol(unit, base_name);
            if (item && item->kind == CBLC_DATA_KIND_STRING)
            {
                char buffer_name[TRANSPILE_IDENTIFIER_MAX];
                char length_name[TRANSPILE_IDENTIFIER_MAX];
                char translated_buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_length_name[TRANSPILE_STATEMENT_TEXT_MAX];

                if (std::strncmp(statement->source, item->cobol_name,
                        sizeof(item->cobol_name)) == 0)
                {
                    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name,
                        sizeof(buffer_name));
                    cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                        sizeof(length_name));
                }
                else if (std::strchr(statement->source, '['))
                {
                    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
                    const char *open_bracket;
                    size_t index_length;

                    open_bracket = std::strchr(statement->source, '[');
                    if (!open_bracket)
                        return (FT_FAILURE);
                    open_bracket += 1;
                    index_length = std::strcspn(open_bracket, "]");
                    if (index_length + 1 >= sizeof(index_expression)
                        || open_bracket[index_length] != ']')
                        return (FT_FAILURE);
                    std::memcpy(index_expression, open_bracket, index_length);
                    index_expression[index_length] = '\0';
                    if (cblc_format_string_component_expression_reference(item->cobol_name,
                            index_expression, "-BUF", buffer_name, sizeof(buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_expression_reference(item->cobol_name,
                            index_expression, "-LEN", length_name, sizeof(length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    size_t zero_based_index;

                    if (cblc_parse_occurs_reference_index(statement->source, &zero_based_index)
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-BUF", buffer_name, sizeof(buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-LEN", length_name, sizeof(length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (std::strchr(statement->source, '['))
                {
                    if (cblc_translate_expression_for_cobol(buffer_name, translated_buffer_name,
                            sizeof(translated_buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_translate_expression_for_cobol(length_name, translated_length_name,
                            sizeof(translated_length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    ft_strlcpy(translated_buffer_name, buffer_name, sizeof(translated_buffer_name));
                    ft_strlcpy(translated_length_name, length_name, sizeof(translated_length_name));
                }
                {
                    char prepared_buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (cblc_prepare_dynamic_string_buffer_view(unit, translated_buffer_name,
                            builder, prepared_buffer_name, sizeof(prepared_buffer_name))
                        == FT_SUCCESS)
                        ft_strlcpy(translated_buffer_name, prepared_buffer_name,
                            sizeof(translated_buffer_name));
                }
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s(1:%s)",
                        translated_buffer_name, translated_length_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                char source_text[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_translate_expression_for_cobol(statement->source, source_text,
                        sizeof(source_text)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s",
                        source_text) < 0)
                    return (FT_FAILURE);
            }
            }
        }
        else if (statement->type == CBLC_STATEMENT_COMPUTE)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_emit_pointer_compute_statement(unit, statement, builder,
                    line, sizeof(line)) == FT_SUCCESS)
            {
                skip_append = 0;
            }
            else
            {
            if (cblc_translate_expression_for_cobol(statement->source, source_text,
                    sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(statement->target, target_text,
                    sizeof(target_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                    target_text, source_text) < 0)
                return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_CALL)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_function *target_function;

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
            if (statement->call_is_external)
            {
                if (std::strncmp(statement->call_identifier, "CBLC-", 5) == 0
                    && statement->call_arguments[0] != '\0')
                {
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            statement->call_arguments) < 0)
                        return (FT_FAILURE);
                }
                else if (statement->call_arguments[0] != '\0')
                {
                    char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (cblc_build_external_call_arguments(unit, statement,
                            using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            using_arguments) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line), "           CALL '%s'",
                            cobol_name) < 0)
                        return (FT_FAILURE);
                }
            }
            else
            {
                if (std::strncmp(statement->call_identifier, "std::free",
                        sizeof(statement->call_identifier)) == 0)
                {
                    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (statement->call_argument_count != 1
                        || cblc_extract_call_argument(statement, 0, argument,
                            sizeof(argument)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_translate_expression_for_cobol(argument, translated_argument,
                            sizeof(translated_argument)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    {
                        const t_cblc_data_item *argument_item;

                        argument_item = cblc_find_data_item_const(unit, argument);
                        if (argument_item)
                            ft_strlcpy(translated_argument, argument_item->cobol_name,
                                sizeof(translated_argument));
                    }
                    if (std::snprintf(line, sizeof(line), "           FREE %s",
                            translated_argument) < 0)
                        return (FT_FAILURE);
                    skip_append = 0;
                    target_function = NULL;
                }
                else
                {
                target_function = cblc_find_function_in_unit(unit,
                    statement->call_identifier);
                if (!target_function)
                    return (FT_FAILURE);
                if (cblc_emit_local_call_argument_moves(unit, target_function,
                        statement, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        cobol_name) < 0)
                    return (FT_FAILURE);
                }
            }
        }
        else if (statement->type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_function *target_function;

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
            if (statement->call_is_external)
            {
                char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
                char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

                if (statement->target[0] == '\0')
                    return (FT_FAILURE);
                using_arguments[0] = '\0';
                if (statement->call_arguments[0] != '\0')
                {
                    if (cblc_build_external_call_arguments(unit, statement,
                            using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (cblc_translate_expression_for_cobol(statement->target, target_text,
                        sizeof(target_text)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_append_external_call_using_argument(using_arguments,
                        sizeof(using_arguments), target_text, 0) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, using_arguments) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::strncmp(statement->call_identifier, "std::malloc",
                        sizeof(statement->call_identifier)) == 0
                    || std::strncmp(statement->call_identifier, "std::realloc",
                        sizeof(statement->call_identifier)) == 0)
                {
                    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (statement->call_argument_count == 0
                        || cblc_extract_call_argument(statement,
                            statement->call_argument_count - 1, argument,
                            sizeof(argument)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_translate_expression_for_cobol(argument, translated_argument,
                            sizeof(translated_argument)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           ALLOCATE %s CHARACTERS RETURNING %s",
                            translated_argument, statement->target) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                target_function = cblc_find_function_in_unit(unit,
                    statement->call_identifier);
                if (!target_function)
                    return (FT_FAILURE);
                if (cblc_emit_local_call_argument_moves(unit, target_function,
                        statement, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        cobol_name) < 0)
                    return (FT_FAILURE);
                size_t perform_length;

                perform_length = std::strlen(line);
                if (perform_length + 1 >= sizeof(line))
                    return (FT_FAILURE);
                line[perform_length] = '.';
                line[perform_length + 1] = '\0';
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (statement->source[0] == '\0' || statement->target[0] == '\0')
                    return (FT_FAILURE);
                {
                    const t_cblc_data_item *target_item;

                    target_item = cblc_find_data_item_by_cobol(unit, statement->target);
                    if (target_item && cblc_data_kind_is_pointer(target_item->kind))
                    {
                        if (std::snprintf(line, sizeof(line), "           SET %s TO %s",
                                statement->target, statement->source) < 0)
                            return (FT_FAILURE);
                    }
                    else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                            statement->source, statement->target) < 0)
                        return (FT_FAILURE);
                }
                }
            }
        }
        else if (statement->type == CBLC_STATEMENT_RETURN)
        {
            if (statement->target[0] == '\0' || statement->source[0] == '\0')
            {
                skip_append = 1;
            }
            else
            {
                const t_cblc_data_item *target_item;

                target_item = cblc_find_data_item_by_cobol(unit, statement->target);
                if (target_item && cblc_data_kind_is_pointer(target_item->kind))
                {
                    if (cblc_emit_pointer_assignment_statement(unit, statement, builder,
                            line, sizeof(line)) != FT_SUCCESS)
                    {
                        if (std::snprintf(line, sizeof(line), "           SET %s TO %s",
                                statement->target, statement->source) < 0)
                            return (FT_FAILURE);
                    }
                }
                else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                        statement->source, statement->target) < 0)
                    return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->type == CBLC_STATEMENT_DESTRUCT)
        {
            if (cblc_emit_lifecycle_statement(unit, statement, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_METHOD_CALL
            || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            if (cblc_emit_method_statement(unit, statement, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else
            return (FT_FAILURE);
        if (!skip_append)
        {
            size_t length;

            length = std::strlen(line);
            if (length == 0 || line[length - 1] != '.')
            {
                if (length + 1 >= sizeof(line))
                    return (FT_FAILURE);
                line[length] = '.';
                line[length + 1] = '\0';
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    if (append_stop_run)
    {
        if (std::strncmp(function->source_name, "main", sizeof(function->source_name)) == 0)
        {
            if (cobol_text_builder_append_line(builder, "           STOP RUN.") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cobol_text_builder_append_line(builder, "           GOBACK.") != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_struct_fields(const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *prefix, size_t level, t_cobol_text_builder *builder)
{
    char line[256];
    char nested_prefix[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !type || !prefix || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s.",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s-LEN PIC 9(4) COMP VALUE 0.",
                    level + 5, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_string_field_uses_dynamic_storage(&type->fields[index]))
            {
                if (std::snprintf(line, sizeof(line),
                        "       %02zu %s-%s-CAP PIC 9(4) COMP VALUE 0.",
                        level + 5, prefix, type->fields[index].cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "       %02zu %s-%s-PTR USAGE POINTER VALUE NULL.",
                        level + 5, prefix, type->fields[index].cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s-BUF PIC X(%zu).",
                        level + 5, prefix, type->fields[index].cobol_name,
                        type->fields[index].length) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC X(%zu).",
                    level, prefix, type->fields[index].cobol_name,
                    type->fields[index].length) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_INT)
        {
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9) OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9).",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s.",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(nested_prefix, sizeof(nested_prefix), "%s-%s", prefix,
                    type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_emit_struct_fields(unit, field_type, nested_prefix, level + 5, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_struct_instance(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, t_cobol_text_builder *builder)
{
    const t_cblc_struct_type *type;
    char line[256];

    if (!unit || !item || !builder)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "       01 %s.", item->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cblc_emit_struct_fields(unit, type, item->cobol_name, 5, builder));
}

static int cblc_translation_unit_calls_are_resolved(const t_cblc_translation_unit *unit)
{
    size_t function_index;

    if (!unit)
        return (FT_FAILURE);
    function_index = 0;
    while (function_index < unit->function_count)
    {
        const t_cblc_function *function;
        size_t statement_index;

        function = &unit->functions[function_index];
        statement_index = 0;
        while (statement_index < function->statement_count)
        {
            const t_cblc_statement *statement;
            const t_cblc_function *local_target;

            statement = &function->statements[statement_index];
            if (statement->type != CBLC_STATEMENT_CALL
                && statement->type != CBLC_STATEMENT_CALL_ASSIGN)
            {
                statement_index += 1;
                continue ;
            }
            if (std::strncmp(statement->call_identifier, "CBLC-", 5) == 0)
            {
                statement_index += 1;
                continue ;
            }
            if (std::strncmp(statement->call_identifier, "std::malloc",
                    sizeof(statement->call_identifier)) == 0
                || std::strncmp(statement->call_identifier, "std::realloc",
                    sizeof(statement->call_identifier)) == 0
                || std::strncmp(statement->call_identifier, "std::free",
                    sizeof(statement->call_identifier)) == 0)
            {
                statement_index += 1;
                continue ;
            }
            local_target = cblc_find_function_in_unit(unit, statement->call_identifier);
            if (local_target)
            {
                statement_index += 1;
                continue ;
            }
            if (!statement->call_is_external)
                return (FT_FAILURE);
            statement_index += 1;
        }
        function_index += 1;
    }
    return (FT_SUCCESS);
}
