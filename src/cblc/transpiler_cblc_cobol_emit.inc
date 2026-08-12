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

            argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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
    {
        if (std::strcmp(item->struct_type_name, "string") == 0)
            return (cblc_sizeof_data_kind(unit, CBLC_DATA_KIND_STRING, item->length,
                0, NULL, out_size));
        return (cblc_sizeof_data_kind(unit, CBLC_DATA_KIND_STRUCT, 0, 0,
            item->struct_type_name, out_size));
    }
    return (cblc_pointer_element_size(item->kind, out_size));
}

static int cblc_emit_string_pointer_fields(const t_cblc_data_item *item,
    t_cobol_text_builder *builder)
{
    char line[256];
    size_t length;

    if (!item || !builder || item->kind != CBLC_DATA_KIND_STRUCT_POINTER
        || std::strcmp(item->struct_type_name, "string") != 0)
        return (FT_FAILURE);
    length = item->length == 0 ? 1 : item->length;
    if (std::snprintf(line, sizeof(line), "       01 %s BASED.",
            item->cobol_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "          05 %s-LEN PIC 9(4) COMP VALUE 0.", item->cobol_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu).",
            item->cobol_name, length) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_string_pointer_element_reference(
    const t_cblc_translation_unit *unit, const char *reference,
    const t_cblc_data_item **out_pointer_item)
{
    char base_name[TRANSPILE_IDENTIFIER_MAX];
    const char *open_bracket;
    const char *close_bracket;
    size_t base_length;
    const t_cblc_data_item *item;

    if (!unit || !reference || !out_pointer_item)
        return (FT_FAILURE);
    open_bracket = std::strchr(reference, '[');
    if (!open_bracket)
        return (FT_FAILURE);
    close_bracket = std::strchr(open_bracket, ']');
    if (!close_bracket || close_bracket[1] != '\0')
        return (FT_FAILURE);
    base_length = static_cast<size_t>(open_bracket - reference);
    if (base_length == 0 || base_length + 1 > sizeof(base_name))
        return (FT_FAILURE);
    std::memcpy(base_name, reference, base_length);
    base_name[base_length] = '\0';
    item = cblc_find_data_item_by_cobol(unit, base_name);
    if (!item || item->kind != CBLC_DATA_KIND_STRUCT_POINTER
        || std::strcmp(item->struct_type_name, "string") != 0)
        return (FT_FAILURE);
    *out_pointer_item = item;
    return (FT_SUCCESS);
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
    {
        const t_cblc_data_item *base_item;
        const t_cblc_struct_type *base_type;
        const t_cblc_struct_field *data_field;
        char data_source_name[TRANSPILE_IDENTIFIER_MAX];

        base_item = cblc_find_data_item_by_cobol(unit, pointer_reference);
        base_type = cblc_find_receiver_type(unit, base_item);
        data_field = cblc_find_field_on_type(base_type, "data");
        if (!base_item || !base_type || !data_field)
            return (FT_FAILURE);
        cblc_build_field_source_name(base_item->source_name, "data",
            data_source_name, sizeof(data_source_name));
        pointer_item = cblc_find_data_item_const(unit, data_source_name);
        if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
            return (FT_FAILURE);
    }
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

static int cblc_emit_prepare_pointer_element_slot(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *based_name,
    size_t based_name_size, size_t slot)
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
    if (slot == 0)
        ft_strlcpy(based_name, name, based_name_size);
    else if (std::snprintf(based_name, based_name_size, "%s-%zu", name, slot + 1) < 0)
        return (FT_FAILURE);
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

static int cblc_emit_prepare_pointer_element(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *based_name, size_t based_name_size)
{
    return (cblc_emit_prepare_pointer_element_slot(unit, reference, builder, based_name,
        based_name_size, 0));
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
    if (unit->is_native_standard_library
        && std::strchr(statement->source, '[')
        && !std::strstr(statement->source, "-32")
        && !std::strstr(statement->source, "+32")
        && !std::strstr(statement->source, " - 32")
        && !std::strstr(statement->source, " + 32"))
    {
        if (cblc_translate_expression_for_cobol(statement->source, source_text,
                sizeof(source_text)) == FT_SUCCESS
            && cblc_translate_expression_for_cobol(statement->target, target_based,
                sizeof(target_based)) == FT_SUCCESS
            && std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_text, target_based) >= 0)
            return (FT_SUCCESS);
    }
    if (unit->is_native_standard_library && std::strchr(statement->target, '['))
    {
        char target_base[TRANSPILE_IDENTIFIER_MAX];

        if (cblc_parse_cobol_reference_base(statement->target, target_base,
                sizeof(target_base)) == FT_SUCCESS)
        {
            target_item = cblc_find_data_item_by_cobol(unit, target_base);
            if (target_item && target_item->kind == CBLC_DATA_KIND_CHAR_POINTER)
                return (FT_FAILURE);
        }
    }
    assignment_source = statement->source;
    if (!statement->is_literal
        && cblc_strip_pointer_cast_reference(statement->source, stripped_source,
            sizeof(stripped_source)) == FT_SUCCESS)
        assignment_source = stripped_source;
    {
        const t_cblc_data_item *string_pointer_item;

        if (cblc_parse_string_pointer_element_reference(unit, statement->target,
                &string_pointer_item) == FT_SUCCESS)
        {
            if (cblc_emit_prepare_struct_pointer_value(unit, statement->target, builder,
                    target_based, sizeof(target_based)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (statement->is_literal)
            {
                char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = statement->source;
                if (cblc_parse_string_literal(&literal_cursor, literal_buffer,
                        sizeof(literal_buffer)) != FT_SUCCESS
                    || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_buffer);
                if (std::snprintf(line, line_size, "           MOVE %s TO %s-BUF",
                        statement->source, target_based) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, line_size, "           COMPUTE %s-LEN = %zu",
                        target_based, literal_length) < 0)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            source_item = cblc_find_data_item_for_emit_by_source(unit, assignment_source);
            if (source_item && source_item->kind == CBLC_DATA_KIND_STRING)
                ft_strlcpy(source_based, source_item->cobol_name, sizeof(source_based));
            else if (cblc_emit_prepare_struct_pointer_value(unit, assignment_source, builder,
                    source_based, sizeof(source_based)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, line_size,
                    "           MOVE %s-BUF(1:%s-LEN) TO %s-BUF",
                    source_based, source_based, target_based) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || std::snprintf(line, line_size, "           COMPUTE %s-LEN = %s-LEN",
                    target_based, source_based) < 0)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
    }
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
        else if (cblc_emit_prepare_pointer_element_slot(unit, assignment_source, builder,
                source_based, sizeof(source_based), 1) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_emit_prepare_struct_pointer_field(unit, assignment_source, builder,
                source_based, sizeof(source_based)) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(assignment_source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (unit->is_native_standard_library
            && (std::strstr(source_text, "-32") || std::strstr(source_text, "- 32")
                || std::strstr(source_text, "+32") || std::strstr(source_text, "+ 32")))
        {
            const char *adjustment;
            char *operator_start;

            operator_start = std::strstr(source_text, " - ");
            adjustment = "-";
            if (!operator_start)
                operator_start = std::strstr(source_text, "-32");
            if (!operator_start)
                operator_start = std::strstr(source_text, "- 32");
            if (!operator_start)
            {
                adjustment = "+";
                operator_start = std::strstr(source_text, " + 32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, "+32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, "+ 32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, " + ");
            }
            if (operator_start)
                *operator_start = '\0';
            if (std::strlen(source_text) > 0
                && source_text[std::strlen(source_text) - 1] == '.')
                source_text[std::strlen(source_text) - 1] = '\0';
            if (std::snprintf(line, line_size,
                    "           MOVE FUNCTION CHAR(FUNCTION ORD(%s) %s 32) TO %s",
                    source_text, adjustment, target_based) < 0)
                return (FT_FAILURE);
            if (!builder->free_format && std::strlen(line) > 72)
            {
                char *to_clause;

                to_clause = std::strstr(line, "TO ");
                if (to_clause)
                {
                    if (cobol_text_builder_append_span(builder, line,
                            static_cast<size_t>(to_clause - line)) != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, " \n") != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, "      -    ") != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, to_clause) != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
                        return (FT_FAILURE);
                    line[0] = '\0';
                }
            }
        }
        else if (std::snprintf(line, line_size, "           MOVE %s TO %s",
                source_text, target_based) < 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (cblc_emit_prepare_pointer_element(unit, statement->target, builder, target_based,
            sizeof(target_based)) == FT_SUCCESS)
    {
        if (statement->is_literal)
            ft_strlcpy(source_text, statement->source, sizeof(source_text));
        else if (cblc_emit_prepare_pointer_element_slot(unit, assignment_source, builder,
                source_based, sizeof(source_based), 1) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(assignment_source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (unit->is_native_standard_library
            && (std::strstr(source_text, "-32") || std::strstr(source_text, "- 32")
                || std::strstr(source_text, "+32") || std::strstr(source_text, "+ 32")))
        {
            const char *adjustment;
            char *operator_start;

            operator_start = std::strstr(source_text, " - ");
            adjustment = "-";
            if (!operator_start)
                operator_start = std::strstr(source_text, "-32");
            if (!operator_start)
                operator_start = std::strstr(source_text, "- 32");
            if (!operator_start)
            {
                adjustment = "+";
                operator_start = std::strstr(source_text, " + 32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, "+32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, "+ 32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, " + ");
            }
            if (operator_start)
                *operator_start = '\0';
            if (std::strlen(source_text) > 0
                && source_text[std::strlen(source_text) - 1] == '.')
                source_text[std::strlen(source_text) - 1] = '\0';
            if (std::snprintf(line, line_size,
                    "           MOVE FUNCTION CHAR(FUNCTION ORD(%s) %s 32) TO %s",
                    source_text, adjustment, target_based) < 0)
                return (FT_FAILURE);
            if (!builder->free_format && std::strlen(line) > 72)
            {
                char *to_clause;

                to_clause = std::strstr(line, "TO ");
                if (to_clause)
                {
                    if (cobol_text_builder_append_span(builder, line,
                            static_cast<size_t>(to_clause - line)) != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, " \n") != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, "      -    ") != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, to_clause) != FT_SUCCESS
                        || cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
                        return (FT_FAILURE);
                    line[0] = '\0';
                }
            }
        }
        else if (std::snprintf(line, line_size, "           MOVE %s TO %s",
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
    const t_cblc_data_item *target_item;

    if (!unit || !statement || !builder || !line || line_size == 0)
        return (FT_FAILURE);
    target_item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (target_item && !cblc_data_kind_is_pointer(target_item->kind)
        && !std::strchr(statement->target, '[')
        && !std::strstr(statement->target, "->"))
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

static int cblc_method_field_is_string(const t_cblc_struct_field *field)
{
    return (field && (field->kind == CBLC_DATA_KIND_STRING
        || std::strcmp(field->struct_type_name, "string") == 0));
}

static int cblc_ascii_identifier_equal(const char *left, const char *right,
    size_t length)
{
    size_t index;

    if (!left || !right)
        return (0);
    index = 0;
    while (index < length)
    {
        char left_character;
        char right_character;

        left_character = left[index];
        right_character = right[index];
        if (left_character >= 'a' && left_character <= 'z')
            left_character = static_cast<char>(left_character - 'a' + 'A');
        if (right_character >= 'a' && right_character <= 'z')
            right_character = static_cast<char>(right_character - 'a' + 'A');
        if (left_character != right_character)
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_rewrite_method_field_references(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method, const char *input,
    char *output, size_t output_size)
{
    const t_cblc_struct_type *type;
    size_t input_index;

    if (!unit || !receiver || !input || !output || output_size == 0)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    output[0] = '\0';
    input_index = 0;
    while (input[input_index] != '\0')
    {
        size_t field_index;
        int matched;

        matched = 0;
        if (method)
        {
            size_t parameter_index;

            parameter_index = 0;
            while (parameter_index < method->parameter_count)
            {
                const t_cblc_parameter *parameter;
                char prefix[TRANSPILE_IDENTIFIER_MAX + 32];
                size_t prefix_length;

                parameter = &method->parameters[parameter_index];
                if (parameter->cobol_name[0] == '\0'
                    || std::snprintf(prefix, sizeof(prefix), "%s.",
                        parameter->cobol_name) < 0)
                {
                    parameter_index += 1;
                    continue ;
                }
                prefix_length = std::strlen(prefix);
                if (std::strncmp(input + input_index, prefix, prefix_length) == 0)
                {
                    const char *field_source;
                    size_t member_prefix_length;
                    size_t candidate_index;

                    field_source = input + input_index + prefix_length;
                    member_prefix_length = prefix_length;
                    if (std::strncmp(field_source, "CBLC-THIS-",
                            std::strlen("CBLC-THIS-")) == 0)
                    {
                        field_source += std::strlen("CBLC-THIS-");
                        member_prefix_length += std::strlen("CBLC-THIS-");
                    }
                    candidate_index = 0;
                    while (candidate_index < type->field_count)
                    {
                        const t_cblc_struct_field *field;
                        size_t field_length;

                        field = &type->fields[candidate_index];
                        field_length = std::strlen(field->source_name);
                        if (cblc_ascii_identifier_equal(field_source, field->source_name,
                                field_length)
                            && (field_source[field_length] == '\0'
                                || field_source[field_length] == ' '
                                || field_source[field_length] == ')'
                                || field_source[field_length] == '+'
                                || field_source[field_length] == '-'
                                || field_source[field_length] == '*'
                                || field_source[field_length] == '/'
                                || field_source[field_length] == '<'
                                || field_source[field_length] == '>'))
                        {
                            char replacement[TRANSPILE_STATEMENT_TEXT_MAX];

                            if (std::snprintf(replacement, sizeof(replacement), "%s-%s",
                                    parameter->cobol_name, field->cobol_name) < 0
                                || ft_strlcat(output, replacement, output_size) >= output_size)
                                return (FT_FAILURE);
                            input_index += member_prefix_length + field_length;
                            matched = 1;
                            break ;
                        }
                        candidate_index += 1;
                    }
                    if (matched)
                        break ;
                }
                parameter_index += 1;
            }
            if (matched)
                continue ;
        }

        matched = 0;
        field_index = 0;
        while (field_index < type->field_count)
        {
            const t_cblc_struct_field *field;
            size_t field_length;
            int at_boundary;
            const char *after_field;

            field = &type->fields[field_index];
            field_length = std::strlen(field->source_name);
            at_boundary = (input_index == 0
                || !((input[input_index - 1] >= 'A' && input[input_index - 1] <= 'Z')
                    || (input[input_index - 1] >= 'a' && input[input_index - 1] <= 'z')
                    || (input[input_index - 1] >= '0' && input[input_index - 1] <= '9')
                    || input[input_index - 1] == '_'));
            after_field = input + input_index + field_length;
            if (at_boundary && std::strncmp(input + input_index, field->source_name,
                    field_length) == 0
                && (after_field[0] == '\0' || after_field[0] == '.'
                    || after_field[0] == '[' || after_field[0] == ' '
                    || after_field[0] == ')' || after_field[0] == '+'
                    || after_field[0] == '-' || after_field[0] == '*'
                    || after_field[0] == '/' || after_field[0] == '<'
                    || after_field[0] == '>' || after_field[0] == '='
                    || after_field[0] == ']' || after_field[0] == ','))
            {
                char replacement[TRANSPILE_STATEMENT_TEXT_MAX];

                if (std::snprintf(replacement, sizeof(replacement), "%s-%s",
                        receiver->cobol_name, field->cobol_name) < 0)
                    return (FT_FAILURE);
                input_index += field_length;
                if (std::strncmp(input + input_index, ".len", 4) == 0
                    && cblc_method_field_is_string(field))
                {
                    ft_strlcat(replacement, "-LEN", sizeof(replacement));
                    input_index += 4;
                }
                else if (input[input_index] == '['
                    && cblc_method_field_is_string(field))
                {
                    const char *index_end;
                    size_t index_length;

                    index_end = std::strchr(input + input_index + 1, ']');
                    if (!index_end)
                        return (FT_FAILURE);
                    index_length = static_cast<size_t>(index_end - (input + input_index + 1));
                    ft_strlcat(replacement, "-BUF(", sizeof(replacement));
                    if (index_length > 0)
                    {
                        char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (index_length >= sizeof(index_expression))
                            return (FT_FAILURE);
                        std::memcpy(index_expression, input + input_index + 1, index_length);
                        index_expression[index_length] = '\0';
                        ft_strlcat(replacement, index_expression, sizeof(replacement));
                    }
                    ft_strlcat(replacement, " + 1:1)", sizeof(replacement));
                    input_index = static_cast<size_t>(index_end - input);
                    input_index += 1;
                }
                if (ft_strlcat(output, replacement, output_size) >= output_size)
                    return (FT_FAILURE);
                matched = 1;
                break ;
            }
            field_index += 1;
        }
        if (!matched)
        {
            char character[2];

            character[0] = input[input_index++];
            character[1] = '\0';
            if (ft_strlcat(output, character, output_size) >= output_size)
                return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int cblc_rewrite_method_data_references(const t_cblc_translation_unit *unit,
    const char *input, char *output, size_t output_size)
{
    size_t input_index;

    if (!unit || !input || !output || output_size == 0)
        return (FT_FAILURE);
    output[0] = '\0';
    input_index = 0;
    while (input[input_index] != '\0')
    {
        const t_cblc_data_item *best_item;
        size_t best_length;
        size_t data_index;

        best_item = NULL;
        best_length = 0;
        data_index = 0;
        while (data_index < unit->data_count)
        {
            const t_cblc_data_item *item;
            size_t item_length;
            char next_character;
            int at_boundary;

            item = &unit->data_items[data_index];
            item_length = std::strlen(item->source_name);
            next_character = input[input_index + item_length];
            at_boundary = (input_index == 0
                || !((input[input_index - 1] >= 'A' && input[input_index - 1] <= 'Z')
                    || (input[input_index - 1] >= 'a' && input[input_index - 1] <= 'z')
                    || (input[input_index - 1] >= '0' && input[input_index - 1] <= '9')
                    || input[input_index - 1] == '_'));
            if (item_length > best_length && item_length > 0 && at_boundary
                && std::strncmp(input + input_index, item->source_name, item_length) == 0
                && (next_character == '\0' || next_character == ' '
                    || next_character == ')' || next_character == '('
                    || next_character == '+' || next_character == '-'
                    || next_character == '*' || next_character == '/'
                    || next_character == '<' || next_character == '>'
                    || next_character == '=' || next_character == '['))
            {
                best_item = item;
                best_length = item_length;
            }
            data_index += 1;
        }
        if (best_item)
        {
            if (ft_strlcat(output, best_item->cobol_name, output_size) >= output_size)
                return (FT_FAILURE);
            input_index += best_length;
        }
        else
        {
            char character[2];

            character[0] = input[input_index++];
            character[1] = '\0';
            if (ft_strlcat(output, character, output_size) >= output_size)
                return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int cblc_restore_method_parameter_sources(const t_cblc_method *method,
    const char *input, char *output, size_t output_size)
{
    size_t index;

    if (!method || !input || !output || output_size == 0)
        return (FT_FAILURE);
    ft_strlcpy(output, input, output_size);
    index = 0;
    while (index < method->parameter_count)
    {
        const char *source_name;

        source_name = method->parameters[index].actual_source_name[0] != '\0'
            ? method->parameters[index].actual_source_name
            : method->parameters[index].source_name;
        if (method->parameters[index].cobol_name[0] != '\0'
            && source_name[0] != '\0')
        {
            cblc_replace_template_text(output, output_size,
                method->parameters[index].cobol_name, source_name);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_rewrite_method_parameter_references(const t_cblc_method *method,
    const char *input, char *output, size_t output_size)
{
    size_t index;

    if (!method || !input || !output || output_size == 0)
        return (FT_FAILURE);
    ft_strlcpy(output, input, output_size);
    index = 0;
    while (index < method->parameter_count)
    {
        const char *source_name;

        source_name = method->parameters[index].actual_source_name[0] != '\0'
            ? method->parameters[index].actual_source_name
            : method->parameters[index].source_name;
        if (source_name[0] != '\0' && method->parameters[index].cobol_name[0] != '\0')
            cblc_replace_template_text(output, output_size, source_name,
                method->parameters[index].cobol_name);
        if (method->parameters[index].source_name[0] != '\0'
            && method->parameters[index].cobol_name[0] != '\0')
            cblc_replace_template_text(output, output_size,
                method->parameters[index].source_name,
                method->parameters[index].cobol_name);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_normalize_method_string_subscripts(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const char *input, char *output, size_t output_size)
{
    const t_cblc_struct_type *type;
    size_t index;

    if (!unit || !receiver || !input || !output || output_size == 0)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    output[0] = '\0';
    index = 0;
    while (input[index] != '\0')
    {
        const t_cblc_struct_field *field;
        char marker[TRANSPILE_IDENTIFIER_MAX];
        size_t field_index;
        const char *open;

        field = NULL;
        field_index = 0;
        while (field_index < type->field_count)
        {
            if (cblc_method_field_is_string(&type->fields[field_index])
                && std::snprintf(marker, sizeof(marker), "%s-%s", receiver->cobol_name,
                    type->fields[field_index].cobol_name) >= 0
                && ft_strlcat(marker, "-BUF", sizeof(marker)) < sizeof(marker)
                && std::strncmp(input + index, marker, std::strlen(marker)) == 0
                && input[index + std::strlen(marker)] == '[')
            {
                field = &type->fields[field_index];
                break ;
            }
            field_index += 1;
        }
        if (!field)
        {
            char character[2];

            character[0] = input[index++];
            character[1] = '\0';
            if (ft_strlcat(output, character, output_size) >= output_size)
                return (FT_FAILURE);
            continue ;
        }
        open = input + index + std::strlen(marker);
        {
            const char *close;
            size_t expression_length;
            char expression[TRANSPILE_STATEMENT_TEXT_MAX];

            close = std::strchr(open + 1, ']');
            if (!close)
                return (FT_FAILURE);
            expression_length = static_cast<size_t>(close - (open + 1));
            if (expression_length >= sizeof(expression))
                return (FT_FAILURE);
            std::memcpy(expression, open + 1, expression_length);
            expression[expression_length] = '\0';
            if (ft_strlcat(output, marker, output_size) >= output_size
                || ft_strlcat(output, "(", output_size) >= output_size
                || ft_strlcat(output, expression, output_size) >= output_size
                || ft_strlcat(output, " + 1:1)", output_size) >= output_size)
                return (FT_FAILURE);
            index = static_cast<size_t>(close - input) + 1;
        }
    }
    return (FT_SUCCESS);
}

static int cblc_emit_method_reference_result(const t_cblc_translation_unit *unit,
    const t_cblc_method *method, const char *source, const char *assign_target,
    t_cobol_text_builder *builder)
{
    const t_cblc_data_item *target_item;
    t_cblc_data_item result_item;
    t_cblc_statement return_statement;
    char prepared_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char translated_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !method || !source || !assign_target || assign_target[0] == '\0' || !builder)
        return (FT_FAILURE);
    target_item = cblc_find_data_item_by_cobol(unit, assign_target);
    if (!target_item)
        target_item = cblc_find_data_item_for_emit_by_source(unit, assign_target);
    if (!target_item)
    {
        std::memset(&result_item, 0, sizeof(result_item));
        if (method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
            result_item.kind = CBLC_DATA_KIND_INT_POINTER;
        else if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
            result_item.kind = CBLC_DATA_KIND_CHAR_POINTER;
        else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            result_item.kind = CBLC_DATA_KIND_STRUCT_POINTER;
        else
            return (FT_FAILURE);
        ft_strlcpy(result_item.cobol_name, assign_target, sizeof(result_item.cobol_name));
        ft_strlcpy(result_item.struct_type_name, method->return_type_name,
            sizeof(result_item.struct_type_name));
        target_item = &result_item;
    }
    if (!target_item)
        return (FT_FAILURE);
    std::memset(&return_statement, 0, sizeof(return_statement));
    return_statement.type = CBLC_STATEMENT_RETURN;
    ft_strlcpy(return_statement.target, assign_target, sizeof(return_statement.target));
    ft_strlcpy(return_statement.source, source, sizeof(return_statement.source));
    if (cblc_data_kind_is_pointer(target_item->kind))
    {
        if (cblc_emit_pointer_assignment_statement(unit, &return_statement, builder,
                line, sizeof(line)) != FT_SUCCESS)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (cblc_emit_prepare_pointer_element(unit, source, builder, prepared_source,
            sizeof(prepared_source)) == FT_SUCCESS)
    {
        if (target_item->kind == CBLC_DATA_KIND_INT
            || target_item->kind == CBLC_DATA_KIND_DOUBLE)
        {
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    assign_target, prepared_source) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                prepared_source, assign_target) < 0)
            return (FT_FAILURE);
    }
    else
    {
        if (cblc_translate_expression_for_cobol(source, translated_source,
                sizeof(translated_source)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (target_item->kind == CBLC_DATA_KIND_INT
            || target_item->kind == CBLC_DATA_KIND_DOUBLE)
        {
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    assign_target, translated_source) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                translated_source, assign_target) < 0)
            return (FT_FAILURE);
    }
    return (cobol_text_builder_append_line(builder, line));
}

static int cblc_emit_method_body(const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_method *method, const char *assign_target, t_cobol_text_builder *builder)
{
    size_t index;
    int control_flow_depth;

    if (!unit || !receiver || !method || !builder)
        return (FT_FAILURE);
    index = 0;
    control_flow_depth = 0;
    while (index < method->statement_count)
    {
        const t_cblc_statement *body_statement;
        t_cblc_statement substituted;
        char rewritten_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char rewritten_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char rewritten_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[256];

        body_statement = &method->statements[index];
        substituted = *body_statement;
        if (cblc_restore_method_parameter_sources(method, body_statement->target,
                substituted.target, sizeof(substituted.target)) != FT_SUCCESS
            || cblc_restore_method_parameter_sources(method, body_statement->source,
                substituted.source, sizeof(substituted.source)) != FT_SUCCESS
            || cblc_restore_method_parameter_sources(method, body_statement->call_arguments,
                substituted.call_arguments, sizeof(substituted.call_arguments)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        if (cblc_rewrite_method_parameter_references(method, substituted.target,
                rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(method, substituted.source,
                rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(method, substituted.call_arguments,
                rewritten_call_arguments, sizeof(rewritten_call_arguments)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
        ft_strlcpy(substituted.call_arguments, rewritten_call_arguments,
            sizeof(substituted.call_arguments));
        if (method->is_copy_assignment
            && (cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.target, rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.source, rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.call_arguments, rewritten_call_arguments,
                    sizeof(rewritten_call_arguments)) != FT_SUCCESS))
            return (FT_FAILURE);
        ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
        ft_strlcpy(substituted.call_arguments, rewritten_call_arguments,
            sizeof(substituted.call_arguments));
        if (cblc_replace_this_prefix(rewritten_target, receiver->cobol_name, substituted.target,
                sizeof(substituted.target)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        if (cblc_replace_this_prefix(rewritten_source, receiver->cobol_name, substituted.source,
                sizeof(substituted.source)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        if (cblc_replace_this_prefix(rewritten_call_arguments, receiver->cobol_name,
                substituted.call_arguments, sizeof(substituted.call_arguments)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        if (!method->is_copy_assignment
            && (cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.target, rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.source, rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.call_arguments, rewritten_call_arguments,
                    sizeof(rewritten_call_arguments)) != FT_SUCCESS))
        {
            return (FT_FAILURE);
        }
        if (!method->is_copy_assignment)
        {
            ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
            ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
            ft_strlcpy(substituted.call_arguments, rewritten_call_arguments,
                sizeof(substituted.call_arguments));
        }
        if (cblc_rewrite_method_parameter_references(method, substituted.target,
                rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(method, substituted.source,
                rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(method, substituted.call_arguments,
                rewritten_call_arguments, sizeof(rewritten_call_arguments)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
        ft_strlcpy(substituted.call_arguments, rewritten_call_arguments,
            sizeof(substituted.call_arguments));
        if (cblc_rewrite_method_data_references(unit, substituted.target,
                mapped_target, sizeof(mapped_target)) != FT_SUCCESS
            || cblc_rewrite_method_data_references(unit, substituted.source,
                mapped_source, sizeof(mapped_source)) != FT_SUCCESS
            || cblc_rewrite_method_data_references(unit, substituted.call_arguments,
                mapped_call_arguments, sizeof(mapped_call_arguments)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        ft_strlcpy(substituted.target, mapped_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, mapped_source, sizeof(substituted.source));
        ft_strlcpy(substituted.call_arguments, mapped_call_arguments,
            sizeof(substituted.call_arguments));
        if (substituted.type == CBLC_STATEMENT_CALL
            || substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *implicit_method;

            receiver_type = cblc_find_receiver_type(unit, receiver);
            implicit_method = receiver_type
                ? cblc_find_method_on_type(receiver_type, substituted.call_identifier) : NULL;
            if (implicit_method
                && ((substituted.type == CBLC_STATEMENT_CALL
                        && implicit_method->return_kind == CBLC_FUNCTION_RETURN_VOID)
                    || (substituted.type == CBLC_STATEMENT_CALL_ASSIGN
                        && implicit_method->return_kind != CBLC_FUNCTION_RETURN_VOID)))
            {
                if (substituted.type == CBLC_STATEMENT_CALL)
                {
                    substituted.type = CBLC_STATEMENT_METHOD_CALL;
                    ft_strlcpy(substituted.target, receiver->cobol_name,
                        sizeof(substituted.target));
                }
                else
                {
                    substituted.type = CBLC_STATEMENT_METHOD_CALL_ASSIGN;
                    ft_strlcpy(substituted.source, receiver->cobol_name,
                        sizeof(substituted.source));
                }
            }
        }
        if (cblc_normalize_method_string_subscripts(unit, receiver, substituted.target,
                rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_normalize_method_string_subscripts(unit, receiver, substituted.source,
                rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
        if (substituted.type == CBLC_STATEMENT_IF_BEGIN
            || substituted.type == CBLC_STATEMENT_IF_ELSE
            || substituted.type == CBLC_STATEMENT_IF_END)
        {
            char control_line[TRANSPILE_STATEMENT_TEXT_MAX];

            if (substituted.type == CBLC_STATEMENT_IF_BEGIN)
            {
                char condition[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_translate_expression_for_cobol(substituted.source,
                        condition, sizeof(condition)) != FT_SUCCESS
                    || std::snprintf(control_line, sizeof(control_line),
                        "           IF %s", condition) < 0)
                    return (FT_FAILURE);
                control_flow_depth += 1;
            }
            else if (substituted.type == CBLC_STATEMENT_IF_ELSE)
                ft_strlcpy(control_line, "           ELSE", sizeof(control_line));
            else
            {
                if (control_flow_depth > 0)
                    control_flow_depth -= 1;
                ft_strlcpy(control_line, "           END-IF", sizeof(control_line));
                if (control_flow_depth == 0)
                    ft_strlcat(control_line, ".", sizeof(control_line));
            }
            if (cobol_text_builder_append_line(builder, control_line) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (substituted.type == CBLC_STATEMENT_WHILE_BEGIN
            || substituted.type == CBLC_STATEMENT_WHILE_END)
        {
            char control_line[TRANSPILE_STATEMENT_TEXT_MAX];

            if (substituted.type == CBLC_STATEMENT_WHILE_BEGIN)
            {
                char condition[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_translate_expression_for_cobol(substituted.source,
                        condition, sizeof(condition)) != FT_SUCCESS
                    || std::snprintf(control_line, sizeof(control_line),
                        "           PERFORM UNTIL NOT (%s)", condition) < 0)
                    return (FT_FAILURE);
                control_flow_depth += 1;
            }
            else
            {
                if (control_flow_depth > 0)
                    control_flow_depth -= 1;
                ft_strlcpy(control_line, "           END-PERFORM", sizeof(control_line));
                if (control_flow_depth == 0)
                    ft_strlcat(control_line, ".", sizeof(control_line));
            }
            if (cobol_text_builder_append_line(builder, control_line) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (substituted.type == CBLC_STATEMENT_RETURN)
        {
            if (method->return_reference_kind != CBLC_REFERENCE_NONE)
            {
                if (cblc_emit_method_reference_result(unit, method, substituted.source,
                        assign_target, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_INT
            || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
            {
                char return_source[TRANSPILE_STATEMENT_TEXT_MAX];

                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (cblc_emit_prepare_pointer_element(unit, substituted.source, builder,
                        return_source, sizeof(return_source)) != FT_SUCCESS
                    && cblc_translate_expression_for_cobol(substituted.source,
                        return_source, sizeof(return_source)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                        assign_target, return_source) < 0)
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
        {
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_method_paragraph_name(const t_cblc_data_item *receiver,
    const t_cblc_method *method, char *buffer, size_t buffer_size)
{
    if (!receiver || !method || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "CBLC-METHOD-%s-%s", receiver->cobol_name,
            method->cobol_name) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_method_is_reusable(const t_cblc_method *method)
{
    if (!method || !method->has_definition
        || method->is_copy_assignment
        || (method->return_reference_kind == CBLC_REFERENCE_NONE
            && method->return_kind != CBLC_FUNCTION_RETURN_VOID
            && method->return_kind != CBLC_FUNCTION_RETURN_INT
            && method->return_kind != CBLC_FUNCTION_RETURN_DOUBLE
            && method->return_kind != CBLC_FUNCTION_RETURN_STRUCT))
        return (0);
    return (1);
}

static int cblc_method_result_name(const t_cblc_data_item *receiver,
    const t_cblc_method *method, char *buffer, size_t buffer_size)
{
    if (!receiver || !method || !buffer || buffer_size == 0
        || method->return_kind == CBLC_FUNCTION_RETURN_VOID)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "CBLC-RESULT-%s-%s", receiver->cobol_name,
            method->cobol_name) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_constructor_paragraph_name(const t_cblc_data_item *receiver,
    size_t constructor_index, char *buffer, size_t buffer_size)
{
    if (!receiver || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "CBLC-CONSTRUCTOR-%s-%zu", receiver->cobol_name,
            constructor_index) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_destructor_paragraph_name(const t_cblc_data_item *receiver,
    char *buffer, size_t buffer_size)
{
    if (!receiver || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "CBLC-DESTRUCTOR-%s", receiver->cobol_name) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static unsigned long long cblc_intrinsic_name_hash(const char *name)
{
    unsigned long long hash;

    hash = 1469598103934665603ULL;
    while (name && *name != '\0')
    {
        hash ^= static_cast<unsigned char>(*name);
        hash *= 1099511628211ULL;
        name += 1;
    }
    return (hash);
}

static int cblc_intrinsic_receiver_name(const t_cblc_data_item *receiver,
    char *buffer, size_t buffer_size)
{
    size_t name_length;
    int written;

    if (!receiver || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    name_length = std::strlen(receiver->cobol_name);
    if (name_length <= 27)
    {
        ft_strlcpy(buffer, receiver->cobol_name, buffer_size);
        return (std::strlen(buffer) == name_length ? FT_SUCCESS : FT_FAILURE);
    }
    written = std::snprintf(buffer, buffer_size, "CBLC-R-%016llX",
        cblc_intrinsic_name_hash(receiver->cobol_name));
    if (written < 0 || static_cast<size_t>(written) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_intrinsic_paragraph_name(const t_cblc_data_item *receiver,
    const char *intrinsic_name, char *buffer, size_t buffer_size)
{
    char receiver_name[TRANSPILE_IDENTIFIER_MAX];
    char normalized_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;
    int written;

    if (!receiver || !intrinsic_name || !buffer || buffer_size == 0
        || cblc_intrinsic_receiver_name(receiver, receiver_name,
            sizeof(receiver_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_strlcpy(normalized_name, intrinsic_name, sizeof(normalized_name));
    index = 0;
    while (normalized_name[index] != '\0')
    {
        if (normalized_name[index] >= 'a' && normalized_name[index] <= 'z')
            normalized_name[index] = static_cast<char>(normalized_name[index] - 'a' + 'A');
        else if (normalized_name[index] == '_')
            normalized_name[index] = '-';
        index += 1;
    }
    written = std::snprintf(buffer, buffer_size, "CBLC-INTRINSIC-%s-%s",
        receiver_name, normalized_name);
    if (written < 0 || static_cast<size_t>(written) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_intrinsic_result_name(const t_cblc_data_item *receiver,
    const char *intrinsic_name, char *buffer, size_t buffer_size)
{
    char receiver_name[TRANSPILE_IDENTIFIER_MAX];
    char normalized_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;
    int written;

    if (!receiver || !intrinsic_name || !buffer || buffer_size == 0
        || cblc_intrinsic_receiver_name(receiver, receiver_name,
            sizeof(receiver_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_strlcpy(normalized_name, intrinsic_name, sizeof(normalized_name));
    index = 0;
    while (normalized_name[index] != '\0')
    {
        if (normalized_name[index] >= 'a' && normalized_name[index] <= 'z')
            normalized_name[index] = static_cast<char>(normalized_name[index] - 'a' + 'A');
        index += 1;
    }
    written = std::snprintf(buffer, buffer_size, "CBLC-INTRINSIC-%s-%s-RESULT",
        receiver_name, normalized_name);
    if (written < 0 || static_cast<size_t>(written) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_intrinsic_argument_name(const t_cblc_data_item *receiver,
    size_t argument_index, const char *component, char *buffer, size_t buffer_size)
{
    char receiver_name[TRANSPILE_IDENTIFIER_MAX];
    int written;

    if (!receiver || !component || !buffer || buffer_size == 0
        || cblc_intrinsic_receiver_name(receiver, receiver_name,
            sizeof(receiver_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    written = std::snprintf(buffer, buffer_size, "CBLC-INTRINSIC-%s-ARG%zu-%s",
        receiver_name, argument_index, component);
    if (written < 0 || static_cast<size_t>(written) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_parameterized_string_intrinsic_call(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_intrinsic_entry *intrinsic, const t_cblc_statement *statement,
    const char *assign_target, t_cobol_text_builder *builder)
{
    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
    char argument_length[TRANSPILE_IDENTIFIER_MAX];
    char argument_buffer[TRANSPILE_IDENTIFIER_MAX];
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    char result_name[TRANSPILE_IDENTIFIER_MAX];
    char line[512];

    if (!unit || !receiver || !intrinsic || !statement || !builder
        || intrinsic->minimum_arguments != 1
        || statement->call_argument_count != 1
        || cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS
        || cblc_intrinsic_argument_name(receiver, 1, "LEN", argument_length,
            sizeof(argument_length)) != FT_SUCCESS
        || cblc_intrinsic_argument_name(receiver, 1, "BUF", argument_buffer,
            sizeof(argument_buffer)) != FT_SUCCESS
        || cblc_intrinsic_paragraph_name(receiver, intrinsic->name, paragraph_name,
            sizeof(paragraph_name)) != FT_SUCCESS
        || cblc_intrinsic_result_name(receiver, "parameterized", result_name,
            sizeof(result_name)) != FT_SUCCESS)
    {
        return (FT_FAILURE);
    }
    if (argument[0] == '"')
    {
        char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *literal_cursor;

        literal_cursor = argument;
        if (cblc_parse_string_literal(&literal_cursor, literal_text,
                sizeof(literal_text)) != FT_SUCCESS || *literal_cursor != '\0')
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s.",
                argument_buffer) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "           MOVE %s TO %s.", argument,
                argument_buffer) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "           COMPUTE %s = %zu.",
                argument_length, std::strlen(literal_text)) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        const t_cblc_data_item *argument_item;

        argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
        if (!argument_item)
            argument_item = cblc_find_data_item_by_cobol(unit, argument);
        if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE %s-BUF TO %s.",
                argument_item->cobol_name, argument_buffer) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s-LEN.",
                argument_length, argument_item->cobol_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (std::snprintf(line, sizeof(line), "           PERFORM %s.", paragraph_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (intrinsic->returns_value)
    {
        if (!assign_target || assign_target[0] == '\0'
            || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                assign_target, result_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (assign_target && assign_target[0] != '\0')
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_transfer(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, t_cobol_text_builder *builder);

static int cblc_constructor_has_exception_control(const t_cblc_constructor *constructor);
static int cblc_emit_exception_payload_copy(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *source_prefix,
    const char *target_prefix, t_cobol_text_builder *builder);

static int cblc_method_has_exception_control(const t_cblc_method *method)
{
    size_t index;

    if (!method)
        return (0);
    index = 0;
    while (index < method->statement_count)
    {
        t_cblc_statement_type type;

        type = method->statements[index].type;
        if (type == CBLC_STATEMENT_THROW
            || type == CBLC_STATEMENT_TRY_BEGIN
            || type == CBLC_STATEMENT_CATCH_BEGIN
            || type == CBLC_STATEMENT_CATCH_END
            || type == CBLC_STATEMENT_TRY_END
            || method->statements[index].exception_may_throw)
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_statement_method_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    const char *receiver_name;

    if (!unit || !statement
        || (statement->type != CBLC_STATEMENT_METHOD_CALL
            && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN))
        return (0);
    receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
        ? statement->target : statement->source;
    receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver)
        return (0);
    receiver_type = cblc_find_receiver_type(unit, receiver);
    if (!receiver_type)
        return (0);
    method = cblc_find_method_on_type(receiver_type, statement->call_identifier);
    return (method && cblc_method_has_exception_control(method));
}

static int cblc_statement_constructor_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement)
{
    const t_cblc_data_item *item;
    const t_cblc_struct_type *type;
    const t_cblc_constructor *constructor;

    if (!unit || !statement || statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT)
        return (0);
    item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!item || item->kind != CBLC_DATA_KIND_STRUCT || item->array_count > 0)
        return (0);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (0);
    constructor = cblc_find_constructor_for_arguments(unit, type,
        statement->call_arguments, statement->call_argument_count);
    return (constructor && cblc_constructor_has_exception_control(constructor));
}

static int cblc_build_exception_match_condition(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size)
{
    size_t index;
    int written;

    if (!unit || !statement || !buffer || buffer_size == 0
        || statement->exception_type_name[0] == '\0')
        return (FT_FAILURE);
    written = std::snprintf(buffer, buffer_size,
        "CBLC-EX-ACTIVE = 'Y' AND CBLC-EX-MATCHED = 'N' AND (CBLC-EX-TYPE-ID = %zu",
        statement->exception_type_id);
    if (written < 0 || static_cast<size_t>(written) >= buffer_size)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->struct_type_count)
    {
        const t_cblc_struct_type *candidate;

        candidate = &unit->struct_types[index];
        if (std::strcmp(candidate->source_name, statement->exception_type_name) != 0
            && cblc_exception_type_derives_from(unit, candidate->source_name,
                statement->exception_type_name, 0))
        {
            size_t remaining;

            remaining = buffer_size - static_cast<size_t>(written);
            written += std::snprintf(buffer + written, remaining,
                " OR CBLC-EX-TYPE-ID = %zu",
                cblc_exception_type_id(candidate->source_name));
            if (written < 0 || static_cast<size_t>(written) >= buffer_size)
                return (FT_FAILURE);
        }
        index += 1;
    }
    if (static_cast<size_t>(written) + 2 >= buffer_size)
        return (FT_FAILURE);
    buffer[written++] = ')';
    buffer[written] = '\0';
    return (FT_SUCCESS);
}

static int cblc_emit_exception_base_payload_views(
    const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_cobol_text_builder *builder)
{
    const t_cblc_struct_type *base_type;
    char base_payload_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !statement || !builder || statement->exception_is_catch_all
        || statement->exception_type_name[0] == '\0'
        || std::strncmp(statement->exception_type_name, "int", sizeof(statement->exception_type_name)) == 0
        || std::strncmp(statement->exception_type_name, "string", sizeof(statement->exception_type_name)) == 0)
        return (FT_SUCCESS);
    base_type = cblc_find_struct_type(unit, statement->exception_type_name);
    if (!base_type
        || cblc_exception_payload_name(statement->exception_type_name,
            base_payload_name, sizeof(base_payload_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->struct_type_count)
    {
        const t_cblc_struct_type *derived_type;

        derived_type = &unit->struct_types[index];
        if (std::strcmp(derived_type->source_name, base_type->source_name) != 0
            && cblc_exception_type_derives_from(unit, derived_type->source_name,
                base_type->source_name, 0))
        {
            char derived_payload_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_exception_payload_name(derived_type->source_name,
                    derived_payload_name, sizeof(derived_payload_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line),
                    "               IF CBLC-EX-TYPE-ID = %zu",
                    cblc_exception_type_id(derived_type->source_name)) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || cblc_emit_exception_payload_copy(unit, base_type,
                    derived_payload_name, base_payload_name, builder) != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "               END-IF") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_string_class_helper_call(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, const t_cblc_data_item *receiver,
    const t_cblc_method *method, const char *assign_target,
    t_cobol_text_builder *builder)
{
    char using_clause[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t index;

    if (!unit || !statement || !receiver || !method || !builder
        || method->implementation_program_name[0] == '\0'
        || statement->call_argument_count != method->parameter_count)
        return (FT_FAILURE);
    using_clause[0] = '\0';
    ft_strlcat(using_clause, "BY REFERENCE ", sizeof(using_clause));
    ft_strlcat(using_clause, receiver->cobol_name, sizeof(using_clause));
    index = 0;
    while (index < method->parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_data_item *argument_item;

        if (cblc_extract_call_argument(statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (method->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
        {
            argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            ft_strlcat(using_clause, " BY REFERENCE ", sizeof(using_clause));
            ft_strlcat(using_clause, argument_item->cobol_name, sizeof(using_clause));
        }
        else if (cblc_argument_is_numeric_literal(argument))
        {
            ft_strlcat(using_clause, " BY VALUE ", sizeof(using_clause));
            ft_strlcat(using_clause, argument, sizeof(using_clause));
        }
        else
        {
            argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (!argument_item)
                return (FT_FAILURE);
            ft_strlcat(using_clause, " BY REFERENCE ", sizeof(using_clause));
            ft_strlcat(using_clause, argument_item->cobol_name, sizeof(using_clause));
        }
        index += 1;
    }
    if (method->return_kind != CBLC_FUNCTION_RETURN_VOID)
    {
        if (!assign_target || assign_target[0] == '\0')
            return (FT_FAILURE);
        ft_strlcat(using_clause, " BY REFERENCE ", sizeof(using_clause));
        ft_strlcat(using_clause, assign_target, sizeof(using_clause));
    }
    if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s.",
            method->implementation_program_name, using_clause) < 0)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line));
}

static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    const t_cblc_intrinsic_entry *intrinsic;
    const char *receiver_name;
    const char *assign_target;
    t_cblc_data_item prepared_receiver;
    char prepared_receiver_name[TRANSPILE_STATEMENT_TEXT_MAX];
    int prepared_indexed_string;

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
    prepared_indexed_string = 0;
    receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver)
    {
        size_t alias_index;

        alias_index = 0;
        while (alias_index < unit->data_count)
        {
            if (std::strcmp(unit->data_items[alias_index].cobol_name, receiver_name) == 0)
            {
                if (unit->data_items[alias_index].is_alias)
                    receiver = &unit->data_items[alias_index];
                break ;
            }
            alias_index += 1;
        }
        if (!receiver)
            return (FT_FAILURE);
    }
    if (receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER
        && std::strcmp(receiver->struct_type_name, "string") == 0
        && std::strchr(receiver->source_name, '['))
    {
        std::memset(&prepared_receiver, 0, sizeof(prepared_receiver));
        if (cblc_emit_prepare_struct_pointer_value(unit, receiver->cobol_name, builder,
                prepared_receiver_name, sizeof(prepared_receiver_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        prepared_receiver = *receiver;
        prepared_receiver.kind = CBLC_DATA_KIND_STRING;
        prepared_receiver.is_alias = 1;
        prepared_receiver.struct_type_name[0] = '\0';
        ft_strlcpy(prepared_receiver.declared_type_name, "string",
            sizeof(prepared_receiver.declared_type_name));
        ft_strlcpy(prepared_receiver.cobol_name, prepared_receiver_name,
            sizeof(prepared_receiver.cobol_name));
        receiver = &prepared_receiver;
        prepared_indexed_string = 1;
    }
    if (prepared_indexed_string
        && std::strcmp(statement->call_identifier, "len") == 0)
    {
        if (!assign_target || assign_target[0] == '\0')
            return (FT_FAILURE);
        return (std::snprintf(prepared_receiver_name, sizeof(prepared_receiver_name),
                    "           COMPUTE %s = %s-LEN.", assign_target,
                    receiver->cobol_name) < 0
            ? FT_FAILURE : cobol_text_builder_append_line(builder, prepared_receiver_name));
    }
    if (prepared_indexed_string
        && std::strcmp(statement->call_identifier, "clear") == 0)
    {
        if (std::snprintf(prepared_receiver_name, sizeof(prepared_receiver_name),
                "           MOVE SPACES TO %s-BUF.", receiver->cobol_name) < 0
            || cobol_text_builder_append_line(builder, prepared_receiver_name) != FT_SUCCESS
            || std::snprintf(prepared_receiver_name, sizeof(prepared_receiver_name),
                "           COMPUTE %s-LEN = 0.", receiver->cobol_name) < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, prepared_receiver_name));
    }
    receiver_type = cblc_find_receiver_type(unit, receiver);
    if (!receiver_type)
    {
        return (FT_FAILURE);
    }
    method = cblc_find_method_on_type(receiver_type, statement->call_identifier);
    if (!method)
    {
        return (FT_FAILURE);
    }
    if (method->implementation_program_name[0] != '\0')
        return (cblc_emit_string_class_helper_call(unit, statement, receiver,
            method, assign_target, builder));
    if (receiver_type->is_builtin
        && std::strncmp(receiver_type->source_name, "string", TRANSPILE_IDENTIFIER_MAX) == 0)
    {
        char line[512];

        intrinsic = cblc_intrinsic_lookup(statement->call_identifier);
        if (!intrinsic || !cblc_intrinsic_accepts_argument_count(intrinsic,
                statement->call_argument_count))
            return (FT_FAILURE);
        (void)intrinsic;

        if (receiver->array_count == 0
            && (std::strncmp(statement->call_identifier, "len",
                    sizeof(statement->call_identifier)) == 0
                || std::strncmp(statement->call_identifier, "capacity",
                    sizeof(statement->call_identifier)) == 0
                || std::strncmp(statement->call_identifier, "empty",
                    sizeof(statement->call_identifier)) == 0
                || std::strncmp(statement->call_identifier, "clear",
                    sizeof(statement->call_identifier)) == 0))
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char result_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_intrinsic_paragraph_name(receiver, statement->call_identifier,
                    paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "           PERFORM %s.",
                    paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::strncmp(statement->call_identifier, "clear",
                    sizeof(statement->call_identifier)) == 0)
                return (FT_SUCCESS);
            if (!assign_target || assign_target[0] == '\0'
                || cblc_intrinsic_result_name(receiver, statement->call_identifier,
                    result_name, sizeof(result_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    assign_target, result_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }

        if (intrinsic->minimum_arguments > 0)
        {
            int intrinsic_status;

            intrinsic_status = cblc_emit_parameterized_string_intrinsic_call(unit, receiver,
                intrinsic, statement, assign_target, builder);
            return (intrinsic_status);
        }

        if (std::strncmp(statement->call_identifier, "append",
                sizeof(statement->call_identifier)) == 0)
        {
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
                        "           CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, receiver->cobol_name, statement->source) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, receiver->cobol_name, statement->source) < 0)
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

                argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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

                argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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

                argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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

                argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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
    if (cblc_method_is_reusable(method) && receiver->kind == CBLC_DATA_KIND_STRUCT)
    {
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
        char result_name[TRANSPILE_IDENTIFIER_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_method_paragraph_name(receiver, method, paragraph_name,
                sizeof(paragraph_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (method->parameter_count > 0
            && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count, method->parameters,
                method->parameter_count, builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           PERFORM %s.", paragraph_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_method_has_exception_control(method) && caller
            && cblc_emit_exception_transfer(unit, caller, statement, statement_index,
                builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (method->return_kind == CBLC_FUNCTION_RETURN_VOID)
            return (FT_SUCCESS);
        if (!assign_target || assign_target[0] == '\0'
            || cblc_method_result_name(receiver, method, result_name,
                sizeof(result_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (method->return_reference_kind != CBLC_REFERENCE_NONE)
        {
            if (cblc_emit_method_reference_result(unit, method, result_name, assign_target,
                    builder) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (method->return_kind == CBLC_FUNCTION_RETURN_INT
            || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
        {
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    assign_target, result_name) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                result_name, assign_target) < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (method->parameter_count > 0)
    {
        if (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count, method->parameters,
                method->parameter_count, builder) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    {
        int method_status;

        method_status = cblc_emit_method_body(unit, receiver, method, assign_target, builder);
        if (method_status != FT_SUCCESS)
            return (method_status);
        return (method_status);
    }
}

static int cblc_emit_copy_assignment(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder)
{
    const t_cblc_data_item *target_item;
    const t_cblc_data_item *source_item;
    const t_cblc_struct_type *type;
    const t_cblc_method *method;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    target_item = cblc_find_data_item_by_cobol(unit, statement->target);
    source_item = cblc_find_data_item_by_cobol(unit, statement->source);
    if (!target_item)
        target_item = cblc_find_data_item_for_emit_by_source(unit, statement->target);
    if (!source_item)
        source_item = cblc_find_data_item_for_emit_by_source(unit, statement->source);
    if (!target_item || !source_item
        || target_item->kind != CBLC_DATA_KIND_STRUCT
        || source_item->kind != CBLC_DATA_KIND_STRUCT
        || std::strncmp(target_item->struct_type_name, source_item->struct_type_name,
            sizeof(target_item->struct_type_name)) != 0)
    {
        return (FT_FAILURE);
    }
    type = cblc_find_struct_type(unit, target_item->struct_type_name);
    if (!type || !type->has_copy_assignment)
    {
        return (FT_FAILURE);
    }
    method = cblc_find_method_on_type(type, "operator=");
    if (!method || !method->has_definition || method->return_kind != CBLC_FUNCTION_RETURN_VOID
        || method->parameter_count != 1)
    {
        return (FT_FAILURE);
    }
    if (cblc_emit_parameter_argument_moves(unit, source_item->source_name, 1,
            method->parameters, method->parameter_count, builder) != FT_SUCCESS)
    {
        return (FT_FAILURE);
    }
    {
        int method_status;

        method_status = cblc_emit_method_body(unit, target_item, method, NULL, builder);
        return (method_status);
    }
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

static int cblc_constructor_has_exception_control(const t_cblc_constructor *constructor)
{
    size_t index;

    if (!constructor)
        return (0);
    index = 0;
    while (index < constructor->statement_count)
    {
        t_cblc_statement_type type;

        type = constructor->statements[index].type;
        if (type == CBLC_STATEMENT_THROW
            || type == CBLC_STATEMENT_TRY_BEGIN
            || type == CBLC_STATEMENT_CATCH_BEGIN
            || constructor->statements[index].exception_may_throw)
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_emit_function(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, t_cobol_text_builder *builder,
    int append_stop_run);

static int cblc_emit_constructor_exception_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_constructor *constructor,
    size_t constructor_index, t_cobol_text_builder *builder)
{
    t_cblc_function exception_function;
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !receiver || !constructor || !builder
        || !cblc_constructor_has_exception_control(constructor)
        || cblc_constructor_paragraph_name(receiver, constructor_index,
            paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&exception_function, 0, sizeof(exception_function));
    ft_strlcpy(exception_function.source_name, receiver->source_name,
        sizeof(exception_function.source_name));
    ft_strlcpy(exception_function.cobol_name, paragraph_name,
        sizeof(exception_function.cobol_name));
    exception_function.return_kind = CBLC_FUNCTION_RETURN_VOID;
    exception_function.statement_capacity = constructor->statement_count;
    exception_function.statement_count = constructor->statement_count;
    exception_function.statements = static_cast<t_cblc_statement *>(cma_calloc(
        constructor->statement_count == 0 ? 1 : constructor->statement_count,
        sizeof(*exception_function.statements)));
    if (!exception_function.statements)
        return (FT_FAILURE);
    index = 0;
    while (index < constructor->statement_count)
    {
        exception_function.statements[index] = constructor->statements[index];
        if (cblc_replace_this_prefix(constructor->statements[index].target,
                receiver->cobol_name, exception_function.statements[index].target,
                sizeof(exception_function.statements[index].target)) != FT_SUCCESS
            || cblc_replace_this_prefix(constructor->statements[index].source,
                receiver->cobol_name, exception_function.statements[index].source,
                sizeof(exception_function.statements[index].source)) != FT_SUCCESS)
        {
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        {
            size_t cleanup_index;

            cleanup_index = 0;
            while (cleanup_index < exception_function.statements[index]
                .exception_cleanup_count)
            {
                char substituted_cleanup[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_replace_this_prefix(
                        constructor->statements[index]
                            .exception_cleanup_targets[cleanup_index],
                        receiver->cobol_name, substituted_cleanup,
                        sizeof(substituted_cleanup)) != FT_SUCCESS)
                {
                    cma_free(exception_function.statements);
                    return (FT_FAILURE);
                }
                ft_strlcpy(exception_function.statements[index]
                        .exception_cleanup_targets[cleanup_index],
                    substituted_cleanup,
                    sizeof(exception_function.statements[index]
                        .exception_cleanup_targets[cleanup_index]));
                cleanup_index += 1;
            }
        }
        index += 1;
    }
    if (cblc_emit_function(unit, &exception_function, builder, 0) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
    {
        cma_free(exception_function.statements);
        return (FT_FAILURE);
    }
    cma_free(exception_function.statements);
    return (FT_SUCCESS);
}

static int cblc_emit_struct_pointer_lifecycle(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, const t_cblc_statement *statement,
    t_cobol_text_builder *builder)
{
    const t_cblc_struct_type *type;
    const t_cblc_constructor *constructor;
    char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t index;

    if (!unit || !item || !statement || !builder
        || item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type || cblc_build_struct_pointer_based_name(item->cobol_name, based_name,
            sizeof(based_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO %s.",
            based_name, item->cobol_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_CALL_ASSIGN)
    {
        constructor = cblc_find_constructor_for_arguments(unit, type,
            statement->call_arguments, statement->call_argument_count);
        if (!constructor)
            return (FT_FAILURE);
        if (constructor->parameter_count > 0
            && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count,
                constructor->parameters, constructor->parameter_count, builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (constructor->statement_count == 0)
        {
            if (std::snprintf(line, sizeof(line), "           INITIALIZE %s.", based_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        index = 0;
        while (index < constructor->statement_count)
        {
            if (cblc_emit_substituted_statement(unit, NULL, &constructor->statements[index],
                    based_name, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    if (type->destructor_statement_count == 0)
        return (FT_SUCCESS);
    index = 0;
    while (index < type->destructor_statement_count)
    {
        if (cblc_emit_substituted_statement(unit, NULL,
                &type->destructor_statements[index], based_name, builder) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index)
{
    const t_cblc_constructor *constructor;
    const t_cblc_data_item *item;
    const t_cblc_struct_type *type;
    const t_cblc_statement *body;
    char line[256];
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
        source_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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
    if (item->array_count > 0)
    {
        if (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && statement->type != CBLC_STATEMENT_DESTRUCT)
            return (FT_FAILURE);
        if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && statement->call_argument_count > 0)
        {
            const t_cblc_constructor *array_constructor;
            size_t element_index;

            array_constructor = cblc_find_constructor_for_arguments(unit, type,
                statement->call_arguments, statement->call_argument_count);
            if (!array_constructor
                || cblc_emit_struct_array_lifecycle(unit, type, item->cobol_name,
                    item->array_count, 0, builder, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            element_index = 1;
            while (element_index <= item->array_count)
            {
                size_t constructor_statement_index;

                if (array_constructor->parameter_count > 0
                    && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                        statement->call_argument_count, array_constructor->parameters,
                        array_constructor->parameter_count, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                constructor_statement_index = 0;
                while (constructor_statement_index < array_constructor->statement_count)
                {
                    if (cblc_emit_array_element_constructor_statement(unit,
                            &array_constructor->statements[constructor_statement_index],
                            item->cobol_name, element_index, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    constructor_statement_index += 1;
                }
                element_index += 1;
            }
            return (FT_SUCCESS);
        }
        return (cblc_emit_struct_array_lifecycle(unit, type, item->cobol_name,
            item->array_count, 0, builder,
            statement->type == CBLC_STATEMENT_DESTRUCT));
    }
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
        if (constructor->is_copy_constructor)
        {
            char copy_argument[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_data_item *copy_source_item;

            if (statement->call_argument_count != 1
                || cblc_extract_call_argument(statement, 0, copy_argument,
                    sizeof(copy_argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            copy_source_item = cblc_find_data_item_for_emit_by_source(unit, copy_argument);
            if (!copy_source_item || copy_source_item->kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
            if (constructor->parameter_count > 0
                && cblc_emit_parameter_argument_moves(unit, copy_source_item->source_name, 1,
                    constructor->parameters,
                    constructor->parameter_count, builder) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            index = 0;
            while (index < constructor->statement_count)
            {
                if (cblc_emit_substituted_statement(unit, NULL,
                        &constructor->statements[index], item->cobol_name, builder) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
                index += 1;
            }
            return (FT_SUCCESS);
        }
        if (constructor->statement_count == 0)
            return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder, 0));
        if (item->kind == CBLC_DATA_KIND_STRUCT)
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_constructor_paragraph_name(item,
                    static_cast<size_t>(constructor - type->constructors), paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (constructor->parameter_count > 0
                && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                    statement->call_argument_count, constructor->parameters,
                    constructor->parameter_count, builder) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            if (std::snprintf(line, sizeof(line), "           PERFORM %s.", paragraph_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (caller && cblc_constructor_has_exception_control(constructor))
            {
                t_cblc_statement transfer_statement;

                transfer_statement = *statement;
                transfer_statement.exception_may_throw = 1;
                if (cblc_emit_exception_transfer(unit, caller, &transfer_statement,
                        statement_index, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            return (FT_SUCCESS);
        }
        if (constructor->parameter_count > 0)
        {
            if (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                    statement->call_argument_count, constructor->parameters,
                    constructor->parameter_count, builder) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
        }
        body = constructor->statements;
        count = constructor->statement_count;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement(unit, NULL, &body[index], item->cobol_name, builder)
                != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            index += 1;
        }
        return (FT_SUCCESS);
    }
    if (statement->type == CBLC_STATEMENT_DESTRUCT && type->destructor_statement_count > 0)
    {
        if (item->kind == CBLC_DATA_KIND_STRUCT)
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_destructor_paragraph_name(item, paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           PERFORM %s.", paragraph_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
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
    {
        int lifecycle_status;

        lifecycle_status = cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder,
            statement->type == CBLC_STATEMENT_DESTRUCT);
        return (lifecycle_status);
    }
}

static int cblc_unit_has_exception_context(const t_cblc_translation_unit *unit);

static int cblc_function_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, size_t depth)
{
    size_t index;

    if (!unit || !function || depth > unit->function_count)
        return (0);
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_THROW)
            return (1);
        if (cblc_statement_method_may_throw(unit, &function->statements[index])
            || cblc_statement_constructor_may_throw(unit,
                &function->statements[index]))
            return (1);
        index += 1;
    }
    index = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement;
        const t_cblc_function *target_function;

        statement = &function->statements[index];
        if (statement->type != CBLC_STATEMENT_CALL
            && statement->type != CBLC_STATEMENT_CALL_ASSIGN)
        {
            index += 1;
            continue ;
        }
        if (statement->call_is_external)
        {
            target_function = cblc_find_function_in_unit(unit, statement->call_identifier);
            if (target_function && target_function->may_throw)
                return (1);
            if (!target_function && cblc_unit_has_exception_context(unit))
                return (1);
            index += 1;
            continue ;
        }
        target_function = cblc_find_function_in_unit(unit, statement->call_identifier);
        if (target_function && target_function != function
            && cblc_function_may_throw(unit, target_function, depth + 1))
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_statement_call_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement)
{
    const t_cblc_function *target_function;

    if (!unit || !statement
        || (statement->type != CBLC_STATEMENT_CALL
            && statement->type != CBLC_STATEMENT_CALL_ASSIGN))
        return (0);
    target_function = cblc_find_function_in_unit(unit, statement->call_identifier);
    if (!target_function)
        return (statement->exception_may_throw != 0);
    if (statement->call_is_external)
        return (target_function->may_throw != 0);
    return (cblc_function_may_throw(unit, target_function, 0));
}

static int cblc_unit_has_exception_context(const t_cblc_translation_unit *unit)
{
    size_t function_index;

    if (!unit)
        return (0);
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

            statement = &function->statements[statement_index];
            if (statement->type == CBLC_STATEMENT_TRY_BEGIN
                || statement->type == CBLC_STATEMENT_CATCH_BEGIN
                || statement->type == CBLC_STATEMENT_THROW
                || statement->exception_may_throw
                || statement->type == CBLC_STATEMENT_FILE_OPEN
                || statement->type == CBLC_STATEMENT_FILE_CLOSE
                || statement->type == CBLC_STATEMENT_FILE_READ
                || statement->type == CBLC_STATEMENT_FILE_WRITE
                || cblc_statement_method_may_throw(unit, statement)
                || cblc_statement_constructor_may_throw(unit, statement))
                return (1);
            statement_index += 1;
        }
        function_index += 1;
    }
    return (0);
}

static int cblc_find_enclosing_exception_region(const t_cblc_function *function,
    size_t statement_index, size_t *region_id)
{
    size_t region_stack[CBLC_EXCEPTION_REGION_MAX];
    size_t depth;
    size_t index;

    if (!function || !region_id || statement_index > function->statement_count)
        return (FT_FAILURE);
    depth = 0;
    index = 0;
    while (index < statement_index)
    {
        const t_cblc_statement *statement;

        statement = &function->statements[index];
        if (statement->type == CBLC_STATEMENT_TRY_BEGIN)
        {
            if (depth >= CBLC_EXCEPTION_REGION_MAX)
                return (FT_FAILURE);
            region_stack[depth] = statement->exception_region_id;
            depth += 1;
        }
        else if (statement->type == CBLC_STATEMENT_TRY_END && depth > 0)
            depth -= 1;
        index += 1;
    }
    if (depth == 0)
        return (FT_FAILURE);
    *region_id = region_stack[depth - 1];
    return (FT_SUCCESS);
}

static int cblc_build_exception_cleanup_paragraph_name(const t_cblc_function *function,
    size_t statement_index, char *buffer, size_t buffer_size)
{
    if (!function || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "CBLC-EX-CLEANUP-%s-%zu",
            function->cobol_name, statement_index) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_transfer(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    char cleanup_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !function || !statement || !builder)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder,
            "           IF CBLC-EX-ACTIVE = 'Y'") != FT_SUCCESS)
        return (FT_FAILURE);
    if (statement->exception_cleanup_count > 0)
    {
        if (cblc_build_exception_cleanup_paragraph_name(function, statement_index,
                cleanup_name, sizeof(cleanup_name)) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "               PERFORM %s",
                cleanup_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!statement->exception_has_region)
    {
        if (std::strncmp(function->source_name, "main", sizeof(function->source_name)) == 0)
        {
            if (cobol_text_builder_append_line(builder, "               STOP RUN") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (cobol_text_builder_append_line(builder, "               GOBACK") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (statement->exception_from_catch)
    {
        if ((statement->exception_to_outer_region
                ? std::snprintf(line, sizeof(line),
                    "               GO TO CBLC-TRY-%s-%zu-CATCH-0",
                    function->cobol_name, statement->exception_region_id)
                : std::snprintf(line, sizeof(line),
                    "               GO TO CBLC-TRY-%s-%zu-DONE",
                    function->cobol_name, statement->exception_region_id)) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(line, sizeof(line), "               GO TO CBLC-TRY-%s-%zu-CATCH-0",
            function->cobol_name, statement->exception_region_id) < 0)
        return (FT_FAILURE);
    if (statement->exception_has_region
        && cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder,
        statement->exception_from_catch ? "           END-IF" : "           END-IF."));
}

static int cblc_emit_file_status_exception(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, const char *status_name, int allow_eof,
    t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement transfer_statement;
    size_t string_exception_type_id;

    if (!unit || !function || !statement || !status_name || !builder)
        return (FT_FAILURE);
    transfer_statement = *statement;
    string_exception_type_id = cblc_exception_type_id("string");
    if (allow_eof)
    {
        if (std::snprintf(line, sizeof(line),
                "           IF %s NOT = \"00\" AND %s NOT = \"10\"",
                status_name, status_name) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(line, sizeof(line), "           IF %s NOT = \"00\"",
            status_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               IF CBLC-EX-ACTIVE = 'Y' OR CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
            string_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
            statement->exception_source_line) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
            statement->exception_source_column) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   PERFORM CBLC-TERMINATE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %s TO CBLC-EX-STRING-BUF",
            status_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 2 TO CBLC-EX-STRING-LEN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 2 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %zu TO CBLC-EX-TYPE-ID",
            string_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "               MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
            string_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 'Y' TO CBLC-EX-ACTIVE") != FT_SUCCESS
        || cblc_emit_exception_transfer(unit, function, &transfer_statement, statement_index,
            builder) != FT_SUCCESS
        || (statement->exception_from_catch
            && cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS))
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_raw_call_exception(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, const char *call_line, t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement transfer_statement;
    size_t region_id;
    size_t call_exception_type_id;

    if (!unit || !function || !statement || !call_line || !builder)
        return (FT_FAILURE);
    transfer_statement = *statement;
    if (cblc_find_enclosing_exception_region(function, statement_index, &region_id)
        == FT_SUCCESS)
    {
        transfer_statement.exception_has_region = 1;
        transfer_statement.exception_region_id = region_id;
    }
    else
        transfer_statement.exception_has_region = 0;
    call_exception_type_id = cblc_exception_type_id("int");
    if (cobol_text_builder_append_line(builder, call_line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               ON EXCEPTION") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   IF CBLC-EX-ACTIVE = 'Y' OR CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                       MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
            call_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                       MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
            statement->exception_source_line) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                       MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
            statement->exception_source_column) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                       PERFORM CBLC-TERMINATE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   MOVE 1 TO CBLC-EX-INT-PAYLOAD") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   MOVE 4 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   MOVE %zu TO CBLC-EX-TYPE-ID",
            call_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
            call_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   MOVE 'Y' TO CBLC-EX-ACTIVE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           END-CALL.") != FT_SUCCESS
        || cblc_emit_exception_transfer(unit, function, &transfer_statement, statement_index,
            builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_allocation_exception(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, const char *allocation_line, const char *pointer_name,
    t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement transfer_statement;
    size_t region_id;
    size_t allocation_exception_type_id;

    if (!unit || !function || !statement || !allocation_line || !pointer_name || !builder
        || cblc_find_enclosing_exception_region(function, statement_index, &region_id)
            != FT_SUCCESS)
        return (FT_FAILURE);
    transfer_statement = *statement;
    transfer_statement.exception_has_region = 1;
    transfer_statement.exception_region_id = region_id;
    allocation_exception_type_id = cblc_exception_type_id("int");
    if (cobol_text_builder_append_line(builder, allocation_line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "           IF %s = NULL", pointer_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               IF CBLC-EX-ACTIVE = 'Y' OR CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
            allocation_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
            statement->exception_source_line) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
            statement->exception_source_column) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                   PERFORM CBLC-TERMINATE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 1 TO CBLC-EX-INT-PAYLOAD") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 4 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %zu TO CBLC-EX-TYPE-ID",
            allocation_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "               MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
            allocation_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 'Y' TO CBLC-EX-ACTIVE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS
        || cblc_emit_exception_transfer(unit, function, &transfer_statement, statement_index,
            builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_cleanup_paragraphs(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, t_cobol_text_builder *builder)
{
    size_t index;

    if (!unit || !function || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement;
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t cleanup_index;

        statement = &function->statements[index];
        if (statement->type != CBLC_STATEMENT_THROW
            && !cblc_statement_call_may_throw(unit, statement))
        {
            index += 1;
            continue ;
        }
        if (statement->exception_cleanup_count == 0)
        {
            index += 1;
            continue ;
        }
        if (cblc_build_exception_cleanup_paragraph_name(function, index,
                paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder,
                "           MOVE 'Y' TO CBLC-EX-CLEANING.") != FT_SUCCESS)
            return (FT_FAILURE);
        cleanup_index = 0;
        while (cleanup_index < statement->exception_cleanup_count)
        {
            t_cblc_statement cleanup_statement;

            std::memset(&cleanup_statement, 0, sizeof(cleanup_statement));
            cleanup_statement.type = CBLC_STATEMENT_DESTRUCT;
            ft_strlcpy(cleanup_statement.target,
                statement->exception_cleanup_targets[cleanup_index],
                sizeof(cleanup_statement.target));
            if (cblc_emit_lifecycle_statement(unit, &cleanup_statement, builder,
                    NULL, static_cast<size_t>(-1))
                != FT_SUCCESS)
                return (FT_FAILURE);
            cleanup_index += 1;
        }
        if (cobol_text_builder_append_line(builder,
                "           MOVE 'N' TO CBLC-EX-CLEANING.") != FT_SUCCESS
            || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
            || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_exception_dynamic_string_copy(const char *source_prefix,
    const char *target_prefix, t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!source_prefix || !target_prefix || !builder
        || std::snprintf(line, sizeof(line),
            "           IF %s-PTR NOT = NULL", source_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s-PTR = NULL OR %s-CAP < %s-CAP",
            target_prefix, target_prefix, source_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   IF %s-PTR NOT = NULL",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                       FREE %s-PTR",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   ALLOCATE %s-CAP CHARACTERS RETURNING %s-PTR",
            source_prefix, target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   IF %s-PTR = NULL",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                       MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
            cblc_exception_type_id("int")) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "                       PERFORM CBLC-TERMINATE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   MOVE %s-CAP TO %s-CAP",
            source_prefix, target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s-BUF TO %s-PTR",
            source_prefix, source_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s-BUF TO %s-PTR",
            target_prefix, target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s-LEN > 0",
            source_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   MOVE %s-BUF(1:%s-LEN) TO %s-BUF(1:%s-LEN)",
            source_prefix, source_prefix, target_prefix, source_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %s-LEN TO %s-LEN",
            source_prefix, target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           ELSE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s-PTR NOT = NULL",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   FREE %s-PTR",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET %s-PTR TO NULL",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE 0 TO %s-CAP",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE 0 TO %s-LEN",
            target_prefix) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_payload_copy(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *source_prefix, const char *target_prefix,
    t_cobol_text_builder *builder)
{
    char source_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t index;

    if (!unit || !type || !source_prefix || !target_prefix || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;

        field = &type->fields[index];
        if (field->array_count > 0)
            return (FT_FAILURE);
        if (std::snprintf(source_name, sizeof(source_name), "%s-%s", source_prefix,
                field->cobol_name) < 0
            || std::snprintf(target_name, sizeof(target_name), "%s-%s", target_prefix,
                field->cobol_name) < 0)
            return (FT_FAILURE);
        if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *nested_type;

            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!nested_type || cblc_emit_exception_payload_copy(unit, nested_type,
                    source_name, target_name, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (field->kind == CBLC_DATA_KIND_STRING)
        {
            if (cblc_string_field_uses_dynamic_storage(field))
            {
                if (cblc_emit_exception_dynamic_string_copy(source_name, target_name,
                        builder) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           MOVE %s-BUF TO %s-BUF",
                    source_name, target_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "           MOVE %s-LEN TO %s-LEN",
                    source_name, target_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (field->kind == CBLC_DATA_KIND_INT || field->kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    source_name, target_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_exception_context_clear(t_cobol_text_builder *builder)
{
    if (!builder
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-ACTIVE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-MATCHED") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-TYPE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-DYNAMIC-TYPE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SOURCE-FILE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SOURCE-LINE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SOURCE-COLUMN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SECONDARY-TYPE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SECONDARY-SOURCE-LINE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SECONDARY-SOURCE-COLUMN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-INT-PAYLOAD") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-STRING-LEN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE SPACES TO CBLC-EX-STRING-BUF") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-RAISING") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-CLEANING") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_payload_cleanup(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t index;

    if (!unit || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->data_count)
    {
        const t_cblc_data_item *item;
        const t_cblc_struct_type *type;

        item = &unit->data_items[index];
        if (std::strncmp(item->source_name, "__cblc_exception_payload_",
                std::strlen("__cblc_exception_payload_")) != 0
            || item->kind != CBLC_DATA_KIND_STRUCT)
        {
            index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, item->struct_type_name);
        if (!type || cblc_emit_lifecycle_recursive(unit, type, item->cobol_name,
                builder, 1) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_exception_catch_cleanup(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t function_index;

    if (!unit || !builder)
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

            statement = &function->statements[statement_index];
            if (statement->type == CBLC_STATEMENT_CATCH_BEGIN
                && statement->target[0] != '\0'
                && !statement->exception_catch_is_reference)
            {
                const t_cblc_data_item *item;
                const t_cblc_struct_type *type;

                item = cblc_find_data_item_for_emit_by_source(unit, statement->target);
                type = item ? cblc_find_struct_type(unit, item->struct_type_name) : NULL;
                if (item && item->kind == CBLC_DATA_KIND_STRUCT && type
                    && cblc_type_requires_destruct(unit, type)
                    && cblc_emit_lifecycle_recursive(unit, type, item->cobol_name,
                        builder, 1) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            statement_index += 1;
        }
        function_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_exception_payload_cleanup_paragraph(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    if (!unit || !builder)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "       CBLC-EX-CLEAR-PAYLOAD.") != FT_SUCCESS
        || cblc_emit_exception_payload_cleanup(unit, builder) != FT_SUCCESS
        || cblc_emit_exception_catch_cleanup(unit, builder) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_function(const t_cblc_translation_unit *unit, const t_cblc_function *function,
    t_cobol_text_builder *builder, int append_stop_run)
{
    char line[256];
    size_t index;
    int exception_handler_depth;
    int control_flow_depth;
    int emit_return_exit;

    if (!unit || !function || !builder)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "       %s.", function->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    exception_handler_depth = 0;
    control_flow_depth = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement;
        int skip_append;

        statement = &function->statements[index];
        skip_append = 0;
        emit_return_exit = 0;
        if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_emit_copy_assignment(unit, statement, builder) == FT_SUCCESS)
            {
                skip_append = 1;
            }
            else if (cblc_emit_pointer_assignment_statement(unit, statement, builder,
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
            if (unit->is_native_standard_library
                && (std::strstr(source_text, "-32") || std::strstr(source_text, "- 32")
                    || std::strstr(source_text, "+32") || std::strstr(source_text, "+ 32")))
            {
                const char *adjustment;
                char *operator_start;

                operator_start = std::strstr(source_text, " - ");
                adjustment = "-";
                if (!operator_start)
                    operator_start = std::strstr(source_text, "-32");
                if (!operator_start)
                    operator_start = std::strstr(source_text, "- 32");
                if (!operator_start)
                {
                    adjustment = "+";
                    operator_start = std::strstr(source_text, " + 32");
                    if (!operator_start)
                        operator_start = std::strstr(source_text, "+32");
                    if (!operator_start)
                        operator_start = std::strstr(source_text, "+ 32");
                    if (!operator_start)
                        operator_start = std::strstr(source_text, " + ");
                }
                if (operator_start)
                    *operator_start = '\0';
                if (std::strlen(source_text) > 0
                    && source_text[std::strlen(source_text) - 1] == '.')
                    source_text[std::strlen(source_text) - 1] = '\0';
                if (std::snprintf(line, sizeof(line),
                        "           MOVE FUNCTION CHAR(FUNCTION ORD(%s) %s 32) TO %s",
                        source_text, adjustment, target_text) < 0)
                    return (FT_FAILURE);
                if (!builder->free_format && std::strlen(line) > 72)
                {
                    char *to_clause;

                    to_clause = std::strstr(line, "TO ");
                    if (to_clause)
                    {
                        if (cobol_text_builder_append_span(builder, line,
                                static_cast<size_t>(to_clause - line)) != FT_SUCCESS
                            || cobol_text_builder_append_string(builder, " \n") != FT_SUCCESS
                            || cobol_text_builder_append_string(builder, "      -    ") != FT_SUCCESS
                            || cobol_text_builder_append_string(builder, to_clause) != FT_SUCCESS
                            || cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
                            return (FT_FAILURE);
                        line[0] = '\0';
                    }
                }
                if (!builder->free_format && std::strlen(line) >= 65)
                {
                    char first_line[256];
                    char *to_clause;

                    to_clause = std::strstr(line, " TO ");
                    if (!to_clause || static_cast<size_t>(to_clause - line) + 1
                        >= sizeof(first_line))
                        return (FT_FAILURE);
                    std::memcpy(first_line, line, static_cast<size_t>(to_clause - line));
                    first_line[to_clause - line] = '\0';
                    ft_strlcat(first_line, " ", sizeof(first_line));
                    if (cobol_text_builder_append_line(builder, first_line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "      -    %s", to_clause + 1) < 0)
                        return (FT_FAILURE);
                    skip_append = 1;
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
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
            else if (std::strncmp(statement->source, "CBLC-EX-STRING",
                    sizeof(statement->source)) == 0)
            {
                if (std::snprintf(line, sizeof(line),
                        "           DISPLAY CBLC-EX-STRING-BUF(1:CBLC-EX-STRING-LEN)") < 0)
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
        else if (statement->type == CBLC_STATEMENT_IF_BEGIN
            || statement->type == CBLC_STATEMENT_IF_ELSE
            || statement->type == CBLC_STATEMENT_IF_END)
        {
            if (statement->type == CBLC_STATEMENT_IF_BEGIN)
            {
                char condition_text[TRANSPILE_STATEMENT_TEXT_MAX];


                if (cblc_translate_expression_for_cobol(statement->source,
                        condition_text, sizeof(condition_text)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           IF %s",
                        condition_text) < 0)
                    return (FT_FAILURE);
                control_flow_depth += 1;
            }
            else if (statement->type == CBLC_STATEMENT_IF_ELSE)
                ft_strlcpy(line, "           ELSE", sizeof(line));
            else
            {
                if (control_flow_depth > 0)
                    control_flow_depth -= 1;
                ft_strlcpy(line, "           END-IF", sizeof(line));
                if (control_flow_depth == 0)
                    ft_strlcat(line, ".", sizeof(line));
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_WHILE_BEGIN
            || statement->type == CBLC_STATEMENT_WHILE_END)
        {
            if (statement->type == CBLC_STATEMENT_WHILE_BEGIN)
            {
                if (statement->source[0] != '\0')
                {
                    char condition_text[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (cblc_translate_expression_for_cobol(statement->source,
                            condition_text, sizeof(condition_text)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           PERFORM UNTIL NOT (%s)", condition_text) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line),
                        "           PERFORM UNTIL CBLC-EOF-FLAG = 'Y'") < 0)
                    return (FT_FAILURE);
                control_flow_depth += 1;
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (control_flow_depth > 0)
                    control_flow_depth -= 1;
                ft_strlcpy(line, "           END-PERFORM", sizeof(line));
                if (control_flow_depth == 0)
                    ft_strlcat(line, ".", sizeof(line));
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_TRY_BEGIN)
        {
            if (std::snprintf(line, sizeof(line),
                    "           MOVE 'N' TO CBLC-EX-ACTIVE.") < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "           MOVE 'N' TO CBLC-EX-MATCHED.") != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_TRY_BODY_END)
        {
            if (std::snprintf(line, sizeof(line),
                    "           GO TO CBLC-TRY-%s-%zu-DONE.", function->cobol_name,
                    statement->exception_region_id) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_CATCH_BEGIN)
        {
            size_t catch_index;
            size_t scan_index;
            char condition[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_data_item *catch_item;

            catch_index = 0;
            scan_index = 0;
            while (scan_index < index)
            {
                if (function->statements[scan_index].type == CBLC_STATEMENT_CATCH_BEGIN
                    && function->statements[scan_index].exception_region_id
                        == statement->exception_region_id)
                    catch_index += 1;
                scan_index += 1;
            }
            if (std::snprintf(line, sizeof(line), "       CBLC-TRY-%s-%zu-CATCH-%zu.",
                    function->cobol_name, statement->exception_region_id, catch_index) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "           MOVE 'N' TO CBLC-EX-MATCHED.") != FT_SUCCESS)
                return (FT_FAILURE);
            if (statement->exception_is_catch_all)
                ft_strlcpy(condition, "CBLC-EX-ACTIVE = 'Y'", sizeof(condition));
            else if (cblc_build_exception_match_condition(unit, statement,
                    condition, sizeof(condition)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           IF %s", condition) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_emit_exception_base_payload_views(unit, statement, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (statement->target[0] != '\0' && !statement->exception_catch_is_reference)
            {
                catch_item = cblc_find_data_item_for_emit_by_source(unit, statement->target);
                if (!catch_item)
                    return (FT_FAILURE);
                if (catch_item->kind == CBLC_DATA_KIND_STRING)
                {
                    char buffer_name[TRANSPILE_IDENTIFIER_MAX];

                    cblc_build_string_component_name(catch_item->cobol_name, "-BUF",
                            buffer_name, sizeof(buffer_name));
                    if (std::snprintf(line, sizeof(line),
                            "               MOVE CBLC-EX-STRING-BUF TO %s",
                            buffer_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    cblc_build_string_component_name(catch_item->cobol_name, "-LEN",
                            buffer_name, sizeof(buffer_name));
                    if (std::snprintf(line, sizeof(line),
                            "               MOVE CBLC-EX-STRING-LEN TO %s",
                            buffer_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else if (catch_item->kind == CBLC_DATA_KIND_STRUCT)
                {
                    char payload_name[TRANSPILE_IDENTIFIER_MAX];
                    const t_cblc_struct_type *catch_type;

                    if (cblc_exception_payload_name(catch_item->struct_type_name,
                            payload_name, sizeof(payload_name)) != FT_SUCCESS
                        || !(catch_type = cblc_find_struct_type(unit,
                            catch_item->struct_type_name)))
                        return (FT_FAILURE);
                    if (cblc_type_requires_destruct(unit, catch_type))
                    {
                        if (cblc_emit_exception_payload_copy(unit, catch_type, payload_name,
                                catch_item->cobol_name, builder) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else if (std::snprintf(line, sizeof(line),
                            "               MOVE %s TO %s", payload_name,
                            catch_item->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line),
                        "               MOVE CBLC-EX-INT-PAYLOAD TO %s",
                        catch_item->cobol_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder,
                    "               MOVE 'Y' TO CBLC-EX-MATCHED") != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "           IF CBLC-EX-MATCHED = 'Y'") != FT_SUCCESS)
                return (FT_FAILURE);
            exception_handler_depth += 1;
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_CATCH_END)
        {
            if (index > 0
                && function->statements[index - 1].type == CBLC_STATEMENT_CATCH_BEGIN
                && cobol_text_builder_append_line(builder, "               CONTINUE") != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "           IF CBLC-EX-MATCHED = 'Y'") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "               MOVE 'N' TO CBLC-EX-ACTIVE") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "               PERFORM CBLC-EX-CLEAR-PAYLOAD") != FT_SUCCESS
                || cblc_emit_exception_context_clear(builder) != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "           MOVE 'N' TO CBLC-EX-MATCHED.") != FT_SUCCESS)
                return (FT_FAILURE);
            if (exception_handler_depth > 0)
                exception_handler_depth -= 1;
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_TRY_END)
        {
            if (std::snprintf(line, sizeof(line), "       CBLC-TRY-%s-%zu-DONE.",
                    function->cobol_name, statement->exception_region_id) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "           IF CBLC-EX-ACTIVE = 'Y'") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "               DISPLAY \"Unhandled CBL-C exception\"") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    std::strncmp(function->source_name, "main",
                        sizeof(function->source_name)) == 0
                        ? "               STOP RUN" : "               GOBACK") != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_THROW)
        {
            char translated_expression[TRANSPILE_STATEMENT_TEXT_MAX];
            int keep_exception_sentence;

            /* A throw nested in an IF/PERFORM must stay in that COBOL
             * sentence so a following ELSE/END-* remains attached.  At a
             * function or handler boundary the transfer can terminate its
             * sentence normally. */
            keep_exception_sentence = (control_flow_depth > 0);

            if (statement->exception_cleanup_count > 0)
            {
                char cleanup_paragraph[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_build_exception_cleanup_paragraph_name(function, index,
                        cleanup_paragraph, sizeof(cleanup_paragraph)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           PERFORM %s%s",
                        cleanup_paragraph, keep_exception_sentence ? "" : ".") < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }

            if (statement->exception_is_rethrow)
            {
                if (statement->exception_from_catch)
                {
                    if ((statement->exception_to_outer_region
                            ? std::snprintf(line, sizeof(line),
                                "           GO TO CBLC-TRY-%s-%zu-CATCH-0",
                                function->cobol_name, statement->exception_region_id)
                            : std::snprintf(line, sizeof(line),
                                "           GO TO CBLC-TRY-%s-%zu-DONE",
                                function->cobol_name, statement->exception_region_id)) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line),
                        "           GO TO CBLC-TRY-%s-%zu-DONE.", function->cobol_name,
                        statement->exception_region_id) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
            else
            {
                if (statement->exception_from_catch
                    && (cobol_text_builder_append_line(builder,
                        "           IF CBLC-EX-ACTIVE = 'Y' AND CBLC-EX-RAISING = 'N' AND CBLC-EX-CLEANING = 'N'") != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           PERFORM CBLC-EX-CLEAR-PAYLOAD") != FT_SUCCESS
                        || cblc_emit_exception_context_clear(builder) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS))
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'") < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "               MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
                        statement->exception_type_id) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "               MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
                        statement->exception_source_line) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "               MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
                        statement->exception_source_column) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "               PERFORM CBLC-TERMINATE") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        keep_exception_sentence ? "           END-IF"
                            : "           END-IF.") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        keep_exception_sentence
                            ? "           MOVE 'Y' TO CBLC-EX-RAISING"
                            : "           MOVE 'Y' TO CBLC-EX-RAISING.") != FT_SUCCESS)
                    return (FT_FAILURE);
                if (statement->exception_payload_is_struct)
                {
                    if (cblc_translate_expression_for_cobol(statement->source,
                            translated_expression, sizeof(translated_expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (statement->exception_payload_requires_copy)
                    {
                        const t_cblc_struct_type *payload_type;

                        payload_type = cblc_find_struct_type(unit,
                            statement->exception_payload_struct_type);
                        if (!payload_type
                            || cblc_emit_exception_payload_copy(unit, payload_type,
                                translated_expression,
                                statement->exception_payload_cobol_name, builder)
                                != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else
                    {
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO %s", translated_expression,
                                statement->exception_payload_cobol_name)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO %s.", translated_expression,
                                statement->exception_payload_cobol_name)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    {
                        const t_cblc_struct_type *payload_type;
                        size_t payload_size;

                        payload_type = cblc_find_struct_type(unit,
                            statement->exception_payload_struct_type);
                        payload_size = cblc_exception_payload_size(unit, payload_type, 0);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-PAYLOAD-SIZE", payload_size)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-PAYLOAD-SIZE.", payload_size)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                }
                else if (statement->exception_payload_is_string)
                {
                    if (statement->is_literal)
                    {
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-STRING-BUF",
                                statement->source)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-STRING-BUF.",
                                statement->source)) < 0)
                            return (FT_FAILURE);
                        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-STRING-LEN",
                                std::strlen(statement->source) >= 2
                                    ? std::strlen(statement->source) - 2 : 0)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-STRING-LEN.",
                                std::strlen(statement->source) >= 2
                                    ? std::strlen(statement->source) - 2 : 0)) < 0)
                            return (FT_FAILURE);
                        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-PAYLOAD-SIZE",
                                std::strlen(statement->source) >= 2
                                    ? std::strlen(statement->source) - 2 : 0)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-PAYLOAD-SIZE.",
                                std::strlen(statement->source) >= 2
                                    ? std::strlen(statement->source) - 2 : 0)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else
                    {
                        char source_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                        char source_length[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (cblc_translate_expression_for_cobol(statement->source,
                                translated_expression, sizeof(translated_expression)) != FT_SUCCESS)
                            return (FT_FAILURE);
                        cblc_build_string_component_name(translated_expression, "-BUF",
                            source_buffer, sizeof(source_buffer));
                        cblc_build_string_component_name(translated_expression, "-LEN",
                            source_length, sizeof(source_length));
                        if (std::snprintf(line, sizeof(line), "           IF %s > %d",
                                source_length, CBLC_EXCEPTION_STRING_PAYLOAD_MAX) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "               MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
                                cblc_exception_type_id("string")) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "               MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
                                statement->exception_source_line) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "               MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
                                statement->exception_source_column) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || cobol_text_builder_append_line(builder,
                                "               PERFORM CBLC-TERMINATE") != FT_SUCCESS
                            || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-STRING-BUF", source_buffer)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-STRING-BUF.", source_buffer)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-STRING-LEN", source_length)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-STRING-LEN.", source_length)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-PAYLOAD-SIZE", source_length)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-PAYLOAD-SIZE.", source_length)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                }
                else
                {
                    if (cblc_translate_expression_for_cobol(statement->source,
                            translated_expression, sizeof(translated_expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if ((keep_exception_sentence
                        ? std::snprintf(line, sizeof(line),
                            "           MOVE %s TO CBLC-EX-INT-PAYLOAD",
                            translated_expression)
                        : std::snprintf(line, sizeof(line),
                            "           MOVE %s TO CBLC-EX-INT-PAYLOAD.",
                            translated_expression)) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (keep_exception_sentence
                        ? cobol_text_builder_append_line(builder,
                            "           MOVE 4 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
                        : cobol_text_builder_append_line(builder,
                            "           MOVE 4 TO CBLC-EX-PAYLOAD-SIZE.") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-TYPE-ID",
                        statement->exception_type_id)
                    : std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-TYPE-ID.",
                        statement->exception_type_id)) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
                        statement->exception_type_id)
                    : std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID.",
                        statement->exception_type_id)) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-SOURCE-LINE",
                        statement->exception_source_line)
                    : std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-SOURCE-LINE.",
                        statement->exception_source_line)) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-SOURCE-FILE-ID",
                        unit->source_file_id)
                    : std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-SOURCE-FILE-ID.",
                        unit->source_file_id)) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-SOURCE-COLUMN",
                        statement->exception_source_column)
                    : std::snprintf(line, sizeof(line), "           MOVE %zu TO CBLC-EX-SOURCE-COLUMN.",
                        statement->exception_source_column)) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER")
                    : std::snprintf(line, sizeof(line), "           MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER.")) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                    ? std::snprintf(line, sizeof(line), "           MOVE 'Y' TO CBLC-EX-ACTIVE")
                    : std::snprintf(line, sizeof(line), "           MOVE 'Y' TO CBLC-EX-ACTIVE.")) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if ((keep_exception_sentence
                        ? std::snprintf(line, sizeof(line),
                            "           MOVE 'N' TO CBLC-EX-RAISING")
                        : std::snprintf(line, sizeof(line),
                            "           MOVE 'N' TO CBLC-EX-RAISING.")) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (!statement->exception_has_region)
                {
                    if (std::strncmp(function->source_name, "main",
                            sizeof(function->source_name)) == 0)
                        ft_strlcpy(line, keep_exception_sentence
                            ? "           STOP RUN" : "           STOP RUN.", sizeof(line));
                    else
                        ft_strlcpy(line, keep_exception_sentence
                            ? "           GOBACK" : "           GOBACK.", sizeof(line));
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    if ((keep_exception_sentence
                            ? (statement->exception_to_outer_region
                                ? std::snprintf(line, sizeof(line),
                                    "           GO TO CBLC-TRY-%s-%zu-CATCH-0",
                                    function->cobol_name, statement->exception_region_id)
                                : std::snprintf(line, sizeof(line),
                                    "           GO TO CBLC-TRY-%s-%zu-DONE",
                                    function->cobol_name, statement->exception_region_id))
                            : std::snprintf(line, sizeof(line),
                                "           GO TO CBLC-TRY-%s-%zu-CATCH-0.",
                                function->cobol_name, statement->exception_region_id)) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                skip_append = 1;
            }
        }
        else if (statement->type == CBLC_STATEMENT_FILE_OPEN
            || statement->type == CBLC_STATEMENT_FILE_CLOSE
            || statement->type == CBLC_STATEMENT_FILE_READ
            || statement->type == CBLC_STATEMENT_FILE_WRITE)
        {
            t_cblc_file_declaration *file;
            const t_cblc_data_item *record;
            char status_name[TRANSPILE_IDENTIFIER_MAX];

            file = cblc_find_file(const_cast<t_cblc_translation_unit *>(unit), statement->target);
            if (!file)
                return (FT_FAILURE);
            if (std::snprintf(status_name, sizeof(status_name), "%s-STATUS",
                    file->cobol_name) < 0)
                return (FT_FAILURE);
            if (statement->type == CBLC_STATEMENT_FILE_OPEN)
            {
                const char *verb;

                verb = std::strncmp(statement->source, "r", sizeof(statement->source)) == 0
                    ? "INPUT" : (std::strncmp(statement->source, "a", sizeof(statement->source)) == 0
                        ? "EXTEND" : "OUTPUT");
                if (std::snprintf(line, sizeof(line), "           OPEN %s %s.",
                        verb, file->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (statement->type == CBLC_STATEMENT_FILE_CLOSE)
            {
                if (std::snprintf(line, sizeof(line), "           CLOSE %s.",
                        file->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                record = cblc_find_data_item_for_emit_by_source(unit, statement->source);
                if (!record || file->record_length == 0)
                    return (FT_FAILURE);
                if (statement->type == CBLC_STATEMENT_FILE_WRITE)
                {
                    if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s-RECORD.",
                            record->cobol_name, file->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           WRITE %s-RECORD%s",
                            file->cobol_name,
                            file->organization == TRANSPILE_FILE_ORGANIZATION_INDEXED ? "" : ".") < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (file->organization == TRANSPILE_FILE_ORGANIZATION_INDEXED
                        && (cobol_text_builder_append_line(builder,
                            "           INVALID KEY CONTINUE") != FT_SUCCESS
                            || cobol_text_builder_append_line(builder,
                                "           END-WRITE") != FT_SUCCESS))
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line), "           READ %s INTO %s",
                            file->cobol_name, record->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "               AT END MOVE 'Y' TO CBLC-EOF-FLAG") != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "           END-READ") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            if (statement->type == CBLC_STATEMENT_FILE_READ
                || statement->type == CBLC_STATEMENT_FILE_WRITE)
            {
                if (cblc_emit_file_status_exception(unit, function, statement, index,
                        status_name,
                        statement->type == CBLC_STATEMENT_FILE_READ, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
            else
            {
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cblc_emit_file_status_exception(unit, function, statement, index,
                        status_name, 0, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
        }
        else if (statement->type == CBLC_STATEMENT_COMPUTE)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t numeric_exception_type_id;
            size_t enclosing_region_id;
            t_cblc_statement transfer_statement;
            int has_enclosing_region;

            has_enclosing_region = cblc_find_enclosing_exception_region(function, index,
                &enclosing_region_id) == FT_SUCCESS;

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
            if (has_enclosing_region)
            {
                transfer_statement = *statement;
                transfer_statement.exception_has_region = 1;
                transfer_statement.exception_region_id = enclosing_region_id;
                numeric_exception_type_id = cblc_exception_type_id("int");
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "               ON SIZE ERROR") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "                   IF CBLC-EX-ACTIVE = 'Y' OR CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                       MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
                        numeric_exception_type_id) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                       MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
                        statement->exception_source_line) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                       MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
                        statement->exception_source_column) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "                       PERFORM CBLC-TERMINATE") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "                   END-IF") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                   MOVE 1 TO CBLC-EX-INT-PAYLOAD") < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "                   MOVE 4 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                   MOVE %zu TO CBLC-EX-TYPE-ID",
                        numeric_exception_type_id) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                   MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
                        numeric_exception_type_id) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "                   MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "                   MOVE 'Y' TO CBLC-EX-ACTIVE") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "           END-COMPUTE.") != FT_SUCCESS
                    || cblc_emit_exception_transfer(unit, function, &transfer_statement, index,
                        builder)
                        != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
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
                int call_may_throw;
                int raw_call_has_exception_adapter;
                char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

                call_may_throw = cblc_statement_call_may_throw(unit, statement);
                raw_call_has_exception_adapter = !call_may_throw
                    && cblc_unit_has_exception_context(unit);
                using_arguments[0] = '\0';
                if (std::strncmp(statement->call_identifier, "CBLC-", 5) == 0
                    && statement->call_arguments[0] != '\0')
                {
                    ft_strlcpy(using_arguments, statement->call_arguments,
                        sizeof(using_arguments));
                    if (call_may_throw
                        && cblc_append_external_call_using_argument(using_arguments,
                            sizeof(using_arguments), "CBLC-EX-CONTEXT", 0) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            using_arguments) < 0)
                        return (FT_FAILURE);
                }
                else if (statement->call_arguments[0] != '\0')
                {
                    if (cblc_build_external_call_arguments(unit, statement,
                            using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (call_may_throw
                        && cblc_append_external_call_using_argument(using_arguments,
                            sizeof(using_arguments), "CBLC-EX-CONTEXT", 0) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            using_arguments) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (call_may_throw)
                    {
                        if (std::snprintf(line, sizeof(line),
                                "           CALL '%s' USING BY REFERENCE CBLC-EX-CONTEXT",
                                cobol_name) < 0)
                            return (FT_FAILURE);
                    }
                    else if (std::snprintf(line, sizeof(line), "           CALL '%s'",
                            cobol_name) < 0)
                        return (FT_FAILURE);
                }
                if (raw_call_has_exception_adapter)
                {
                    if (cblc_emit_raw_call_exception(unit, function, statement, index,
                            line, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                }
                else if (call_may_throw)
                {
                    ft_strlcat(line, ".", sizeof(line));
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cblc_emit_exception_transfer(unit, function, statement, index,
                            builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                }
            }
            else
            {
                if (std::strncmp(statement->call_identifier, "CBLC-DELETE-ARRAY",
                        std::strlen("CBLC-DELETE-ARRAY")) == 0)
                {
                    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (statement->call_argument_count != 1
                        || cblc_extract_call_argument(statement, 0, argument,
                            sizeof(argument)) != FT_SUCCESS
                        || cblc_translate_expression_for_cobol(argument, translated_argument,
                            sizeof(translated_argument)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           FREE %s",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           SET %s TO NULL.",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                    target_function = NULL;
                }
                else if (std::strncmp(statement->call_identifier, "CBLC-DELETE",
                        std::strlen("CBLC-DELETE")) == 0)
                {
                    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    const t_cblc_data_item *pointer_item;
                    t_cblc_statement destructor_statement;

                    if (statement->call_argument_count != 1
                        || cblc_extract_call_argument(statement, 0, argument,
                            sizeof(argument)) != FT_SUCCESS
                        || cblc_translate_expression_for_cobol(argument, translated_argument,
                            sizeof(translated_argument)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    pointer_item = cblc_find_data_item_for_emit_by_source(unit, argument);
                    if (!pointer_item)
                        pointer_item = cblc_find_data_item_by_cobol(unit, argument);
                    if (!pointer_item)
                        return (FT_FAILURE);
                    destructor_statement = *statement;
                    destructor_statement.type = CBLC_STATEMENT_DESTRUCT;
                    if (cblc_emit_struct_pointer_lifecycle(unit, pointer_item,
                            &destructor_statement, builder) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           FREE %s",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           SET %s TO NULL.",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                    target_function = NULL;
                }
                else if (std::strncmp(statement->call_identifier, "std::free",
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

                        argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
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
                ft_strlcpy(cobol_name, target_function->cobol_name, sizeof(cobol_name));
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        cobol_name) < 0)
                    return (FT_FAILURE);
                if (cblc_function_may_throw(unit, target_function, 0))
                {
                    ft_strlcat(line, ".", sizeof(line));
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cblc_emit_exception_transfer(unit, function, statement, index,
                            builder)
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                }
                if (cblc_emit_local_call_reference_copy_backs(unit, target_function,
                        statement, builder) != FT_SUCCESS)
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
                int call_may_throw;
                int raw_call_has_exception_adapter;

                if (statement->target[0] == '\0')
                    return (FT_FAILURE);
                call_may_throw = cblc_statement_call_may_throw(unit, statement);
                raw_call_has_exception_adapter = !call_may_throw
                    && cblc_unit_has_exception_context(unit);
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
                if (call_may_throw
                    && cblc_append_external_call_using_argument(using_arguments,
                        sizeof(using_arguments), "CBLC-EX-CONTEXT", 0) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, using_arguments) < 0)
                    return (FT_FAILURE);
                if (raw_call_has_exception_adapter)
                {
                    if (cblc_emit_raw_call_exception(unit, function, statement, index,
                            line, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                }
                else if (call_may_throw)
                {
                    ft_strlcat(line, ".", sizeof(line));
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cblc_emit_exception_transfer(unit, function, statement, index,
                            builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                }
            }
            else
            {
                if (std::strncmp(statement->call_identifier, "CBLC-NEW-ARRAY",
                        std::strlen("CBLC-NEW-ARRAY")) == 0)
                {
                    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char mapped_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    const t_cblc_data_item *pointer_item;
                    const t_cblc_data_item *receiver_item;
                    char receiver_name[TRANSPILE_IDENTIFIER_MAX];
                    const char *field_suffix;

                    pointer_item = cblc_find_data_item_by_cobol(unit, statement->target);
                    if (!pointer_item
                        || statement->call_argument_count != 1
                        || cblc_extract_call_argument(statement, 0, argument,
                            sizeof(argument)) != FT_SUCCESS
                        || ft_strlcpy(mapped_argument, argument,
                            sizeof(mapped_argument)) >= sizeof(mapped_argument)
                        || std::snprintf(receiver_name, sizeof(receiver_name), "%s",
                            statement->target) < 0)
                        return (FT_FAILURE);
                    field_suffix = std::strstr(receiver_name, "-CBLC-USER-DATA");
                    if (field_suffix)
                    {
                        receiver_name[static_cast<size_t>(field_suffix - receiver_name)] = '\0';
                        receiver_item = cblc_find_data_item_by_cobol(unit, receiver_name);
                        if (receiver_item
                            && cblc_rewrite_method_field_references(unit, receiver_item, NULL,
                                argument, mapped_argument, sizeof(mapped_argument)) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    if (cblc_translate_expression_for_cobol(mapped_argument,
                            translated_argument, sizeof(translated_argument)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           ALLOCATE %s CHARACTERS RETURNING %s",
                            translated_argument, statement->target) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                    target_function = NULL;
                }
                else if (std::strncmp(statement->call_identifier, "CBLC-NEW",
                        std::strlen("CBLC-NEW")) == 0)
                {
                    size_t allocation_size;
                    const t_cblc_data_item *pointer_item;

                    pointer_item = cblc_find_data_item_by_cobol(unit, statement->target);
                    if (!pointer_item)
                        return (FT_FAILURE);
                    if (statement->source[0] == '\0'
                        || cblc_sizeof_data_kind(unit, CBLC_DATA_KIND_STRUCT, 0, 0,
                            statement->source, &allocation_size) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           ALLOCATE %zu CHARACTERS RETURNING %s",
                            allocation_size, statement->target) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cblc_emit_struct_pointer_lifecycle(unit, pointer_item,
                            statement, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                    target_function = NULL;
                }
                else if (std::strncmp(statement->call_identifier, "std::malloc",
                        sizeof(statement->call_identifier)) == 0
                    || std::strncmp(statement->call_identifier, "std::realloc",
                        sizeof(statement->call_identifier)) == 0)
                {
                    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    size_t allocation_region_id;
                    int allocation_has_exception_region;

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
                    allocation_has_exception_region =
                        cblc_find_enclosing_exception_region(function, index,
                            &allocation_region_id) == FT_SUCCESS;
                    if (allocation_has_exception_region)
                    {
                        if (cblc_emit_allocation_exception(unit, function, statement, index,
                                line, statement->target, builder) != FT_SUCCESS)
                            return (FT_FAILURE);
                        skip_append = 1;
                    }
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
                ft_strlcpy(cobol_name, target_function->cobol_name, sizeof(cobol_name));
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
                if (cblc_function_may_throw(unit, target_function, 0)
                    && cblc_emit_exception_transfer(unit, function, statement, index, builder)
                        != FT_SUCCESS)
                    return (FT_FAILURE);
                if (statement->source[0] == '\0' || statement->target[0] == '\0')
                    return (FT_FAILURE);
                {
                    const t_cblc_data_item *target_item;

                    target_item = cblc_find_data_item_by_cobol(unit, statement->target);
                    if (target_item && cblc_data_kind_is_pointer(target_item->kind))
                    {
                        const t_cblc_data_item *source_item;

                        source_item = cblc_find_data_item_by_cobol(unit, statement->source);
                        if (!source_item)
                            source_item = cblc_find_data_item_for_emit_by_source(unit,
                                statement->source);
                        if ((!source_item || source_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                                || !cblc_data_kind_is_pointer(source_item->kind))
                            && !std::strchr(statement->source, '[')
                            && !std::strstr(statement->source, "->"))
                        {
                            if (std::snprintf(line, sizeof(line),
                                    "           SET %s TO ADDRESS OF %s",
                                    statement->target, statement->source) < 0)
                                return (FT_FAILURE);
                        }
                        else if (std::snprintf(line, sizeof(line), "           SET %s TO %s",
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
            emit_return_exit = 1;
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
                    const t_cblc_data_item *source_item;

                    source_item = cblc_find_data_item_by_cobol(unit, statement->source);
                    if (!source_item)
                        source_item = cblc_find_data_item_for_emit_by_source(unit,
                            statement->source);
                    if (source_item && (source_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                            || !cblc_data_kind_is_pointer(source_item->kind)))
                    {
                        if (std::snprintf(line, sizeof(line), "           SET %s TO ADDRESS OF %s",
                                statement->target, statement->source) < 0)
                            return (FT_FAILURE);
                    }
                    else if (!source_item && !std::strchr(statement->source, '[')
                        && !std::strstr(statement->source, "->"))
                    {
                        if (std::snprintf(line, sizeof(line), "           SET %s TO ADDRESS OF %s",
                                statement->target, statement->source) < 0)
                            return (FT_FAILURE);
                    }
                    else if (cblc_emit_pointer_assignment_statement(unit, statement, builder,
                            line, sizeof(line)) != FT_SUCCESS)
                    {
                        if (std::snprintf(line, sizeof(line), "           SET %s TO %s",
                                statement->target, statement->source) < 0)
                            return (FT_FAILURE);
                    }
                }
                else
                {
                    char prepared_source[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (cblc_emit_prepare_pointer_element(unit, statement->source, builder,
                            prepared_source, sizeof(prepared_source)) == FT_SUCCESS)
                    {
                        if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                                prepared_source, statement->target) < 0)
                            return (FT_FAILURE);
                    }
                    else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                            statement->source, statement->target) < 0)
                        return (FT_FAILURE);
                }
            }
        }
        else if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->type == CBLC_STATEMENT_DESTRUCT)
        {
            int lifecycle_status;

            lifecycle_status = cblc_emit_lifecycle_statement(unit, statement, builder,
                function, index);
            if (lifecycle_status != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_METHOD_CALL
            || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            if (cblc_emit_method_statement(unit, statement, builder, function, index)
                != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            skip_append = 1;
        }
        else
            return (FT_FAILURE);
        if (!skip_append)
        {
            size_t length;

            length = std::strlen(line);
            if (exception_handler_depth == 0 && control_flow_depth == 0
                && (length == 0 || line[length - 1] != '.'))
            {
                if (length + 1 >= sizeof(line))
                    return (FT_FAILURE);
                line[length] = '.';
                line[length + 1] = '\0';
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (emit_return_exit)
        {
            if (statement->exception_from_catch
                && (cobol_text_builder_append_line(builder,
                    "           MOVE 'N' TO CBLC-EX-ACTIVE") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "           PERFORM CBLC-EX-CLEAR-PAYLOAD") != FT_SUCCESS
                    || cblc_emit_exception_context_clear(builder) != FT_SUCCESS))
                return (FT_FAILURE);
            if (std::strncmp(function->source_name, "main", sizeof(function->source_name)) == 0)
                ft_strlcpy(line, statement->exception_from_catch || control_flow_depth > 0
                    ? "           STOP RUN" : "           STOP RUN.", sizeof(line));
            else
                ft_strlcpy(line, statement->exception_from_catch || control_flow_depth > 0
                    ? "           GOBACK" : "           GOBACK.", sizeof(line));
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    if (cblc_emit_exception_cleanup_paragraphs(unit, function, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (append_stop_run && !unit->is_native_standard_library
        && cblc_emit_exception_payload_cleanup_paragraph(unit, builder) != FT_SUCCESS)
        return (FT_FAILURE);
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
        else if (type->fields[index].kind == CBLC_DATA_KIND_DOUBLE)
        {
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s USAGE COMP-2 OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s USAGE COMP-2.",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (cblc_data_kind_is_pointer(type->fields[index].kind))
        {
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line),
                        "       %02zu %s-%s USAGE POINTER OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line),
                    "       %02zu %s-%s USAGE POINTER VALUE NULL.",
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

static int cblc_emit_exception_struct_dynamic_buffers(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *prefix, t_cobol_text_builder *builder)
{
    char nested_prefix[TRANSPILE_IDENTIFIER_MAX];
    char line[256];
    size_t index;

    if (!unit || !type || !prefix || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;

        field = &type->fields[index];
        if (field->kind == CBLC_DATA_KIND_STRING
            && cblc_string_field_uses_dynamic_storage(field))
        {
            if (std::snprintf(line, sizeof(line), "       01 %s-%s-BUF BASED PIC X(%zu).",
                    prefix, field->cobol_name, field->length) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *nested_type;

            if (std::snprintf(nested_prefix, sizeof(nested_prefix), "%s-%s", prefix,
                    field->cobol_name) < 0)
                return (FT_FAILURE);
            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!nested_type || cblc_emit_exception_struct_dynamic_buffers(unit,
                    nested_type, nested_prefix, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_method_exception_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    const char *assign_target, t_cobol_text_builder *builder)
{
    t_cblc_function exception_function;
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !receiver || !method || !assign_target || !builder
        || !cblc_method_has_exception_control(method)
        || cblc_method_paragraph_name(receiver, method, paragraph_name,
            sizeof(paragraph_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&exception_function, 0, sizeof(exception_function));
    ft_strlcpy(exception_function.source_name, method->source_name,
        sizeof(exception_function.source_name));
    ft_strlcpy(exception_function.cobol_name, paragraph_name,
        sizeof(exception_function.cobol_name));
    exception_function.return_kind = method->return_kind;
    ft_strlcpy(exception_function.return_type_name, method->return_type_name,
        sizeof(exception_function.return_type_name));
    exception_function.statement_capacity = method->statement_count;
    exception_function.statement_count = method->statement_count;
    exception_function.statements = static_cast<t_cblc_statement *>(cma_calloc(
        method->statement_count == 0 ? 1 : method->statement_count,
        sizeof(*exception_function.statements)));
    if (!exception_function.statements)
        return (FT_FAILURE);
    index = 0;
    while (index < method->statement_count)
    {
        exception_function.statements[index] = method->statements[index];
        if (cblc_replace_this_prefix(method->statements[index].target,
                receiver->cobol_name, exception_function.statements[index].target,
                sizeof(exception_function.statements[index].target)) != FT_SUCCESS
            || cblc_replace_this_prefix(method->statements[index].source,
                receiver->cobol_name, exception_function.statements[index].source,
                sizeof(exception_function.statements[index].source)) != FT_SUCCESS)
        {
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        if (exception_function.statements[index].type == CBLC_STATEMENT_CALL
            || exception_function.statements[index].type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *called_method;

            receiver_type = cblc_find_receiver_type(unit, receiver);
            called_method = receiver_type
                ? cblc_find_method_on_type(receiver_type,
                    exception_function.statements[index].call_identifier) : NULL;
            if (called_method
                && ((exception_function.statements[index].type == CBLC_STATEMENT_CALL
                        && called_method->return_kind == CBLC_FUNCTION_RETURN_VOID)
                    || (exception_function.statements[index].type
                            == CBLC_STATEMENT_CALL_ASSIGN
                        && called_method->return_kind != CBLC_FUNCTION_RETURN_VOID)))
            {
                if (exception_function.statements[index].type == CBLC_STATEMENT_CALL)
                {
                    exception_function.statements[index].type = CBLC_STATEMENT_METHOD_CALL;
                    ft_strlcpy(exception_function.statements[index].target,
                        receiver->cobol_name,
                        sizeof(exception_function.statements[index].target));
                }
                else
                {
                    exception_function.statements[index].type =
                        CBLC_STATEMENT_METHOD_CALL_ASSIGN;
                    ft_strlcpy(exception_function.statements[index].source,
                        receiver->cobol_name,
                        sizeof(exception_function.statements[index].source));
                }
            }
        }
        if (exception_function.statements[index].type == CBLC_STATEMENT_RETURN
            && method->return_kind != CBLC_FUNCTION_RETURN_VOID
            && exception_function.statements[index].source[0] != '\0')
            ft_strlcpy(exception_function.statements[index].target, assign_target,
                sizeof(exception_function.statements[index].target));
        index += 1;
    }
    if (cblc_emit_function(unit, &exception_function, builder, 0) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
    {
        cma_free(exception_function.statements);
        return (FT_FAILURE);
    }
    cma_free(exception_function.statements);
    return (FT_SUCCESS);
}

static int cblc_emit_struct_instance(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, t_cobol_text_builder *builder)
{
    const t_cblc_struct_type *type;
    char line[256];
    char child_prefix[TRANSPILE_IDENTIFIER_MAX];
    int has_child_items;
    size_t index;

    if (!unit || !item || !builder)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (item->array_count > 0)
    {
        if (std::snprintf(line, sizeof(line), "       01 %s OCCURS %zu TIMES.",
                item->cobol_name, item->array_count) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(line, sizeof(line), "       01 %s.", item->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_emit_struct_fields(unit, type, item->cobol_name, 5, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    has_child_items = 0;
    if (std::snprintf(child_prefix, sizeof(child_prefix), "%s.", item->source_name) < 0)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].source_name, child_prefix,
                std::strlen(child_prefix)) == 0)
        {
            has_child_items = 1;
            break ;
        }
        index += 1;
    }
    if (!has_child_items
        && cblc_emit_exception_struct_dynamic_buffers(unit, type, item->cobol_name,
            builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
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
