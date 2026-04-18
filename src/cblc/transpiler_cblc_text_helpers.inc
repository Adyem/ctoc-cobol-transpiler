static void cobol_text_builder_init(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static void cobol_text_builder_dispose(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    if (builder->data)
        cma_free(builder->data);
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static int cobol_text_builder_reserve(t_cobol_text_builder *builder, size_t desired_capacity)
{
    char *new_data;

    if (!builder)
        return (FT_FAILURE);
    if (builder->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 128)
        desired_capacity = 128;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (builder->data && builder->length > 0)
        std::memcpy(new_data, builder->data, builder->length);
    if (builder->data)
        cma_free(builder->data);
    builder->data = new_data;
    builder->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_span(t_cobol_text_builder *builder, const char *text, size_t length)
{
    if (!builder)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (cobol_text_builder_reserve(builder, builder->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memcpy(builder->data + builder->length, text, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_string(t_cobol_text_builder *builder, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (cobol_text_builder_append_span(builder, text, std::strlen(text)));
}

static int cobol_text_builder_append_line(t_cobol_text_builder *builder, const char *text)
{
    if (cobol_text_builder_append_string(builder, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_match_keyword(const char **cursor, const char *keyword)
{
    size_t length;

    if (!cursor || !*cursor || !keyword)
        return (0);
    length = std::strlen(keyword);
    if (std::strncmp(*cursor, keyword, length) != 0)
        return (0);
    if ((*cursor)[length] != '\0' && ((*cursor)[length] == '_' || ((*cursor)[length] >= 'a'
                && (*cursor)[length] <= 'z') || ((*cursor)[length] >= 'A'
                && (*cursor)[length] <= 'Z') || ((*cursor)[length] >= '0'
                && (*cursor)[length] <= '9')))
        return (0);
    *cursor += length;
    return (1);
}

static void cblc_skip_whitespace(const char **cursor)
{
    if (!cursor || !*cursor)
        return ;
    while (**cursor == ' ' || **cursor == '\t' || **cursor == '\n' || **cursor == '\r')
        *cursor += 1;
}

static int cblc_is_identifier_char(char character)
{
    if (character == '_')
        return (1);
    if (character >= 'a' && character <= 'z')
        return (1);
    if (character >= 'A' && character <= 'Z')
        return (1);
    if (character >= '0' && character <= '9')
        return (1);
    return (0);
}

static int cblc_parse_identifier(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    length = 0;
    while (cblc_is_identifier_char((*cursor)[length]))
        length += 1;
    while ((*cursor)[length] == ':' && (*cursor)[length + 1] == ':'
        && (*cursor)[length + 2] != '\0'
        && cblc_is_identifier_char((*cursor)[length + 2]))
    {
        length += 2;
        while (cblc_is_identifier_char((*cursor)[length]))
            length += 1;
    }
    if (length == 0)
        return (FT_FAILURE);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, *cursor, length);
    buffer[length] = '\0';
    *cursor += length;
    return (FT_SUCCESS);
}

static void cblc_identifier_to_cobol(const char *identifier, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!identifier || !buffer || buffer_size == 0)
        return ;
    index = 0;
    while (identifier[index] != '\0' && index + 1 < buffer_size)
    {
        char character;

        character = identifier[index];
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - 'a' + 'A');
        else if (character == '_')
            character = '-';
        buffer[index] = character;
        index += 1;
    }
    buffer[index] = '\0';
}

static void cblc_build_string_component_name(const char *base, const char *suffix, char *buffer,
    size_t buffer_size)
{
    if (!buffer || buffer_size == 0)
        return ;
    if (!base)
    {
        buffer[0] = '\0';
        return ;
    }
    ft_strlcpy(buffer, base, buffer_size);
    if (suffix)
        ft_strlcat(buffer, suffix, buffer_size);
}

static int cblc_string_item_uses_dynamic_storage(const t_cblc_data_item *item)
{
    if (!item || item->kind != CBLC_DATA_KIND_STRING)
        return (0);
    if (item->is_const || item->array_count > 0 || item->is_alias)
        return (0);
    if (std::strstr(item->source_name, "->"))
        return (0);
    return (1);
}

static int cblc_string_field_uses_dynamic_storage(const t_cblc_struct_field *field)
{
    if (!field || field->kind != CBLC_DATA_KIND_STRING)
        return (0);
    if (field->is_const)
        return (0);
    return (1);
}

static int cblc_string_item_array_uses_dynamic_storage(const t_cblc_data_item *item)
{
    if (!item || item->kind != CBLC_DATA_KIND_STRING)
        return (0);
    if (item->is_const || item->array_count == 0 || item->is_alias)
        return (0);
    if (std::strstr(item->source_name, "->"))
        return (0);
    return (1);
}

static int cblc_string_item_is_member_storage(const t_cblc_data_item *item)
{
    if (!item || item->kind != CBLC_DATA_KIND_STRING)
        return (0);
    if (!std::strchr(item->source_name, '.'))
        return (0);
    return (1);
}

static int cblc_emit_dynamic_string_ensure_capacity(const t_cblc_data_item *item,
    size_t desired_capacity, t_cobol_text_builder *builder)
{
    char line[256];
    char cap_name[TRANSPILE_IDENTIFIER_MAX];
    char ptr_name[TRANSPILE_IDENTIFIER_MAX];
    char buffer_name[TRANSPILE_IDENTIFIER_MAX];

    if (!item || !builder)
        return (FT_FAILURE);
    if (!cblc_string_item_uses_dynamic_storage(item))
        return (FT_SUCCESS);
    if (desired_capacity == 0)
        desired_capacity = 1;
    cblc_build_string_component_name(item->cobol_name, "-CAP", cap_name, sizeof(cap_name));
    cblc_build_string_component_name(item->cobol_name, "-PTR", ptr_name, sizeof(ptr_name));
    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name, sizeof(buffer_name));
    if (std::snprintf(line, sizeof(line), "           IF %s < %zu", cap_name,
            desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               IF %s NOT = NULL", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "                   FREE %s", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line),
            "               ALLOCATE %zu CHARACTERS RETURNING %s",
            desired_capacity, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            buffer_name, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               COMPUTE %s = %zu",
            cap_name, desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, "           END-IF."));
}

static int cblc_emit_dynamic_string_ensure_capacity_for_references(const char *cap_name,
    const char *ptr_name, const char *buffer_name, size_t desired_capacity,
    t_cobol_text_builder *builder)
{
    char line[256];

    if (!cap_name || !ptr_name || !buffer_name || !builder)
        return (FT_FAILURE);
    if (desired_capacity == 0)
        desired_capacity = 1;
    if (std::snprintf(line, sizeof(line), "           IF %s < %zu", cap_name,
            desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               IF %s NOT = NULL", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "                   FREE %s", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line),
            "               ALLOCATE %zu CHARACTERS RETURNING %s",
            desired_capacity, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            buffer_name, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               COMPUTE %s = %zu",
            cap_name, desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, "           END-IF."));
}

static int cblc_parse_dynamic_string_buffer_reference(const t_cblc_translation_unit *unit,
    const char *reference, const t_cblc_data_item **out_item, char *buffer_name,
    size_t buffer_name_size, char *cap_name, size_t cap_name_size, char *ptr_name,
    size_t ptr_name_size)
{
    char base_name[TRANSPILE_IDENTIFIER_MAX];
    const char *suffix;
    const char *index_start;
    const char *index_end;
    const t_cblc_data_item *item;
    size_t base_length;
    size_t index_length;

    if (!unit || !reference || !out_item || !buffer_name || buffer_name_size == 0
        || !cap_name || cap_name_size == 0 || !ptr_name || ptr_name_size == 0)
        return (FT_FAILURE);
    suffix = std::strstr(reference, "-BUF");
    if (!suffix)
        return (FT_FAILURE);
    base_length = static_cast<size_t>(suffix - reference);
    if (base_length == 0 || base_length + 1 > sizeof(base_name))
        return (FT_FAILURE);
    std::memcpy(base_name, reference, base_length);
    base_name[base_length] = '\0';
    suffix += std::strlen("-BUF");
    item = cblc_find_data_item_by_cobol(unit, base_name);
    if (!item || item->kind != CBLC_DATA_KIND_STRING)
        return (FT_FAILURE);
    if (*suffix == '\0')
    {
        if (!cblc_string_item_uses_dynamic_storage(item))
            return (FT_FAILURE);
        ft_strlcpy(buffer_name, reference, buffer_name_size);
        cblc_build_string_component_name(item->cobol_name, "-CAP", cap_name, cap_name_size);
        cblc_build_string_component_name(item->cobol_name, "-PTR", ptr_name, ptr_name_size);
        *out_item = item;
        return (FT_SUCCESS);
    }
    if (!cblc_string_item_array_uses_dynamic_storage(item))
        return (FT_FAILURE);
    if (*suffix != '(')
        return (FT_FAILURE);
    index_start = suffix + 1;
    index_end = std::strchr(index_start, ')');
    if (!index_end || index_end[1] != '\0')
        return (FT_FAILURE);
    index_length = static_cast<size_t>(index_end - index_start);
    if (index_length == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer_name, buffer_name_size, "%s-BUF", item->cobol_name) < 0)
        return (FT_FAILURE);
    if (std::snprintf(cap_name, cap_name_size, "%s-CAP(%.*s)", item->cobol_name,
            static_cast<int>(index_length), index_start) < 0)
        return (FT_FAILURE);
    if (std::snprintf(ptr_name, ptr_name_size, "%s-PTR(%.*s)", item->cobol_name,
            static_cast<int>(index_length), index_start) < 0)
        return (FT_FAILURE);
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_prepare_dynamic_string_buffer_reference(const t_cblc_translation_unit *unit,
    const char *reference, size_t desired_capacity, t_cobol_text_builder *builder,
    char *out_reference, size_t out_reference_size)
{
    char buffer_name[TRANSPILE_IDENTIFIER_MAX];
    char cap_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char ptr_name[TRANSPILE_STATEMENT_TEXT_MAX];
    const t_cblc_data_item *item;
    char line[256];

    if (!unit || !reference || !builder || !out_reference || out_reference_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_dynamic_string_buffer_reference(unit, reference, &item, buffer_name,
            sizeof(buffer_name), cap_name, sizeof(cap_name), ptr_name, sizeof(ptr_name))
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_emit_dynamic_string_ensure_capacity_for_references(cap_name, ptr_name,
            buffer_name, desired_capacity, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO %s",
            buffer_name, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_strlcpy(out_reference, buffer_name, out_reference_size);
    (void)item;
    return (FT_SUCCESS);
}

static int cblc_prepare_dynamic_string_buffer_view(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *out_reference,
    size_t out_reference_size)
{
    return (cblc_prepare_dynamic_string_buffer_reference(unit, reference, 1, builder,
            out_reference, out_reference_size));
}

static int cblc_parse_numeric_literal(const char **cursor, size_t *out_value)
{
    size_t value;
    const char *start;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    value = 0;
    start = *cursor;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
    }
    if (*cursor == start)
        return (FT_FAILURE);
    *out_value = value;
    return (FT_SUCCESS);
}
