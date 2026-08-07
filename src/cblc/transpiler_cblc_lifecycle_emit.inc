static int cblc_emit_struct_array_lifecycle(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *prefix, size_t element_count,
    size_t depth, t_cobol_text_builder *builder, int is_destruct)
{
    size_t field_index;

    if (!unit || !type || !prefix || element_count == 0 || !builder
        || depth > CBLC_TEMPLATE_PARAMETER_MAX)
        return (FT_FAILURE);
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field;
        char field_name[TRANSPILE_IDENTIFIER_MAX];

        field = &type->fields[field_index];
        if (field->array_count > 0 || (cblc_data_kind_is_pointer(field->kind)
                && !field->is_borrowed)
            || std::snprintf(field_name, sizeof(field_name), "%s-%s", prefix,
                field->cobol_name) < 0)
            return (FT_FAILURE);
        if (cblc_data_kind_is_pointer(field->kind))
        {
            size_t element_index;

            if (is_destruct)
            {
                field_index += 1;
                continue ;
            }
            element_index = 0;
            while (element_index < element_count)
            {
                char line[256];

                if (std::snprintf(line, sizeof(line), "           SET %s(%zu) TO NULL.",
                        field_name, element_index + 1) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                element_index += 1;
            }
            field_index += 1;
            continue ;
        }
        if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *nested_type;

            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!nested_type
                || cblc_emit_struct_array_lifecycle(unit, nested_type, field_name,
                    element_count, depth + 1, builder, is_destruct) != FT_SUCCESS)
                return (FT_FAILURE);
            field_index += 1;
            continue ;
        }
        if (field->kind != CBLC_DATA_KIND_STRING
            && field->kind != CBLC_DATA_KIND_CHAR
            && field->kind != CBLC_DATA_KIND_INT)
            return (FT_FAILURE);
        {
            size_t element_index;

            element_index = 0;
            while (element_index < element_count)
            {
                char line[256];
                size_t cobol_index;

                cobol_index = element_index + 1;
                if (field->kind == CBLC_DATA_KIND_STRING)
                {
                    char length_name[TRANSPILE_IDENTIFIER_MAX];

                    if (std::snprintf(length_name, sizeof(length_name), "%s-LEN(%zu)",
                            field_name, cobol_index) < 0
                        || std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.",
                            length_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (field->is_const)
                    {
                        char buffer_name[TRANSPILE_IDENTIFIER_MAX];

                        if (!is_destruct
                            && (std::snprintf(buffer_name, sizeof(buffer_name),
                                "%s-BUF(%zu)", field_name, cobol_index) < 0
                                || std::snprintf(line, sizeof(line),
                                    "           MOVE SPACES TO %s.", buffer_name) < 0
                                || cobol_text_builder_append_line(builder, line)
                                    != FT_SUCCESS))
                            return (FT_FAILURE);
                    }
                    else if (is_destruct)
                    {
                        char pointer_name[TRANSPILE_IDENTIFIER_MAX];
                        char capacity_name[TRANSPILE_IDENTIFIER_MAX];

                        if (std::snprintf(pointer_name, sizeof(pointer_name), "%s-PTR(%zu)",
                                field_name, cobol_index) < 0
                            || std::snprintf(capacity_name, sizeof(capacity_name),
                                "%s-CAP(%zu)", field_name, cobol_index) < 0
                            || std::snprintf(line, sizeof(line),
                                "           IF %s NOT = NULL", pointer_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line), "               FREE %s",
                                pointer_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || cobol_text_builder_append_line(builder, "           END-IF.")
                                != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "           SET %s TO NULL.", pointer_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.",
                                capacity_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else
                    {
                        char capacity_name[TRANSPILE_IDENTIFIER_MAX];
                        char pointer_name[TRANSPILE_IDENTIFIER_MAX];
                        char buffer_name[TRANSPILE_IDENTIFIER_MAX];
                        size_t initial_capacity;

                        initial_capacity = field->length == 0 ? 1 : field->length;
                        if (std::snprintf(capacity_name, sizeof(capacity_name),
                                "%s-CAP(%zu)", field_name, cobol_index) < 0
                            || std::snprintf(pointer_name, sizeof(pointer_name),
                                "%s-PTR(%zu)", field_name, cobol_index) < 0
                            || std::snprintf(buffer_name, sizeof(buffer_name), "%s-BUF",
                                field_name) < 0
                            || cblc_emit_dynamic_string_ensure_capacity_for_references(
                                capacity_name, pointer_name, buffer_name,
                                initial_capacity, builder) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "           SET ADDRESS OF %s TO %s.", buffer_name,
                                pointer_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "           MOVE SPACES TO %s.", buffer_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                }
                else if (!is_destruct
                    && std::snprintf(line, sizeof(line), "           MOVE %s TO %s(%zu).",
                        field->kind == CBLC_DATA_KIND_CHAR ? "SPACES" : "0",
                        field_name, cobol_index) < 0)
                    return (FT_FAILURE);
                else if (!is_destruct
                    && cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                element_index += 1;
            }
        }
        field_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_lifecycle_recursive(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *prefix, t_cobol_text_builder *builder,
    int is_destruct)
{
    char line[256];
    char nested_prefix[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !type || !prefix || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (cblc_data_kind_is_pointer(type->fields[index].kind))
        {
            if (!type->fields[index].is_borrowed)
                return (FT_FAILURE);
            if (!is_destruct
                && (std::snprintf(line, sizeof(line), "           SET %s-%s TO NULL.",
                        prefix, type->fields[index].cobol_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS))
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (type->fields[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (type->fields[index].array_count == 0)
            {
                if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-%s-LEN.",
                        prefix, type->fields[index].cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cblc_string_field_uses_dynamic_storage(&type->fields[index]))
            {
                if (type->fields[index].array_count > 0)
                {
                    size_t element_index;
                    size_t initial_capacity;

                    initial_capacity = type->fields[index].length;
                    if (initial_capacity == 0)
                        initial_capacity = 1;
                    element_index = 0;
                    while (element_index < type->fields[index].array_count)
                    {
                        size_t cobol_index;

                        cobol_index = element_index + 1;
                        if (std::snprintf(line, sizeof(line),
                                "           MOVE 0 TO %s-%s-LEN(%zu).",
                                prefix, type->fields[index].cobol_name, cobol_index) < 0)
                            return (FT_FAILURE);
                        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if (is_destruct)
                        {
                            if (std::snprintf(line, sizeof(line),
                                    "           IF %s-%s-PTR(%zu) NOT = NULL",
                                    prefix, type->fields[index].cobol_name, cobol_index) < 0)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                                return (FT_FAILURE);
                            if (std::snprintf(line, sizeof(line),
                                    "               FREE %s-%s-PTR(%zu)",
                                    prefix, type->fields[index].cobol_name, cobol_index) < 0)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, "           END-IF.")
                                != FT_SUCCESS)
                                return (FT_FAILURE);
                            if (std::snprintf(line, sizeof(line),
                                    "           SET %s-%s-PTR(%zu) TO NULL.",
                                    prefix, type->fields[index].cobol_name, cobol_index) < 0)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                                return (FT_FAILURE);
                            if (std::snprintf(line, sizeof(line),
                                    "           MOVE 0 TO %s-%s-CAP(%zu).",
                                    prefix, type->fields[index].cobol_name, cobol_index) < 0)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                                return (FT_FAILURE);
                        }
                        else
                        {
                            char cap_name[TRANSPILE_STATEMENT_TEXT_MAX];
                            char ptr_name[TRANSPILE_STATEMENT_TEXT_MAX];
                            char buffer_name[TRANSPILE_IDENTIFIER_MAX];

                            if (std::snprintf(cap_name, sizeof(cap_name), "%s-%s-CAP(%zu)",
                                    prefix, type->fields[index].cobol_name, cobol_index) < 0)
                                return (FT_FAILURE);
                            if (std::snprintf(ptr_name, sizeof(ptr_name), "%s-%s-PTR(%zu)",
                                    prefix, type->fields[index].cobol_name, cobol_index) < 0)
                                return (FT_FAILURE);
                            if (std::snprintf(buffer_name, sizeof(buffer_name), "%s-%s-BUF",
                                    prefix, type->fields[index].cobol_name) < 0)
                                return (FT_FAILURE);
                            if (cblc_emit_dynamic_string_ensure_capacity_for_references(cap_name,
                                    ptr_name, buffer_name, initial_capacity, builder) != FT_SUCCESS)
                                return (FT_FAILURE);
                            if (std::snprintf(line, sizeof(line),
                                    "           SET ADDRESS OF %s TO %s",
                                    buffer_name, ptr_name) < 0)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                                return (FT_FAILURE);
                            if (std::snprintf(line, sizeof(line),
                                    "           MOVE SPACES TO %s.",
                                    buffer_name) < 0)
                                return (FT_FAILURE);
                            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                                return (FT_FAILURE);
                        }
                        element_index += 1;
                    }
                }
                else if (is_destruct)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-%s-PTR NOT = NULL",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "               FREE %s-%s-PTR",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, "           END-IF.")
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s-%s-PTR TO NULL.",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           MOVE 0 TO %s-%s-CAP.",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    size_t initial_capacity;

                    initial_capacity = type->fields[index].length;
                    if (initial_capacity == 0)
                        initial_capacity = 1;
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-%s-CAP < %zu",
                            prefix, type->fields[index].cobol_name, initial_capacity) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "               IF %s-%s-PTR NOT = NULL",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "                   FREE %s-%s-PTR",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, "               END-IF")
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "               ALLOCATE %zu CHARACTERS RETURNING %s-%s-PTR",
                            initial_capacity, prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "               SET ADDRESS OF %s-%s-BUF TO %s-%s-PTR",
                            prefix, type->fields[index].cobol_name, prefix,
                            type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "               COMPUTE %s-%s-CAP = %zu",
                            prefix, type->fields[index].cobol_name, initial_capacity) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, "           END-IF.")
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           MOVE SPACES TO %s-%s-BUF.",
                            prefix, type->fields[index].cobol_name) < 0)
                        return (FT_FAILURE);
                    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-%s-BUF.",
                        prefix, type->fields[index].cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-%s.",
                    prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_INT)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-%s.",
                    prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            if (std::snprintf(nested_prefix, sizeof(nested_prefix), "%s-%s", prefix,
                    type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_emit_lifecycle_recursive(unit, field_type, nested_prefix, builder,
                    is_destruct)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}
