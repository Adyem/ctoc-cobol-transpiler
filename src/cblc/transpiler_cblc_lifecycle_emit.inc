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
