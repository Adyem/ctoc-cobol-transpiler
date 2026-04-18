static int cblc_replace_this_prefix(const char *input, const char *replacement, char *output,
    size_t output_size)
{
    const char *needle;
    size_t needle_length;
    size_t replacement_length;
    size_t out_length;

    if (!input || !replacement || !output || output_size == 0)
        return (FT_FAILURE);
    needle = "CBLC-THIS";
    needle_length = std::strlen(needle);
    replacement_length = std::strlen(replacement);
    out_length = 0;
    while (*input != '\0')
    {
        if (std::strncmp(input, needle, needle_length) == 0)
        {
            if (out_length + replacement_length >= output_size)
                return (FT_FAILURE);
            std::memcpy(output + out_length, replacement, replacement_length);
            out_length += replacement_length;
            input += needle_length;
            continue ;
        }
        if (out_length + 1 >= output_size)
            return (FT_FAILURE);
        output[out_length] = *input;
        out_length += 1;
        input += 1;
    }
    output[out_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_emit_substituted_statement(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement, const char *replacement,
    t_cobol_text_builder *builder)
{
    t_cblc_statement substituted;

    auto append_statement_line = [&](char *line) -> int
    {
        size_t length;

        length = std::strlen(line);
        if (length == 0 || line[length - 1] != '.')
        {
            if (length + 1 >= 256)
                return (FT_FAILURE);
            line[length] = '.';
            line[length + 1] = '\0';
        }
        return (cobol_text_builder_append_line(builder, line));
    };

    if (!unit || !statement || !replacement || !builder)
        return (FT_FAILURE);
    substituted = *statement;
    if (cblc_replace_this_prefix(statement->target, replacement, substituted.target,
            sizeof(substituted.target)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_replace_this_prefix(statement->source, replacement, substituted.source,
            sizeof(substituted.source)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (substituted.type == CBLC_STATEMENT_ASSIGNMENT)
    {
        char line[256];
        char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_emit_pointer_assignment_statement(unit, &substituted, builder,
                line, sizeof(line)) == FT_SUCCESS)
            return (append_statement_line(line));
        if (substituted.is_literal)
            ft_strlcpy(source_text, substituted.source, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(substituted.source, source_text,
                    sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_expression_for_cobol(substituted.target, target_text,
                sizeof(target_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (substituted.is_literal)
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
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_DISPLAY)
    {
        char line[256];
        char base_name[TRANSPILE_IDENTIFIER_MAX];
        const t_cblc_data_item *item;

        if (substituted.is_literal)
        {
            if (std::snprintf(line, sizeof(line), "           DISPLAY %s", substituted.source) < 0)
                return (FT_FAILURE);
            return (append_statement_line(line));
        }
        item = cblc_find_data_item_by_cobol(unit, substituted.source);
        if (item && item->kind == CBLC_DATA_KIND_STRING
            && (std::strstr(substituted.source, "->") || std::strchr(substituted.source, '[')))
        {
            char buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];
            char length_name[TRANSPILE_STATEMENT_TEXT_MAX];
            char translated_buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];
            char translated_length_name[TRANSPILE_STATEMENT_TEXT_MAX];

            if (std::snprintf(buffer_name, sizeof(buffer_name), "%s-BUF",
                    substituted.source) < 0)
                return (FT_FAILURE);
            if (std::snprintf(length_name, sizeof(length_name), "%s-LEN",
                    substituted.source) < 0)
                return (FT_FAILURE);
            if (cblc_emit_prepare_struct_pointer_field(unit, buffer_name, builder,
                    translated_buffer_name, sizeof(translated_buffer_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_emit_prepare_struct_pointer_field(unit, length_name, builder,
                    translated_length_name, sizeof(translated_length_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           DISPLAY %s(1:%s)",
                    translated_buffer_name, translated_length_name) < 0)
                return (FT_FAILURE);
            return (append_statement_line(line));
        }
        if (cblc_parse_cobol_reference_base(substituted.source, base_name, sizeof(base_name))
            == FT_SUCCESS)
        {
            item = cblc_find_data_item_by_cobol(unit, base_name);
            if (item && item->kind == CBLC_DATA_KIND_STRING)
            {
                char buffer_name[TRANSPILE_IDENTIFIER_MAX];
                char length_name[TRANSPILE_IDENTIFIER_MAX];
                char translated_buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_length_name[TRANSPILE_STATEMENT_TEXT_MAX];

                if (std::strncmp(substituted.source, item->cobol_name,
                        sizeof(item->cobol_name)) == 0)
                {
                    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name,
                        sizeof(buffer_name));
                    cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                        sizeof(length_name));
                }
                else
                {
                    size_t zero_based_index;

                    if (cblc_parse_occurs_reference_index(substituted.source, &zero_based_index)
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-BUF", buffer_name, sizeof(buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-LEN", length_name, sizeof(length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (std::strchr(substituted.source, '['))
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
                    if (std::strstr(substituted.source, "->"))
                    {
                        if (cblc_emit_prepare_struct_pointer_field(unit, buffer_name, builder,
                                translated_buffer_name, sizeof(translated_buffer_name))
                            != FT_SUCCESS)
                            return (FT_FAILURE);
                        if (cblc_emit_prepare_struct_pointer_field(unit, length_name, builder,
                                translated_length_name, sizeof(translated_length_name))
                            != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else
                    {
                        ft_strlcpy(translated_buffer_name, buffer_name,
                            sizeof(translated_buffer_name));
                        ft_strlcpy(translated_length_name, length_name,
                            sizeof(translated_length_name));
                    }
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
                return (append_statement_line(line));
            }
        }
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_translate_expression_for_cobol(substituted.source, source_text,
                    sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           DISPLAY %s", source_text) < 0)
                return (FT_FAILURE);
        }
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_COMPUTE)
    {
        char line[256];
        char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_emit_pointer_compute_statement(unit, &substituted, builder,
                line, sizeof(line)) == FT_SUCCESS)
            return (append_statement_line(line));
        if (cblc_translate_expression_for_cobol(substituted.source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_expression_for_cobol(substituted.target, target_text,
                sizeof(target_text)) != FT_SUCCESS)
            return (FT_FAILURE);

        if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                target_text, source_text) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_CALL)
    {
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char line[256];
        const t_cblc_function *target_function;

        cblc_identifier_to_cobol(substituted.call_identifier, cobol_name, sizeof(cobol_name));
        if (substituted.call_is_external)
        {
            if (std::strncmp(substituted.call_identifier, "CBLC-", 5) == 0
                && substituted.call_arguments[0] != '\0')
            {
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, substituted.call_arguments) < 0)
                    return (FT_FAILURE);
            }
            else if (substituted.call_arguments[0] != '\0')
            {
                char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_build_external_call_arguments(unit, &substituted,
                        using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, using_arguments) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           CALL '%s'", cobol_name) < 0)
                return (FT_FAILURE);
        }
        else
        {
            if (std::strncmp(substituted.call_identifier, "std::free",
                    sizeof(substituted.call_identifier)) == 0)
            {
                char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

                if (substituted.call_argument_count != 1
                    || cblc_extract_call_argument(&substituted, 0, argument,
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
                return (append_statement_line(line));
            }
            target_function = cblc_find_function_in_unit(unit,
                substituted.call_identifier);
            if (!target_function)
                return (FT_FAILURE);
            if (cblc_emit_local_call_argument_moves(unit, target_function,
                    &substituted, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           PERFORM %s", cobol_name) < 0)
                return (FT_FAILURE);
        }
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
    {
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char line[256];
        const t_cblc_function *target_function;

        cblc_identifier_to_cobol(substituted.call_identifier, cobol_name, sizeof(cobol_name));
        if (std::strncmp(substituted.call_identifier, "std::malloc",
                sizeof(substituted.call_identifier)) == 0
            || std::strncmp(substituted.call_identifier, "std::realloc",
                sizeof(substituted.call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (substituted.call_argument_count == 0
                || cblc_extract_call_argument(&substituted,
                    substituted.call_argument_count - 1, argument,
                    sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(argument, translated_argument,
                    sizeof(translated_argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line),
                    "           ALLOCATE %s CHARACTERS RETURNING %s",
                    translated_argument, substituted.target) < 0)
                return (FT_FAILURE);
            return (append_statement_line(line));
        }
        target_function = cblc_find_function_in_unit(unit, substituted.call_identifier);
        if (!target_function)
            return (FT_FAILURE);
        if (cblc_emit_local_call_argument_moves(unit, target_function, &substituted,
                builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           PERFORM %s", cobol_name) < 0)
            return (FT_FAILURE);
        if (append_statement_line(line) != FT_SUCCESS)
            return (FT_FAILURE);
        {
            const t_cblc_data_item *target_item;

            target_item = cblc_find_data_item_by_cobol(unit, substituted.target);
            if (target_item && cblc_data_kind_is_pointer(target_item->kind))
            {
                if (std::snprintf(line, sizeof(line), "           SET %s TO %s",
                        substituted.target, substituted.source) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    substituted.source, substituted.target) < 0)
                return (FT_FAILURE);
        }
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_RETURN)
        return (FT_SUCCESS);
    if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        || substituted.type == CBLC_STATEMENT_DESTRUCT)
        return (cblc_emit_lifecycle_statement(unit, &substituted, builder));
    if (substituted.type == CBLC_STATEMENT_METHOD_CALL
        || substituted.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        return (cblc_emit_method_statement(unit, &substituted, builder));
    (void)function;
    return (FT_FAILURE);
}
