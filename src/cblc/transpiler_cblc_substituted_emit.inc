static int cblc_translate_condition_for_cobol(
    const t_cblc_translation_unit *unit, const char *expression,
    t_cobol_text_builder *builder, char *translated, size_t translated_size);

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

static int cblc_map_substituted_receiver_fields(const t_cblc_translation_unit *unit,
    const char *receiver_name, const char *input, char *output, size_t output_size,
    const t_cblc_data_item *receiver_context = NULL)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *type;
    const char *receiver_cobol_name;
    size_t index;

    if (!unit || !receiver_name || !input || !output || output_size == 0)
        return (FT_FAILURE);
    receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    receiver_cobol_name = NULL;
    if (receiver && receiver->kind == CBLC_DATA_KIND_STRUCT)
    {
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        receiver_cobol_name = receiver->cobol_name;
    }
    else if (receiver_context
        && receiver_context->kind == CBLC_DATA_KIND_STRUCT
        && std::strcmp(receiver_context->cobol_name, receiver_name) == 0)
    {
        type = cblc_find_struct_type(unit, receiver_context->struct_type_name);
        receiver_cobol_name = receiver_context->cobol_name;
    }
    else
        return (FT_FAILURE);
    if (!type)
        return (FT_FAILURE);
    output[0] = '\0';
    index = 0;
    while (input[index] != '\0')
    {
        size_t field_index;
        int matched;

        matched = 0;
        field_index = 0;
        while (field_index < type->field_count)
        {
            const t_cblc_struct_field *field;
            size_t field_length;
            int boundary;
            char replacement[TRANSPILE_STATEMENT_TEXT_MAX];

            field = &type->fields[field_index];
            field_length = std::strlen(field->source_name);
            boundary = (index == 0
                || ((input[index - 1] < 'A' || input[index - 1] > 'Z')
                    && (input[index - 1] < 'a' || input[index - 1] > 'z')
                    && (input[index - 1] < '0' || input[index - 1] > '9')
                    && input[index - 1] != '_'));
            if (boundary && std::strncmp(input + index, field->source_name, field_length) == 0
                && (input[index + field_length] == '\0'
                    || input[index + field_length] == ' '
                    || input[index + field_length] == '+'
                    || input[index + field_length] == '-'
                    || input[index + field_length] == '*'
                    || input[index + field_length] == '/'
                    || input[index + field_length] == '['
                    || input[index + field_length] == ']'
                    || input[index + field_length] == '<'
                    || input[index + field_length] == '>'))
            {
                if (std::snprintf(replacement, sizeof(replacement), "%s-%s",
                        receiver_cobol_name, field->cobol_name) < 0
                    || ft_strlcat(output, replacement, output_size) >= output_size)
                    return (FT_FAILURE);
                index += field_length;
                matched = 1;
                break ;
            }
            field_index += 1;
        }
        if (!matched)
        {
            char character[2];

            character[0] = input[index++];
            character[1] = '\0';
            if (ft_strlcat(output, character, output_size) >= output_size)
                return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int cblc_emit_substituted_statement_with_flow(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement, const char *replacement,
    t_cobol_text_builder *builder, int *control_flow_depth,
    const t_cblc_data_item *receiver_context);

static int cblc_replace_this_prefix_for_array_element(const char *input,
    const char *replacement, size_t element_index, char *output, size_t output_size)
{
    const char *cursor;
    size_t out_length;
    const size_t needle_length = std::strlen("CBLC-THIS");

    if (!input || !replacement || !output || output_size == 0 || element_index == 0)
        return (FT_FAILURE);
    cursor = input;
    out_length = 0;
    while (*cursor != '\0')
    {
        if (std::strncmp(cursor, "CBLC-THIS", needle_length) == 0)
        {
            const char *suffix_start;
            const char *suffix_end;
            size_t replacement_length;
            size_t suffix_length;
            char index_text[32];

            replacement_length = std::strlen(replacement);
            if (out_length + replacement_length >= output_size)
                return (FT_FAILURE);
            std::memcpy(output + out_length, replacement, replacement_length);
            out_length += replacement_length;
            suffix_start = cursor + needle_length;
            suffix_end = suffix_start;
            while (*suffix_end == '-' || (*suffix_end >= 'A' && *suffix_end <= 'Z')
                || (*suffix_end >= '0' && *suffix_end <= '9') || *suffix_end == '_')
                suffix_end += 1;
            suffix_length = static_cast<size_t>(suffix_end - suffix_start);
            if (std::snprintf(index_text, sizeof(index_text), "(%zu)", element_index) < 0
                || out_length + suffix_length + std::strlen(index_text) >= output_size)
                return (FT_FAILURE);
            if (suffix_length > 0)
            {
                std::memcpy(output + out_length, suffix_start, suffix_length);
                out_length += suffix_length;
                std::memcpy(output + out_length, index_text, std::strlen(index_text));
                out_length += std::strlen(index_text);
            }
            cursor = suffix_end;
            continue ;
        }
        if (out_length + 1 >= output_size)
            return (FT_FAILURE);
        output[out_length] = *cursor;
        out_length += 1;
        cursor += 1;
    }
    output[out_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_emit_array_element_constructor_statement(
    const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    const char *receiver, size_t element_index, t_cobol_text_builder *builder,
    int *control_flow_depth, const t_cblc_function *function)
{
    t_cblc_statement substituted;

    if (!unit || !statement || !receiver || element_index == 0 || !builder)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && std::strcmp(statement->target, "CBLC-THIS-VALUE") == 0)
        return (FT_SUCCESS);
    substituted = *statement;
    if (cblc_replace_this_prefix_for_array_element(statement->target, receiver,
            element_index, substituted.target, sizeof(substituted.target)) != FT_SUCCESS
        || cblc_replace_this_prefix_for_array_element(statement->source, receiver,
            element_index, substituted.source, sizeof(substituted.source)) != FT_SUCCESS
        || cblc_replace_this_prefix_for_array_element(statement->call_arguments, receiver,
            element_index, substituted.call_arguments,
            sizeof(substituted.call_arguments)) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cblc_emit_substituted_statement_with_flow(unit, function,
        &substituted, "", builder, control_flow_depth));
}

static int cblc_emit_exception_context_dispose(t_cobol_text_builder *builder);

static int cblc_emit_substituted_integer_throw(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    int *control_flow_depth)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    const int nested = control_flow_depth && *control_flow_depth > 0;

    if (!unit || !function || !statement || !builder || statement->exception_is_rethrow
        || statement->exception_payload_is_struct || statement->exception_payload_is_message
        || statement->exception_cleanup_count > 0)
        return (FT_FAILURE);
    if (cblc_translate_expression_for_cobol(statement->source, expression,
            sizeof(expression)) != FT_SUCCESS)
        return (FT_FAILURE);
#define CBLC_APPEND_THROW_LINE(...) \
    do \
    { \
        if (std::snprintf(line, sizeof(line), __VA_ARGS__) < 0 \
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS) \
            return (FT_FAILURE); \
    } while (0)
    CBLC_APPEND_THROW_LINE("           IF CBLC-EX-RAISING = 'Y' OR CBLC-EX-CLEANING = 'Y'");
    CBLC_APPEND_THROW_LINE("               MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
        statement->exception_type_id);
    CBLC_APPEND_THROW_LINE("               MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-LINE",
        statement->exception_source_line);
    CBLC_APPEND_THROW_LINE("               MOVE %zu TO CBLC-EX-SECONDARY-SOURCE-COLUMN",
        statement->exception_source_column);
    if (cobol_text_builder_append_line(builder, "               PERFORM CBLC-TERMINATE")
        != FT_SUCCESS
        || cobol_text_builder_append_line(builder, nested
            ? "           END-IF" : "           END-IF.") != FT_SUCCESS)
        return (FT_FAILURE);
    if (statement->exception_from_catch)
    {
        if (cobol_text_builder_append_line(builder,
                "           IF CBLC-EX-ACTIVE = 'Y' AND CBLC-EX-RAISING = 'N' AND CBLC-EX-CLEANING = 'N'")
                != FT_SUCCESS
            || cblc_emit_exception_context_dispose(builder) != FT_SUCCESS
            || cobol_text_builder_append_line(builder,
                nested ? "           END-IF" : "           END-IF.") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    CBLC_APPEND_THROW_LINE("           MOVE 'Y' TO CBLC-EX-RAISING%s", nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE %s TO CBLC-EX-INT-PAYLOAD%s", expression,
        nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE 4 TO CBLC-EX-PAYLOAD-SIZE%s", nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE %zu TO CBLC-EX-TYPE-ID%s",
        statement->exception_type_id, nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID%s",
        statement->exception_type_id, nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE %zu TO CBLC-EX-SOURCE-LINE%s",
        statement->exception_source_line, nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE %zu TO CBLC-EX-SOURCE-FILE-ID%s",
        unit->source_file_id, nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE %zu TO CBLC-EX-SOURCE-COLUMN%s",
        statement->exception_source_column, nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER%s",
        nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE 'Y' TO CBLC-EX-ACTIVE%s", nested ? "" : ".");
    CBLC_APPEND_THROW_LINE("           MOVE 'N' TO CBLC-EX-RAISING%s", nested ? "" : ".");
    if (statement->exception_has_region)
    {
        if (std::snprintf(line, sizeof(line),
                "           GO TO CBLC-TRY-%s-%zu-%s%s", function->cobol_name,
                statement->exception_region_id,
                statement->exception_from_catch
                    ? (statement->exception_to_outer_region ? "CATCH-0" : "DONE")
                    : "CATCH-0", nested ? "" : ".") < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (function->returns_to_paragraph)
    {
        if (function->paragraph_return_label[0] == '\0'
            || std::snprintf(line, sizeof(line), "           GO TO %s%s",
                function->paragraph_return_label, nested ? "" : ".") < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (std::strncmp(function->source_name, "main",
            sizeof(function->source_name)) == 0)
    {
        if (cobol_text_builder_append_line(builder,
                nested ? "           STOP RUN" : "           STOP RUN.") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (cobol_text_builder_append_line(builder,
            nested ? "           GOBACK" : "           GOBACK.") != FT_SUCCESS)
        return (FT_FAILURE);
#undef CBLC_APPEND_THROW_LINE
    return (FT_SUCCESS);
}

static int cblc_emit_substituted_statement_with_flow(
    const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement, const char *replacement,
    t_cobol_text_builder *builder, int *control_flow_depth,
    const t_cblc_data_item *receiver_context)
{
    t_cblc_statement substituted;
    int control_flow_base_depth;

    auto append_statement_line = [&](char *line) -> int
    {
        size_t length;

        cobol_text_builder_set_control_flow_depth(builder,
            control_flow_base_depth
                + (control_flow_depth ? *control_flow_depth : 0));
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
    control_flow_base_depth = builder->control_flow_depth;
    if (control_flow_depth && *control_flow_depth <= control_flow_base_depth)
        control_flow_base_depth -= *control_flow_depth;
    else if (control_flow_depth)
        control_flow_base_depth = 0;
    cobol_text_builder_set_control_flow_depth(builder,
        control_flow_base_depth
            + (control_flow_depth ? *control_flow_depth : 0));
    /* A constructor body carries an implicit lifecycle record for its receiver.
     * The receiver is already materialized before this paragraph runs, so
     * replaying that record would try to construct the concrete receiver as a
     * field. Explicit member lifecycle records still flow through normally. */
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && std::strcmp(statement->target, "CBLC-THIS-VALUE") == 0)
        return (FT_SUCCESS);
    substituted = *statement;
    if (cblc_replace_this_prefix(statement->target, replacement, substituted.target,
            sizeof(substituted.target)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_replace_this_prefix(statement->source, replacement, substituted.source,
            sizeof(substituted.source)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_replace_this_prefix(statement->call_arguments, replacement,
            substituted.call_arguments, sizeof(substituted.call_arguments)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        || substituted.type == CBLC_STATEMENT_DESTRUCT)
    {
        char mapped_target[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_map_substituted_receiver_fields(unit, replacement,
                substituted.target, mapped_target, sizeof(mapped_target),
                receiver_context) == FT_SUCCESS)
            ft_strlcpy(substituted.target, mapped_target, sizeof(substituted.target));
    }
    if (substituted.type == CBLC_STATEMENT_IF_BEGIN
        || substituted.type == CBLC_STATEMENT_IF_ELSE
        || substituted.type == CBLC_STATEMENT_IF_END)
    {
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (substituted.type == CBLC_STATEMENT_IF_BEGIN)
        {
            char condition[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_translate_condition_for_cobol(unit, substituted.source,
                    builder, condition, sizeof(condition)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "           IF %s", condition) < 0)
                return (FT_FAILURE);
            if (control_flow_depth)
                *control_flow_depth += 1;
            cobol_text_builder_set_control_flow_depth(builder,
                control_flow_base_depth
                    + (control_flow_depth ? *control_flow_depth : 0));
            return (cobol_text_builder_append_line(builder, line));
        }
        if (substituted.type == CBLC_STATEMENT_IF_ELSE)
        {
            cobol_text_builder_set_control_flow_depth(builder,
                control_flow_base_depth
                    + (control_flow_depth ? *control_flow_depth : 0));
            return (cobol_text_builder_append_line(builder, "           ELSE"));
        }
        if (control_flow_depth && *control_flow_depth > 0)
            *control_flow_depth -= 1;
        cobol_text_builder_set_control_flow_depth(builder,
            control_flow_base_depth
                + (control_flow_depth ? *control_flow_depth : 0));
        if (std::snprintf(line, sizeof(line), "%s",
                builder->control_flow_depth > 0
                    ? "           END-IF" : "           END-IF.") < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (substituted.type == CBLC_STATEMENT_THROW)
        return (cblc_emit_substituted_integer_throw(unit, function,
            &substituted, builder, control_flow_depth));
    if (substituted.type == CBLC_STATEMENT_WHILE_BEGIN
        || substituted.type == CBLC_STATEMENT_WHILE_END)
    {
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (substituted.type == CBLC_STATEMENT_WHILE_BEGIN)
        {
            char condition[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_translate_condition_for_cobol(unit, substituted.source,
                    builder, condition, sizeof(condition)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line),
                    "           PERFORM UNTIL NOT (%s)", condition) < 0)
                return (FT_FAILURE);
            if (control_flow_depth)
                *control_flow_depth += 1;
            cobol_text_builder_set_control_flow_depth(builder,
                control_flow_base_depth
                    + (control_flow_depth ? *control_flow_depth : 0));
            return (cobol_text_builder_append_line(builder, line));
        }
        if (control_flow_depth && *control_flow_depth > 0)
            *control_flow_depth -= 1;
        cobol_text_builder_set_control_flow_depth(builder,
            control_flow_base_depth
                + (control_flow_depth ? *control_flow_depth : 0));
        if (std::snprintf(line, sizeof(line), "%s",
                builder->control_flow_depth > 0
                    ? "           END-PERFORM" : "           END-PERFORM.") < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (substituted.type == CBLC_STATEMENT_ASSIGNMENT)
    {
        char line[256];
        char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

        const char *target_open;

        target_open = std::strchr(substituted.target, '[');
        if (target_open)
        {
            char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t pointer_name_length;
            t_cblc_data_item synthetic_pointer;
            const t_cblc_data_item *pointer_item;

            pointer_name_length = static_cast<size_t>(target_open - substituted.target);
            if (pointer_name_length == 0 || pointer_name_length >= sizeof(pointer_name))
                return (FT_FAILURE);
            std::memcpy(pointer_name, substituted.target, pointer_name_length);
            pointer_name[pointer_name_length] = '\0';
            pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_name,
                &synthetic_pointer, receiver_context);
            if (pointer_item && pointer_item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
            {
                if (cblc_emit_struct_pointer_element_copy_assignment(unit,
                        &substituted, builder, receiver_context) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
        }

        {
            const int pointer_assignment_status =
                cblc_emit_pointer_assignment_statement(unit, &substituted, builder,
                    line, sizeof(line), receiver_context, function);

            if (pointer_assignment_status == FT_SUCCESS)
            {
                const int append_status = append_statement_line(line);

                return (append_status);
            }
        }
        if (substituted.is_literal)
            ft_strlcpy(source_text, substituted.source, sizeof(source_text));
        else if (cblc_translate_condition_for_cobol(unit, substituted.source,
                    builder, source_text, sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_condition_for_cobol(unit, substituted.target,
                builder, target_text, sizeof(target_text)) != FT_SUCCESS)
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

        {
            size_t operator_index = 0;
            char arithmetic_operator = '\0';
            int char_target = std::strcmp(target_text, "CBLC-PTR-CHAR") == 0
                || std::strcmp(target_text, "CBLC-PTR-CHAR-2") == 0;
            const t_cblc_data_item *target_item =
                cblc_find_data_item_for_emit_by_source(unit, substituted.target);

            if (!target_item)
                target_item = cblc_find_data_item_by_cobol(unit,
                    substituted.target);
            if (target_item && target_item->kind == CBLC_DATA_KIND_CHAR)
                char_target = 1;
            while (source_text[operator_index] != '\0')
            {
                if ((source_text[operator_index] == '+'
                        || source_text[operator_index] == '-'
                        || source_text[operator_index] == '*'
                        || source_text[operator_index] == '/')
                    && operator_index > 0
                    && std::isspace(static_cast<unsigned char>(
                        source_text[operator_index - 1]))
                    && std::isspace(static_cast<unsigned char>(
                        source_text[operator_index + 1])))
                {
                    arithmetic_operator = source_text[operator_index];
                    break ;
                }
                operator_index += 1;
            }
            if (arithmetic_operator != '\0' && char_target
                && (arithmetic_operator == '+' || arithmetic_operator == '-'))
            {
                size_t left_start = 0;
                size_t left_end = operator_index;
                size_t right_start = operator_index + 1;
                size_t source_length = std::strlen(source_text);
                char character_expression[TRANSPILE_STATEMENT_TEXT_MAX];

                while (left_start < left_end
                    && std::isspace(static_cast<unsigned char>(
                        source_text[left_start])))
                    left_start += 1;
                while (left_end > left_start
                    && std::isspace(static_cast<unsigned char>(
                        source_text[left_end - 1])))
                    left_end -= 1;
                while (right_start < source_length
                    && std::isspace(static_cast<unsigned char>(
                        source_text[right_start])))
                    right_start += 1;
                if (left_end == left_start || right_start >= source_length)
                    return (FT_FAILURE);
                if (std::snprintf(character_expression,
                        sizeof(character_expression),
                        "FUNCTION CHAR(FUNCTION ORD(%.*s) %c %s)",
                        static_cast<int>(left_end - left_start),
                        source_text + left_start, arithmetic_operator,
                        source_text + right_start) < 0
                    || std::snprintf(line, sizeof(line),
                        "           MOVE %s TO %s", character_expression,
                        target_text) < 0)
                    return (FT_FAILURE);
            }
            else if (arithmetic_operator != '\0')
            {
                if (std::snprintf(line, sizeof(line),
                        "           COMPUTE %s = %s", target_text,
                        source_text) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line),
                    "           MOVE %s TO %s", source_text,
                    target_text) < 0)
                return (FT_FAILURE);
        }
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
        if (item && item->kind == CBLC_DATA_KIND_CHAR_BUFFER
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
            if (item && item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
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
        if (cblc_translate_condition_for_cobol(unit, substituted.source,
                builder, source_text, sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_condition_for_cobol(unit, substituted.target,
                builder, target_text, sizeof(target_text)) != FT_SUCCESS)
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
            if (std::strncmp(substituted.call_identifier, "CBLC-DELETE-ARRAY",
                    std::strlen("CBLC-DELETE-ARRAY")) == 0)
            {
                char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

                if (substituted.call_argument_count != 1
                    || cblc_extract_call_argument(&substituted, 0, argument,
                        sizeof(argument)) != FT_SUCCESS
                    || cblc_translate_expression_for_cobol(argument, translated_argument,
                        sizeof(translated_argument)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           FREE %s",
                        translated_argument) < 0
                    || append_statement_line(line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           SET %s TO NULL",
                        translated_argument) < 0)
                    return (FT_FAILURE);
                return (append_statement_line(line));
            }
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
                    else
                    {
                        const t_cblc_data_item *receiver_item;
                        const t_cblc_struct_type *receiver_type;
                        const t_cblc_struct_field *field;

                        receiver_item = cblc_find_data_item_by_cobol(unit, replacement);
                        receiver_type = receiver_item
                            ? cblc_find_struct_type(unit, receiver_item->struct_type_name) : NULL;
                        field = receiver_type ? cblc_find_field_on_type(receiver_type, argument) : NULL;
                        if (field && std::snprintf(translated_argument,
                                sizeof(translated_argument), "%s-%s", replacement,
                                field->cobol_name) < 0)
                            return (FT_FAILURE);
                    }
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
        if (std::strncmp(substituted.call_identifier, "CBLC-NEW-ARRAY",
                std::strlen("CBLC-NEW-ARRAY")) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char mapped_argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (substituted.call_argument_count != 1
                || cblc_extract_call_argument(&substituted, 0, argument,
                    sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_map_substituted_receiver_fields(unit, replacement, argument,
                    mapped_argument, sizeof(mapped_argument), receiver_context)
                != FT_SUCCESS)
                ft_strlcpy(mapped_argument, argument, sizeof(mapped_argument));
            if (cblc_translate_expression_for_cobol(mapped_argument, translated_argument,
                        sizeof(translated_argument)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line),
                    "           ALLOCATE %s CHARACTERS RETURNING %s",
                    translated_argument, substituted.target) < 0)
                return (FT_FAILURE);
            return (append_statement_line(line));
        }
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
    {
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (function && function->paragraph_return_label[0] != '\0')
        {
            if (std::snprintf(line, sizeof(line), "           GO TO %s",
                    function->paragraph_return_label) < 0)
                return (FT_FAILURE);
            return (append_statement_line(line));
        }
        if (std::snprintf(line, sizeof(line), "           CONTINUE") < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        || substituted.type == CBLC_STATEMENT_DESTRUCT)
        return (cblc_emit_lifecycle_statement(unit, &substituted, builder,
            NULL, static_cast<size_t>(-1), receiver_context));
    if (substituted.type == CBLC_STATEMENT_METHOD_CALL
        || substituted.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
    {
        if (receiver_context
            && substituted.type == CBLC_STATEMENT_METHOD_CALL
            && std::strcmp(substituted.target,
                receiver_context->cobol_name) == 0)
        {
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *self_method;

            receiver_type = cblc_find_struct_type(unit,
                receiver_context->struct_type_name);
            self_method = cblc_find_method_on_type_for_arguments(unit,
                receiver_type, substituted.call_identifier,
                substituted.call_arguments, substituted.call_argument_count);
            if (!self_method
                || self_method->return_kind != CBLC_FUNCTION_RETURN_VOID
                || (self_method->parameter_count > 0
                    && cblc_emit_parameter_argument_moves(unit,
                        substituted.call_arguments,
                        substituted.call_argument_count,
                        self_method->parameters,
                        self_method->parameter_count, builder, NULL, 0)
                        != FT_SUCCESS))
                return (FT_FAILURE);
            if (cblc_method_may_throw(unit, receiver_type, self_method, 0))
                return (cblc_emit_method_exception_body(unit, receiver_context,
                    self_method, "", builder, 1));
            return (cblc_emit_method_body(unit, receiver_context,
                self_method, NULL, builder));
        }
        return (cblc_emit_method_statement(unit, &substituted, builder, NULL,
            static_cast<size_t>(-1)));
    }
    (void)function;
    return (FT_FAILURE);
}
