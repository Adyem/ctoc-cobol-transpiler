static int cblc_translate_pointer_index_for_cobol(
    const t_cblc_translation_unit *unit, const char *index_expression,
    char *translated, size_t translated_size,
    const t_cblc_function *function_context = NULL,
    const t_cobol_text_builder *builder = NULL);

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
        return (cblc_sizeof_data_kind(unit, CBLC_DATA_KIND_STRUCT, 0, 0,
            item->struct_type_name, out_size));
    }
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

static int cblc_is_constructor_pointer_parameter(
    const t_cblc_translation_unit *unit, const char *pointer_cobol_name,
    const char *struct_type_name)
{
    size_t type_index;

    if (!unit || !pointer_cobol_name || !struct_type_name)
        return (0);
    for (type_index = 0; type_index < unit->struct_type_count; ++type_index)
    {
        const t_cblc_struct_type *type = &unit->struct_types[type_index];
        size_t constructor_index;

        for (constructor_index = 0;
            constructor_index < type->constructor_count; ++constructor_index)
        {
            const t_cblc_constructor *constructor =
                &type->constructors[constructor_index];
            size_t parameter_index;

            for (parameter_index = 0;
                parameter_index < constructor->parameter_count;
                ++parameter_index)
            {
                const t_cblc_parameter *parameter =
                    &constructor->parameters[parameter_index];

                if (parameter->kind
                        == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                    && std::strcmp(parameter->cobol_name,
                        pointer_cobol_name) == 0
                    && (std::strcmp(parameter->type_name,
                            struct_type_name) == 0
                        || (type->is_template_instantiation
                            && type->template_source_name[0] != '\0'
                            && std::strcmp(parameter->type_name,
                                type->template_source_name) == 0)))
                    return (1);
            }
        }
    }
    return (0);
}

static int cblc_build_struct_pointer_based_name(const t_cblc_translation_unit *unit,
    const char *pointer_cobol_name, const char *struct_type_name,
    char *buffer, size_t buffer_size)
{
    size_t index;
    int ambiguous;
    int written;
    int direct_pointer_item;
    const char *unprefixed_pointer_name;

    if (!pointer_cobol_name || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    unprefixed_pointer_name =
        std::strncmp(pointer_cobol_name, "CBLC-SPTR-",
            sizeof("CBLC-SPTR-") - 1) == 0
        ? pointer_cobol_name + sizeof("CBLC-SPTR-") - 1
        : pointer_cobol_name;
    /* A BASED record describes a type layout, not a pointer variable or a
     * nested access path. Reuse one stable record name for every pointer to
     * the same user-defined type; each call site still SETs its address from
     * the actual pointer. This is essential for deeply nested values, where
     * lifecycle paragraphs otherwise refer to path-specific aliases that do
     * not have a corresponding declaration. */
    if (unit && struct_type_name)
    {
        const t_cblc_struct_type *type =
            cblc_find_struct_type(unit, struct_type_name);

        if (type && !type->is_builtin)
        {
            unsigned long long hash = 14695981039346656037ULL;
            const unsigned char *name_cursor =
                reinterpret_cast<const unsigned char *>(type->source_name);
            int hash_collision = 0;

            /* source_name is the canonical, concrete specialization name.
             * Hash it rather than using the struct table position so adding
             * an unrelated declaration cannot rename generated storage. */
            /* A same-type pointer parameter may alias a method receiver. Give
             * it a separate BASED view so expressions such as
             * `elements = source->elements` do not rebind the receiver's
             * record before the assignment target is emitted. */
            if (std::strncmp(pointer_cobol_name, "CBLC-P-",
                    sizeof("CBLC-P-") - 1) == 0)
            {
                const unsigned char *pointer_cursor =
                    reinterpret_cast<const unsigned char *>(pointer_cobol_name);

                while (*pointer_cursor)
                {
                    hash ^= static_cast<unsigned long long>(*pointer_cursor);
                    hash *= 1099511628211ULL;
                    ++pointer_cursor;
                }
                while (*name_cursor)
                {
                    hash ^= static_cast<unsigned long long>(*name_cursor);
                    hash *= 1099511628211ULL;
                    ++name_cursor;
                }
                written = std::snprintf(buffer, buffer_size,
                    "CBLC-SPTR-P%016llX", hash);
                return (written >= 0
                    && static_cast<size_t>(written) < buffer_size
                    && static_cast<size_t>(written)
                        < TRANSPILE_IDENTIFIER_MAX
                    ? FT_SUCCESS : FT_FAILURE);
            }
            if (cblc_is_constructor_pointer_parameter(unit,
                    pointer_cobol_name, struct_type_name))
            {
                const unsigned char *pointer_cursor =
                    reinterpret_cast<const unsigned char *>(pointer_cobol_name);

                while (*pointer_cursor)
                {
                    hash ^= static_cast<unsigned long long>(*pointer_cursor);
                    hash *= 1099511628211ULL;
                    ++pointer_cursor;
                }
                written = std::snprintf(buffer, buffer_size,
                    "CBLC-SPTR-C%016llX", hash);
                return (written >= 0
                    && static_cast<size_t>(written) < buffer_size
                    && static_cast<size_t>(written)
                        < TRANSPILE_IDENTIFIER_MAX
                    ? FT_SUCCESS : FT_FAILURE);
            }
            while (*name_cursor)
            {
                hash ^= static_cast<unsigned long long>(*name_cursor);
                hash *= 1099511628211ULL;
                ++name_cursor;
            }
            for (index = 0; index < unit->struct_type_count; ++index)
                if (&unit->struct_types[index] == type)
                {
                    size_t other_index;

                    for (other_index = 0;
                        other_index < unit->struct_type_count; ++other_index)
                    {
                        const t_cblc_struct_type *other =
                            &unit->struct_types[other_index];
                        unsigned long long other_hash =
                            14695981039346656037ULL;
                        const unsigned char *other_cursor =
                            reinterpret_cast<const unsigned char *>(
                                other->source_name);

                        if (other == type || other->is_builtin)
                            continue ;
                        while (*other_cursor)
                        {
                            other_hash ^= static_cast<unsigned long long>(
                                *other_cursor);
                            other_hash *= 1099511628211ULL;
                            ++other_cursor;
                        }
                        if (other_hash == hash
                            && std::strcmp(other->source_name,
                                type->source_name) != 0)
                        {
                            hash_collision = 1;
                            break ;
                        }
                    }
                    if (hash_collision)
                        written = std::snprintf(buffer, buffer_size,
                            "CBLC-SPTR-T%zu", index);
                    else
                        written = std::snprintf(buffer, buffer_size,
                            "CBLC-SPTR-H%016llX", hash);
                    return (written >= 0
                        && static_cast<size_t>(written) < buffer_size
                        && static_cast<size_t>(written)
                            < TRANSPILE_IDENTIFIER_MAX
                        ? FT_SUCCESS : FT_FAILURE);
                }
            return (FT_FAILURE);
        }
    }
    /* A pointer field's own name is already declared in its parent record.
     * Reusing that spelling for the child BASED view creates two COBOL data
     * items with the same name (for example, VECTOR-ELEMENTS as both a
     * pointer and a BASED String record). Give non-root field views a stable,
     * type-qualified alias; direct source pointer variables keep their
     * readable historical spelling. */
    direct_pointer_item = 0;
    if (unit)
    {
        for (index = 0; index < unit->data_count; ++index)
            if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT_POINTER
                && !unit->data_items[index].is_alias
                && !std::strchr(unit->data_items[index].source_name, '.')
                && std::strcmp(unit->data_items[index].cobol_name,
                    pointer_cobol_name) == 0)
            {
                direct_pointer_item = 1;
                break ;
            }
    }
    if (std::strncmp(pointer_cobol_name, "CBLC-SPTR-H",
            sizeof("CBLC-SPTR-H") - 1) == 0)
        direct_pointer_item = 0;
    if (unit && struct_type_name && !direct_pointer_item)
    {
        unsigned long long hash = 14695981039346656037ULL;
        const unsigned char *cursor =
            reinterpret_cast<const unsigned char *>(pointer_cobol_name);

        while (*cursor)
        {
            hash ^= static_cast<unsigned long long>(*cursor++);
            hash *= 1099511628211ULL;
        }
        cursor = reinterpret_cast<const unsigned char *>(struct_type_name);
        while (*cursor)
        {
            hash ^= static_cast<unsigned long long>(*cursor++);
            hash *= 1099511628211ULL;
        }
        written = std::snprintf(buffer, buffer_size,
            "CBLC-SPTR-V%016llX", hash);
        return (written >= 0 && static_cast<size_t>(written) < buffer_size
            && static_cast<size_t>(written) < TRANSPILE_IDENTIFIER_MAX
            ? FT_SUCCESS : FT_FAILURE);
    }
    ambiguous = 0;
    if (unit && struct_type_name)
    {
        index = 0;
        while (index < unit->data_count)
        {
            const t_cblc_data_item *candidate;

            candidate = &unit->data_items[index];
            if (candidate->kind == CBLC_DATA_KIND_STRUCT_POINTER
                && std::strcmp(candidate->cobol_name, pointer_cobol_name) == 0
                && std::strcmp(candidate->struct_type_name, struct_type_name) != 0)
            {
                ambiguous = 1;
                break ;
            }
            index += 1;
        }
    }
    if (!ambiguous
        && sizeof("CBLC-SPTR-") - 1 + std::strlen(unprefixed_pointer_name)
            < buffer_size
        && sizeof("CBLC-SPTR-") - 1 + std::strlen(unprefixed_pointer_name)
            <= 40)
    {
        written = std::snprintf(buffer, buffer_size, "CBLC-SPTR-%s",
            unprefixed_pointer_name);
        return (written >= 0 && static_cast<size_t>(written) < buffer_size
            ? FT_SUCCESS : FT_FAILURE);
    }
    {
        unsigned long long hash;
        const char *cursor;
        size_t prefix_length;
        size_t suffix_length;
        size_t available;

        hash = 1469598103934665603ULL;
        cursor = unprefixed_pointer_name;
        while (*cursor != '\0')
        {
            hash ^= static_cast<unsigned char>(*cursor++);
            hash *= 1099511628211ULL;
        }
        if (ambiguous && struct_type_name)
        {
            cursor = struct_type_name;
            while (*cursor != '\0')
            {
                hash ^= static_cast<unsigned char>(*cursor++);
                hash *= 1099511628211ULL;
            }
        }
        prefix_length = sizeof("CBLC-SPTR-") - 1;
        suffix_length = sizeof("-H0000000000000000") - 1;
        if (buffer_size <= prefix_length + suffix_length + 1
            || TRANSPILE_IDENTIFIER_MAX <= prefix_length + suffix_length + 1)
            return (FT_FAILURE);
        /* Keep emitted names independent of caller scratch-buffer size. */
        /* COBOL implementations commonly cap data-names at 63 characters.
         * Leave room for the longest standard-library member suffixes too;
         * using the much larger CBL-C identifier limit here produces a valid
         * base name whose derived BASED fields are rejected by the COBOL
         * compiler. */
        available = 40 - prefix_length - suffix_length;
        if (available > std::strlen(unprefixed_pointer_name))
            available = std::strlen(unprefixed_pointer_name);
        written = std::snprintf(buffer, buffer_size, "CBLC-SPTR-%.*s-H%016llX",
            static_cast<int>(available), unprefixed_pointer_name, hash);
    }
    if (written < 0 || static_cast<size_t>(written) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static const t_cblc_parameter *cblc_find_parameter_by_cobol_name(
    const t_cblc_translation_unit *unit, const char *cobol_name)
{
    size_t index;

    if (!unit || !cobol_name)
        return (NULL);
    index = 0;
    while (index < unit->function_count)
    {
        size_t parameter_index = 0;

        while (parameter_index < unit->functions[index].parameter_count)
        {
            const t_cblc_parameter *parameter =
                &unit->functions[index].parameters[parameter_index];

            if (std::strcmp(parameter->cobol_name, cobol_name) == 0)
                return (parameter);
            parameter_index += 1;
        }
        index += 1;
    }
    index = 0;
    while (index < unit->struct_type_count)
    {
        size_t method_index = 0;

        while (method_index < unit->struct_types[index].method_count)
        {
            const t_cblc_method *method =
                &unit->struct_types[index].methods[method_index];
            size_t parameter_index = 0;

            while (parameter_index < method->parameter_count)
            {
                const t_cblc_parameter *parameter =
                    &method->parameters[parameter_index];

                if (std::strcmp(parameter->cobol_name, cobol_name) == 0)
                    return (parameter);
                parameter_index += 1;
            }
            method_index += 1;
        }
        index += 1;
    }
    return (NULL);
}

static int cblc_parse_pointer_member_identifier(const char **cursor,
    char *identifier, size_t identifier_size)
{
    const char *start;
    size_t length;

    if (!cursor || !*cursor || !identifier || identifier_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    while (std::isalnum(static_cast<unsigned char>(**cursor))
        || **cursor == '_' || **cursor == '-')
        *cursor += 1;
    length = static_cast<size_t>(*cursor - start);
    if (length > 4
        && (std::strncmp(start + length - 4, "-BUF", 4) == 0
            || std::strncmp(start + length - 4, "-LEN", 4) == 0))
    {
        *cursor -= 4;
        length -= 4;
    }
    if (length == 0 || length + 1 > identifier_size)
        return (FT_FAILURE);
    std::memcpy(identifier, start, length);
    identifier[length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_emit_prepare_struct_pointer_field(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *field_reference,
    size_t field_reference_size, int allow_pointer_member)
{
    char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
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
    t_cblc_data_item synthetic_pointer_item;
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
    if (cblc_parse_pointer_member_identifier(&field_cursor, field_identifier,
            sizeof(field_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    pointer_item = cblc_find_data_item_by_cobol(unit, pointer_name);
    if (!pointer_item)
    {
        const t_cblc_parameter *parameter;

        parameter = cblc_find_parameter_by_cobol_name(unit, pointer_name);
        if (parameter
            && parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
        {
            std::memset(&synthetic_pointer_item, 0, sizeof(synthetic_pointer_item));
            synthetic_pointer_item.kind = CBLC_DATA_KIND_STRUCT_POINTER;
            ft_strlcpy(synthetic_pointer_item.cobol_name, parameter->cobol_name,
                sizeof(synthetic_pointer_item.cobol_name));
            ft_strlcpy(synthetic_pointer_item.source_name,
                parameter->actual_source_name[0] != '\0'
                    ? parameter->actual_source_name : parameter->source_name,
                sizeof(synthetic_pointer_item.source_name));
            ft_strlcpy(synthetic_pointer_item.struct_type_name, parameter->type_name,
                sizeof(synthetic_pointer_item.struct_type_name));
            pointer_item = &synthetic_pointer_item;
        }
    }
    if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, pointer_item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    {
        const char *direct_field_start;
        size_t direct_field_length;
        char direct_field_name[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_data_item *direct_field_item;

        direct_field_start = arrow ? arrow + 2 : field_cursor;
        while (std::isspace(static_cast<unsigned char>(*direct_field_start)))
            direct_field_start += 1;
        direct_field_length = 0;
        while ((std::isalnum(static_cast<unsigned char>(direct_field_start[direct_field_length]))
                    || direct_field_start[direct_field_length] == '_'
                    || direct_field_start[direct_field_length] == '-')
            && direct_field_length + 1 < sizeof(direct_field_name))
            direct_field_length += 1;
        direct_field_name[0] = '\0';
        if (direct_field_length > 0)
        {
            std::memcpy(direct_field_name, direct_field_start, direct_field_length);
            direct_field_name[direct_field_length] = '\0';
            direct_field_item = cblc_find_data_item_by_cobol(unit,
                direct_field_name);
            if (direct_field_item
                && (direct_field_item->kind == CBLC_DATA_KIND_INT
                    || direct_field_item->kind == CBLC_DATA_KIND_CHAR
                    || direct_field_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                    || (allow_pointer_member
                        && cblc_data_kind_is_pointer(direct_field_item->kind))))
            {
                if (ft_strlcpy(field_reference, direct_field_item->cobol_name,
                        field_reference_size) >= field_reference_size)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
        }
    }
    field_cobol_path[0] = '\0';
    component_suffix = NULL;
    field = NULL;
    while (1)
    {
        char next_field_cobol_path[TRANSPILE_STATEMENT_TEXT_MAX];

        field = cblc_find_field_on_type(type, field_identifier);
        if (!field)
        {
            size_t field_index;

            field_index = 0;
            while (field_index < type->field_count)
            {
                const char *field_source_name;
                size_t identifier_length;
                size_t source_length;
                size_t suffix_index;
                int source_suffix_matches;

                field_source_name = type->fields[field_index].source_name;
                identifier_length = std::strlen(field_identifier);
                source_length = std::strlen(field_source_name);
                source_suffix_matches = identifier_length > source_length
                    && field_identifier[identifier_length - source_length - 1] == '-';
                suffix_index = 0;
                while (source_suffix_matches && suffix_index < source_length)
                {
                    if (std::toupper(static_cast<unsigned char>(
                            field_identifier[identifier_length - source_length + suffix_index]))
                        != std::toupper(static_cast<unsigned char>(field_source_name[suffix_index])))
                        source_suffix_matches = 0;
                    suffix_index += 1;
                }
                if (std::strcmp(type->fields[field_index].cobol_name,
                        field_identifier) == 0 || source_suffix_matches)
                {
                    field = &type->fields[field_index];
                    break ;
                }
                field_index += 1;
            }
        }
        if (!field)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "pointer member '%s' is not a source or COBOL field of type '%s'",
                field_identifier, type->source_name);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            return (FT_FAILURE);
        }
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
        if (cblc_parse_pointer_member_identifier(&field_cursor, field_identifier,
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
    if (field->kind == CBLC_DATA_KIND_CHAR_BUFFER
        && (*component_suffix == '\0'
            || (std::strncmp(component_suffix, "-BUF", sizeof("-BUF")) != 0
                && std::strncmp(component_suffix, "-LEN", sizeof("-LEN")) != 0)))
        return (FT_FAILURE);
    if (field->kind != CBLC_DATA_KIND_INT && field->kind != CBLC_DATA_KIND_CHAR
        && field->kind != CBLC_DATA_KIND_CHAR_BUFFER
        && !(allow_pointer_member && cblc_data_kind_is_pointer(field->kind)))
        return (FT_FAILURE);
    if (cblc_build_struct_pointer_based_name(unit, pointer_item->cobol_name,
            pointer_item->struct_type_name, based_name,
            sizeof(based_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (has_index)
    {
        if (std::snprintf(line, sizeof(line),
                pointer_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                    || pointer_item->kind == CBLC_DATA_KIND_CHAR
                    ? "           SET CBLC-PTR-CURSOR TO ADDRESS OF %s."
                    : "           SET CBLC-PTR-CURSOR TO %s.",
                pointer_item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(index_expression, "0", sizeof(index_expression)) != 0)
        {
            if (cblc_pointer_item_element_size(unit, pointer_item, &element_size) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_pointer_index_for_cobol(unit, index_expression,
                    translated_index, sizeof(translated_index), NULL, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE CBLC-PTR-OFFSET = (%s) * %zu.",
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

static const t_cblc_struct_field *cblc_find_nested_pointer_field(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *path, size_t depth)
{
    size_t field_index;
    const t_cblc_struct_field *compressed_match;
    size_t compressed_match_count;

    if (!unit || !type || !path || path[0] != '-' || depth >= 16)
        return (NULL);
    path += 1;
    /* Prefer a complete structural field-name match. */
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field = &type->fields[field_index];
        const size_t name_length = std::strlen(field->cobol_name);

        if (name_length > 0 && std::strncmp(path, field->cobol_name, name_length) == 0
            && (path[name_length] == '\0' || path[name_length] == '-'))
        {
            if (path[name_length] == '\0')
                return field->kind == CBLC_DATA_KIND_STRUCT_POINTER ? field : NULL;
            if (field->kind == CBLC_DATA_KIND_STRUCT
                || field->kind == CBLC_DATA_KIND_STRUCT_POINTER)
            {
                const t_cblc_struct_type *nested = cblc_find_struct_type(unit,
                    field->struct_type_name);
                const t_cblc_struct_field *resolved = nested
                    ? cblc_find_nested_pointer_field(unit, nested,
                        path + name_length, depth + 1) : NULL;
                if (resolved)
                    return resolved;
            }
        }
        field_index += 1;
    }

    /*
     * Deep instance-field names may be compacted to fit the target COBOL
     * identifier limit. Recover a compacted component only when its retained
     * prefix identifies exactly one field and the remaining path resolves
     * recursively. This avoids guessing from a partially matching path.
     */
    compressed_match = NULL;
    compressed_match_count = 0;
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field = &type->fields[field_index];
        const size_t name_length = std::strlen(field->cobol_name);
        const char *boundary = std::strchr(path, '-');

        while (boundary)
        {
            const size_t retained_length = static_cast<size_t>(boundary - path);
            const char *remaining_path = boundary;
            const t_cblc_struct_field *resolved = NULL;
            const int retained_name_matches = retained_length >= 4
                && retained_length < name_length
                && (std::strncmp(path, field->cobol_name, retained_length) == 0
                    || std::strncmp(path,
                        field->cobol_name + name_length - retained_length,
                        retained_length) == 0);

            if (retained_name_matches
                && (field->kind == CBLC_DATA_KIND_STRUCT
                    || field->kind == CBLC_DATA_KIND_STRUCT_POINTER))
            {
                const t_cblc_struct_type *nested = cblc_find_struct_type(unit,
                    field->struct_type_name);
                resolved = nested
                    ? cblc_find_nested_pointer_field(unit, nested,
                        remaining_path, depth + 1) : NULL;
            }
            else if (retained_name_matches
                && field->kind == CBLC_DATA_KIND_STRUCT_POINTER
                && remaining_path[1] == '\0')
                resolved = field;

            if (resolved)
            {
                if (!compressed_match)
                    compressed_match = resolved;
                else if (compressed_match != resolved)
                    compressed_match_count = 2;
            }
            boundary = std::strchr(boundary + 1, '-');
        }
        if (std::strlen(path) >= 4 && std::strlen(path) < name_length
            && (std::strncmp(path, field->cobol_name, std::strlen(path)) == 0
                || std::strncmp(path,
                    field->cobol_name + name_length - std::strlen(path),
                    std::strlen(path)) == 0)
            && field->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            if (!compressed_match)
                compressed_match = field;
            else if (compressed_match != field)
                compressed_match_count = 2;
        }
        field_index += 1;
    }
    return (compressed_match_count < 2 ? compressed_match : NULL);
}

struct s_cblc_struct_pointer_field_match
{
    const t_cblc_struct_field *field;
    char cobol_name[TRANSPILE_IDENTIFIER_MAX];
    size_t match_count;
};

static int cblc_find_struct_pointer_field_by_layout(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *instance_cobol_name, const char *pointer_name, size_t depth,
    s_cblc_struct_pointer_field_match *match)
{
    size_t field_index;

    if (!unit || !type || !instance_cobol_name || !pointer_name
        || !match || depth >= 32)
        return (FT_FAILURE);
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field = &type->fields[field_index];
        char field_cobol_name[TRANSPILE_STATEMENT_TEXT_MAX];

        /* Mirror cblc_emit_nested_pointer_layouts when resolving concrete
         * names emitted for based pointer fields. */
        if (cblc_build_instance_field_cobol_name(instance_cobol_name,
                field->cobol_name, field_cobol_name,
                sizeof(field_cobol_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (field->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
            const int based_name_status = cblc_build_struct_pointer_based_name(unit,
                field_cobol_name, field->struct_type_name, based_name,
                sizeof(based_name));

            if (std::strcmp(pointer_name, field_cobol_name) == 0
                || (based_name_status == FT_SUCCESS
                    && std::strcmp(pointer_name, based_name) == 0))
            {
                if (!match->field)
                {
                    match->field = field;
                    ft_strlcpy(match->cobol_name, field_cobol_name,
                        sizeof(match->cobol_name));
                    match->match_count = 1;
                }
                else if (match->field != field
                    || std::strcmp(match->cobol_name, field_cobol_name) != 0)
                    match->match_count = 2;
            }
        }
        if (field->kind == CBLC_DATA_KIND_STRUCT
            || field->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            const t_cblc_struct_type *nested = cblc_find_struct_type(unit,
                field->struct_type_name);

            if (nested && cblc_find_struct_pointer_field_by_layout(unit, nested,
                    field_cobol_name, pointer_name, depth + 1,
                    match) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        field_index += 1;
    }
    return (FT_SUCCESS);
}

static const t_cblc_data_item *cblc_resolve_struct_pointer_item(
    const t_cblc_translation_unit *unit, const char *pointer_name,
    t_cblc_data_item *synthetic_item,
    const t_cblc_data_item *receiver_context)
{
    const t_cblc_data_item *item;
    size_t item_index;

    if (!unit || !pointer_name || !synthetic_item)
        return (NULL);
    if (receiver_context
        && std::strncmp(pointer_name, "CBLC-THIS-",
            sizeof("CBLC-THIS-") - 1) == 0)
    {
        char receiver_pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];

        if (std::snprintf(receiver_pointer_name, sizeof(receiver_pointer_name),
                "%s-%s", receiver_context->cobol_name,
                pointer_name + sizeof("CBLC-THIS-") - 1) < 0
            || std::strlen(receiver_pointer_name) >= sizeof(receiver_pointer_name))
            return (NULL);
        return (cblc_resolve_struct_pointer_item(unit, receiver_pointer_name,
            synthetic_item, receiver_context));
    }
    item = cblc_find_data_item_by_cobol(unit, pointer_name);
    if (!item)
        item = cblc_find_data_item_for_emit_by_source(unit, pointer_name);
    if (item && item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        return item;
    /* Call sites can already carry the canonical BASED alias rather than the
     * underlying pointer data-item name. Resolve that spelling back to its
     * owning pointer item before attempting nested-field traversal. */
    item_index = 0;
    while (item_index < unit->data_count)
    {
        const t_cblc_data_item *candidate = &unit->data_items[item_index];
        char candidate_based_name[TRANSPILE_STATEMENT_TEXT_MAX];

        if (candidate->kind == CBLC_DATA_KIND_STRUCT_POINTER
            && cblc_build_struct_pointer_based_name(unit,
                candidate->cobol_name, candidate->struct_type_name,
                candidate_based_name, sizeof(candidate_based_name)) == FT_SUCCESS
            && std::strcmp(candidate_based_name, pointer_name) == 0)
            return (candidate);
        ++item_index;
    }
    /* Method bodies may be emitted for a based receiver that is not itself a
     * translation-unit data item. Resolve its concrete nested layout first;
     * scanning global roots cannot discover aliases emitted under this name. */
    if (receiver_context
        && (receiver_context->kind == CBLC_DATA_KIND_STRUCT
            || receiver_context->kind == CBLC_DATA_KIND_STRUCT_POINTER))
    {
        const t_cblc_struct_type *receiver_type;
        s_cblc_struct_pointer_field_match receiver_match;
        size_t receiver_name_length;

        receiver_type = cblc_find_struct_type(unit,
            receiver_context->struct_type_name);
        receiver_name_length = std::strlen(receiver_context->cobol_name);
        if (receiver_type && receiver_name_length > 0
            && std::strncmp(pointer_name, receiver_context->cobol_name,
                receiver_name_length) == 0
            && pointer_name[receiver_name_length] == '-')
        {
            const char *field_name = pointer_name + receiver_name_length + 1;
            size_t field_index = 0;

            while (field_index < receiver_type->field_count)
            {
                const t_cblc_struct_field *field =
                    &receiver_type->fields[field_index];

                if (std::strcmp(field_name, field->cobol_name) == 0)
                {
                    /* Resolve against the concrete receiver before considering
                     * other roots. In nested vector<string>, both vector<T>
                     * and string have a field named data, but string::data is
                     * CHAR_POINTER and must never be reinterpreted as T*. */
                    if (field->kind != CBLC_DATA_KIND_STRUCT_POINTER)
                        return (NULL);
                    *synthetic_item = *receiver_context;
                    synthetic_item->kind = CBLC_DATA_KIND_STRUCT_POINTER;
                    ft_strlcpy(synthetic_item->source_name, pointer_name,
                        sizeof(synthetic_item->source_name));
                    if (cblc_build_instance_field_cobol_name(
                            receiver_context->cobol_name, field->cobol_name,
                            synthetic_item->cobol_name,
                            sizeof(synthetic_item->cobol_name)) != FT_SUCCESS)
                        return (NULL);
                    ft_strlcpy(synthetic_item->struct_type_name,
                        field->struct_type_name,
                        sizeof(synthetic_item->struct_type_name));
                    synthetic_item->array_count = 0;
                    return synthetic_item;
                }
                field_index += 1;
            }
        }
        std::memset(&receiver_match, 0, sizeof(receiver_match));
        if (receiver_type
            && cblc_find_struct_pointer_field_by_layout(unit, receiver_type,
                receiver_context->cobol_name, pointer_name, 0,
                &receiver_match) == FT_SUCCESS
            && receiver_match.field && receiver_match.match_count == 1)
        {
            *synthetic_item = *receiver_context;
            synthetic_item->kind = CBLC_DATA_KIND_STRUCT_POINTER;
            ft_strlcpy(synthetic_item->source_name, pointer_name,
                sizeof(synthetic_item->source_name));
            ft_strlcpy(synthetic_item->cobol_name,
                receiver_match.cobol_name,
                sizeof(synthetic_item->cobol_name));
            ft_strlcpy(synthetic_item->struct_type_name,
                receiver_match.field->struct_type_name,
                sizeof(synthetic_item->struct_type_name));
            synthetic_item->array_count = 0;
            return synthetic_item;
        }
    }
    item_index = 0;
    while (item_index < unit->data_count)
    {
        const t_cblc_data_item *root = &unit->data_items[item_index];
        const t_cblc_struct_type *root_type;
        s_cblc_struct_pointer_field_match match;

        if (root->kind != CBLC_DATA_KIND_STRUCT_POINTER
            && root->kind != CBLC_DATA_KIND_STRUCT)
        {
            item_index += 1;
            continue ;
        }
        root_type = cblc_find_struct_type(unit, root->struct_type_name);
        std::memset(&match, 0, sizeof(match));
        if (root_type && root->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            char source_alias[TRANSPILE_IDENTIFIER_MAX];
            char cobol_alias[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_build_template_instance_names(root->cobol_name,
                    root->struct_type_name, source_alias, sizeof(source_alias),
                    cobol_alias, sizeof(cobol_alias)) == FT_SUCCESS
                && cblc_find_struct_pointer_field_by_layout(unit, root_type,
                    cobol_alias, pointer_name, 0, &match) != FT_SUCCESS)
            {
                item_index += 1;
                continue ;
            }
        }
        if (root_type && (!match.field || match.match_count != 1))
        {
            std::memset(&match, 0, sizeof(match));
            if (cblc_find_struct_pointer_field_by_layout(unit, root_type,
                    root->cobol_name, pointer_name, 0, &match) != FT_SUCCESS)
            {
                item_index += 1;
                continue ;
            }
        }
        if (root_type && match.field && match.match_count == 1)
        {
            *synthetic_item = *root;
            synthetic_item->kind = CBLC_DATA_KIND_STRUCT_POINTER;
            ft_strlcpy(synthetic_item->source_name, pointer_name,
                sizeof(synthetic_item->source_name));
            ft_strlcpy(synthetic_item->cobol_name, match.cobol_name,
                sizeof(synthetic_item->cobol_name));
            ft_strlcpy(synthetic_item->struct_type_name,
                match.field->struct_type_name,
                sizeof(synthetic_item->struct_type_name));
            synthetic_item->array_count = 0;
            return synthetic_item;
        }
        item_index += 1;
    }
    item_index = 0;
    while (item_index < unit->data_count)
    {
        const t_cblc_data_item *root = &unit->data_items[item_index];
        char root_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *field_path;
        const t_cblc_struct_type *root_type;
        const t_cblc_struct_field *field;
        int field_path_is_based_alias;

        field_path_is_based_alias = 0;
        if (root->kind != CBLC_DATA_KIND_STRUCT_POINTER
            && root->kind != CBLC_DATA_KIND_STRUCT)
        {
            item_index += 1;
            continue ;
        }
        if (std::strncmp(pointer_name, root->cobol_name,
                std::strlen(root->cobol_name)) == 0
            && pointer_name[std::strlen(root->cobol_name)] == '-')
            field_path = pointer_name + std::strlen(root->cobol_name);
        else if (root->kind == CBLC_DATA_KIND_STRUCT_POINTER
            && cblc_build_struct_pointer_based_name(unit, root->cobol_name,
                    root->struct_type_name,
                    root_based_name, sizeof(root_based_name)) == FT_SUCCESS
            && std::strncmp(pointer_name, root_based_name,
                std::strlen(root_based_name)) == 0
            && pointer_name[std::strlen(root_based_name)] == '-')
        {
            field_path = pointer_name + std::strlen(root_based_name);
            field_path_is_based_alias = 1;
        }
        else
        {
            item_index += 1;
            continue ;
        }
        root_type = cblc_find_struct_type(unit, root->struct_type_name);
        field = root_type ? cblc_find_nested_pointer_field(unit, root_type,
            field_path, 0) : NULL;
        if (field)
        {
            *synthetic_item = *root;
            synthetic_item->kind = CBLC_DATA_KIND_STRUCT_POINTER;
            ft_strlcpy(synthetic_item->source_name, pointer_name,
                sizeof(synthetic_item->source_name));
            if (field_path_is_based_alias)
                ft_strlcpy(synthetic_item->cobol_name, pointer_name,
                    sizeof(synthetic_item->cobol_name));
            else if (cblc_build_instance_field_cobol_name(root->cobol_name,
                    field_path + 1, synthetic_item->cobol_name,
                    sizeof(synthetic_item->cobol_name)) != FT_SUCCESS)
                    return (NULL);
            ft_strlcpy(synthetic_item->struct_type_name, field->struct_type_name,
                sizeof(synthetic_item->struct_type_name));
            synthetic_item->array_count = 0;
            return synthetic_item;
        }
        item_index += 1;
    }
    return (NULL);
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
    const char *reference, t_cobol_text_builder *builder, char *based_name,
    size_t based_name_size, const t_cblc_data_item *receiver_context,
    const t_cblc_function *function_context)
{
    const t_cblc_data_item *pointer_item;
    char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
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
    t_cblc_data_item synthetic_pointer_item;

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
    pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_reference,
        &synthetic_pointer_item, receiver_context);
    if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
    {
        const t_cblc_data_item *base_item;
        const t_cblc_struct_type *base_type;
        const t_cblc_struct_field *data_field;
        char data_source_name[TRANSPILE_IDENTIFIER_MAX];

        base_item = cblc_find_data_item_by_cobol(unit, pointer_reference);
        base_type = cblc_find_receiver_type(unit, base_item);
        data_field = cblc_find_field_on_type(base_type, "data");
        if (!data_field)
            data_field = cblc_find_field_on_type(base_type, "elements");
        if (!base_item || !base_type || !data_field)
            return (FT_FAILURE);
        cblc_build_field_source_name(base_item->source_name, data_field->source_name,
            data_source_name, sizeof(data_source_name));
        pointer_item = cblc_find_data_item_const(unit, data_source_name);
        if (!pointer_item)
        {
            size_t item_index;

            item_index = unit->data_count;
            while (item_index > 0)
            {
                const t_cblc_data_item *candidate;

                candidate = &unit->data_items[--item_index];
                if (candidate->kind != CBLC_DATA_KIND_STRUCT_POINTER
                    || std::strcmp(candidate->source_name, data_source_name) != 0)
                    continue ;
                if (base_item->owner_function_name[0] != '\0'
                    && std::strcmp(candidate->owner_function_name,
                        base_item->owner_function_name) != 0)
                    continue ;
                pointer_item = candidate;
                break ;
            }
        }
        if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
            return (FT_FAILURE);
    }
    if (cblc_build_struct_pointer_based_name(unit,
            pointer_item->cobol_name, pointer_item->struct_type_name,
            based_name, based_name_size) != FT_SUCCESS)
        return (FT_FAILURE);
    if (has_index)
    {
        if (std::snprintf(line, sizeof(line),
                pointer_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                    || pointer_item->kind == CBLC_DATA_KIND_CHAR
                    ? "           SET CBLC-PTR-CURSOR TO ADDRESS OF %s."
                    : "           SET CBLC-PTR-CURSOR TO %s.",
                pointer_item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strncmp(index_expression, "0", sizeof(index_expression)) != 0)
        {
            if (cblc_pointer_item_element_size(unit, pointer_item, &element_size) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_pointer_index_for_cobol(unit, index_expression,
                    translated_index, sizeof(translated_index), function_context,
                    builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE CBLC-PTR-OFFSET = (%s) * %zu.",
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
    size_t based_name_size, size_t slot,
    const t_cblc_data_item *receiver_context = NULL,
    t_cblc_data_kind *out_element_kind = NULL,
    const t_cblc_function *function_context = NULL)
{
    const t_cblc_data_item *pointer_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_struct_field *receiver_field;
    char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[256];
    t_cblc_data_kind element_kind;
    t_cblc_data_item synthetic_pointer_item;
    size_t pointer_name_length;
    size_t field_index;
    int has_index;
    size_t element_size;
    const char *name;

    if (!unit || !reference || !builder || !based_name || based_name_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_pointer_element_reference(unit, reference, &pointer_item, &element_kind,
            index_expression, sizeof(index_expression), &has_index) != FT_SUCCESS)
    {
        const char *open_bracket;
        const char *close_bracket;
        const char *base_suffix;
        size_t index_length;
        size_t receiver_name_length;

        open_bracket = std::strchr(reference, '[');
        close_bracket = open_bracket ? std::strchr(open_bracket, ']') : NULL;
        if (!open_bracket || !close_bracket || close_bracket[1] != '\0')
            return (FT_FAILURE);
        pointer_name_length = static_cast<size_t>(open_bracket - reference);
        if (pointer_name_length == 0 || pointer_name_length >= sizeof(pointer_name))
            return (FT_FAILURE);
        std::memcpy(pointer_name, reference, pointer_name_length);
        pointer_name[pointer_name_length] = '\0';
        base_suffix = std::strrchr(pointer_name, '-');
        if (function_context && base_suffix
            && std::strncmp(pointer_name, "CBLC-SCOPE-",
                sizeof("CBLC-SCOPE-") - 1) == 0)
        {
            size_t parameter_index = 0;

            base_suffix += 1;
            while (parameter_index < function_context->parameter_count)
            {
                const t_cblc_parameter *parameter =
                    &function_context->parameters[parameter_index];
                const char *parameter_name = parameter->source_name;
                size_t suffix_index = 0;
                size_t name_index = 0;

                if (parameter->kind
                    != TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER)
                {
                    parameter_index += 1;
                    continue ;
                }
                while (base_suffix[suffix_index] != '\0'
                    && parameter_name[name_index] != '\0'
                    && std::toupper(static_cast<unsigned char>(
                            base_suffix[suffix_index]))
                        == std::toupper(static_cast<unsigned char>(
                            parameter_name[name_index])))
                {
                    suffix_index += 1;
                    name_index += 1;
                }
                if (base_suffix[suffix_index] == '\0'
                    && parameter_name[name_index] == '\0')
                {
                    index_length = static_cast<size_t>(
                        close_bracket - open_bracket - 1);
                    if (index_length == 0
                        || index_length + 1 > sizeof(index_expression))
                        return (FT_FAILURE);
                    std::memcpy(index_expression, open_bracket + 1,
                        index_length);
                    index_expression[index_length] = '\0';
                    std::memset(&synthetic_pointer_item, 0,
                        sizeof(synthetic_pointer_item));
                    synthetic_pointer_item.kind = CBLC_DATA_KIND_CHAR_POINTER;
                    ft_strlcpy(synthetic_pointer_item.cobol_name, pointer_name,
                        sizeof(synthetic_pointer_item.cobol_name));
                    pointer_item = &synthetic_pointer_item;
                    element_kind = CBLC_DATA_KIND_CHAR;
                    has_index = 1;
                    break ;
                }
                parameter_index += 1;
            }
        }
        if (!has_index)
        {
        receiver_field = NULL;
        if (receiver_context
            && (receiver_context->kind == CBLC_DATA_KIND_STRUCT
                || receiver_context->kind == CBLC_DATA_KIND_STRUCT_POINTER))
        {
            receiver_name_length = std::strlen(receiver_context->cobol_name);
            if (receiver_name_length > 0
                && std::strncmp(pointer_name, receiver_context->cobol_name,
                    receiver_name_length) == 0
                && pointer_name[receiver_name_length] == '-')
            {
                receiver_type = cblc_find_struct_type(unit,
                    receiver_context->struct_type_name);
                if (!receiver_type)
                    return (FT_FAILURE);
                field_index = 0;
                while (field_index < receiver_type->field_count)
                {
                    if (std::strcmp(pointer_name + receiver_name_length + 1,
                            receiver_type->fields[field_index].cobol_name) == 0)
                    {
                        receiver_field = &receiver_type->fields[field_index];
                        break ;
                    }
                    field_index += 1;
                }
            }
        }
        if (!receiver_field)
        {
            if (!receiver_context
                || (receiver_context->kind != CBLC_DATA_KIND_STRUCT
                    && receiver_context->kind != CBLC_DATA_KIND_STRUCT_POINTER))
                return (FT_FAILURE);
            receiver_name_length = std::strlen(receiver_context->cobol_name);
            if (receiver_name_length == 0
                || std::strncmp(pointer_name, receiver_context->cobol_name,
                    receiver_name_length) != 0
                || pointer_name[receiver_name_length] != '-')
                return (FT_FAILURE);
            receiver_type = cblc_find_struct_type(unit,
                receiver_context->struct_type_name);
            if (!receiver_type)
                return (FT_FAILURE);
            field_index = 0;
            while (field_index < receiver_type->field_count)
            {
                if (std::strcmp(pointer_name + receiver_name_length + 1,
                        receiver_type->fields[field_index].cobol_name) == 0)
                {
                    receiver_field = &receiver_type->fields[field_index];
                    break ;
                }
                field_index += 1;
            }
        }
        if (receiver_field->kind != CBLC_DATA_KIND_CHAR_POINTER
            && receiver_field->kind != CBLC_DATA_KIND_INT_POINTER)
            return (FT_FAILURE);
        index_length = static_cast<size_t>(close_bracket - open_bracket - 1);
        if (index_length == 0 || index_length + 1 > sizeof(index_expression))
            return (FT_FAILURE);
        std::memcpy(index_expression, open_bracket + 1, index_length);
        index_expression[index_length] = '\0';
        std::memset(&synthetic_pointer_item, 0, sizeof(synthetic_pointer_item));
        synthetic_pointer_item.kind = receiver_field->kind;
        synthetic_pointer_item.length = receiver_field->length;
        ft_strlcpy(synthetic_pointer_item.cobol_name, pointer_name,
            sizeof(synthetic_pointer_item.cobol_name));
        pointer_item = &synthetic_pointer_item;
        element_kind = receiver_field->kind == CBLC_DATA_KIND_CHAR_POINTER
            ? CBLC_DATA_KIND_CHAR : CBLC_DATA_KIND_INT;
        has_index = 1;
        }
    }
    if (out_element_kind)
        *out_element_kind = element_kind;
    name = cblc_pointer_based_name(element_kind);
    if (!name)
        return (FT_FAILURE);
    if (slot == 0)
        ft_strlcpy(based_name, name, based_name_size);
    else if (std::snprintf(based_name, based_name_size, "%s-%zu", name, slot + 1) < 0)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line),
            pointer_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                || pointer_item->kind == CBLC_DATA_KIND_CHAR
                ? "           SET CBLC-PTR-CURSOR TO ADDRESS OF %s."
                : "           SET CBLC-PTR-CURSOR TO %s.",
            pointer_item->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (has_index && std::strncmp(index_expression, "0", sizeof(index_expression)) != 0)
    {
        if (cblc_pointer_item_element_size(unit, pointer_item, &element_size) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_pointer_index_for_cobol(unit, index_expression,
                translated_index, sizeof(translated_index), function_context, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           COMPUTE CBLC-PTR-OFFSET = (%s) * %zu.",
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
    const char *reference, t_cobol_text_builder *builder, char *based_name,
    size_t based_name_size, const t_cblc_data_item *receiver_context,
    const t_cblc_function *function_context)
{
    return (cblc_emit_prepare_pointer_element_slot(unit, reference, builder, based_name,
        based_name_size, 0, receiver_context, NULL, function_context));
}

static int cblc_emit_pointer_assignment_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder, char *line,
    size_t line_size, const t_cblc_data_item *receiver_context,
    const t_cblc_function *function_context)
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
    if (statement->is_literal
        && std::strcmp(statement->source, "CBLC-NUL-CHAR") == 0)
    {
        char target_text[TRANSPILE_STATEMENT_TEXT_MAX];
        const int prepare_status = cblc_emit_prepare_pointer_element(unit,
            statement->target, builder, target_text, sizeof(target_text),
            receiver_context, function_context);

        if (prepare_status == FT_SUCCESS)
        {
            if (std::snprintf(line, line_size,
                    "           MOVE LOW-VALUES TO %s", target_text) < 0)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (cblc_translate_expression_for_cobol(statement->target, target_text,
                sizeof(target_text)) != FT_SUCCESS
            || std::snprintf(line, line_size,
                "           MOVE LOW-VALUES TO %s", target_text) < 0)
        {
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    if (unit->is_native_standard_library && std::strchr(statement->target, '['))
    {
        char target_base[TRANSPILE_IDENTIFIER_MAX];

        if (cblc_parse_cobol_reference_base(statement->target, target_base,
                sizeof(target_base)) == FT_SUCCESS)
        {
            target_item = cblc_find_data_item_by_cobol(unit, target_base);
            if (target_item && target_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                && !receiver_context)
                return (FT_FAILURE);
        }
    }
    assignment_source = statement->source;
    if (!statement->is_literal
        && cblc_strip_pointer_cast_reference(statement->source, stripped_source,
            sizeof(stripped_source)) == FT_SUCCESS)
        assignment_source = stripped_source;
    if (cblc_emit_prepare_struct_pointer_value(unit, statement->target, builder,
            target_based, sizeof(target_based), receiver_context) == FT_SUCCESS)
    {
        if (statement->is_literal)
            return (FT_FAILURE);
        if (cblc_emit_prepare_struct_pointer_value(unit, assignment_source, builder,
                source_based, sizeof(source_based), receiver_context) == FT_SUCCESS)
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
                source_based, sizeof(source_based), 1, receiver_context, NULL,
                function_context) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_emit_prepare_struct_pointer_field(unit, assignment_source, builder,
                source_based, sizeof(source_based)) == FT_SUCCESS)
            ft_strlcpy(source_text, source_based, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(assignment_source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if ((std::strcmp(target_based, "CBLC-PTR-CHAR") == 0
                || std::strcmp(target_based, "CBLC-PTR-CHAR-2") == 0)
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
                        || cobol_text_builder_append_string(builder, "           ") != FT_SUCCESS
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
    {
        const int pointer_element_status = cblc_emit_prepare_pointer_element(unit,
            statement->target, builder, target_based, sizeof(target_based),
            receiver_context, function_context);

        if (pointer_element_status == FT_SUCCESS)
        {
            if (statement->is_literal)
                ft_strlcpy(source_text, statement->source, sizeof(source_text));
            else if (cblc_emit_prepare_pointer_element_slot(unit, assignment_source,
                    builder, source_based, sizeof(source_based), 1,
                    receiver_context, NULL, function_context) == FT_SUCCESS)
                ft_strlcpy(source_text, source_based, sizeof(source_text));
            else if (cblc_translate_expression_for_cobol(assignment_source,
                    source_text, sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if ((std::strcmp(target_based, "CBLC-PTR-CHAR") == 0
                    || std::strcmp(target_based, "CBLC-PTR-CHAR-2") == 0)
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
                            || cobol_text_builder_append_string(builder,
                                "           ") != FT_SUCCESS
                            || cobol_text_builder_append_string(builder,
                                to_clause) != FT_SUCCESS
                            || cobol_text_builder_append_string(builder, "\n")
                                != FT_SUCCESS)
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
            builder, source_based, sizeof(source_based), receiver_context,
            function_context) == FT_SUCCESS)
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
        if (cblc_translate_pointer_index_for_cobol(unit, operand,
                translated_operand, sizeof(translated_operand), NULL, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, line_size, "           COMPUTE CBLC-PTR-OFFSET = (%s) * %zu.",
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

static int cblc_method_field_is_char_buffer(const t_cblc_struct_field *field)
{
    return (field && field->kind == CBLC_DATA_KIND_CHAR_BUFFER);
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
                const char *parameter_names[3];
                char prefix[TRANSPILE_IDENTIFIER_MAX + 32];
                size_t prefix_length;
                size_t parameter_name_index;

                parameter = &method->parameters[parameter_index];
                parameter_names[0] = parameter->cobol_name;
                parameter_names[1] = parameter->source_name;
                parameter_names[2] = parameter->actual_source_name;
                parameter_name_index = 0;
                while (parameter_name_index < 3 && !matched)
                {
                    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                        && (std::strcmp(parameter->type_name,
                                receiver->struct_type_name) == 0
                            || (type->is_template_instantiation
                                && type->template_source_name[0] != '\0'
                                && std::strcmp(parameter->type_name,
                                    type->template_source_name) == 0))
                        && parameter_names[parameter_name_index]
                        && parameter_names[parameter_name_index][0] != '\0'
                        && std::snprintf(prefix, sizeof(prefix), "%s->",
                            parameter_names[parameter_name_index]) >= 0)
                    {
                        prefix_length = std::strlen(prefix);
                        if (std::strncmp(input + input_index, prefix,
                                prefix_length) == 0)
                        {
                            const char *field_source;
                            size_t candidate_index;

                            field_source = input + input_index + prefix_length;
                            candidate_index = 0;
                            while (candidate_index < type->field_count)
                            {
                                const t_cblc_struct_field *field;
                                size_t field_length;
                                char pointer_based_name[TRANSPILE_IDENTIFIER_MAX];
                                char replacement[TRANSPILE_STATEMENT_TEXT_MAX];

                                field = &type->fields[candidate_index];
                                field_length = std::strlen(field->source_name);
                                if (std::strncmp(field_source, field->source_name,
                                        field_length) == 0
                                    && (field_source[field_length] == '\0'
                                        || field_source[field_length] == '.'
                                        || field_source[field_length] == '['
                                        || field_source[field_length] == ','
                                        || field_source[field_length] == ' '
                                        || field_source[field_length] == ')'
                                        || field_source[field_length] == '+'
                                        || field_source[field_length] == '-'
                                        || field_source[field_length] == '*'
                                        || field_source[field_length] == '/'
                                        || field_source[field_length] == '<'
                                        || field_source[field_length] == '>'
                                        || field_source[field_length] == '='
                                        || field_source[field_length] == ';'))
                                {
                                    if (cblc_build_struct_pointer_based_name(unit,
                                            parameter->cobol_name,
                                            receiver->struct_type_name,
                                            pointer_based_name,
                                            sizeof(pointer_based_name)) != FT_SUCCESS
                                        || cblc_build_instance_field_cobol_name(
                                            pointer_based_name, field->cobol_name,
                                            replacement, sizeof(replacement)) != FT_SUCCESS
                                        || ft_strlcat(output, replacement,
                                            output_size) >= output_size)
                                        return (FT_FAILURE);
                                    input_index += prefix_length + field_length;
                                    matched = 1;
                                    break ;
                                }
                                candidate_index += 1;
                            }
                        }
                        if (matched)
                            break ;
                        parameter_name_index += 1;
                        continue ;
                    }
                    if (!parameter_names[parameter_name_index]
                        || parameter_names[parameter_name_index][0] == '\0'
                        || std::snprintf(prefix, sizeof(prefix), "%s.",
                            parameter_names[parameter_name_index]) < 0)
                    {
                        parameter_name_index += 1;
                        continue ;
                    }
                    prefix_length = std::strlen(prefix);
                    if (std::strncmp(input + input_index, prefix,
                            prefix_length) != 0)
                    {
                        parameter_name_index += 1;
                        continue ;
                    }
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
                        const char *field_cobol_suffix;
                        size_t field_cobol_suffix_length;
                        int source_name_matches;
                        int cobol_name_matches;

                        field = &type->fields[candidate_index];
                        field_length = std::strlen(field->source_name);
                        field_cobol_suffix = field->cobol_name;
                        if (std::strncmp(field_cobol_suffix, "CBLC-USER-",
                                sizeof("CBLC-USER-") - 1) == 0)
                            field_cobol_suffix += sizeof("CBLC-USER-") - 1;
                        field_cobol_suffix_length = std::strlen(field_cobol_suffix);
                        source_name_matches = cblc_ascii_identifier_equal(field_source,
                            field->source_name, field_length);
                        cobol_name_matches = cblc_ascii_identifier_equal(field_source,
                            field_cobol_suffix, field_cobol_suffix_length);
                        if (((source_name_matches
                                    && (field_source[field_length] == '\0'
                                        || field_source[field_length] == '['
                                        || field_source[field_length] == '.'
                                        || field_source[field_length] == ','
                                        || field_source[field_length] == ' '
                                        || field_source[field_length] == ')'
                                        || field_source[field_length] == '+'
                                        || field_source[field_length] == '-'
                                        || field_source[field_length] == '*'
                                        || field_source[field_length] == '/'
                                        || field_source[field_length] == '<'
                                        || field_source[field_length] == '>'))
                                || (cobol_name_matches
                                    && (field_source[field_cobol_suffix_length] == '\0'
                                        || field_source[field_cobol_suffix_length] == ' '
                                        || field_source[field_cobol_suffix_length] == ')'
                                        || field_source[field_cobol_suffix_length] == '+'
                                        || field_source[field_cobol_suffix_length] == '-'
                                        || field_source[field_cobol_suffix_length] == '*'
                                        || field_source[field_cobol_suffix_length] == '/'
                                        || field_source[field_cobol_suffix_length] == '<'
                                        || field_source[field_cobol_suffix_length] == '>'))))
                        {
                            char replacement[TRANSPILE_STATEMENT_TEXT_MAX];
                            size_t matched_field_length;

                            if (cblc_build_instance_field_cobol_name(
                                    parameter->cobol_name, field->cobol_name,
                                    replacement, sizeof(replacement)) != FT_SUCCESS
                                || ft_strlcat(output, replacement, output_size) >= output_size)
                                return (FT_FAILURE);
                            matched_field_length = source_name_matches
                                ? field_length : field_cobol_suffix_length;
                            input_index += member_prefix_length + matched_field_length;
                            matched = 1;
                            break ;
                        }
                        candidate_index += 1;
                    }
                    if (matched)
                        break ;
                    parameter_name_index += 1;
                }
                if (matched)
                    break ;
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
                    && cblc_method_field_is_char_buffer(field))
                {
                    ft_strlcat(replacement, "-LEN", sizeof(replacement));
                    input_index += 4;
                }
                else if (input[input_index] == '['
                    && cblc_method_field_is_char_buffer(field))
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
    const char *input, const char *protected_receiver, char *output,
    size_t output_size, const t_cblc_method *method_context = NULL)
{
    size_t input_index;
    size_t protected_receiver_length;

    if (!unit || !input || !output || output_size == 0)
        return (FT_FAILURE);
    protected_receiver_length = protected_receiver
        ? std::strlen(protected_receiver) : 0;
    output[0] = '\0';
    input_index = 0;
    while (input[input_index] != '\0')
    {
        if (protected_receiver_length > 0
            && std::strncmp(input + input_index, protected_receiver,
                protected_receiver_length) == 0
            && (input[input_index + protected_receiver_length] == '-'
                || input[input_index + protected_receiver_length] == '['
                || input[input_index + protected_receiver_length] == '\0'))
        {
            size_t bracket_depth;

            if (ft_strlcat(output, protected_receiver, output_size) >= output_size)
                return (FT_FAILURE);
            input_index += protected_receiver_length;
            bracket_depth = 0;
            while (input[input_index] != '\0')
            {
                const char current = input[input_index];

                if (bracket_depth == 0
                    && (current == ' ' || current == '\t' || current == ','
                        || current == ')' || current == '=' || current == '+'
                        || current == '*' || current == '/' || current == '<'
                        || current == '>'))
                    break ;
                if (current == '[')
                    bracket_depth += 1;
                else if (current == ']' && bracket_depth > 0)
                    bracket_depth -= 1;
                {
                    char character[2];

                    character[0] = input[input_index++];
                    character[1] = '\0';
                    if (ft_strlcat(output, character, output_size) >= output_size)
                        return (FT_FAILURE);
                }
            }
            continue ;
        }
        if (method_context)
        {
            const t_cblc_data_item *scoped_item = NULL;
            const char *scoped_parameter_replacement = NULL;
            size_t scoped_name_length = 0;
            size_t scope_begin = method_context->template_storage_data_start;
            size_t scope_end = scope_begin
                + method_context->template_storage_data_count;

            if (method_context->template_storage_data_count == 0
                || scope_begin >= unit->data_count)
            {
                scope_begin = 0;
                scope_end = unit->data_count;
            }
            else if (scope_end < scope_begin || scope_end > unit->data_count)
                scope_end = unit->data_count;
            for (size_t parameter_index = 0;
                parameter_index < method_context->parameter_count;
                ++parameter_index)
            {
                const t_cblc_parameter *parameter =
                    &method_context->parameters[parameter_index];
                const char *names[] = {parameter->actual_source_name,
                    parameter->source_name};

                for (const char *name : names)
                {
                    const size_t name_length = std::strlen(name);
                    const char after = input[input_index + name_length];

                    if (name_length > scoped_name_length
                        && name_length > 0
                        && std::strncmp(input + input_index, name,
                            name_length) == 0
                        && (after == '\0' || std::isspace(
                                static_cast<unsigned char>(after))
                            || after == ')' || after == '(' || after == '['
                            || after == '+' || after == '-' || after == '*'
                            || after == '/' || after == '<' || after == '>'
                            || after == '='))
                    {
                        scoped_parameter_replacement = parameter->cobol_name;
                        scoped_name_length = name_length;
                        break ;
                    }
                }
                if (scoped_name_length > 0)
                {
                    scoped_item = NULL;
                    break ;
                }
            }
            if (scoped_name_length == 0)
            {
                for (size_t item_index = scope_begin; item_index < scope_end;
                    ++item_index)
                {
                    const t_cblc_data_item *candidate =
                        &unit->data_items[item_index];
                    const char *names[] = {candidate->local_source_name,
                        candidate->source_name};

                    /* The storage range is captured from this concrete
                     * specialization. Its owner spelling can retain the
                     * primary template name, so the range—not that spelling—
                     * is the authoritative scope boundary here. */
                    if (!candidate->is_function_local)
                        continue ;
                    for (const char *name : names)
                    {
                        const size_t name_length = std::strlen(name);
                        const char after = input[input_index + name_length];

                        if (name_length > scoped_name_length
                            && name_length > 0
                            && std::strncmp(input + input_index, name,
                                name_length) == 0
                            && (after == '\0' || std::isspace(
                                    static_cast<unsigned char>(after))
                                || after == ')' || after == '(' || after == '['
                                || after == '+' || after == '-' || after == '*'
                                || after == '/' || after == '<' || after == '>'
                                || after == '='))
                        {
                            scoped_item = candidate;
                            scoped_name_length = name_length;
                        }
                    }
                }
            }
            if (scoped_name_length > 0)
            {
                const char *replacement = scoped_item
                    ? scoped_item->cobol_name
                    : scoped_parameter_replacement;

                if (!replacement
                    || ft_strlcat(output, replacement, output_size)
                        >= output_size)
                    return (FT_FAILURE);
                input_index += scoped_name_length;
                continue ;
            }
        }
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
            if (method_context && item->is_function_local
                && method_context->scope_source_name[0] != '\0'
                && std::strcmp(item->owner_function_name,
                    method_context->scope_source_name) != 0)
            {
                data_index += 1;
                continue ;
            }
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

static int cblc_replace_method_local_reference(const char *input,
    const char *local_name, const char *cobol_name, char *output,
    size_t output_size)
{
    size_t input_index;
    size_t output_length;
    size_t local_length;

    if (!input || !local_name || local_name[0] == '\0' || !cobol_name
        || !output || output_size == 0)
        return (FT_FAILURE);
    local_length = std::strlen(local_name);
    output_length = 0;
    input_index = 0;
    while (input[input_index] != '\0')
    {
        const char before = input_index > 0 ? input[input_index - 1] : '\0';
        const char after = input[input_index + local_length];
        const int before_is_identifier =
            (before >= 'A' && before <= 'Z') || (before >= 'a' && before <= 'z')
            || (before >= '0' && before <= '9') || before == '_';
        const int after_is_identifier =
            (after >= 'A' && after <= 'Z') || (after >= 'a' && after <= 'z')
            || (after >= '0' && after <= '9') || after == '_';

        if (!before_is_identifier
            && std::strncmp(input + input_index, local_name, local_length) == 0
            && !after_is_identifier)
        {
            const size_t replacement_length = std::strlen(cobol_name);

            if (replacement_length >= output_size - output_length)
                return (FT_FAILURE);
            std::memcpy(output + output_length, cobol_name, replacement_length);
            output_length += replacement_length;
            input_index += local_length;
        }
        else
        {
            if (output_length + 1 >= output_size)
                return (FT_FAILURE);
            output[output_length++] = input[input_index++];
        }
    }
    output[output_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_rewrite_scoped_method_data_references(
    const t_cblc_translation_unit *unit, const t_cblc_method *method,
    const char *input, const char *protected_receiver, char *output,
    size_t output_size)
{
    char rewritten[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t data_index;
    size_t data_end;

    if (!unit || !method || !input || !output || output_size == 0
        || cblc_rewrite_method_data_references(unit, input, protected_receiver,
            rewritten, sizeof(rewritten), method) != FT_SUCCESS)
        return (FT_FAILURE);
    data_index = method->template_storage_data_start;
    data_end = data_index + method->template_storage_data_count;
    if (method->template_storage_data_count == 0)
    {
        ft_strlcpy(output, rewritten, output_size);
        return (FT_SUCCESS);
    }
    if (data_index >= unit->data_count || data_end < data_index)
        return (FT_FAILURE);
    if (data_end > unit->data_count)
        data_end = unit->data_count;
    while (data_index < data_end)
    {
        const t_cblc_data_item *local_item = &unit->data_items[data_index];
        if (local_item->is_function_local
            && local_item->local_source_name[0] != '\0')
        {
            if (cblc_replace_method_local_reference(rewritten,
                    local_item->local_source_name, local_item->cobol_name,
                    output, output_size) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(rewritten, output, sizeof(rewritten));
        }
        data_index += 1;
    }
    ft_strlcpy(output, rewritten, output_size);
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

static int cblc_replace_method_parameter_reference(const char *input,
    const char *parameter_name, const char *cobol_name, char *output,
    size_t output_size, int allow_cobol_suffix)
{
    size_t input_index;
    size_t output_length;
    size_t parameter_length;

    if (!input || !parameter_name || parameter_name[0] == '\0' || !cobol_name
        || !output || output_size == 0)
        return (FT_FAILURE);
    parameter_length = std::strlen(parameter_name);
    input_index = 0;
    output_length = 0;
    while (input[input_index] != '\0')
    {
        const char before = input_index > 0 ? input[input_index - 1] : '\0';
        const char after = input[input_index + parameter_length];
        const int before_is_identifier =
            std::isalnum(static_cast<unsigned char>(before)) || before == '_';
        const int after_is_identifier =
            std::isalnum(static_cast<unsigned char>(after)) || after == '_';
        const int member_reference = before == '.' || before == '>';
        const int suffix_allowed = allow_cobol_suffix && after == '-';

        if (!before_is_identifier && !after_is_identifier && !member_reference
            && (suffix_allowed || after != '-')
            && std::strncmp(input + input_index, parameter_name,
                parameter_length) == 0)
        {
            const size_t replacement_length = std::strlen(cobol_name);

            if (replacement_length >= output_size - output_length)
                return (FT_FAILURE);
            std::memcpy(output + output_length, cobol_name, replacement_length);
            output_length += replacement_length;
            input_index += parameter_length;
        }
        else
        {
            if (output_length + 1 >= output_size)
                return (FT_FAILURE);
            output[output_length++] = input[input_index++];
        }
    }
    output[output_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_rewrite_method_parameter_references(const t_cblc_translation_unit *unit,
    const t_cblc_method *method, const char *input, char *output, size_t output_size)
{
    size_t index;
    size_t data_begin;
    size_t data_end;

    if (!unit || !method || !input || !output || output_size == 0)
        return (FT_FAILURE);
    ft_strlcpy(output, input, output_size);
    data_begin = method->template_storage_data_start;
    data_end = data_begin + method->template_storage_data_count;
    if (method->template_storage_data_count == 0 || data_begin >= unit->data_count)
    {
        data_begin = 0;
        data_end = unit->data_count;
    }
    else if (data_end < data_begin || data_end > unit->data_count)
        data_end = unit->data_count;
    index = 0;
    while (index < method->parameter_count)
    {
        const t_cblc_data_item *alias_item;
        const char *source_name;
        size_t data_index;

        alias_item = NULL;
        data_index = data_begin;
        while (data_index < data_end)
        {
            if (unit->data_items[data_index].is_alias
                && std::strcmp(unit->data_items[data_index].source_name,
                    method->parameters[index].source_name) == 0)
            {
                alias_item = &unit->data_items[data_index];
                break ;
            }
            data_index += 1;
        }
        source_name = method->parameters[index].actual_source_name[0] != '\0'
            ? method->parameters[index].actual_source_name
            : method->parameters[index].source_name;
        if (alias_item && alias_item->is_alias
            && alias_item->cobol_name[0] != '\0'
            && method->parameters[index].cobol_name[0] != '\0')
        {
            char replaced[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_replace_method_parameter_reference(output,
                    alias_item->cobol_name,
                    method->parameters[index].cobol_name, replaced,
                    sizeof(replaced), 1) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(output, replaced, output_size);
        }
        if (source_name[0] != '\0' && method->parameters[index].cobol_name[0] != '\0')
        {
            char replaced[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_replace_method_parameter_reference(output, source_name,
                    method->parameters[index].cobol_name, replaced,
                    sizeof(replaced), 0) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(output, replaced, output_size);
        }
        if (method->parameters[index].source_name[0] != '\0'
            && method->parameters[index].cobol_name[0] != '\0')
        {
            char replaced[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_replace_method_parameter_reference(output,
                    method->parameters[index].source_name,
                    method->parameters[index].cobol_name, replaced,
                    sizeof(replaced), 0) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(output, replaced, output_size);
        }
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
            if (cblc_method_field_is_char_buffer(&type->fields[field_index])
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

static int cblc_method_result_name(const t_cblc_data_item *receiver,
    const t_cblc_method *method, char *buffer, size_t buffer_size);
static int cblc_method_return_paragraph_name(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    char *buffer, size_t buffer_size);
static const t_cblc_struct_field *cblc_method_direct_pointer_field(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const char *reference);
static int cblc_emit_allocation_exception(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, const char *allocation_line, const char *pointer_name,
    const char *exception_type_name, t_cobol_text_builder *builder);

static int cblc_translate_pointer_index_for_cobol(
    const t_cblc_translation_unit *unit, const char *index_expression,
    char *translated, size_t translated_size,
    const t_cblc_function *function_context,
    const t_cobol_text_builder *builder)
{
    const t_cblc_data_item *index_item;
    size_t matches;

    if (!unit || !index_expression || !translated || translated_size == 0)
        return (FT_FAILURE);
    index_item = NULL;
    if (builder && builder->active_method
        && builder->active_method->template_storage_data_count > 0)
    {
        size_t index;
        size_t end;

        index = builder->active_method->template_storage_data_start;
        end = index + builder->active_method->template_storage_data_count;
        if (end > unit->data_count)
            end = unit->data_count;
        while (index < end)
        {
            const t_cblc_data_item *candidate = &unit->data_items[index++];

            if (candidate->is_function_local
                && candidate->kind == CBLC_DATA_KIND_INT
                && (!function_context
                    || function_context->source_name[0] == '\0'
                    || std::strcmp(candidate->owner_function_name,
                        function_context->source_name) == 0)
                && (std::strcmp(candidate->source_name, index_expression) == 0
                    || std::strcmp(candidate->local_source_name,
                        index_expression) == 0))
            {
                index_item = candidate;
                break ;
            }
        }
    }
    if (!index_item && builder && builder->active_constructor
        && builder->active_constructor->template_storage_data_count > 0)
    {
        size_t index;
        size_t end;

        index = builder->active_constructor->template_storage_data_start;
        end = index + builder->active_constructor->template_storage_data_count;
        if (end > unit->data_count)
            end = unit->data_count;
        while (index < end)
        {
            const t_cblc_data_item *candidate = &unit->data_items[index++];

            if (candidate->is_function_local
                && candidate->kind == CBLC_DATA_KIND_INT
                && (!function_context
                    || function_context->source_name[0] == '\0'
                    || std::strcmp(candidate->owner_function_name,
                        function_context->source_name) == 0)
                && (std::strcmp(candidate->source_name, index_expression) == 0
                    || std::strcmp(candidate->local_source_name,
                        index_expression) == 0))
            {
                index_item = candidate;
                break ;
            }
        }
    }
    if (function_context && function_context->source_name[0] != '\0')
    {
        size_t index = unit->data_count;

        while (!index_item && index > 0)
        {
            const t_cblc_data_item *candidate = &unit->data_items[--index];

            if (candidate->is_function_local
                && std::strcmp(candidate->owner_function_name,
                    function_context->source_name) == 0
                && (std::strcmp(candidate->source_name, index_expression) == 0
                    || std::strcmp(candidate->local_source_name,
                        index_expression) == 0))
            {
                index_item = candidate;
                break ;
            }
        }
    }
    if (!index_item)
    {
        size_t index = unit->data_count;
        matches = 0;

        while (index > 0)
        {
            const t_cblc_data_item *candidate = &unit->data_items[--index];

            if (candidate->is_function_local
                && candidate->kind == CBLC_DATA_KIND_INT
                && std::strcmp(candidate->local_source_name,
                    index_expression) == 0)
            {
                if (builder && builder->active_function_name
                    && std::strcmp(candidate->owner_function_name,
                        builder->active_function_name) == 0)
                {
                    index_item = candidate;
                    break ;
                }
                index_item = candidate;
                matches += 1;
            }
        }
        if (matches != 1 && !(builder && builder->active_function_name))
            index_item = NULL;
    }
    if (!index_item)
        index_item = cblc_find_data_item_for_emit_by_source(unit, index_expression);
    if (index_item && index_item->kind == CBLC_DATA_KIND_INT)
    {
        if (std::strlen(index_item->cobol_name) >= translated_size)
            return (FT_FAILURE);
        ft_strlcpy(translated, index_item->cobol_name, translated_size);
        return (FT_SUCCESS);
    }
    return (cblc_translate_expression_for_cobol(index_expression, translated,
        translated_size));
}

static int cblc_translate_condition_for_cobol(
    const t_cblc_translation_unit *unit, const char *expression,
    t_cobol_text_builder *builder, char *translated, size_t translated_size);
static const t_cblc_constructor *cblc_find_copy_constructor(
    const t_cblc_struct_type *type);
static int cblc_constructor_paragraph_name(const t_cblc_data_item *receiver,
    size_t constructor_index, char *buffer, size_t buffer_size);
static int cblc_constructor_return_paragraph_name(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    size_t constructor_index, char *buffer, size_t buffer_size);

static int cblc_emit_struct_copy_return(const t_cblc_translation_unit *unit,
    const char *target_name, const char *return_type_name,
    const char *source_reference,
    const t_cblc_data_item *receiver_context, t_cobol_text_builder *builder,
    int *out_handled)
{
    const t_cblc_data_item *target_item;
    const t_cblc_data_item *source_item;
    const t_cblc_struct_type *return_type;
    const t_cblc_constructor *copy_constructor;
    char source_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t constructor_index;
    t_cblc_data_item based_receiver;
    t_cblc_data_item synthetic_target_item;
    char constructor_name[TRANSPILE_IDENTIFIER_MAX];
    char return_label[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !target_name || !return_type_name || !source_reference
        || !builder || !out_handled)
        return (FT_FAILURE);
    *out_handled = 0;
    target_item = cblc_find_data_item_by_cobol(unit, target_name);
    if (!target_item)
        target_item = cblc_find_data_item_for_emit_by_source(unit, target_name);
    if (!target_item || target_item->kind != CBLC_DATA_KIND_STRUCT)
    {
        return_type = cblc_find_struct_type(unit, return_type_name);
        if (!return_type)
            return (FT_SUCCESS);
        std::memset(&synthetic_target_item, 0, sizeof(synthetic_target_item));
        synthetic_target_item.kind = CBLC_DATA_KIND_STRUCT;
        ft_strlcpy(synthetic_target_item.cobol_name, target_name,
            sizeof(synthetic_target_item.cobol_name));
        ft_strlcpy(synthetic_target_item.struct_type_name, return_type->source_name,
            sizeof(synthetic_target_item.struct_type_name));
        target_item = &synthetic_target_item;
    }
    return_type = cblc_find_struct_type(unit, target_item->struct_type_name);
    if (!return_type)
        return (FT_SUCCESS);
    copy_constructor = cblc_find_copy_constructor(return_type);
    if (!copy_constructor || !copy_constructor->has_definition
        || copy_constructor->parameter_count != 1
        || copy_constructor->parameters[0].kind
            != TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        return (FT_SUCCESS);
    source_item = cblc_find_data_item_by_cobol(unit, source_reference);
    if (!source_item)
        source_item = cblc_find_data_item_for_emit_by_source(unit, source_reference);
    if (source_item && source_item->kind == CBLC_DATA_KIND_STRUCT
        && std::strcmp(source_item->struct_type_name,
            return_type->source_name) == 0)
        ft_strlcpy(source_based_name, source_item->cobol_name,
            sizeof(source_based_name));
    else if (cblc_emit_prepare_struct_pointer_value(unit, source_reference,
            builder, source_based_name, sizeof(source_based_name),
            receiver_context) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
            source_based_name, copy_constructor->parameters[0].cobol_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    based_receiver = *target_item;
    based_receiver.kind = CBLC_DATA_KIND_STRUCT;
    based_receiver.is_alias = 0;
    ft_strlcpy(based_receiver.struct_type_name, return_type->source_name,
        sizeof(based_receiver.struct_type_name));
    constructor_index = static_cast<size_t>(copy_constructor
        - return_type->constructors);
    if (cblc_constructor_paragraph_name(&based_receiver, constructor_index,
            constructor_name, sizeof(constructor_name)) != FT_SUCCESS
        || cblc_constructor_return_paragraph_name(unit, &based_receiver,
            constructor_index, return_label, sizeof(return_label)) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "           PERFORM %s THRU %s.", constructor_name, return_label) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    *out_handled = 1;
    return (FT_SUCCESS);
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_data_item *receiver_context);

static int cblc_emit_method_value_parameter_cleanup(
    const t_cblc_translation_unit *unit, const t_cblc_method *method,
    const t_cblc_data_item *receiver, t_cobol_text_builder *builder)
{
    size_t parameter_index;

    if (!unit || !method || !receiver || !builder)
        return (FT_FAILURE);
    parameter_index = method->parameter_count;
    while (parameter_index > 0)
    {
        const t_cblc_parameter *parameter;
        t_cblc_statement destroy_statement;

        parameter_index -= 1;
        parameter = &method->parameters[parameter_index];
        if (parameter->kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            continue ;
        std::memset(&destroy_statement, 0, sizeof(destroy_statement));
        destroy_statement.type = CBLC_STATEMENT_DESTRUCT;
        ft_strlcpy(destroy_statement.target, parameter->cobol_name,
            sizeof(destroy_statement.target));
        if (cblc_emit_lifecycle_statement(unit, &destroy_statement, builder,
                NULL, static_cast<size_t>(-1), receiver) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_builder_has_based_data_declaration(
    const t_cobol_text_builder *builder, const char *based_name);

static int cblc_emit_struct_pointer_parameter_bindings(
    const t_cblc_translation_unit *unit, const t_cblc_parameter *parameters,
    size_t parameter_count, t_cobol_text_builder *builder,
    const t_cblc_struct_type *owner_type = NULL)
{
    size_t parameter_index;

    if (!unit || (!parameters && parameter_count > 0) || !builder)
        return (FT_FAILURE);
    for (parameter_index = 0; parameter_index < parameter_count;
        ++parameter_index)
    {
        const t_cblc_parameter *parameter = &parameters[parameter_index];
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        const char *concrete_type_name = parameter->type_name;

        if (parameter->kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
            continue ;
        if (owner_type && owner_type->is_template_instantiation
            && owner_type->template_source_name[0] != '\0'
            && std::strcmp(parameter->type_name,
                owner_type->template_source_name) == 0)
            concrete_type_name = owner_type->source_name;
        if (cblc_build_struct_pointer_based_name(unit, parameter->cobol_name,
                concrete_type_name, based_name, sizeof(based_name))
                != FT_SUCCESS)
        {
            cobol_text_builder_set_diagnostic(builder, 0,
                "could not form BASED view for class-pointer parameter");
            return (FT_FAILURE);
        }
        if (!cblc_builder_has_based_data_declaration(builder, based_name))
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "no BASED record '%s' for class-pointer parameter '%s' of type '%s'",
                based_name, parameter->cobol_name, parameter->type_name);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            return (FT_FAILURE);
        }
        if (std::snprintf(line, sizeof(line),
                "           IF %s NOT = NULL.", parameter->cobol_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line),
                "               SET ADDRESS OF %s TO %s.", based_name,
                parameter->cobol_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || cobol_text_builder_append_line(builder,
                "           END-IF.") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_emit_method_body(const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_method *method, const char *assign_target, t_cobol_text_builder *builder)
{
    t_cobol_text_builder_lifecycle_scope lifecycle_scope(builder, method, NULL);
    t_cblc_function method_context;
    size_t index;
    int control_flow_depth;
    int control_flow_stack[CBLC_EXCEPTION_REGION_MAX];

    if (!unit || !receiver || !method || !builder)
        return (FT_FAILURE);
    /* Method bodies are lowered through the same statement emitter as free
     * functions. Preserve the method's parameter ABI there: without this
     * context, a call such as append(source) cannot resolve `source` as the
     * current method parameter when its body is inlined for a receiver. */
    std::memset(&method_context, 0, sizeof(method_context));
    ft_strlcpy(method_context.source_name, method->source_name,
        sizeof(method_context.source_name));
    ft_strlcpy(method_context.cobol_name, receiver->cobol_name,
        sizeof(method_context.cobol_name));
    std::memcpy(method_context.parameters, method->parameters,
        sizeof(method_context.parameters));
    method_context.parameter_count = method->parameter_count;
    method_context.statements = method->statements;
    method_context.statement_count = method->statement_count;
    method_context.return_kind = method->return_kind;
    ft_strlcpy(method_context.return_type_name, method->return_type_name,
        sizeof(method_context.return_type_name));
    if (assign_target && assign_target[0] != '\0')
        ft_strlcpy(method_context.return_cobol_name, assign_target,
            sizeof(method_context.return_cobol_name));
    method_context.exception_type_count = method->exception_type_count;
    std::memcpy(method_context.exception_type_ids, method->exception_type_ids,
        sizeof(method_context.exception_type_ids));
    method_context.exception_types_unknown = method->exception_types_unknown;
    method_context.returns_to_paragraph = 1;
    if (cblc_method_return_paragraph_name(unit, receiver, method,
            method_context.paragraph_return_label,
            sizeof(method_context.paragraph_return_label)) != FT_SUCCESS)
        return (FT_FAILURE);
    /* Pointer parameters to class values are represented by an address slot,
     * while member expressions in the method body lower through a BASED
     * record. Bind each parameter's concrete view for this invocation before
     * any substituted statement reads or writes its fields. Without this,
     * `source->elements` in move_assign can dereference an unbound (or stale)
     * BASED alias when instantiated for nested vector types. */
    if (cblc_emit_struct_pointer_parameter_bindings(unit, method->parameters,
            method->parameter_count, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    control_flow_depth = 0;
    if (assign_target && assign_target[0] != '\0'
        && (method->return_kind == CBLC_FUNCTION_RETURN_CHAR
            || method->return_kind == CBLC_FUNCTION_RETURN_INT
            || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE))
    {
        char result_name[TRANSPILE_IDENTIFIER_MAX];
        char result_init_line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_method_result_name(receiver, method, result_name,
                sizeof(result_name)) == FT_SUCCESS
            && std::strcmp(assign_target, result_name) == 0)
        {
            if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR)
            {
                if (std::snprintf(result_init_line, sizeof(result_init_line),
                        "           MOVE SPACE TO %s.", assign_target) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(result_init_line, sizeof(result_init_line),
                    "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, result_init_line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
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
        int implicit_self_return_call;

        body_statement = &method->statements[index];
        implicit_self_return_call = 0;
        if (receiver->kind == CBLC_DATA_KIND_STRUCT
            && std::strncmp(receiver->cobol_name, "CBLC-SPTR-",
                sizeof("CBLC-SPTR-") - 1) == 0
            && body_statement->type == CBLC_STATEMENT_METHOD_CALL
            && std::strcmp(body_statement->target, "CBLC-THIS") == 0
            && std::strcmp(body_statement->call_identifier,
                "destroy_current_elements") == 0
            && body_statement->call_argument_count == 0)
        {
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *cleanup_method;

            receiver_type = cblc_find_receiver_type(unit, receiver);
            cleanup_method = cblc_find_method_on_type(receiver_type,
                "destroy_current_elements");
            if (!cleanup_method
                || cblc_emit_method_body(unit, receiver, cleanup_method, NULL,
                    builder) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        substituted = *body_statement;
        if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *self_method;
            const t_cblc_data_item *call_target;
            int return_matches_target;

            receiver_type = cblc_find_receiver_type(unit, receiver);
            self_method = cblc_find_method_on_type_for_arguments(unit,
                receiver_type, substituted.call_identifier,
                substituted.call_arguments, substituted.call_argument_count);
            if (!self_method && receiver_type)
            {
                const t_cblc_method *name_match = cblc_find_method_on_type(
                    receiver_type, substituted.call_identifier);

                if (name_match
                    && name_match->parameter_count
                        == substituted.call_argument_count)
                    self_method = name_match;
            }
            call_target = cblc_find_data_item_by_cobol(unit,
                substituted.target);
            return_matches_target = 0;
            if (self_method && call_target)
            {
                if (self_method->return_kind == CBLC_FUNCTION_RETURN_INT)
                    return_matches_target = call_target->kind == CBLC_DATA_KIND_INT;
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_CHAR)
                    return_matches_target = call_target->kind == CBLC_DATA_KIND_CHAR;
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
                    return_matches_target = call_target->kind == CBLC_DATA_KIND_DOUBLE;
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
                    return_matches_target = call_target->kind == CBLC_DATA_KIND_STRUCT
                        && std::strcmp(call_target->struct_type_name,
                            self_method->return_type_name) == 0;
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
                    return_matches_target = call_target->kind
                            == CBLC_DATA_KIND_STRUCT_POINTER
                        && std::strcmp(call_target->struct_type_name,
                            self_method->return_type_name) == 0;
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
                    return_matches_target = cblc_pointer_assignment_is_compatible(
                        call_target->kind, CBLC_DATA_KIND_CHAR_POINTER);
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
                    return_matches_target = cblc_pointer_assignment_is_compatible(
                        call_target->kind, CBLC_DATA_KIND_INT_POINTER);
                else if (self_method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
                    return_matches_target = cblc_pointer_assignment_is_compatible(
                        call_target->kind, CBLC_DATA_KIND_VOID_POINTER);
            }
            if (return_matches_target)
            {
                substituted.type = CBLC_STATEMENT_METHOD_CALL_ASSIGN;
                implicit_self_return_call = 1;
            }
        }
        if (substituted.type == CBLC_STATEMENT_CALL
            && !substituted.call_is_external)
        {
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *self_method;

            receiver_type = cblc_find_receiver_type(unit, receiver);
            self_method = cblc_find_method_on_type(receiver_type,
                substituted.call_identifier);
            if (self_method
                && self_method->return_kind == CBLC_FUNCTION_RETURN_VOID
                && self_method->parameter_count == substituted.call_argument_count)
            {
                if (receiver->kind == CBLC_DATA_KIND_STRUCT
                    && std::strncmp(receiver->cobol_name, "CBLC-SPTR-",
                        sizeof("CBLC-SPTR-") - 1) == 0
                    && std::strcmp(self_method->source_name,
                        "destroy_current_elements") == 0
                    && self_method->parameter_count == 0)
                {
                    if (cblc_emit_method_body(unit, receiver, self_method, NULL,
                            builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    index += 1;
                    continue ;
                }
                substituted.type = CBLC_STATEMENT_METHOD_CALL;
                ft_strlcpy(substituted.target, receiver->cobol_name,
                    sizeof(substituted.target));
            }
        }
        if (cblc_restore_method_parameter_sources(method, body_statement->target,
                substituted.target, sizeof(substituted.target)) != FT_SUCCESS
            || cblc_restore_method_parameter_sources(method, body_statement->source,
                substituted.source, sizeof(substituted.source)) != FT_SUCCESS
            || cblc_restore_method_parameter_sources(method, body_statement->call_arguments,
                substituted.call_arguments, sizeof(substituted.call_arguments)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_rewrite_method_parameter_references(unit, method, substituted.target,
                rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, substituted.source,
                rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method,
                substituted.call_arguments,
                rewritten_call_arguments, sizeof(rewritten_call_arguments)) != FT_SUCCESS)
            return (FT_FAILURE);
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
        {
            return (FT_FAILURE);
        }
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
            return (FT_FAILURE);
        if (!method->is_copy_assignment
            && (cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.target, rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.source, rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    substituted.call_arguments, rewritten_call_arguments,
                    sizeof(rewritten_call_arguments)) != FT_SUCCESS))
            return (FT_FAILURE);
        if (!method->is_copy_assignment)
        {
            ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
            ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
            ft_strlcpy(substituted.call_arguments, rewritten_call_arguments,
                sizeof(substituted.call_arguments));
        }
        if (cblc_rewrite_method_parameter_references(unit, method, substituted.target,
                rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, substituted.source,
                rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method,
                substituted.call_arguments,
                rewritten_call_arguments, sizeof(rewritten_call_arguments)) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
        ft_strlcpy(substituted.call_arguments, rewritten_call_arguments,
            sizeof(substituted.call_arguments));
        if (cblc_rewrite_scoped_method_data_references(unit, method,
                substituted.target,
                receiver->cobol_name, mapped_target, sizeof(mapped_target)) != FT_SUCCESS
            || cblc_rewrite_scoped_method_data_references(unit, method,
                substituted.source,
                receiver->cobol_name, mapped_source, sizeof(mapped_source)) != FT_SUCCESS
            || cblc_rewrite_scoped_method_data_references(unit, method,
                substituted.call_arguments,
                receiver->cobol_name, mapped_call_arguments,
                sizeof(mapped_call_arguments)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        ft_strlcpy(substituted.target, mapped_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, mapped_source, sizeof(substituted.source));
        ft_strlcpy(substituted.call_arguments, mapped_call_arguments,
            sizeof(substituted.call_arguments));
        if (cblc_normalize_method_string_subscripts(unit, receiver, substituted.target,
                rewritten_target, sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_normalize_method_string_subscripts(unit, receiver, substituted.source,
                rewritten_source, sizeof(rewritten_source)) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        ft_strlcpy(substituted.target, rewritten_target, sizeof(substituted.target));
        ft_strlcpy(substituted.source, rewritten_source, sizeof(substituted.source));
        /* Method parsing can materialize the return slot as a separate
         * assignment before the RETURN statement. Redirect both that write
         * and the RETURN to the paragraph's typed result slot. */
        if (method->return_kind != CBLC_FUNCTION_RETURN_VOID
            && assign_target && assign_target[0] != '\0'
            && (std::strcmp(substituted.target, "CBLC-THIS-RET") == 0
                || (std::strlen(receiver->cobol_name)
                        + sizeof("-RET") - 1
                        == std::strlen(substituted.target)
                    && std::strncmp(substituted.target,
                        receiver->cobol_name,
                        std::strlen(receiver->cobol_name)) == 0
                    && std::strcmp(substituted.target
                            + std::strlen(receiver->cobol_name), "-RET") == 0)))
        {
            if (std::strcmp(substituted.source, substituted.target) == 0)
                ft_strlcpy(substituted.source, assign_target,
                    sizeof(substituted.source));
            ft_strlcpy(substituted.target, assign_target,
                sizeof(substituted.target));
        }
        if (implicit_self_return_call)
            ft_strlcpy(substituted.source, receiver->cobol_name,
                sizeof(substituted.source));
        if (substituted.type == CBLC_STATEMENT_IF_BEGIN
            || substituted.type == CBLC_STATEMENT_IF_ELSE
            || substituted.type == CBLC_STATEMENT_IF_END)
        {
            char control_line[TRANSPILE_STATEMENT_TEXT_MAX];

            if (substituted.type == CBLC_STATEMENT_IF_BEGIN)
            {
                char condition[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_translate_condition_for_cobol(unit, substituted.source,
                        builder, condition, sizeof(condition)) != FT_SUCCESS
                    || std::snprintf(control_line, sizeof(control_line),
                        "           IF %s", condition) < 0)
                    return (FT_FAILURE);
                if (control_flow_depth >= CBLC_EXCEPTION_REGION_MAX)
                    return (FT_FAILURE);
                control_flow_stack[control_flow_depth] = CBLC_STATEMENT_IF_BEGIN;
                control_flow_depth += 1;
                cobol_text_builder_set_control_flow_depth(builder, control_flow_depth);
                if (cobol_text_builder_append_line(builder, control_line)
                    != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "               CONTINUE") != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
                continue ;
            }
            else if (substituted.type == CBLC_STATEMENT_IF_ELSE)
            {
                cobol_text_builder_set_control_flow_depth(builder, control_flow_depth);
                ft_strlcpy(control_line, "           ELSE", sizeof(control_line));
                if (cobol_text_builder_append_line(builder, control_line)
                    != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "               CONTINUE") != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
                continue ;
            }
            else
            {
                if (control_flow_depth == 0
                    || control_flow_stack[control_flow_depth - 1]
                        != CBLC_STATEMENT_IF_BEGIN)
                    return (FT_FAILURE);
                control_flow_depth -= 1;
                cobol_text_builder_set_control_flow_depth(builder, control_flow_depth);
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

                if (cblc_translate_condition_for_cobol(unit, substituted.source,
                        builder, condition, sizeof(condition)) != FT_SUCCESS
                    || std::snprintf(control_line, sizeof(control_line),
                        "           PERFORM UNTIL NOT (%s)", condition) < 0)
                    return (FT_FAILURE);
                if (control_flow_depth >= CBLC_EXCEPTION_REGION_MAX)
                    return (FT_FAILURE);
                control_flow_stack[control_flow_depth] = CBLC_STATEMENT_WHILE_BEGIN;
                control_flow_depth += 1;
                cobol_text_builder_set_control_flow_depth(builder, control_flow_depth);
            }
            else
            {
                if (control_flow_depth == 0
                    || control_flow_stack[control_flow_depth - 1]
                        != CBLC_STATEMENT_WHILE_BEGIN)
                    return (FT_FAILURE);
                control_flow_depth -= 1;
                cobol_text_builder_set_control_flow_depth(builder, control_flow_depth);
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
            char line[TRANSPILE_STATEMENT_TEXT_MAX];
            char return_paragraph[TRANSPILE_IDENTIFIER_MAX];

            if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR)
            {
                char return_source[TRANSPILE_STATEMENT_TEXT_MAX];

                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (cblc_emit_prepare_pointer_element(unit, substituted.source, builder,
                        return_source, sizeof(return_source), receiver) != FT_SUCCESS
                    && cblc_translate_expression_for_cobol(substituted.source,
                        return_source, sizeof(return_source)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                        return_source, assign_target) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_INT
            || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
            {
                char return_source[TRANSPILE_STATEMENT_TEXT_MAX];

                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (cblc_emit_prepare_pointer_element(unit, substituted.source, builder,
                        return_source, sizeof(return_source), receiver) != FT_SUCCESS
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
                int copy_return_handled;

                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (cblc_emit_struct_copy_return(unit, assign_target,
                        method->return_type_name, substituted.source, receiver, builder,
                        &copy_return_handled) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "failed to copy struct return from '%s' to '%s' in method '%s'",
                        substituted.source, assign_target, method->source_name);
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
                if (!copy_return_handled
                    && (std::snprintf(line, sizeof(line),
                            "           MOVE %s TO %s.", substituted.source,
                            assign_target) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS))
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            {
                const t_cblc_data_item *source_item;
                const t_cblc_struct_field *direct_pointer_field;
                const char *source_reference;
                char based_return_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char pointer_field_value[TRANSPILE_STATEMENT_TEXT_MAX];

                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                direct_pointer_field = cblc_method_direct_pointer_field(unit,
                    receiver, substituted.source);
                if (direct_pointer_field)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s TO %s.", assign_target,
                            substituted.source) < 0)
                        return (FT_FAILURE);
                }
                else if (cblc_emit_prepare_struct_pointer_field(unit,
                        substituted.source, builder, pointer_field_value,
                        sizeof(pointer_field_value), 1) == FT_SUCCESS)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s TO %s.", assign_target,
                            pointer_field_value) < 0)
                        return (FT_FAILURE);
                }
                else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER
                    && cblc_emit_prepare_struct_pointer_value(unit,
                        substituted.source, builder, based_return_name,
                        sizeof(based_return_name), receiver) == FT_SUCCESS)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s TO ADDRESS OF %s.", assign_target,
                            based_return_name) < 0)
                        return (FT_FAILURE);
                }
                else if (cblc_emit_prepare_pointer_element(unit, substituted.source, builder,
                        based_return_name, sizeof(based_return_name), receiver)
                    == FT_SUCCESS)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s TO CBLC-PTR-CURSOR.", assign_target) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    source_reference = substituted.source;
                    if (source_reference[0] == '&')
                        source_reference += 1;
                    source_item = cblc_find_data_item_for_emit_by_source(unit,
                        source_reference);
                    if (!source_item)
                        source_item = cblc_find_data_item_by_cobol(unit,
                            source_reference);
                    if (!source_item)
                    {
                        return (FT_FAILURE);
                    }
                    if (cblc_data_kind_is_pointer(source_item->kind))
                    {
                        if (std::snprintf(line, sizeof(line),
                                "           SET %s TO %s.", assign_target,
                                source_item->cobol_name) < 0)
                            return (FT_FAILURE);
                    }
                    else if (source_reference == substituted.source)
                        return (FT_FAILURE);
                    else if (std::snprintf(line, sizeof(line),
                            "           SET %s TO ADDRESS OF %s.", assign_target,
                            source_item->cobol_name) < 0)
                        return (FT_FAILURE);
                }
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cblc_emit_method_value_parameter_cleanup(unit, method,
                    receiver, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_method_return_paragraph_name(unit, receiver, method,
                    return_paragraph, sizeof(return_paragraph)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "           GO TO %s",
                    return_paragraph) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (substituted.type == CBLC_STATEMENT_METHOD_CALL
            && std::strcmp(substituted.target, "CBLC-THIS") == 0)
            ft_strlcpy(substituted.target, receiver->cobol_name,
                sizeof(substituted.target));
        if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN
            && std::strcmp(substituted.call_identifier, "CBLC-NEW-ARRAY") == 0)
        {
            char allocation_argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];
            char allocation_line[TRANSPILE_STATEMENT_TEXT_MAX];

            if (substituted.call_argument_count != 1
                || cblc_extract_call_argument(&substituted, 0,
                    allocation_argument, sizeof(allocation_argument)) != FT_SUCCESS
                || cblc_translate_expression_for_cobol(allocation_argument,
                    translated_argument, sizeof(translated_argument)) != FT_SUCCESS
                || std::snprintf(allocation_line, sizeof(allocation_line),
                    "           ALLOCATE %s CHARACTERS RETURNING %s",
                    translated_argument, substituted.target) < 0)
                return (FT_FAILURE);
            if (cblc_emit_allocation_exception(unit, &method_context,
                    &substituted, index, allocation_line, substituted.target,
                    "bad_alloc", builder) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if (cblc_emit_substituted_statement_with_flow(unit, &method_context, &substituted,
                receiver->cobol_name, builder, &control_flow_depth, receiver)
            != FT_SUCCESS)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "failed emitting '%s' method body statement %zu (kind %d, target '%s', source '%s')",
                method->source_name, index,
                static_cast<int>(substituted.type), substituted.target,
                substituted.source);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (control_flow_depth != 0)
        return (FT_FAILURE);
    cobol_text_builder_set_control_flow_depth(builder, 0);
    return (FT_SUCCESS);
}

static unsigned long long cblc_emitted_name_hash(const t_cblc_data_item *receiver,
    const t_cblc_method *method)
{
    unsigned long long hash;
    const char *parts[3];
    size_t part_index;
    const char *cursor;

    hash = 1469598103934665603ULL;
    parts[0] = receiver->cobol_name;
    parts[1] = receiver->struct_type_name;
    parts[2] = method ? method->cobol_name : "";
    part_index = 0;
    while (part_index < 3)
    {
        cursor = parts[part_index++];
        while (*cursor != '\0')
        {
            hash ^= static_cast<unsigned char>(*cursor++);
            hash *= 1099511628211ULL;
        }
        hash ^= 0xFFU;
        hash *= 1099511628211ULL;
    }
    return (hash);
}

static int cblc_method_paragraph_name(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    char *buffer, size_t buffer_size)
{
    char return_name[TRANSPILE_IDENTIFIER_MAX];
    int written;

    if (!unit || !receiver || !method || !buffer || buffer_size == 0
        || cblc_method_return_paragraph_name(unit, receiver, method,
            return_name, sizeof(return_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    written = std::snprintf(buffer, buffer_size, "CBLC-METHOD-%s-%s",
        receiver->cobol_name, method->cobol_name);
    if (written >= 0 && static_cast<size_t>(written) < buffer_size
        && written <= 40)
        return (FT_SUCCESS);
    if (std::snprintf(buffer, buffer_size, "CBLC-METHOD-%s", return_name) < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_method_is_reusable(const t_cblc_method *method)
{
    if (!method || !method->has_definition
        || method->is_copy_assignment
        || (method->return_kind != CBLC_FUNCTION_RETURN_VOID
            && method->return_kind != CBLC_FUNCTION_RETURN_CHAR
            && method->return_kind != CBLC_FUNCTION_RETURN_INT
            && method->return_kind != CBLC_FUNCTION_RETURN_DOUBLE
            && method->return_kind != CBLC_FUNCTION_RETURN_STRUCT
            && method->return_kind != CBLC_FUNCTION_RETURN_VOID_POINTER
            && method->return_kind != CBLC_FUNCTION_RETURN_CHAR_POINTER
            && method->return_kind != CBLC_FUNCTION_RETURN_INT_POINTER
            && method->return_kind != CBLC_FUNCTION_RETURN_STRUCT_POINTER))
        return (0);
    return (1);
}

static const t_cblc_struct_field *cblc_method_direct_pointer_field(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const char *reference)
{
    const t_cblc_struct_type *type;
    const char *field_name;
    size_t receiver_length;
    size_t field_index;

    if (!unit || !receiver || !reference
        || (receiver->kind != CBLC_DATA_KIND_STRUCT
            && receiver->kind != CBLC_DATA_KIND_STRUCT_POINTER))
        return (NULL);
    receiver_length = std::strlen(receiver->cobol_name);
    if (receiver_length == 0
        || std::strncmp(reference, receiver->cobol_name, receiver_length) != 0
        || reference[receiver_length] != '-')
        return (NULL);
    field_name = reference + receiver_length + 1;
    if (std::strchr(field_name, '-') || std::strchr(field_name, '['))
        return (NULL);
    type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!type)
        return (NULL);
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field = &type->fields[field_index];

        if (cblc_data_kind_is_pointer(field->kind)
            && (std::strcmp(field_name, field->cobol_name) == 0
                || std::strcmp(field_name, field->source_name) == 0))
            return (field);
        field_index += 1;
    }
    return (NULL);
}

static int cblc_method_result_name(const t_cblc_data_item *receiver,
    const t_cblc_method *method, char *buffer, size_t buffer_size)
{
    unsigned long long hash;
    const char *cursor;
    int written;

    if (!receiver || !method || !buffer || buffer_size == 0
        || method->return_kind == CBLC_FUNCTION_RETURN_VOID)
        return (FT_FAILURE);
    /* A reusable method paragraph has one static return destination. Key that
     * destination by concrete receiver type and method, not by the particular
     * object used at the first call site. Besides keeping the ABI consistent,
     * this makes return storage available when the receiver is a temporary
     * produced by another method or constructor. */
    written = std::snprintf(buffer, buffer_size, "CBLC-RESULT-%s-%s",
        receiver->struct_type_name, method->cobol_name);
    if (written >= 0 && static_cast<size_t>(written) < buffer_size
        && written <= 40)
        return (FT_SUCCESS);
    hash = 1469598103934665603ULL;
    cursor = receiver->struct_type_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= 0xFFU;
    hash *= 1099511628211ULL;
    cursor = method->source_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= 0xFEU;
    hash *= 1099511628211ULL;
    hash ^= static_cast<unsigned long long>(method->return_kind);
    hash *= 1099511628211ULL;
    cursor = method->return_type_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= static_cast<unsigned long long>(method->parameter_count);
    hash *= 1099511628211ULL;
    cursor = method->cobol_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    if (std::snprintf(buffer, buffer_size, "CBLC-RESULT-%016llX",
            hash) < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_constructor_paragraph_name(const t_cblc_data_item *receiver,
    size_t constructor_index, char *buffer, size_t buffer_size)
{
    unsigned long long hash;
    const char *cursor;
    int written;

    if (!receiver || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    written = std::snprintf(buffer, buffer_size, "CBLC-CONSTRUCTOR-%s-%zu",
        receiver->cobol_name, constructor_index);
    if (written >= 0 && static_cast<size_t>(written) < buffer_size)
        return (FT_SUCCESS);
    hash = 1469598103934665603ULL;
    cursor = receiver->cobol_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    cursor = receiver->struct_type_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= static_cast<unsigned long long>(constructor_index);
    hash *= 1099511628211ULL;
    if (std::snprintf(buffer, buffer_size, "CBLC-CON-%016llX-%zu", hash,
            constructor_index) < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_destructor_paragraph_name(const t_cblc_data_item *receiver,
    char *buffer, size_t buffer_size)
{
    int written;

    if (!receiver || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    written = std::snprintf(buffer, buffer_size, "CBLC-DESTRUCTOR-%s", receiver->cobol_name);
    if (written >= 0 && static_cast<size_t>(written) < buffer_size)
        return (FT_SUCCESS);
    if (std::snprintf(buffer, buffer_size, "CBLC-DTOR-%016llX",
            cblc_emitted_name_hash(receiver, NULL)) < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_transfer(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, t_cobol_text_builder *builder);

static int cblc_constructor_has_exception_control(const t_cblc_constructor *constructor);
static int cblc_constructor_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *owner_type,
    const t_cblc_constructor *constructor, size_t depth);
static int cblc_method_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *owner_type, const t_cblc_method *method,
    size_t depth);
static const t_cblc_constructor *cblc_find_move_constructor(
    const t_cblc_struct_type *type);
static int cblc_emit_exception_copy_by_assignment(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *source_prefix, const char *target_prefix,
    t_cobol_text_builder *builder)
{
    const t_cblc_method *copy_assignment;
    t_cblc_data_item receiver;
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !type || !source_prefix || !target_prefix || !builder
        || !type->is_class || !type->has_copy_assignment)
        return (FT_FAILURE);
    copy_assignment = cblc_find_method_on_type(type, "operator=");
    if (!copy_assignment || !copy_assignment->has_definition
        || copy_assignment->return_kind != CBLC_FUNCTION_RETURN_VOID
        || copy_assignment->parameter_count != 1
        || copy_assignment->parameters[0].kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT
        || std::strncmp(copy_assignment->parameters[0].type_name, type->source_name,
            sizeof(copy_assignment->parameters[0].type_name)) != 0
        || std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
            source_prefix, copy_assignment->parameters[0].cobol_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&receiver, 0, sizeof(receiver));
    receiver.kind = CBLC_DATA_KIND_STRUCT;
    ft_strlcpy(receiver.source_name, target_prefix, sizeof(receiver.source_name));
    ft_strlcpy(receiver.cobol_name, target_prefix, sizeof(receiver.cobol_name));
    ft_strlcpy(receiver.struct_type_name, type->source_name,
        sizeof(receiver.struct_type_name));
    return (cblc_emit_method_body(unit, &receiver, copy_assignment, NULL, builder));
}

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
    method = cblc_find_method_on_type_for_arguments(unit, receiver_type,
        statement->call_identifier, statement->call_arguments,
        statement->call_argument_count);
    return (method && cblc_method_may_throw(unit, receiver_type, method, 0));
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
    if (!item)
        item = cblc_find_data_item_for_emit_by_source(unit, statement->target);
    if (item && item->kind == CBLC_DATA_KIND_STRUCT && item->array_count == 0)
        type = cblc_find_struct_type(unit, item->struct_type_name);
    else
    {
        const t_cblc_data_item *pointer_item;
        t_cblc_data_item synthetic_pointer_item;
        char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *open_bracket;
        size_t pointer_name_length;

        open_bracket = std::strchr(statement->target, '[');
        pointer_name_length = open_bracket
            ? static_cast<size_t>(open_bracket - statement->target) : 0;
        if (pointer_name_length == 0 || pointer_name_length >= sizeof(pointer_name))
            return (0);
        std::memcpy(pointer_name, statement->target, pointer_name_length);
        pointer_name[pointer_name_length] = '\0';
        pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_name,
            &synthetic_pointer_item, NULL);
        if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
            return (0);
        type = cblc_find_struct_type(unit, pointer_item->struct_type_name);
    }
    if (!type)
        return (0);
    constructor = cblc_find_constructor_for_arguments(unit, type,
        statement->call_arguments, statement->call_argument_count);
    return (constructor && cblc_constructor_may_throw(unit, type, constructor, 0));
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

static int cblc_constructor_return_paragraph_name(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    size_t constructor_index, char *buffer, size_t buffer_size)
{
    size_t data_index;
    unsigned long long hash;
    const char *cursor;

    if (!unit || !receiver || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        if (&unit->data_items[data_index] == receiver)
        {
            if (std::snprintf(buffer, buffer_size, "CBLC-CRET-%zu-%zu",
                    data_index, constructor_index) < 0
                || std::strlen(buffer) >= buffer_size)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        data_index += 1;
    }
    /* A based receiver is often reconstructed as a temporary data item at a
     * call site. Prefer the canonical concrete receiver with the same emitted
     * identity before mapping through its owning pointer declaration. Mapping
     * through the pointer can select a different data index than the
     * constructor paragraph's return label, producing an unresolved PERFORM
     * range for nested/template-generated class elements. */
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *candidate;

        candidate = &unit->data_items[data_index];
        if (!candidate->is_alias
            && std::strcmp(candidate->cobol_name, receiver->cobol_name) == 0
            && std::strcmp(candidate->struct_type_name,
                receiver->struct_type_name) == 0)
        {
            if (std::snprintf(buffer, buffer_size, "CBLC-CRET-%zu-%zu",
                    data_index, constructor_index) < 0
                || std::strlen(buffer) >= buffer_size)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        data_index += 1;
    }
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *pointer_item;
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];

        pointer_item = &unit->data_items[data_index];
        if (!pointer_item->is_alias
            && pointer_item->kind == CBLC_DATA_KIND_STRUCT_POINTER
            && std::strcmp(pointer_item->struct_type_name,
                receiver->struct_type_name) == 0
            && cblc_build_struct_pointer_based_name(unit, pointer_item->cobol_name,
                pointer_item->struct_type_name, based_name, sizeof(based_name))
                == FT_SUCCESS
            && std::strcmp(based_name, receiver->cobol_name) == 0)
        {
            if (std::snprintf(buffer, buffer_size, "CBLC-CRET-%zu-%zu",
                    data_index, constructor_index) < 0
                || std::strlen(buffer) >= buffer_size)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        data_index += 1;
    }
    /* A nested based receiver (for example the inner vector in
     * vector<vector<vector<string>>>) has no standalone unit data-item index.
     * Derive a stable short return label from its concrete receiver identity. */
    hash = 1469598103934665603ULL;
    cursor = receiver->cobol_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    cursor = receiver->struct_type_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= static_cast<unsigned long long>(constructor_index);
    hash *= 1099511628211ULL;
    if (std::snprintf(buffer, buffer_size, "CBLC-CRET-%016llX", hash) < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_method_return_paragraph_name(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    char *buffer, size_t buffer_size)
{
    const t_cblc_struct_type *type;
    size_t receiver_index;
    size_t method_index;
    unsigned long long hash;
    const char *cursor;

    if (!unit || !receiver || !method || !buffer || buffer_size == 0
        || !unit->struct_types)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!type || !type->methods)
        return (FT_FAILURE);
    method_index = 0;
    while (method_index < type->method_count && &type->methods[method_index] != method)
        method_index += 1;
    if (method_index >= type->method_count)
        return (FT_FAILURE);
    receiver_index = 0;
    while (receiver_index < unit->data_count
        && &unit->data_items[receiver_index] != receiver)
        receiver_index += 1;
    if (receiver_index < unit->data_count)
    {
        if (std::snprintf(buffer, buffer_size, "CBLC-MRET-%zu-%zu",
                receiver_index, method_index) < 0
            || std::strlen(buffer) >= buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    hash = 1469598103934665603ULL;
    cursor = receiver->cobol_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    cursor = receiver->struct_type_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    cursor = method->cobol_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    /* All methods emitted from one template specialization share its COBOL
     * class name. Include the source method identity so long-name fallback
     * paragraphs for (for example) size() and at() cannot collide. */
    cursor = method->source_name;
    while (*cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= static_cast<unsigned long long>(method->overload_index);
    hash *= 1099511628211ULL;
    if (std::snprintf(buffer, buffer_size, "CBLC-MRET-%016llX", hash) < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
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
        || std::strncmp(statement->exception_type_name, "int", sizeof(statement->exception_type_name)) == 0)
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
    const t_cblc_function *caller, t_cobol_text_builder *builder)
{
    char using_clause[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t index;

    if (!unit || !statement || !receiver || !method || !builder
        || method->implementation_program_name[0] == '\0'
        || statement->call_argument_count != method->parameter_count)
    {
        if (builder)
            cobol_text_builder_set_diagnostic(builder, 0,
                "invalid string class helper call metadata or argument count");
        return (FT_FAILURE);
    }
    using_clause[0] = '\0';
    ft_strlcat(using_clause, "BY REFERENCE ", sizeof(using_clause));
    ft_strlcat(using_clause, receiver->cobol_name, sizeof(using_clause));
    index = 0;
    while (index < method->parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_data_item *argument_item;
        const char *argument_cobol_name;

        if (cblc_extract_call_argument(statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
        {
            cobol_text_builder_set_diagnostic(builder, 0,
                "unable to extract string class helper argument");
            return (FT_FAILURE);
        }
        argument_cobol_name = NULL;
        if (method->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER)
        {
            argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (argument_item && argument_item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
                argument_cobol_name = argument_item->cobol_name;
            else
            {
                cobol_text_builder_set_diagnostic(builder, 0,
                    "string class helper character-buffer argument is not a buffer item");
                return (FT_FAILURE);
            }
            ft_strlcat(using_clause, " BY REFERENCE ", sizeof(using_clause));
            ft_strlcat(using_clause, argument_cobol_name, sizeof(using_clause));
        }
        else if (cblc_argument_is_numeric_literal(argument))
        {
            ft_strlcat(using_clause, " BY VALUE ", sizeof(using_clause));
            ft_strlcat(using_clause, argument, sizeof(using_clause));
        }
        else
        {
            argument_item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (argument_item)
                argument_cobol_name = argument_item->cobol_name;
            else if (caller)
            {
                size_t caller_parameter_index;

                caller_parameter_index = 0;
                while (caller_parameter_index < caller->parameter_count)
                {
                    const t_cblc_parameter *caller_parameter;

                    caller_parameter = &caller->parameters[caller_parameter_index];
                    if (((caller_parameter->source_name[0] != '\0'
                                && std::strcmp(argument,
                                    caller_parameter->source_name) == 0)
                            || (caller_parameter->actual_source_name[0] != '\0'
                                && std::strcmp(argument,
                                    caller_parameter->actual_source_name) == 0)
                            || (caller_parameter->cobol_name[0] != '\0'
                                && std::strcmp(argument,
                                    caller_parameter->cobol_name) == 0))
                        && caller_parameter->kind
                            == method->parameters[index].kind
                        && std::strcmp(caller_parameter->type_name,
                            method->parameters[index].type_name) == 0)
                    {
                        argument_cobol_name = caller_parameter->cobol_name;
                        break ;
                    }
                    caller_parameter_index += 1;
                }
            }
            if (!argument_cobol_name)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "string class helper cannot resolve argument '%.80s' for parameter '%.80s'",
                    argument, method->parameters[index].source_name);
                cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                return (FT_FAILURE);
            }
            ft_strlcat(using_clause, " BY REFERENCE ", sizeof(using_clause));
            ft_strlcat(using_clause, argument_cobol_name, sizeof(using_clause));
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

static void cblc_set_method_exception_lowering_diagnostic(
    t_cobol_text_builder *builder, const t_cblc_data_item *receiver,
    const t_cblc_method *method, const char *phase, size_t statement_index)
{
    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
    size_t failing_index;
    const t_cblc_statement *failing_statement;

    if (!builder || !receiver || !method || !phase
        || builder->diagnostic_message[0] != '\0')
        return ;
    failing_index = builder->active_statement_index != static_cast<size_t>(-1)
        ? builder->active_statement_index : statement_index;
    failing_statement = builder->active_statement;
    std::snprintf(diagnostic, sizeof(diagnostic),
        "failed to lower method '%s::%s' during %s at statement %zu (kind %d, target '%.48s', source '%.48s')",
        receiver->struct_type_name, method->source_name, phase,
        failing_index, failing_statement
            ? static_cast<int>(failing_statement->type) : -1,
        failing_statement ? failing_statement->target : "",
        failing_statement ? failing_statement->source : "");
    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
}

static int cblc_emit_method_exception_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    const char *assign_target, t_cobol_text_builder *builder, int emit_call);
static int cblc_builder_has_based_data_declaration(
    const t_cobol_text_builder *builder, const char *name);

static int cblc_type_contains_method(const t_cblc_struct_type *type,
    const t_cblc_method *method)
{
    size_t index;

    if (!type || !method)
        return (0);
    index = 0;
    while (index < type->method_count)
    {
        if (&type->methods[index] == method)
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_emit_method_statement_with_method(
    const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_method *selected_method,
    const t_cblc_data_item *receiver_context)
{
    const t_cblc_data_item *receiver;
    t_cblc_data_item synthetic_receiver;
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
    /* A concrete receiver context is authoritative when the lowered receiver
     * spelling is identical. A global synthetic/alias item can share that
     * spelling while carrying a different specialization, so resolving the
     * global name first can dispatch a method against the wrong class type. */
    if (receiver_context
        && std::strcmp(receiver_name, receiver_context->cobol_name) == 0)
        receiver = receiver_context;
    else
        receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver && receiver_context)
    {
        receiver = cblc_resolve_struct_pointer_item(unit, receiver_name,
            &synthetic_receiver, receiver_context);
        if (receiver)
        {
            synthetic_receiver = *receiver;
            receiver = &synthetic_receiver;
        }
    }
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
        {
            size_t data_index;

            data_index = 0;
            while (data_index < unit->data_count)
            {
                const t_cblc_data_item *candidate;
                char candidate_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char canonical_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
                int based_name_uses_elements_suffix;

                candidate = &unit->data_items[data_index];
                if (candidate->kind == CBLC_DATA_KIND_STRUCT_POINTER)
                {
                    int matches_based_name;

                    matches_based_name = 0;
                    canonical_based_name[0] = '\0';
                    based_name_uses_elements_suffix = 0;
                    if (cblc_build_struct_pointer_based_name(unit,
                            candidate->cobol_name, candidate->struct_type_name,
                            candidate_based_name, sizeof(candidate_based_name))
                            == FT_SUCCESS)
                    {
                        ft_strlcpy(canonical_based_name, candidate_based_name,
                            sizeof(canonical_based_name));
                        if (std::strcmp(candidate_based_name, receiver_name) == 0)
                            matches_based_name = 1;
                    }
                    /* A based receiver may already have been expanded while
                     * lowering an enclosing method body. In that context its
                     * canonical spelling can predate a later type-collision
                     * hash discovered for the same synthetic local. Accept
                     * the un-hashed spelling only when the concrete candidate
                     * type resolves this exact method overload. */
                    if (!matches_based_name
                        && std::snprintf(candidate_based_name,
                            sizeof(candidate_based_name), "CBLC-SPTR-%s",
                            candidate->cobol_name) >= 0
                        && (std::strcmp(candidate_based_name, receiver_name) == 0
                            || (std::snprintf(candidate_based_name,
                                    sizeof(candidate_based_name),
                                    "CBLC-SPTR-%s-ELEMENTS",
                                    candidate->cobol_name) >= 0
                                && std::strcmp(candidate_based_name,
                                    receiver_name) == 0)))
                    {
                        if (std::strstr(receiver_name, "-ELEMENTS")
                            && std::strlen(receiver_name)
                                >= sizeof("-ELEMENTS") - 1
                            && std::strcmp(receiver_name
                                    + std::strlen(receiver_name)
                                    - (sizeof("-ELEMENTS") - 1),
                                "-ELEMENTS") == 0)
                            based_name_uses_elements_suffix = 1;
                        const t_cblc_struct_type *candidate_type;

                        candidate_type = cblc_find_struct_type(unit,
                            candidate->struct_type_name);
                        if (candidate_type
                        && (selected_method
                            ? cblc_type_contains_method(candidate_type,
                                selected_method)
                            : cblc_find_method_on_type_for_arguments(unit,
                                candidate_type, statement->call_identifier,
                                statement->call_arguments,
                                statement->call_argument_count) != NULL))
                            matches_based_name = 1;
                    }
                    if (!matches_based_name)
                    {
                        data_index += 1;
                        continue ;
                    }
                    t_cblc_data_item based_receiver;

                    if (canonical_based_name[0] != '\0'
                        && based_name_uses_elements_suffix)
                    {
                        char elements_pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (std::snprintf(elements_pointer_name,
                                sizeof(elements_pointer_name), "%s-ELEMENTS",
                                candidate->cobol_name) < 0
                            || cblc_build_struct_pointer_based_name(unit,
                                elements_pointer_name,
                                candidate->struct_type_name,
                                canonical_based_name,
                                sizeof(canonical_based_name)) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    based_receiver = *candidate;
                    based_receiver.kind = CBLC_DATA_KIND_STRUCT;
                    based_receiver.is_alias = 0;
                    ft_strlcpy(based_receiver.source_name,
                        canonical_based_name[0] != '\0'
                            ? canonical_based_name : receiver_name,
                        sizeof(based_receiver.source_name));
                    ft_strlcpy(based_receiver.cobol_name,
                        canonical_based_name[0] != '\0'
                            ? canonical_based_name : receiver_name,
                        sizeof(based_receiver.cobol_name));
                    if (cblc_find_receiver_type(unit, &based_receiver))
                    {
                        receiver_type = cblc_find_receiver_type(unit, &based_receiver);
                        method = selected_method ? selected_method
                            : cblc_find_method_on_type_for_arguments(unit,
                                receiver_type, statement->call_identifier,
                                statement->call_arguments,
                                statement->call_argument_count);
                        if (!method
                            || !cblc_type_contains_method(receiver_type, method)
                            || std::strcmp(method->source_name,
                                statement->call_identifier) != 0)
                            return (FT_FAILURE);
                        {
                            int based_status;

                            if (cblc_method_may_throw(unit, receiver_type, method, 0))
                            {
                                based_status = cblc_emit_method_exception_body(unit,
                                    &based_receiver, method,
                                    assign_target ? assign_target : "", builder, 1);
                                if (based_status == FT_SUCCESS && caller
                                    && cblc_emit_exception_transfer(unit, caller,
                                        statement, statement_index, builder)
                                        != FT_SUCCESS)
                                    based_status = FT_FAILURE;
                            }
                            else if (cblc_method_is_reusable(method))
                                based_status = cblc_emit_method_exception_body(unit,
                                    &based_receiver, method,
                                    assign_target ? assign_target : "", builder, 1);
                            else
                                based_status = cblc_emit_method_body(unit,
                                    &based_receiver, method, assign_target, builder);
                            if (based_status != FT_SUCCESS)
                            {
                                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                                std::snprintf(diagnostic, sizeof(diagnostic),
                                    "failed emitting method body '%s::%s' for based receiver '%s'",
                                    receiver_type->source_name, method->source_name,
                                    based_receiver.cobol_name);
                                cobol_text_builder_set_diagnostic(builder, 0,
                                    diagnostic);
                            }
                            return (based_status);
                        }
                    }
                    return (FT_FAILURE);
                }
                data_index += 1;
            }
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "cannot resolve method '%.32s' for receiver '%.64s' in context '%.48s'",
                    statement->call_identifier, receiver_name,
                    receiver_context ? receiver_context->struct_type_name : "<none>");
                cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            }
            return (FT_FAILURE);
        }
    }
    receiver_type = cblc_find_receiver_type(unit, receiver);
    if (!receiver_type)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "receiver '%s' resolved without a concrete class type for method '%s'",
            receiver_name, statement->call_identifier);
        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        return (FT_FAILURE);
    }
    method = selected_method ? selected_method
        : cblc_find_method_on_type_for_arguments(unit, receiver_type,
            statement->call_identifier, statement->call_arguments,
            statement->call_argument_count);
    /* A method body's formal parameter is mapped to receiver-specific COBOL
     * storage before this second lowering pass, so it may no longer resolve
     * as a translation-unit data item. Parsing already type-checked the call;
     * permit the fallback only when name and arity identify one method. */
    if (!method && !selected_method)
        method = cblc_find_unique_method_on_type_for_arity(receiver_type,
            statement->call_identifier, statement->call_argument_count);
    if (!method || !cblc_type_contains_method(receiver_type, method)
        || std::strcmp(method->source_name, statement->call_identifier) != 0)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "method '%s' is not declared for concrete receiver type '%s' (receiver '%s')",
            statement->call_identifier, receiver_type->source_name, receiver_name);
        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        return (FT_FAILURE);
    }
    if (method->implementation_program_name[0] != '\0')
    {
        if (receiver->is_alias
            && receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER
            && std::strchr(receiver->cobol_name, '['))
        {
            char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
            t_cblc_data_item based_receiver;

            if (cblc_emit_prepare_struct_pointer_value(unit,
                    receiver->cobol_name, builder, based_name,
                    sizeof(based_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            based_receiver = *receiver;
            based_receiver.kind = CBLC_DATA_KIND_STRUCT;
            based_receiver.is_alias = 0;
            ft_strlcpy(based_receiver.source_name, based_name,
                sizeof(based_receiver.source_name));
            ft_strlcpy(based_receiver.cobol_name, based_name,
                sizeof(based_receiver.cobol_name));
            return (cblc_emit_string_class_helper_call(unit, statement,
                &based_receiver, method, assign_target, caller, builder));
        }
        return (cblc_emit_string_class_helper_call(unit, statement, receiver,
            method, assign_target, caller, builder));
    }
    /* Class method bodies are lowered against one stable BASED record per
     * concrete class type.  Bind that record at each call site when the
     * receiver is an ordinary object; otherwise a prior pointer-based call
     * can leave the record aimed at an unrelated (or already-destroyed)
     * object.  A receiver that is already the canonical BASED record keeps
     * the address established by its owning operation. */
    if (receiver->kind == CBLC_DATA_KIND_STRUCT
        && std::strncmp(receiver->source_name, "CBLC-SPTR-H",
            sizeof("CBLC-SPTR-H") - 1) != 0)
    {
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_build_struct_pointer_based_name(unit, receiver->cobol_name,
                receiver->struct_type_name, based_name, sizeof(based_name))
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_builder_has_based_data_declaration(builder, based_name))
        {
            if (std::snprintf(line, sizeof(line),
                    "           SET ADDRESS OF %s TO ADDRESS OF %s.", based_name,
                    receiver->cobol_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    /* Based receivers enter the inline method body below and return before the
     * reusable/direct receiver paths emit their argument staging. Stage the
     * logical arguments here so pointer-receiver calls use the same ABI slots,
     * including expressions that require a member-place setup. */
    if (receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER
        && method->parameter_count > 0
        && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
            statement->call_argument_count, method->parameters,
            method->parameter_count, builder,
            caller ? caller->parameters : NULL,
            caller ? caller->parameter_count : 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (receiver->is_alias
        && receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER
        && std::strchr(receiver->cobol_name, '['))
    {
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        t_cblc_data_item based_receiver;

        if (cblc_emit_prepare_struct_pointer_value(unit, receiver->cobol_name,
                builder, based_name, sizeof(based_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        based_receiver = *receiver;
        based_receiver.kind = CBLC_DATA_KIND_STRUCT;
        based_receiver.is_alias = 0;
        ft_strlcpy(based_receiver.source_name, based_name,
            sizeof(based_receiver.source_name));
        ft_strlcpy(based_receiver.cobol_name, based_name,
            sizeof(based_receiver.cobol_name));
        {
            int based_status;

            if (cblc_method_may_throw(unit, receiver_type, method, 0))
            {
                based_status = cblc_emit_method_exception_body(unit, &based_receiver,
                    method, assign_target ? assign_target : "", builder, 1);
                if (based_status == FT_SUCCESS && caller
                    && cblc_emit_exception_transfer(unit, caller, statement,
                        statement_index, builder) != FT_SUCCESS)
                    based_status = FT_FAILURE;
            }
            else if (cblc_method_is_reusable(method))
                based_status = cblc_emit_method_exception_body(unit, &based_receiver,
                    method, assign_target ? assign_target : "", builder, 1);
            else
                based_status = cblc_emit_method_body(unit, &based_receiver, method,
                    assign_target, builder);
            return (based_status);
        }
    }
    /* A chained method result is held in a normal struct-pointer local, not
     * in the indexed alias form handled above. Resolve its based record view
     * before lowering a method body so vector<T>::at()->next() accesses the
     * object addressed by the intermediate pointer. */
    if (receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];
        t_cblc_data_item based_receiver;
        int based_status;

        if (cblc_build_struct_pointer_based_name(unit, receiver->cobol_name,
                receiver->struct_type_name, based_name, sizeof(based_name))
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line),
                "           SET ADDRESS OF %s TO %s.", based_name,
                receiver->cobol_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        based_receiver = *receiver;
        based_receiver.kind = CBLC_DATA_KIND_STRUCT;
        based_receiver.is_alias = 0;
        ft_strlcpy(based_receiver.source_name, based_name,
            sizeof(based_receiver.source_name));
        ft_strlcpy(based_receiver.cobol_name, based_name,
            sizeof(based_receiver.cobol_name));
        if (cblc_method_may_throw(unit, receiver_type, method, 0))
        {
            based_status = cblc_emit_method_exception_body(unit,
                &based_receiver, method, assign_target ? assign_target : "",
                builder, 1);
            if (based_status == FT_SUCCESS && caller
                && cblc_emit_exception_transfer(unit, caller, statement,
                    statement_index, builder) != FT_SUCCESS)
                based_status = FT_FAILURE;
        }
        else if (cblc_method_is_reusable(method))
        {
            based_status = cblc_emit_method_exception_body(unit,
                &based_receiver, method, assign_target ? assign_target : "",
                builder, 1);
        }
        else
            based_status = cblc_emit_method_body(unit, &based_receiver, method,
                assign_target, builder);
        if (based_status != FT_SUCCESS)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "failed emitting method body '%s::%s' for based receiver '%s'",
                receiver_type->source_name, method->source_name,
                based_receiver.cobol_name);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        }
        return (based_status);
    }
    if (cblc_method_is_reusable(method) && receiver->kind == CBLC_DATA_KIND_STRUCT)
    {
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
        char result_name[TRANSPILE_IDENTIFIER_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_method_paragraph_name(unit, receiver, method, paragraph_name,
                sizeof(paragraph_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (receiver == receiver_context
            || (!cblc_find_data_item_by_cobol(unit, receiver->cobol_name)
            && !cblc_find_data_item_for_emit_by_source(unit,
                receiver->source_name)))
        {
            int based_status;

            if (method->parameter_count > 0
                && cblc_emit_parameter_argument_moves(unit,
                    statement->call_arguments, statement->call_argument_count,
                    method->parameters, method->parameter_count, builder,
                    caller ? caller->parameters : NULL,
                    caller ? caller->parameter_count : 0) != FT_SUCCESS)
            {
                cblc_set_method_exception_lowering_diagnostic(builder,
                    receiver, method, "reusable method argument staging",
                    statement_index);
                return (FT_FAILURE);
            }
            based_status = cblc_emit_method_exception_body(unit, receiver, method,
                assign_target ? assign_target : "", builder, 1);
            if (based_status == FT_SUCCESS && cblc_method_may_throw(unit,
                    receiver_type, method, 0) && caller
                && cblc_emit_exception_transfer(unit, caller, statement,
                    statement_index, builder) != FT_SUCCESS)
                based_status = FT_FAILURE;
            if (based_status != FT_SUCCESS
                || method->return_kind == CBLC_FUNCTION_RETURN_VOID)
                return (based_status);
            if (!assign_target || assign_target[0] == '\0'
                || cblc_method_result_name(receiver, method, result_name,
                    sizeof(result_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (method->return_kind == CBLC_FUNCTION_RETURN_INT
                || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line),
                        "           COMPUTE %s = %s.", assign_target,
                        result_name) < 0)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            {
                if (std::snprintf(line, sizeof(line), "           SET %s TO %s.",
                        assign_target, result_name) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                    result_name, assign_target) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (method->parameter_count > 0
            && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count, method->parameters,
                method->parameter_count, builder,
                caller ? caller->parameters : NULL,
                caller ? caller->parameter_count : 0) != FT_SUCCESS)
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "reusable paragraph argument staging", statement_index);
            return (FT_FAILURE);
        }
        /* The reusable-paragraph fast path stages the ABI pointer slots
         * itself instead of going through cblc_emit_method_exception_body.
         * Bind each class-pointer parameter's BASED view after staging and
         * before the paragraph dereferences it. */
        if (cblc_emit_struct_pointer_parameter_bindings(unit,
                method->parameters, method->parameter_count, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        {
            char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_method_return_paragraph_name(unit, receiver, method,
                    return_paragraph_name, sizeof(return_paragraph_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line),
                    "           PERFORM %s THRU %s.", paragraph_name,
                    return_paragraph_name) < 0)
                return (FT_FAILURE);
        }
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_method_may_throw(unit, receiver_type, method, 0) && caller
            && cblc_emit_exception_transfer(unit, caller, statement, statement_index,
                builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (method->return_kind == CBLC_FUNCTION_RETURN_VOID)
            return (FT_SUCCESS);
        if (!assign_target || assign_target[0] == '\0'
            || cblc_method_result_name(receiver, method, result_name,
                sizeof(result_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (method->return_kind == CBLC_FUNCTION_RETURN_INT
            || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
        {
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    assign_target, result_name) < 0)
                return (FT_FAILURE);
        }
        else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
            || method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
            || method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
            || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
        {
            if (std::snprintf(line, sizeof(line), "           SET %s TO %s.",
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
                method->parameter_count, builder,
                caller ? caller->parameters : NULL,
                caller ? caller->parameter_count : 0) != FT_SUCCESS)
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "argument staging", statement_index);
            return (FT_FAILURE);
        }
    }
    {
        int method_status;

        method_status = cblc_emit_method_body(unit, receiver, method, assign_target, builder);
        if (method_status != FT_SUCCESS)
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "inline body emission", statement_index);
            return (method_status);
        }
        return (method_status);
    }
}

static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_data_item *receiver_context)
{
    return (cblc_emit_method_statement_with_method(unit, statement, builder,
        caller, statement_index, NULL, receiver_context));
}

static int cblc_emit_copy_assignment_with_method(
    const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_method *selected_method, const t_cblc_function *caller,
    size_t statement_index)
{
    const t_cblc_data_item *target_item;
    const t_cblc_data_item *source_item;
    const t_cblc_struct_type *type;
    const t_cblc_method *method;
    int method_belongs_to_type;
    size_t method_index;

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
    method = selected_method ? selected_method
        : cblc_find_method_on_type(type, "operator=");
    if (selected_method)
    {
        method_belongs_to_type = 0;
        method_index = 0;
        while (method_index < type->method_count)
        {
            if (&type->methods[method_index] == selected_method)
            {
                method_belongs_to_type = 1;
                break ;
            }
            method_index += 1;
        }
        if (!method_belongs_to_type)
            return (FT_FAILURE);
    }
    if (!method || !method->has_definition || method->return_kind != CBLC_FUNCTION_RETURN_VOID
        || method->parameter_count != 1)
    {
        return (FT_FAILURE);
    }
    {
        const t_cblc_constructor *copy_constructor;
        int copy_constructed_parameter;

        copy_constructor = cblc_find_copy_constructor(type);
        copy_constructed_parameter = 0;
        if (method->parameters[0].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT
            && copy_constructor && copy_constructor->has_definition
            && copy_constructor->parameter_count == 1
            && copy_constructor->parameters[0].kind
                == TRANSPILE_FUNCTION_PARAMETER_STRUCT
            && std::strcmp(copy_constructor->parameters[0].type_name,
                type->source_name) == 0)
        {
            int copy_return_handled;

            if (cblc_emit_struct_copy_return(unit,
                    method->parameters[0].cobol_name, type->source_name,
                    source_item->source_name, NULL, builder,
                    &copy_return_handled) != FT_SUCCESS)
                return (FT_FAILURE);
            copy_constructed_parameter = copy_return_handled;
            if (copy_constructed_parameter
                && caller && cblc_constructor_may_throw(unit, type,
                    copy_constructor, 0))
            {
                t_cblc_statement transfer_statement;

                transfer_statement = *statement;
                transfer_statement.exception_may_throw = 1;
                if (cblc_emit_exception_transfer(unit, caller,
                        &transfer_statement, statement_index, builder)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        if (!copy_constructed_parameter
            && cblc_emit_parameter_argument_moves(unit, source_item->source_name, 1,
                method->parameters, method->parameter_count, builder) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    {
        int method_status;

        if (cblc_method_may_throw(unit, type, method, 0))
        {
            method_status = cblc_emit_method_exception_body(unit, target_item,
                method, "", builder, 1);
            if (method_status == FT_SUCCESS && caller
                && cblc_emit_exception_transfer(unit, caller, statement,
                    statement_index, builder) != FT_SUCCESS)
                method_status = FT_FAILURE;
        }
        else
            method_status = cblc_emit_method_body(unit, target_item, method,
                NULL, builder);
        return (method_status);
    }
}

static int cblc_emit_copy_assignment(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index)
{
    return (cblc_emit_copy_assignment_with_method(unit, statement, builder,
        NULL, caller, statement_index));
}

static int cblc_emit_struct_pointer_element_copy_assignment(
    const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    t_cobol_text_builder *builder,
    const t_cblc_data_item *receiver_context)
{
    const t_cblc_data_item *target_pointer;
    const t_cblc_struct_type *element_type;
    const t_cblc_method *copy_assignment;
    t_cblc_data_item synthetic_target_pointer;
    t_cblc_data_item based_receiver;
    char target_pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
    const char *target_open;
    size_t target_name_length;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    target_open = std::strchr(statement->target, '[');
    if (!target_open)
        return (FT_FAILURE);
    target_name_length = static_cast<size_t>(target_open - statement->target);
    if (target_name_length == 0 || target_name_length >= sizeof(target_pointer_name))
        return (FT_FAILURE);
    std::memcpy(target_pointer_name, statement->target, target_name_length);
    target_pointer_name[target_name_length] = '\0';
    target_pointer = cblc_resolve_struct_pointer_item(unit, target_pointer_name,
        &synthetic_target_pointer, receiver_context);
    if (!target_pointer || target_pointer->kind != CBLC_DATA_KIND_STRUCT_POINTER)
    {
        return (FT_FAILURE);
    }
    element_type = cblc_find_struct_type(unit, target_pointer->struct_type_name);
    if (!element_type)
    {
        return (FT_FAILURE);
    }
    copy_assignment = cblc_find_method_on_type(element_type, "operator=");
    if (!element_type->has_copy_assignment || !copy_assignment
        || !copy_assignment->has_definition
        || copy_assignment->return_kind != CBLC_FUNCTION_RETURN_VOID
        || copy_assignment->parameter_count != 1
        || copy_assignment->parameters[0].kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT
        || std::strncmp(copy_assignment->parameters[0].type_name,
            element_type->source_name,
            sizeof(copy_assignment->parameters[0].type_name)) != 0)
    {
        const char *source_open;
        const t_cblc_data_item *source_pointer;
        t_cblc_data_item synthetic_source_pointer;
        char source_object_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_object_name[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t field_index;

        if (element_type->has_copy_assignment || copy_assignment)
            return (FT_FAILURE);
        source_open = std::strchr(statement->source, '[');
        if (source_open)
        {
            char source_pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t source_name_length;

            source_name_length = static_cast<size_t>(source_open - statement->source);
            if (source_name_length == 0
                || source_name_length >= sizeof(source_pointer_name))
                return (FT_FAILURE);
            std::memcpy(source_pointer_name, statement->source, source_name_length);
            source_pointer_name[source_name_length] = '\0';
            source_pointer = cblc_resolve_struct_pointer_item(unit,
                source_pointer_name, &synthetic_source_pointer, receiver_context);
            if (!source_pointer)
                return (FT_FAILURE);
            if (source_pointer->kind != CBLC_DATA_KIND_STRUCT_POINTER
                || std::strncmp(source_pointer->struct_type_name,
                    element_type->source_name,
                    sizeof(source_pointer->struct_type_name)) != 0)
                return (FT_FAILURE);
        if (cblc_emit_prepare_struct_pointer_value(unit, statement->source,
                    builder, source_object_name, sizeof(source_object_name),
                    receiver_context) != FT_SUCCESS)
        {
                return (FT_FAILURE);
        }
        }
        else
        {
            const t_cblc_data_item *source_item;

            source_item = cblc_find_data_item_by_cobol(unit, statement->source);
            if (!source_item)
                source_item = cblc_find_data_item_for_emit_by_source(unit,
                    statement->source);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_STRUCT
                || std::strncmp(source_item->struct_type_name,
                    element_type->source_name,
                    sizeof(source_item->struct_type_name)) != 0)
                return (FT_FAILURE);
            ft_strlcpy(source_object_name, source_item->cobol_name,
                sizeof(source_object_name));
        }
        if (cblc_emit_prepare_struct_pointer_value(unit, statement->target, builder,
                target_object_name, sizeof(target_object_name),
                receiver_context) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
        field_index = 0;
        while (field_index < element_type->field_count)
        {
            const t_cblc_struct_field *field;
            char line[TRANSPILE_STATEMENT_TEXT_MAX];

            field = &element_type->fields[field_index];
            if (field->array_count != 0
                || (field->kind != CBLC_DATA_KIND_INT
                    && field->kind != CBLC_DATA_KIND_DOUBLE
                    && field->kind != CBLC_DATA_KIND_CHAR))
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line),
                    "           MOVE %s-%s TO %s-%s.", source_object_name,
                    field->cobol_name, target_object_name, field->cobol_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            field_index += 1;
        }
        return (FT_SUCCESS);
    }

    if (std::strchr(statement->source, '['))
    {
        const char *source_open;
        size_t source_name_length;
        char source_pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char source_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[TRANSPILE_STATEMENT_TEXT_MAX];
        t_cblc_data_item synthetic_source_pointer;
        const t_cblc_data_item *source_pointer;

        source_open = std::strchr(statement->source, '[');
        source_name_length = static_cast<size_t>(source_open - statement->source);
        if (source_name_length == 0 || source_name_length >= sizeof(source_pointer_name))
        {
            return (FT_FAILURE);
        }
        std::memcpy(source_pointer_name, statement->source, source_name_length);
        source_pointer_name[source_name_length] = '\0';
        source_pointer = cblc_resolve_struct_pointer_item(unit, source_pointer_name,
            &synthetic_source_pointer, receiver_context);
        if (!source_pointer || source_pointer->kind != CBLC_DATA_KIND_STRUCT_POINTER
            || std::strncmp(source_pointer->struct_type_name,
                element_type->source_name,
                sizeof(source_pointer->struct_type_name)) != 0
            || cblc_emit_prepare_struct_pointer_value(unit, statement->source,
                builder, source_based_name, sizeof(source_based_name),
                receiver_context) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                source_based_name, copy_assignment->parameters[0].cobol_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
    }
    else
    {
        const t_cblc_data_item *source_item;

        source_item = cblc_find_data_item_by_cobol(unit, statement->source);
        if (!source_item)
            source_item = cblc_find_data_item_for_emit_by_source(unit,
                statement->source);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRUCT
            || std::strncmp(source_item->struct_type_name, element_type->source_name,
                sizeof(source_item->struct_type_name)) != 0
            || cblc_emit_parameter_argument_moves(unit, source_item->source_name, 1,
                copy_assignment->parameters, copy_assignment->parameter_count,
                builder) != FT_SUCCESS)
        {
            return (FT_FAILURE);
        }
    }
    if (cblc_emit_prepare_struct_pointer_value(unit, statement->target, builder,
            target_based_name, sizeof(target_based_name),
            receiver_context) != FT_SUCCESS)
    {
        return (FT_FAILURE);
    }
    based_receiver = *target_pointer;
    based_receiver.kind = CBLC_DATA_KIND_STRUCT;
    based_receiver.is_alias = 0;
    ft_strlcpy(based_receiver.source_name, target_based_name,
        sizeof(based_receiver.source_name));
    ft_strlcpy(based_receiver.cobol_name, target_based_name,
        sizeof(based_receiver.cobol_name));
    ft_strlcpy(based_receiver.struct_type_name, element_type->source_name,
        sizeof(based_receiver.struct_type_name));
    {
        const int method_status = cblc_method_may_throw(unit, element_type,
                copy_assignment, 0)
            ? cblc_emit_method_exception_body(unit, &based_receiver,
                copy_assignment, "", builder, 1)
            : cblc_emit_method_body(unit, &based_receiver, copy_assignment,
                NULL, builder);
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

static int cblc_method_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *owner_type, const t_cblc_method *method,
    size_t depth)
{
    size_t index;

    if (!unit || !owner_type || !method || depth > unit->struct_type_count + 16)
        return (1);
    if (cblc_method_has_exception_control(method))
        return (1);
    index = 0;
    while (index < method->statement_count)
    {
        const t_cblc_statement *statement;
        const char *receiver_name;
        const t_cblc_data_item *receiver_item;
        const t_cblc_struct_type *receiver_type;
        const t_cblc_method *called_method;

        statement = &method->statements[index];
        if (statement->exception_may_throw)
            return (1);
        if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && cblc_statement_constructor_may_throw(unit, statement))
            return (1);
        if (statement->type == CBLC_STATEMENT_CALL
            || statement->type == CBLC_STATEMENT_CALL_ASSIGN
            || statement->type == CBLC_STATEMENT_FILE_OPEN
            || statement->type == CBLC_STATEMENT_FILE_READ
            || statement->type == CBLC_STATEMENT_FILE_WRITE
            || statement->type == CBLC_STATEMENT_FILE_CLOSE)
            return (1);
        if (statement->type != CBLC_STATEMENT_METHOD_CALL
            && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            index += 1;
            continue ;
        }
        receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
            ? statement->target : statement->source;
        receiver_item = cblc_find_data_item_by_cobol(unit, receiver_name);
        if (!receiver_item)
            receiver_item = cblc_find_data_item_for_emit_by_source(unit,
                receiver_name);
        receiver_type = receiver_item
            ? cblc_find_receiver_type(unit, receiver_item) : owner_type;
        if (!receiver_type)
            receiver_type = owner_type;
        called_method = cblc_find_method_on_type_for_arguments(unit,
            receiver_type, statement->call_identifier,
            statement->call_arguments, statement->call_argument_count);
        if (!called_method
            || cblc_method_may_throw(unit, receiver_type, called_method,
                depth + 1))
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_constructor_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *owner_type,
    const t_cblc_constructor *constructor, size_t depth)
{
    size_t index;

    if (!unit || !owner_type || !constructor
        || depth > unit->struct_type_count + 16)
        return (1);
    if (cblc_constructor_has_exception_control(constructor))
        return (1);
    index = 0;
    while (index < constructor->statement_count)
    {
        const t_cblc_statement *statement;
        const char *receiver_name;
        const t_cblc_data_item *receiver_item;
        const t_cblc_struct_type *receiver_type;
        const t_cblc_method *called_method;

        statement = &constructor->statements[index];
        if (statement->exception_may_throw
            || statement->type == CBLC_STATEMENT_CALL
            || statement->type == CBLC_STATEMENT_CALL_ASSIGN
            || statement->type == CBLC_STATEMENT_FILE_OPEN
            || statement->type == CBLC_STATEMENT_FILE_READ
            || statement->type == CBLC_STATEMENT_FILE_WRITE
            || statement->type == CBLC_STATEMENT_FILE_CLOSE)
            return (1);
        if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && cblc_statement_constructor_may_throw(unit, statement))
            return (1);
        if (statement->type != CBLC_STATEMENT_METHOD_CALL
            && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            index += 1;
            continue ;
        }
        receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
            ? statement->target : statement->source;
        receiver_item = cblc_find_data_item_by_cobol(unit, receiver_name);
        if (!receiver_item)
            receiver_item = cblc_find_data_item_for_emit_by_source(unit,
                receiver_name);
        receiver_type = receiver_item
            ? cblc_find_receiver_type(unit, receiver_item) : owner_type;
        if (!receiver_type)
            receiver_type = owner_type;
        called_method = cblc_find_method_on_type_for_arguments(unit,
            receiver_type, statement->call_identifier,
            statement->call_arguments, statement->call_argument_count);
        if (!called_method
            || cblc_method_may_throw(unit, receiver_type, called_method,
                depth + 1))
            return (1);
        index += 1;
    }
    return (0);
}

static const t_cblc_constructor *cblc_find_move_constructor(
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!type)
        return (NULL);
    index = 0;
    while (index < type->constructor_count)
    {
        if (type->constructors[index].is_move_constructor)
            return (&type->constructors[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_constructor *cblc_find_copy_constructor(
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!type)
        return (NULL);
    index = 0;
    while (index < type->constructor_count)
    {
        if (type->constructors[index].is_copy_constructor)
            return (&type->constructors[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_emit_function(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, t_cobol_text_builder *builder,
    int append_stop_run,
    const t_cblc_data_item *receiver_context = NULL,
    const t_cblc_parameter *entry_pointer_parameters = NULL,
    size_t entry_pointer_parameter_count = 0,
    const t_cblc_struct_type *entry_pointer_parameter_owner = NULL);

static int cblc_constructor_has_pointer_parameter(
    const t_cblc_constructor *constructor)
{
    if (!constructor)
        return (0);
    for (size_t index = 0; index < constructor->parameter_count; ++index)
        if (constructor->parameters[index].kind
            == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
            return (1);
    return (0);
}

static int cblc_emit_constructor_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_constructor *constructor,
    size_t constructor_index, t_cobol_text_builder *builder)
{
    t_cobol_text_builder_lifecycle_scope lifecycle_scope(builder, NULL, constructor);
    const t_cblc_struct_type *owner_type;
    t_cblc_function exception_function;
    t_cblc_method parameter_context;
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    owner_type = unit && receiver
        ? cblc_find_struct_type(unit, receiver->struct_type_name) : NULL;
    if (!unit || !receiver || !constructor || !builder || !owner_type
        || cblc_constructor_paragraph_name(receiver, constructor_index,
            paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
        || cblc_constructor_return_paragraph_name(unit, receiver,
            constructor_index, return_paragraph_name,
            sizeof(return_paragraph_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memset(&exception_function, 0, sizeof(exception_function));
    ft_strlcpy(exception_function.source_name,
        constructor->scope_source_name[0] != '\0'
            ? constructor->scope_source_name : receiver->source_name,
        sizeof(exception_function.source_name));
    ft_strlcpy(exception_function.cobol_name, paragraph_name,
        sizeof(exception_function.cobol_name));
    ft_strlcpy(exception_function.paragraph_return_label, return_paragraph_name,
        sizeof(exception_function.paragraph_return_label));
    exception_function.returns_to_paragraph = 1;
    exception_function.return_kind = CBLC_FUNCTION_RETURN_VOID;
    exception_function.parameter_count = constructor->parameter_count;
    std::memset(&parameter_context, 0, sizeof(parameter_context));
    parameter_context.parameter_count = constructor->parameter_count;
    parameter_context.template_storage_data_start =
        constructor->template_storage_data_start;
    parameter_context.template_storage_data_count =
        constructor->template_storage_data_count;
    if (constructor->parameter_count > 0)
    {
        if (constructor->parameter_count > TRANSPILE_FUNCTION_PARAMETER_MAX)
            return (FT_FAILURE);
        std::memcpy(exception_function.parameters, constructor->parameters,
            constructor->parameter_count * sizeof(*constructor->parameters));
        std::memcpy(parameter_context.parameters, constructor->parameters,
            constructor->parameter_count * sizeof(*constructor->parameters));
    }
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
        char rewritten_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char rewritten_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char rewritten_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char parameter_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char parameter_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char parameter_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

        exception_function.statements[index] = constructor->statements[index];
        if (cblc_rewrite_method_field_references(unit, receiver, &parameter_context,
                constructor->statements[index].target, rewritten_target,
                sizeof(rewritten_target)) != FT_SUCCESS
            || cblc_rewrite_method_field_references(unit, receiver, &parameter_context,
                constructor->statements[index].source, rewritten_source,
                sizeof(rewritten_source)) != FT_SUCCESS
            || cblc_rewrite_method_field_references(unit, receiver, &parameter_context,
                constructor->statements[index].call_arguments, rewritten_arguments,
                sizeof(rewritten_arguments)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, &parameter_context,
                rewritten_target, parameter_target,
                sizeof(parameter_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, &parameter_context,
                rewritten_source, parameter_source,
                sizeof(parameter_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, &parameter_context,
                rewritten_arguments, parameter_arguments,
                sizeof(parameter_arguments)) != FT_SUCCESS
            || cblc_replace_this_prefix(parameter_target, receiver->cobol_name,
                exception_function.statements[index].target,
                sizeof(exception_function.statements[index].target)) != FT_SUCCESS
            || cblc_replace_this_prefix(parameter_source, receiver->cobol_name,
                exception_function.statements[index].source,
                sizeof(exception_function.statements[index].source)) != FT_SUCCESS
            || cblc_replace_this_prefix(parameter_arguments, receiver->cobol_name,
                exception_function.statements[index].call_arguments,
                sizeof(exception_function.statements[index].call_arguments)) != FT_SUCCESS)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "failed to rewrite exception constructor statement %zu (kind %d, call '%s') for '%s'",
                index, static_cast<int>(constructor->statements[index].type),
                constructor->statements[index].call_identifier,
                receiver->struct_type_name);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        /* Constructors are stored separately from methods, so their
         * unqualified same-type calls (for example max_size()) do not pass
         * through method-body implicit-self resolution. Resolve those calls
         * here before the shared function emitter can misclassify them as
         * external COBOL CALLs. */
        if (exception_function.statements[index].type
                == CBLC_STATEMENT_CALL_ASSIGN
            && exception_function.statements[index].call_is_external)
        {
            t_cblc_statement *statement =
                &exception_function.statements[index];
            const t_cblc_method *self_method =
                cblc_find_method_on_type_for_arguments(unit, owner_type,
                    statement->call_identifier, statement->call_arguments,
                    statement->call_argument_count);
            const t_cblc_data_item *call_target =
                cblc_find_data_item_by_cobol(unit, statement->target);
            int return_matches_target = 0;

            if (self_method && call_target)
            {
                if (self_method->return_kind == CBLC_FUNCTION_RETURN_INT)
                    return_matches_target = call_target->kind
                        == CBLC_DATA_KIND_INT;
                else if (self_method->return_kind
                        == CBLC_FUNCTION_RETURN_CHAR)
                    return_matches_target = call_target->kind
                        == CBLC_DATA_KIND_CHAR;
                else if (self_method->return_kind
                        == CBLC_FUNCTION_RETURN_DOUBLE)
                    return_matches_target = call_target->kind
                        == CBLC_DATA_KIND_DOUBLE;
            }
            if (return_matches_target)
            {
                statement->type = CBLC_STATEMENT_METHOD_CALL_ASSIGN;
                ft_strlcpy(statement->source, receiver->cobol_name,
                    sizeof(statement->source));
            }
        }
        {
            size_t cleanup_index;

            cleanup_index = 0;
            while (cleanup_index < exception_function.statements[index]
                .exception_cleanup_count)
            {
                char substituted_cleanup[TRANSPILE_IDENTIFIER_MAX];

                char rewritten_cleanup[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_rewrite_method_field_references(unit, receiver,
                        &parameter_context,
                        constructor->statements[index]
                            .exception_cleanup_targets[cleanup_index],
                        rewritten_cleanup, sizeof(rewritten_cleanup)) != FT_SUCCESS
                    || cblc_replace_this_prefix(
                        rewritten_cleanup,
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
    if (cblc_emit_function(unit, &exception_function, builder, 0, receiver,
            constructor->parameters, constructor->parameter_count, owner_type)
        != FT_SUCCESS)
    {
        if (builder->diagnostic_message[0] == '\0')
        {
            const t_cblc_statement *failed_statement = builder->active_statement;
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "failed emitting exception constructor %zu for '%s' at nested function '%s' statement %zu (kind %d, target '%.32s', source '%.32s', args %zu)",
                constructor_index, receiver->struct_type_name,
                builder->active_function_name ? builder->active_function_name : "<none>",
                builder->active_statement_index,
                failed_statement ? static_cast<int>(failed_statement->type) : -1,
                failed_statement ? failed_statement->target : "",
                failed_statement ? failed_statement->source : "",
                failed_statement ? failed_statement->call_argument_count : 0);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        }
        cma_free(exception_function.statements);
        return (FT_FAILURE);
    }
    if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
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
    int control_flow_depth;

    if (!unit || !item || !statement || !builder
        || item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type || cblc_build_struct_pointer_based_name(unit, item->cobol_name,
            item->struct_type_name, based_name,
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
                constructor->parameters, constructor->parameter_count, builder,
                NULL, 0, constructor->is_copy_constructor) != FT_SUCCESS)
            return (FT_FAILURE);
        if (constructor->statement_count == 0)
        {
            if (std::snprintf(line, sizeof(line), "           INITIALIZE %s.", based_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        control_flow_depth = 0;
        index = 0;
        while (index < constructor->statement_count)
        {
            if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                    &constructor->statements[index], based_name, builder,
                    &control_flow_depth) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    if (type->destructor_statement_count == 0)
        return (FT_SUCCESS);
    control_flow_depth = 0;
    index = 0;
    while (index < type->destructor_statement_count)
    {
        if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                &type->destructor_statements[index], based_name, builder,
                &control_flow_depth) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_pointer_lifecycle_body_statement(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *based_name, const t_cblc_statement *statement,
    t_cobol_text_builder *builder, int *control_flow_depth,
    const t_cblc_parameter *parameters = NULL, size_t parameter_count = 0)
{
    if (!unit || !type || !based_name || !statement || !builder)
        return (FT_FAILURE);
    if (((statement->type == CBLC_STATEMENT_CALL && !statement->call_is_external)
            || (statement->type == CBLC_STATEMENT_METHOD_CALL
                && std::strcmp(statement->target, "CBLC-THIS") == 0))
        && statement->call_argument_count == 0)
    {
        const t_cblc_method *method;

        method = cblc_find_method_on_type_for_arguments(unit, type,
            statement->call_identifier, statement->call_arguments,
            statement->call_argument_count);
        if (method && method->has_definition
            && method->return_kind == CBLC_FUNCTION_RETURN_VOID
            && method->parameter_count == 0)
        {
            t_cblc_data_item receiver;

            std::memset(&receiver, 0, sizeof(receiver));
            receiver.kind = CBLC_DATA_KIND_STRUCT;
            ft_strlcpy(receiver.source_name, based_name, sizeof(receiver.source_name));
            ft_strlcpy(receiver.cobol_name, based_name, sizeof(receiver.cobol_name));
            ft_strlcpy(receiver.struct_type_name, type->source_name,
                sizeof(receiver.struct_type_name));
            return (cblc_emit_method_body(unit, &receiver, method, NULL, builder));
        }
    }
    {
        t_cblc_data_item receiver;
        t_cblc_method parameter_context;
        t_cblc_statement rewritten_statement;
        const t_cblc_statement *effective_statement;

        std::memset(&receiver, 0, sizeof(receiver));
        receiver.kind = CBLC_DATA_KIND_STRUCT;
        ft_strlcpy(receiver.source_name, based_name, sizeof(receiver.source_name));
        ft_strlcpy(receiver.cobol_name, based_name, sizeof(receiver.cobol_name));
        ft_strlcpy(receiver.struct_type_name, type->source_name,
            sizeof(receiver.struct_type_name));
        effective_statement = statement;
        if (parameters && parameter_count > 0
            && parameter_count <= TRANSPILE_FUNCTION_PARAMETER_MAX)
        {
            char rewritten_target[TRANSPILE_STATEMENT_TEXT_MAX];
            char rewritten_source[TRANSPILE_STATEMENT_TEXT_MAX];
            char rewritten_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
            char parameter_target[TRANSPILE_STATEMENT_TEXT_MAX];
            char parameter_source[TRANSPILE_STATEMENT_TEXT_MAX];
            char parameter_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

            std::memset(&parameter_context, 0, sizeof(parameter_context));
            parameter_context.parameter_count = parameter_count;
            std::memcpy(parameter_context.parameters, parameters,
                parameter_count * sizeof(*parameters));
            if (cblc_rewrite_method_field_references(unit, &receiver,
                    &parameter_context, statement->target, rewritten_target,
                    sizeof(rewritten_target)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, &receiver,
                    &parameter_context, statement->source, rewritten_source,
                    sizeof(rewritten_source)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, &receiver,
                    &parameter_context, statement->call_arguments,
                    rewritten_arguments, sizeof(rewritten_arguments)) != FT_SUCCESS)
                return (FT_FAILURE);
            /* Lifecycle bodies are emitted outside the ordinary method-body
             * path. Apply the same parameter-to-ABI-name rewrite here after
             * resolving receiver fields, otherwise constructor names such as
             * `source` and `value` leak into generated COBOL verbatim. */
            if (cblc_rewrite_method_parameter_references(unit, &parameter_context,
                    rewritten_target, parameter_target,
                    sizeof(parameter_target)) != FT_SUCCESS
                || cblc_rewrite_method_parameter_references(unit, &parameter_context,
                    rewritten_source, parameter_source,
                    sizeof(parameter_source)) != FT_SUCCESS
                || cblc_rewrite_method_parameter_references(unit, &parameter_context,
                    rewritten_arguments, parameter_arguments,
                    sizeof(parameter_arguments)) != FT_SUCCESS)
                return (FT_FAILURE);
            rewritten_statement = *statement;
            ft_strlcpy(rewritten_statement.target, parameter_target,
                sizeof(rewritten_statement.target));
            ft_strlcpy(rewritten_statement.source, parameter_source,
                sizeof(rewritten_statement.source));
            ft_strlcpy(rewritten_statement.call_arguments, parameter_arguments,
                sizeof(rewritten_statement.call_arguments));
            effective_statement = &rewritten_statement;
        }
    {
        const int status = cblc_emit_substituted_statement_with_flow(unit, NULL,
            effective_statement, based_name, builder, control_flow_depth, &receiver);

        return (status);
    }
    }
}

static int cblc_emit_save_pointer_element_destination(
    t_cobol_text_builder *builder)
{
    if (!builder
        || cobol_text_builder_append_line(builder,
            "           ADD 1 TO CBLC-PTR-SAVE-DEPTH.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder,
        "           SET CBLC-PTR-CURSOR-SAVED(CBLC-PTR-SAVE-DEPTH) TO CBLC-PTR-CURSOR."));
}

static int cblc_emit_restore_pointer_element_destination(
    const char *based_name, t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!based_name || !builder
        || cobol_text_builder_append_line(builder,
            "           SET CBLC-PTR-CURSOR TO CBLC-PTR-CURSOR-SAVED(CBLC-PTR-SAVE-DEPTH).") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           SUBTRACT 1 FROM CBLC-PTR-SAVE-DEPTH.") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "           SET ADDRESS OF %s TO CBLC-PTR-CURSOR.",
            based_name) < 0)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line));
}

static int cblc_emit_pointer_element_lifecycle(const t_cblc_translation_unit *unit,
    const char *reference, int is_destruct, t_cobol_text_builder *builder,
    const t_cblc_function *caller, const t_cblc_statement *statement,
    size_t statement_index, const t_cblc_data_item *receiver_context,
    const t_cblc_constructor *typed_constructor)
{
    const t_cblc_data_item *pointer_item;
    t_cblc_data_kind element_kind;
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[256];
    int has_index;
    int has_pointer_element;
    int has_struct_pointer_element;
    t_cblc_data_item synthetic_pointer_item;

    if (!unit || !reference || !builder)
        return (FT_FAILURE);
    has_struct_pointer_element = 0;
    has_pointer_element = cblc_parse_pointer_element_reference(unit, reference,
        &pointer_item,
            &element_kind, index_expression, sizeof(index_expression), &has_index)
        == FT_SUCCESS;
    if (has_pointer_element)
        has_struct_pointer_element = element_kind == CBLC_DATA_KIND_STRUCT
            && !is_destruct;
    else if (!is_destruct)
    {
        const char *open_bracket;

        open_bracket = std::strchr(reference, '[');
        if (open_bracket)
        {
            char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t pointer_name_length;

            pointer_name_length = static_cast<size_t>(open_bracket - reference);
            if (pointer_name_length > 0
                && pointer_name_length < sizeof(pointer_name))
            {
                std::memcpy(pointer_name, reference, pointer_name_length);
                pointer_name[pointer_name_length] = '\0';
                pointer_item = cblc_resolve_struct_pointer_item(unit,
                    pointer_name, &synthetic_pointer_item, receiver_context);
                has_struct_pointer_element = pointer_item
                    && pointer_item->kind == CBLC_DATA_KIND_STRUCT_POINTER;
            }
        }
    }
    if (has_pointer_element && !has_struct_pointer_element)
    {
        if (is_destruct)
        {
            const t_cblc_struct_type *element_type;
            int control_flow_depth;
            size_t destructor_index;

            if (element_kind != CBLC_DATA_KIND_STRUCT)
                return (cobol_text_builder_append_line(builder,
                    "           CONTINUE."));
            if (cblc_emit_prepare_pointer_element(unit, reference, builder,
                    based_name, sizeof(based_name), receiver_context, caller)
                    != FT_SUCCESS
                || !pointer_item
                || pointer_item->struct_type_name[0] == '\0')
                return (FT_FAILURE);
            element_type = cblc_find_struct_type(unit,
                pointer_item->struct_type_name);
            if (!element_type)
                return (FT_FAILURE);
            control_flow_depth = 0;
            destructor_index = 0;
            while (destructor_index < element_type->destructor_statement_count)
            {
                if (cblc_emit_pointer_lifecycle_body_statement(unit, element_type,
                        based_name,
                        &element_type->destructor_statements[destructor_index],
                        builder, &control_flow_depth) != FT_SUCCESS)
                    return (FT_FAILURE);
                destructor_index += 1;
            }
            if (control_flow_depth != 0
                || cblc_emit_lifecycle_recursive(unit, element_type, based_name,
                    builder, 1, caller) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (cblc_emit_prepare_pointer_element(unit, reference, builder,
                based_name, sizeof(based_name), receiver_context, caller)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (statement && statement->call_argument_count == 1)
        {
            char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
            char source_based_name[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(statement, 0, source_reference,
                    sizeof(source_reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (element_kind == CBLC_DATA_KIND_INT
                || element_kind == CBLC_DATA_KIND_CHAR)
            {
                const t_cblc_data_item *source_item;

                source_item = cblc_find_data_item_const(unit, source_reference);
                if (!source_item)
                    source_item = cblc_find_data_item_for_emit_by_source(unit,
                        source_reference);
                if (source_item
                    && source_item->kind == element_kind)
                    ft_strlcpy(source_based_name, source_item->cobol_name,
                        sizeof(source_based_name));
                else if (cblc_emit_prepare_pointer_element_slot(unit,
                        source_reference, builder, source_based_name,
                        sizeof(source_based_name), 1, receiver_context, NULL,
                        caller) != FT_SUCCESS
                    && cblc_translate_expression_for_cobol(source_reference,
                        source_based_name, sizeof(source_based_name)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (cblc_emit_prepare_pointer_element(unit, source_reference,
                    builder, source_based_name, sizeof(source_based_name),
                    receiver_context) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line),
                    "           MOVE %s TO %s.", source_based_name,
                    based_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (element_kind == CBLC_DATA_KIND_INT)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.",
                    based_name) < 0)
                return (FT_FAILURE);
        }
        else if (element_kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE LOW-VALUES TO %s.",
                    based_name) < 0)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (!has_struct_pointer_element
        && cblc_emit_prepare_pointer_element_slot(unit, reference, builder,
            based_name, sizeof(based_name), 0, receiver_context,
            &element_kind) == FT_SUCCESS)
    {
        if (is_destruct)
            return (cobol_text_builder_append_line(builder,
                "           CONTINUE."));
        if (statement && statement->call_argument_count == 1)
        {
            char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
            char source_value[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(statement, 0, source_reference,
                    sizeof(source_reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_emit_prepare_pointer_element_slot(unit, source_reference,
                    builder, source_value, sizeof(source_value), 1,
                    receiver_context, NULL, caller) != FT_SUCCESS
                && cblc_translate_expression_for_cobol(source_reference,
                    source_value, sizeof(source_value)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line),
                    "           MOVE %s TO %s.", source_value, based_name) < 0)
                return (FT_FAILURE);
        }
        else if (element_kind == CBLC_DATA_KIND_INT)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.",
                    based_name) < 0)
                return (FT_FAILURE);
        }
        else if (element_kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line),
                    "           MOVE LOW-VALUES TO %s.", based_name) < 0)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (cblc_emit_prepare_struct_pointer_value(unit, reference, builder,
            based_name, sizeof(based_name), receiver_context) != FT_SUCCESS)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "cannot resolve indexed class element storage for lifecycle operation '%s' (receiver '%s', type '%s')",
            reference, receiver_context ? receiver_context->cobol_name : "",
            receiver_context ? receiver_context->struct_type_name : "");
        cobol_text_builder_set_diagnostic(builder, 0,
            diagnostic);
        return (FT_FAILURE);
    }
    {
        const char *open_bracket;
        char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t pointer_name_length;

        open_bracket = std::strchr(reference, '[');
        pointer_name_length = open_bracket
            ? static_cast<size_t>(open_bracket - reference) : std::strlen(reference);
        if (pointer_name_length == 0 || pointer_name_length >= sizeof(pointer_name))
            return (FT_FAILURE);
        std::memcpy(pointer_name, reference, pointer_name_length);
        pointer_name[pointer_name_length] = '\0';
        pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_name,
            &synthetic_pointer_item, receiver_context);
        if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
        {
            cobol_text_builder_set_diagnostic(builder, 0,
                "indexed class lifecycle target does not resolve to a class pointer");
            return (FT_FAILURE);
        }
    }
    {
        const t_cblc_struct_type *type;
        size_t index;
        int control_flow_depth;

        type = cblc_find_struct_type(unit, pointer_item->struct_type_name);
        if (!type)
        {
            cobol_text_builder_set_diagnostic(builder, 0,
                "indexed class lifecycle target has no canonical class type");
            return (FT_FAILURE);
        }
        if (typed_constructor)
        {
            int belongs_to_type = 0;

            for (size_t constructor_index = 0;
                constructor_index < type->constructor_count; ++constructor_index)
                if (&type->constructors[constructor_index] == typed_constructor)
                {
                    belongs_to_type = 1;
                    break ;
                }
            if (!belongs_to_type || is_destruct)
                return (FT_FAILURE);
        }
        control_flow_depth = 0;
        if (is_destruct)
        {
            if (type->destructor_statement_count == 0)
                return (cblc_emit_lifecycle_recursive(unit, type, based_name,
                    builder, 1));
            index = 0;
            while (index < type->destructor_statement_count)
            {
                if (cblc_emit_pointer_lifecycle_body_statement(unit, type,
                        based_name, &type->destructor_statements[index], builder,
                        &control_flow_depth) != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
            }
            if (cblc_emit_lifecycle_recursive(unit, type, based_name,
                    builder, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (statement && statement->is_relocate_construct
                && type->has_copy_constructor
                && cblc_constructor_may_throw(unit, type,
                    cblc_find_move_constructor(type), 0))
            {
                const t_cblc_constructor *copy_constructor;
                const t_cblc_data_item *source_object_item;
                t_cblc_data_item based_receiver;
                char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                size_t constructor_index;

                copy_constructor = cblc_find_copy_constructor(type);
                source_object_item = NULL;
                if (!copy_constructor || !copy_constructor->has_definition
                    || (typed_constructor
                        && typed_constructor != copy_constructor)
                    || copy_constructor->parameter_count != 1
                    || copy_constructor->parameters[0].kind
                        != TRANSPILE_FUNCTION_PARAMETER_STRUCT
                    || std::strcmp(copy_constructor->parameters[0].type_name,
                        type->source_name) != 0
                    || cblc_extract_call_argument(statement, 0, source_reference,
                        sizeof(source_reference)) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                        "relocation copy fallback has no matching one-argument copy constructor or source");
                    return (FT_FAILURE);
                }
                if (cblc_emit_save_pointer_element_destination(builder)
                    != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                        "relocation copy fallback could not preserve the destination pointer view");
                    return (FT_FAILURE);
                }
                source_object_item = cblc_find_data_item_for_emit_by_source(unit,
                    source_reference);
                if (source_object_item
                    && source_object_item->kind == CBLC_DATA_KIND_STRUCT
                    && std::strcmp(source_object_item->struct_type_name,
                        type->source_name) == 0)
                {
                    if (ft_strlcpy(source_based_name,
                            source_object_item->cobol_name,
                            sizeof(source_based_name))
                        >= sizeof(source_based_name))
                        return (FT_FAILURE);
                }
                else if (cblc_emit_prepare_struct_pointer_value(unit,
                        source_reference, builder, source_based_name,
                        sizeof(source_based_name), receiver_context, caller) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "relocation copy fallback source '%s' did not resolve to a concrete object or class pointer (kind=%d type='%s')",
                        source_reference,
                        source_object_item
                            ? static_cast<int>(source_object_item->kind) : -1,
                        source_object_item
                            ? source_object_item->struct_type_name : "<unresolved>");
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                    return (FT_FAILURE);
                }
                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s TO %s.", source_based_name,
                        copy_constructor->parameters[0].cobol_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                        "relocation copy fallback could not stage its source argument");
                    return (FT_FAILURE);
                }
                /* Preparing a pointer-backed source object can rebind the
                 * shared BASED layout for this class to the source element.
                 * Restore the destination binding after staging the by-value
                 * copy argument, before the constructor writes its receiver. */
                if (cblc_emit_restore_pointer_element_destination(based_name,
                        builder) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                        "relocation copy fallback could not restore the destination pointer view");
                    return (FT_FAILURE);
                }
                based_receiver = *pointer_item;
                based_receiver.kind = CBLC_DATA_KIND_STRUCT;
                based_receiver.is_alias = 0;
                ft_strlcpy(based_receiver.declared_type_name,
                    based_receiver.struct_type_name,
                    sizeof(based_receiver.declared_type_name));
                ft_strlcpy(based_receiver.source_name, based_name,
                    sizeof(based_receiver.source_name));
                ft_strlcpy(based_receiver.cobol_name, based_name,
                    sizeof(based_receiver.cobol_name));
                constructor_index = static_cast<size_t>(copy_constructor
                    - type->constructors);
                if (cblc_constructor_paragraph_name(&based_receiver,
                        constructor_index, paragraph_name,
                        sizeof(paragraph_name)) != FT_SUCCESS
                    || cblc_constructor_return_paragraph_name(unit,
                        &based_receiver, constructor_index,
                        return_paragraph_name,
                        sizeof(return_paragraph_name)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "           PERFORM %s THRU %s.", paragraph_name,
                        return_paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                        "relocation copy fallback could not emit the selected copy constructor call");
                    return (FT_FAILURE);
                }
                if (caller && cblc_constructor_may_throw(unit, type,
                        copy_constructor, 0)
                    && cblc_emit_exception_transfer(unit, caller, statement,
                        statement_index, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            if (statement && statement->is_relocate_construct
                && !type->has_copy_constructor
                && cblc_find_move_constructor(type))
            {
                const t_cblc_constructor *move_constructor;
                const t_cblc_data_item *source_pointer_item;
                t_cblc_data_item source_synthetic_pointer_item;
                t_cblc_data_item based_receiver;
                char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_based_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                const char *source_open_bracket;
                size_t source_pointer_name_length;
                size_t constructor_index;

                move_constructor = cblc_find_move_constructor(type);
                if (!move_constructor || !move_constructor->has_definition
                    || (typed_constructor
                        && typed_constructor != move_constructor)
                    || move_constructor->parameter_count != 1
                    || move_constructor->parameters[0].kind
                        != TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
                    return (FT_FAILURE);
                if (cblc_constructor_may_throw(unit, type, move_constructor, 0))
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "vector reallocation requires a copy constructor or a non-throwing move constructor; '%s' only has a potentially throwing move constructor",
                        type->source_name);
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_VECTOR_THROWING_MOVE_RELOCATION,
                        diagnostic);
                    return (FT_FAILURE);
                }
                if (cblc_extract_call_argument(statement, 0, source_reference,
                        sizeof(source_reference)) != FT_SUCCESS)
                    return (FT_FAILURE);
                source_open_bracket = std::strchr(source_reference, '[');
                source_pointer_name_length = source_open_bracket
                    ? static_cast<size_t>(source_open_bracket - source_reference) : 0;
                if (source_pointer_name_length == 0
                    || source_pointer_name_length >= sizeof(source_pointer_name))
                    return (FT_FAILURE);
                std::memcpy(source_pointer_name, source_reference,
                    source_pointer_name_length);
                source_pointer_name[source_pointer_name_length] = '\0';
                source_pointer_item = cblc_resolve_struct_pointer_item(unit,
                    source_pointer_name, &source_synthetic_pointer_item,
                    receiver_context);
                if (!source_pointer_item
                    || source_pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                    || std::strcmp(source_pointer_item->struct_type_name,
                        type->source_name) != 0)
                    return (FT_FAILURE);
                if (cblc_emit_save_pointer_element_destination(builder)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_emit_prepare_struct_pointer_value(unit, source_reference,
                        builder, source_based_name, sizeof(source_based_name),
                        receiver_context, caller) != FT_SUCCESS)
                    return (FT_FAILURE);
                /* The source and destination often have the same concrete
                 * class type and therefore share one BASED layout. Capture
                 * the source object's address while that view still points
                 * at the source element; restoring the destination view
                 * first would make ADDRESS OF source_based_name refer to the
                 * destination and accidentally lower a self-move. */
                if (std::snprintf(line, sizeof(line),
                        "           SET %s TO ADDRESS OF %s.",
                        move_constructor->parameters[0].cobol_name,
                        source_based_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_emit_restore_pointer_element_destination(based_name,
                        builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                based_receiver = *pointer_item;
                based_receiver.kind = CBLC_DATA_KIND_STRUCT;
                based_receiver.is_alias = 0;
                ft_strlcpy(based_receiver.source_name, based_name,
                    sizeof(based_receiver.source_name));
                ft_strlcpy(based_receiver.cobol_name, based_name,
                    sizeof(based_receiver.cobol_name));
                constructor_index = static_cast<size_t>(move_constructor
                    - type->constructors);
                if (cblc_constructor_paragraph_name(&based_receiver,
                        constructor_index, paragraph_name,
                        sizeof(paragraph_name)) != FT_SUCCESS
                    || cblc_constructor_return_paragraph_name(unit,
                        &based_receiver, constructor_index,
                        return_paragraph_name,
                        sizeof(return_paragraph_name)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "           PERFORM %s THRU %s.", paragraph_name,
                        return_paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            const t_cblc_constructor *constructor;
            int parameter_moves_succeeded;
            const char *constructor_arguments;
            char rewritten_constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
            t_cblc_method caller_parameter_context;

            constructor = NULL;
            constructor_arguments = statement ? statement->call_arguments : "";
            if (caller && receiver_context && statement
                && statement->call_argument_count > 0
                && caller->parameter_count > 0
                && caller->parameter_count <= TRANSPILE_FUNCTION_PARAMETER_MAX
                && (receiver_context->kind == CBLC_DATA_KIND_STRUCT
                    || receiver_context->kind == CBLC_DATA_KIND_STRUCT_POINTER))
            {
                std::memset(&caller_parameter_context, 0,
                    sizeof(caller_parameter_context));
                caller_parameter_context.parameter_count = caller->parameter_count;
                std::memcpy(caller_parameter_context.parameters,
                    caller->parameters, caller->parameter_count
                        * sizeof(*caller->parameters));
                /* Constructor-exception statements carry parameter fields in
                 * their concrete COBOL spelling already. Rewriting those as
                 * source identifiers would bind e.g. source.data[i] against
                 * the destination receiver and corrupt the nested layout. */
                if (std::strncmp(statement->call_arguments, "CBLC-P-",
                        sizeof("CBLC-P-") - 1) != 0)
                {
                    if (cblc_rewrite_method_field_references(unit, receiver_context,
                            &caller_parameter_context, statement->call_arguments,
                            rewritten_constructor_arguments,
                            sizeof(rewritten_constructor_arguments)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    constructor_arguments = rewritten_constructor_arguments;
                }
            }
            if (typed_constructor && statement
                && statement->call_argument_count > 0)
            {
                /* Re-resolve against the final lowered argument spellings.
                 * Literal materialization and receiver rebinding can change
                 * an argument from source syntax into a concrete typed
                 * temporary; a pre-lowering constructor symbol must not
                 * override that concrete ABI type. */
                constructor = cblc_find_constructor_for_arguments(unit, type,
                    constructor_arguments, statement->call_argument_count);
                if (!constructor)
                    constructor = typed_constructor;
            }
            else if (typed_constructor)
                constructor = typed_constructor;
            else if (statement)
                constructor = cblc_find_constructor_for_arguments(unit, type,
                    constructor_arguments, statement->call_argument_count);
            else
                constructor = cblc_find_constructor_for_arguments(unit, type, NULL, 0);
            if (statement && statement->is_move_construct
                && (!constructor || !constructor->is_move_constructor))
                return (FT_FAILURE);
            if (statement && statement->call_argument_count == 1 && !constructor)
            {
                size_t constructor_index;

                constructor_index = 0;
                while (constructor_index < type->constructor_count)
                {
                    if (type->constructors[constructor_index].is_copy_constructor)
                    {
                        constructor = &type->constructors[constructor_index];
                        break ;
                    }
                    constructor_index += 1;
                }
            }
            if (statement && statement->call_argument_count == 1 && !constructor)
            {
                t_cblc_statement copy_statement;

                std::memset(&copy_statement, 0, sizeof(copy_statement));
                copy_statement.type = CBLC_STATEMENT_ASSIGNMENT;
                ft_strlcpy(copy_statement.target, reference,
                    sizeof(copy_statement.target));
                if (cblc_extract_call_argument(statement, 0,
                        copy_statement.source,
                        sizeof(copy_statement.source)) != FT_SUCCESS
                    || cblc_emit_struct_pointer_element_copy_assignment(unit,
                        &copy_statement, builder, receiver_context) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (FT_SUCCESS);
            }
            parameter_moves_succeeded = 1;
            if (constructor && constructor->parameter_count > 0
                && cblc_emit_save_pointer_element_destination(builder)
                    != FT_SUCCESS)
                return (FT_FAILURE);
            if (constructor && constructor->parameter_count > 0
                && (!statement
                    || cblc_emit_parameter_argument_moves(unit,
                        constructor_arguments,
                        statement->call_argument_count,
                        constructor->parameters, constructor->parameter_count,
                        builder, caller ? caller->parameters : NULL,
                        caller ? caller->parameter_count : 0,
                        constructor->is_copy_constructor) != FT_SUCCESS))
            {
                parameter_moves_succeeded = 0;
            }
            if (!parameter_moves_succeeded && statement
                && statement->call_argument_count == 1)
            {
                t_cblc_statement default_construct;
                t_cblc_statement copy_assignment;

                default_construct = *statement;
                default_construct.call_arguments[0] = '\0';
                default_construct.call_argument_count = 0;
                if (cblc_emit_restore_pointer_element_destination(based_name,
                        builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_emit_pointer_element_lifecycle(unit, reference, 0,
                        builder, caller, &default_construct, statement_index,
                        receiver_context, NULL) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "copy-constructor argument binding failed and default element construction failed");
                    return (FT_FAILURE);
                }
                std::memset(&copy_assignment, 0, sizeof(copy_assignment));
                copy_assignment.type = CBLC_STATEMENT_ASSIGNMENT;
                ft_strlcpy(copy_assignment.target, reference,
                    sizeof(copy_assignment.target));
                if (cblc_extract_call_argument(statement, 0,
                        copy_assignment.source,
                        sizeof(copy_assignment.source)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_emit_struct_pointer_element_copy_assignment(unit,
                        &copy_assignment, builder, receiver_context) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "copy-constructor argument binding and class element copy-assignment both failed");
                    return (FT_FAILURE);
                }
                return (FT_SUCCESS);
            }
            if (constructor && constructor->parameter_count > 0
                && cblc_emit_restore_pointer_element_destination(based_name,
                    builder) != FT_SUCCESS)
                return (FT_FAILURE);
            if (constructor && constructor->has_definition
                && constructor->statement_count > 0)
            {
                t_cblc_data_item based_receiver;
                char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                size_t constructor_index;

                constructor_index = static_cast<size_t>(constructor - type->constructors);
                based_receiver = *pointer_item;
                based_receiver.kind = CBLC_DATA_KIND_STRUCT;
                based_receiver.is_alias = 0;
                ft_strlcpy(based_receiver.declared_type_name,
                    based_receiver.struct_type_name,
                    sizeof(based_receiver.declared_type_name));
                ft_strlcpy(based_receiver.source_name, based_name,
                    sizeof(based_receiver.source_name));
                ft_strlcpy(based_receiver.cobol_name, based_name,
                    sizeof(based_receiver.cobol_name));
                if (cblc_constructor_paragraph_name(&based_receiver,
                        constructor_index, paragraph_name,
                        sizeof(paragraph_name)) != FT_SUCCESS
                    || cblc_constructor_return_paragraph_name(unit, &based_receiver,
                        constructor_index, return_paragraph_name,
                        sizeof(return_paragraph_name)) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "cannot name the selected indexed-element constructor paragraph");
                    return (FT_FAILURE);
                }
                {
                    size_t request_index;
                    int already_requested;

                    request_index = 0;
                    already_requested = 0;
                    while (request_index
                        < builder->lifecycle_constructor_requests.size())
                    {
                        const t_cblc_lifecycle_constructor_request *request;

                        request = &builder->lifecycle_constructor_requests[
                            request_index];
                        if (request->constructor_index == constructor_index
                            && std::strcmp(request->receiver.cobol_name,
                                based_receiver.cobol_name) == 0
                            && std::strcmp(request->receiver.struct_type_name,
                                based_receiver.struct_type_name) == 0)
                        {
                            already_requested = 1;
                            break ;
                        }
                        request_index += 1;
                    }
                    if (!already_requested)
                    {
                        t_cblc_lifecycle_constructor_request request;

                        request.receiver = based_receiver;
                        request.constructor_index = constructor_index;
                        builder->lifecycle_constructor_requests.push_back(request);
                    }
                }
                if (std::snprintf(line, sizeof(line),
                        "           PERFORM %s THRU %s.", paragraph_name,
                        return_paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "cannot emit the selected indexed-element constructor call");
                    return (FT_FAILURE);
                }
                if (caller && statement
                    && cblc_constructor_may_throw(unit, type, constructor, 0)
                    && cblc_emit_exception_transfer(unit, caller, statement,
                        statement_index, builder) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "cannot emit exception transfer for indexed-element construction");
                    return (FT_FAILURE);
                }
                return (FT_SUCCESS);
            }
            if (!constructor || constructor->statement_count == 0)
            {
                if (cblc_emit_lifecycle_recursive(unit, type, based_name,
                        builder, 0) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "recursive initialization failed for indexed class element");
                    return (FT_FAILURE);
                }
                return (FT_SUCCESS);
            }
            index = 0;
            while (index < constructor->statement_count)
            {
                if (cblc_emit_pointer_lifecycle_body_statement(unit, type,
                        based_name, &constructor->statements[index], builder,
                        &control_flow_depth, constructor->parameters,
                        constructor->parameter_count) != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
            }
        }
        return (control_flow_depth == 0 ? FT_SUCCESS : FT_FAILURE);
    }
}

static int cblc_emit_lifecycle_statement_selected(
    const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_data_item *receiver_context,
    const t_cblc_constructor *selected_constructor)
{
    const t_cblc_constructor *constructor;
    const t_cblc_data_item *item;
    const t_cblc_struct_type *type;
    const t_cblc_statement *body;
    char line[256];
    size_t count;
    size_t index;
    int control_flow_depth;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!item && statement->type == CBLC_STATEMENT_DESTRUCT)
        item = cblc_find_method_parameter_storage_by_cobol(unit,
            statement->target);
    if (!item)
    {
        if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->type == CBLC_STATEMENT_DESTRUCT)
        {
            int pointer_lifecycle_status;

            pointer_lifecycle_status = cblc_emit_pointer_element_lifecycle(unit,
                statement->target, statement->type == CBLC_STATEMENT_DESTRUCT,
                builder, caller, statement, statement_index, receiver_context,
                selected_constructor);
            return (pointer_lifecycle_status);
        }
        return (FT_FAILURE);
    }
    /* A pointer's own lifetime does not construct or destroy its pointee. */
    if (cblc_data_kind_is_pointer(item->kind))
        return (FT_SUCCESS);
    if (item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
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
        if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
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
    if (selected_constructor)
    {
        int constructor_belongs_to_type = 0;

        for (size_t constructor_index = 0;
            constructor_index < type->constructor_count; ++constructor_index)
            if (&type->constructors[constructor_index] == selected_constructor)
            {
                constructor_belongs_to_type = 1;
                break ;
            }
        if (!constructor_belongs_to_type
            || (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
                && !statement->is_move_construct
                && !statement->is_relocate_construct))
            return (FT_FAILURE);
    }
    if (item->array_count > 0)
    {
        size_t element_index;

        if (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && statement->type != CBLC_STATEMENT_DESTRUCT)
            return (FT_FAILURE);
        if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && statement->call_argument_count > 0)
        {
            const t_cblc_constructor *array_constructor;

            array_constructor = selected_constructor
                ? selected_constructor
                : cblc_find_constructor_for_arguments(unit, type,
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
                        array_constructor->parameter_count, builder, NULL, 0,
                        array_constructor->is_copy_constructor) != FT_SUCCESS)
                    return (FT_FAILURE);
                constructor_statement_index = 0;
                control_flow_depth = 0;
                while (constructor_statement_index < array_constructor->statement_count)
                {
                    if (cblc_emit_array_element_constructor_statement(unit,
                            &array_constructor->statements[constructor_statement_index],
                            item->cobol_name, element_index, builder,
                            &control_flow_depth, caller) != FT_SUCCESS)
                        return (FT_FAILURE);
                    constructor_statement_index += 1;
                }
                if (control_flow_depth != 0)
                    return (FT_FAILURE);
                element_index += 1;
            }
            return (FT_SUCCESS);
        }
        if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT)
        {
            const t_cblc_constructor *array_constructor;

            if (cblc_emit_struct_array_lifecycle(unit, type, item->cobol_name,
                    item->array_count, 0, builder, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            array_constructor = selected_constructor
                ? selected_constructor
                : cblc_find_constructor_for_arguments(unit, type, "", 0);
            if (!array_constructor)
                return (type->has_default_constructor ? FT_FAILURE : FT_SUCCESS);
            element_index = 1;
            while (element_index <= item->array_count)
            {
                size_t constructor_statement_index;

                if (array_constructor->parameter_count > 0
                    && cblc_emit_parameter_argument_moves(unit, "",
                        0, array_constructor->parameters,
                        array_constructor->parameter_count, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                constructor_statement_index = 0;
                control_flow_depth = 0;
                while (constructor_statement_index < array_constructor->statement_count)
                {
                    if (cblc_emit_array_element_constructor_statement(unit,
                            &array_constructor->statements[constructor_statement_index],
                            item->cobol_name, element_index, builder,
                            &control_flow_depth, caller) != FT_SUCCESS)
                        return (FT_FAILURE);
                    constructor_statement_index += 1;
                }
                if (control_flow_depth != 0)
                    return (FT_FAILURE);
                element_index += 1;
            }
            return (FT_SUCCESS);
        }
        element_index = item->array_count;
        while (element_index > 0)
        {
            size_t destructor_statement_index;

            control_flow_depth = 0;
            destructor_statement_index = 0;
            while (destructor_statement_index < type->destructor_statement_count)
            {
                if (cblc_emit_array_element_constructor_statement(unit,
                        &type->destructor_statements[destructor_statement_index],
                        item->cobol_name, element_index, builder,
                        &control_flow_depth, caller) != FT_SUCCESS)
                    return (FT_FAILURE);
                destructor_statement_index += 1;
            }
            if (control_flow_depth != 0)
                return (FT_FAILURE);
            element_index -= 1;
        }
        return (cblc_emit_struct_array_lifecycle(unit, type, item->cobol_name,
            item->array_count, 0, builder, 1));
    }
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && ((statement->call_argument_count == 0 && type->has_default_constructor)
            || statement->call_argument_count > 0))
    {
        constructor = selected_constructor
            ? selected_constructor
            : cblc_find_constructor_for_arguments(unit, type,
                statement->call_arguments, statement->call_argument_count);
        if (!constructor)
        {
            if (statement->call_argument_count == 0)
                return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name,
                    builder, 0, caller));
            cobol_text_builder_set_diagnostic(builder,
                TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                "no constructor overload matches the synthesized lifecycle arguments");
            return (FT_FAILURE);
        }
        if (constructor->is_copy_constructor)
        {
            char copy_argument[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_data_item *copy_source_item;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            size_t constructor_index;

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
                    constructor->parameter_count, builder, NULL, 0, 1) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            constructor_index = static_cast<size_t>(constructor - type->constructors);
            if (cblc_constructor_paragraph_name(item, constructor_index,
                    paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
                || cblc_constructor_return_paragraph_name(unit, item,
                    constructor_index, return_paragraph_name,
                    sizeof(return_paragraph_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "           PERFORM %s THRU %s.",
                    paragraph_name, return_paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (caller && cblc_constructor_may_throw(unit, type, constructor, 0))
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
        if (constructor->statement_count == 0)
        {
            const int recursive_status = cblc_emit_lifecycle_recursive(unit,
                type, item->cobol_name, builder, 0, caller);

            if (recursive_status != FT_SUCCESS)
                cobol_text_builder_set_diagnostic(builder,
                    TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                    "empty constructor body could not initialize the target's class members");
            return (recursive_status);
        }
        if (item->kind == CBLC_DATA_KIND_STRUCT)
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_constructor_paragraph_name(item,
                    static_cast<size_t>(constructor - type->constructors), paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS
                || cblc_constructor_return_paragraph_name(unit, item,
                    static_cast<size_t>(constructor - type->constructors),
                    return_paragraph_name, sizeof(return_paragraph_name)) != FT_SUCCESS)
            {
                cobol_text_builder_set_diagnostic(builder,
                    TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                    "constructor paragraph naming failed for a class instance");
                return (FT_FAILURE);
            }
            if (constructor->parameter_count > 0
                && cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                    statement->call_argument_count, constructor->parameters,
                    constructor->parameter_count, builder) != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "failed to marshal constructor arguments '%s' for '%s' (count=%zu, parameters=%zu)",
                    statement->call_arguments, item->struct_type_name,
                    statement->call_argument_count, constructor->parameter_count);
                cobol_text_builder_set_diagnostic(builder,
                    TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                return (FT_FAILURE);
            }
            if (std::snprintf(line, sizeof(line), "           PERFORM %s THRU %s.",
                    paragraph_name, return_paragraph_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (caller && cblc_constructor_may_throw(unit, type, constructor, 0))
            {
                t_cblc_statement transfer_statement;

                transfer_statement = *statement;
                transfer_statement.exception_may_throw = 1;
                if (cblc_emit_exception_transfer(unit, caller, &transfer_statement,
                        statement_index, builder) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                        "exception transfer for a constructed class instance failed");
                    return (FT_FAILURE);
                }
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
        control_flow_depth = 0;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement_with_flow(unit, NULL, &body[index],
                    item->cobol_name, builder, &control_flow_depth) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            index += 1;
        }
        if (control_flow_depth != 0)
            return (FT_FAILURE);
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
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name,
                builder, 1, caller));
        }
        body = type->destructor_statements;
        count = type->destructor_statement_count;
        control_flow_depth = 0;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement_with_flow(unit, NULL, &body[index],
                    item->cobol_name, builder, &control_flow_depth) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        if (control_flow_depth != 0)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    {
        int lifecycle_status;

        lifecycle_status = cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder,
            statement->type == CBLC_STATEMENT_DESTRUCT, caller);
        return (lifecycle_status);
    }
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder,
    const t_cblc_function *caller, size_t statement_index,
    const t_cblc_data_item *receiver_context)
{
    return (cblc_emit_lifecycle_statement_selected(unit, statement, builder,
        caller, statement_index, receiver_context, NULL));
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
        if (function->statements[index].exception_may_throw)
            return (1);
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

static int cblc_exception_cleanup_lists_equal(const t_cblc_statement *left,
    const t_cblc_statement *right)
{
    size_t index;

    if (!left || !right || left->exception_cleanup_count == 0
        || left->exception_cleanup_count != right->exception_cleanup_count)
        return (0);
    index = 0;
    while (index < left->exception_cleanup_count)
    {
        if (std::strcmp(left->exception_cleanup_targets[index],
                right->exception_cleanup_targets[index]) != 0)
            return (0);
        index += 1;
    }
    return (1);
}

static const char *cblc_cleanup_owner_cobol_name(
    const t_cblc_translation_unit *unit, const t_cblc_function *function);

static int cblc_build_exception_cleanup_paragraph_name(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    size_t statement_index, char *buffer, size_t buffer_size)
{
    size_t canonical_index;
    size_t index;

    if (!function || statement_index >= function->statement_count
        || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    canonical_index = statement_index;
    index = 0;
    while (index < statement_index)
    {
        if (cblc_exception_cleanup_lists_equal(&function->statements[index],
                &function->statements[statement_index]))
        {
            canonical_index = index;
            break ;
        }
        index += 1;
    }
    const char *owner_cobol_name = cblc_cleanup_owner_cobol_name(unit, function);
    int written;

    if (function->is_exception_helper)
        written = std::snprintf(buffer, buffer_size,
            "CBLC-EX-CLEANUP-%s-EXH-%zu-%zu", owner_cobol_name,
            function->helper_exception_region_id, canonical_index);
    else
        written = std::snprintf(buffer, buffer_size,
            "CBLC-EX-CLEANUP-%s-%zu", owner_cobol_name, canonical_index);
    if (written < 0
        || std::strlen(buffer) >= buffer_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_transfer(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index, t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    char cleanup_name[TRANSPILE_IDENTIFIER_MAX];
    char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !function || !statement || !builder)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder,
            "           IF CBLC-EX-ACTIVE = 'Y'") != FT_SUCCESS)
        return (FT_FAILURE);
    if (statement->exception_cleanup_count > 0)
    {
        if (cblc_build_exception_cleanup_paragraph_name(unit, function,
                statement_index, cleanup_name, sizeof(cleanup_name)) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "               PERFORM %s",
                cleanup_name) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!statement->exception_has_region)
    {
        if (std::strncmp(function->source_name, "main", sizeof(function->source_name)) == 0)
        {
            if (cobol_text_builder_append_line(builder,
                    "               GO TO CBLC-ROOT-UNCAUGHT") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_function_return_paragraph_name(unit, function,
                    return_paragraph_name, sizeof(return_paragraph_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "               GO TO %s",
                    return_paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
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
    size_t message_exception_type_id;

    if (!unit || !function || !statement || !status_name || !builder)
        return (FT_FAILURE);
    transfer_statement = *statement;
    message_exception_type_id = cblc_exception_type_id("exception_message");
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
            message_exception_type_id) < 0
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
        || std::snprintf(line, sizeof(line), "               MOVE %s TO CBLC-EX-MESSAGE-BUF",
            status_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 2 TO CBLC-EX-MESSAGE-LEN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 2 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %zu TO CBLC-EX-TYPE-ID",
            message_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "               MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
            message_exception_type_id) < 0
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
    const char *exception_type_name, t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    char payload_code_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement transfer_statement;
    size_t region_id;
    size_t allocation_exception_type_id;
    const t_cblc_struct_type *exception_type;
    const t_cblc_struct_field *code_field;
    const t_cblc_data_item *payload_item;
    int has_struct_payload;

    if (!unit || !function || !statement || !allocation_line || !pointer_name
        || !exception_type_name || !builder)
        return (FT_FAILURE);
    transfer_statement = *statement;
    if (cblc_find_enclosing_exception_region(function, statement_index,
            &region_id) == FT_SUCCESS)
    {
        transfer_statement.exception_has_region = 1;
        transfer_statement.exception_region_id = region_id;
    }
    else
        transfer_statement.exception_has_region = 0;
    allocation_exception_type_id = cblc_exception_type_id(exception_type_name);
    exception_type = NULL;
    code_field = NULL;
    payload_item = NULL;
    payload_code_name[0] = '\0';
    has_struct_payload = std::strcmp(exception_type_name, "bad_alloc") == 0;
    if (has_struct_payload)
    {
        size_t field_index;

        exception_type = cblc_find_struct_type(unit, exception_type_name);
        payload_item = cblc_find_data_item_for_emit_by_source(unit,
            "__cblc_exception_payload_bad_alloc");
        if (!exception_type)
            return (FT_FAILURE);
        field_index = 0;
        while (field_index < exception_type->field_count)
        {
            if (std::strcmp(exception_type->fields[field_index].source_name,
                    "code") == 0
                && exception_type->fields[field_index].kind == CBLC_DATA_KIND_INT)
            {
                code_field = &exception_type->fields[field_index];
                break ;
            }
            field_index += 1;
        }
        if (!code_field || (payload_item
                && std::snprintf(payload_code_name, sizeof(payload_code_name),
                    "%s-%s", payload_item->cobol_name,
                    code_field->cobol_name) < 0))
            return (FT_FAILURE);
    }
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
        || (has_struct_payload && payload_item
            && (std::snprintf(line, sizeof(line), "               MOVE 1 TO %s",
                    payload_code_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS))
        || (has_struct_payload
            ? cobol_text_builder_append_line(builder, payload_item
                ? "               MOVE 4 TO CBLC-EX-PAYLOAD-SIZE"
                : "               MOVE 0 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
            : (cobol_text_builder_append_line(builder,
                "               MOVE 1 TO CBLC-EX-INT-PAYLOAD") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    "               MOVE 4 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS))
        || std::snprintf(line, sizeof(line), "               MOVE %zu TO CBLC-EX-TYPE-ID",
            allocation_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "               MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
            allocation_exception_type_id) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, has_struct_payload
            ? (payload_item ? "               MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER"
                : "               MOVE 'N' TO CBLC-EX-PAYLOAD-OWNER")
            : "               MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "               MOVE 'Y' TO CBLC-EX-ACTIVE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS
        || cblc_emit_exception_transfer(unit, function, &transfer_statement, statement_index,
            builder) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

/* Keep unsupported separate from libft's success/failure values. */
static const int CBLC_TYPED_EMIT_UNSUPPORTED = 2;

static const cblc::backend::cleanup_edge *cblc_find_typed_throw_cleanup_edge(
    const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::lifecycle_cleanup_plan *cleanup_plan,
    const cblc::backend::typed_operation **out_operation)
{
    const cblc::backend::typed_operation *throw_operation;
    const cblc::backend::cleanup_edge *selected_edge;

    if (out_operation)
        *out_operation = NULL;
    if (!statement || !typed_function || !cleanup_plan
        || statement->source_offset == static_cast<size_t>(-1)
        || typed_function->function_id != cleanup_plan->function_id
        || typed_function->source_unit_id != cleanup_plan->source_unit_id)
        return (NULL);
    throw_operation = NULL;
    for (const cblc::backend::typed_operation &operation :
        typed_function->operations)
        if (operation.operation == cblc::backend::opcode::throw_value
            && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            if (throw_operation)
                return (NULL);
            throw_operation = &operation;
        }
    if (!throw_operation)
        return (NULL);
    selected_edge = NULL;
    for (const cblc::backend::cleanup_edge &edge : cleanup_plan->failure_edges)
        if (edge.operation == throw_operation->id
            && edge.outcome == cblc::backend::outcome_kind::thrown_exception)
        {
            if (selected_edge
                && selected_edge->destruction_order != edge.destruction_order)
                return (NULL);
            selected_edge = &edge;
        }
    if (selected_edge && out_operation)
        *out_operation = throw_operation;
    return (selected_edge);
}

static int cblc_typed_throw_cleanup_is_emittable(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::cleanup_edge *edge)
{
    if (!unit || !function || !typed_function || !edge)
        return (0);
    for (cblc::backend::value_id value : edge->destruction_order)
    {
        const cblc::backend::typed_place *place = NULL;
        const cblc::backend::typed_symbol *symbol = NULL;
        const t_cblc_data_item *item = NULL;

        for (const cblc::backend::typed_place &candidate : typed_function->places)
            if (candidate.base_value == value)
            {
                if (place)
                    return (0);
                place = &candidate;
            }
        if (!place || place->symbol == 0)
            return (0);
        for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
            if (candidate.id == place->symbol)
            {
                symbol = &candidate;
                break;
            }
        if (!symbol || symbol->canonical_name.empty())
            return (0);
        for (size_t item_index = 0; item_index < unit->data_count; ++item_index)
        {
            const t_cblc_data_item *candidate = &unit->data_items[item_index];
            const bool name_matches =
                std::strcmp(candidate->source_name,
                    symbol->canonical_name.c_str()) == 0
                || (candidate->local_source_name[0] != '\0'
                    && std::strcmp(candidate->local_source_name,
                        symbol->canonical_name.c_str()) == 0);
            if (!name_matches || candidate->is_alias || candidate->is_shadowed
                || !candidate->is_function_local
                || std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0)
                continue;
            if (item)
                return (0);
            item = candidate;
        }
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT
            || item->is_template_item || item->array_count != 0)
            return (0);
        if (!place->projections.empty())
        {
            /* A root object's destructor recursively handles its member
             * subobjects. A projected value without that root in the edge is
             * a partial-construction case and needs element/member lowering. */
            bool has_root = false;
            for (cblc::backend::value_id candidate_value : edge->destruction_order)
                for (const cblc::backend::typed_place &candidate_place :
                    typed_function->places)
                    if (candidate_place.base_value == candidate_value
                        && candidate_place.symbol == place->symbol
                        && candidate_place.projections.empty())
                        has_root = true;
            if (!has_root)
                return (0);
        }
    }
    return (1);
}

static int cblc_build_typed_throw_cleanup_name(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::lifecycle_cleanup_plan *cleanup_plan,
    const cblc::backend::typed_operation *operation,
    const cblc::backend::cleanup_edge *edge,
    char *buffer, size_t buffer_size)
{
    const cblc::backend::typed_operation *canonical_operation;

    if (!function || !typed_function || !cleanup_plan || !operation || !edge
        || !buffer || buffer_size == 0
        || operation->operation != cblc::backend::opcode::throw_value)
        return (FT_FAILURE);
    canonical_operation = operation;
    for (const cblc::backend::typed_operation &candidate :
        typed_function->operations)
    {
        bool matching_cleanup = false;

        if (candidate.id >= operation->id)
            break;
        if (candidate.operation != cblc::backend::opcode::throw_value)
            continue;
        for (const cblc::backend::cleanup_edge &candidate_edge :
            cleanup_plan->failure_edges)
            if (candidate_edge.operation == candidate.id
                && candidate_edge.outcome
                    == cblc::backend::outcome_kind::thrown_exception
                && candidate_edge.destruction_order
                    == edge->destruction_order)
            {
                matching_cleanup = true;
                break;
            }
        if (matching_cleanup)
        {
            canonical_operation = &candidate;
            break;
        }
    }
    const char *owner_cobol_name = cblc_cleanup_owner_cobol_name(unit, function);
    const int written = std::snprintf(buffer, buffer_size,
        "CBLC-EX-CLEANUP-%s-TIR-%u", owner_cobol_name,
        canonical_operation->id);
    return (written >= 0 && static_cast<size_t>(written) < buffer_size
        ? FT_SUCCESS : FT_FAILURE);
}

static int cblc_emit_typed_throw_cleanup_paragraph(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::lifecycle_cleanup_plan *cleanup_plan,
    const cblc::backend::cleanup_edge *edge,
    const cblc::backend::typed_operation *operation,
    t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    std::unordered_set<cblc::backend::symbol_id> emitted_roots;

    if (!unit || !function || !statement || !typed_function || !edge
        || !operation || !builder
        || !cblc_typed_throw_cleanup_is_emittable(unit, function,
            typed_function, edge)
        || cblc_build_typed_throw_cleanup_name(unit, function, typed_function,
            cleanup_plan, operation, edge,
            paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'Y' TO CBLC-EX-CLEANING.") != FT_SUCCESS)
        return (FT_FAILURE);

    for (cblc::backend::value_id value : edge->destruction_order)
    {
        const cblc::backend::typed_place *place = NULL;
        const cblc::backend::typed_symbol *symbol = NULL;
        const t_cblc_data_item *item = NULL;

        for (const cblc::backend::typed_place &candidate : typed_function->places)
            if (candidate.base_value == value)
                place = &candidate;
        if (!place || !place->projections.empty())
            continue;
        for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
            if (candidate.id == place->symbol)
            {
                symbol = &candidate;
                break;
            }
        if (!symbol)
            return (FT_FAILURE);
        for (size_t item_index = 0; item_index < unit->data_count; ++item_index)
        {
            const t_cblc_data_item *candidate = &unit->data_items[item_index];
            if (candidate->is_alias || candidate->is_shadowed
                || !candidate->is_function_local
                || std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0)
                continue;
            if (std::strcmp(candidate->source_name,
                    symbol->canonical_name.c_str()) == 0
                || (candidate->local_source_name[0] != '\0'
                    && std::strcmp(candidate->local_source_name,
                        symbol->canonical_name.c_str()) == 0))
            {
                if (item)
                    return (FT_FAILURE);
                item = candidate;
            }
        }
        if (!item || !emitted_roots.insert(place->symbol).second)
            return (FT_FAILURE);
        t_cblc_statement cleanup_statement{};
        cleanup_statement.type = CBLC_STATEMENT_DESTRUCT;
        ft_strlcpy(cleanup_statement.target, item->cobol_name,
            sizeof(cleanup_statement.target));
        if (cblc_emit_lifecycle_statement(unit, &cleanup_statement, builder,
                NULL, static_cast<size_t>(-1), NULL) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-CLEANING.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_typed_throw_cleanup_paragraphs(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::lifecycle_cleanup_plan *cleanup_plan,
    t_cobol_text_builder *builder)
{
    std::unordered_set<std::string> emitted_paragraphs;

    if (!unit || !function || !typed_function || !cleanup_plan || !builder)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (size_t statement_index = 0;
        statement_index < function->statement_count; ++statement_index)
    {
        const t_cblc_statement *statement = &function->statements[statement_index];
        if (statement->type != CBLC_STATEMENT_THROW)
            continue;
        const cblc::backend::typed_operation *operation = NULL;
        const cblc::backend::cleanup_edge *edge =
            cblc_find_typed_throw_cleanup_edge(statement, typed_function,
                cleanup_plan, &operation);
        if (!edge || edge->destruction_order.empty()
            || !cblc_typed_throw_cleanup_is_emittable(unit, function,
                typed_function, edge))
            continue;
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
        if (cblc_build_typed_throw_cleanup_name(unit, function, typed_function,
                cleanup_plan, operation, edge, paragraph_name,
                sizeof(paragraph_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!emitted_paragraphs.insert(paragraph_name).second)
            continue;
        if (cblc_emit_typed_throw_cleanup_paragraph(unit, function, statement,
                typed_function, cleanup_plan, edge, operation, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_terminate_cobol_sentence_before_paragraph(
    t_cobol_text_builder *builder)
{
    size_t cursor;

    if (!builder)
        return (FT_FAILURE);
    cursor = builder->length;
    while (cursor > 0
        && (builder->data[cursor - 1] == '\n'
            || builder->data[cursor - 1] == '\r'
            || builder->data[cursor - 1] == ' '
            || builder->data[cursor - 1] == '\t'))
        cursor -= 1;
    if (cursor > 0 && builder->data[cursor - 1] != '.')
        return (cobol_text_builder_append_line(builder,
            "           CONTINUE."));
    return (FT_SUCCESS);
}

static int cblc_emit_exception_cleanup_paragraphs(const t_cblc_translation_unit *unit,
    const t_cblc_function *function,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::lifecycle_cleanup_plan *cleanup_plan,
    t_cobol_text_builder *builder)
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
        if (statement->type == CBLC_STATEMENT_THROW
            && typed_function && cleanup_plan)
        {
            const cblc::backend::typed_operation *operation = NULL;
            const cblc::backend::cleanup_edge *edge =
                cblc_find_typed_throw_cleanup_edge(statement, typed_function,
                    cleanup_plan, &operation);
            if (edge && (edge->destruction_order.empty()
                    || cblc_typed_throw_cleanup_is_emittable(unit, function,
                        typed_function, edge)))
            {
                index += 1;
                continue;
            }
        }
        if (statement->exception_cleanup_count == 0)
        {
            index += 1;
            continue ;
        }
        {
            size_t previous_index = 0;

            while (previous_index < index)
            {
                if (cblc_exception_cleanup_lists_equal(
                        &function->statements[previous_index], statement))
                    break ;
                previous_index += 1;
            }
            if (previous_index < index)
            {
                index += 1;
                continue ;
            }
        }
        if (cblc_terminate_cobol_sentence_before_paragraph(builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_build_exception_cleanup_paragraph_name(unit, function, index,
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
                    NULL, static_cast<size_t>(-1), NULL)
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
    if (type->has_destructor || type->destructor_statement_count > 0)
        return (cblc_emit_exception_copy_by_assignment(unit, type, source_prefix,
            target_prefix, builder));
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
        else if (field->kind == CBLC_DATA_KIND_CHAR_BUFFER)
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
            "           MOVE 0 TO CBLC-EX-MESSAGE-LEN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE SPACES TO CBLC-EX-MESSAGE-BUF") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-RAISING") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-CLEANING") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_exception_context_dispose(t_cobol_text_builder *builder)
{
    if (!builder
        || cobol_text_builder_append_line(builder,
            "           MOVE 'Y' TO CBLC-EX-CLEANING") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           PERFORM CBLC-EX-CLEAR-PAYLOAD") != FT_SUCCESS
        || cblc_emit_exception_context_clear(builder) != FT_SUCCESS)
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
    if (!unit || !builder
        || cblc_terminate_cobol_sentence_before_paragraph(builder)
            != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "       CBLC-EX-CLEAR-PAYLOAD.") != FT_SUCCESS
        || cblc_emit_exception_payload_cleanup(unit, builder) != FT_SUCCESS
        || cblc_emit_exception_catch_cleanup(unit, builder) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "       CBLC-EX-CLEAR-PAYLOAD-END.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_function_return_paragraph_name(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!unit || !function || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (function->paragraph_return_label[0] != '\0')
    {
        if (ft_strlcpy(buffer, function->paragraph_return_label, buffer_size) >= buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    index = 0;
    while (index < unit->function_count)
    {
        if (std::strcmp(unit->functions[index].source_name, function->source_name) == 0
            && std::strcmp(unit->functions[index].cobol_name, function->cobol_name) == 0)
        {
            if (std::snprintf(buffer, buffer_size, "CBLC-FRETURN-%zu", index) < 0
                || std::strlen(buffer) >= buffer_size)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_validate_typed_direct_call(
    const t_cblc_translation_unit *unit, const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function)
{
    const cblc::backend::typed_operation *call = NULL;
    const cblc::backend::typed_callable *dispatch = NULL;
    const t_cblc_function *target;

    if (!unit || !statement || !typed_function
        || (statement->type != CBLC_STATEMENT_CALL
            && statement->type != CBLC_STATEMENT_CALL_ASSIGN)
        || statement->source_offset == static_cast<size_t>(-1))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (const cblc::backend::typed_operation &operation :
        typed_function->operations)
        if (operation.operation == cblc::backend::opcode::call
            && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            if (call)
                return (FT_FAILURE);
            call = &operation;
        }
    if (!call)
        return (FT_FAILURE);
    for (const cblc::backend::typed_callable &candidate :
        typed_function->callables)
        if (candidate.id == call->callee)
        {
            dispatch = &candidate;
            break ;
        }
    target = cblc_find_function_in_unit(unit, statement->call_identifier);
    if (!dispatch || dispatch->kind != cblc::backend::callable_kind::function
        || dispatch->canonical_name != statement->call_identifier
        || !dispatch->has_definition || !target || target->is_template
        || target->statement_count == 0 || !target->statements
        || call->operands.size() != target->parameter_count
        || (statement->type == CBLC_STATEMENT_CALL
            ? (!call->results.empty() || !call->places.empty()
                || target->return_kind != CBLC_FUNCTION_RETURN_VOID)
            : (call->results.size() != 1 || call->places.size() != 1
                || target->return_kind == CBLC_FUNCTION_RETURN_VOID)))
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_typed_method_call(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement, size_t statement_index,
    const cblc::backend::typed_function *typed_function,
    t_cobol_text_builder *builder)
{
    const cblc::backend::typed_operation *call;
    const cblc::backend::typed_callable *dispatch;
    const cblc::backend::typed_place *receiver_place;
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *selected_method;
    const char *receiver_name;
    size_t selected_method_count;

    if (!unit || !function || !statement || !typed_function || !builder
        || (statement->type != CBLC_STATEMENT_METHOD_CALL
            && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        || statement->source_offset == static_cast<size_t>(-1))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
        ? statement->target : statement->source;
    if (std::strcmp(receiver_name, "CBLC-THIS") == 0)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    call = NULL;
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
        if (operation.operation == cblc::backend::opcode::call
            && operation.callee != 0 && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            if (call)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "multiple typed call operations match method '%s' at source offset %zu",
                    statement->call_identifier, statement->source_offset);
                cobol_text_builder_set_diagnostic(builder,
                    TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                return (FT_FAILURE);
            }
            call = &operation;
        }
    if (!call)
    {
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
            "typed IR has no concrete call operation for the resolved class-method source location");
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    }
    dispatch = NULL;
    for (const cblc::backend::typed_callable &candidate : typed_function->callables)
        if (candidate.id == call->callee)
        {
            dispatch = &candidate;
            break ;
        }
    receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver)
        receiver = cblc_find_data_item_for_emit_by_source(unit, receiver_name);
    receiver_type = receiver ? cblc_find_receiver_type(unit, receiver) : NULL;
    if (!receiver_type || (dispatch
            && dispatch->canonical_name.compare(0,
                dispatch->canonical_name.find("::"),
                receiver_type->source_name) != 0))
    {
        for (size_t item_index = 0; item_index < unit->data_count;
            ++item_index)
        {
            const t_cblc_data_item *candidate = &unit->data_items[item_index];
            const t_cblc_struct_type *candidate_type;

            if (std::strcmp(candidate->cobol_name, receiver_name) != 0)
                continue;
            candidate_type = cblc_find_receiver_type(unit, candidate);
            if (candidate_type && dispatch
                && dispatch->canonical_name.compare(0,
                    dispatch->canonical_name.find("::"),
                    candidate_type->source_name) == 0)
            {
                receiver = candidate;
                receiver_type = candidate_type;
                break ;
            }
        }
    }
    receiver_place = NULL;
    if (call->places.empty())
    {
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
            "typed method call has no receiver place");
        return (FT_FAILURE);
    }
    for (const cblc::backend::typed_place &place : typed_function->places)
        if (place.id == call->places.front())
        {
            receiver_place = &place;
            break ;
        }
    if (!dispatch || !receiver || !receiver_type || !receiver_place
        || dispatch->kind != cblc::backend::callable_kind::method
        || dispatch->owner_type != receiver_place->type
        || dispatch->canonical_name
            != std::string(receiver_type->source_name) + "::"
                + statement->call_identifier)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "typed method dispatch mismatch for '%s' on '%s' (dispatch=%s owner=%llu place-type=%llu)",
            statement->call_identifier,
            receiver_type ? receiver_type->source_name : "<unresolved>",
            dispatch ? dispatch->canonical_name.c_str() : "<unresolved>",
            static_cast<unsigned long long>(dispatch ? dispatch->owner_type : 0),
            static_cast<unsigned long long>(receiver_place
                ? receiver_place->type : 0));
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
        return (FT_FAILURE);
    }
    selected_method = NULL;
    selected_method_count = 0;
    for (size_t method_index = 0; method_index < receiver_type->method_count;
        ++method_index)
    {
        const t_cblc_method *candidate = &receiver_type->methods[method_index];
        if (candidate->overload_index != dispatch->overload_index
            || std::strcmp(candidate->source_name,
                statement->call_identifier) != 0)
            continue ;
        selected_method = candidate;
        selected_method_count += 1;
    }
    if (selected_method_count != 1 || !selected_method->has_definition)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "typed method dispatch selected %zu definitions for '%s::%s'",
            selected_method_count, receiver_type->source_name,
            statement->call_identifier);
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
        return (FT_FAILURE);
    }
    {
        const int status = cblc_emit_method_statement_with_method(unit, statement,
            builder, function, statement_index, selected_method, NULL);
        if (status != FT_SUCCESS && builder->diagnostic_message[0] == '\0')
        {
            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(message, sizeof(message),
                "typed dispatch selected '%s::%s', but receiver '%s' (kind=%d alias=%d borrowed=%d source='%.48s') could not be emitted",
                receiver_type->source_name, selected_method->source_name,
                receiver_name, static_cast<int>(receiver->kind),
                receiver->is_alias, receiver->is_borrowed,
                receiver->source_name);
            cobol_text_builder_set_diagnostic(builder,
                TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, message);
        }
        return status;
    }
}

static int cblc_emit_typed_copy_assignment(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function,
    t_cobol_text_builder *builder, size_t statement_index)
{
    const cblc::backend::typed_operation *assignment;
    const cblc::backend::typed_place *target_place;
    const cblc::backend::typed_place *source_place;
    const cblc::backend::typed_symbol *target_symbol;
    const cblc::backend::typed_symbol *source_symbol;
    const cblc::backend::typed_callable *dispatch;
    const t_cblc_data_item *target_item;
    const t_cblc_data_item *source_item;
    const t_cblc_struct_type *type;
    const t_cblc_method *selected_method;
    size_t selected_method_count;
    t_cblc_statement typed_statement;

    if (!unit || !function || !statement || !typed_function || !builder
        || statement->type != CBLC_STATEMENT_ASSIGNMENT
        || statement->source_offset == static_cast<size_t>(-1))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    assignment = NULL;
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
        if (operation.operation == cblc::backend::opcode::copy_assign
            && operation.places.size() == 1 && operation.operands.size() == 1
            && operation.callee != 0 && !operation.effects.may_fail
            && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            assignment = &operation;
            break ;
        }
    if (!assignment)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    target_place = NULL;
    source_place = NULL;
    for (const cblc::backend::typed_place &place : typed_function->places)
    {
        if (place.id == assignment->places.front())
            target_place = &place;
        if (place.base_value == assignment->operands.front())
            source_place = &place;
    }
    if (!target_place || !source_place || target_place->symbol == 0
        || source_place->symbol == 0 || !target_place->projections.empty()
        || !source_place->projections.empty()
        || target_place->type != source_place->type)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    target_symbol = NULL;
    source_symbol = NULL;
    for (const cblc::backend::typed_symbol &symbol : typed_function->symbols)
    {
        if (symbol.id == target_place->symbol)
            target_symbol = &symbol;
        if (symbol.id == source_place->symbol)
            source_symbol = &symbol;
    }
    if (!target_symbol || !source_symbol
        || target_symbol->canonical_name.empty()
        || source_symbol->canonical_name.empty())
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    target_item = NULL;
    source_item = NULL;
    for (size_t index = 0; index < unit->data_count; ++index)
    {
        const t_cblc_data_item *candidate = &unit->data_items[index];
        const bool matches_target = std::strcmp(candidate->source_name,
                target_symbol->canonical_name.c_str()) == 0
            || (candidate->local_source_name[0] != '\0'
                && std::strcmp(candidate->local_source_name,
                    target_symbol->canonical_name.c_str()) == 0);
        const bool matches_source = std::strcmp(candidate->source_name,
                source_symbol->canonical_name.c_str()) == 0
            || (candidate->local_source_name[0] != '\0'
                && std::strcmp(candidate->local_source_name,
                    source_symbol->canonical_name.c_str()) == 0);
        if (candidate->is_alias || candidate->is_shadowed
            || (candidate->is_function_local
                && std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0))
            continue ;
        if (matches_target)
        {
            if (target_item)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            target_item = candidate;
        }
        if (matches_source)
        {
            if (source_item)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            source_item = candidate;
        }
    }
    if (!target_item || !source_item
        || target_item->kind != CBLC_DATA_KIND_STRUCT
        || source_item->kind != CBLC_DATA_KIND_STRUCT
        || target_item->array_count != 0 || source_item->array_count != 0
        || std::strcmp(target_item->struct_type_name,
            source_item->struct_type_name) != 0)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    type = cblc_find_struct_type(unit, target_item->struct_type_name);
    dispatch = NULL;
    for (const cblc::backend::typed_callable &callable : typed_function->callables)
        if (callable.id == assignment->callee)
        {
            dispatch = &callable;
            break ;
        }
    if (!type || !dispatch
        || dispatch->kind != cblc::backend::callable_kind::copy_assignment
        || dispatch->owner_type != target_place->type
        || dispatch->parameter_types.size() != 1
        || dispatch->parameter_types.front() != source_place->type
        || dispatch->canonical_name
            != std::string(type->source_name) + "::operator=")
        return (FT_FAILURE);
    selected_method = NULL;
    selected_method_count = 0;
    for (size_t method_index = 0; method_index < type->method_count;
        ++method_index)
    {
        const t_cblc_method *candidate = &type->methods[method_index];
        if (std::strcmp(candidate->source_name, "operator=") != 0
            || candidate->overload_index != dispatch->overload_index
            || candidate->parameter_count != 1
            || candidate->parameters[0].kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT
            || std::strcmp(candidate->parameters[0].type_name,
                target_item->struct_type_name) != 0)
            continue ;
        selected_method = candidate;
        selected_method_count += 1;
    }
    if (selected_method_count != 1 || !selected_method->has_definition
        || selected_method->return_kind != CBLC_FUNCTION_RETURN_VOID)
        return (FT_FAILURE);
    typed_statement = *statement;
    ft_strlcpy(typed_statement.target, target_item->cobol_name,
        sizeof(typed_statement.target));
    ft_strlcpy(typed_statement.source, source_item->cobol_name,
        sizeof(typed_statement.source));
    return (cblc_emit_copy_assignment_with_method(unit, &typed_statement,
        builder, selected_method, function, statement_index));
}

static const t_cblc_data_item *cblc_find_typed_scalar_item_by_name(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const std::string &name)
{
    const t_cblc_data_item *found = NULL;

    if (!unit || !function || name.empty())
        return (NULL);
    for (size_t index = 0; index < unit->data_count; ++index)
    {
        const t_cblc_data_item *candidate = &unit->data_items[index];
        const bool matches_name = std::strcmp(candidate->source_name,
                name.c_str()) == 0
            || (candidate->local_source_name[0] != '\0'
                && std::strcmp(candidate->local_source_name,
                    name.c_str()) == 0);

        if (candidate->is_alias || candidate->is_shadowed
            || (candidate->is_function_local
                && std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0)
            || !matches_name)
            continue ;
        if (found)
            return (NULL);
        found = candidate;
    }
    return (found);
}

static int cblc_format_typed_scalar_value(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const cblc::backend::typed_function *typed_function,
    cblc::backend::value_id value_id, cblc::backend::type_id expected_type,
    std::string *expression, bool *contains_arithmetic, size_t depth)
{
    const cblc::backend::typed_value *value = NULL;
    const cblc::backend::typed_operation *producer = NULL;

    if (!unit || !function || !typed_function || !expression
        || !contains_arithmetic || depth > 64)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (const cblc::backend::typed_value &candidate : typed_function->values)
        if (candidate.id == value_id)
        {
            value = &candidate;
            break ;
        }
    if (!value || value->type != expected_type)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    if (!value->constant_value.empty())
    {
        if (!cblc_argument_is_numeric_literal(value->constant_value.c_str()))
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        *expression = value->constant_value;
        return (FT_SUCCESS);
    }
    for (const cblc::backend::typed_operation &candidate : typed_function->operations)
    {
        for (cblc::backend::value_id result_id : candidate.results)
            if (result_id == value_id)
            {
                producer = &candidate;
                break ;
            }
        if (producer)
            break ;
    }
    if (!producer)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    if (producer->operation == cblc::backend::opcode::load
        && producer->places.size() == 1)
    {
        const cblc::backend::typed_place *place = NULL;
        const cblc::backend::typed_symbol *symbol = NULL;
        const t_cblc_data_item *item;

        for (const cblc::backend::typed_place &candidate : typed_function->places)
            if (candidate.id == producer->places.front())
            {
                place = &candidate;
                break ;
            }
        if (!place || place->symbol == 0 || place->type != expected_type)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
            if (candidate.id == place->symbol)
            {
                symbol = &candidate;
                break ;
            }
        if (!symbol)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        item = cblc_find_typed_scalar_item_by_name(unit, function,
            symbol->canonical_name);
        if (!item || item->array_count != 0
            || (item->kind != CBLC_DATA_KIND_INT
                && item->kind != CBLC_DATA_KIND_DOUBLE)
            || static_cast<cblc::backend::type_id>(item->kind) + 1U
                != expected_type)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        *expression = item->cobol_name;
        return (FT_SUCCESS);
    }
    if ((producer->operation != cblc::backend::opcode::add
            && producer->operation != cblc::backend::opcode::subtract)
        || producer->operands.size() != 2)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    std::string left;
    std::string right;
    if (cblc_format_typed_scalar_value(unit, function, typed_function,
            producer->operands[0], expected_type, &left,
            contains_arithmetic, depth + 1) != FT_SUCCESS
        || cblc_format_typed_scalar_value(unit, function, typed_function,
            producer->operands[1], expected_type, &right,
            contains_arithmetic, depth + 1) != FT_SUCCESS)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    *contains_arithmetic = true;
    *expression = "(" + left
        + (producer->operation == cblc::backend::opcode::add ? " + " : " - ")
        + right + ")";
    return (FT_SUCCESS);
}

static int cblc_emit_typed_scalar_expression_assignment(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function,
    t_cobol_text_builder *builder)
{
    const cblc::backend::typed_operation *assignment = NULL;
    const cblc::backend::typed_place *destination = NULL;
    const cblc::backend::typed_symbol *symbol = NULL;
    const t_cblc_data_item *target = NULL;
    std::string expression;
    bool contains_arithmetic = false;
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !function || !statement || !typed_function || !builder
        || statement->type != CBLC_STATEMENT_ASSIGNMENT
        || statement->source_offset == static_cast<size_t>(-1))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
        if (operation.operation == cblc::backend::opcode::assign
            && operation.places.size() == 1 && operation.operands.size() == 1
            && !operation.effects.may_fail && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            assignment = &operation;
            break ;
        }
    if (!assignment)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (const cblc::backend::typed_place &candidate : typed_function->places)
        if (candidate.id == assignment->places.front())
        {
            destination = &candidate;
            break ;
        }
    if (!destination || destination->symbol == 0
        || !destination->projections.empty()
        || destination->type != static_cast<cblc::backend::type_id>(
            CBLC_DATA_KIND_INT) + 1U)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
        if (candidate.id == destination->symbol)
        {
            symbol = &candidate;
            break ;
        }
    if (!symbol)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    target = cblc_find_typed_scalar_item_by_name(unit, function,
        symbol->canonical_name);
    if (!target || target->kind != CBLC_DATA_KIND_INT
        || target->array_count != 0)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    if (cblc_format_typed_scalar_value(unit, function, typed_function,
            assignment->operands.front(), destination->type, &expression,
            &contains_arithmetic, 0) != FT_SUCCESS || !contains_arithmetic)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
            target->cobol_name, expression.c_str()) < 0
        || std::strlen(line) >= sizeof(line))
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line) == FT_SUCCESS
        ? FT_SUCCESS : FT_FAILURE);
}

static int cblc_emit_typed_scalar_literal_assignment(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function,
    t_cobol_text_builder *builder)
{
    const cblc::backend::typed_operation *assignment;
    const cblc::backend::typed_place *destination;
    const cblc::backend::typed_symbol *symbol;
    const cblc::backend::typed_value *literal;
    const t_cblc_data_item *target_item;
    const t_cblc_data_item *source_item;
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !function || !statement || !typed_function || !builder
        || (statement->type != CBLC_STATEMENT_ASSIGNMENT
            && statement->type != CBLC_STATEMENT_COMPUTE)
        || statement->source_offset == static_cast<size_t>(-1))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    assignment = NULL;
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
    {
        if (operation.operation != cblc::backend::opcode::assign
            || operation.places.size() != 1 || operation.operands.size() != 1
            || operation.effects.may_fail || operation.provenance.empty())
            continue ;
        if (operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            assignment = &operation;
            break ;
        }
    }
    if (!assignment)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    destination = NULL;
    for (const cblc::backend::typed_place &place : typed_function->places)
        if (place.id == assignment->places.front())
        {
            destination = &place;
            break ;
        }
    if (!destination || destination->symbol == 0
        || !destination->projections.empty())
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    symbol = NULL;
    for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
        if (candidate.id == destination->symbol)
        {
            symbol = &candidate;
            break ;
        }
    if (!symbol || symbol->canonical_name.empty())
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    target_item = NULL;
    for (size_t index = 0; index < unit->data_count; ++index)
    {
        const t_cblc_data_item *candidate;
        int matches_name;

        candidate = &unit->data_items[index];
        if (candidate->is_alias || candidate->is_shadowed
            || (candidate->is_function_local
                && std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0))
            continue ;
        matches_name = std::strcmp(candidate->source_name,
                symbol->canonical_name.c_str()) == 0
            || (candidate->local_source_name[0] != '\0'
                && std::strcmp(candidate->local_source_name,
                    symbol->canonical_name.c_str()) == 0);
        if (!matches_name)
            continue ;
        if (target_item)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        target_item = candidate;
    }
    if (!target_item || target_item->kind != CBLC_DATA_KIND_INT)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    literal = NULL;
    for (const cblc::backend::typed_value &value : typed_function->values)
        if (value.id == assignment->operands.front())
        {
            literal = &value;
            break ;
        }
    if (literal && literal->constant_value.empty())
        literal = NULL;
    source_item = NULL;
    if (!literal)
    {
        const cblc::backend::typed_operation *load = NULL;
        const cblc::backend::typed_place *source_place = NULL;
        const cblc::backend::typed_symbol *source_symbol = NULL;

        for (const cblc::backend::typed_operation &operation : typed_function->operations)
            if (operation.operation == cblc::backend::opcode::load
                && operation.results.size() == 1
                && operation.results.front() == assignment->operands.front()
                && operation.places.size() == 1)
            {
                load = &operation;
                break ;
            }
        if (!load)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        for (const cblc::backend::typed_place &place : typed_function->places)
            if (place.id == load->places.front())
            {
                source_place = &place;
                break ;
            }
        if (!source_place || source_place->symbol == 0
            || source_place->type != destination->type
            || !source_place->projections.empty())
        {
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        }
        for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
            if (candidate.id == source_place->symbol)
            {
                source_symbol = &candidate;
                break ;
            }
        if (!source_symbol || source_symbol->canonical_name.empty())
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        for (size_t index = 0; index < unit->data_count; ++index)
        {
            const t_cblc_data_item *candidate = &unit->data_items[index];
            const bool matches_name = std::strcmp(candidate->source_name,
                    source_symbol->canonical_name.c_str()) == 0
                || (candidate->local_source_name[0] != '\0'
                    && std::strcmp(candidate->local_source_name,
                        source_symbol->canonical_name.c_str()) == 0);
            if (candidate->is_alias || candidate->is_shadowed
                || (candidate->is_function_local
                    && std::strcmp(candidate->owner_function_name,
                        function->source_name) != 0)
                || !matches_name)
                continue ;
            if (source_item)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            source_item = candidate;
        }
        if (!source_item || source_item->kind != CBLC_DATA_KIND_INT)
        {
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        }
    }

    const char *source_text = NULL;
    if (literal)
    {
        const char *digit;
        size_t digit_count;

        if (literal->constant_value.empty() || literal->type != destination->type)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        digit = literal->constant_value.c_str();
        if (*digit == '+' || *digit == '-')
            ++digit;
        digit_count = std::strlen(digit);
        if (!cblc_argument_is_numeric_literal(literal->constant_value.c_str())
            || digit_count == 0 || digit_count > 9)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        source_text = literal->constant_value.c_str();
    }
    else
        source_text = source_item->cobol_name;
    if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
            source_text, target_item->cobol_name) < 0
        || std::strlen(line) >= sizeof(line))
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line) == FT_SUCCESS
        ? FT_SUCCESS : FT_FAILURE);
}

static int cblc_prepare_typed_scalar_return(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const t_cblc_statement *statement,
    const cblc::backend::typed_function *typed_function,
    char *line, size_t line_size)
{
    const cblc::backend::typed_operation *returned;
    const cblc::backend::typed_value *return_value;
    const t_cblc_data_item *return_item;
    const t_cblc_data_item *source_item;
    const t_cblc_data_item *root_item;
    const char *source_text;

    if (!unit || !function || !statement || !typed_function || !line
        || line_size == 0 || statement->type != CBLC_STATEMENT_RETURN
        || statement->source_offset == static_cast<size_t>(-1)
        || statement->target[0] == '\0')
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    returned = NULL;
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
        if (operation.operation == cblc::backend::opcode::return_value
            && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            if (returned)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            returned = &operation;
        }
    if (!returned || returned->operands.size() != 1)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    return_value = NULL;
    for (const cblc::backend::typed_value &value : typed_function->values)
        if (value.id == returned->operands.front())
        {
            return_value = &value;
            break ;
        }
    if (!return_value)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    return_item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!return_item || return_item->array_count != 0
        || (return_item->kind != CBLC_DATA_KIND_INT
            && return_item->kind != CBLC_DATA_KIND_DOUBLE
            && return_item->kind != CBLC_DATA_KIND_CHAR)
        || return_value->type
            != static_cast<cblc::backend::type_id>(return_item->kind) + 1U)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    source_text = NULL;
    source_item = NULL;
    root_item = NULL;
    if (!return_value->constant_value.empty())
    {
        for (const char *cursor = return_value->constant_value.c_str(); *cursor;
            ++cursor)
            if (!std::isdigit(static_cast<unsigned char>(*cursor))
                && *cursor != '+' && *cursor != '-' && *cursor != '.'
                && *cursor != 'e' && *cursor != 'E')
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
        source_text = return_value->constant_value.c_str();
    }
    else
    {
        const cblc::backend::typed_operation *load = NULL;
        const cblc::backend::typed_place *source_place = NULL;
        const cblc::backend::typed_symbol *source_symbol = NULL;

        for (const cblc::backend::typed_operation &operation :
                typed_function->operations)
            if (operation.operation == cblc::backend::opcode::load
                && operation.results.size() == 1
                && operation.results.front() == return_value->id
                && operation.places.size() == 1)
            {
                if (load)
                    return (CBLC_TYPED_EMIT_UNSUPPORTED);
                load = &operation;
            }
        if (!load)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        for (const cblc::backend::typed_place &place : typed_function->places)
            if (place.id == load->places.front())
            {
                source_place = &place;
                break ;
            }
        if (!source_place || source_place->symbol == 0
            || source_place->type != return_value->type)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        for (const cblc::backend::typed_symbol &symbol : typed_function->symbols)
            if (symbol.id == source_place->symbol)
            {
                source_symbol = &symbol;
                break ;
            }
        if (!source_symbol || source_symbol->canonical_name.empty())
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        for (size_t index = 0; index < unit->data_count; ++index)
        {
            const t_cblc_data_item *candidate = &unit->data_items[index];
            const bool matches_name =
                std::strcmp(candidate->source_name,
                    source_symbol->canonical_name.c_str()) == 0
                || (candidate->local_source_name[0] != '\0'
                    && std::strcmp(candidate->local_source_name,
                        source_symbol->canonical_name.c_str()) == 0);

            if (candidate->is_alias || candidate->is_shadowed
                || (candidate->is_function_local
                    && std::strcmp(candidate->owner_function_name,
                        function->source_name) != 0)
                || candidate->array_count != 0 || !matches_name)
                continue ;
            if (root_item)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            root_item = candidate;
        }
        if (!root_item)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        if (source_place->projections.empty())
        {
            if (root_item->kind != return_item->kind)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            source_item = root_item;
        }
        else
        {
            const t_cblc_data_item *current_item = root_item;

            if (current_item->kind != CBLC_DATA_KIND_STRUCT)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            for (const cblc::backend::place_projection &projection :
                    source_place->projections)
            {
                const t_cblc_struct_type *owner_type;
                const t_cblc_struct_field *field;
                char field_source_name[TRANSPILE_IDENTIFIER_MAX];
                const t_cblc_data_item *resolved_field = NULL;
                const size_t field_index = projection.resolved_member > 0
                    ? projection.resolved_member - 1 : static_cast<size_t>(-1);

                if (projection.projection_kind
                        != cblc::backend::place_projection::kind::field
                    || current_item->kind != CBLC_DATA_KIND_STRUCT
                    || field_index == static_cast<size_t>(-1))
                    return (CBLC_TYPED_EMIT_UNSUPPORTED);
                owner_type = cblc_find_struct_type(unit,
                    current_item->struct_type_name);
                if (!owner_type || field_index >= owner_type->field_count)
                    return (CBLC_TYPED_EMIT_UNSUPPORTED);
                field = &owner_type->fields[field_index];
                if (field->array_count > 0)
                    return (CBLC_TYPED_EMIT_UNSUPPORTED);
                cblc_build_field_source_name(current_item->source_name,
                    field->source_name, field_source_name,
                    sizeof(field_source_name));
                for (size_t item_index = 0; item_index < unit->data_count;
                    ++item_index)
                {
                    const t_cblc_data_item *candidate =
                        &unit->data_items[item_index];

                    if (candidate->is_alias || candidate->is_shadowed
                        || candidate->array_count != 0
                        || std::strcmp(candidate->source_name,
                            field_source_name) != 0
                        || std::strcmp(candidate->owner_function_name,
                            current_item->owner_function_name) != 0)
                        continue ;
                    if (resolved_field)
                        return (CBLC_TYPED_EMIT_UNSUPPORTED);
                    resolved_field = candidate;
                }
                if (!resolved_field
                    || resolved_field->kind != field->kind)
                    return (CBLC_TYPED_EMIT_UNSUPPORTED);
                current_item = resolved_field;
            }
            if (current_item->kind != return_item->kind)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            source_item = current_item;
        }
        source_text = source_item->cobol_name;
    }
    if (!source_text
        || std::snprintf(line, line_size, "           MOVE %s TO %s",
            source_text, return_item->cobol_name) < 0
        || std::strlen(line) >= line_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_typed_scope_exit(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::control_flow_graph *typed_cfg,
    const cblc::backend::lifecycle_cleanup_plan *cleanup_plan,
    t_cobol_text_builder *builder,
    const t_cblc_data_item *receiver_context)
{
    const cblc::backend::typed_operation *scope_exit;
    const cblc::backend::typed_place *place;
    const cblc::backend::typed_symbol *symbol;
    const cblc::backend::typed_callable *destructor_dispatch;
    const t_cblc_data_item *item;
    cblc::backend::operation_id scope_exit_id;
    cblc::backend::value_id destroyed_value;
    size_t cfg_operation_occurrences;
    size_t normal_exit_count;
    size_t element_operation_count;
    const t_cblc_struct_type *item_type;
    t_cblc_statement typed_statement;

    if (!unit || !function || !statement || !typed_function || !typed_cfg
        || !cleanup_plan || !builder || statement->type != CBLC_STATEMENT_DESTRUCT
        || statement->source_offset == static_cast<size_t>(-1)
        || typed_cfg->function_id != typed_function->function_id
        || cleanup_plan->function_id != typed_function->function_id
        || typed_cfg->source_unit_id != typed_function->source_unit_id
        || cleanup_plan->source_unit_id != typed_function->source_unit_id)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    scope_exit = NULL;
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
        if (operation.operation == cblc::backend::opcode::scope_exit
            && operation.places.size() == 1 && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            scope_exit = &operation;
            break ;
        }
    if (!scope_exit)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    scope_exit_id = scope_exit->id;
    destroyed_value = 0;
    for (const cblc::backend::operation_outcome &outcome : scope_exit->outcomes)
        if (outcome.kind == cblc::backend::outcome_kind::normal
            && outcome.destroyed.size() == 1)
        {
            destroyed_value = outcome.destroyed.front();
            break ;
        }
    if (destroyed_value == 0)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    cfg_operation_occurrences = 0;
    for (const cblc::backend::basic_block &block : typed_cfg->blocks)
        for (cblc::backend::operation_id operation_id : block.operations)
            if (operation_id == scope_exit_id)
                cfg_operation_occurrences += 1;
    if (cfg_operation_occurrences != 1)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    normal_exit_count = 0;
    for (const cblc::backend::cleanup_site &site : cleanup_plan->exits)
        if (site.exit_kind == cblc::backend::cfg_terminator_kind::normal_exit)
        {
            bool object_remains_live = false;

            ++normal_exit_count;
            for (cblc::backend::value_id live_value : site.destruction_order)
                if (live_value == destroyed_value)
                {
                    object_remains_live = true;
                    break ;
                }
            if (object_remains_live)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
        }
    if (normal_exit_count == 0)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    place = NULL;
    for (const cblc::backend::typed_place &candidate : typed_function->places)
        if (candidate.id == scope_exit->places.front())
        {
            place = &candidate;
            break ;
        }
    if (!place || place->symbol == 0
        || place->projections.size() > 1
        || (!place->projections.empty()
            && place->projections.front().projection_kind
                != cblc::backend::place_projection::kind::index))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    symbol = NULL;
    for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
        if (candidate.id == place->symbol)
        {
            symbol = &candidate;
            break ;
        }
    if (!symbol || symbol->canonical_name.empty())
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    item = NULL;
    for (size_t index = 0; index < unit->data_count; ++index)
    {
        const t_cblc_data_item *candidate = &unit->data_items[index];
        const bool matches_name = std::strcmp(candidate->source_name,
                symbol->canonical_name.c_str()) == 0
            || (candidate->local_source_name[0] != '\0'
                && std::strcmp(candidate->local_source_name,
                    symbol->canonical_name.c_str()) == 0);
        if (candidate->is_alias || candidate->is_shadowed
            || (candidate->is_function_local
                && std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0)
            || !matches_name)
            continue ;
        if (item)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        item = candidate;
    }
    item_type = item ? cblc_find_struct_type(unit, item->struct_type_name) : NULL;
    if (!item || item->kind != CBLC_DATA_KIND_STRUCT || item->is_template_item
        || !item_type
        || !cblc_type_requires_destruct(unit, item_type))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    destructor_dispatch = NULL;
    if (scope_exit->callee != 0)
    {
        for (const cblc::backend::typed_callable &callable :
            typed_function->callables)
            if (callable.id == scope_exit->callee)
            {
                destructor_dispatch = &callable;
                break ;
            }
        if (!destructor_dispatch
            || destructor_dispatch->kind
                != cblc::backend::callable_kind::destructor
            || destructor_dispatch->owner_type != place->type
            || destructor_dispatch->canonical_name
                != std::string(item_type->source_name) + "::~"
                    + item_type->source_name
            || !destructor_dispatch->has_definition
            || destructor_dispatch->exceptions
                != cblc::backend::exception_effect::non_throwing)
            return (FT_FAILURE);
    }
    else if (item_type->has_destructor
        || item_type->has_destructor_definition)
        return (FT_FAILURE);
    if (item->array_count == 0)
    {
        if (!place->projections.empty())
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
    }
    else
    {
        size_t expected_element = item->array_count;

        element_operation_count = 0;
        for (const cblc::backend::typed_operation &operation : typed_function->operations)
        {
            const cblc::backend::typed_place *element_place = NULL;
            cblc::backend::value_id element_value = 0;

            if (operation.operation != cblc::backend::opcode::scope_exit
                || operation.places.size() != 1 || operation.provenance.empty()
                || operation.provenance.front().source_offset
                    != static_cast<std::uint64_t>(statement->source_offset))
                continue ;
            for (const cblc::backend::typed_place &candidate : typed_function->places)
                if (candidate.id == operation.places.front())
                {
                    element_place = &candidate;
                    break ;
                }
            if (!element_place || element_place->symbol != symbol->id
                || element_place->projections.size() != 1)
                continue ;
            if ((operation.callee == 0) != (scope_exit->callee == 0)
                || operation.callee != scope_exit->callee)
                return (FT_FAILURE);
            if (expected_element == 0
                || element_place->projections.front().projection_kind
                    != cblc::backend::place_projection::kind::index
                || element_place->projections.front().resolved_member
                    != expected_element--)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            for (const cblc::backend::operation_outcome &outcome : operation.outcomes)
                if (outcome.kind == cblc::backend::outcome_kind::normal
                    && outcome.destroyed.size() == 1)
                {
                    element_value = outcome.destroyed.front();
                    break ;
                }
            if (element_value == 0)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            cfg_operation_occurrences = 0;
            for (const cblc::backend::basic_block &block : typed_cfg->blocks)
                for (cblc::backend::operation_id operation_id : block.operations)
                    if (operation_id == operation.id)
                        cfg_operation_occurrences += 1;
            if (cfg_operation_occurrences != 1)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            for (const cblc::backend::cleanup_site &site : cleanup_plan->exits)
                if (site.exit_kind == cblc::backend::cfg_terminator_kind::normal_exit)
                    for (cblc::backend::value_id live_value : site.destruction_order)
                        if (live_value == element_value)
                            return (CBLC_TYPED_EMIT_UNSUPPORTED);
            element_operation_count += 1;
        }
        if (expected_element != 0
            || element_operation_count != item->array_count)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
    }
    typed_statement = *statement;
    ft_strlcpy(typed_statement.target, item->cobol_name,
        sizeof(typed_statement.target));
    return (cblc_emit_lifecycle_statement(unit, &typed_statement, builder,
        function, statement_index, receiver_context));
}

static int cblc_emit_typed_construct(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::control_flow_graph *typed_cfg,
    t_cobol_text_builder *builder,
    const t_cblc_data_item *receiver_context)
{
    const cblc::backend::typed_operation *construction;
    const cblc::backend::typed_place *place;
    const cblc::backend::typed_symbol *symbol;
    const cblc::backend::typed_callable *constructor_dispatch;
    const t_cblc_data_item *requested_item;
    const t_cblc_data_item *item;
    const t_cblc_struct_type *item_type;
    const t_cblc_constructor *selected_constructor;
    t_cblc_statement typed_statement;
    size_t cfg_operation_occurrences;
    size_t element_operation_count;
    size_t selected_constructor_count;

    if (!unit || !function || !statement || !typed_function || !typed_cfg
        || !builder || statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
        || statement->source_offset == static_cast<size_t>(-1)
        || typed_cfg->function_id != typed_function->function_id
        || typed_cfg->source_unit_id != typed_function->source_unit_id)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    construction = NULL;
    requested_item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!requested_item)
        requested_item = cblc_find_data_item_for_emit_by_source(unit,
            statement->target);
    if (!requested_item)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
        if (operation.operation == cblc::backend::opcode::construct
            && operation.places.size() == 1 && operation.results.size() == 1
            && !operation.provenance.empty()
            && operation.provenance.front().source_offset
                == static_cast<std::uint64_t>(statement->source_offset))
        {
            const cblc::backend::typed_place *candidate_place = NULL;
            const cblc::backend::typed_symbol *candidate_symbol = NULL;

            for (const cblc::backend::typed_place &candidate : typed_function->places)
                if (candidate.id == operation.places.front())
                {
                    candidate_place = &candidate;
                    break ;
                }
            if (!candidate_place)
                continue ;
            for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
                if (candidate.id == candidate_place->symbol)
                {
                    candidate_symbol = &candidate;
                    break ;
                }
            if (!candidate_symbol
                || (std::strcmp(candidate_symbol->canonical_name.c_str(),
                        requested_item->source_name) != 0
                    && (requested_item->local_source_name[0] == '\0'
                        || std::strcmp(candidate_symbol->canonical_name.c_str(),
                            requested_item->local_source_name) != 0)))
                continue ;
            construction = &operation;
            break ;
        }
    if (!construction)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    cfg_operation_occurrences = 0;
    for (const cblc::backend::basic_block &block : typed_cfg->blocks)
        for (cblc::backend::operation_id operation_id : block.operations)
            if (operation_id == construction->id)
                cfg_operation_occurrences += 1;
    if (cfg_operation_occurrences != 1)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    place = NULL;
    for (const cblc::backend::typed_place &candidate : typed_function->places)
        if (candidate.id == construction->places.front())
        {
            place = &candidate;
            break ;
        }
    if (!place || place->symbol == 0 || place->projections.size() > 1
        || (!place->projections.empty()
            && place->projections.front().projection_kind
                != cblc::backend::place_projection::kind::index)
        || place->type == 0)
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    symbol = NULL;
    for (const cblc::backend::typed_symbol &candidate : typed_function->symbols)
        if (candidate.id == place->symbol)
        {
            symbol = &candidate;
            break ;
        }
    if (!symbol || symbol->canonical_name.empty())
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    item = NULL;
    for (size_t index = 0; index < unit->data_count; ++index)
    {
        const t_cblc_data_item *candidate = &unit->data_items[index];
        const bool matches_name = std::strcmp(candidate->source_name,
                symbol->canonical_name.c_str()) == 0
            || (candidate->local_source_name[0] != '\0'
                && std::strcmp(candidate->local_source_name,
                    symbol->canonical_name.c_str()) == 0);
        if (candidate->is_alias || candidate->is_shadowed
            || (candidate->is_function_local
                && std::strcmp(candidate->owner_function_name,
                    function->source_name) != 0)
            || !matches_name)
            continue ;
        if (item)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
        item = candidate;
    }
    item_type = item ? cblc_find_struct_type(unit, item->struct_type_name) : NULL;
    if (!item || item->kind != CBLC_DATA_KIND_STRUCT || item->is_template_item
        || !item_type || !cblc_type_requires_default_construct(unit, item_type))
        return (CBLC_TYPED_EMIT_UNSUPPORTED);
    constructor_dispatch = NULL;
    selected_constructor = NULL;
    if (construction->callee != 0)
    {
        for (const cblc::backend::typed_callable &callable :
            typed_function->callables)
            if (callable.id == construction->callee)
            {
                constructor_dispatch = &callable;
                break ;
            }
        if (!constructor_dispatch
            || constructor_dispatch->kind
                != cblc::backend::callable_kind::constructor
            || constructor_dispatch->owner_type != place->type
            || constructor_dispatch->canonical_name
                != std::string(item_type->source_name) + "::<constructor>"
            || !constructor_dispatch->has_definition
            || constructor_dispatch->exceptions != construction->effects.exceptions)
            return (FT_FAILURE);
        selected_constructor_count = 0;
        for (size_t constructor_index = 0;
            constructor_index < item_type->constructor_count;
            ++constructor_index)
            if (cblc::backend::canonical_constructor_symbol_id(item_type,
                    &item_type->constructors[constructor_index])
                == construction->callee)
            {
                selected_constructor = &item_type->constructors[constructor_index];
                selected_constructor_count += 1;
            }
        if (selected_constructor_count != 1)
            return (FT_FAILURE);
    }
    else if (statement->call_argument_count != 0)
        return (FT_FAILURE);
    if (item->array_count == 0)
    {
        if (!place->projections.empty())
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
    }
    else
    {
        size_t expected_element = 1;

        element_operation_count = 0;
        for (const cblc::backend::typed_operation &operation : typed_function->operations)
        {
            const cblc::backend::typed_place *element_place = NULL;

            if (operation.operation != cblc::backend::opcode::construct
                || operation.places.size() != 1 || operation.results.size() != 1
                || operation.provenance.empty()
                || operation.provenance.front().source_offset
                    != static_cast<std::uint64_t>(statement->source_offset))
                continue ;
            for (const cblc::backend::typed_place &candidate : typed_function->places)
                if (candidate.id == operation.places.front())
                {
                    element_place = &candidate;
                    break ;
                }
            if (!element_place || element_place->symbol != symbol->id
                || element_place->projections.size() != 1)
                continue ;
            if (operation.callee != construction->callee)
                return (FT_FAILURE);
            if (element_place->projections.front().projection_kind
                    != cblc::backend::place_projection::kind::index
                || element_place->projections.front().resolved_member
                    != expected_element++)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            cfg_operation_occurrences = 0;
            for (const cblc::backend::basic_block &block : typed_cfg->blocks)
                for (cblc::backend::operation_id operation_id : block.operations)
                    if (operation_id == operation.id)
                        cfg_operation_occurrences += 1;
            if (cfg_operation_occurrences != 1)
                return (CBLC_TYPED_EMIT_UNSUPPORTED);
            element_operation_count += 1;
        }
        if (expected_element != item->array_count + 1
            || element_operation_count != item->array_count)
            return (CBLC_TYPED_EMIT_UNSUPPORTED);
    }
    typed_statement = *statement;
    ft_strlcpy(typed_statement.target, item->cobol_name,
        sizeof(typed_statement.target));
    return (cblc_emit_lifecycle_statement_selected(unit, &typed_statement,
        builder, function, statement_index, receiver_context,
        selected_constructor));
}

/* Transfer construction is emitted only when typed IR has selected one
 * concrete copy/move constructor and the CFG owns that operation. Pass that
 * exact constructor into the COBOL lifecycle emitter so it cannot perform a
 * second, potentially divergent overload selection. */
static int cblc_typed_transfer_unsupported(t_cobol_text_builder *builder,
    const char *reason)
{
    if (builder && reason)
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, reason);
    return (CBLC_TYPED_EMIT_UNSUPPORTED);
}

static int cblc_emit_typed_transfer_construct(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement,
    size_t statement_index,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::control_flow_graph *typed_cfg,
    t_cobol_text_builder *builder,
    const t_cblc_data_item *receiver_context)
{
    const cblc::backend::typed_operation *selected_operation;
    const cblc::backend::typed_callable *selected_callable;
    const cblc::backend::typed_place *destination;
    const cblc::backend::typed_type *destination_type;
    const cblc::backend::typed_place *source_place;
    const cblc::backend::typed_symbol *source_symbol;
    const t_cblc_struct_type *owner_type;
    const t_cblc_data_item *source_item;
    const t_cblc_constructor *selected_constructor;
    t_cblc_statement typed_statement;
    int source_is_receiver;
    cblc::backend::opcode selected_opcode;
    size_t selected_constructor_count;
    size_t cfg_occurrences;

    if (!unit || !function || !statement || !typed_function || !typed_cfg
        || !builder || (!statement->is_move_construct
            && !statement->is_relocate_construct)
        || statement->source_offset == static_cast<size_t>(-1)
        || typed_cfg->function_id != typed_function->function_id
        || typed_cfg->source_unit_id != typed_function->source_unit_id)
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer does not belong to the emitted function/CFG"));
    selected_operation = NULL;
    selected_opcode = statement->is_move_construct
        ? cblc::backend::opcode::move_construct
        : cblc::backend::opcode::copy_construct;
    for (const cblc::backend::typed_operation &operation : typed_function->operations)
    {
        const bool operation_matches = statement->is_relocate_construct
            ? (operation.operation == cblc::backend::opcode::move_construct
                || operation.operation == cblc::backend::opcode::copy_construct)
            : operation.operation == selected_opcode;

        if (!operation_matches || operation.callee == 0
            || operation.places.size() != 1 || operation.results.size() != 1
            || operation.provenance.empty()
            || operation.provenance.front().source_offset
                != static_cast<std::uint64_t>(statement->source_offset))
            continue ;
        if (selected_operation)
            return (cblc_typed_transfer_unsupported(builder,
                "typed transfer source offset maps to multiple lifecycle operations"));
        selected_operation = &operation;
        selected_opcode = operation.operation;
    }
    if (!selected_operation)
        return (cblc_typed_transfer_unsupported(builder,
            "typed CFG has no copy/move operation for the relocation source offset"));
    cfg_occurrences = 0;
    for (const cblc::backend::basic_block &block : typed_cfg->blocks)
        for (cblc::backend::operation_id operation_id : block.operations)
            if (operation_id == selected_operation->id)
                cfg_occurrences += 1;
    if (cfg_occurrences != 1)
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer operation is not owned exactly once by its CFG"));
    destination = NULL;
    for (const cblc::backend::typed_place &place : typed_function->places)
        if (place.id == selected_operation->places.front())
        {
            destination = &place;
            break ;
        }
    if (!destination || destination->type == 0)
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer has no destination place or concrete destination type"));
    if (selected_operation->effects.reads.empty())
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer has no source place in its verified read effects"));
    source_place = NULL;
    for (const cblc::backend::typed_place &place : typed_function->places)
        if (place.id == selected_operation->effects.reads.front())
        {
            source_place = &place;
            break ;
        }
    if (!source_place)
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer source place is absent from its function IR"));
    source_symbol = NULL;
    if (source_place->symbol != 0)
        for (const cblc::backend::typed_symbol &symbol : typed_function->symbols)
            if (symbol.id == source_place->symbol)
            {
                source_symbol = &symbol;
                break ;
            }
    if (!source_symbol && source_place->base_value != 0)
        for (const cblc::backend::typed_place &base_place : typed_function->places)
            if (base_place.base_value == source_place->base_value
                && base_place.symbol != 0)
                for (const cblc::backend::typed_symbol &symbol : typed_function->symbols)
                    if (symbol.id == base_place.symbol)
                    {
                        source_symbol = &symbol;
                        break ;
                    }
    if (!source_symbol || source_symbol->canonical_name.empty())
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer source place has no source symbol for COBOL lowering"));
    source_is_receiver = source_symbol->canonical_name == "this";
    source_item = NULL;
    if (source_is_receiver && receiver_context
        && receiver_context->kind == CBLC_DATA_KIND_STRUCT)
    {
        for (const cblc::backend::typed_type &type : typed_function->types)
            if (type.id == source_place->type
                && type.canonical_name == receiver_context->struct_type_name)
            {
                source_item = receiver_context;
                break ;
            }
    }
    for (size_t parameter_index = 0;
        source_item == NULL && parameter_index < function->parameter_count;
        ++parameter_index)
    {
        const t_cblc_parameter *parameter = &function->parameters[parameter_index];

        if ((parameter->actual_source_name[0] != '\0'
                && source_symbol->canonical_name == parameter->actual_source_name)
            || (parameter->source_name[0] != '\0'
                && source_symbol->canonical_name == parameter->source_name)
            || (parameter->cobol_name[0] != '\0'
                && source_symbol->canonical_name == parameter->cobol_name))
        {
            source_item = cblc_find_data_item_by_cobol(unit,
                parameter->cobol_name);
            break ;
        }
    }
    if (!source_item)
        for (size_t data_index = 0; data_index < unit->data_count;
            ++data_index)
        {
            const t_cblc_data_item *candidate = &unit->data_items[data_index];

            if ((!candidate->is_function_local && !candidate->is_alias)
                || ((candidate->is_function_local || candidate->is_alias)
                    && std::strcmp(candidate->owner_function_name,
                        function->source_name) != 0)
                || (source_symbol->canonical_name != candidate->local_source_name
                    && source_symbol->canonical_name != candidate->source_name
                    && source_symbol->canonical_name != candidate->cobol_name))
                continue ;
            source_item = candidate;
            break ;
        }
    if ((!source_item && !source_is_receiver)
        || (source_item && source_item->struct_type_name[0] == '\0'))
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "typed transfer source symbol '%s' in '%s' has no matching concrete CBL-C storage (reads=%zu, source kind=%d)",
            source_symbol->canonical_name.c_str(), function->source_name,
            selected_operation->effects.reads.size(),
            source_place->projections.empty() ? -1
                : static_cast<int>(
                    source_place->projections.back().projection_kind));
        return (cblc_typed_transfer_unsupported(builder, diagnostic));
    }
    selected_callable = NULL;
    for (const cblc::backend::typed_callable &callable : typed_function->callables)
        if (callable.id == selected_operation->callee)
        {
            selected_callable = &callable;
            break ;
        }
    if (!selected_callable
        || selected_callable->kind != (selected_opcode
                == cblc::backend::opcode::move_construct
            ? cblc::backend::callable_kind::move_constructor
            : cblc::backend::callable_kind::copy_constructor)
        || selected_callable->owner_type != destination->type
        || !selected_callable->has_definition
        || selected_callable->exceptions != selected_operation->effects.exceptions)
    {
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
            "typed transfer's selected callable does not match its concrete destination type and exception effects");
        return (FT_FAILURE);
    }
    destination_type = NULL;
    for (const cblc::backend::typed_type &type : typed_function->types)
        if (type.id == destination->type)
        {
            destination_type = &type;
            break ;
        }
    owner_type = destination_type
        ? cblc_find_struct_type(unit, destination_type->canonical_name.c_str()) : NULL;
    if (!owner_type || (source_item
            && std::strcmp(source_item->struct_type_name,
                owner_type->source_name) != 0))
        return (cblc_typed_transfer_unsupported(builder,
            "typed transfer source/destination has no matching concrete CBL-C class declaration"));
    selected_constructor = NULL;
    selected_constructor_count = 0;
    for (size_t constructor_index = 0;
        constructor_index < owner_type->constructor_count; ++constructor_index)
        if (cblc::backend::canonical_constructor_symbol_id(owner_type,
                &owner_type->constructors[constructor_index])
            == selected_operation->callee)
        {
            selected_constructor = &owner_type->constructors[constructor_index];
            selected_constructor_count += 1;
        }
    if (selected_constructor_count != 1
        || (selected_opcode == cblc::backend::opcode::move_construct
            && !selected_constructor->is_move_constructor)
        || (selected_opcode == cblc::backend::opcode::copy_construct
            && !selected_constructor->is_copy_constructor))
    {
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
            "typed transfer's constructor symbol did not resolve to one matching CBL-C constructor");
        return (FT_FAILURE);
    }

    /* Relocation's typed policy is move-if-nothrow, otherwise copy.  Confirm
     * that the legacy COBOL selector has precisely the same available choice
     * before delegating its established statement/exception emission. */
    if (statement->is_relocate_construct)
    {
        const t_cblc_constructor *move_constructor;
        const t_cblc_constructor *copy_constructor;
        int expected_move;

        move_constructor = cblc_find_move_constructor(owner_type);
        copy_constructor = cblc_find_copy_constructor(owner_type);
        expected_move = move_constructor && move_constructor->has_definition
            && !cblc_constructor_may_throw(unit, owner_type, move_constructor, 0);
        if ((expected_move && selected_constructor != move_constructor)
            || (!expected_move && (!copy_constructor
                || selected_constructor != copy_constructor)))
        {
            cobol_text_builder_set_diagnostic(builder,
                TRANSPILE_ERROR_BACKEND_IR_VERIFICATION,
                "typed relocation constructor disagrees with the CBL-C move-if-nothrow selection rule");
            return (FT_FAILURE);
        }
    }
    {
        typed_statement = *statement;
        if (source_item)
        {
            ft_strlcpy(typed_statement.call_arguments, source_item->cobol_name,
                sizeof(typed_statement.call_arguments));
            typed_statement.call_argument_count = 1;
        }
        const int emit_status = cblc_emit_lifecycle_statement_selected(unit,
            &typed_statement, builder, function, statement_index, receiver_context,
            selected_constructor);

        if (emit_status != FT_SUCCESS)
        {
            if (builder->diagnostic_message[0] == '\0')
            {
                char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(message, sizeof(message),
                    "typed transfer for '%s' selected constructor '%s' but COBOL lifecycle emission failed",
                    destination_type->canonical_name.c_str(),
                    selected_constructor->is_move_constructor
                        ? "move" : "copy");
                cobol_text_builder_set_diagnostic(builder,
                    TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, message);
            }
        }
        return (emit_status);
    }
}

static int cblc_lower_method_transfer_statement(
    const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver_context,
    const t_cblc_function *emission_function,
    const t_cblc_statement *emission_statement,
    cblc::backend::typed_function *out_typed_function,
    cblc::backend::control_flow_graph *out_cfg,
    cblc::backend::lifecycle_cleanup_plan *out_cleanup,
    t_cobol_text_builder *builder)
{
    std::vector<t_cblc_struct_type> adapted_types;
    std::vector<t_cblc_method> adapted_methods;
    t_cblc_translation_unit adapted_unit;
    t_cblc_method adapted_method;
    t_cblc_statement selected_statement;
    size_t type_index;
    size_t method_index;
    const t_cblc_method *source_method;
    cblc::backend::verification_diagnostic diagnostic;

    if (!unit || !receiver_context || !emission_function
        || !emission_statement || !out_typed_function || !out_cfg
        || !out_cleanup || !builder)
        return (FT_FAILURE);
    type_index = 0;
    while (type_index < unit->struct_type_count
        && std::strcmp(unit->struct_types[type_index].source_name,
            receiver_context->struct_type_name) != 0)
        ++type_index;
    if (type_index >= unit->struct_type_count)
        return (FT_FAILURE);
    const t_cblc_struct_type *owner_type = &unit->struct_types[type_index];
    source_method = NULL;
    method_index = 0;
    while (method_index < owner_type->method_count)
    {
        const t_cblc_method *candidate = &owner_type->methods[method_index];

        if (std::strcmp(candidate->source_name,
                emission_function->source_name) == 0)
        {
            size_t statement_index = 0;

            while (statement_index < candidate->statement_count)
            {
                const t_cblc_statement *candidate_statement =
                    &candidate->statements[statement_index];

                if (candidate_statement->source_offset
                        == emission_statement->source_offset
                    && (candidate_statement->is_move_construct
                        || candidate_statement->is_relocate_construct))
                {
                    source_method = candidate;
                    selected_statement = *candidate_statement;
                    break;
                }
                ++statement_index;
            }
            if (source_method)
                break;
        }
        ++method_index;
    }
    if (!source_method)
        return (FT_FAILURE);

    /* Lower the concrete lifecycle operation in its declaring method's
     * storage/type environment, isolating it from legacy try/catch markers.
     * The operation keeps its canonical source offset and exact template type;
     * its typed CFG must verify before the COBOL emitter can consume it. */
    adapted_unit = *unit;
    adapted_types.assign(unit->struct_types,
        unit->struct_types + unit->struct_type_count);
    adapted_methods.assign(owner_type->methods,
        owner_type->methods + owner_type->method_count);
    adapted_method = *source_method;
    adapted_method.statements = &selected_statement;
    adapted_method.statement_count = 1;
    adapted_method.statement_capacity = 1;
    adapted_methods[method_index] = adapted_method;
    adapted_types[type_index].methods = adapted_methods.data();
    adapted_unit.struct_types = adapted_types.data();

    if (!cblc::backend::lower_scalar_method(&adapted_unit, type_index,
            method_index, out_typed_function, &diagnostic))
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        int diagnostic_code;

        diagnostic_code = TRANSPILE_ERROR_BACKEND_IR_VERIFICATION;
        if (selected_statement.is_relocate_construct
            && std::strcmp(owner_type->template_source_name, "vector") == 0
            && std::strcmp(emission_function->source_name, "reallocate") == 0
            && diagnostic.message.find(
                "typed relocation requires a defined nothrow move constructor or a defined copy constructor")
                != std::string::npos)
        {
            diagnostic_code = TRANSPILE_ERROR_VECTOR_THROWING_MOVE_RELOCATION;
            std::snprintf(message, sizeof(message),
                "vector reallocation requires a copy constructor or a non-throwing move constructor; '%s' has no safe relocation constructor",
                owner_type->template_argument_name);
        }
        else
            std::snprintf(message, sizeof(message),
                "typed lifecycle lowering failed in '%s::%s' at source offset %zu: %.220s",
                receiver_context->struct_type_name, emission_function->source_name,
                selected_statement.source_offset, diagnostic.message.c_str());
        cobol_text_builder_set_diagnostic(builder, diagnostic_code, message);
        return (FT_FAILURE);
    }
    if (!cblc::backend::build_structured_control_flow_graph_with_cleanup(
            *out_typed_function, out_cfg, out_cleanup, &diagnostic,
            cblc_lifetime_policy_for_unit(unit)))
    {
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(message, sizeof(message),
            "typed lifecycle CFG in method '%s::%s' failed verification: %s",
            receiver_context->struct_type_name, emission_function->source_name,
            diagnostic.message.c_str());
        cobol_text_builder_set_diagnostic(builder,
            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, message);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_method_transfer_is_scalar_pointer(
    const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver_context,
    const t_cblc_statement *statement)
{
    const t_cblc_struct_type *owner_type;
    const t_cblc_data_item *destination_pointer;
    char destination_base[TRANSPILE_IDENTIFIER_MAX];
    char source_base[TRANSPILE_IDENTIFIER_MAX];
    size_t field_index;

    if (!unit || !receiver_context || !statement
        || !statement->is_relocate_construct
        || cblc_parse_cobol_reference_base(statement->target, destination_base,
            sizeof(destination_base)) != FT_SUCCESS
        || cblc_parse_cobol_reference_base(statement->call_arguments, source_base,
            sizeof(source_base)) != FT_SUCCESS)
        return (0);
    owner_type = cblc_find_struct_type(unit,
        receiver_context->struct_type_name);
    if (!owner_type)
        return (0);
    /* Scalar vector elements have no class lifecycle to dispatch. The
     * concrete specialization's backing field is authoritative; do not depend
     * on exception-wrapper temporaries being present in the data-item table. */
    if (owner_type->is_template_instantiation
        && std::strcmp(owner_type->template_source_name, "vector") == 0)
    {
        field_index = 0;
        while (field_index < owner_type->field_count)
        {
            const t_cblc_struct_field *field = &owner_type->fields[field_index];

            if (std::strcmp(field->source_name, "elements") == 0
                && (field->kind == CBLC_DATA_KIND_CHAR_POINTER
                    || field->kind == CBLC_DATA_KIND_INT_POINTER))
                return (1);
            ++field_index;
        }
    }
    destination_pointer = cblc_find_data_item_for_emit_by_source(unit,
        destination_base);
    if (!destination_pointer)
        destination_pointer = cblc_find_data_item_by_cobol(unit,
            destination_base);
    if (!destination_pointer
        || (destination_pointer->kind != CBLC_DATA_KIND_CHAR_POINTER
            && destination_pointer->kind != CBLC_DATA_KIND_INT_POINTER
            && destination_pointer->kind != CBLC_DATA_KIND_VOID_POINTER))
        return (0);
    field_index = 0;
    while (field_index < owner_type->field_count)
    {
        const t_cblc_struct_field *field = &owner_type->fields[field_index];

        if (std::strcmp(field->source_name, source_base) == 0
            && field->kind == destination_pointer->kind)
            return (1);
        ++field_index;
    }
    return (0);
}

static unsigned long long cblc_exception_helper_hash(const char *function_name,
    size_t region_id)
{
    unsigned long long hash;
    const char *cursor;

    hash = 1469598103934665603ULL;
    cursor = function_name ? function_name : "";
    while (*cursor)
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= static_cast<unsigned long long>(region_id);
    hash *= 1099511628211ULL;
    return (hash);
}

static int cblc_exception_helper_names(const char *function_name,
    size_t region_id, char *paragraph_name, size_t paragraph_name_size,
    char *return_name, size_t return_name_size)
{
    unsigned long long hash;

    if (!function_name || !paragraph_name || paragraph_name_size == 0
        || !return_name || return_name_size == 0)
        return (FT_FAILURE);
    hash = cblc_exception_helper_hash(function_name, region_id);
    if (std::snprintf(paragraph_name, paragraph_name_size,
            "CBLC-TRY-CALL-%016llX", hash) < 0
        || std::strlen(paragraph_name) >= paragraph_name_size
        || std::snprintf(return_name, return_name_size,
            "CBLC-TRY-RET-%016llX", hash) < 0
        || std::strlen(return_name) >= return_name_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static const char *cblc_cleanup_owner_cobol_name(
    const t_cblc_translation_unit *unit, const t_cblc_function *function)
{
    if (!unit || !function || !function->is_exception_helper)
        return (function ? function->cobol_name : "");
    for (size_t function_index = 0; function_index < unit->function_count;
        ++function_index)
    {
        const t_cblc_function *candidate = &unit->functions[function_index];
        char helper_name[TRANSPILE_IDENTIFIER_MAX];
        char return_name[TRANSPILE_IDENTIFIER_MAX];

        if (std::strcmp(candidate->source_name, function->source_name) != 0
            || cblc_exception_helper_names(candidate->cobol_name,
                function->helper_exception_region_id, helper_name,
                sizeof(helper_name), return_name, sizeof(return_name)) != FT_SUCCESS
            || std::strcmp(helper_name, function->cobol_name) != 0)
            continue ;
        return (candidate->cobol_name);
    }
    return (function->cobol_name);
}

static int cblc_exception_region_end(const t_cblc_function *function,
    size_t begin_index, size_t *out_end_index)
{
    size_t index;
    size_t region_id;

    if (!function || !out_end_index || begin_index >= function->statement_count
        || function->statements[begin_index].type != CBLC_STATEMENT_TRY_BEGIN)
        return (FT_FAILURE);
    region_id = function->statements[begin_index].exception_region_id;
    index = begin_index + 1;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement = &function->statements[index];

        if (statement->type == CBLC_STATEMENT_TRY_END
            && statement->exception_region_id == region_id)
        {
            *out_end_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_emit_exception_helpers(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, t_cobol_text_builder *builder,
    const t_cblc_data_item *receiver_context)
{
    size_t index;

    if (!unit || !function || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *try_begin;

        try_begin = &function->statements[index];
        if (try_begin->type != CBLC_STATEMENT_TRY_BEGIN)
        {
            index += 1;
            continue ;
        }
        {
            t_cblc_function helper;
            size_t end_index;
            size_t body_start;
            char helper_name[TRANSPILE_IDENTIFIER_MAX];
            char return_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_exception_region_end(function, index, &end_index) != FT_SUCCESS
                || cblc_exception_helper_names(function->cobol_name,
                    try_begin->exception_region_id, helper_name, sizeof(helper_name),
                    return_name, sizeof(return_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            helper = *function;
            body_start = index + 1;
            helper.statements = &function->statements[body_start];
            helper.statement_count = end_index - body_start + 1;
            helper.statement_capacity = helper.statement_count;
            ft_strlcpy(helper.cobol_name, helper_name, sizeof(helper.cobol_name));
            helper.returns_to_paragraph = 1;
            helper.is_exception_helper = 1;
            helper.helper_exception_region_id = try_begin->exception_region_id;
            ft_strlcpy(helper.helper_parent_return_label,
                function->paragraph_return_label,
                sizeof(helper.helper_parent_return_label));
            ft_strlcpy(helper.paragraph_return_label, return_name,
                sizeof(helper.paragraph_return_label));
            /* The same concrete function/region can be reached through
             * several nested pointer-backed receivers. Paragraph identity is
             * already canonicalized from that function and region, so emit
             * its helper body once and let all call sites share it. */
            if (builder->known_paragraph_definitions
                && builder->known_paragraph_definitions->find(helper_name)
                    != builder->known_paragraph_definitions->end())
            {
                index = end_index + 1;
                continue ;
            }
            if (cblc_emit_function(unit, &helper, builder, 0,
                    receiver_context) != FT_SUCCESS)
                return (FT_FAILURE);
            index = end_index + 1;
        }
    }
    return (FT_SUCCESS);
}

static int cblc_translate_condition_for_cobol(
    const t_cblc_translation_unit *unit, const char *expression,
    t_cobol_text_builder *builder, char *translated, size_t translated_size)
{
    char rewritten[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t input_index;
    size_t output_length;

    if (!unit || !expression || !builder || !translated || translated_size == 0)
        return (FT_FAILURE);
    rewritten[0] = '\0';
    output_length = 0;
    input_index = 0;
    while (expression[input_index] != '\0')
    {
        if (expression[input_index] == '\'' || expression[input_index] == '"')
        {
            const char quote = expression[input_index];

            do
            {
                if (output_length + 1 >= sizeof(rewritten))
                    return (FT_FAILURE);
                rewritten[output_length++] = expression[input_index];
                if (expression[input_index] == quote
                    && (input_index == 0
                        || expression[input_index - 1] != '\\'))
                {
                    input_index += 1;
                    break ;
                }
                input_index += 1;
            } while (expression[input_index] != '\0');
            rewritten[output_length] = '\0';
            continue ;
        }
        if (std::strncmp(expression + input_index, "std::ord", 8) == 0)
        {
            static const char cobol_ord[] = "FUNCTION ORD";

            if (sizeof(cobol_ord) - 1 >= sizeof(rewritten) - output_length)
                return (FT_FAILURE);
            std::memcpy(rewritten + output_length, cobol_ord,
                sizeof(cobol_ord) - 1);
            output_length += sizeof(cobol_ord) - 1;
            rewritten[output_length] = '\0';
            input_index += 8;
            continue ;
        }
        if ((std::isalpha(static_cast<unsigned char>(expression[input_index]))
                || expression[input_index] == '_')
            && (input_index == 0
                || (!std::isalnum(static_cast<unsigned char>(
                        expression[input_index - 1]))
                    && expression[input_index - 1] != '_'
                    && expression[input_index - 1] != '-')))
        {
            size_t token_end;
            size_t subscript_end;
            char pointer_name[TRANSPILE_IDENTIFIER_MAX];
            char subscript[TRANSPILE_STATEMENT_TEXT_MAX];
            const t_cblc_data_item *pointer_item;
            int is_character_pointer;

            token_end = input_index;
            while ((std::isalnum(static_cast<unsigned char>(expression[token_end]))
                    || expression[token_end] == '_'
                    || expression[token_end] == '-')
                && token_end - input_index + 1 < sizeof(pointer_name))
                token_end += 1;
            pointer_name[0] = '\0';
            if (token_end > input_index
                && expression[token_end] == '[')
            {
                std::memcpy(pointer_name, expression + input_index,
                    token_end - input_index);
                pointer_name[token_end - input_index] = '\0';
                pointer_item = cblc_find_data_item_by_cobol(unit,
                    pointer_name);
                is_character_pointer = pointer_item
                    && pointer_item->kind == CBLC_DATA_KIND_CHAR_POINTER;
                /* A class pointer field is flattened to ROOT-FIELD in the
                 * emitted expression and is not itself a top-level data
                 * item. Resolve that spelling through the declared object's
                 * concrete field metadata so String::data[index] receives
                 * the same pointer-view lowering as a char* parameter. */
                if (!is_character_pointer)
                {
                    size_t data_index;

                    data_index = unit->data_count;
                    while (data_index > 0 && !is_character_pointer)
                    {
                        const t_cblc_data_item *root_item =
                            &unit->data_items[--data_index];
                        const t_cblc_struct_type *root_type;
                        size_t field_index;

                        if (root_item->is_alias
                            || (root_item->kind != CBLC_DATA_KIND_STRUCT
                                && root_item->kind
                                    != CBLC_DATA_KIND_STRUCT_POINTER))
                            continue ;
                        root_type = cblc_find_struct_type(unit,
                            root_item->struct_type_name);
                        if (!root_type)
                            continue ;
                        field_index = 0;
                        while (field_index < root_type->field_count)
                        {
                            const t_cblc_struct_field *field =
                                &root_type->fields[field_index];
                            char flattened_name[TRANSPILE_IDENTIFIER_MAX];

                            if (field->kind == CBLC_DATA_KIND_CHAR_POINTER
                                && std::snprintf(flattened_name,
                                    sizeof(flattened_name), "%s-%s",
                                    root_item->cobol_name,
                                    field->cobol_name) >= 0
                                && std::strcmp(flattened_name, pointer_name) == 0)
                            {
                                is_character_pointer = 1;
                                break ;
                            }
                            field_index += 1;
                        }
                    }
                }
                if (!is_character_pointer)
                {
                    const char *field_suffix = std::strrchr(pointer_name, '-');

                    if (field_suffix && field_suffix[1] != '\0')
                    {
                        for (size_t type_index = 0;
                            type_index < unit->struct_type_count
                                && !is_character_pointer;
                            ++type_index)
                        {
                            const t_cblc_struct_type *candidate_type =
                                &unit->struct_types[type_index];

                            for (size_t field_index = 0;
                                field_index < candidate_type->field_count;
                                ++field_index)
                            {
                                const t_cblc_struct_field *field =
                                    &candidate_type->fields[field_index];

                                const size_t pointer_name_length =
                                    std::strlen(pointer_name);
                                const size_t field_name_length =
                                    std::strlen(field->cobol_name);

                                if (field->kind == CBLC_DATA_KIND_CHAR_POINTER
                                    && pointer_name_length > field_name_length
                                    && pointer_name[pointer_name_length
                                        - field_name_length - 1] == '-'
                                    && std::strcmp(pointer_name
                                        + pointer_name_length - field_name_length,
                                        field->cobol_name) == 0)
                                {
                                    is_character_pointer = 1;
                                    break ;
                                }
                            }
                        }
                    }
                }
                subscript_end = token_end + 1;
                while (expression[subscript_end] != '\0'
                    && expression[subscript_end] != ']')
                    subscript_end += 1;
                if (is_character_pointer
                    && expression[subscript_end] == ']'
                    && subscript_end > token_end + 1
                    && subscript_end - token_end - 1 < sizeof(subscript))
                {
                    char address_line[TRANSPILE_IDENTIFIER_MAX + 80];
                    const size_t subscript_length =
                        subscript_end - token_end - 1;

                    std::memcpy(subscript, expression + token_end + 1,
                        subscript_length);
                    subscript[subscript_length] = '\0';
                    if (std::snprintf(address_line, sizeof(address_line),
                            "           SET ADDRESS OF CBLC-PTR-CHAR-TABLE TO %s.",
                            pointer_name) < 0
                        || cobol_text_builder_append_line(builder,
                            address_line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(address_line, sizeof(address_line),
                            "           COMPUTE CBLC-PTR-OFFSET = (%s) + 1.",
                            subscript) < 0
                        || cobol_text_builder_append_line(builder,
                            address_line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    {
                        char lowered[TRANSPILE_STATEMENT_TEXT_MAX];
                        const int lowered_length = std::snprintf(lowered,
                            sizeof(lowered), "CBLC-PTR-CHAR-TABLE(CBLC-PTR-OFFSET:1)");

                        if (lowered_length < 0
                            || static_cast<size_t>(lowered_length)
                                >= sizeof(lowered)
                            || output_length + static_cast<size_t>(lowered_length)
                                >= sizeof(rewritten))
                            return (FT_FAILURE);
                        std::memcpy(rewritten + output_length, lowered,
                            static_cast<size_t>(lowered_length));
                        output_length += static_cast<size_t>(lowered_length);
                        rewritten[output_length] = '\0';
                    }
                    input_index = subscript_end + 1;
                    continue ;
                }
            }
        }
        if (expression[input_index] == '-' && expression[input_index + 1] == '>')
        {
            size_t reference_start;
            size_t reference_end;
            size_t pointer_end;
            char reference[TRANSPILE_STATEMENT_TEXT_MAX];
            char based_field[TRANSPILE_STATEMENT_TEXT_MAX];

            reference_start = input_index;
            while (reference_start > 0)
            {
                const char previous = expression[reference_start - 1];

                if (!std::isalnum(static_cast<unsigned char>(previous))
                    && previous != '_' && previous != '-')
                    break ;
                reference_start -= 1;
            }
            if (reference_start == input_index)
                return (FT_FAILURE);
            pointer_end = input_index;
            reference_end = input_index + 2;
            while (std::isspace(static_cast<unsigned char>(expression[reference_end])))
                reference_end += 1;
            while (std::isalnum(static_cast<unsigned char>(expression[reference_end]))
                || expression[reference_end] == '_'
                || expression[reference_end] == '-')
                reference_end += 1;
            if (reference_end == input_index + 2)
                return (FT_FAILURE);
            while (expression[reference_end] == '.')
            {
                size_t nested_start = reference_end + 1;

                while (std::isalnum(static_cast<unsigned char>(expression[nested_start]))
                    || expression[nested_start] == '_'
                    || expression[nested_start] == '-')
                    nested_start += 1;
                if (nested_start == reference_end + 1)
                    break ;
                reference_end = nested_start;
            }
            if (reference_end - reference_start >= sizeof(reference))
                return (FT_FAILURE);
            std::memcpy(reference, expression + reference_start,
                pointer_end - reference_start);
            reference[pointer_end - reference_start] = '\0';
            ft_strlcat(reference, "->", sizeof(reference));
            {
                size_t source_member = input_index + 2;
                size_t destination_member = std::strlen(reference);

                while (std::isspace(static_cast<unsigned char>(expression[source_member])))
                    source_member += 1;
                while (source_member < reference_end)
                    reference[destination_member++] = expression[source_member++];
                reference[destination_member] = '\0';
            }
            if (cblc_emit_prepare_struct_pointer_field(unit, reference, builder,
                    based_field, sizeof(based_field), 1) != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
                char pointer_name[TRANSPILE_IDENTIFIER_MAX];
                const char *arrow_position;
                const t_cblc_parameter *parameter;

                arrow_position = std::strstr(reference, "->");
                pointer_name[0] = '\0';
                if (arrow_position
                    && static_cast<size_t>(arrow_position - reference)
                        < sizeof(pointer_name))
                {
                    std::memcpy(pointer_name, reference,
                        static_cast<size_t>(arrow_position - reference));
                    pointer_name[arrow_position - reference] = '\0';
                }
                parameter = cblc_find_parameter_by_cobol_name(unit, pointer_name);

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "could not lower pointer member '%s' in condition '%.120s' (parameter=%s kind=%d type=%s)",
                    reference, expression, parameter ? "found" : "missing",
                    parameter ? static_cast<int>(parameter->kind) : -1,
                    parameter ? parameter->type_name : "<none>");
                cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                return (FT_FAILURE);
            }
            if (output_length < input_index - reference_start)
                return (FT_FAILURE);
            output_length -= input_index - reference_start;
            rewritten[output_length] = '\0';
            if (std::strlen(based_field) >= sizeof(rewritten) - output_length)
                return (FT_FAILURE);
            std::memcpy(rewritten + output_length, based_field,
                std::strlen(based_field));
            output_length += std::strlen(based_field);
            rewritten[output_length] = '\0';
            input_index = reference_end;
            continue ;
        }
        if (expression[input_index] == '-')
        {
            size_t arrow_end;

            arrow_end = input_index + 1;
            while (std::isspace(static_cast<unsigned char>(expression[arrow_end])))
                arrow_end += 1;
            if (expression[arrow_end] == '>')
            {
                size_t reference_start;
                size_t reference_end;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];
                char based_field[TRANSPILE_STATEMENT_TEXT_MAX];

                reference_start = input_index;
                while (reference_start > 0)
                {
                    const char previous = expression[reference_start - 1];

                    if (!std::isalnum(static_cast<unsigned char>(previous))
                        && previous != '_' && previous != '-')
                        break ;
                    reference_start -= 1;
                }
                reference_end = arrow_end + 1;
                while (std::isspace(static_cast<unsigned char>(expression[reference_end])))
                    reference_end += 1;
                while (std::isalnum(static_cast<unsigned char>(expression[reference_end]))
                    || expression[reference_end] == '_'
                    || expression[reference_end] == '-')
                    reference_end += 1;
                if (reference_start == input_index
                    || reference_end == arrow_end + 1
                    || reference_end - reference_start >= sizeof(reference))
                    return (FT_FAILURE);
                std::memcpy(reference, expression + reference_start,
                    input_index - reference_start);
                reference[input_index - reference_start] = '\0';
                ft_strlcat(reference, "->", sizeof(reference));
                {
                    size_t source_member = arrow_end + 1;
                    size_t destination_member = std::strlen(reference);

                    while (std::isspace(static_cast<unsigned char>(expression[source_member])))
                        source_member += 1;
                    while (source_member < reference_end)
                        reference[destination_member++] = expression[source_member++];
                    reference[destination_member] = '\0';
                }
                if (cblc_emit_prepare_struct_pointer_field(unit, reference, builder,
                        based_field, sizeof(based_field), 1) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
                    char pointer_name[TRANSPILE_IDENTIFIER_MAX];
                    const t_cblc_parameter *parameter;

                    if (input_index - reference_start >= sizeof(pointer_name))
                        return (FT_FAILURE);
                    std::memcpy(pointer_name, expression + reference_start,
                        input_index - reference_start);
                    pointer_name[input_index - reference_start] = '\0';
                    parameter = cblc_find_parameter_by_cobol_name(unit, pointer_name);

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "could not lower pointer member '%s' in condition '%.100s' (parameter=%s kind=%d type=%s)",
                        reference, expression, parameter ? "found" : "missing",
                        parameter ? static_cast<int>(parameter->kind) : -1,
                        parameter ? parameter->type_name : "<none>");
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
                if (output_length < input_index - reference_start)
                    return (FT_FAILURE);
                output_length -= input_index - reference_start;
                rewritten[output_length] = '\0';
                if (std::strlen(based_field) >= sizeof(rewritten) - output_length)
                    return (FT_FAILURE);
                std::memcpy(rewritten + output_length, based_field,
                    std::strlen(based_field));
                output_length += std::strlen(based_field);
                rewritten[output_length] = '\0';
                input_index = reference_end;
                continue ;
            }
        }
        if (output_length + 2 >= sizeof(rewritten))
            return (FT_FAILURE);
        rewritten[output_length++] = expression[input_index++];
        rewritten[output_length] = '\0';
    }
    if (builder->active_function_name && builder->active_function_name[0] != '\0')
    {
        char scoped[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t source_index;
        size_t scoped_length;

        source_index = 0;
        scoped_length = 0;
        scoped[0] = '\0';
        while (rewritten[source_index] != '\0')
        {
            if (rewritten[source_index] == '\'' || rewritten[source_index] == '"')
            {
                const char quote = rewritten[source_index];

                do
                {
                    if (scoped_length + 1 >= sizeof(scoped))
                        return (FT_FAILURE);
                    scoped[scoped_length++] = rewritten[source_index];
                    if (rewritten[source_index] == quote
                        && (source_index == 0 || rewritten[source_index - 1] != '\\'))
                    {
                        source_index += 1;
                        break ;
                    }
                    source_index += 1;
                } while (rewritten[source_index] != '\0');
                scoped[scoped_length] = '\0';
                continue ;
            }
            if ((rewritten[source_index] >= 'A' && rewritten[source_index] <= 'Z')
                || (rewritten[source_index] >= 'a' && rewritten[source_index] <= 'z')
                || rewritten[source_index] == '_')
            {
                /* Lowered COBOL references can be longer than source
                 * identifiers (for example, a based member path). */
                char token[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *replacement;
                size_t token_length;
                size_t scan;

                scan = source_index;
                token_length = 0;
                while ((rewritten[scan] >= 'A' && rewritten[scan] <= 'Z')
                    || (rewritten[scan] >= 'a' && rewritten[scan] <= 'z')
                    || (rewritten[scan] >= '0' && rewritten[scan] <= '9')
                    || rewritten[scan] == '_' || rewritten[scan] == '-')
                {
                    if (token_length + 1 >= sizeof(token))
                        return (FT_FAILURE);
                    token[token_length++] = rewritten[scan++];
                }
                token[token_length] = '\0';
                replacement = NULL;
                /* Prefer the lexical method/constructor storage slice over
                 * a translation-unit-wide function-name scan. Inlined class
                 * methods share one output program, so a stale active free-
                 * function name can otherwise bind a local `index` to an
                 * unrelated method (for example vector::at's index). */
                if (builder->active_method)
                {
                    const t_cblc_method *method = builder->active_method;
                    size_t parameter_index = 0;

                    while (parameter_index < method->parameter_count)
                    {
                        const t_cblc_parameter *parameter =
                            &method->parameters[parameter_index];

                        if (std::strcmp(parameter->source_name, token) == 0
                            || (parameter->actual_source_name[0] != '\0'
                                && std::strcmp(parameter->actual_source_name,
                                    token) == 0))
                        {
                            replacement = parameter->cobol_name;
                            break ;
                        }
                        parameter_index += 1;
                    }
                    if (!replacement)
                    {
                        size_t begin = method->template_storage_data_start;
                        size_t end = begin + method->template_storage_data_count;

                        if (method->template_storage_data_count == 0
                            || begin >= unit->data_count)
                        {
                            begin = 0;
                            end = unit->data_count;
                        }
                        else if (end < begin || end > unit->data_count)
                            end = unit->data_count;
                        for (size_t item_index = begin; item_index < end;
                            ++item_index)
                        {
                            const t_cblc_data_item *item =
                                &unit->data_items[item_index];

                            if (!item->is_function_local)
                                continue ;
                            if (method->scope_source_name[0] != '\0'
                                && std::strcmp(item->owner_function_name,
                                    method->scope_source_name) != 0)
                                continue ;
                            if (std::strcmp(item->source_name, token) == 0
                                || (item->local_source_name[0] != '\0'
                                    && std::strcmp(item->local_source_name,
                                        token) == 0))
                            {
                                replacement = item->cobol_name;
                                break ;
                            }
                        }
                    }
                }
                if (!replacement && builder->active_constructor)
                {
                    const t_cblc_constructor *constructor =
                        builder->active_constructor;

                    for (size_t parameter_index = 0;
                        parameter_index < constructor->parameter_count;
                        ++parameter_index)
                    {
                        const t_cblc_parameter *parameter =
                            &constructor->parameters[parameter_index];

                        if (std::strcmp(parameter->source_name, token) == 0
                            || (parameter->actual_source_name[0] != '\0'
                                && std::strcmp(parameter->actual_source_name,
                                    token) == 0))
                        {
                            replacement = parameter->cobol_name;
                            break ;
                        }
                    }
                }
                for (size_t function_index = 0;
                    !replacement && function_index < unit->function_count;
                    ++function_index)
                {
                    const t_cblc_function *function =
                        &unit->functions[function_index];

                    if (std::strcmp(function->source_name,
                            builder->active_function_name) != 0)
                        continue ;
                    for (size_t parameter_index = 0;
                        parameter_index < function->parameter_count;
                        ++parameter_index)
                    {
                        const t_cblc_parameter *parameter =
                            &function->parameters[parameter_index];

                        if (std::strcmp(parameter->source_name, token) == 0
                            || (parameter->actual_source_name[0] != '\0'
                                && std::strcmp(parameter->actual_source_name,
                                    token) == 0))
                        {
                            replacement = parameter->cobol_name;
                            break ;
                        }
                    }
                    break ;
                }
                if (!replacement)
                {
                    for (size_t item_index = 0; item_index < unit->data_count;
                        ++item_index)
                    {
                        const t_cblc_data_item *item =
                            &unit->data_items[item_index];

                        if (!item->is_function_local
                            || std::strcmp(item->owner_function_name,
                                builder->active_function_name) != 0)
                            continue ;
                        if (std::strcmp(item->source_name, token) == 0
                            || (item->local_source_name[0] != '\0'
                                && std::strcmp(item->local_source_name, token) == 0))
                        {
                            replacement = item->cobol_name;
                            break ;
                        }
                    }
                }
                if (replacement)
                {
                    const size_t replacement_length = std::strlen(replacement);

                    if (replacement_length >= sizeof(scoped) - scoped_length)
                        return (FT_FAILURE);
                    std::memcpy(scoped + scoped_length, replacement,
                        replacement_length);
                    scoped_length += replacement_length;
                }
                else
                {
                    if (token_length >= sizeof(scoped) - scoped_length)
                        return (FT_FAILURE);
                    std::memcpy(scoped + scoped_length, token, token_length);
                    scoped_length += token_length;
                }
                scoped[scoped_length] = '\0';
                source_index = scan;
                continue ;
            }
            if (scoped_length + 1 >= sizeof(scoped))
                return (FT_FAILURE);
            scoped[scoped_length++] = rewritten[source_index++];
            scoped[scoped_length] = '\0';
        }
        return (cblc_translate_expression_for_cobol(scoped, translated,
            translated_size));
    }
    return (cblc_translate_expression_for_cobol(rewritten, translated,
        translated_size));
}

static int cblc_emit_function(const t_cblc_translation_unit *unit, const t_cblc_function *function,
    t_cobol_text_builder *builder, int append_stop_run,
    const t_cblc_data_item *receiver_context,
    const t_cblc_parameter *entry_pointer_parameters,
    size_t entry_pointer_parameter_count,
    const t_cblc_struct_type *entry_pointer_parameter_owner)
{
    t_cblc_function paragraph_function;
    cblc::backend::typed_function typed_ir;
    cblc::backend::verification_diagnostic typed_ir_emit_diagnostic;
    const cblc::backend::typed_function *typed_ir_for_emit;
    cblc::backend::control_flow_graph typed_cfg;
    const cblc::backend::control_flow_graph *typed_cfg_for_emit;
    cblc::backend::lifecycle_cleanup_plan typed_cleanup_plan;
    const cblc::backend::lifecycle_cleanup_plan *typed_cleanup_for_emit;
    char line[256];
    size_t index;
    int exception_handler_depth;
    int control_flow_depth;
    int control_flow_stack[CBLC_EXCEPTION_REGION_MAX];
    int emit_return_exit;
    const char *saved_active_function_name;
    const t_cblc_statement *saved_active_statement;
    size_t saved_active_statement_index;
    const int trace_function_progress =
        std::getenv("CTOC_TRACE_COBOL_FUNCTIONS") != NULL;

    if (!unit || !function || !builder)
        return (FT_FAILURE);
    saved_active_function_name = builder->active_function_name;
    saved_active_statement = builder->active_statement;
    saved_active_statement_index = builder->active_statement_index;
    builder->active_function_name = function->source_name;
    builder->active_statement = NULL;
    builder->active_statement_index = static_cast<size_t>(-1);
    if (trace_function_progress)
        std::fprintf(stderr,
            "COBOL function begin: name=%s statements=%zu generated_bytes=%zu\n",
            function->source_name, function->statement_count, builder->length);
    if (!append_stop_run && !function->returns_to_paragraph
        && std::strncmp(function->source_name, "main", sizeof(function->source_name)) != 0)
    {
        paragraph_function = *function;
        paragraph_function.returns_to_paragraph = 1;
        if (cblc_function_return_paragraph_name(unit, function,
                paragraph_function.paragraph_return_label,
                sizeof(paragraph_function.paragraph_return_label)) != FT_SUCCESS)
            return (FT_FAILURE);
        function = &paragraph_function;
    }
    typed_ir_for_emit = NULL;
    typed_cfg_for_emit = NULL;
    typed_cleanup_for_emit = NULL;
    typed_ir_emit_diagnostic = cblc::backend::verification_diagnostic{};
    for (size_t function_index = 0; function_index < unit->function_count;
        ++function_index)
    {
        const t_cblc_function *candidate;
        cblc::backend::verification_diagnostic ir_diagnostic;
        int candidate_matches;

        candidate = &unit->functions[function_index];
        candidate_matches = std::strcmp(candidate->source_name,
            function->source_name) == 0
            && std::strcmp(candidate->cobol_name, function->cobol_name) == 0;
        if (!candidate_matches && function->is_exception_helper
            && std::strcmp(candidate->source_name, function->source_name) == 0)
        {
            char expected_helper_name[TRANSPILE_IDENTIFIER_MAX];
            char unused_return_name[TRANSPILE_IDENTIFIER_MAX];

            candidate_matches = cblc_exception_helper_names(candidate->cobol_name,
                    function->helper_exception_region_id, expected_helper_name,
                    sizeof(expected_helper_name), unused_return_name,
                    sizeof(unused_return_name)) == FT_SUCCESS
                && std::strcmp(expected_helper_name, function->cobol_name) == 0;
        }
        if (!candidate_matches)
            continue ;
        if (cblc::backend::lower_scalar_function(unit, function_index,
                &typed_ir, &ir_diagnostic)
            && cblc::backend::build_structured_control_flow_graph_with_cleanup(
                typed_ir, &typed_cfg, &typed_cleanup_plan, &ir_diagnostic,
                cblc_lifetime_policy_for_unit(unit)))
        {
            typed_ir_for_emit = &typed_ir;
            typed_cfg_for_emit = &typed_cfg;
            typed_cleanup_for_emit = &typed_cleanup_plan;
        }
        else
            typed_ir_emit_diagnostic = ir_diagnostic;
        break ;
    }
    if (std::snprintf(line, sizeof(line), "       %s.", function->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    /* Constructor pointer parameters are shared COBOL argument slots. Bind
     * their concrete BASED views only after entering the constructor
     * paragraph, so the binding executes for every constructor invocation
     * rather than as unrelated fall-through procedure text during startup. */
    if (entry_pointer_parameter_count > 0
        && cblc_emit_struct_pointer_parameter_bindings(unit,
            entry_pointer_parameters, entry_pointer_parameter_count, builder,
            entry_pointer_parameter_owner) != FT_SUCCESS)
        return (FT_FAILURE);
    /* A hosted executable owns this exception context. Initialize it before
     * injected global-constructor statements can transfer to the root handler. */
    if (append_stop_run
        && std::strcmp(function->source_name, "main") == 0
        && cblc_unit_has_exception_context(unit)
        && !cblc_is_external_entry_function(unit)
        && cobol_text_builder_append_line(builder,
            "           PERFORM CBLC-ROOT-INIT.") != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    exception_handler_depth = 0;
    control_flow_depth = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement;
        int skip_append;
        int typed_emission_status;

        statement = &function->statements[index];
        if (trace_function_progress)
            std::fprintf(stderr,
                "COBOL function statement begin: name=%s index=%zu kind=%d generated_bytes=%zu\n",
                function->source_name, index, static_cast<int>(statement->type),
                builder->length);
        builder->active_statement = statement;
        builder->active_statement_index = index;
        skip_append = 0;
        typed_emission_status = CBLC_TYPED_EMIT_UNSUPPORTED;
        emit_return_exit = 0;
        if (statement->type == CBLC_STATEMENT_TRY_BEGIN)
        {
            size_t try_end_index;
            char helper_name[TRANSPILE_IDENTIFIER_MAX];
            char return_name[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_exception_region_end(function, index, &try_end_index) != FT_SUCCESS
                || cblc_exception_helper_names(function->cobol_name,
                    statement->exception_region_id, helper_name, sizeof(helper_name),
                    return_name, sizeof(return_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line),
                    "           PERFORM %s THRU %s%s",
                    helper_name, return_name,
                    control_flow_depth == 0 && exception_handler_depth == 0
                        ? "." : "") < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder,
                    "           IF CBLC-EX-ACTIVE = 'Y'") != FT_SUCCESS)
                return (FT_FAILURE);
            if (statement->exception_has_parent_region)
            {
                if (std::snprintf(line, sizeof(line),
                        "               GO TO CBLC-TRY-%s-%zu-CATCH-0",
                        function->cobol_name,
                        statement->exception_parent_region_id) < 0)
                    return (FT_FAILURE);
            }
            else if (function->is_exception_helper
                || (std::strncmp(function->source_name, "main",
                        sizeof(function->source_name)) != 0))
            {
                if (function->paragraph_return_label[0] == '\0'
                    || std::snprintf(line, sizeof(line), "               GO TO %s",
                        function->paragraph_return_label) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                if (cobol_text_builder_append_line(builder,
                        "               GO TO CBLC-ROOT-UNCAUGHT") != FT_SUCCESS)
                    return (FT_FAILURE);
                line[0] = '\0';
            }
            if (line[0] != '\0'
                && cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder,
                    "           END-IF") != FT_SUCCESS)
                return (FT_FAILURE);
            index = try_end_index + 1;
            continue ;
        }
        if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];
            const char *target_open;

            typed_emission_status = typed_ir_for_emit
                ? cblc_emit_typed_copy_assignment(unit, function, statement,
                    typed_ir_for_emit, builder, index)
                : CBLC_TYPED_EMIT_UNSUPPORTED;
            if (typed_emission_status == FT_SUCCESS)
            {
                skip_append = 1;
            }
            else if (typed_emission_status == FT_FAILURE)
                return (FT_FAILURE);
            else if (cblc_emit_copy_assignment(unit, statement, builder,
                    function, index) == FT_SUCCESS)
            {
                skip_append = 1;
            }
            else if (typed_ir_for_emit
                && (typed_emission_status =
                    cblc_emit_typed_scalar_expression_assignment(unit,
                        function, statement, typed_ir_for_emit, builder))
                    == FT_SUCCESS)
            {
                skip_append = 1;
            }
            else if (typed_emission_status == FT_FAILURE)
                return (FT_FAILURE);
            else if (typed_ir_for_emit
                && (typed_emission_status =
                    cblc_emit_typed_scalar_literal_assignment(unit, function,
                        statement, typed_ir_for_emit, builder)) == FT_SUCCESS)
            {
                skip_append = 1;
            }
            else if (typed_emission_status == FT_FAILURE)
                return (FT_FAILURE);
            else if ((target_open = std::strchr(statement->target, '[')) != NULL)
            {
                char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t pointer_name_length;
                t_cblc_data_item synthetic_pointer;
                const t_cblc_data_item *pointer_item;

                pointer_name_length = static_cast<size_t>(target_open - statement->target);
                if (pointer_name_length == 0
                    || pointer_name_length >= sizeof(pointer_name))
                    return (FT_FAILURE);
                std::memcpy(pointer_name, statement->target, pointer_name_length);
                pointer_name[pointer_name_length] = '\0';
                pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_name,
                    &synthetic_pointer, receiver_context);
                if (pointer_item
                    && pointer_item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
                {
                    if (cblc_emit_struct_pointer_element_copy_assignment(unit,
                            statement, builder, receiver_context) != FT_SUCCESS)
                        return (FT_FAILURE);
                    skip_append = 1;
                }
                else if (cblc_emit_pointer_assignment_statement(unit, statement,
                        builder, line, sizeof(line), receiver_context,
                        function) == FT_SUCCESS)
                    skip_append = 0;
                else
                    skip_append = 0;
            }
            else if (cblc_emit_pointer_assignment_statement(unit, statement, builder,
                    line, sizeof(line), receiver_context, function) == FT_SUCCESS)
            {
                skip_append = 0;
            }
            else
            {
            if (statement->is_literal)
                ft_strlcpy(source_text, statement->source, sizeof(source_text));
            else if (cblc_translate_condition_for_cobol(unit, statement->source,
                        builder, source_text, sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_condition_for_cobol(unit, statement->target,
                    builder, target_text, sizeof(target_text)) != FT_SUCCESS)
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
                            || cobol_text_builder_append_string(builder, "           ") != FT_SUCCESS
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
                        || std::snprintf(line, sizeof(line), "           %s", to_clause + 1) < 0)
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
            if (item && item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
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
            else if (std::strncmp(statement->source, "CBLC-EX-MESSAGE",
                    sizeof(statement->source)) == 0)
            {
                if (std::snprintf(line, sizeof(line),
                        "           DISPLAY CBLC-EX-MESSAGE-BUF(1:CBLC-EX-MESSAGE-LEN)") < 0)
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

                if (cblc_translate_condition_for_cobol(unit, statement->source,
                        builder, condition_text, sizeof(condition_text)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           IF %s",
                        condition_text) < 0)
                    return (FT_FAILURE);
                if (control_flow_depth >= CBLC_EXCEPTION_REGION_MAX)
                    return (FT_FAILURE);
                control_flow_stack[control_flow_depth] = CBLC_STATEMENT_IF_BEGIN;
                control_flow_depth += 1;
                cobol_text_builder_set_control_flow_depth(builder,
                    control_flow_depth);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "               CONTINUE") != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
            else if (statement->type == CBLC_STATEMENT_IF_ELSE)
            {
                if (control_flow_depth == 0
                    || control_flow_stack[control_flow_depth - 1]
                        != CBLC_STATEMENT_IF_BEGIN)
                    return (FT_FAILURE);
                cobol_text_builder_set_control_flow_depth(builder,
                    control_flow_depth);
                ft_strlcpy(line, "           ELSE", sizeof(line));
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "               CONTINUE") != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
            else
            {
                if (control_flow_depth == 0
                    || control_flow_stack[control_flow_depth - 1]
                        != CBLC_STATEMENT_IF_BEGIN)
                    return (FT_FAILURE);
                control_flow_depth -= 1;
                cobol_text_builder_set_control_flow_depth(builder,
                    control_flow_depth);
                ft_strlcpy(line, "           END-IF", sizeof(line));
                if (control_flow_depth == 0)
                    ft_strlcat(line, ".", sizeof(line));
            }
            if (!skip_append
                && cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            /* IF_END is appended in this control-flow branch because its
             * period depends on the updated nesting depth. Do not append it
             * a second time in the shared statement epilogue below. */
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

                    if (cblc_translate_condition_for_cobol(unit, statement->source,
                            builder, condition_text, sizeof(condition_text)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           PERFORM UNTIL NOT (%s)", condition_text) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line),
                        "           PERFORM UNTIL CBLC-EOF-FLAG = 'Y'") < 0)
                    return (FT_FAILURE);
                if (control_flow_depth >= CBLC_EXCEPTION_REGION_MAX)
                    return (FT_FAILURE);
                control_flow_stack[control_flow_depth] = CBLC_STATEMENT_WHILE_BEGIN;
                control_flow_depth += 1;
                cobol_text_builder_set_control_flow_depth(builder,
                    control_flow_depth);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (control_flow_depth == 0
                    || control_flow_stack[control_flow_depth - 1]
                        != CBLC_STATEMENT_WHILE_BEGIN)
                    return (FT_FAILURE);
                control_flow_depth -= 1;
                cobol_text_builder_set_control_flow_depth(builder,
                    control_flow_depth);
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
                if (catch_item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
                {
                    char buffer_name[TRANSPILE_IDENTIFIER_MAX];

                    cblc_build_string_component_name(catch_item->cobol_name, "-BUF",
                            buffer_name, sizeof(buffer_name));
                    if (std::snprintf(line, sizeof(line),
                            "               MOVE CBLC-EX-MESSAGE-BUF TO %s",
                            buffer_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    cblc_build_string_component_name(catch_item->cobol_name, "-LEN",
                            buffer_name, sizeof(buffer_name));
                    if (std::snprintf(line, sizeof(line),
                            "               MOVE CBLC-EX-MESSAGE-LEN TO %s",
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
                    "           IF CBLC-EX-MATCHED = 'Y'") != FT_SUCCESS
                /* A matched exception is handled, not still propagating.
                 * Keep its payload intact for the catch binding, but clear
                 * ACTIVE before lowering the handler so normal operations in
                 * the handler are not mistaken for a second exception. */
                || cobol_text_builder_append_line(builder,
                    "               MOVE 'N' TO CBLC-EX-ACTIVE") != FT_SUCCESS)
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
                || cblc_emit_exception_context_dispose(builder) != FT_SUCCESS
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
                    "           IF CBLC-EX-ACTIVE = 'Y'") != FT_SUCCESS)
                return (FT_FAILURE);
            if (function->is_exception_helper)
            {
                if (function->paragraph_return_label[0] == '\0'
                    || std::snprintf(line, sizeof(line), "               GO TO %s",
                        function->paragraph_return_label) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (cobol_text_builder_append_line(builder,
                        "               DISPLAY \"Unhandled CBL-C exception\"") != FT_SUCCESS
                || cobol_text_builder_append_line(builder,
                    std::strncmp(function->source_name, "main",
                        sizeof(function->source_name)) == 0
                        ? "               STOP RUN" : "               GOBACK") != FT_SUCCESS)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
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

            const cblc::backend::typed_operation *typed_throw_operation = NULL;
            const cblc::backend::cleanup_edge *typed_throw_cleanup =
                typed_ir_for_emit && typed_cleanup_for_emit
                ? cblc_find_typed_throw_cleanup_edge(statement,
                    typed_ir_for_emit, typed_cleanup_for_emit,
                    &typed_throw_operation)
                : NULL;
            const bool emit_typed_cleanup = typed_throw_cleanup
                && cblc_typed_throw_cleanup_is_emittable(unit, function,
                    typed_ir_for_emit, typed_throw_cleanup);

            if (emit_typed_cleanup && !typed_throw_cleanup->destruction_order.empty())
            {
                char cleanup_paragraph[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_build_typed_throw_cleanup_name(unit, function,
                        typed_ir_for_emit, typed_cleanup_for_emit,
                        typed_throw_operation, typed_throw_cleanup,
                        cleanup_paragraph,
                        sizeof(cleanup_paragraph)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           PERFORM %s%s",
                        cleanup_paragraph, keep_exception_sentence ? "" : ".") < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (!emit_typed_cleanup
                && statement->exception_cleanup_count > 0)
            {
                char cleanup_paragraph[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_build_exception_cleanup_paragraph_name(unit, function, index,
                        cleanup_paragraph, sizeof(cleanup_paragraph)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           PERFORM %s%s",
                        cleanup_paragraph, keep_exception_sentence ? "" : ".") < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }

            if (statement->exception_is_rethrow)
            {
                /* CATCH-BEGIN marks the current exception inactive while its
                 * handler runs. A bare `throw;` resumes propagation of that
                 * same payload, so reactivate it before transferring control
                 * to the enclosing handler/TRY dispatcher. */
                if (cobol_text_builder_append_line(builder,
                        "           MOVE 'Y' TO CBLC-EX-ACTIVE") != FT_SUCCESS)
                    return (FT_FAILURE);
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
                        || cblc_emit_exception_context_dispose(builder) != FT_SUCCESS
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
                else if (statement->exception_payload_is_message)
                {
                    if (statement->is_literal)
                    {
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-MESSAGE-BUF",
                                statement->source)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-MESSAGE-BUF.",
                                statement->source)) < 0)
                            return (FT_FAILURE);
                        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-MESSAGE-LEN",
                                std::strlen(statement->source) >= 2
                                    ? std::strlen(statement->source) - 2 : 0)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %zu TO CBLC-EX-MESSAGE-LEN.",
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
                                source_length, CBLC_EXCEPTION_MESSAGE_MAX) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                            || std::snprintf(line, sizeof(line),
                                "               MOVE %zu TO CBLC-EX-SECONDARY-TYPE-ID",
                                cblc_exception_type_id("exception_message")) < 0
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
                                "           MOVE %s TO CBLC-EX-MESSAGE-BUF", source_buffer)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-MESSAGE-BUF.", source_buffer)) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                        if ((keep_exception_sentence
                            ? std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-MESSAGE-LEN", source_length)
                            : std::snprintf(line, sizeof(line),
                                "           MOVE %s TO CBLC-EX-MESSAGE-LEN.", source_length)) < 0
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
                    if (function->returns_to_paragraph)
                    {
                        if (function->paragraph_return_label[0] == '\0'
                            || std::snprintf(line, sizeof(line),
                                "           GO TO %s%s",
                                function->paragraph_return_label,
                                keep_exception_sentence ? "" : ".") < 0)
                            return (FT_FAILURE);
                    }
                    else if (std::strncmp(function->source_name, "main",
                            sizeof(function->source_name)) == 0)
                        ft_strlcpy(line, keep_exception_sentence
                            ? "           STOP RUN" : "           STOP RUN.", sizeof(line));
                    else if (!function->returns_to_paragraph)
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
            char numeric_payload_cobol_name[TRANSPILE_IDENTIFIER_MAX];
            char numeric_payload_field_name[TRANSPILE_IDENTIFIER_MAX];
            size_t numeric_exception_type_id;
            size_t numeric_exception_payload_size;
            size_t enclosing_region_id;
            const char *numeric_exception_type_name;
            const t_cblc_data_item *numeric_target_item;
            const t_cblc_struct_type *numeric_exception_type;
            const t_cblc_struct_field *numeric_exception_code_field;
            const t_cblc_data_item *numeric_exception_payload_item;
            t_cblc_statement transfer_statement;
            int has_enclosing_region;
            int typed_compute_status;

            /* Exception-helper functions contain a slice of the original
             * function's statements and therefore have no TRY_BEGIN marker
             * to rediscover here. Preserve the parser-recorded region on
             * the statement so arithmetic failures inside that helper still
             * enter the original catch dispatch. */
            has_enclosing_region = statement->exception_has_region;
            if (has_enclosing_region)
                enclosing_region_id = statement->exception_region_id;
            else
                has_enclosing_region = cblc_find_enclosing_exception_region(
                    function, index, &enclosing_region_id) == FT_SUCCESS;
            /* Typed scalar lowering currently treats indexed char-pointer
             * fields as COBOL reference modification on the pointer item
             * itself. That compiles but reads unrelated storage. Route any
             * indexed expression through the expression emitter, which
             * resolves char-pointer fields to the shared BASED byte view and
             * computes the pointer offset at the statement's execution point. */
            typed_compute_status = typed_ir_for_emit
                    && !std::strchr(statement->source, '[')
                ? cblc_emit_typed_scalar_literal_assignment(unit, function,
                statement, typed_ir_for_emit, builder) : CBLC_TYPED_EMIT_UNSUPPORTED;

            if (typed_compute_status == FT_SUCCESS)
            {
                skip_append = 1;
            }
            else if (typed_compute_status == FT_FAILURE)
                return (FT_FAILURE);
            else if (cblc_emit_pointer_compute_statement(unit, statement, builder,
                    line, sizeof(line)) == FT_SUCCESS)
            {
                skip_append = 0;
            }
            else
            {
            if (cblc_translate_condition_for_cobol(unit, statement->source,
                    builder, source_text, sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_condition_for_cobol(unit, statement->target,
                    builder, target_text, sizeof(target_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                    target_text, source_text) < 0)
                return (FT_FAILURE);
            if (has_enclosing_region)
            {
                transfer_statement = *statement;
                transfer_statement.exception_has_region = 1;
                transfer_statement.exception_region_id = enclosing_region_id;
                numeric_target_item = cblc_find_data_item_by_cobol(unit,
                    target_text);
                numeric_exception_type_name = numeric_target_item
                    && numeric_target_item->kind == CBLC_DATA_KIND_DOUBLE
                    ? "range_error" : "overflow_error";
                numeric_exception_type_id = cblc_exception_type_id(
                    numeric_exception_type_name);
                numeric_exception_type = cblc_find_struct_type(unit,
                    numeric_exception_type_name);
                numeric_exception_code_field = NULL;
                numeric_exception_payload_item = NULL;
                numeric_exception_payload_size = 0;
                numeric_payload_cobol_name[0] = '\0';
                numeric_payload_field_name[0] = '\0';
                if (numeric_exception_type)
                {
                    size_t field_index;

                    field_index = 0;
                    while (field_index < numeric_exception_type->field_count)
                    {
                        if (std::strcmp(numeric_exception_type->fields[field_index].source_name,
                                "code") == 0
                            && numeric_exception_type->fields[field_index].kind
                                == CBLC_DATA_KIND_INT)
                        {
                            numeric_exception_code_field
                                = &numeric_exception_type->fields[field_index];
                            break ;
                        }
                        field_index += 1;
                    }
                    if (!numeric_exception_code_field
                        || cblc_exception_payload_name(numeric_exception_type_name,
                            numeric_payload_cobol_name,
                            sizeof(numeric_payload_cobol_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    numeric_exception_payload_item = cblc_find_data_item_by_cobol(unit,
                        numeric_payload_cobol_name);
                    if (numeric_exception_payload_item
                        && std::snprintf(numeric_payload_field_name,
                            sizeof(numeric_payload_field_name), "%s-%s",
                            numeric_exception_payload_item->cobol_name,
                            numeric_exception_code_field->cobol_name) < 0)
                        return (FT_FAILURE);
                    if (numeric_exception_payload_item)
                    {
                        numeric_exception_payload_size = cblc_exception_payload_size(
                            unit, numeric_exception_type, 0);
                        if (numeric_exception_payload_size == 0
                            || numeric_exception_payload_size > CBLC_EXCEPTION_PAYLOAD_MAX)
                            return (FT_FAILURE);
                    }
                }
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
                    || (numeric_exception_payload_item
                        && (std::snprintf(line, sizeof(line), "                   MOVE 1 TO %s",
                                numeric_payload_field_name) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS))
                    || (std::snprintf(line, sizeof(line),
                            "                   MOVE %zu TO CBLC-EX-PAYLOAD-SIZE",
                            numeric_exception_payload_size) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    || std::snprintf(line, sizeof(line),
                        "                   MOVE %zu TO CBLC-EX-TYPE-ID",
                        numeric_exception_type_id) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                   MOVE %zu TO CBLC-EX-DYNAMIC-TYPE-ID",
                        numeric_exception_type_id) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, numeric_exception_payload_item
                        ? "                   MOVE 'Y' TO CBLC-EX-PAYLOAD-OWNER"
                        : "                   MOVE 'N' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
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
            if (typed_ir_for_emit && !statement->call_is_external)
            {
                target_function = cblc_find_function_in_unit(unit,
                    statement->call_identifier);
                if (target_function
                    && cblc_validate_typed_direct_call(unit, statement,
                        typed_ir_for_emit) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
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
                        || std::snprintf(line, sizeof(line),
                            "           IF %s NOT = NULL", translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               FREE %s",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "               SET %s TO NULL.",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           END-IF.") != FT_SUCCESS)
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
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s NOT = NULL", translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cblc_emit_struct_pointer_lifecycle(unit, pointer_item,
                            &destructor_statement, builder) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               FREE %s",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "               SET %s TO NULL.",
                            translated_argument) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           END-IF.") != FT_SUCCESS)
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
                if (std::strncmp(target_function->source_name, "main",
                        sizeof(target_function->source_name)) != 0)
                {
                    char return_label[TRANSPILE_IDENTIFIER_MAX];

                    if (cblc_function_return_paragraph_name(unit, target_function,
                            return_label, sizeof(return_label)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           PERFORM %s THRU %s", cobol_name,
                            return_label) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line), "           PERFORM %s",
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
                }
            }
        }
        else if (statement->type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_function *target_function;

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
            if (typed_ir_for_emit && !statement->call_is_external)
            {
                target_function = cblc_find_function_in_unit(unit,
                    statement->call_identifier);
                if (target_function
                    && cblc_validate_typed_direct_call(unit, statement,
                        typed_ir_for_emit) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
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
                    char referenced_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    char translated_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                    t_cblc_method constructor_scope_context;
                    const t_cblc_method *scope_context;
                    const t_cblc_data_item *receiver_item;
                    char receiver_name[TRANSPILE_IDENTIFIER_MAX];
                    const char *field_suffix;
                    size_t parameter_index;

                    argument[0] = '\0';
                    mapped_argument[0] = '\0';
                    referenced_argument[0] = '\0';
                    translated_argument[0] = '\0';
                    scope_context = builder->active_method;
                    if (!scope_context && builder->active_constructor)
                    {
                        std::memset(&constructor_scope_context, 0,
                            sizeof(constructor_scope_context));
                        constructor_scope_context.parameter_count =
                            builder->active_constructor->parameter_count;
                        std::memcpy(constructor_scope_context.parameters,
                            builder->active_constructor->parameters,
                            sizeof(constructor_scope_context.parameters));
                        constructor_scope_context.template_storage_data_start =
                            builder->active_constructor->template_storage_data_start;
                        constructor_scope_context.template_storage_data_count =
                            builder->active_constructor->template_storage_data_count;
                        ft_strlcpy(constructor_scope_context.scope_source_name,
                            builder->active_constructor->scope_source_name,
                            sizeof(constructor_scope_context.scope_source_name));
                        scope_context = &constructor_scope_context;
                    }
                    /* CBLC-NEW-ARRAY also backs char* members (not only
                     * struct-pointer fields), so do not require this target
                     * to resolve through the struct-pointer helper. */
                    if (statement->call_argument_count != 1
                        || cblc_extract_call_argument(statement, 0, argument,
                            sizeof(argument)) != FT_SUCCESS
                        || ft_strlcpy(mapped_argument, argument,
                            sizeof(mapped_argument)) >= sizeof(mapped_argument)
                        || std::snprintf(receiver_name, sizeof(receiver_name), "%s",
                            statement->target) < 0)
                    {
                        return (FT_FAILURE);
                    }
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
                    parameter_index = 0;
                    while (parameter_index < function->parameter_count)
                    {
                        const t_cblc_parameter *parameter;

                        parameter = &function->parameters[parameter_index];
                        if (parameter->cobol_name[0] != '\0'
                            && parameter->source_name[0] != '\0')
                            cblc_replace_template_text(mapped_argument,
                                sizeof(mapped_argument), parameter->cobol_name,
                                parameter->source_name);
                        parameter_index += 1;
                    }
                    if ((scope_context
                            ? cblc_rewrite_scoped_method_data_references(unit,
                                scope_context, mapped_argument, NULL,
                                referenced_argument,
                                sizeof(referenced_argument))
                            : cblc_rewrite_method_data_references(unit,
                                mapped_argument, NULL, referenced_argument,
                                sizeof(referenced_argument))) != FT_SUCCESS
                        || cblc_translate_expression_for_cobol(referenced_argument,
                            translated_argument, sizeof(translated_argument)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           ALLOCATE %s CHARACTERS RETURNING %s",
                            translated_argument, statement->target) < 0
                        || cblc_emit_allocation_exception(unit, function, statement,
                            index, line, statement->target, "bad_alloc",
                            builder) != FT_SUCCESS)
                    {
                        return (FT_FAILURE);
                    }
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
                    if (cblc_emit_allocation_exception(unit, function, statement,
                            index, line, statement->target, "bad_alloc",
                            builder) != FT_SUCCESS
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
                    /* Exception-helper functions contain sliced statements
                     * without their original TRY_BEGIN marker. In that case
                     * the parser-recorded region on the allocation statement
                     * is authoritative, just as it is for COMPUTE lowering. */
                    allocation_has_exception_region =
                        statement->exception_has_region;
                    if (allocation_has_exception_region)
                        allocation_region_id = statement->exception_region_id;
                    else
                        allocation_has_exception_region =
                            cblc_find_enclosing_exception_region(function, index,
                                &allocation_region_id) == FT_SUCCESS;
                    if (allocation_has_exception_region)
                    {
                        if (cblc_emit_allocation_exception(unit, function, statement, index,
                                line, statement->target, "int", builder) != FT_SUCCESS)
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
                if (std::strncmp(target_function->source_name, "main",
                        sizeof(target_function->source_name)) != 0)
                {
                    char return_label[TRANSPILE_IDENTIFIER_MAX];

                    if (cblc_function_return_paragraph_name(unit, target_function,
                            return_label, sizeof(return_label)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "           PERFORM %s THRU %s", cobol_name,
                            return_label) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line), "           PERFORM %s",
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
            int typed_return_status;

            emit_return_exit = 1;
            typed_return_status = typed_ir_for_emit
                ? cblc_prepare_typed_scalar_return(unit, function,
                    statement, typed_ir_for_emit, line, sizeof(line))
                : CBLC_TYPED_EMIT_UNSUPPORTED;
            if (typed_return_status == FT_SUCCESS)
            {
                skip_append = 0;
            }
            else if (typed_return_status == FT_FAILURE)
                return (FT_FAILURE);
            else if (statement->target[0] == '\0' || statement->source[0] == '\0')
            {
                skip_append = 1;
            }
            else
            {
                const t_cblc_data_item *target_item;

                target_item = cblc_find_data_item_by_cobol(unit, statement->target);
                if ((target_item && cblc_data_kind_is_pointer(target_item->kind))
                    || function->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
                    || function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
                    || function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
                    || function->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
                {
                    const t_cblc_data_item *source_item;
                    char based_return_name[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (statement->source[0] == '&')
                    {
                        if (std::snprintf(line, sizeof(line),
                                "           SET %s TO ADDRESS OF %s",
                                statement->target, statement->source + 1) < 0)
                            return (FT_FAILURE);
                    }
                    else if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER
                        && cblc_emit_prepare_struct_pointer_value(unit,
                            statement->source, builder, based_return_name,
                            sizeof(based_return_name), receiver_context) == FT_SUCCESS)
                    {
                        if (std::snprintf(line, sizeof(line),
                                "           SET %s TO ADDRESS OF %s",
                                statement->target, based_return_name) < 0)
                            return (FT_FAILURE);
                    }
                    else if (cblc_emit_prepare_pointer_element(unit, statement->source, builder,
                            based_return_name, sizeof(based_return_name), receiver_context)
                        == FT_SUCCESS)
                    {
                        if (std::snprintf(line, sizeof(line), "           SET %s TO ADDRESS OF %s",
                                statement->target, based_return_name) < 0)
                            return (FT_FAILURE);
                    }
                    else
                    {
                        source_item = cblc_find_data_item_by_cobol(unit, statement->source);
                        if (!source_item)
                            source_item = cblc_find_data_item_for_emit_by_source(unit,
                                statement->source);
                        if (source_item && (source_item->kind == CBLC_DATA_KIND_CHAR_POINTER
                                || !cblc_data_kind_is_pointer(source_item->kind)))
                        {
                            if (std::snprintf(line, sizeof(line),
                                    "           SET %s TO ADDRESS OF %s",
                                    statement->target, statement->source) < 0)
                                return (FT_FAILURE);
                        }
                        else if (!source_item && !std::strchr(statement->source, '[')
                            && !std::strstr(statement->source, "->"))
                        {
                            if (std::snprintf(line, sizeof(line),
                                    "           SET %s TO ADDRESS OF %s",
                                    statement->target, statement->source) < 0)
                                return (FT_FAILURE);
                        }
                        else if (cblc_emit_pointer_assignment_statement(unit, statement, builder,
                                line, sizeof(line), receiver_context, function) != FT_SUCCESS)
                        {
                            if (std::snprintf(line, sizeof(line), "           SET %s TO %s",
                                    statement->target, statement->source) < 0)
                                return (FT_FAILURE);
                        }
                    }
                }
                else
                {
                    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
                    {
                        int copy_return_handled;

                        if (cblc_emit_struct_copy_return(unit, statement->target,
                                function->return_type_name, statement->source,
                                receiver_context, builder,
                                &copy_return_handled) != FT_SUCCESS)
                        {
                            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                            std::snprintf(diagnostic, sizeof(diagnostic),
                                "failed to copy struct return from '%s' to '%s' (type '%s')",
                                statement->source, statement->target,
                                function->return_type_name);
                            cobol_text_builder_set_diagnostic(builder, 0,
                                diagnostic);
                            return (FT_FAILURE);
                        }
                        if (copy_return_handled)
                            skip_append = 1;
                        else if (std::snprintf(line, sizeof(line),
                                "           MOVE %s TO %s", statement->source,
                                statement->target) < 0)
                            return (FT_FAILURE);
                    }
                    else
                    {
                        char prepared_source[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (cblc_emit_prepare_pointer_element(unit, statement->source,
                                builder, prepared_source, sizeof(prepared_source),
                                receiver_context) == FT_SUCCESS)
                        {
                            if (std::snprintf(line, sizeof(line),
                                    "           MOVE %s TO %s", prepared_source,
                                    statement->target) < 0)
                                return (FT_FAILURE);
                        }
                        else if (std::snprintf(line, sizeof(line),
                                "           MOVE %s TO %s", statement->source,
                                statement->target) < 0)
                            return (FT_FAILURE);
                    }
                }
            }
        }
        else if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->is_move_construct || statement->is_relocate_construct
            || statement->type == CBLC_STATEMENT_DESTRUCT)
        {
            int lifecycle_status;

            if (statement->type == CBLC_STATEMENT_DESTRUCT
                && statement->lifecycle_completion_flag[0] != '\0'
                && statement->lifecycle_cleanup_paragraph[0] != '\0')
            {
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        statement->lifecycle_cleanup_paragraph) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                skip_append = 1;
            }
            else
            {
            if ((statement->is_move_construct
                    || statement->is_relocate_construct)
                && !typed_ir_for_emit && receiver_context)
            {
                cblc::backend::typed_function method_transfer_ir;
                cblc::backend::control_flow_graph method_transfer_cfg;
                cblc::backend::lifecycle_cleanup_plan method_transfer_cleanup;

                if (cblc_method_transfer_is_scalar_pointer(unit,
                        receiver_context, statement))
                    lifecycle_status = cblc_emit_lifecycle_statement(unit,
                        statement, builder, function, index, receiver_context);
                else if (cblc_lower_method_transfer_statement(unit, receiver_context,
                        function, statement, &method_transfer_ir,
                        &method_transfer_cfg, &method_transfer_cleanup,
                        builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                else
                    lifecycle_status = cblc_emit_typed_transfer_construct(unit,
                        function, statement, index, &method_transfer_ir,
                        &method_transfer_cfg, builder, receiver_context);
                if (lifecycle_status != FT_SUCCESS)
                {
                    cblc_set_method_exception_lowering_diagnostic(builder,
                        receiver_context, cblc_find_method_on_type(
                            cblc_find_struct_type(unit,
                                receiver_context->struct_type_name),
                            function->source_name),
                        "typed lifecycle transfer emission",
                        index);
                    return (FT_FAILURE);
                }
                skip_append = 1;
                ++index;
                continue;
            }
            lifecycle_status = (statement->is_move_construct
                    || statement->is_relocate_construct)
                    && typed_ir_for_emit && typed_cfg_for_emit
                ? cblc_emit_typed_transfer_construct(unit, function, statement,
                    index, typed_ir_for_emit, typed_cfg_for_emit, builder,
                    receiver_context)
                : statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                    && typed_ir_for_emit && typed_cfg_for_emit
                ? cblc_emit_typed_construct(unit, function, statement, index,
                    typed_ir_for_emit, typed_cfg_for_emit, builder,
                    receiver_context)
                : (statement->type == CBLC_STATEMENT_DESTRUCT
                    && typed_ir_for_emit && typed_cfg_for_emit
                    && typed_cleanup_for_emit
                ? cblc_emit_typed_scope_exit(unit, function, statement, index,
                    typed_ir_for_emit, typed_cfg_for_emit, typed_cleanup_for_emit,
                    builder, receiver_context)
                : CBLC_TYPED_EMIT_UNSUPPORTED);
            if (lifecycle_status == CBLC_TYPED_EMIT_UNSUPPORTED
                && !((statement->is_move_construct
                        || statement->is_relocate_construct)))
                lifecycle_status = cblc_emit_lifecycle_statement(unit, statement,
                    builder, function, index, receiver_context);
            if (lifecycle_status != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "lifecycle dispatch failed for '%s' (kind=%d, offset=%zu, arguments='%s', count=%zu, status=%d)",
                    statement->target, static_cast<int>(statement->type),
                    statement->source_offset, statement->call_arguments,
                    statement->call_argument_count, lifecycle_status);
                cobol_text_builder_set_diagnostic(builder,
                    TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                return (FT_FAILURE);
            }
            if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                && statement->lifecycle_completion_flag[0] != '\0')
            {
                if (std::snprintf(line, sizeof(line),
                        "           MOVE 'Y' TO %s",
                        statement->lifecycle_completion_flag) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            skip_append = 1;
            }
        }
        else if (statement->type == CBLC_STATEMENT_METHOD_CALL
            || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            int method_status;

            method_status = FT_FAILURE;
            if (receiver_context && statement->type == CBLC_STATEMENT_METHOD_CALL
                && std::strcmp(statement->target, "CBLC-THIS") == 0)
            {
                const t_cblc_struct_type *receiver_type;
                const t_cblc_method *self_method;

                receiver_type = cblc_find_struct_type(unit,
                    receiver_context->struct_type_name);
                self_method = cblc_find_method_on_type(receiver_type,
                    statement->call_identifier);
                if (self_method)
                {
                    method_status = cblc_method_may_throw(unit, receiver_type,
                            self_method, 0)
                        ? cblc_emit_method_exception_body(unit, receiver_context,
                            self_method, "", builder, 1)
                        : cblc_emit_method_body(unit, receiver_context,
                            self_method, NULL, builder);
                }
            }
            else
            {
                method_status = typed_ir_for_emit
                    ? cblc_emit_typed_method_call(unit, function, statement,
                        index, typed_ir_for_emit, builder)
                    : CBLC_TYPED_EMIT_UNSUPPORTED;
                if (method_status == CBLC_TYPED_EMIT_UNSUPPORTED)
                    method_status = cblc_emit_method_statement(unit, statement,
                        builder, function, index, receiver_context);
            }
            if (method_status != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                if (builder->diagnostic_message[0] == '\0')
                {
                    if (!typed_ir_for_emit
                        && typed_ir_emit_diagnostic.code
                            != cblc::backend::verification_error::none)
                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "typed function lowering stopped at: %.230s",
                            typed_ir_emit_diagnostic.message.c_str());
                    else if (builder->active_constructor && receiver_context)
                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed lowering method call '%s' on '%s' at source offset %zu while emitting constructor '%s' for receiver '%s' (%s)",
                            statement->call_identifier,
                            statement->type == CBLC_STATEMENT_METHOD_CALL
                                ? statement->target : statement->source,
                            statement->source_offset,
                            builder->active_constructor->scope_source_name,
                            receiver_context->cobol_name,
                            receiver_context->struct_type_name);
                    else if (builder->active_method && receiver_context)
                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed lowering method call '%s' on '%s' at source offset %zu while emitting method '%s'",
                            statement->call_identifier,
                            statement->type == CBLC_STATEMENT_METHOD_CALL
                                ? statement->target : statement->source,
                            statement->source_offset,
                            builder->active_method->scope_source_name);
                    else
                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed lowering method call '%s' on '%s' at source offset %zu",
                            statement->call_identifier,
                            statement->type == CBLC_STATEMENT_METHOD_CALL
                                ? statement->target : statement->source,
                            statement->source_offset);
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                }
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
                && cblc_emit_exception_context_dispose(builder) != FT_SUCCESS)
                return (FT_FAILURE);
            if (function->returns_to_paragraph)
            {
                if (function->is_exception_helper)
                {
                    if (function->helper_parent_return_label[0] != '\0')
                    {
                        if (std::snprintf(line, sizeof(line), "           GO TO %s%s",
                                function->helper_parent_return_label,
                                statement->exception_from_catch || control_flow_depth > 0
                                    ? "" : ".") < 0)
                            return (FT_FAILURE);
                    }
                    else if (std::strncmp(function->source_name, "main",
                            sizeof(function->source_name)) == 0)
                        ft_strlcpy(line, "           STOP RUN", sizeof(line));
                    else
                        ft_strlcpy(line, "           GOBACK", sizeof(line));
                }
                else if (function->paragraph_return_label[0] == '\0'
                    || std::snprintf(line, sizeof(line), "           GO TO %s%s",
                        function->paragraph_return_label,
                        statement->exception_from_catch || control_flow_depth > 0
                            ? "" : ".") < 0)
                    return (FT_FAILURE);
            }
            else if (std::strncmp(function->source_name, "main", sizeof(function->source_name)) == 0)
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
    if (control_flow_depth != 0)
        return (FT_FAILURE);
    cobol_text_builder_set_control_flow_depth(builder, 0);
    if (function->returns_to_paragraph)
    {
        if (function->paragraph_return_label[0] == '\0'
            || std::snprintf(line, sizeof(line), "           GO TO %s.",
                function->paragraph_return_label) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (typed_ir_for_emit && typed_cleanup_for_emit
        && !function->is_exception_helper)
    {
        const int typed_cleanup_status = cblc_emit_typed_throw_cleanup_paragraphs(
            unit, function, typed_ir_for_emit, typed_cleanup_for_emit, builder);
        if (typed_cleanup_status != FT_SUCCESS
            && typed_cleanup_status != CBLC_TYPED_EMIT_UNSUPPORTED)
            return (FT_FAILURE);
    }
    if (cblc_emit_exception_cleanup_paragraphs(unit, function,
            typed_ir_for_emit, typed_cleanup_for_emit, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (function->returns_to_paragraph)
    {
        char return_label[TRANSPILE_STATEMENT_TEXT_MAX];

        if (function->paragraph_return_label[0] == '\0'
            || std::snprintf(return_label, sizeof(return_label), "       %s.",
                function->paragraph_return_label) < 0
            || cobol_text_builder_append_line(builder, return_label) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
            return (FT_FAILURE);
    }
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
    if (cblc_emit_exception_helpers(unit, function, builder,
            receiver_context) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    builder->active_function_name = saved_active_function_name;
    builder->active_statement = saved_active_statement;
    builder->active_statement_index = saved_active_statement_index;
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
        if (type->fields[index].kind == CBLC_DATA_KIND_CHAR_BUFFER)
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
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9) COMP OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9) COMP.",
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

            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line),
                        "       %02zu %s-%s OCCURS %zu TIMES.", level, prefix,
                        type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s.",
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
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line),
                        "       %02zu %s-%s-CBLC-LIVE-COUNT PIC 9(9) COMP VALUE 0.",
                        level, prefix, type->fields[index].cobol_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_builder_has_based_data_declaration(
    const t_cobol_text_builder *builder, const char *based_name);

static int cblc_emit_nested_pointer_layouts(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *prefix, size_t depth,
    t_cobol_text_builder *builder)
{
    char field_path[TRANSPILE_STATEMENT_TEXT_MAX];
    char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char line[256];
    size_t index;

    if (!unit || !type || !prefix || !builder
        || depth > CBLC_TEMPLATE_DEFAULT_MAX_DEPTH)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;
        const t_cblc_struct_type *field_type;

        field = &type->fields[index];
        if (std::snprintf(field_path, sizeof(field_path), "%s-%s", prefix,
                field->cobol_name) < 0)
            return (FT_FAILURE);
        if (field->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            field_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!field_type
                || cblc_build_struct_pointer_based_name(unit, field_path,
                    field->struct_type_name, based_name, sizeof(based_name))
                    != FT_SUCCESS
                )
                return (FT_FAILURE);
            if (!cblc_builder_has_based_data_declaration(builder, based_name))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s BASED.",
                        based_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cblc_emit_struct_fields(unit, field_type, based_name, 5,
                        builder) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            /* A specialization's top-level BASED record is shared, but each
             * nested pointer path used by lifecycle emission still needs its
             * own compatible child view. Do not skip that traversal merely
             * because the parent specialization layout already exists. */
            if (cblc_emit_nested_pointer_layouts(unit, field_type,
                    field_path, depth + 1, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            field_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!field_type
                || cblc_emit_nested_pointer_layouts(unit, field_type,
                    field_path, depth + 1, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_builder_has_based_data_declaration(
    const t_cobol_text_builder *builder, const char *based_name)
{
    char declaration[TRANSPILE_STATEMENT_TEXT_MAX];
    int written;

    if (!builder || !builder->data || !based_name)
        return (0);
    if (cobol_text_builder_has_based_name(builder, based_name))
        return (1);
    written = std::snprintf(declaration, sizeof(declaration),
        "       01 %s BASED.", based_name);
    if (written < 0 || static_cast<size_t>(written) >= sizeof(declaration))
        return (0);
    if (std::strstr(builder->data, declaration) != NULL)
    {
        (void)cobol_text_builder_register_based_name(
            const_cast<t_cobol_text_builder *>(builder), based_name);
        return (1);
    }
    return (0);
}

static int cblc_emit_constructor_pointer_parameter_layouts(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    size_t type_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    for (type_index = 0; type_index < unit->struct_type_count; ++type_index)
    {
        const t_cblc_struct_type *owner = &unit->struct_types[type_index];
        size_t constructor_index;

        for (constructor_index = 0;
            constructor_index < owner->constructor_count; ++constructor_index)
        {
            const t_cblc_constructor *constructor =
                &owner->constructors[constructor_index];
            size_t parameter_index;

            for (parameter_index = 0;
                parameter_index < constructor->parameter_count;
                ++parameter_index)
            {
                const t_cblc_parameter *parameter =
                    &constructor->parameters[parameter_index];
                const t_cblc_struct_type *parameter_type;
                char based_name[TRANSPILE_IDENTIFIER_MAX];
                char line[TRANSPILE_STATEMENT_TEXT_MAX];

                if (parameter->kind
                        != TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
                    continue ;
                parameter_type = cblc_find_struct_type(unit,
                    parameter->type_name);
                if (!parameter_type && owner->is_template_instantiation
                    && owner->template_source_name[0] != '\0'
                    && std::strcmp(parameter->type_name,
                        owner->template_source_name) == 0)
                    parameter_type = owner;
                /* The unspecialized vector declaration retains dependent
                 * `vector *` parameters. It has no concrete object layout;
                 * each concrete specialization is handled by the matching
                 * instantiation record above/below. */
                if (!parameter_type && owner->is_template)
                    continue ;
                const char *concrete_type_name = parameter_type
                    ? parameter_type->source_name : parameter->type_name;
                if (!parameter_type
                    || cblc_build_struct_pointer_based_name(unit,
                        parameter->cobol_name, concrete_type_name,
                        based_name, sizeof(based_name)) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "cannot resolve constructor pointer parameter '%s' of type '%s' while emitting its BASED layout",
                        parameter->cobol_name, parameter->type_name);
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                    return (FT_FAILURE);
                }
                if (!cblc_builder_has_based_data_declaration(builder,
                        based_name))
                {
                    if (std::snprintf(line, sizeof(line),
                            "       01 %s BASED.", based_name) < 0
                        || cobol_text_builder_append_line(builder, line)
                            != FT_SUCCESS
                        || cblc_emit_struct_fields(unit, parameter_type,
                            based_name, 5, builder) != FT_SUCCESS)
                    {
                        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "cannot emit BASED layout '%s' for constructor pointer parameter '%s'",
                            based_name, parameter->cobol_name);
                        cobol_text_builder_set_diagnostic(builder,
                            TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                        return (FT_FAILURE);
                    }
                }
                if (cblc_emit_nested_pointer_layouts(unit, parameter_type,
                        based_name, 0, builder) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "cannot emit nested BASED views for constructor pointer parameter '%s' of type '%s'",
                        parameter->cobol_name, parameter->type_name);
                    cobol_text_builder_set_diagnostic(builder,
                        TRANSPILE_ERROR_BACKEND_IR_VERIFICATION, diagnostic);
                    return (FT_FAILURE);
                }
            }
        }
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
        if (field->kind == CBLC_DATA_KIND_CHAR_BUFFER
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

static int cblc_emit_method_exception_body_inline(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    const char *assign_target, t_cobol_text_builder *builder)
{
    t_cobol_text_builder_lifecycle_scope lifecycle_scope(builder, method, NULL);
    t_cblc_function exception_function;
    const t_cblc_struct_type *receiver_type;
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !receiver || !method || !assign_target || !builder)
        return (FT_FAILURE);
    receiver_type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!receiver_type)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "method '%s::%s' has no resolved receiver type",
            receiver->struct_type_name, method->source_name);
        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        return (FT_FAILURE);
    }
    if (cblc_method_paragraph_name(unit, receiver, method, paragraph_name,
            sizeof(paragraph_name)) != FT_SUCCESS)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "could not form paragraph name for method '%s::%s'",
            receiver->struct_type_name, method->source_name);
        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        return (FT_FAILURE);
    }
    std::memset(&exception_function, 0, sizeof(exception_function));
    ft_strlcpy(exception_function.source_name, method->source_name,
        sizeof(exception_function.source_name));
    ft_strlcpy(exception_function.cobol_name, paragraph_name,
        sizeof(exception_function.cobol_name));
    std::memcpy(exception_function.parameters, method->parameters,
        sizeof(exception_function.parameters));
    exception_function.parameter_count = method->parameter_count;
    exception_function.exception_type_count = method->exception_type_count;
    std::memcpy(exception_function.exception_type_ids,
        method->exception_type_ids,
        sizeof(exception_function.exception_type_ids));
    exception_function.exception_types_unknown = method->exception_types_unknown;
    if (cblc_method_return_paragraph_name(unit, receiver, method,
            exception_function.paragraph_return_label,
            sizeof(exception_function.paragraph_return_label)) != FT_SUCCESS)
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "could not form return paragraph for method '%s::%s'",
            receiver->struct_type_name, method->source_name);
        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
        return (FT_FAILURE);
    }
    exception_function.returns_to_paragraph = 1;
    exception_function.return_kind = method->return_kind;
    ft_strlcpy(exception_function.return_type_name, method->return_type_name,
        sizeof(exception_function.return_type_name));
    if (assign_target && assign_target[0] != '\0')
        ft_strlcpy(exception_function.return_cobol_name, assign_target,
            sizeof(exception_function.return_cobol_name));
    exception_function.statement_capacity = method->statement_count;
    exception_function.statement_count = method->statement_count;
    exception_function.statements = static_cast<t_cblc_statement *>(cma_calloc(
        method->statement_count == 0 ? 1 : method->statement_count,
        sizeof(*exception_function.statements)));
    if (!exception_function.statements)
    {
        cobol_text_builder_set_diagnostic(builder, 0,
            "could not allocate lowered exception-aware method statements");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < method->statement_count)
    {
        t_cblc_statement *lowered_statement;
        char restored_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char restored_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char restored_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char parameter_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char parameter_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char parameter_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char receiver_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char receiver_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char receiver_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char field_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char field_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char field_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_source[TRANSPILE_STATEMENT_TEXT_MAX];
        char mapped_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
        char normalized_target[TRANSPILE_STATEMENT_TEXT_MAX];
        char normalized_source[TRANSPILE_STATEMENT_TEXT_MAX];

        exception_function.statements[index] = method->statements[index];
        lowered_statement = &exception_function.statements[index];
        if (cblc_restore_method_parameter_sources(method, lowered_statement->target,
                restored_target, sizeof(restored_target)) != FT_SUCCESS
            || cblc_restore_method_parameter_sources(method, lowered_statement->source,
                restored_source, sizeof(restored_source)) != FT_SUCCESS
            || cblc_restore_method_parameter_sources(method, lowered_statement->call_arguments,
                restored_arguments, sizeof(restored_arguments)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, restored_target,
                parameter_target, sizeof(parameter_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, restored_source,
                parameter_source, sizeof(parameter_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, restored_arguments,
                parameter_arguments, sizeof(parameter_arguments)) != FT_SUCCESS)
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "copy-assignment field substitution", index);
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        if (method->is_copy_assignment
            && (cblc_rewrite_method_field_references(unit, receiver, method,
                    parameter_target, field_target, sizeof(field_target)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    parameter_source, field_source, sizeof(field_source)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    parameter_arguments, field_arguments,
                    sizeof(field_arguments)) != FT_SUCCESS))
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "receiver substitution", index);
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        if (!method->is_copy_assignment)
        {
            ft_strlcpy(field_target, parameter_target, sizeof(field_target));
            ft_strlcpy(field_source, parameter_source, sizeof(field_source));
            ft_strlcpy(field_arguments, parameter_arguments, sizeof(field_arguments));
        }
        if (cblc_replace_this_prefix(field_target, receiver->cobol_name,
                receiver_target, sizeof(receiver_target)) != FT_SUCCESS
            || cblc_replace_this_prefix(field_source, receiver->cobol_name,
                receiver_source, sizeof(receiver_source)) != FT_SUCCESS
            || cblc_replace_this_prefix(field_arguments, receiver->cobol_name,
                receiver_arguments, sizeof(receiver_arguments)) != FT_SUCCESS)
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "field substitution", index);
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        if (!method->is_copy_assignment
            && (cblc_rewrite_method_field_references(unit, receiver, method,
                    receiver_target, field_target, sizeof(field_target)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    receiver_source, field_source, sizeof(field_source)) != FT_SUCCESS
                || cblc_rewrite_method_field_references(unit, receiver, method,
                    receiver_arguments, field_arguments,
                    sizeof(field_arguments)) != FT_SUCCESS))
        {
            cblc_set_method_exception_lowering_diagnostic(builder, receiver,
                method, "scoped data and expression rewriting", index);
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        if (cblc_rewrite_method_parameter_references(unit, method, field_target,
                receiver_target, sizeof(receiver_target)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, field_source,
                receiver_source, sizeof(receiver_source)) != FT_SUCCESS
            || cblc_rewrite_method_parameter_references(unit, method, field_arguments,
                receiver_arguments, sizeof(receiver_arguments)) != FT_SUCCESS
            || cblc_rewrite_scoped_method_data_references(unit, method,
                receiver_target,
                receiver->cobol_name, mapped_target, sizeof(mapped_target)) != FT_SUCCESS
            || cblc_rewrite_scoped_method_data_references(unit, method,
                receiver_source,
                receiver->cobol_name, mapped_source, sizeof(mapped_source)) != FT_SUCCESS
            || cblc_rewrite_scoped_method_data_references(unit, method,
                receiver_arguments,
                receiver->cobol_name, mapped_arguments, sizeof(mapped_arguments))
                != FT_SUCCESS
            || cblc_normalize_method_string_subscripts(unit, receiver, mapped_target,
                normalized_target, sizeof(normalized_target)) != FT_SUCCESS
            || cblc_normalize_method_string_subscripts(unit, receiver, mapped_source,
                normalized_source, sizeof(normalized_source)) != FT_SUCCESS)
        {
            cma_free(exception_function.statements);
            return (FT_FAILURE);
        }
        ft_strlcpy(lowered_statement->target, normalized_target,
            sizeof(lowered_statement->target));
        ft_strlcpy(lowered_statement->source, normalized_source,
            sizeof(lowered_statement->source));
        ft_strlcpy(lowered_statement->call_arguments, mapped_arguments,
            sizeof(lowered_statement->call_arguments));
        if (method->return_kind != CBLC_FUNCTION_RETURN_VOID
            && assign_target && assign_target[0] != '\0'
            && (std::strcmp(exception_function.statements[index].target,
                    "CBLC-THIS-RET") == 0
                || (std::strlen(receiver->cobol_name) + sizeof("-RET") - 1
                        == std::strlen(exception_function.statements[index].target)
                    && std::strncmp(exception_function.statements[index].target,
                        receiver->cobol_name,
                        std::strlen(receiver->cobol_name)) == 0
                    && std::strcmp(exception_function.statements[index].target
                            + std::strlen(receiver->cobol_name), "-RET") == 0)))
            ft_strlcpy(exception_function.statements[index].target,
                assign_target,
                sizeof(exception_function.statements[index].target));
        index += 1;
    }
    if (cblc_emit_function(unit, &exception_function, builder, 0, receiver) != FT_SUCCESS)
    {
        cblc_set_method_exception_lowering_diagnostic(builder, receiver,
            method, "COBOL function emission", method->statement_count);
        cma_free(exception_function.statements);
        return (FT_FAILURE);
    }
    if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
    {
        cma_free(exception_function.statements);
        return (FT_FAILURE);
    }
    cma_free(exception_function.statements);
    return (FT_SUCCESS);
}

static int cblc_cobol_text_has_paragraph_definition(const char *text,
    const char *paragraph_name)
{
    const char *line;
    size_t name_length;

    if (!text || !paragraph_name || paragraph_name[0] == '\0')
        return (0);
    name_length = std::strlen(paragraph_name);
    line = text;
    while (*line != '\0')
    {
        const char *line_end;
        const char *token;

        line_end = std::strchr(line, '\n');
        if (!line_end)
            line_end = line + std::strlen(line);
        token = line;
        while (token < line_end && *token == ' ')
            token += 1;
        if (token - line == 7
            && static_cast<size_t>(line_end - token) > name_length
            && std::strncmp(token, paragraph_name, name_length) == 0
            && token[name_length] == '.'
            && (token + name_length + 1 == line_end
                || token[name_length + 1] == ' '
                || token[name_length + 1] == '\t'
                || token[name_length + 1] == '\r'))
            return (1);
        line = *line_end == '\0' ? line_end : line_end + 1;
    }
    return (0);
}

static int cblc_emit_method_paragraph_result_copy(
    const t_cblc_method *method, const char *assign_target,
    const std::string *paragraph_result_target,
    t_cobol_text_builder *builder)
{
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!method || !builder)
        return (FT_FAILURE);
    if (method->return_kind == CBLC_FUNCTION_RETURN_VOID)
        return (FT_SUCCESS);
    /* Some inline nested-method lowerings do not register a separate shared
     * paragraph result target. Preserve their existing direct-result path. */
    if (!paragraph_result_target || paragraph_result_target->empty())
        return (FT_SUCCESS);
    if (!assign_target || assign_target[0] == '\0')
        return (FT_FAILURE);
    if (std::strcmp(assign_target, paragraph_result_target->c_str()) == 0)
        return (FT_SUCCESS);
    if (method->return_kind == CBLC_FUNCTION_RETURN_INT
        || method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
    {
        if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                assign_target, paragraph_result_target->c_str()) < 0)
            return (FT_FAILURE);
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
        || method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
        || method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
        || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        if (std::snprintf(line, sizeof(line), "           SET %s TO %s.",
                assign_target, paragraph_result_target->c_str()) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
            paragraph_result_target->c_str(), assign_target) < 0)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, line));
}

static int cblc_emit_method_exception_body(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_method *method,
    const char *assign_target, t_cobol_text_builder *builder, int emit_call)
{
    t_cobol_text_builder deferred_body;
    const t_cblc_struct_type *receiver_type;
    char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
    char return_name[TRANSPILE_IDENTIFIER_MAX];
    char paragraph_result_name[TRANSPILE_IDENTIFIER_MAX];
    const std::string *paragraph_result_target;
    char call_line[TRANSPILE_STATEMENT_TEXT_MAX];
    int status;

    if (!unit || !receiver || !method || !assign_target || !builder)
        return (FT_FAILURE);
    receiver_type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!receiver_type
        || cblc_method_paragraph_name(unit, receiver, method, paragraph_name,
            sizeof(paragraph_name)) != FT_SUCCESS
        || cblc_method_return_paragraph_name(unit, receiver, method, return_name,
            sizeof(return_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    paragraph_result_name[0] = '\0';
    if (method->return_kind != CBLC_FUNCTION_RETURN_VOID
        && cblc_method_result_name(receiver, method, paragraph_result_name,
            sizeof(paragraph_result_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    paragraph_result_target = NULL;
    if (emit_call
        && cblc_emit_struct_pointer_parameter_bindings(unit,
            method->parameters, method->parameter_count, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (builder->emitted_method_paragraphs
        && builder->emitted_method_paragraphs->find(paragraph_name)
            != builder->emitted_method_paragraphs->end())
    {
        if (!emit_call)
            return (FT_SUCCESS);
        if (std::snprintf(call_line, sizeof(call_line),
                "           PERFORM %s THRU %s.", paragraph_name, return_name) < 0
            || cobol_text_builder_append_line(builder, call_line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (builder->method_paragraph_return_targets)
            paragraph_result_target = &(*builder->method_paragraph_return_targets)[paragraph_name];
        return (cblc_emit_method_paragraph_result_copy(method, assign_target,
            paragraph_result_target, builder));
    }
    if (builder->known_paragraph_definitions
        ? builder->known_paragraph_definitions->find(paragraph_name)
            != builder->known_paragraph_definitions->end()
        : ((builder->data
                && cblc_cobol_text_has_paragraph_definition(builder->data,
                    paragraph_name))
            || (builder->deferred_procedure_text
                && cblc_cobol_text_has_paragraph_definition(
                    builder->deferred_procedure_text, paragraph_name))))
    {
        if (builder->emitted_method_paragraphs)
            builder->emitted_method_paragraphs->insert(paragraph_name);
        if (!emit_call)
            return (FT_SUCCESS);
        if (std::snprintf(call_line, sizeof(call_line),
                "           PERFORM %s THRU %s.", paragraph_name, return_name) < 0
            || cobol_text_builder_append_line(builder, call_line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (builder->method_paragraph_return_targets)
            paragraph_result_target = &(*builder->method_paragraph_return_targets)[paragraph_name];
        return (cblc_emit_method_paragraph_result_copy(method, assign_target,
            paragraph_result_target, builder));
    }
    cobol_text_builder_init(&deferred_body);
    deferred_body.free_format = builder->free_format;
    deferred_body.control_flow_depth = builder->control_flow_depth;
    deferred_body.emitted_method_paragraphs = builder->emitted_method_paragraphs;
    deferred_body.method_paragraph_return_targets =
        builder->method_paragraph_return_targets;
    deferred_body.known_paragraph_definitions =
        builder->known_paragraph_definitions;
    if (builder->emitted_method_paragraphs)
        builder->emitted_method_paragraphs->insert(paragraph_name);
    if (builder->method_paragraph_return_targets)
        (*builder->method_paragraph_return_targets)[paragraph_name] =
            paragraph_result_name[0] != '\0'
                ? paragraph_result_name : assign_target;
    status = cblc_emit_method_exception_body_inline(unit, receiver, method,
        paragraph_result_name[0] != '\0'
            ? paragraph_result_name : assign_target,
        &deferred_body);
    if (status != FT_SUCCESS)
    {
        if (deferred_body.diagnostic_message[0] != '\0')
            cobol_text_builder_set_diagnostic(builder,
                deferred_body.diagnostic_code,
                deferred_body.diagnostic_message);
        cobol_text_builder_dispose(&deferred_body);
        return (FT_FAILURE);
    }
    if (cobol_text_builder_append_deferred(builder, deferred_body.data,
            deferred_body.length) != FT_SUCCESS
        || cobol_text_builder_append_deferred(builder,
            deferred_body.deferred_procedure_text,
            deferred_body.deferred_procedure_length) != FT_SUCCESS)
    {
        cobol_text_builder_dispose(&deferred_body);
        return (FT_FAILURE);
    }
    cobol_text_builder_dispose(&deferred_body);
    if (!emit_call)
        return (FT_SUCCESS);
    if (std::snprintf(call_line, sizeof(call_line),
            "           PERFORM %s THRU %s.", paragraph_name, return_name) < 0
        || cobol_text_builder_append_line(builder, call_line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (builder->method_paragraph_return_targets)
        paragraph_result_target = &(*builder->method_paragraph_return_targets)[paragraph_name];
    return (cblc_emit_method_paragraph_result_copy(method, assign_target,
        paragraph_result_target, builder));
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

static int cblc_translation_unit_calls_are_resolved(
    const t_cblc_translation_unit *unit,
    const t_cblc_function **out_unresolved_function,
    const t_cblc_statement **out_unresolved_statement)
{
    size_t function_index;

    if (out_unresolved_function)
        *out_unresolved_function = NULL;
    if (out_unresolved_statement)
        *out_unresolved_statement = NULL;
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
            {
                if (out_unresolved_function)
                    *out_unresolved_function = function;
                if (out_unresolved_statement)
                    *out_unresolved_statement = statement;
                return (FT_FAILURE);
            }
            statement_index += 1;
        }
        function_index += 1;
    }
    return (FT_SUCCESS);
}
