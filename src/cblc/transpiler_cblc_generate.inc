static size_t cblc_native_result_digits(const t_cblc_translation_unit *unit,
    const char *cobol_name);

static int cblc_string_item_is_array_member(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const char *separator;
    char owner_name[TRANSPILE_IDENTIFIER_MAX];
    size_t owner_length;
    const t_cblc_data_item *owner;

    if (!unit || !item || item->kind != CBLC_DATA_KIND_STRING)
        return (0);
    separator = std::strchr(item->source_name, '.');
    if (!separator)
        return (0);
    owner_length = static_cast<size_t>(separator - item->source_name);
    if (owner_length == 0 || owner_length >= sizeof(owner_name))
        return (0);
    std::memcpy(owner_name, item->source_name, owner_length);
    owner_name[owner_length] = '\0';
    owner = cblc_find_data_item_const(unit, owner_name);
    return (owner && owner->array_count > 0);
}

static int cblc_emit_reusable_method_paragraphs(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *receiver;
        const t_cblc_struct_type *type;
        size_t method_index;

        receiver = &unit->data_items[data_index];
        if (receiver->is_alias || receiver->kind != CBLC_DATA_KIND_STRUCT
            || receiver->array_count > 0
            || (!receiver->is_function_local && receiver->owner_function_name[0] != '\0'))
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        method_index = 0;
        while (method_index < type->method_count)
        {
            const t_cblc_method *method;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];

            method = &type->methods[method_index];
            if (!cblc_method_is_reusable(method))
            {
                method_index += 1;
                continue ;
            }
            if (cblc_method_paragraph_name(receiver, method, paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            {
                char result_name[TRANSPILE_IDENTIFIER_MAX];
                const char *assign_target;

                assign_target = NULL;
                if (method->return_kind != CBLC_FUNCTION_RETURN_VOID)
                {
                    if (cblc_method_result_name(receiver, method, result_name,
                            sizeof(result_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    assign_target = result_name;
                }
                if (cblc_method_has_exception_control(method))
                {
                    if (cblc_emit_method_exception_body(unit, receiver, method,
                            assign_target ? assign_target : "", builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_emit_method_body(unit, receiver, method, assign_target,
                            builder) != FT_SUCCESS)
                    {
                        return (FT_FAILURE);
                    }

                    if (cobol_text_builder_append_line(builder, "           EXIT.")
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
            method_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_unit_uses_exceptions(const t_cblc_translation_unit *unit)
{
    size_t function_index;
    size_t type_index;

    if (!unit)
        return (0);
    function_index = 0;
    while (function_index < unit->function_count)
    {
        size_t statement_index;

        statement_index = 0;
        while (statement_index < unit->functions[function_index].statement_count)
        {
            t_cblc_statement_type type;

            type = unit->functions[function_index].statements[statement_index].type;
            if (type == CBLC_STATEMENT_TRY_BEGIN
                || type == CBLC_STATEMENT_CATCH_BEGIN
                || type == CBLC_STATEMENT_THROW)
                return (1);
            if ((type == CBLC_STATEMENT_CALL || type == CBLC_STATEMENT_CALL_ASSIGN)
                && cblc_statement_call_may_throw(unit,
                    &unit->functions[function_index].statements[statement_index]))
                return (1);
            if (cblc_statement_method_may_throw(unit,
                    &unit->functions[function_index].statements[statement_index])
                || cblc_statement_constructor_may_throw(unit,
                    &unit->functions[function_index].statements[statement_index]))
                return (1);
            statement_index += 1;
        }
        function_index += 1;
    }
    type_index = 0;
    while (type_index < unit->struct_type_count)
    {
        size_t method_index;

        method_index = 0;
        while (method_index < unit->struct_types[type_index].method_count)
        {
            if (cblc_method_has_exception_control(
                    &unit->struct_types[type_index].methods[method_index]))
                return (1);
            method_index += 1;
        }
        if (unit->struct_types[type_index].destructor_statement_count > 0)
        {
            size_t statement_index;

            statement_index = 0;
            while (statement_index < unit->struct_types[type_index].destructor_statement_count)
            {
                t_cblc_statement_type type;

                type = unit->struct_types[type_index].destructor_statements[statement_index].type;
                if (type == CBLC_STATEMENT_THROW
                    || unit->struct_types[type_index].destructor_statements[statement_index].exception_may_throw)
                    return (1);
                statement_index += 1;
            }
        }
        type_index += 1;
    }
    return (0);
}

static int cblc_emit_reusable_lifecycle_paragraphs(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *receiver;
        const t_cblc_struct_type *type;
        size_t constructor_index;

        receiver = &unit->data_items[data_index];
        if (receiver->is_alias || receiver->kind != CBLC_DATA_KIND_STRUCT
            || (!receiver->is_function_local && receiver->owner_function_name[0] != '\0'))
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        constructor_index = 0;
        while (constructor_index < type->constructor_count)
        {
            const t_cblc_constructor *constructor;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t statement_index;

            constructor = &type->constructors[constructor_index];
            if (constructor->is_copy_constructor
                || !constructor->has_definition || constructor->statement_count == 0)
            {
                constructor_index += 1;
                continue ;
            }
            if (cblc_constructor_paragraph_name(receiver, constructor_index,
                    paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_constructor_has_exception_control(constructor))
            {
                if (cblc_emit_constructor_exception_body(unit, receiver, constructor,
                        constructor_index, builder) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                statement_index = 0;
                while (statement_index < constructor->statement_count)
                {
                    if (cblc_emit_substituted_statement(unit, NULL,
                            &constructor->statements[statement_index], receiver->cobol_name,
                            builder) != FT_SUCCESS)
                    {
                        return (FT_FAILURE);
                    }
                    statement_index += 1;
                }
                if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
            constructor_index += 1;
        }
        if (type->destructor_statement_count > 0)
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t statement_index;

            if (cblc_destructor_paragraph_name(receiver, paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            statement_index = 0;
            while (statement_index < type->destructor_statement_count)
            {
                if (cblc_emit_substituted_statement(unit, NULL,
                        &type->destructor_statements[statement_index], receiver->cobol_name,
                        builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                statement_index += 1;
            }
            if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_reusable_method_results(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *receiver;
        const t_cblc_struct_type *type;
        size_t method_index;

        receiver = &unit->data_items[data_index];
        if (receiver->is_alias || receiver->kind != CBLC_DATA_KIND_STRUCT
            || (!receiver->is_function_local && receiver->owner_function_name[0] != '\0'))
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        method_index = 0;
        while (method_index < type->method_count)
        {
            const t_cblc_method *method;
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            char line[256];

            method = &type->methods[method_index];
            if (!cblc_method_is_reusable(method)
                || method->return_kind == CBLC_FUNCTION_RETURN_VOID)
            {
                method_index += 1;
                continue ;
            }
            if (cblc_method_result_name(receiver, method, result_name,
                    sizeof(result_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (method->return_reference_kind != CBLC_REFERENCE_NONE)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s USAGE POINTER VALUE NULL.", result_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s PIC S9(9) VALUE 0.", result_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s USAGE COMP-2 VALUE 0.", result_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                t_cblc_data_item result_item;

                std::memset(&result_item, 0, sizeof(result_item));
                result_item.kind = CBLC_DATA_KIND_STRUCT;
                ft_strlcpy(result_item.struct_type_name, method->return_type_name,
                    sizeof(result_item.struct_type_name));
                ft_strlcpy(result_item.cobol_name, result_name,
                    sizeof(result_item.cobol_name));
                if (cblc_emit_struct_instance(unit, &result_item, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            method_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_reusable_string_intrinsic_results(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *item;
        const char *native_names[] = {"len"};
        const char *regular_names[] = {"len", "capacity", "empty"};
        const char **names;
        size_t name_count;
        size_t name_index;

        item = &unit->data_items[data_index];
        if (item->is_alias || item->kind != CBLC_DATA_KIND_STRING
            || item->array_count > 0 || cblc_string_item_is_array_member(unit, item))
        {
            data_index += 1;
            continue ;
        }
        names = unit->is_native_standard_library ? native_names : regular_names;
        name_count = unit->is_native_standard_library
            ? sizeof(native_names) / sizeof(native_names[0])
            : sizeof(regular_names) / sizeof(regular_names[0]);
        name_index = 0;
        while (name_index < name_count)
        {
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            char line[256];

            if (cblc_intrinsic_result_name(item, names[name_index], result_name,
                    sizeof(result_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu) VALUE 0.",
                    result_name,
                    cblc_native_result_digits(unit, result_name)) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            name_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static size_t cblc_reusable_string_intrinsic_buffer_length(
    const t_cblc_translation_unit *unit)
{
    size_t data_index;
    size_t maximum_length;

    maximum_length = 256;
    if (!unit)
        return (maximum_length);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        if (!unit->data_items[data_index].is_alias
            && unit->data_items[data_index].kind == CBLC_DATA_KIND_STRING
            && unit->data_items[data_index].array_count == 0
            && unit->data_items[data_index].length > maximum_length)
            maximum_length = unit->data_items[data_index].length;
        data_index += 1;
    }
    return (maximum_length);
}

static int cblc_emit_reusable_string_intrinsic_arguments(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    size_t data_index;
    size_t buffer_length;

    if (!unit || !builder)
        return (FT_FAILURE);
    buffer_length = cblc_reusable_string_intrinsic_buffer_length(unit);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *item;
        size_t argument_index;

        item = &unit->data_items[data_index];
        if (item->is_alias || item->kind != CBLC_DATA_KIND_STRING
            || item->array_count > 0 || cblc_string_item_is_array_member(unit, item))
        {
            data_index += 1;
            continue ;
        }
        argument_index = 0;
        while (argument_index < 2)
        {
            char group_name[TRANSPILE_IDENTIFIER_MAX];
            char length_name[TRANSPILE_IDENTIFIER_MAX];
            char buffer_name[TRANSPILE_IDENTIFIER_MAX];
            char line[256];

            if (cblc_intrinsic_argument_name(item, argument_index, "GROUP",
                    group_name, sizeof(group_name)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(item, argument_index, "LEN",
                    length_name, sizeof(length_name)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(item, argument_index, "BUF",
                    buffer_name, sizeof(buffer_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       01 %s.", group_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "          05 %s PIC S9(9) COMP VALUE 0.",
                    length_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "          05 %s PIC X(%zu).",
                    buffer_name, buffer_length) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            argument_index += 1;
        }
        {
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            char status_name[TRANSPILE_IDENTIFIER_MAX];
            char line[256];

            if (cblc_intrinsic_result_name(item, "parameterized", result_name,
                    sizeof(result_name)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(item, 2, "STATUS", status_name,
                    sizeof(status_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       01 %s PIC S9(9) VALUE 0.",
                    result_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       01 %s PIC S9(9) VALUE 0.",
                    status_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_reusable_string_intrinsic_paragraphs(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *item;
        const char *native_names[] = {"len"};
        const char *regular_names[] = {"len", "capacity", "empty", "clear"};
        const char **names;
        size_t name_count;
        size_t name_index;

        item = &unit->data_items[data_index];
        if (item->is_alias || item->kind != CBLC_DATA_KIND_STRING
            || item->array_count > 0 || cblc_string_item_is_array_member(unit, item))
        {
            data_index += 1;
            continue ;
        }
        names = unit->is_native_standard_library ? native_names : regular_names;
        name_count = unit->is_native_standard_library
            ? sizeof(native_names) / sizeof(native_names[0])
            : sizeof(regular_names) / sizeof(regular_names[0]);
        name_index = 0;
        while (name_index < name_count)
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            char line[256];

            if (cblc_intrinsic_paragraph_name(item, names[name_index], paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::strcmp(names[name_index], "len") == 0)
            {
                if (cblc_intrinsic_result_name(item, names[name_index], result_name,
                        sizeof(result_name)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s-LEN.",
                        result_name, item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (std::strcmp(names[name_index], "capacity") == 0)
            {
                if (cblc_intrinsic_result_name(item, names[name_index], result_name,
                        sizeof(result_name)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           COMPUTE %s = %zu.",
                        result_name, item->length) < 0)
                    return (FT_FAILURE);
            }
            else if (std::strcmp(names[name_index], "empty") == 0)
            {
                if (cblc_intrinsic_result_name(item, names[name_index], result_name,
                        sizeof(result_name)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           IF %s-LEN = 0",
                        item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                    return (FT_FAILURE);
                name_index += 1;
                continue ;
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                        item->cobol_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = 0.",
                        item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
            name_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_reusable_parameterized_string_intrinsic_paragraphs(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    const t_cblc_intrinsic_entry *intrinsic_entries;
    size_t intrinsic_count;
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    intrinsic_entries = cblc_intrinsic_get_entries(&intrinsic_count);
    if (!intrinsic_entries)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *receiver;
        size_t intrinsic_index;

        receiver = &unit->data_items[data_index];
        if (receiver->is_alias || receiver->kind != CBLC_DATA_KIND_STRING
            || receiver->array_count > 0 || cblc_string_item_is_array_member(unit, receiver))
        {
            data_index += 1;
            continue ;
        }
        intrinsic_index = 0;
        while (intrinsic_index < intrinsic_count)
        {
            const t_cblc_intrinsic_entry *intrinsic_entry;
            const char *intrinsic_name;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char argument_zero_length[TRANSPILE_IDENTIFIER_MAX];
            char argument_zero_buffer[TRANSPILE_IDENTIFIER_MAX];
            char argument_one_group[TRANSPILE_IDENTIFIER_MAX];
            char argument_one_length[TRANSPILE_IDENTIFIER_MAX];
            char argument_one_buffer[TRANSPILE_IDENTIFIER_MAX];
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            char status_name[TRANSPILE_IDENTIFIER_MAX];
            char line[512];

            intrinsic_entry = &intrinsic_entries[intrinsic_index];
            if (intrinsic_entry->minimum_arguments != 1
                || intrinsic_entry->maximum_arguments != 1)
            {
                intrinsic_index += 1;
                continue ;
            }
            intrinsic_name = intrinsic_entry->name;
            if (cblc_intrinsic_paragraph_name(receiver, intrinsic_name, paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(receiver, 0, "LEN", argument_zero_length,
                    sizeof(argument_zero_length)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(receiver, 0, "BUF", argument_zero_buffer,
                    sizeof(argument_zero_buffer)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(receiver, 1, "GROUP", argument_one_group,
                    sizeof(argument_one_group)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(receiver, 1, "LEN", argument_one_length,
                    sizeof(argument_one_length)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(receiver, 1, "BUF", argument_one_buffer,
                    sizeof(argument_one_buffer)) != FT_SUCCESS
                || cblc_intrinsic_result_name(receiver, "parameterized", result_name,
                    sizeof(result_name)) != FT_SUCCESS
                || cblc_intrinsic_argument_name(receiver, 2, "STATUS", status_name,
                    sizeof(status_name)) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::strcmp(intrinsic_name, "append") == 0)
            {
                if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.", status_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_string_item_uses_dynamic_storage(receiver))
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-CAP < %s-LEN + %s",
                            receiver->cobol_name, receiver->cobol_name, argument_one_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line), "               MOVE SPACES TO %s",
                            argument_zero_buffer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               IF %s-LEN > 0",
                            receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "                   MOVE %s-BUF(1:%s-LEN) TO %s(1:%s-LEN)",
                            receiver->cobol_name, receiver->cobol_name, argument_zero_buffer,
                            receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               END-IF") < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               MOVE %s-LEN TO %s",
                            receiver->cobol_name, argument_zero_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               IF %s-PTR NOT = NULL",
                            receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "                   FREE %s-PTR",
                            receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "               IF %s-LEN + %s > %zu",
                            receiver->cobol_name, argument_one_length, receiver->length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "                   ALLOCATE %s-LEN + %s CHARACTERS RETURNING %s-PTR",
                            receiver->cobol_name, argument_one_length, receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "                   COMPUTE %s-CAP = %s-LEN + %s",
                            receiver->cobol_name, receiver->cobol_name, argument_one_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "               ELSE") != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "                   ALLOCATE %zu CHARACTERS RETURNING %s-PTR",
                            receiver->length, receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "                   MOVE %zu TO %s-CAP",
                            receiver->length, receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s-BUF TO %s-PTR",
                            receiver->cobol_name, receiver->cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               IF %s > 0",
                            argument_zero_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "                   MOVE %s(1:%s) TO %s-BUF(1:%s)",
                            argument_zero_buffer, argument_zero_length, receiver->cobol_name,
                            argument_zero_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (!cblc_string_item_uses_dynamic_storage(receiver))
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN + %s > %zu",
                            receiver->cobol_name, argument_one_length, receiver->length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               MOVE 1 TO %s",
                            status_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "           ELSE") != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               IF %s > 0",
                            argument_one_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line),
                            "                   MOVE %s(1:%s) TO %s-BUF(%s-LEN + 1:%s)",
                            argument_one_buffer, argument_one_length, receiver->cobol_name,
                            receiver->cobol_name, argument_one_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "               COMPUTE %s-LEN = %s-LEN + %s",
                            receiver->cobol_name, receiver->cobol_name, argument_one_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line), "           IF %s > 0",
                        argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "               MOVE %s(1:%s) TO %s-BUF(%s-LEN + 1:%s)",
                        argument_one_buffer, argument_one_length, receiver->cobol_name,
                        receiver->cobol_name, argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           END-IF") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = %s-LEN + %s.",
                        receiver->cobol_name, receiver->cobol_name, argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (std::strcmp(intrinsic_name, "compare") == 0)
            {
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCMP-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, argument_one_group, result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (std::strcmp(intrinsic_name, "equals") == 0)
            {
                if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.", result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           IF %s-LEN = %s",
                        receiver->cobol_name, argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "               IF %s-LEN = 0",
                        receiver->cobol_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "                   MOVE 1 TO %s",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "               ELSE") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "                   IF %s-BUF(1:%s-LEN) = %s(1:%s)",
                        receiver->cobol_name, receiver->cobol_name, argument_one_buffer,
                        argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "                       MOVE 1 TO %s",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (std::strcmp(intrinsic_name, "starts_with") == 0
                || std::strcmp(intrinsic_name, "ends_with") == 0)
            {
                int is_prefix;

                is_prefix = std::strcmp(intrinsic_name, "starts_with") == 0;
                if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.", result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           IF %s = 0",
                        argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "               MOVE 1 TO %s",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           ELSE") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "               IF %s-LEN >= %s",
                        receiver->cobol_name, argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_prefix)
                {
                    if (std::snprintf(line, sizeof(line),
                            "                   IF %s-BUF(1:%s) = %s(1:%s)",
                            receiver->cobol_name, argument_one_length, argument_one_buffer,
                            argument_one_length) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line),
                            "                   IF %s-BUF(%s-LEN - %s + 1:%s) = %s(1:%s)",
                            receiver->cobol_name, receiver->cobol_name, argument_one_length,
                            argument_one_length, argument_one_buffer, argument_one_length) < 0)
                        return (FT_FAILURE);
                }
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "                       MOVE 1 TO %s",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s.", result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "           IF %s = 0",
                        argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "               MOVE 1 TO %s",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           ELSE") != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "               INSPECT %s-BUF(1:%s-LEN) TALLYING %s FOR ALL %s(1:%s)",
                        receiver->cobol_name, receiver->cobol_name, result_name,
                        argument_one_buffer, argument_one_length) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "               IF %s > 0",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "                   MOVE 1 TO %s",
                        result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
            intrinsic_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static size_t cblc_native_result_digits(const t_cblc_translation_unit *unit,
    const char *cobol_name)
{
    if (!unit || !unit->is_native_standard_library || !cobol_name)
        return (9);
    if (std::strcmp(cobol_name, "F-RESULT") != 0
        && std::strcmp(cobol_name, "F-OPERAND") != 0
        && std::strcmp(cobol_name, "CBLC-RETURN-F") != 0)
        return (9);
    if (std::strcmp(unit->program_name, "CBLC-ATOL") == 0
        || std::strcmp(unit->program_name, "CBLC-ATOL-STRING") == 0)
        return (18);
    if (std::strcmp(unit->program_name, "CBLC-ATOLL") == 0
        || std::strcmp(unit->program_name, "CBLC-ATOLL-STRING") == 0)
        return (36);
    if (std::strcmp(unit->program_name, "CBLC-ABS") == 0)
        return (18);
    return (9);
}

static const char *cblc_native_numeric_storage_format(
    const t_cblc_translation_unit *unit, const char *cobol_name)
{
    if (unit && unit->is_native_standard_library
        && cblc_native_result_digits(unit, cobol_name) <= 18)
        return (" COMP-5");
    return ("");
}

static int cblc_native_parameter_is_by_value(const t_cblc_translation_unit *unit,
    size_t parameter_index)
{
    const char *program_name;

    if (!unit || !unit->is_native_standard_library)
        return (0);
    program_name = unit->program_name;
    if (std::strcmp(program_name, "CBLC-STRLEN") == 0)
        return (parameter_index == 1);
    if (std::strcmp(program_name, "CBLC-STRNLEN") == 0)
        return (parameter_index == 1 || parameter_index == 2);
    if (std::strcmp(program_name, "CBLC-STRTOD") == 0)
        return (parameter_index == 1);
    if (std::strcmp(program_name, "CBLC-TOUPPER") == 0
        || std::strcmp(program_name, "CBLC-TOLOWER") == 0)
        return (parameter_index == 1);
    if (std::strcmp(program_name, "CBLC-STRCMP") == 0
        || std::strcmp(program_name, "CBLC-STRCPY") == 0)
        return (parameter_index == 1 || parameter_index == 3);
    if (std::strcmp(program_name, "CBLC-STRNCPY") == 0)
        return (parameter_index == 1 || parameter_index == 3 || parameter_index == 4);
    if (std::strcmp(program_name, "CBLC-STRCAT") == 0)
        return (parameter_index == 1 || parameter_index == 3 || parameter_index == 5);
    if (std::strcmp(program_name, "CBLC-STRNCPY-STRING") == 0
        || std::strcmp(program_name, "CBLC-MEMCMP-STRING") == 0)
        return (parameter_index == 2);
    return (0);
}

static int cblc_native_parameter_is_status(const t_cblc_function *entry_function,
    size_t parameter_index)
{
    if (!entry_function || parameter_index >= entry_function->parameter_count)
        return (0);
    return (std::strstr(entry_function->parameters[parameter_index].cobol_name,
            "STATUS") != NULL);
}

static int cblc_native_parameter_uses_comp5(const t_cblc_translation_unit *unit,
    const t_cblc_function *entry_function, size_t parameter_index)
{
    const char *program_name;
    const char *parameter_name;

    if (!unit || !entry_function || parameter_index >= entry_function->parameter_count
        || entry_function->parameters[parameter_index].kind
            != TRANSPILE_FUNCTION_PARAMETER_INT)
        return (0);
    if (cblc_native_parameter_is_status(entry_function, parameter_index))
        return (0);
    program_name = unit->program_name;
    parameter_name = entry_function->parameters[parameter_index].cobol_name;
    if (std::strcmp(program_name, "CBLC-ABS") == 0)
        return (1);
    if (std::strstr(parameter_name, "RESULT") != NULL)
        return (0);
    if (std::strstr(parameter_name, "LENGTH") != NULL
        || std::strstr(parameter_name, "LIMIT") != NULL
        || std::strstr(parameter_name, "REQUEST") != NULL
        || std::strstr(parameter_name, "SERIAL") != NULL
        || std::strstr(parameter_name, "SCALE") != NULL
        || std::strstr(parameter_name, "DURATION") != NULL
        || std::strstr(parameter_name, "COMPARISON") != NULL)
        return (1);
    return (0);
}

int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    t_cobol_text_builder builder;
    char line[256];
    size_t index;
    size_t entry_index;
    const t_cblc_function *entry_function;
    int entry_uses_exception_context;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    if (cblc_translation_unit_calls_are_resolved(unit) != FT_SUCCESS)
    {
        return (FT_FAILURE);
    }
    entry_index = unit->entry_function_index;
    if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
        entry_index = 0;
    entry_function = NULL;
    if (unit->function_count > 0 && entry_index < unit->function_count)
        entry_function = &unit->functions[entry_index];
    entry_uses_exception_context = entry_function
        && cblc_is_external_entry_function(unit)
        && cblc_unit_uses_exceptions(unit);
    cobol_text_builder_init(&builder);
    builder.free_format = unit->is_native_standard_library ? 1 : 0;
    if (unit->is_native_standard_library
        && cobol_text_builder_append_line(&builder, ">>SOURCE FORMAT IS FREE") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       IDENTIFICATION DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(line, sizeof(line), "       PROGRAM-ID. %s.", unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       ENVIRONMENT DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (unit->file_count > 0)
    {
        if (cobol_text_builder_append_line(&builder, "       INPUT-OUTPUT SECTION.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "       FILE-CONTROL.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->file_count)
        {
            const char *organization_clause;
            const char *record_key_clause;

            organization_clause = unit->files[index].organization
                == TRANSPILE_FILE_ORGANIZATION_INDEXED ? " ORGANIZATION IS INDEXED"
                : (unit->files[index].organization == TRANSPILE_FILE_ORGANIZATION_RELATIVE
                    ? " ORGANIZATION IS RELATIVE" : "");
            record_key_clause = unit->files[index].organization
                == TRANSPILE_FILE_ORGANIZATION_INDEXED ? " RECORD KEY IS " : "";
            if (std::snprintf(line, sizeof(line),
                    "           SELECT %s ASSIGN TO \"%s\"%s%s%s%s FILE STATUS IS %s-STATUS.",
                    unit->files[index].cobol_name, unit->files[index].path,
                    organization_clause, record_key_clause,
                    record_key_clause[0] != '\0' ? unit->files[index].cobol_name : "",
                    record_key_clause[0] != '\0' ? "-RECORD" : "",
                    unit->files[index].cobol_name) < 0
                || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
    }
    if (cobol_text_builder_append_line(&builder, "       DATA DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (unit->file_count > 0)
    {
        if (cobol_text_builder_append_line(&builder, "       FILE SECTION.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->file_count)
        {
            size_t record_length;

            record_length = unit->files[index].record_length;
            if (record_length == 0)
                record_length = 256;
            if (std::snprintf(line, sizeof(line), "       FD %s.",
                    unit->files[index].cobol_name) < 0
                || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       01 %s-RECORD PIC X(%zu).",
                    unit->files[index].cobol_name, record_length) < 0
                || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
    }
    if (cobol_text_builder_append_line(&builder, "       WORKING-STORAGE SECTION.") != FT_SUCCESS)
        goto cleanup;
    if (cblc_unit_uses_exceptions(unit) && !entry_uses_exception_context)
    {
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-EX-CONTEXT.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-ACTIVE PIC X VALUE 'N'.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-MATCHED PIC X VALUE 'N'.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-TYPE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-DYNAMIC-TYPE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-SIZE PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-OWNER PIC X VALUE 'N'.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-FILE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-LINE PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-COLUMN PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-RAISING PIC X VALUE 'N'.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-CLEANING PIC X VALUE 'N'.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-SECONDARY-TYPE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-SECONDARY-SOURCE-LINE PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-SECONDARY-SOURCE-COLUMN PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-INT-PAYLOAD PIC S9(9) VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-STRING.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "             10 CBLC-EX-STRING-LEN PIC 9(4) COMP VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "             10 CBLC-EX-STRING-BUF PIC X(256).") != FT_SUCCESS)
            goto cleanup;
    }
    if (unit->file_count > 0
        && cobol_text_builder_append_line(&builder,
            "       01 CBLC-EOF-FLAG PIC X VALUE 'N'.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->file_count)
    {
        if (std::snprintf(line, sizeof(line), "       01 %s-STATUS PIC X(2) VALUE SPACES.",
                unit->files[index].cobol_name) < 0
            || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    index = 0;
    while (index < unit->copy_include_count)
    {
        char copy_name[TRANSPILE_IDENTIFIER_MAX];
        size_t name_index;

        ft_strlcpy(copy_name, unit->copy_includes[index].name, sizeof(copy_name));
        name_index = 0;
        while (copy_name[name_index] != '\0')
        {
            if (copy_name[name_index] >= 'a' && copy_name[name_index] <= 'z')
                copy_name[name_index] = static_cast<char>(copy_name[name_index] - 'a' + 'A');
            name_index += 1;
        }
        if (std::snprintf(line, sizeof(line), "       COPY %s.", copy_name) < 0)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    index = 0;
    while (index < unit->data_count)
    {
        if (unit->data_items[index].is_template_item)
        {
            index += 1;
            continue ;
        }
        if (unit->data_items[index].is_alias || std::strchr(unit->data_items[index].source_name, '.')
            || std::strstr(unit->data_items[index].source_name, "->")
            || cblc_is_entry_parameter_item(unit, &unit->data_items[index])
            || cblc_is_entry_return_item(unit, &unit->data_items[index]))
        {
            if (cblc_string_item_is_member_storage(&unit->data_items[index])
                && (cblc_string_item_uses_dynamic_storage(&unit->data_items[index])
                    || cblc_string_item_array_uses_dynamic_storage(&unit->data_items[index])))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
            continue ;
        }
        if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (unit->data_items[index].initializer_text[0] == '\'')
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) VALUE \"%c\".",
                            unit->data_items[index].cobol_name,
                            unit->data_items[index].length,
                            unit->data_items[index].initializer_text[1]) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu).",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].length) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_INT)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9)%s OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        cblc_native_numeric_storage_format(unit,
                            unit->data_items[index].cobol_name),
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu)%s VALUE %s.",
                        unit->data_items[index].cobol_name,
                        cblc_native_result_digits(unit,
                            unit->data_items[index].cobol_name),
                        cblc_native_numeric_storage_format(unit,
                            unit->data_items[index].cobol_name),
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu)%s.",
                    unit->data_items[index].cobol_name,
                    cblc_native_result_digits(unit,
                        unit->data_items[index].cobol_name),
                    cblc_native_numeric_storage_format(unit,
                        unit->data_items[index].cobol_name)) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_DOUBLE)
        {
            if (std::snprintf(line, sizeof(line), "       01 %s USAGE COMP-2.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            if (std::snprintf(line, sizeof(line), "          05 %s-LEN PIC 9(4) COMP VALUE %zu.",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].is_const
                        ? unit->data_items[index].initializer_length
                        : 0) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_string_item_array_uses_dynamic_storage(&unit->data_items[index]))
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-CAP PIC 9(4) COMP VALUE 0.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line),
                        "          05 %s-PTR USAGE POINTER VALUE NULL.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_string_item_uses_dynamic_storage(&unit->data_items[index]))
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-CAP PIC 9(4) COMP VALUE 0.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line),
                        "          05 %s-PTR USAGE POINTER VALUE NULL.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
            continue ;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_emit_struct_instance(unit, &unit->data_items[index], &builder) != FT_SUCCESS)
            {
                goto cleanup;
            }
            index += 1;
            continue ;
        }
        else if (cblc_data_kind_is_pointer(unit->data_items[index].kind))
        {
            if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER VALUE NULL.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else
        {
            goto cleanup;
        }
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (cblc_emit_reusable_method_results(unit, &builder) != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (!unit->is_native_standard_library
        && cblc_emit_reusable_string_intrinsic_arguments(unit, &builder) != FT_SUCCESS)
        goto cleanup;
    if (cblc_emit_reusable_string_intrinsic_results(unit, &builder) != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (cblc_unit_has_pointer_data(unit))
    {
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CURSOR USAGE POINTER VALUE NULL.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-OFFSET PIC S9(9) VALUE 0.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CHAR BASED PIC X.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT BASED PIC S9(9).") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-PTR BASED USAGE POINTER.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CHAR-2 BASED PIC X.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-2 BASED PIC S9(9).") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-PTR-2 BASED USAGE POINTER.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->data_count)
        {
            if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT_POINTER
                && !unit->data_items[index].is_alias
                && !unit->data_items[index].is_template_item)
            {
                char based_name[TRANSPILE_IDENTIFIER_MAX];
                const t_cblc_struct_type *type;

                if (cblc_build_struct_pointer_based_name(unit->data_items[index].cobol_name,
                        based_name, sizeof(based_name)) != FT_SUCCESS)
                    goto cleanup;
                if (std::strcmp(unit->data_items[index].struct_type_name, "string") == 0)
                {
                    t_cblc_data_item string_pointer_item;

                    string_pointer_item = unit->data_items[index];
                    ft_strlcpy(string_pointer_item.cobol_name, based_name,
                        sizeof(string_pointer_item.cobol_name));
                    if (cblc_emit_string_pointer_fields(&string_pointer_item, &builder)
                        != FT_SUCCESS)
                        goto cleanup;
                    index += 1;
                    continue ;
                }
                type = cblc_find_struct_type(unit, unit->data_items[index].struct_type_name);
                if (!type)
                {
                    goto cleanup;
                }
                if (std::snprintf(line, sizeof(line), "       01 %s BASED.",
                        based_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (cblc_emit_struct_fields(unit, type, based_name, 5, &builder)
                    != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
        }
    }
    if (entry_function && cblc_is_external_entry_function(unit)
        && (entry_function->parameter_count > 0
            || entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID
            || entry_uses_exception_context))
    {
        if (cobol_text_builder_append_line(&builder, "       LINKAGE SECTION.") != FT_SUCCESS)
            goto cleanup;
        if (entry_uses_exception_context
            && (cobol_text_builder_append_line(&builder,
                    "       01 CBLC-EX-CONTEXT.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-ACTIVE PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-MATCHED PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-TYPE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-DYNAMIC-TYPE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-SIZE PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-OWNER PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-FILE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-LINE PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-COLUMN PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-RAISING PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-CLEANING PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SECONDARY-TYPE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SECONDARY-SOURCE-LINE PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SECONDARY-SOURCE-COLUMN PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-INT-PAYLOAD PIC S9(9).") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-STRING.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "             10 CBLC-EX-STRING-LEN PIC 9(4) COMP.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "             10 CBLC-EX-STRING-BUF PIC X(256).") != FT_SUCCESS))
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT)
            {
                size_t integer_digits;
                int status_parameter;

                integer_digits = 9;
                status_parameter = cblc_native_parameter_is_status(entry_function, index);
                if (unit->is_native_standard_library && index == 2
                    && (std::strcmp(unit->program_name, "CBLC-ATOL") == 0
                        || std::strcmp(unit->program_name, "CBLC-ATOL-STRING") == 0))
                    integer_digits = 18;
                if (unit->is_native_standard_library && index == 2
                    && (std::strcmp(unit->program_name, "CBLC-ATOLL") == 0
                        || std::strcmp(unit->program_name, "CBLC-ATOLL-STRING") == 0))
                    integer_digits = 36;
                if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-ABS") == 0)
                    integer_digits = 18;
                if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 1)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(4).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 2)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(2).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 3)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(2).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 4)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(8) COMP-3.",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (status_parameter
                    && (std::strcmp(unit->program_name, "CBLC-STRCPY") != 0
                        && std::strcmp(unit->program_name, "CBLC-STRNCPY") != 0
                        && std::strcmp(unit->program_name, "CBLC-STRCAT") != 0
                        && std::strcmp(unit->program_name, "CBLC-TOUPPER") != 0
                        && std::strcmp(unit->program_name, "CBLC-TOLOWER") != 0))
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9.",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu)%s.",
                            entry_function->parameters[index].cobol_name,
                            integer_digits,
                            cblc_native_parameter_uses_comp5(unit, entry_function, index)
                                ? " COMP-5" : "") < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE COMP-2.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC X.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
            {
                if (unit->is_native_standard_library
                    && entry_function->parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC X(255).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind
                == TRANSPILE_FUNCTION_PARAMETER_STRING)
            {
                size_t parameter_length;

                parameter_length = entry_function->parameters[index].length;
                if (parameter_length == 0)
                    parameter_length = 255;
                if (std::snprintf(line, sizeof(line), "       01 %s.",
                        entry_function->parameters[index].cobol_name) < 0
                    || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "          05 %s-LEN PIC 9(4) COMP.",
                        entry_function->parameters[index].cobol_name) < 0
                    || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "          05 %s-BUF PIC X(%zu).",
                        entry_function->parameters[index].cobol_name,
                        parameter_length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                index += 1;
                continue ;
            }
            else
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            const t_cblc_data_item *return_item;

            if (entry_function->return_item_index < 0
                || static_cast<size_t>(entry_function->return_item_index) >= unit->data_count)
                goto cleanup;
            return_item = &unit->data_items[entry_function->return_item_index];
            if (return_item->kind == CBLC_DATA_KIND_INT)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu).",
                        entry_function->return_cobol_name,
                        cblc_native_result_digits(unit,
                            entry_function->return_cobol_name)) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (return_item->kind == CBLC_DATA_KIND_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE COMP-2.",
                        entry_function->return_cobol_name) < 0
                    || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (return_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_emit_struct_instance(unit, return_item, &builder) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_data_kind_is_pointer(return_item->kind))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER.",
                        entry_function->return_cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else
                goto cleanup;
        }
        ft_strlcpy(line, "       PROCEDURE DIVISION USING", sizeof(line));
        if (entry_uses_exception_context)
            ft_strlcat(line, " CBLC-EX-CONTEXT", sizeof(line));
        if (entry_function->parameter_count == 0
            && entry_function->return_kind == CBLC_FUNCTION_RETURN_VOID
            && !entry_uses_exception_context)
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            char clause[TRANSPILE_IDENTIFIER_MAX + 32];
            int by_value;

            by_value = unit->is_native_standard_library
                && entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT
                && cblc_native_parameter_is_by_value(unit, index);
            if (std::snprintf(clause, sizeof(clause), " %s%s",
                    by_value ? "BY VALUE " : (unit->is_native_standard_library
                        ? "BY REFERENCE " : ""),
                    entry_function->parameters[index].cobol_name) < 0)
                goto cleanup;
            if (std::strlen(line) + std::strlen(clause) >= 65)
            {
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                ft_strlcpy(line, "           ", sizeof(line));
            }
            ft_strlcat(line, clause, sizeof(line));
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            char clause[TRANSPILE_IDENTIFIER_MAX + 2];

            if (std::snprintf(clause, sizeof(clause), " %s%s",
                    unit->is_native_standard_library ? "BY REFERENCE " : "",
                    entry_function->return_cobol_name) < 0)
                goto cleanup;
            if (std::strlen(line) + std::strlen(clause) >= 65)
            {
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                ft_strlcpy(line, "           ", sizeof(line));
            }
            ft_strlcat(line, clause, sizeof(line));
        }
        if (std::strlen(line) + 2 >= sizeof(line))
            goto cleanup;
        ft_strlcat(line, ".", sizeof(line));
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
    }
    else
    {
        if (cobol_text_builder_append_line(&builder, "       PROCEDURE DIVISION.") != FT_SUCCESS)
            goto cleanup;
    }
    if (unit->function_count > 0)
    {
        if (cblc_emit_function(unit, &unit->functions[entry_index], &builder, 1) != FT_SUCCESS)
        {
            goto cleanup;
        }
        index = 0;
        while (index < unit->function_count)
        {
            if (index != entry_index && !unit->functions[index].is_template)
            {
                if (cblc_emit_function(unit, &unit->functions[index], &builder, 0) != FT_SUCCESS)
                {
                    goto cleanup;
                }
            }
            index += 1;
        }
    }
    else
    {
        if (cobol_text_builder_append_line(&builder, "MAIN.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
            goto cleanup;
    }
        if (cblc_emit_reusable_method_paragraphs(unit, &builder) != FT_SUCCESS)
        {
            goto cleanup;
        }
    if (cblc_emit_reusable_string_intrinsic_paragraphs(unit, &builder) != FT_SUCCESS)
        goto cleanup;
    if (!unit->is_native_standard_library
        && cblc_emit_reusable_parameterized_string_intrinsic_paragraphs(unit, &builder)
            != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (cblc_emit_reusable_lifecycle_paragraphs(unit, &builder) != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (cblc_unit_uses_exceptions(unit)
        && (cobol_text_builder_append_line(&builder, "       CBLC-TERMINATE.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Fatal CBL-C double exception\"") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception type: \" CBLC-EX-TYPE-ID") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception file ID: \" CBLC-EX-SOURCE-FILE-ID") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception line: \" CBLC-EX-SOURCE-LINE") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception column: \" CBLC-EX-SOURCE-COLUMN") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Secondary exception type: \" CBLC-EX-SECONDARY-TYPE-ID") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Secondary exception line: \" CBLC-EX-SECONDARY-SOURCE-LINE") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Secondary exception column: \" CBLC-EX-SECONDARY-SOURCE-COLUMN") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "") != FT_SUCCESS))
        goto cleanup;
    if (std::snprintf(line, sizeof(line), "       END PROGRAM %s.",
            unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
        goto cleanup;
    *out_text = builder.data;
    builder.data = NULL;
cleanup:
    cobol_text_builder_dispose(&builder);
    if (!*out_text)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

#include "src/cblc/transpiler_cblc_context_integration.inc"
