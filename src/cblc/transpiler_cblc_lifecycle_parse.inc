static int cblc_register_parameter_destructors(
    const t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_append_parameter_destructor_statements(
    const t_cblc_translation_unit *unit, t_cblc_function *function);

static int cblc_type_has_throwing_constructor(
    const t_cblc_struct_type *type)
{
    if (!type)
        return (0);
    for (size_t constructor_index = 0;
        constructor_index < type->constructor_count; ++constructor_index)
    {
        const t_cblc_constructor *constructor =
            &type->constructors[constructor_index];
        if (constructor->exception_types_unknown
            || constructor->exception_type_count != 0)
            return (1);
        for (size_t statement_index = 0;
            statement_index < constructor->statement_count; ++statement_index)
            if (constructor->statements[statement_index].type
                    == CBLC_STATEMENT_THROW
                || constructor->statements[statement_index].exception_may_throw)
                return (1);
    }
    return (0);
}

static int cblc_bind_lifecycle_scope(t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t *saved_data_count)
{
    const char *root_source;
    const char *root_cobol;
    size_t alias_start;
    size_t alias_end;
    size_t index;

    if (!unit || !type || !saved_data_count)
        return (FT_FAILURE);
    *saved_data_count = unit->data_count;
    root_source = "this";
    root_cobol = "CBLC-THIS";
    if (cblc_add_temp_alias_item(unit, root_source, root_cobol, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_add_struct_instance_field_items(unit, type, root_source, root_cobol,
            NULL, 0, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    alias_start = *saved_data_count + 1;
    alias_end = unit->data_count;
    index = alias_start;
    while (index < alias_end)
    {
        char source_name[TRANSPILE_IDENTIFIER_MAX];
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char struct_type_name[TRANSPILE_IDENTIFIER_MAX];
        const char *declared_struct_type;
        t_cblc_data_kind kind;
        size_t length;
        size_t array_count;
        int is_const;

        ft_strlcpy(source_name, unit->data_items[index].source_name, sizeof(source_name));
        ft_strlcpy(cobol_name, unit->data_items[index].cobol_name, sizeof(cobol_name));
        ft_strlcpy(struct_type_name, unit->data_items[index].struct_type_name,
            sizeof(struct_type_name));
        kind = unit->data_items[index].kind;
        length = unit->data_items[index].length;
        array_count = unit->data_items[index].array_count;
        is_const = unit->data_items[index].is_const;
        if (std::strncmp(source_name, "this.", 5) == 0)
        {
            declared_struct_type = NULL;
            if (struct_type_name[0] != '\0')
                declared_struct_type = struct_type_name;
            if (cblc_add_temp_alias_item(unit, source_name + 5, cobol_name,
                    kind, length, array_count, declared_struct_type, is_const) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static void cblc_unbind_lifecycle_scope(t_cblc_translation_unit *unit, size_t saved_data_count)
{
    if (!unit)
        return ;
    if (saved_data_count <= unit->data_count)
        unit->data_count = saved_data_count;
}

static void cblc_unbind_method_scope_preserving_locals(
    t_cblc_translation_unit *unit, size_t saved_data_count,
    const char *method_function_name)
{
    size_t read_index;
    size_t write_index;

    if (!unit || !method_function_name || saved_data_count > unit->data_count)
        return ;
    write_index = saved_data_count;
    read_index = saved_data_count;
    while (read_index < unit->data_count)
    {
        const t_cblc_data_item *item;

        item = &unit->data_items[read_index];
        if (item->is_function_local && !item->is_alias
            && std::strcmp(item->owner_function_name, method_function_name) == 0)
        {
            if (write_index != read_index)
                unit->data_items[write_index] = *item;
            write_index += 1;
        }
        read_index += 1;
    }
    unit->data_count = write_index;
}

static int cblc_register_scope(t_cblc_translation_unit *unit, size_t parent_id,
    const char *owner_function_name, size_t *out_scope_id)
{
    t_cblc_scope *scopes;
    t_cblc_scope *scope;
    size_t capacity;

    if (!unit || !out_scope_id || parent_id >= unit->scope_count)
        return (FT_FAILURE);
    if (unit->scope_count >= unit->scope_capacity)
    {
        capacity = unit->scope_capacity == 0 ? 4 : unit->scope_capacity * 2;
        scopes = static_cast<t_cblc_scope *>(cma_calloc(capacity, sizeof(*scopes)));
        if (!scopes)
            return (FT_FAILURE);
        if (unit->scopes)
        {
            std::memcpy(scopes, unit->scopes, unit->scope_count * sizeof(*scopes));
            cma_free(unit->scopes);
        }
        unit->scopes = scopes;
        unit->scope_capacity = capacity;
    }
    scope = &unit->scopes[unit->scope_count];
    std::memset(scope, 0, sizeof(*scope));
    scope->id = unit->scope_count;
    scope->parent_id = parent_id;
    scope->depth = unit->scopes[parent_id].depth + 1;
    if (owner_function_name)
        ft_strlcpy(scope->owner_function_name, owner_function_name,
            sizeof(scope->owner_function_name));
    *out_scope_id = scope->id;
    unit->scope_count += 1;
    return (FT_SUCCESS);
}

static void cblc_exception_source_position(const t_cblc_translation_unit *unit,
    const char *position, size_t *line, size_t *column);
static void cblc_capture_statement_source_spans(
    const t_cblc_translation_unit *unit, t_cblc_function *function,
    size_t first_statement, const char *start, const char *end);
static int cblc_build_unique_local_storage_names(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const char *local_identifier, char *actual_source_name,
    size_t actual_source_size, char *actual_cobol_name,
    size_t actual_cobol_size);
static int cblc_parse_method_call_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item);

static int cblc_condition_method_chain_end(const char *condition,
    size_t start, size_t *out_end, size_t *out_call_count,
    int *out_has_arguments)
{
    size_t cursor;
    size_t call_count;
    int has_arguments;

    if (!condition || !out_end || !out_call_count || !out_has_arguments
        || !cblc_is_identifier_char(condition[start]))
        return (FT_FAILURE);
    cursor = start;
    while (cblc_is_identifier_char(condition[cursor]))
        cursor += 1;
    call_count = 0;
    has_arguments = 0;
    while (1)
    {
        size_t open_index;
        size_t depth;
        int quote;

        while (condition[cursor] == ' ' || condition[cursor] == '\t')
            cursor += 1;
        if (condition[cursor] == '-')
        {
            if (condition[cursor + 1] != '>')
                break ;
            cursor += 2;
        }
        else if (condition[cursor] == '.')
            cursor += 1;
        else
            break ;
        while (condition[cursor] == ' ' || condition[cursor] == '\t')
            cursor += 1;
        if (!cblc_is_identifier_char(condition[cursor]))
            return (FT_FAILURE);
        while (cblc_is_identifier_char(condition[cursor]))
            cursor += 1;
        while (condition[cursor] == ' ' || condition[cursor] == '\t')
            cursor += 1;
        if (condition[cursor] != '(')
            return (FT_FAILURE);
        open_index = cursor++;
        depth = 1;
        quote = 0;
        while (condition[cursor] != '\0' && depth != 0)
        {
            if (quote != 0)
            {
                if (condition[cursor] == quote
                    && (cursor == 0 || condition[cursor - 1] != '\\'))
                    quote = 0;
            }
            else if (condition[cursor] == '\'' || condition[cursor] == '"')
                quote = condition[cursor];
            else if (condition[cursor] == '(')
                depth += 1;
            else if (condition[cursor] == ')')
                depth -= 1;
            cursor += 1;
        }
        if (depth != 0 || quote != 0)
            return (FT_FAILURE);
        if (cursor - open_index > 2)
            has_arguments = 1;
        call_count += 1;
        while (condition[cursor] == ' ' || condition[cursor] == '\t')
            cursor += 1;
        if (condition[cursor] == '-' && condition[cursor + 1] == '>')
            continue ;
        *out_end = cursor;
        *out_call_count = call_count;
        *out_has_arguments = has_arguments;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_condition_find_method_occurrence(const char *expression,
    size_t expression_length, const char *method_name,
    const size_t *used_offsets, size_t used_count, size_t *out_offset)
{
    const char *search;
    const char *candidate;
    size_t best_offset;
    size_t best_depth;
    int found;

    if (!expression || !method_name || method_name[0] == '\0'
        || !out_offset)
        return (FT_FAILURE);
    search = expression;
    best_offset = 0;
    best_depth = 0;
    found = 0;
    while ((candidate = std::strstr(search, method_name)) != NULL
        && static_cast<size_t>(candidate - expression) < expression_length)
    {
        size_t candidate_offset;
        size_t after_name;
        size_t before_name;
        size_t nesting;
        size_t scan_index;
        int quote;
        int is_used;

        candidate_offset = static_cast<size_t>(candidate - expression);
        after_name = candidate_offset + std::strlen(method_name);
        before_name = candidate_offset;
        while (expression[after_name] == ' ' || expression[after_name] == '\t')
            after_name += 1;
        while (before_name > 0
            && (expression[before_name - 1] == ' '
                || expression[before_name - 1] == '\t'))
            before_name -= 1;
        if ((candidate_offset > 0
                && cblc_is_identifier_char(expression[candidate_offset - 1]))
            || cblc_is_identifier_char(expression[after_name])
            || expression[after_name] != '('
            || before_name == 0
            || (expression[before_name - 1] != '.'
                && expression[before_name - 1] != '>'))
        {
            search = candidate + 1;
            continue ;
        }
        is_used = 0;
        for (size_t used_index = 0; used_index < used_count; ++used_index)
            if (used_offsets[used_index] == candidate_offset)
                is_used = 1;
        if (is_used)
        {
            search = candidate + 1;
            continue ;
        }
        nesting = 0;
        quote = 0;
        scan_index = 0;
        while (scan_index < candidate_offset)
        {
            if (quote != 0)
            {
                if (expression[scan_index] == quote
                    && (scan_index == 0
                        || expression[scan_index - 1] != '\\'))
                    quote = 0;
            }
            else if (expression[scan_index] == '\''
                || expression[scan_index] == '"')
                quote = expression[scan_index];
            else if (expression[scan_index] == '(')
                nesting += 1;
            else if (expression[scan_index] == ')' && nesting > 0)
                nesting -= 1;
            scan_index += 1;
        }
        if (!found || nesting > best_depth
            || (nesting == best_depth && candidate_offset < best_offset))
        {
            best_offset = candidate_offset;
            best_depth = nesting;
            found = 1;
        }
        search = candidate + 1;
    }
    if (!found)
        return (FT_FAILURE);
    *out_offset = best_offset;
    return (FT_SUCCESS);
}

/* Route argument-taking and chained condition calls through the normal method
 * assignment parser so overload resolution, pointer borrowing, and argument
 * temporary destruction stay consistent with ordinary expressions. */
static int cblc_lower_condition_method_chain(t_cblc_translation_unit *unit,
    t_cblc_function *function, const char *source_condition,
    const char *condition, size_t start, size_t end, char *rewritten,
    size_t rewritten_size, size_t *output_length)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    char local_base[TRANSPILE_IDENTIFIER_MAX];
    char local_source[TRANSPILE_IDENTIFIER_MAX];
    char local_cobol[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_data_kind result_kinds[3] = {
        CBLC_DATA_KIND_INT, CBLC_DATA_KIND_DOUBLE, CBLC_DATA_KIND_CHAR
    };
    t_cblc_data_item *target_item;
    const char *expression_cursor;
    size_t original_data_count;
    size_t original_statement_count;
    size_t original_destructor_count;
    size_t first_statement;
    size_t expression_length;
    size_t source_offset;
    size_t used_method_offsets[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t used_method_count;
    size_t candidate_index;
    int parse_result;

    if (!unit || !function || !source_condition || !condition || start >= end
        || !rewritten || !rewritten_size || !output_length)
        return (-1);
    expression_length = end - start;
    if (expression_length + 2 > sizeof(expression))
        return (0);
    std::memcpy(expression, condition + start, expression_length);
    expression[expression_length] = ';';
    expression[expression_length + 1] = '\0';
    original_data_count = unit->data_count;
    original_statement_count = function->statement_count;
    original_destructor_count = function->local_destructor_count;
    candidate_index = 0;
    parse_result = FT_FAILURE;
    while (candidate_index < sizeof(result_kinds) / sizeof(result_kinds[0]))
    {
        unit->data_count = original_data_count;
        function->statement_count = original_statement_count;
        function->local_destructor_count = original_destructor_count;
        if (std::snprintf(local_base, sizeof(local_base),
                "CBLC_CONDITION_CHAIN_%zu", function->statement_count) < 0
            || cblc_build_unique_local_storage_names(unit, function, local_base,
                local_source, sizeof(local_source), local_cobol,
                sizeof(local_cobol)) != FT_SUCCESS
            || cblc_add_named_data_item(unit, local_source, local_cobol,
                result_kinds[candidate_index], 0, 0, NULL,
                function->source_name, 1, 0, 0) != FT_SUCCESS)
        {
            unit->data_count = original_data_count;
            function->statement_count = original_statement_count;
            function->local_destructor_count = original_destructor_count;
            return (-1);
        }
        target_item = cblc_find_data_item(unit, local_source);
        expression_cursor = expression;
        first_statement = function->statement_count;
        parse_result = target_item
            ? cblc_parse_method_call_assignment(&expression_cursor, unit,
                function, target_item) : FT_FAILURE;
        if (parse_result == FT_SUCCESS && expression_cursor != expression)
            break ;
        candidate_index += 1;
    }
    if (candidate_index >= sizeof(result_kinds) / sizeof(result_kinds[0]))
    {
        unit->data_count = original_data_count;
        function->statement_count = original_statement_count;
        function->local_destructor_count = original_destructor_count;
        return (0);
    }
    if (*output_length + std::strlen(local_source) >= rewritten_size)
    {
        unit->data_count = original_data_count;
        function->statement_count = original_statement_count;
        function->local_destructor_count = original_destructor_count;
        return (-1);
    }
    source_offset = cblc_source_offset(unit, source_condition + start);
    used_method_count = 0;
    for (size_t statement_index = first_statement;
        statement_index < function->statement_count; ++statement_index)
    {
        t_cblc_statement *statement = &function->statements[statement_index];
        size_t method_offset;

        if (statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            continue ;
        if (used_method_count >= sizeof(used_method_offsets)
                / sizeof(used_method_offsets[0])
            || cblc_condition_find_method_occurrence(condition + start,
                expression_length, statement->call_identifier,
                used_method_offsets, used_method_count,
                &method_offset) != FT_SUCCESS)
        {
            unit->data_count = original_data_count;
            function->statement_count = original_statement_count;
            function->local_destructor_count = original_destructor_count;
            return (-1);
        }
        used_method_offsets[used_method_count++] = method_offset;
        statement->source_offset = source_offset + method_offset;
        statement->source_length = std::strlen(statement->call_identifier);
        cblc_exception_source_position(unit,
            source_condition + start + method_offset,
            &statement->exception_source_line,
            &statement->exception_source_column);
        cblc_exception_source_position(unit,
            source_condition + start + method_offset,
            &statement->source_line, &statement->source_column);
    }
    std::memcpy(rewritten + *output_length, local_source,
        std::strlen(local_source));
    *output_length += std::strlen(local_source);
    rewritten[*output_length] = '\0';
    return (1);
}

static int cblc_lower_scalar_method_calls_in_if_condition(
    t_cblc_translation_unit *unit, t_cblc_function *function,
    const char *source_condition, char *condition, size_t condition_size)
{
    char rewritten[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t original_data_count;
    size_t original_statement_count;
    size_t input_index;
    size_t output_length;
    int quote;
    int has_short_circuit;

    if (!unit || !function || !source_condition || !condition
        || condition_size == 0)
        return (FT_FAILURE);
    /* This helper handles one leaf operand. Keep the guard as a safety net:
     * any logical tree reaching it must stay intact rather than being hoisted. */
    has_short_circuit = 0;
    quote = 0;
    input_index = 0;
    while (condition[input_index] != '\0')
    {
        if (quote != 0)
        {
            if (condition[input_index] == quote
                && (input_index == 0 || condition[input_index - 1] != '\\'))
                quote = 0;
        }
        else if (condition[input_index] == '\'' || condition[input_index] == '"')
            quote = condition[input_index];
        else if ((condition[input_index] == '&'
                && condition[input_index + 1] == '&')
            || (condition[input_index] == '|'
                && condition[input_index + 1] == '|'))
        {
            has_short_circuit = 1;
            break ;
        }
        input_index += 1;
    }
    if (has_short_circuit)
        return (FT_SUCCESS);
    original_data_count = unit->data_count;
    original_statement_count = function->statement_count;
    rewritten[0] = '\0';
    input_index = 0;
    output_length = 0;
    quote = 0;
    while (condition[input_index] != '\0')
    {
        size_t token_start;
        size_t receiver_end;
        size_t method_start;
        size_t method_end;
        size_t call_end;
        size_t chain_call_count;
        int chain_has_arguments;

        if (quote != 0)
        {
            char literal_character = condition[input_index++];

            if (output_length + 1 >= sizeof(rewritten))
                goto fail;
            rewritten[output_length++] = literal_character;
            rewritten[output_length] = '\0';
            if (literal_character == quote
                && (input_index < 2 || condition[input_index - 2] != '\\'))
                quote = 0;
            continue ;
        }
        if (condition[input_index] == '\'' || condition[input_index] == '"')
        {
            quote = condition[input_index];
            if (output_length + 1 >= sizeof(rewritten))
                goto fail;
            rewritten[output_length++] = condition[input_index++];
            rewritten[output_length] = '\0';
            continue ;
        }
        if (!cblc_is_identifier_char(condition[input_index]))
        {
            if (output_length + 1 >= sizeof(rewritten))
                goto fail;
            rewritten[output_length++] = condition[input_index++];
            rewritten[output_length] = '\0';
            continue ;
        }
        token_start = input_index;
        while (cblc_is_identifier_char(condition[input_index]))
            input_index += 1;
        receiver_end = input_index;
        if (cblc_condition_method_chain_end(condition, token_start,
                &call_end, &chain_call_count, &chain_has_arguments)
            == FT_SUCCESS && (chain_call_count > 1 || chain_has_arguments))
        {
            int chain_result;

            chain_result = cblc_lower_condition_method_chain(unit, function,
                source_condition, condition, token_start, call_end, rewritten,
                sizeof(rewritten), &output_length);
            if (chain_result < 0)
                goto fail;
            if (chain_result > 0)
            {
                input_index = call_end;
                continue ;
            }
        }
        while (condition[input_index] == ' ' || condition[input_index] == '\t')
            input_index += 1;
        if (condition[input_index] == '-')
        {
            input_index += 1;
            if (condition[input_index] != '>')
            {
                input_index = receiver_end;
                goto copy_identifier;
            }
            input_index += 1;
        }
        else if (condition[input_index] == '.')
            input_index += 1;
        else
        {
            input_index = receiver_end;
            goto copy_identifier;
        }
        while (condition[input_index] == ' ' || condition[input_index] == '\t')
            input_index += 1;
        if (!cblc_is_identifier_char(condition[input_index]))
        {
            input_index = receiver_end;
            goto copy_identifier;
        }
        method_start = input_index;
        while (cblc_is_identifier_char(condition[input_index]))
            input_index += 1;
        method_end = input_index;
        while (condition[input_index] == ' ' || condition[input_index] == '\t')
            input_index += 1;
        if (condition[input_index] != '(')
        {
            input_index = receiver_end;
            goto copy_identifier;
        }
        input_index += 1;
        while (condition[input_index] == ' ' || condition[input_index] == '\t')
            input_index += 1;
        if (condition[input_index] != ')')
        {
            input_index = receiver_end;
            goto copy_identifier;
        }
        call_end = input_index + 1;
        {
            char receiver_name[TRANSPILE_IDENTIFIER_MAX];
            char method_name[TRANSPILE_IDENTIFIER_MAX];
            char local_base[TRANSPILE_IDENTIFIER_MAX];
            char local_source[TRANSPILE_IDENTIFIER_MAX];
            char local_cobol[TRANSPILE_IDENTIFIER_MAX];
            t_cblc_data_item *receiver_item;
            const t_cblc_struct_type *receiver_type;
            const t_cblc_method *method;
            t_cblc_data_kind result_kind;
            t_cblc_statement *statement;

            if (receiver_end - token_start >= sizeof(receiver_name)
                || method_end - method_start >= sizeof(method_name))
                goto fail;
            std::memcpy(receiver_name, condition + token_start,
                receiver_end - token_start);
            receiver_name[receiver_end - token_start] = '\0';
            std::memcpy(method_name, condition + method_start,
                method_end - method_start);
            method_name[method_end - method_start] = '\0';
            receiver_item = cblc_find_data_item(unit, receiver_name);
            receiver_item = cblc_resolve_implicit_member_item(unit,
                receiver_name, receiver_item);
            receiver_type = receiver_item
                ? cblc_find_receiver_type(unit, receiver_item) : NULL;
            method = receiver_type
                ? cblc_find_method_on_type(receiver_type, method_name) : NULL;
            if (!method || method->parameter_count != 0
                || (method->return_kind != CBLC_FUNCTION_RETURN_INT
                    && method->return_kind != CBLC_FUNCTION_RETURN_DOUBLE
                    && method->return_kind != CBLC_FUNCTION_RETURN_CHAR))
            {
                input_index = receiver_end;
                goto copy_identifier;
            }
            result_kind = method->return_kind == CBLC_FUNCTION_RETURN_INT
                ? CBLC_DATA_KIND_INT
                : (method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE
                    ? CBLC_DATA_KIND_DOUBLE : CBLC_DATA_KIND_CHAR);
            if (std::snprintf(local_base, sizeof(local_base),
                    "CBLC_CONDITION_VALUE_%zu", function->statement_count) < 0
                || cblc_build_unique_local_storage_names(unit, function,
                    local_base, local_source, sizeof(local_source), local_cobol,
                    sizeof(local_cobol)) != FT_SUCCESS
                || cblc_add_named_data_item(unit, local_source, local_cobol,
                    result_kind, 0, 0, NULL, function->source_name, 1, 0, 0)
                    != FT_SUCCESS
                || cblc_append_statement(function,
                    CBLC_STATEMENT_METHOD_CALL_ASSIGN, local_cobol,
                    receiver_item->cobol_name, 0) != FT_SUCCESS)
                goto fail;
            statement = &function->statements[function->statement_count - 1];
            statement->source_offset = cblc_source_offset(unit,
                source_condition + method_start);
            statement->source_length = method_end - method_start;
            cblc_exception_source_position(unit,
                source_condition + method_start,
                &statement->exception_source_line,
                &statement->exception_source_column);
            cblc_exception_source_position(unit,
                source_condition + method_start,
                &statement->source_line, &statement->source_column);
            ft_strlcpy(statement->call_identifier, method_name,
                sizeof(statement->call_identifier));
            statement->call_argument_count = 0;
            if (output_length + std::strlen(local_source)
                    >= sizeof(rewritten))
                goto fail;
            std::memcpy(rewritten + output_length, local_source,
                std::strlen(local_source));
            output_length += std::strlen(local_source);
            rewritten[output_length] = '\0';
            input_index = call_end;
            continue ;
        }
copy_identifier:
        if (output_length + receiver_end - token_start >= sizeof(rewritten))
            goto fail;
        std::memcpy(rewritten + output_length, condition + token_start,
            receiver_end - token_start);
        output_length += receiver_end - token_start;
        rewritten[output_length] = '\0';
    }
    if (output_length + 1 > condition_size)
        goto fail;
    ft_strlcpy(condition, rewritten, condition_size);
    return (FT_SUCCESS);
fail:
    unit->data_count = original_data_count;
    function->statement_count = original_statement_count;
    return (FT_FAILURE);
}

static int cblc_condition_strip_outer_parentheses(const char *condition,
    size_t *start, size_t *length)
{
    int changed;

    if (!condition || !start || !length)
        return (FT_FAILURE);
    changed = 1;
    while (changed)
    {
        size_t index;
        size_t depth;
        char quote;

        changed = 0;
        while (*length > 0 && (condition[*start] == ' '
                || condition[*start] == '\t' || condition[*start] == '\n'
                || condition[*start] == '\r'))
        {
            *start += 1;
            *length -= 1;
            changed = 1;
        }
        while (*length > 0 && (condition[*start + *length - 1] == ' '
                || condition[*start + *length - 1] == '\t'
                || condition[*start + *length - 1] == '\n'
                || condition[*start + *length - 1] == '\r'))
        {
            *length -= 1;
            changed = 1;
        }
        if (*length < 2 || condition[*start] != '('
            || condition[*start + *length - 1] != ')')
            continue ;
        depth = 0;
        quote = '\0';
        index = *start;
        while (index < *start + *length)
        {
            char current;

            current = condition[index];
            if (quote != '\0')
            {
                if (current == quote && (index == *start
                        || condition[index - 1] != '\\'))
                    quote = '\0';
            }
            else if (current == '\'' || current == '"')
                quote = current;
            else if (current == '(')
                depth += 1;
            else if (current == ')')
            {
                if (depth == 0)
                    return (FT_FAILURE);
                depth -= 1;
                if (depth == 0 && index != *start + *length - 1)
                    break ;
            }
            index += 1;
        }
        if (index == *start + *length && depth == 0 && quote == '\0')
        {
            *start += 1;
            *length -= 2;
            while (*length > 0 && (condition[*start] == ' '
                    || condition[*start] == '\t' || condition[*start] == '\n'
                    || condition[*start] == '\r'))
            {
                *start += 1;
                *length -= 1;
            }
            while (*length > 0 && (condition[*start + *length - 1] == ' '
                    || condition[*start + *length - 1] == '\t'
                    || condition[*start + *length - 1] == '\n'
                    || condition[*start + *length - 1] == '\r'))
                *length -= 1;
            changed = 1;
        }
    }
    return (FT_SUCCESS);
}

static int cblc_condition_find_logical_operator(const char *condition,
    size_t start, size_t length, const char *logical_operator,
    size_t *operator_position)
{
    size_t index;
    size_t depth;
    size_t operator_length;
    char quote;

    if (!condition || !logical_operator || !operator_position)
        return (FT_FAILURE);
    operator_length = std::strlen(logical_operator);
    depth = 0;
    quote = '\0';
    index = start;
    while (index < start + length)
    {
        char current;

        current = condition[index];
        if (quote != '\0')
        {
            if (current == quote && (index == start
                    || condition[index - 1] != '\\'))
                quote = '\0';
        }
        else if (current == '\'' || current == '"')
            quote = current;
        else if (current == '(')
            depth += 1;
        else if (current == ')')
        {
            if (depth == 0)
                return (FT_FAILURE);
            depth -= 1;
        }
        else if (depth == 0 && index + operator_length <= start + length
            && std::memcmp(condition + index, logical_operator,
                operator_length) == 0)
        {
            *operator_position = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    if (depth != 0 || quote != '\0')
        return (FT_FAILURE);
    *operator_position = static_cast<size_t>(-1);
    return (FT_SUCCESS);
}

static int cblc_add_condition_integer(t_cblc_translation_unit *unit,
    t_cblc_function *function, const char *stem, char *source_name,
    size_t source_size, char *cobol_name, size_t cobol_size)
{
    char local_base[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !function || !stem || !source_name || source_size == 0
        || !cobol_name || cobol_size == 0
        || std::snprintf(local_base, sizeof(local_base), "%s_%zu", stem,
            function->statement_count) < 0
        || cblc_build_unique_local_storage_names(unit, function, local_base,
            source_name, source_size, cobol_name, cobol_size) != FT_SUCCESS
        || cblc_add_named_data_item(unit, source_name, cobol_name,
            CBLC_DATA_KIND_INT, 0, 0, NULL, function->source_name,
            1, 0, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_append_condition_assignment(t_cblc_function *function,
    const char *target, const char *literal)
{
    return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
        target, literal, 1));
}

/* Emit each logical tree as left-first branches. Every path assigns a 0/1
 * result, and a right operand exists only inside the branch that evaluates it. */
static int cblc_lower_boolean_expression(t_cblc_translation_unit *unit,
    t_cblc_function *function, const char *source_condition,
    const char *source_origin,
    size_t expression_start, size_t expression_length,
    const char *result_source, const char *result_cobol, size_t recursion_depth)
{
    char left_source[TRANSPILE_IDENTIFIER_MAX];
    char left_cobol[TRANSPILE_IDENTIFIER_MAX];
    size_t operator_position;
    size_t expression_end;
    size_t left_start;
    size_t left_length;
    size_t right_start;
    size_t right_length;
    const char *logical_operator;
    int is_or;

    if (!unit || !function || !source_condition || !source_origin || !result_source
        || !result_cobol || recursion_depth > 64 || expression_length == 0)
        return (FT_FAILURE);
    if (cblc_condition_strip_outer_parentheses(source_condition,
            &expression_start, &expression_length) != FT_SUCCESS)
        return (FT_FAILURE);
    expression_end = expression_start + expression_length;
    operator_position = static_cast<size_t>(-1);
    if (cblc_condition_find_logical_operator(source_condition, expression_start,
            expression_length, "||", &operator_position) != FT_SUCCESS)
        return (FT_FAILURE);
    logical_operator = "||";
    is_or = 1;
    if (operator_position == static_cast<size_t>(-1))
    {
        if (cblc_condition_find_logical_operator(source_condition,
                expression_start, expression_length, "&&",
                &operator_position) != FT_SUCCESS)
            return (FT_FAILURE);
        logical_operator = "&&";
        is_or = 0;
    }
    if (operator_position == static_cast<size_t>(-1))
    {
        char leaf[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t leaf_start;
        size_t leaf_length;

        leaf_start = expression_start;
        leaf_length = expression_length;
        while (leaf_length > 0 && (source_condition[leaf_start] == ' '
                || source_condition[leaf_start] == '\t'
                || source_condition[leaf_start] == '\n'
                || source_condition[leaf_start] == '\r'))
        {
            leaf_start += 1;
            leaf_length -= 1;
        }
        while (leaf_length > 0 && (source_condition[leaf_start + leaf_length - 1] == ' '
                || source_condition[leaf_start + leaf_length - 1] == '\t'
                || source_condition[leaf_start + leaf_length - 1] == '\n'
                || source_condition[leaf_start + leaf_length - 1] == '\r'))
            leaf_length -= 1;
        if (leaf_length == 0 || leaf_length >= sizeof(leaf))
            return (FT_FAILURE);
        std::memcpy(leaf, source_condition + leaf_start, leaf_length);
        leaf[leaf_length] = '\0';
        if (cblc_lower_scalar_method_calls_in_if_condition(unit, function,
                source_origin + leaf_start, leaf, sizeof(leaf)) != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_IF_BEGIN, NULL,
                leaf, 0) != FT_SUCCESS
            || cblc_append_condition_assignment(function, result_cobol, "1")
                != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_IF_ELSE,
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_append_condition_assignment(function, result_cobol, "0")
                != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_IF_END,
                NULL, NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (operator_position <= expression_start
        || operator_position + std::strlen(logical_operator) >= expression_end)
        return (FT_FAILURE);
    left_start = expression_start;
    left_length = operator_position - expression_start;
    right_start = operator_position + std::strlen(logical_operator);
    right_length = expression_end - right_start;
    if (cblc_add_condition_integer(unit, function, "CBLC_BOOLEAN_OPERAND",
            left_source, sizeof(left_source), left_cobol,
            sizeof(left_cobol)) != FT_SUCCESS
        || cblc_lower_boolean_expression(unit, function, source_condition,
            source_origin, left_start, left_length, left_source, left_cobol,
            recursion_depth + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    {
        char branch_condition[TRANSPILE_STATEMENT_TEXT_MAX];

        if (std::snprintf(branch_condition, sizeof(branch_condition),
                "%s == 1", left_source) < 0
            || cblc_append_statement(function, CBLC_STATEMENT_IF_BEGIN, NULL,
                branch_condition, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (is_or)
    {
        if (cblc_append_condition_assignment(function, result_cobol, "1")
                != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_IF_ELSE,
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_lower_boolean_expression(unit, function, source_condition,
                source_origin, right_start, right_length, result_source, result_cobol,
                recursion_depth + 1) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (cblc_lower_boolean_expression(unit, function, source_condition,
                source_origin, right_start, right_length, result_source, result_cobol,
                recursion_depth + 1) != FT_SUCCESS
        || cblc_append_statement(function, CBLC_STATEMENT_IF_ELSE,
            NULL, NULL, 0) != FT_SUCCESS
        || cblc_append_condition_assignment(function, result_cobol, "0")
            != FT_SUCCESS)
        return (FT_FAILURE);
    return (cblc_append_statement(function, CBLC_STATEMENT_IF_END,
        NULL, NULL, 0));
}

static int cblc_lower_short_circuit_condition(t_cblc_translation_unit *unit,
    t_cblc_function *function, const char *source_condition,
    char *condition, size_t condition_size, int *was_lowered)
{
    size_t operator_position;
    char result_source[TRANSPILE_IDENTIFIER_MAX];
    char result_cobol[TRANSPILE_IDENTIFIER_MAX];
    char rewritten[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t original_data_count;
    size_t original_statement_count;
    size_t expression_start;
    size_t condition_length;

    if (!unit || !function || !source_condition || !condition
        || condition_size == 0 || !was_lowered)
        return (FT_FAILURE);
    *was_lowered = 0;
    condition_length = std::strlen(condition);
    expression_start = 0;
    if (cblc_condition_strip_outer_parentheses(condition, &expression_start,
            &condition_length) != FT_SUCCESS)
        return (FT_FAILURE);
    operator_position = static_cast<size_t>(-1);
    if (cblc_condition_find_logical_operator(condition, expression_start,
            condition_length,
            "||", &operator_position) != FT_SUCCESS)
        return (FT_FAILURE);
    if (operator_position == static_cast<size_t>(-1)
        && cblc_condition_find_logical_operator(condition, expression_start,
            condition_length,
            "&&", &operator_position) != FT_SUCCESS)
        return (FT_FAILURE);
    if (operator_position == static_cast<size_t>(-1))
        return (FT_SUCCESS);
    original_data_count = unit->data_count;
    original_statement_count = function->statement_count;
    if (cblc_add_condition_integer(unit, function, "CBLC_BOOLEAN_RESULT",
            result_source, sizeof(result_source), result_cobol,
            sizeof(result_cobol)) != FT_SUCCESS
        || cblc_lower_boolean_expression(unit, function, condition,
            source_condition, expression_start, condition_length,
            result_source, result_cobol, 0) != FT_SUCCESS
        || std::snprintf(rewritten, sizeof(rewritten), "%s == 1",
            result_source) < 0
        || std::strlen(rewritten) >= condition_size)
    {
        unit->data_count = original_data_count;
        function->statement_count = original_statement_count;
        return (FT_FAILURE);
    }
    ft_strlcpy(condition, rewritten, condition_size);
    *was_lowered = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_condition(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, int lower_scalar_method_calls,
    char *condition, size_t condition_size)
{
    const char *condition_start;
    size_t condition_length;
    size_t depth;
    char quote;
    char raw_condition[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t normalized_length;
    size_t condition_index;
    int logical_condition_lowered;

    if (!cursor || !*cursor || !unit || !condition || condition_size == 0)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    condition_start = *cursor;
    depth = 1;
    quote = '\0';
    while (**cursor != '\0' && depth > 0)
    {
        if (quote != '\0')
        {
            if (**cursor == quote && (*cursor == condition_start
                    || (*cursor)[-1] != '\\'))
                quote = '\0';
        }
        else if (**cursor == '\'' || **cursor == '"')
            quote = **cursor;
        else if (**cursor == '(')
            depth += 1;
        else if (**cursor == ')')
            depth -= 1;
        *cursor += 1;
    }
    if (depth != 0)
        return (FT_FAILURE);
    condition_length = static_cast<size_t>((*cursor - 1) - condition_start);
    while (condition_length > 0
        && (condition_start[condition_length - 1] == ' '
            || condition_start[condition_length - 1] == '\t'
            || condition_start[condition_length - 1] == '\n'
            || condition_start[condition_length - 1] == '\r'))
        condition_length -= 1;
    if (condition_length == 0 || condition_length >= sizeof(raw_condition))
        return (FT_FAILURE);
    std::memcpy(raw_condition, condition_start, condition_length);
    raw_condition[condition_length] = '\0';
    logical_condition_lowered = 0;
    if (lower_scalar_method_calls
        && (cblc_lower_short_circuit_condition(unit, function,
                condition_start, raw_condition, sizeof(raw_condition),
                &logical_condition_lowered) != FT_SUCCESS
            || (!logical_condition_lowered
                && cblc_lower_scalar_method_calls_in_if_condition(unit,
                    function, condition_start, raw_condition,
                    sizeof(raw_condition)) != FT_SUCCESS)))
        return (FT_FAILURE);
    normalized_length = 0;
    condition_index = 0;
    while (raw_condition[condition_index] != '\0')
    {
        if (raw_condition[condition_index] == '\''
            || raw_condition[condition_index] == '"')
        {
            char literal_quote;

            literal_quote = raw_condition[condition_index++];
            if (normalized_length + 1 >= condition_size)
                return (FT_FAILURE);
            condition[normalized_length++] = literal_quote;
            while (raw_condition[condition_index] != '\0')
            {
                char literal_character;

                literal_character = raw_condition[condition_index++];
                if (normalized_length + 1 >= condition_size)
                    return (FT_FAILURE);
                condition[normalized_length++] = literal_character;
                if (literal_character == literal_quote)
                    break ;
            }
            continue ;
        }
        if (cblc_is_identifier_char(raw_condition[condition_index]))
        {
            char identifier[TRANSPILE_IDENTIFIER_MAX];
            size_t identifier_length;
            t_cblc_data_item *item;
            const char *mapped_name;
            size_t mapped_length;
            char mapped_identifier[TRANSPILE_IDENTIFIER_MAX];

            identifier_length = 0;
            while (cblc_is_identifier_char(raw_condition[condition_index]))
            {
                if (identifier_length + 1 >= sizeof(identifier))
                    return (FT_FAILURE);
                identifier[identifier_length++] = raw_condition[condition_index++];
            }
            identifier[identifier_length] = '\0';
            item = cblc_find_data_item(unit, identifier);
            item = cblc_resolve_implicit_member_item(unit, identifier, item);
            mapped_name = item ? item->cobol_name : NULL;
            if (!mapped_name)
            {
                cblc_identifier_to_cobol(identifier, mapped_identifier,
                    sizeof(mapped_identifier));
                mapped_name = mapped_identifier;
            }
            mapped_length = std::strlen(mapped_name);
            if (normalized_length + mapped_length + 1 > condition_size)
                return (FT_FAILURE);
            std::memcpy(condition + normalized_length, mapped_name, mapped_length);
            normalized_length += mapped_length;
            if (item && item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                && raw_condition[condition_index] == '.'
                && std::strncmp(raw_condition + condition_index, ".len",
                    sizeof(".len") - 1) == 0)
            {
                if (normalized_length + std::strlen("-LEN") + 1 > condition_size)
                    return (FT_FAILURE);
                std::memcpy(condition + normalized_length, "-LEN", std::strlen("-LEN"));
                normalized_length += std::strlen("-LEN");
                condition_index += std::strlen(".len");
            }
            continue ;
        }
        if (normalized_length + 1 >= condition_size)
            return (FT_FAILURE);
        condition[normalized_length++] = raw_condition[condition_index++];
    }
    condition[normalized_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_parse_generic_while(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char condition[TRANSPILE_STATEMENT_TEXT_MAX];
    char active_source[TRANSPILE_IDENTIFIER_MAX];
    char active_cobol[TRANSPILE_IDENTIFIER_MAX];
    size_t original_statement_count;
    size_t original_data_count;
    size_t original_scope_count;
    size_t original_scope_id;
    size_t original_local_destructor_count;
    size_t original_exception_type_count;
    size_t original_exception_region_depth;
    size_t original_next_exception_region_id;
    size_t original_active_catch_region_id;
    size_t original_active_catch_cleanup_base;
    size_t lowered_call_count;
    t_cblc_statement active_assignment;
    t_cblc_statement while_begin;
    int original_saw_return;
    int original_may_throw;
    int original_exception_types_unknown;
    int original_exception_in_catch;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    original_statement_count = function->statement_count;
    original_data_count = unit->data_count;
    original_scope_count = unit->scope_count;
    original_scope_id = unit->active_scope_id;
    original_local_destructor_count = function->local_destructor_count;
    original_exception_type_count = function->exception_type_count;
    original_exception_region_depth = function->exception_region_depth;
    original_next_exception_region_id = function->next_exception_region_id;
    original_active_catch_region_id = function->active_catch_region_id;
    original_active_catch_cleanup_base = function->active_catch_cleanup_base;
    original_saw_return = function->saw_return;
    original_may_throw = function->may_throw;
    original_exception_types_unknown = function->exception_types_unknown;
    original_exception_in_catch = function->exception_in_catch;
    if (!cblc_match_keyword(cursor, "while"))
        return (FT_FAILURE);
    if (cblc_parse_condition(cursor, unit, function, 1, condition,
            sizeof(condition)) != FT_SUCCESS)
        goto fail;
    lowered_call_count = function->statement_count - original_statement_count;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        goto fail;
    if (lowered_call_count > 0)
    {
        char active_base[TRANSPILE_IDENTIFIER_MAX];
        char while_condition[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t while_begin_index;
        size_t condition_index;

        if (std::snprintf(active_base, sizeof(active_base),
                "CBLC_WHILE_ACTIVE_%zu", original_statement_count) < 0
            || cblc_build_unique_local_storage_names(unit, function,
                active_base, active_source, sizeof(active_source), active_cobol,
                sizeof(active_cobol)) != FT_SUCCESS
            || cblc_add_named_data_item(unit, active_source, active_cobol,
                CBLC_DATA_KIND_INT, 0, 0, NULL, function->source_name,
                1, 0, 0) != FT_SUCCESS
            || std::snprintf(while_condition, sizeof(while_condition),
                "%s == 0", active_source) < 0)
            goto fail;
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                active_cobol, "0", 1) != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_WHILE_BEGIN,
                NULL, while_condition, 0) != FT_SUCCESS)
            goto fail;
        active_assignment = function->statements[function->statement_count - 2];
        while_begin = function->statements[function->statement_count - 1];
        std::memmove(&function->statements[original_statement_count + 2],
            &function->statements[original_statement_count],
            lowered_call_count * sizeof(function->statements[0]));
        function->statements[original_statement_count] = active_assignment;
        function->statements[original_statement_count + 1] = while_begin;
        while_begin_index = original_statement_count + 1;
        cblc_exception_source_position(unit, start,
            &function->statements[while_begin_index].exception_source_line,
            &function->statements[while_begin_index].exception_source_column);
        if (cblc_append_statement(function, CBLC_STATEMENT_IF_BEGIN, NULL,
                condition, 0) != FT_SUCCESS)
            goto fail;
        condition_index = function->statement_count - 1;
        cblc_exception_source_position(unit, start,
            &function->statements[condition_index].exception_source_line,
            &function->statements[condition_index].exception_source_column);
        if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_IF_ELSE,
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                active_cobol, "1", 1) != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_IF_END,
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_append_statement(function, CBLC_STATEMENT_WHILE_END,
                NULL, NULL, 0) != FT_SUCCESS)
            goto fail;
        return (FT_SUCCESS);
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_WHILE_BEGIN,
            NULL, condition, 0) != FT_SUCCESS)
        goto fail;
    cblc_exception_source_position(unit, start,
        &function->statements[function->statement_count - 1].exception_source_line,
        &function->statements[function->statement_count - 1].exception_source_column);
    if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS
        || cblc_append_statement(function, CBLC_STATEMENT_WHILE_END, NULL, NULL, 0)
            != FT_SUCCESS)
        goto fail;
    return (FT_SUCCESS);
fail:
    function->statement_count = original_statement_count;
    function->local_destructor_count = original_local_destructor_count;
    function->exception_type_count = original_exception_type_count;
    function->exception_region_depth = original_exception_region_depth;
    function->next_exception_region_id = original_next_exception_region_id;
    function->active_catch_region_id = original_active_catch_region_id;
    function->active_catch_cleanup_base = original_active_catch_cleanup_base;
    function->saw_return = original_saw_return;
    function->may_throw = original_may_throw;
    function->exception_types_unknown = original_exception_types_unknown;
    function->exception_in_catch = original_exception_in_catch;
    unit->data_count = original_data_count;
    unit->scope_count = original_scope_count;
    unit->active_scope_id = original_scope_id;
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_parse_generic_if(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char condition[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "if")
        || cblc_parse_condition(cursor, unit, function, 1, condition,
            sizeof(condition)) != FT_SUCCESS)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{'
        || cblc_append_statement(function, CBLC_STATEMENT_IF_BEGIN, NULL, condition, 0)
            != FT_SUCCESS)
        goto fail;
    cblc_exception_source_position(unit, start,
        &function->statements[function->statement_count - 1].exception_source_line,
        &function->statements[function->statement_count - 1].exception_source_column);
    if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (cblc_match_keyword(cursor, "else"))
    {
        cblc_skip_whitespace(cursor);
        if (**cursor != '{'
            || cblc_append_statement(function, CBLC_STATEMENT_IF_ELSE, NULL, NULL, 0)
                != FT_SUCCESS
            || cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS)
            goto fail;
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_IF_END, NULL, NULL, 0)
        != FT_SUCCESS)
        goto fail;
    return (FT_SUCCESS);
fail:
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_parse_statement_block(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, int allow_return)
{
    size_t scope_destructor_base;
    size_t scope_data_base;
    size_t parent_scope_id;
    size_t scope_id;
    int saw_return_in_block;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    parent_scope_id = unit->active_scope_id;
    if (cblc_register_scope(unit, parent_scope_id, function->source_name, &scope_id)
        != FT_SUCCESS)
        return (FT_FAILURE);
    unit->active_scope_id = scope_id;
    scope_destructor_base = function->local_destructor_count;
    scope_data_base = unit->data_count;
    saw_return_in_block = 0;
    *cursor += 1;
    while (**cursor != '\0')
    {
        const char *statement_start;
        size_t first_statement;

        cblc_skip_whitespace(cursor);
        statement_start = *cursor;
        first_statement = function->statement_count;
        cblc_exception_source_position(unit, *cursor, &function->active_source_line,
            &function->active_source_column);
        if (**cursor == '}')
        {
            if (!saw_return_in_block)
            {
                while (function->local_destructor_count > scope_destructor_base)
                {
                    function->local_destructor_count -= 1;
                    if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                            function->local_destructor_targets[function->local_destructor_count],
                            NULL, 0) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else
                function->local_destructor_count = scope_destructor_base;
            while (scope_data_base < unit->data_count)
            {
                if (unit->data_items[scope_data_base].is_alias
                    || unit->data_items[scope_data_base].is_function_local)
                    unit->data_items[scope_data_base].is_active = 0;
                scope_data_base += 1;
            }
            *cursor += 1;
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            unit->active_scope_id = parent_scope_id;
            return (FT_SUCCESS);
        }
        if (**cursor == '{')
        {
            if (cblc_parse_statement_block(cursor, unit, function, allow_return) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_try_statement(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_throw_statement(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_match_keyword(cursor, "return"))
        {
            (*cursor) -= std::strlen("return");
            if (!allow_return)
                return (FT_FAILURE);
            if (cblc_parse_return(cursor, unit, function) != FT_SUCCESS)
                return (FT_FAILURE);
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            saw_return_in_block = 1;
            continue ;
        }
        if (cblc_match_keyword(cursor, "display"))
        {
            (*cursor) -= std::strlen("display");
            if (cblc_parse_display(cursor, unit, function) != FT_SUCCESS)
                return (FT_FAILURE);
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_file_read_while(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_generic_while(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_generic_if(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_file_statement(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_local_struct_instance_declaration(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_local_scalar_declaration(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_local_pointer_declaration(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_local_struct_pointer_declaration(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_std_strcpy(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_delete_statement(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_std_free(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_method_call(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_call(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (cblc_parse_assignment(cursor, unit, function) == FT_SUCCESS)
        {
            cblc_capture_statement_source_spans(unit, function, first_statement,
                statement_start, *cursor);
            continue ;
        }
        if (unit->parse_error_message[0] == '\0')
        {
            std::snprintf(unit->parse_error_message, sizeof(unit->parse_error_message),
                "unable to parse statement in '%s' near '%.32s'",
                function->source_name, *cursor);
        }
        return (FT_FAILURE);
    }
    return (FT_FAILURE);
}

static void cblc_capture_statement_source_spans(const t_cblc_translation_unit *unit,
    t_cblc_function *function, size_t first_statement, const char *start,
    const char *end)
{
    size_t source_start;
    size_t source_end;
    size_t index;

    if (!unit || !function || !start || !end || end < start)
        return ;
    source_start = cblc_source_offset(unit, start);
    source_end = cblc_source_offset(unit, end);
    if (source_start == static_cast<size_t>(-1)
        || source_end == static_cast<size_t>(-1) || source_end < source_start)
        return ;
    index = first_statement;
    while (index < function->statement_count)
    {
        t_cblc_statement *statement;

        statement = &function->statements[index];
        if (statement->source_offset == static_cast<size_t>(-1))
        {
            statement->source_offset = source_start;
            statement->source_length = source_end - source_start;
            cblc_exception_source_position(unit, start, &statement->source_line,
                &statement->source_column);
        }
        index += 1;
    }
}

static int cblc_lifecycle_method_is_nothrow(const t_cblc_struct_type *type,
    const char *method_name, size_t depth)
{
    size_t method_index;

    if (!type || !method_name || depth > CBLC_EXCEPTION_REGION_MAX)
        return (0);
    method_index = 0;
    while (method_index < type->method_count)
    {
        const t_cblc_method *method;
        size_t statement_index;

        method = &type->methods[method_index];
        if (std::strcmp(method->source_name, method_name) != 0)
        {
            method_index += 1;
            continue ;
        }
        if (method->return_kind != CBLC_FUNCTION_RETURN_VOID)
            return (0);
        statement_index = 0;
        while (statement_index < method->statement_count)
        {
            const t_cblc_statement *statement;

            statement = &method->statements[statement_index];
            if (statement->exception_may_throw
                || statement->type == CBLC_STATEMENT_THROW
                || statement->type == CBLC_STATEMENT_TRY_BEGIN
                || statement->type == CBLC_STATEMENT_CATCH_BEGIN
                || statement->type == CBLC_STATEMENT_FILE_OPEN
                || statement->type == CBLC_STATEMENT_FILE_READ
                || statement->type == CBLC_STATEMENT_FILE_WRITE
                || statement->type == CBLC_STATEMENT_FILE_CLOSE
                || statement->type == CBLC_STATEMENT_CALL
                || statement->type == CBLC_STATEMENT_CALL_ASSIGN
                || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                || statement->type == CBLC_STATEMENT_METHOD_CALL)
                return (0);
            statement_index += 1;
        }
        return (1);
    }
    return (0);
}

static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity)
{
    t_cblc_function lifecycle_function;
    size_t saved_data_count;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    std::memset(&lifecycle_function, 0, sizeof(lifecycle_function));
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
        return (FT_FAILURE);
    g_cblc_member_access_type = type;
    status = cblc_parse_statement_block(cursor, unit, &lifecycle_function, 0);
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    g_cblc_member_access_type = saved_access_type;
    if (status != FT_SUCCESS)
    {
        if (lifecycle_function.statements)
            cma_free(lifecycle_function.statements);
        return (FT_FAILURE);
    }
    {
        size_t statement_index;

        statement_index = 0;
        while (statement_index < lifecycle_function.statement_count)
        {
            t_cblc_statement_type statement_type;

            statement_type = lifecycle_function.statements[statement_index].type;
            if (statement_type == CBLC_STATEMENT_THROW
                || statement_type == CBLC_STATEMENT_TRY_BEGIN
                || statement_type == CBLC_STATEMENT_CATCH_BEGIN
                || statement_type == CBLC_STATEMENT_CATCH_END
                || statement_type == CBLC_STATEMENT_TRY_END
                || ((statement_type == CBLC_STATEMENT_CALL
                        || statement_type == CBLC_STATEMENT_CALL_ASSIGN)
                    && std::strncmp(lifecycle_function.statements[statement_index].call_identifier,
                        "std::free", sizeof(lifecycle_function.statements[statement_index].call_identifier)) != 0
                    && std::strncmp(lifecycle_function.statements[statement_index].call_identifier,
                        "CBLC-DELETE", std::strlen("CBLC-DELETE")) != 0)
                || (statement_type == CBLC_STATEMENT_METHOD_CALL
                    && !cblc_lifecycle_method_is_nothrow(type,
                        lifecycle_function.statements[statement_index].call_identifier, 0))
                || statement_type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                || statement_type == CBLC_STATEMENT_FILE_OPEN
                || statement_type == CBLC_STATEMENT_FILE_READ
                || statement_type == CBLC_STATEMENT_FILE_WRITE
                || statement_type == CBLC_STATEMENT_FILE_CLOSE)
            {
                unit->parse_error_code = TRANSPILE_ERROR_EXCEPTION_DESTRUCTOR_THROW;
                std::snprintf(unit->parse_error_message,
                    sizeof(unit->parse_error_message),
                    "destructor for '%s' cannot contain throwing operations or exception handling",
                    type->source_name);
                cma_free(lifecycle_function.statements);
                return (FT_FAILURE);
            }
            statement_index += 1;
        }
    }
    *out_statements = lifecycle_function.statements;
    *out_count = lifecycle_function.statement_count;
    *out_capacity = lifecycle_function.statement_capacity;
    return (FT_SUCCESS);
}

static int cblc_find_field_index(const t_cblc_struct_type *type, const char *identifier,
    size_t *out_index)
{
    size_t index;

    if (!type || !identifier || !out_index)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (std::strncmp(type->fields[index].source_name, identifier,
                sizeof(type->fields[index].source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_find_current_constructor_field_index(const t_cblc_data_item *item,
    size_t *out_index)
{
    const t_cblc_struct_type *type;
    size_t index;
    char expected_cobol[TRANSPILE_IDENTIFIER_MAX];
    char expected_source[TRANSPILE_IDENTIFIER_MAX];

    if (!item || !out_index || !g_cblc_constructor_parse_state.active
        || !g_cblc_constructor_parse_state.type)
        return (FT_FAILURE);
    type = g_cblc_constructor_parse_state.type;
    index = 0;
    while (index < type->field_count)
    {
        if (std::snprintf(expected_cobol, sizeof(expected_cobol), "CBLC-THIS-%s",
                type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (std::strncmp(item->cobol_name, expected_cobol, sizeof(item->cobol_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        if (std::strncmp(item->source_name, type->fields[index].source_name,
                sizeof(item->source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        if (std::snprintf(expected_source, sizeof(expected_source), "this.%s",
                type->fields[index].source_name) < 0)
            return (FT_FAILURE);
        if (std::strncmp(item->source_name, expected_source, sizeof(item->source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_append_string_initializer_literal(t_cblc_function *function,
    const t_cblc_data_item *item, const char *literal_text)
{
    char buffer_target[TRANSPILE_IDENTIFIER_MAX];
    char length_target[TRANSPILE_IDENTIFIER_MAX];
    char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char length_literal[32];
    size_t assigned_length;

    if (!function || !item || !literal_text)
        return (FT_FAILURE);
    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_target,
        sizeof(buffer_target));
    cblc_build_string_component_name(item->cobol_name, "-LEN", length_target,
        sizeof(length_target));
    assigned_length = std::strlen(literal_text);
    if (assigned_length > item->length)
        assigned_length = item->length;
    cobol_source[0] = '"';
    cobol_source[1] = '\0';
    ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
    ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
    if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
            cobol_source, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
            length_literal, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_constructor_member_initializer(const char **cursor,
    t_cblc_translation_unit *unit, const t_cblc_struct_type *type, int *seen_fields,
    t_cblc_function *field_functions)
{
    char member_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;
    size_t field_index;

    if (!cursor || !*cursor || !unit || !type || !seen_fields || !field_functions)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, member_name, sizeof(member_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_find_field_index(type, member_name, &field_index) != FT_SUCCESS)
        return (FT_FAILURE);
    if (type->has_base_type)
    {
        const t_cblc_struct_type *base_type;

        base_type = cblc_find_struct_type(unit, type->base_type_name);
        if (base_type && field_index < base_type->field_count)
            return (FT_FAILURE);
    }
    if (seen_fields[field_index])
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, member_name);
    if (!item)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
    {
        if (item->kind == CBLC_DATA_KIND_INT)
        {
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_COMPUTE,
                    item->cobol_name, "0", 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (item->kind == CBLC_DATA_KIND_CHAR_BUFFER || item->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                    item->cobol_name, NULL, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
    }
    else if (item->kind == CBLC_DATA_KIND_INT)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_COMPUTE,
                item->cobol_name, expression, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
    {
        if (**cursor == '"')
        {
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_string_initializer_literal(&field_functions[field_index], item,
                    literal_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[2];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            if (cblc_append_string_initializer_literal(&field_functions[field_index], item,
                    literal_text) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
                return (FT_FAILURE);
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else if (item->kind == CBLC_DATA_KIND_CHAR)
    {
        char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];

        if (**cursor == '"')
        {
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = '"';
            literal_text[1] = literal_value;
            literal_text[2] = '"';
            literal_text[3] = '\0';
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, literal_text, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    seen_fields[field_index] = 1;
    return (FT_SUCCESS);
}

static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *constructor_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity)
{
    t_cblc_function *field_functions;
    int *seen_fields;
    size_t saved_data_count;
    size_t index;
    size_t base_field_count;
    const t_cblc_struct_type *base_type;
    const t_cblc_constructor *base_constructor;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !constructor_function
        || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    field_functions = static_cast<t_cblc_function *>(cma_calloc(type->field_count == 0 ? 1 : type->field_count,
            sizeof(*field_functions)));
    seen_fields = static_cast<int *>(cma_calloc(type->field_count == 0 ? 1 : type->field_count,
            sizeof(*seen_fields)));
    if (!field_functions || !seen_fields)
    {
        if (field_functions)
            cma_free(field_functions);
        if (seen_fields)
            cma_free(seen_fields);
        return (FT_FAILURE);
    }
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
    {
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    if (cblc_add_parameter_aliases(unit, constructor_function->parameters,
            constructor_function->parameter_count, type) != FT_SUCCESS)
    {
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    if (cblc_register_parameter_destructors(unit, constructor_function)
        != FT_SUCCESS)
    {
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    g_cblc_constructor_parse_state.type = type;
    g_cblc_constructor_parse_state.initialized_fields = seen_fields;
    g_cblc_constructor_parse_state.active = 1;
    g_cblc_member_access_type = type;
    base_type = NULL;
    base_constructor = NULL;
    base_field_count = 0;
    if (type->has_base_type)
    {
        base_type = cblc_find_struct_type(unit, type->base_type_name);
        if (!base_type)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        base_field_count = base_type->field_count;
        base_constructor = cblc_find_constructor_for_arguments(unit, base_type,
            NULL, 0);
        if (base_type->constructor_count > 0
            && (!base_constructor || !base_constructor->has_definition))
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        if (base_constructor)
        {
            size_t base_statement_index;

            base_statement_index = 0;
            while (base_statement_index < base_constructor->statement_count)
            {
                if (cblc_append_existing_statement(constructor_function,
                        &base_constructor->statements[base_statement_index]) != FT_SUCCESS)
                {
                    status = FT_FAILURE;
                    goto cleanup;
                }
                base_statement_index += 1;
            }
            index = 0;
            while (index < base_field_count)
            {
                const t_cblc_data_item *base_field_item;

                base_field_item = cblc_find_data_item(unit,
                    base_type->fields[index].source_name);
                if (!base_field_item)
                {
                    status = FT_FAILURE;
                    goto cleanup;
                }
                if (base_field_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                    || base_field_item->kind == CBLC_DATA_KIND_STRUCT)
                {
                    if (constructor_function->local_destructor_count
                        >= constructor_function->local_destructor_capacity
                        && cblc_function_ensure_local_destructor_capacity(
                            constructor_function,
                            constructor_function->local_destructor_capacity == 0 ? 4
                            : constructor_function->local_destructor_capacity * 2)
                            != FT_SUCCESS)
                    {
                        status = FT_FAILURE;
                        goto cleanup;
                    }
                    ft_strlcpy(constructor_function->local_destructor_targets[
                            constructor_function->local_destructor_count],
                        base_field_item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
                    constructor_function->local_destructor_count += 1;
                }
                index += 1;
            }
        }
        else
        {
            index = 0;
            while (index < base_field_count)
            {
                const t_cblc_data_item *base_field_item;

                base_field_item = cblc_find_data_item(unit,
                    base_type->fields[index].source_name);
                if (!base_field_item)
                {
                    status = FT_FAILURE;
                    goto cleanup;
                }
                if (base_field_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                    || base_field_item->kind == CBLC_DATA_KIND_STRUCT)
                {
                    if (cblc_append_statement(constructor_function,
                            CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                            base_field_item->cobol_name, NULL, 0) != FT_SUCCESS)
                    {
                        status = FT_FAILURE;
                        goto cleanup;
                    }
                    if (constructor_function->local_destructor_count
                        >= constructor_function->local_destructor_capacity
                        && cblc_function_ensure_local_destructor_capacity(
                            constructor_function,
                            constructor_function->local_destructor_capacity == 0 ? 4
                            : constructor_function->local_destructor_capacity * 2)
                            != FT_SUCCESS)
                    {
                        status = FT_FAILURE;
                        goto cleanup;
                    }
                    ft_strlcpy(constructor_function->local_destructor_targets[
                            constructor_function->local_destructor_count],
                        base_field_item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
                    constructor_function->local_destructor_count += 1;
                }
                index += 1;
            }
        }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == ':')
    {
        *cursor += 1;
        while (**cursor != '\0')
        {
            cblc_skip_whitespace(cursor);
            if (cblc_parse_constructor_member_initializer(cursor, unit, type, seen_fields,
                    field_functions) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            cblc_skip_whitespace(cursor);
            if (**cursor == ',')
            {
                *cursor += 1;
                continue ;
            }
            break ;
        }
    }
    index = base_field_count;
    while (index < type->field_count)
    {
        t_cblc_data_item *field_item;
        size_t statement_index;

        field_item = cblc_find_data_item(unit, type->fields[index].source_name);
        if (!field_item)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        if (!seen_fields[index] && !type->fields[index].is_template_parameter)
        {
            if (field_item->kind == CBLC_DATA_KIND_CHAR_BUFFER || field_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_append_statement(constructor_function, CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                        field_item->cobol_name, NULL, 0) != FT_SUCCESS)
                {
                    status = FT_FAILURE;
                    goto cleanup;
                }
            }
        }
        statement_index = 0;
        while (statement_index < field_functions[index].statement_count)
        {
            size_t appended_statement_index;

            if (cblc_append_existing_statement(constructor_function,
                    &field_functions[index].statements[statement_index]) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            appended_statement_index = constructor_function->statement_count - 1;
            constructor_function->statements[appended_statement_index]
                .exception_cleanup_count = 0;
            if (constructor_function->local_destructor_count
                < CBLC_EXCEPTION_CLEANUP_MAX)
            {
                size_t cleanup_index;

                cleanup_index = constructor_function->local_destructor_count;
                while (cleanup_index > 0
                    && constructor_function->statements[appended_statement_index]
                        .exception_cleanup_count < CBLC_EXCEPTION_CLEANUP_MAX)
                {
                    cleanup_index -= 1;
                    ft_strlcpy(constructor_function->statements[appended_statement_index]
                            .exception_cleanup_targets[constructor_function->statements[
                                appended_statement_index].exception_cleanup_count],
                        constructor_function->local_destructor_targets[cleanup_index],
                        sizeof(constructor_function->statements[appended_statement_index]
                            .exception_cleanup_targets[0]));
                    constructor_function->statements[appended_statement_index]
                        .exception_cleanup_count += 1;
                }
            }
            statement_index += 1;
        }
        if (!type->fields[index].is_template_parameter
            && (field_item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                || field_item->kind == CBLC_DATA_KIND_STRUCT)
            && constructor_function->local_destructor_count
                < CBLC_EXCEPTION_CLEANUP_MAX)
        {
            if (constructor_function->local_destructor_count
                >= constructor_function->local_destructor_capacity
                && cblc_function_ensure_local_destructor_capacity(constructor_function,
                    constructor_function->local_destructor_capacity == 0 ? 4
                    : constructor_function->local_destructor_capacity * 2) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            ft_strlcpy(constructor_function->local_destructor_targets[
                    constructor_function->local_destructor_count],
                field_item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            constructor_function->local_destructor_count += 1;
        }
        index += 1;
    }
    status = cblc_parse_statement_block(cursor, unit, constructor_function, 0);
    if (status != FT_SUCCESS)
        goto cleanup;
    if (cblc_append_parameter_destructor_statements(unit,
            constructor_function) != FT_SUCCESS)
    {
        status = FT_FAILURE;
        goto cleanup;
    }
    index = base_field_count;
    while (index < type->field_count)
    {
        if (type->fields[index].is_const && !seen_fields[index])
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    *out_statements = constructor_function->statements;
    *out_count = constructor_function->statement_count;
    *out_capacity = constructor_function->statement_capacity;
    constructor_function->statements = NULL;
    constructor_function->statement_count = 0;
    constructor_function->statement_capacity = 0;
    status = FT_SUCCESS;
cleanup:
    g_cblc_constructor_parse_state.type = NULL;
    g_cblc_constructor_parse_state.initialized_fields = NULL;
    g_cblc_constructor_parse_state.active = 0;
    g_cblc_member_access_type = saved_access_type;
    if (status == FT_SUCCESS)
        cblc_unbind_method_scope_preserving_locals(unit, saved_data_count,
            constructor_function->source_name);
    else
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
    index = 0;
    while (index < type->field_count)
    {
        if (field_functions[index].statements)
            cma_free(field_functions[index].statements);
        index += 1;
    }
    cma_free(field_functions);
    cma_free(seen_fields);
    return (status);
}

static int cblc_inject_entry_lifecycle(t_cblc_translation_unit *unit)
{
    t_cblc_function *function;
    t_cblc_statement *new_statements;
    char (*construct_targets)[TRANSPILE_IDENTIFIER_MAX];
    char (*construct_arguments)[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t *construct_argument_counts;
    char (*destruct_targets)[TRANSPILE_IDENTIFIER_MAX];
    char (*global_flags)[TRANSPILE_IDENTIFIER_MAX];
    char (*construct_flags)[TRANSPILE_IDENTIFIER_MAX];
    char (*destruct_flags)[TRANSPILE_IDENTIFIER_MAX];
    char (*destruct_cleanup_paragraphs)[TRANSPILE_IDENTIFIER_MAX];
    char (*global_flag_states)[2];
    size_t construct_count;
    size_t destruct_count;
    size_t initial_data_count;
    size_t global_flag_count;
    size_t return_count;
    size_t new_count;
    size_t index;
    size_t write_index;

    if (!unit || unit->function_count == 0)
        return (FT_SUCCESS);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (FT_SUCCESS);
    function = &unit->functions[unit->entry_function_index];
    initial_data_count = unit->data_count;
    index = initial_data_count;
    if (index == 0)
        index = 1;
    construct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*construct_targets)));
    construct_arguments = static_cast<char (*)[TRANSPILE_STATEMENT_TEXT_MAX]>(cma_calloc(index,
            sizeof(*construct_arguments)));
    construct_argument_counts = static_cast<size_t *>(cma_calloc(index,
            sizeof(*construct_argument_counts)));
    destruct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*destruct_targets)));
    global_flags = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*global_flags)));
    construct_flags = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*construct_flags)));
    destruct_flags = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*destruct_flags)));
    destruct_cleanup_paragraphs = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*destruct_cleanup_paragraphs)));
    global_flag_states = static_cast<char (*)[2]>(cma_calloc(index,
            sizeof(*global_flag_states)));
    if (!construct_targets || !construct_arguments || !construct_argument_counts
        || !destruct_targets || !global_flags || !construct_flags
        || !destruct_flags || !destruct_cleanup_paragraphs
        || !global_flag_states)
    {
        if (construct_targets)
            cma_free(construct_targets);
        if (construct_arguments)
            cma_free(construct_arguments);
        if (construct_argument_counts)
            cma_free(construct_argument_counts);
        if (destruct_targets)
            cma_free(destruct_targets);
        if (global_flags)
            cma_free(global_flags);
        if (construct_flags)
            cma_free(construct_flags);
        if (destruct_flags)
            cma_free(destruct_flags);
        if (destruct_cleanup_paragraphs)
            cma_free(destruct_cleanup_paragraphs);
        if (global_flag_states)
            cma_free(global_flag_states);
        return (FT_FAILURE);
    }
    construct_count = 0;
    destruct_count = 0;
    global_flag_count = 0;
    index = 0;
    while (index < initial_data_count)
    {
        const t_cblc_data_item *item;
        const t_cblc_struct_type *type;
        char flag_name[TRANSPILE_IDENTIFIER_MAX];
        int needs_construct;
        int needs_destruct;
        size_t parameter_index;
        int is_entry_parameter;

        item = &unit->data_items[index];
        is_entry_parameter = 0;
        parameter_index = 0;
        while (parameter_index < function->parameter_count)
        {
            if (std::strncmp(item->cobol_name,
                    function->parameters[parameter_index].cobol_name,
                    sizeof(item->cobol_name)) == 0)
            {
                is_entry_parameter = 1;
                break ;
            }
            parameter_index += 1;
        }
        if (item->is_template_item || item->is_alias || item->is_function_local
            || item->owner_function_name[0] != '\0'
            || std::strchr(item->source_name, '.') || is_entry_parameter)
        {
            index += 1;
            continue ;
        }
        type = cblc_find_declared_type_for_item(unit, item);
        if (item->kind == CBLC_DATA_KIND_STRUCT && !type)
            type = cblc_find_struct_type(unit, item->struct_type_name);
        if ((item->kind == CBLC_DATA_KIND_CHAR_BUFFER || item->kind == CBLC_DATA_KIND_STRUCT) && !type)
        {
            cma_free(construct_targets);
            cma_free(construct_arguments);
            cma_free(construct_argument_counts);
            cma_free(destruct_targets);
            cma_free(global_flags);
            cma_free(construct_flags);
            cma_free(destruct_flags);
            cma_free(destruct_cleanup_paragraphs);
            cma_free(global_flag_states);
            return (FT_FAILURE);
        }
        needs_construct = cblc_item_requires_default_construct(unit, item);
        needs_destruct = cblc_item_requires_destruct(unit, item);
        flag_name[0] = '\0';
        if (needs_construct || needs_destruct)
        {
            if (std::snprintf(flag_name, sizeof(flag_name),
                    "CBLC-GLOBAL-LIVE-%zu", index) < 0
                || std::strlen(flag_name) >= sizeof(flag_name))
            {
                cma_free(construct_targets);
                cma_free(construct_arguments);
                cma_free(construct_argument_counts);
                cma_free(destruct_targets);
                cma_free(global_flags);
                cma_free(construct_flags);
                cma_free(destruct_flags);
                cma_free(destruct_cleanup_paragraphs);
                cma_free(global_flag_states);
                return (FT_FAILURE);
            }
            ft_strlcpy(global_flags[index], flag_name,
                TRANSPILE_IDENTIFIER_MAX);
            global_flag_states[index][0] = needs_construct ? 'N' : 'Y';
            global_flag_count += 1;
        }
        if (needs_construct)
        {
            ft_strlcpy(construct_targets[construct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            ft_strlcpy(construct_arguments[construct_count], item->constructor_arguments,
                TRANSPILE_STATEMENT_TEXT_MAX);
            construct_argument_counts[construct_count] = item->constructor_argument_count;
            ft_strlcpy(construct_flags[construct_count], flag_name,
                TRANSPILE_IDENTIFIER_MAX);
            construct_count += 1;
        }
        if (needs_destruct)
        {
            ft_strlcpy(destruct_targets[destruct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            ft_strlcpy(destruct_flags[destruct_count], flag_name,
                TRANSPILE_IDENTIFIER_MAX);
            if (std::snprintf(destruct_cleanup_paragraphs[destruct_count],
                    TRANSPILE_IDENTIFIER_MAX, "CBLC-GLOBAL-DROP-%zu", index) < 0)
            {
                cma_free(construct_targets);
                cma_free(construct_arguments);
                cma_free(construct_argument_counts);
                cma_free(destruct_targets);
                cma_free(global_flags);
                cma_free(construct_flags);
                cma_free(destruct_flags);
                cma_free(destruct_cleanup_paragraphs);
                cma_free(global_flag_states);
                return (FT_FAILURE);
            }
            destruct_count += 1;
        }
        if ((needs_construct || needs_destruct)
            && cblc_add_named_data_item(unit, flag_name, flag_name,
                CBLC_DATA_KIND_CHAR, 1, 0, NULL, NULL, 0, 0, 0) != FT_SUCCESS)
        {
            cma_free(construct_targets);
            cma_free(construct_arguments);
            cma_free(construct_argument_counts);
            cma_free(destruct_targets);
            cma_free(global_flags);
            cma_free(construct_flags);
            cma_free(destruct_flags);
            cma_free(destruct_cleanup_paragraphs);
            cma_free(global_flag_states);
            return (FT_FAILURE);
        }
        index += 1;
    }
    return_count = 0;
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
            return_count += 1;
        index += 1;
    }
    new_count = function->statement_count + construct_count + global_flag_count
        + (destruct_count * return_count);
    if (return_count == 0)
        new_count += destruct_count;
    if (new_count == 0)
    {
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        cma_free(global_flags);
        cma_free(construct_flags);
        cma_free(destruct_flags);
        cma_free(destruct_cleanup_paragraphs);
        cma_free(global_flag_states);
        return (FT_SUCCESS);
    }
    new_statements = static_cast<t_cblc_statement *>(cma_calloc(new_count, sizeof(*new_statements)));
    if (!new_statements)
    {
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        cma_free(global_flags);
        cma_free(construct_flags);
        cma_free(destruct_flags);
        cma_free(destruct_cleanup_paragraphs);
        cma_free(global_flag_states);
        return (FT_FAILURE);
    }
    write_index = 0;
    index = 0;
    while (index < initial_data_count)
    {
        if (global_flags[index][0] != '\0')
        {
            new_statements[write_index].type = CBLC_STATEMENT_ASSIGNMENT;
            ft_strlcpy(new_statements[write_index].target,
                global_flags[index],
                sizeof(new_statements[write_index].target));
            std::snprintf(new_statements[write_index].source,
                sizeof(new_statements[write_index].source), "'%c'",
                global_flag_states[index][0]);
            new_statements[write_index].is_literal = 1;
            write_index += 1;
        }
        index += 1;
    }
    index = 0;
    while (index < construct_count)
    {
        new_statements[write_index].type = CBLC_STATEMENT_DEFAULT_CONSTRUCT;
        ft_strlcpy(new_statements[write_index].target, construct_targets[index],
            sizeof(new_statements[write_index].target));
        ft_strlcpy(new_statements[write_index].call_arguments, construct_arguments[index],
            sizeof(new_statements[write_index].call_arguments));
        new_statements[write_index].call_argument_count = construct_argument_counts[index];
        ft_strlcpy(new_statements[write_index].lifecycle_completion_flag,
            construct_flags[index],
            sizeof(new_statements[write_index].lifecycle_completion_flag));
        write_index += 1;
        index += 1;
    }
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
        {
            size_t destruct_index;

            destruct_index = destruct_count;
            while (destruct_index > 0)
            {
                destruct_index -= 1;
                new_statements[write_index].type = CBLC_STATEMENT_DESTRUCT;
                ft_strlcpy(new_statements[write_index].target, destruct_targets[destruct_index],
                    sizeof(new_statements[write_index].target));
                ft_strlcpy(new_statements[write_index].lifecycle_completion_flag,
                    destruct_flags[destruct_index],
                    sizeof(new_statements[write_index].lifecycle_completion_flag));
                ft_strlcpy(new_statements[write_index].lifecycle_cleanup_paragraph,
                    destruct_cleanup_paragraphs[destruct_index],
                    sizeof(new_statements[write_index].lifecycle_cleanup_paragraph));
                write_index += 1;
            }
        }
        new_statements[write_index] = function->statements[index];
        write_index += 1;
        index += 1;
    }
    if (return_count == 0)
    {
        index = destruct_count;
        while (index > 0)
        {
            index -= 1;
            new_statements[write_index].type = CBLC_STATEMENT_DESTRUCT;
            ft_strlcpy(new_statements[write_index].target, destruct_targets[index],
                sizeof(new_statements[write_index].target));
            ft_strlcpy(new_statements[write_index].lifecycle_completion_flag,
                destruct_flags[index],
                sizeof(new_statements[write_index].lifecycle_completion_flag));
            ft_strlcpy(new_statements[write_index].lifecycle_cleanup_paragraph,
                destruct_cleanup_paragraphs[index],
                sizeof(new_statements[write_index].lifecycle_cleanup_paragraph));
            write_index += 1;
        }
    }
    if (write_index != new_count)
    {
        cma_free(new_statements);
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        cma_free(global_flags);
        cma_free(construct_flags);
        cma_free(destruct_flags);
        cma_free(destruct_cleanup_paragraphs);
        cma_free(global_flag_states);
        return (FT_FAILURE);
    }
    if (function->statements)
        cma_free(function->statements);
    function->statements = new_statements;
    function->statement_count = new_count;
    function->statement_capacity = new_count;
    cma_free(construct_targets);
    cma_free(construct_arguments);
    cma_free(construct_argument_counts);
    cma_free(destruct_targets);
    cma_free(global_flags);
    cma_free(construct_flags);
    cma_free(destruct_flags);
    cma_free(destruct_cleanup_paragraphs);
    cma_free(global_flag_states);
    return (FT_SUCCESS);
}

static int cblc_append_local_destructor_statements(t_cblc_function *function)
{
    size_t index;

    if (!function)
        return (FT_FAILURE);
    index = function->local_destructor_count;
    while (index > 0)
    {
        index -= 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                function->local_destructor_targets[index], NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_register_parameter_destructors(
    const t_cblc_translation_unit *unit, t_cblc_function *function)
{
    size_t index;

    if (!unit || !function)
        return (FT_FAILURE);
    for (index = 0; index < function->parameter_count; ++index)
    {
        const t_cblc_parameter *parameter = &function->parameters[index];
        const t_cblc_struct_type *type;

        if (parameter->kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            continue;
        type = cblc_find_struct_type(unit, parameter->type_name);
        if (!type || !cblc_type_requires_destruct(unit, type))
            continue;
        if (function->local_destructor_count
            >= function->local_destructor_capacity
            && cblc_function_ensure_local_destructor_capacity(function,
                function->local_destructor_capacity == 0 ? 4
                    : function->local_destructor_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_strlcpy(function->local_destructor_targets[
                function->local_destructor_count],
            parameter->cobol_name, TRANSPILE_IDENTIFIER_MAX);
        function->local_destructor_count += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_append_parameter_destructor_statements(
    const t_cblc_translation_unit *unit, t_cblc_function *function)
{
    size_t index;

    if (!unit || !function)
        return (FT_FAILURE);
    index = function->parameter_count;
    while (index > 0)
    {
        const t_cblc_parameter *parameter = &function->parameters[--index];
        const t_cblc_struct_type *type;

        if (parameter->kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            continue;
        type = cblc_find_struct_type(unit, parameter->type_name);
        if (type && cblc_type_requires_destruct(unit, type)
            && cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                parameter->cobol_name, NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_add_local_struct_alias_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *alias_source_name, const char *actual_cobol_name)
{
    const t_cblc_data_item *existing_item;
    size_t index;

    if (!unit || !type || !alias_source_name || !actual_cobol_name)
        return (FT_FAILURE);
    existing_item = cblc_find_data_item(unit, alias_source_name);
    if (!existing_item)
    {
        if (cblc_add_named_data_item(unit, alias_source_name, actual_cobol_name,
                CBLC_DATA_KIND_STRUCT, 0, 0, type->source_name, NULL, 0, 1, 0)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (existing_item->kind != CBLC_DATA_KIND_STRUCT
        || std::strncmp(existing_item->struct_type_name, type->source_name,
            sizeof(existing_item->struct_type_name)) != 0
        || std::strncmp(existing_item->cobol_name, actual_cobol_name,
            sizeof(existing_item->cobol_name)) != 0)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char alias_field_source[TRANSPILE_IDENTIFIER_MAX];
        char actual_field_cobol[TRANSPILE_IDENTIFIER_MAX];

        cblc_build_field_source_name(alias_source_name, type->fields[index].source_name,
            alias_field_source, sizeof(alias_field_source));
        if (std::snprintf(actual_field_cobol, sizeof(actual_field_cobol), "%s-%s",
                actual_cobol_name, type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_add_local_struct_alias_items(unit, field_type, alias_field_source,
                    actual_field_cobol) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (cblc_add_named_data_item(unit, alias_field_source, actual_field_cobol,
                type->fields[index].kind, type->fields[index].length,
                type->fields[index].array_count,
                type->fields[index].struct_type_name[0] != '\0'
                    ? type->fields[index].struct_type_name : NULL,
                NULL, 0, 1, type->fields[index].is_const) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_local_storage_name_exists(const t_cblc_translation_unit *unit,
    const char *source_name, const char *cobol_name)
{
    size_t index;

    if (!unit || !source_name || !cobol_name)
        return (0);
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].source_name, source_name,
                sizeof(unit->data_items[index].source_name)) == 0
            || std::strncmp(unit->data_items[index].cobol_name, cobol_name,
                sizeof(unit->data_items[index].cobol_name)) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static unsigned long long cblc_local_storage_name_hash(const char *function_name,
    const char *local_name, size_t suffix)
{
    unsigned long long hash;
    const char *cursor;

    hash = 1469598103934665603ULL;
    cursor = function_name;
    while (cursor && *cursor)
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    hash ^= static_cast<unsigned char>('_');
    hash *= 1099511628211ULL;
    cursor = local_name;
    while (cursor && *cursor)
    {
        hash ^= static_cast<unsigned char>(*cursor++);
        hash *= 1099511628211ULL;
    }
    while (suffix > 0)
    {
        hash ^= static_cast<unsigned char>(suffix & 0xffU);
        hash *= 1099511628211ULL;
        suffix >>= 8;
    }
    return (hash);
}

static int cblc_build_unique_local_storage_names(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const char *local_identifier,
    char *actual_source_name, size_t actual_source_size,
    char *actual_cobol_name, size_t actual_cobol_size)
{
    char cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char candidate_source[TRANSPILE_IDENTIFIER_MAX];
    char candidate_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char candidate_cobol[TRANSPILE_IDENTIFIER_MAX];
    char full_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char full_cobol[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t suffix;
    int source_length;
    int cobol_length;

    if (!unit || !function || !local_identifier || !actual_source_name
        || actual_source_size == 0 || !actual_cobol_name || actual_cobol_size == 0)
        return (FT_FAILURE);
    source_length = std::snprintf(full_source, sizeof(full_source), "%s__%s",
        function->source_name, local_identifier);
    cobol_length = std::snprintf(full_cobol, sizeof(full_cobol), "%s-%s",
        function->cobol_name, local_identifier);
    if (source_length < 0 || static_cast<size_t>(source_length) >= sizeof(full_source)
        || cobol_length < 0 || static_cast<size_t>(cobol_length) >= sizeof(full_cobol))
        return (FT_FAILURE);
    if (static_cast<size_t>(source_length) < sizeof(candidate_source)
        && static_cast<size_t>(cobol_length) < sizeof(cobol_source))
    {
        ft_strlcpy(candidate_source, full_source, sizeof(candidate_source));
        ft_strlcpy(cobol_source, full_cobol, sizeof(cobol_source));
    }
    else
    {
        unsigned long long hash;
        const size_t source_suffix = sizeof("__H0000000000000000") - 1;
        const size_t cobol_suffix = sizeof("-H0000000000000000") - 1;

        hash = cblc_local_storage_name_hash(function->source_name,
            local_identifier, 0);
        if (sizeof(candidate_source) <= source_suffix + 1
            || sizeof(cobol_source) <= cobol_suffix + 1
            || std::snprintf(candidate_source, sizeof(candidate_source),
                "%.*s__H%016llX",
                static_cast<int>(sizeof(candidate_source) - source_suffix - 1),
                full_source, hash) < 0
            || std::snprintf(cobol_source, sizeof(cobol_source),
                "%.*s-H%016llX",
                static_cast<int>(sizeof(cobol_source) - cobol_suffix - 1),
                full_cobol, hash) < 0)
            return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(cobol_source, candidate_cobol, sizeof(candidate_cobol));
    if (!cblc_local_storage_name_exists(unit, candidate_source, candidate_cobol))
    {
        ft_strlcpy(actual_source_name, candidate_source, actual_source_size);
        ft_strlcpy(actual_cobol_name, candidate_cobol, actual_cobol_size);
        return (FT_SUCCESS);
    }
    suffix = 1;
    while (suffix < 100000)
    {
        if (std::snprintf(full_source, sizeof(full_source), "%s__%s__%zu",
                function->source_name, local_identifier, suffix) < 0
            || std::snprintf(full_cobol, sizeof(full_cobol), "%s-%s-%zu",
                function->cobol_name, local_identifier, suffix) < 0)
            return (FT_FAILURE);
        if (std::strlen(full_source) < sizeof(candidate_source)
            && std::strlen(full_cobol) < sizeof(candidate_cobol_source))
        {
            ft_strlcpy(candidate_source, full_source, sizeof(candidate_source));
            ft_strlcpy(candidate_cobol_source, full_cobol,
                sizeof(candidate_cobol_source));
        }
        else
        {
            unsigned long long hash;
            const size_t source_suffix = sizeof("__H0000000000000000") - 1;
            const size_t cobol_suffix = sizeof("-H0000000000000000") - 1;

            hash = cblc_local_storage_name_hash(function->source_name,
                local_identifier, suffix);
            if (std::snprintf(candidate_source, sizeof(candidate_source),
                    "%.*s__H%016llX",
                    static_cast<int>(sizeof(candidate_source) - source_suffix - 1),
                    full_source, hash) < 0
                || std::snprintf(candidate_cobol_source,
                    sizeof(candidate_cobol_source), "%.*s-H%016llX",
                    static_cast<int>(sizeof(candidate_cobol_source) - cobol_suffix - 1),
                    full_cobol, hash) < 0)
                return (FT_FAILURE);
        }
        cblc_identifier_to_cobol(candidate_cobol_source, candidate_cobol,
            sizeof(candidate_cobol));
        if (!cblc_local_storage_name_exists(unit, candidate_source, candidate_cobol))
        {
            ft_strlcpy(actual_source_name, candidate_source, actual_source_size);
            ft_strlcpy(actual_cobol_name, candidate_cobol, actual_cobol_size);
            return (FT_SUCCESS);
        }
        suffix += 1;
    }
    return (FT_FAILURE);
}

static int cblc_parse_local_scalar_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_kind kind;
    size_t length;
    size_t array_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    length = 0;
    array_count = 0;
    if (cblc_match_keyword(cursor, "int"))
        kind = CBLC_DATA_KIND_INT;
    else if (cblc_match_keyword(cursor, "double") || cblc_match_keyword(cursor, "float"))
        kind = CBLC_DATA_KIND_DOUBLE;
    else if (cblc_match_keyword(cursor, "char"))
    {
        kind = CBLC_DATA_KIND_CHAR;
        length = 1;
    }
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '[')
    {
        if (kind == CBLC_DATA_KIND_INT)
        {
            if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
        else
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            if (cblc_parse_numeric_literal(cursor, &length) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            cblc_skip_whitespace(cursor);
            if (**cursor != ']')
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            *cursor += 1;
            cblc_skip_whitespace(cursor);
        }
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_build_unique_local_storage_names(unit, function, local_identifier,
            actual_source_name, sizeof(actual_source_name), actual_cobol_name,
            sizeof(actual_cobol_name)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind,
            length, array_count, NULL, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name, kind,
            length, array_count, NULL, NULL, 0, 1, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(unit->data_items[unit->data_count - 2].local_source_name,
        local_identifier,
        sizeof(unit->data_items[unit->data_count - 2].local_source_name));
    return (FT_SUCCESS);
}

static int cblc_parse_local_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    t_cblc_data_kind kind;
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function)
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
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
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
    if (cblc_build_unique_local_storage_names(unit, function, local_identifier,
            actual_source_name, sizeof(actual_source_name), actual_cobol_name,
            sizeof(actual_cobol_name)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
            NULL, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name, kind, 0, 0,
            NULL, NULL, 0, 1, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(unit->data_items[unit->data_count - 2].local_source_name,
        local_identifier,
        sizeof(unit->data_items[unit->data_count - 2].local_source_name));
    return (FT_SUCCESS);
}

static int cblc_parse_local_struct_pointer_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    const t_cblc_struct_type *type;
    char template_arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    size_t template_argument_count;
    size_t template_parameter_index;
    int is_template_parameter;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
    cblc_skip_whitespace(cursor);
    if (**cursor == '<')
    {
        *cursor += 1;
        if (cblc_parse_template_argument_list(cursor, unit, template_arguments,
                &template_argument_count, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != '>')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        type = cblc_instantiate_template_struct_arguments(unit, type_identifier,
            template_arguments, template_argument_count);
        if (!type)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        {
            size_t argument_ref_id;

            if (cblc_template_argument_ref(unit, type->source_name,
                    &argument_ref_id) == FT_SUCCESS)
                cblc_record_struct_template_use(unit, type_identifier,
                    argument_ref_id, cblc_source_offset(unit, start));
        }
        ft_strlcpy(type_identifier, type->source_name, sizeof(type_identifier));
    }
    is_template_parameter = cblc_active_template_parameter_index(type_identifier,
        &template_parameter_index) == FT_SUCCESS;
    if ((!type || type->is_builtin) && !is_template_parameter)
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
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
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
    if (cblc_build_unique_local_storage_names(unit, function, local_identifier,
            actual_source_name, sizeof(actual_source_name), actual_cobol_name,
            sizeof(actual_cobol_name)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name,
            CBLC_DATA_KIND_STRUCT_POINTER, 0, 0,
            type ? type->source_name : type_identifier,
            function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name,
            CBLC_DATA_KIND_STRUCT_POINTER, 0, 0,
            type ? type->source_name : type_identifier, NULL, 0, 1, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(unit->data_items[unit->data_count - 2].local_source_name,
        local_identifier,
        sizeof(unit->data_items[unit->data_count - 2].local_source_name));
    return (FT_SUCCESS);
}

static int cblc_parse_local_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    const t_cblc_struct_type *type;
    char template_arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    size_t template_argument_count;
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t constructor_argument_count;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    size_t array_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    array_count = 0;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
    cblc_skip_whitespace(cursor);
    if (**cursor == '<')
    {
        *cursor += 1;
        if (cblc_parse_template_argument_list(cursor, unit, template_arguments,
                &template_argument_count, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != '>')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        type = cblc_instantiate_template_struct_arguments(unit, type_identifier,
            template_arguments, template_argument_count);
        if (type)
        {
            size_t argument_ref_id;

            if (cblc_template_argument_ref(unit, type->source_name, &argument_ref_id)
                == FT_SUCCESS)
                cblc_record_struct_template_use(unit, type_identifier, argument_ref_id,
                    cblc_source_offset(unit, start));
        }
    }
    if (!type || type->is_builtin)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '[')
    {
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
    }
    if (**cursor == '(')
    {
        *cursor += 1;
        if (cblc_parse_call_argument_list(cursor, unit, constructor_arguments,
                sizeof(constructor_arguments), &constructor_argument_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count > 0
        && !cblc_find_constructor_for_arguments(unit, type, constructor_arguments,
            constructor_argument_count))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count == 0
        && type->constructor_count > 0
        && !type->has_default_constructor)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_build_unique_local_storage_names(unit, function, local_identifier,
            actual_source_name, sizeof(actual_source_name), actual_cobol_name,
            sizeof(actual_cobol_name)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name,
            CBLC_DATA_KIND_STRUCT, 0, array_count,
            type->source_name, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(unit->data_items[unit->data_count - 1].local_source_name,
        local_identifier,
        sizeof(unit->data_items[unit->data_count - 1].local_source_name));
    if (cblc_add_struct_instance_field_items(unit, type, actual_source_name, actual_cobol_name,
            function->source_name, 1, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_local_struct_alias_items(unit, type, local_identifier, actual_cobol_name)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_type_requires_default_construct(unit, type) || constructor_argument_count > 0)
    {
        if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
                NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_strlcpy(function->statements[function->statement_count - 1].call_arguments,
            constructor_arguments, sizeof(function->statements[function->statement_count - 1].call_arguments));
        function->statements[function->statement_count - 1].call_argument_count = constructor_argument_count;
    }
    if (cblc_type_requires_destruct(unit, type)
        || cblc_type_has_throwing_constructor(type))
    {
        if (function->local_destructor_count >= function->local_destructor_capacity)
        {
            if (cblc_function_ensure_local_destructor_capacity(function,
                    function->local_destructor_capacity == 0 ? 4
                    : function->local_destructor_capacity * 2) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        ft_strlcpy(function->local_destructor_targets[function->local_destructor_count],
            actual_cobol_name, TRANSPILE_IDENTIFIER_MAX);
        function->local_destructor_count += 1;
    }
    return (FT_SUCCESS);
}
