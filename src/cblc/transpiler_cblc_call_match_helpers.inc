static int cblc_argument_is_numeric_literal(const char *argument)
{
    size_t index;
    int digits;
    int decimal_point;

    if (!argument || argument[0] == '\0')
        return (0);
    index = 0;
    if (argument[index] == '+' || argument[index] == '-')
        index += 1;
    digits = 0;
    decimal_point = 0;
    while (argument[index] != '\0')
    {
        if (argument[index] >= '0' && argument[index] <= '9')
            digits = 1;
        else if (argument[index] == '.' && !decimal_point)
            decimal_point = 1;
        else
            return (0);
        index += 1;
    }
    return (digits);
}

static int cblc_argument_is_numeric_expression(
    const t_cblc_translation_unit *unit, const char *argument)
{
    size_t index = 0;
    int saw_value = 0;

    if (!unit || !argument || argument[0] == '\0')
        return (0);
    while (argument[index] != '\0')
    {
        const unsigned char current =
            static_cast<unsigned char>(argument[index]);

        if (std::isalpha(current) || argument[index] == '_')
        {
            char identifier[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_data_item *item;
            size_t length = 0;

            while ((std::isalnum(static_cast<unsigned char>(argument[index]))
                    || argument[index] == '_' || argument[index] == '-')
                && length + 1 < sizeof(identifier))
                identifier[length++] = argument[index++];
            identifier[length] = '\0';
            item = cblc_find_data_item_for_emit_by_source(unit, identifier);
            if (!item)
                item = cblc_find_data_item_by_cobol(unit, identifier);
            if (!item || (item->kind != CBLC_DATA_KIND_INT
                    && item->kind != CBLC_DATA_KIND_DOUBLE
                    && item->kind != CBLC_DATA_KIND_CHAR))
                return (0);
            saw_value = 1;
            continue ;
        }
        if (std::isdigit(current))
            saw_value = 1;
        else if (!std::isspace(current) && argument[index] != '+'
            && argument[index] != '-' && argument[index] != '*'
            && argument[index] != '/' && argument[index] != '%'
            && argument[index] != '(' && argument[index] != ')'
            && argument[index] != '.')
            return (0);
        index += 1;
    }
    return (saw_value);
}

static int cblc_extract_call_argument(const t_cblc_statement *statement,
    size_t argument_index, char *buffer, size_t buffer_size)
{
    const char *cursor;
    size_t current_index;
    size_t length;

    if (!statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    cursor = statement->call_arguments;
    current_index = 0;
    while (*cursor != '\0' && current_index < argument_index)
    {
        while (*cursor != '\0' && *cursor != ',')
            cursor += 1;
        if (*cursor == ',')
        {
            cursor += 1;
            current_index += 1;
        }
    }
    if (current_index != argument_index)
        return (FT_FAILURE);
    while (*cursor == ' ' || *cursor == '\t')
        cursor += 1;
    length = 0;
    while (cursor[length] != '\0' && cursor[length] != ',')
        length += 1;
    while (length > 0
        && (cursor[length - 1] == ' ' || cursor[length - 1] == '\t'))
        length -= 1;
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, cursor, length);
    buffer[length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_parse_pointer_cast_argument_kind(const t_cblc_translation_unit *unit,
    const char *argument, t_cblc_data_kind *out_kind, char *out_struct_type_name,
    size_t out_struct_type_name_size)
{
    const char *cursor;
    t_cblc_data_kind cast_kind;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_data_item *item;

    if (!unit || !argument || !out_kind || !out_struct_type_name
        || out_struct_type_name_size == 0)
        return (FT_FAILURE);
    out_struct_type_name[0] = '\0';
    cursor = argument;
    cblc_skip_whitespace(&cursor);
    if (*cursor != '(')
        return (FT_FAILURE);
    cursor += 1;
    cblc_skip_whitespace(&cursor);
    if (cblc_parse_pointer_cast_type(&cursor, unit, &cast_kind, cast_struct_type_name,
            sizeof(cast_struct_type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(&cursor);
    if (*cursor != '*')
        return (FT_FAILURE);
    cursor += 1;
    cblc_skip_whitespace(&cursor);
    if (*cursor != ')')
        return (FT_FAILURE);
    cursor += 1;
    cblc_skip_whitespace(&cursor);
    if (cblc_parse_identifier(&cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(&cursor);
    if (*cursor != '\0')
        return (FT_FAILURE);
    item = cblc_find_data_item_const(unit, identifier);
    if (!item)
        item = cblc_find_data_item_by_cobol(unit, identifier);
    if (!item || !cblc_data_kind_is_pointer(item->kind))
        return (FT_FAILURE);
    *out_kind = cast_kind;
    ft_strlcpy(out_struct_type_name, cast_struct_type_name, out_struct_type_name_size);
    return (FT_SUCCESS);
}

static int cblc_indexed_expression_struct_element_type(
    const t_cblc_translation_unit *unit, const char *argument,
    char *out_type_name, size_t out_type_name_size)
{
    const char *open_bracket;
    const char *close_bracket;
    const char *index_cursor;
    const char *path_end;
    size_t path_length;
    size_t bracket_depth;
    char path[TRANSPILE_STATEMENT_TEXT_MAX];
    const char *cursor;
    char root_name[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_data_item *root_item;
    char current_type_name[TRANSPILE_IDENTIFIER_MAX];
    int current_is_pointer;
    size_t root_length;

    if (!unit || !argument || !out_type_name || out_type_name_size == 0)
        return (FT_FAILURE);
    out_type_name[0] = '\0';
    open_bracket = std::strchr(argument, '[');
    if (!open_bracket)
        return (FT_FAILURE);
    bracket_depth = 1;
    close_bracket = NULL;
    index_cursor = open_bracket + 1;
    while (*index_cursor != '\0')
    {
        if (*index_cursor == '[')
            bracket_depth += 1;
        else if (*index_cursor == ']')
        {
            bracket_depth -= 1;
            if (bracket_depth == 0)
            {
                close_bracket = index_cursor;
                break ;
            }
        }
        index_cursor += 1;
    }
    if (!close_bracket)
        return (FT_FAILURE);
    path_end = open_bracket;
    while (path_end > argument
        && (path_end[-1] == ' ' || path_end[-1] == '\t'))
        path_end -= 1;
    index_cursor = close_bracket + 1;
    while (*index_cursor == ' ' || *index_cursor == '\t')
        index_cursor += 1;
    if (*index_cursor != '\0' || path_end == argument)
        return (FT_FAILURE);
    path_length = static_cast<size_t>(path_end - argument);
    if (path_length >= sizeof(path))
        return (FT_FAILURE);
    std::memcpy(path, argument, path_length);
    path[path_length] = '\0';

    root_item = cblc_find_data_item_for_emit_by_source(unit, path);
    if (!root_item)
        root_item = cblc_find_data_item_by_cobol(unit, path);
    if (root_item && root_item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        if (ft_strlcpy(out_type_name, root_item->struct_type_name,
                out_type_name_size) >= out_type_name_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    root_length = 0;
    if (!((path[root_length] >= 'A' && path[root_length] <= 'Z')
            || (path[root_length] >= 'a' && path[root_length] <= 'z')
            || path[root_length] == '_'))
        return (FT_FAILURE);
    while ((path[root_length] >= 'A' && path[root_length] <= 'Z')
        || (path[root_length] >= 'a' && path[root_length] <= 'z')
        || (path[root_length] >= '0' && path[root_length] <= '9')
        || path[root_length] == '_')
        root_length += 1;
    if (root_length == 0 || root_length >= sizeof(root_name))
        return (FT_FAILURE);
    std::memcpy(root_name, path, root_length);
    root_name[root_length] = '\0';
    root_item = cblc_find_data_item_for_emit_by_source(unit, root_name);
    if (!root_item)
        root_item = cblc_find_data_item_by_cobol(unit, root_name);
    if (!root_item || (root_item->kind != CBLC_DATA_KIND_STRUCT
            && root_item->kind != CBLC_DATA_KIND_STRUCT_POINTER))
        return (FT_FAILURE);
    ft_strlcpy(current_type_name, root_item->struct_type_name,
        sizeof(current_type_name));
    current_is_pointer = root_item->kind == CBLC_DATA_KIND_STRUCT_POINTER;
    cursor = path + root_length;
    while (*cursor != '\0')
    {
        const t_cblc_struct_type *owner_type;
        const t_cblc_struct_field *field;
        char member_name[TRANSPILE_IDENTIFIER_MAX];
        size_t member_length;

        while (*cursor == ' ' || *cursor == '\t')
            cursor += 1;
        if (*cursor == '.')
            cursor += 1;
        else if (cursor[0] == '-' && cursor[1] == '>')
            cursor += 2;
        else
            return (FT_FAILURE);
        while (*cursor == ' ' || *cursor == '\t')
            cursor += 1;
        member_length = 0;
        while ((cursor[member_length] >= 'A' && cursor[member_length] <= 'Z')
            || (cursor[member_length] >= 'a' && cursor[member_length] <= 'z')
            || (cursor[member_length] >= '0' && cursor[member_length] <= '9')
            || cursor[member_length] == '_')
            member_length += 1;
        if (member_length == 0 || member_length >= sizeof(member_name))
            return (FT_FAILURE);
        std::memcpy(member_name, cursor, member_length);
        member_name[member_length] = '\0';
        cursor += member_length;
        owner_type = cblc_find_struct_type(unit, current_type_name);
        field = cblc_find_field_on_type(owner_type, member_name);
        if (!field || (field->kind != CBLC_DATA_KIND_STRUCT_POINTER
                && field->kind != CBLC_DATA_KIND_STRUCT)
            || field->struct_type_name[0] == '\0')
            return (FT_FAILURE);
        ft_strlcpy(current_type_name, field->struct_type_name,
            sizeof(current_type_name));
        current_is_pointer = field->kind == CBLC_DATA_KIND_STRUCT_POINTER;
    }
    if (!current_is_pointer)
        return (FT_FAILURE);
    if (ft_strlcpy(out_type_name, current_type_name,
            out_type_name_size) >= out_type_name_size)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_argument_matches_parameter(const t_cblc_translation_unit *unit,
    const char *argument, const t_cblc_parameter *parameter)
{
    const t_cblc_data_item *item;
    t_cblc_data_kind cast_kind;
    char cast_struct_type_name[TRANSPILE_IDENTIFIER_MAX];
    char indexed_element_type_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !argument || !parameter)
        return (0);
    if (cblc_indexed_expression_struct_element_type(unit, argument,
            indexed_element_type_name, sizeof(indexed_element_type_name))
        == FT_SUCCESS)
        return (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT
            && std::strcmp(indexed_element_type_name, parameter->type_name) == 0);
    item = cblc_find_data_item_for_emit_by_source(unit, argument);
    if (parameter->array_count > 0)
    {
        if (!item || item->array_count != parameter->array_count)
            return (0);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
            return (item->kind == CBLC_DATA_KIND_INT);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
            return (item->kind == CBLC_DATA_KIND_DOUBLE);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
            return (item->kind == CBLC_DATA_KIND_CHAR);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER)
            return (item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                && parameter->length > 0 && item->length == parameter->length);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        {
            const t_cblc_struct_type *source_type;
            const t_cblc_struct_type *target_type;

            source_type = item->kind == CBLC_DATA_KIND_STRUCT
                ? cblc_find_struct_type(unit, item->struct_type_name) : NULL;
            target_type = cblc_find_struct_type(unit, parameter->type_name);
            return (source_type && target_type
                && std::strncmp(item->struct_type_name, parameter->type_name,
                    sizeof(item->struct_type_name)) == 0
                && cblc_struct_layout_fingerprint(unit, source_type, 0)
                    == cblc_struct_layout_fingerprint(unit, target_type, 0));
        }
        return (0);
    }
    if (item && item->array_count > 0
        && (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT
            || parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR
            || parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER
            || parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT))
        return (0);
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
    {
        /* A quoted source string is not an integer just because its decoded
         * contents happen to contain decimal digits (for example, "0").
         * Keeping the source spelling here is essential for overload
         * resolution between String(int capacity) and String(char *text). */
        if (argument[0] == '"')
            return (0);
        if (item && (item->kind == CBLC_DATA_KIND_STRUCT
                || cblc_data_kind_is_pointer(item->kind)
                || item->kind == CBLC_DATA_KIND_CHAR_BUFFER
                || (item->kind == CBLC_DATA_KIND_CHAR && item->length > 1)))
            return (0);
        /* An unresolved expression is not evidence of an integer. In
         * particular, a class-valued `container.elements[index]` reference
         * must not fall through to an integer constructor merely because
         * indexed template-field type resolution has not succeeded yet. */
        if (!item && !cblc_argument_is_numeric_expression(unit, argument))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
    {
        if (argument[0] == '"')
            return (0);
        if (item && item->kind != CBLC_DATA_KIND_DOUBLE)
            return (0);
        if (!item && !cblc_argument_is_numeric_expression(unit, argument))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
    {
        if (item && item->kind != CBLC_DATA_KIND_CHAR)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER)
    {
        if (argument[0] == '"')
            return (1);
        if (!item || item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
    {
        const t_cblc_struct_type *source_type;
        const t_cblc_struct_type *target_type;

        if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
            return (0);
        if (std::strncmp(item->struct_type_name, parameter->type_name,
                sizeof(item->struct_type_name)) != 0)
            return (0);
        source_type = cblc_find_struct_type(unit, item->struct_type_name);
        target_type = cblc_find_struct_type(unit, parameter->type_name);
        return (source_type && target_type
            && cblc_struct_layout_fingerprint(unit, source_type, 0)
                == cblc_struct_layout_fingerprint(unit, target_type, 0));
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
            return (cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_VOID_POINTER,
                    cast_kind));
        if (!item || !cblc_data_kind_is_pointer(item->kind))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER)
    {
        /* String literals are immutable byte sequences with pointer semantics
         * at call sites; accept them for read-only-compatible char* APIs such
         * as String's owning constructor. Lowering materializes their storage
         * separately from numeric and named-pointer arguments. */
        if (argument[0] == '"')
            return (1);
        if (item && item->kind == CBLC_DATA_KIND_CHAR && item->length > 1)
            return (1);
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
            return (cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_CHAR_POINTER,
                    cast_kind));
        if (!item || !cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_CHAR_POINTER,
                item->kind))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
            return (cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_INT_POINTER,
                    cast_kind));
        if (!item || !cblc_pointer_assignment_is_compatible(CBLC_DATA_KIND_INT_POINTER,
                item->kind))
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
    {
        if (cblc_parse_pointer_cast_argument_kind(unit, argument, &cast_kind,
                cast_struct_type_name, sizeof(cast_struct_type_name)) == FT_SUCCESS)
        {
            if (cast_kind != CBLC_DATA_KIND_STRUCT_POINTER)
                return (0);
            if (std::strncmp(cast_struct_type_name, parameter->type_name,
                    sizeof(cast_struct_type_name)) != 0)
                return (0);
            return (1);
        }
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
            return (0);
        if (std::strncmp(item->struct_type_name, parameter->type_name,
                sizeof(item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    return (0);
}

static size_t cblc_argument_conversion_cost(
    const t_cblc_translation_unit *unit, const char *argument,
    const t_cblc_parameter *parameter)
{
    const t_cblc_data_item *item;
    const char *literal;
    int floating_literal;
    int integer_literal;

    if (!cblc_argument_matches_parameter(unit, argument, parameter))
        return (static_cast<size_t>(-1));
    item = cblc_find_data_item_for_emit_by_source(unit, argument);
    if (!item)
    {
        literal = argument;
        while (*literal == ' ' || *literal == '\t')
            literal += 1;
        floating_literal = std::strchr(literal, '.') != NULL
            || std::strchr(literal, 'e') != NULL
            || std::strchr(literal, 'E') != NULL;
        integer_literal = std::isdigit(static_cast<unsigned char>(*literal))
            || ((*literal == '+' || *literal == '-')
                && std::isdigit(static_cast<unsigned char>(literal[1])));
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
        {
            if (integer_literal && !floating_literal)
                return (0);
            if (literal[0] == '\'')
                return (1);
            return (2);
        }
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
            return (floating_literal ? 0 : 1);
        if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
            return (literal[0] == '\'' ? 0 : 2);
        return (0);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
    {
        if (item->kind == CBLC_DATA_KIND_INT)
            return (0);
        if (item->kind == CBLC_DATA_KIND_CHAR)
            return (1);
        if (item->kind == CBLC_DATA_KIND_DOUBLE)
            return (2);
    }
    else if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE
        && item->kind == CBLC_DATA_KIND_DOUBLE)
        return (0);
    else if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_CHAR
        && item->kind == CBLC_DATA_KIND_CHAR)
        return (0);
    return (0);
}

static const t_cblc_method *cblc_find_method_on_type_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *identifier, const char *call_arguments,
    size_t call_argument_count)
{
    t_cblc_statement arguments;
    const t_cblc_method *best_method = NULL;
    size_t best_cost = static_cast<size_t>(-1);
    size_t method_index;
    int ambiguous = 0;

    if (!unit || !type || !identifier || !call_arguments)
        return (NULL);
    std::memset(&arguments, 0, sizeof(arguments));
    ft_strlcpy(arguments.call_arguments, call_arguments,
        sizeof(arguments.call_arguments));
    arguments.call_argument_count = call_argument_count;
    method_index = 0;
    while (method_index < type->method_count)
    {
        const t_cblc_method *method = &type->methods[method_index];
        size_t argument_index;
        size_t total_cost = 0;
        int matches = 1;

        if (std::strcmp(method->source_name, identifier) != 0
            || method->parameter_count != call_argument_count)
        {
            method_index += 1;
            continue ;
        }
        argument_index = 0;
        while (argument_index < call_argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t argument_cost;

            if (cblc_extract_call_argument(&arguments, argument_index, argument,
                    sizeof(argument)) != FT_SUCCESS)
            {
                matches = 0;
                break ;
            }
            argument_cost = cblc_argument_conversion_cost(unit, argument,
                &method->parameters[argument_index]);
            if (argument_cost == static_cast<size_t>(-1))
            {
                matches = 0;
                break ;
            }
            total_cost += argument_cost;
            argument_index += 1;
        }
        if (matches)
        {
            if (total_cost < best_cost)
            {
                best_method = method;
                best_cost = total_cost;
                ambiguous = 0;
            }
            else if (total_cost == best_cost)
                ambiguous = 1;
        }
        method_index += 1;
    }
    return (ambiguous ? NULL : best_method);
}

const t_cblc_constructor *cblc_find_constructor_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *call_arguments, size_t call_argument_count)
{
    size_t constructor_index;
    const t_cblc_constructor *best_constructor = NULL;
    size_t best_cost = static_cast<size_t>(-1);
    int ambiguous = 0;

    if (!unit || !type)
        return (NULL);
    constructor_index = 0;
    while (constructor_index < type->constructor_count)
    {
        const t_cblc_constructor *constructor;
        t_cblc_statement statement;
        size_t parameter_index;
        size_t total_cost;
        int matches;

        constructor = &type->constructors[constructor_index];
        if (constructor->parameter_count != call_argument_count)
        {
            constructor_index += 1;
            continue ;
        }
        std::memset(&statement, 0, sizeof(statement));
        if (call_arguments)
            ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
        statement.call_argument_count = call_argument_count;
        parameter_index = 0;
        total_cost = 0;
        matches = 1;
        while (parameter_index < call_argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t argument_cost;

            if (cblc_extract_call_argument(&statement, parameter_index, argument,
                    sizeof(argument)) != FT_SUCCESS)
            {
                matches = 0;
                break ;
            }
            argument_cost = cblc_argument_conversion_cost(unit, argument,
                &constructor->parameters[parameter_index]);
            if (argument_cost == static_cast<size_t>(-1))
            {
                matches = 0;
                break ;
            }
            total_cost += argument_cost;
            parameter_index += 1;
        }
        if (matches)
        {
            if (total_cost < best_cost)
            {
                best_constructor = constructor;
                best_cost = total_cost;
                ambiguous = 0;
            }
            else if (total_cost == best_cost)
                ambiguous = 1;
        }
        constructor_index += 1;
    }
    return (ambiguous ? NULL : best_constructor);
}

static int cblc_call_arguments_match_parameters(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count,
    const t_cblc_parameter *parameters, size_t parameter_count)
{
    t_cblc_statement statement;
    size_t index;

    if (!unit || !call_arguments || !parameters
        || call_argument_count != parameter_count)
        return (0);
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = call_argument_count;
    index = 0;
    while (index < parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_extract_call_argument(&statement, index, argument, sizeof(argument)) != FT_SUCCESS
            || !cblc_argument_matches_parameter(unit, argument, &parameters[index]))
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_emit_struct_array_dynamic_string_copy(
    const char *source_pointer, const char *source_capacity, const char *source_buffer,
    const char *source_length, const char *destination_pointer,
    const char *destination_capacity, const char *destination_buffer,
    const char *destination_length, t_cobol_text_builder *builder)
{
    char line[256];

    if (!source_pointer || !source_capacity || !source_buffer || !source_length
        || !destination_pointer || !destination_capacity || !destination_buffer
        || !destination_length || !builder)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "           IF %s NOT = NULL", source_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s < %s", destination_capacity,
            source_capacity) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   IF %s NOT = NULL",
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                       FREE %s",
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "                   END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line),
            "                   ALLOCATE %s CHARACTERS RETURNING %s", source_capacity,
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   MOVE %s TO %s",
            source_capacity, destination_capacity) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            source_buffer, source_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            destination_buffer, destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %s TO %s", source_buffer,
            destination_buffer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE %s TO %s",
            source_length, destination_length) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           ELSE") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               IF %s NOT = NULL",
            destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "                   FREE %s", destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               SET %s TO NULL", destination_pointer) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE 0 TO %s", destination_capacity) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || std::snprintf(line, sizeof(line), "               MOVE 0 TO %s", destination_length) < 0
        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           END-IF.") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_struct_array_field_copies(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *source_prefix,
    const char *destination_prefix, size_t element_count, size_t depth,
    t_cobol_text_builder *builder)
{
    size_t field_index;

    if (!unit || !type || !source_prefix || !destination_prefix || !builder
        || depth > CBLC_TEMPLATE_PARAMETER_MAX)
        return (FT_FAILURE);
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field;
        char source_field[TRANSPILE_IDENTIFIER_MAX];
        char destination_field[TRANSPILE_IDENTIFIER_MAX];
        size_t element_index;

        field = &type->fields[field_index];
        if (field->array_count > 0)
            return (FT_FAILURE);
        if (std::snprintf(source_field, sizeof(source_field), "%s-%s", source_prefix,
                field->cobol_name) < 0
            || std::snprintf(destination_field, sizeof(destination_field), "%s-%s",
                destination_prefix, field->cobol_name) < 0)
            return (FT_FAILURE);
        if (cblc_data_kind_is_pointer(field->kind))
        {
            if (!field->is_borrowed)
                return (FT_FAILURE);
            element_index = 0;
            while (element_index < element_count)
            {
                char line[256];

                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s(%zu) TO %s(%zu).", source_field,
                        element_index + 1, destination_field, element_index + 1) < 0
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
                || cblc_emit_struct_array_field_copies(unit, nested_type, source_field,
                    destination_field, element_count, depth + 1, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            field_index += 1;
            continue ;
        }
        if (field->kind != CBLC_DATA_KIND_CHAR_BUFFER
            && field->kind != CBLC_DATA_KIND_CHAR
            && field->kind != CBLC_DATA_KIND_INT)
            return (FT_FAILURE);
        element_index = 0;
        while (element_index < element_count)
        {
            size_t cobol_index;
            char line[256];

            cobol_index = element_index + 1;
            if (field->kind == CBLC_DATA_KIND_CHAR_BUFFER)
            {
                char source_buffer[TRANSPILE_IDENTIFIER_MAX];
                char destination_buffer[TRANSPILE_IDENTIFIER_MAX];
                char source_pointer[TRANSPILE_IDENTIFIER_MAX];
                char source_capacity[TRANSPILE_IDENTIFIER_MAX];
                char destination_pointer[TRANSPILE_IDENTIFIER_MAX];
                char destination_capacity[TRANSPILE_IDENTIFIER_MAX];
                char source_length[TRANSPILE_IDENTIFIER_MAX];
                char destination_length[TRANSPILE_IDENTIFIER_MAX];

                if (std::snprintf(source_buffer, sizeof(source_buffer), "%s-BUF",
                        source_field) < 0
                    || std::snprintf(destination_buffer, sizeof(destination_buffer), "%s-BUF",
                        destination_field) < 0
                    || std::snprintf(source_pointer, sizeof(source_pointer), "%s-PTR(%zu)",
                        source_field, cobol_index) < 0
                    || std::snprintf(source_capacity, sizeof(source_capacity), "%s-CAP(%zu)",
                        source_field, cobol_index) < 0
                    || std::snprintf(destination_pointer, sizeof(destination_pointer),
                        "%s-PTR(%zu)", destination_field, cobol_index) < 0
                    || std::snprintf(destination_capacity, sizeof(destination_capacity),
                        "%s-CAP(%zu)", destination_field, cobol_index) < 0
                    || std::snprintf(source_length, sizeof(source_length), "%s-LEN(%zu)",
                        source_field, cobol_index) < 0
                    || std::snprintf(destination_length, sizeof(destination_length),
                        "%s-LEN(%zu)", destination_field, cobol_index) < 0)
                    return (FT_FAILURE);
                if (cblc_string_field_uses_dynamic_storage(field))
                {
                    if (cblc_emit_struct_array_dynamic_string_copy(source_pointer,
                            source_capacity, source_buffer, source_length,
                            destination_pointer, destination_capacity, destination_buffer,
                            destination_length, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(source_buffer, sizeof(source_buffer), "%s-BUF(%zu)",
                            source_field, cobol_index) < 0
                        || std::snprintf(destination_buffer, sizeof(destination_buffer),
                            "%s-BUF(%zu)", destination_field, cobol_index) < 0)
                        return (FT_FAILURE);
                }
                if (!cblc_string_field_uses_dynamic_storage(field)
                    && (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                            source_buffer, destination_buffer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                            destination_length, source_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS))
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           MOVE %s(%zu) TO %s(%zu).",
                    source_field, cobol_index, destination_field, cobol_index) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            element_index += 1;
        }
        field_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_prepare_struct_pointer_field(
    const t_cblc_translation_unit *unit, const char *reference,
    t_cobol_text_builder *builder, char *field_reference,
    size_t field_reference_size, int allow_pointer_member);
static int cblc_emit_struct_copy_return(const t_cblc_translation_unit *unit,
    const char *target_name, const char *return_type_name,
    const char *source_reference,
    const t_cblc_data_item *receiver_context, t_cobol_text_builder *builder,
    int *out_handled);

static int cblc_emit_parameter_argument_moves(const t_cblc_translation_unit *unit,
    const char *call_arguments,
    size_t call_argument_count, const t_cblc_parameter *parameters,
    size_t parameter_count, t_cobol_text_builder *builder,
    const t_cblc_parameter *source_parameters,
    size_t source_parameter_count, int borrow_struct_arguments)
{
    size_t index;

    if (!unit || !call_arguments || !parameters || !builder)
        return (FT_FAILURE);
    if (parameter_count != call_argument_count)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[256];
        t_cblc_statement statement;

        std::memset(&statement, 0, sizeof(statement));
        ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
        statement.call_argument_count = call_argument_count;
        if (cblc_extract_call_argument(&statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (parameters[index].array_count > 0)
        {
            const t_cblc_data_item *item;
            size_t element_index;

            item = cblc_find_data_item_const(unit, argument);
            if (!item)
                item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (!item || item->array_count != parameters[index].array_count)
                return (FT_FAILURE);
            if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER)
            {
                if (item->kind != CBLC_DATA_KIND_CHAR_BUFFER
                    || item->length != parameters[index].length)
                    return (FT_FAILURE);
                element_index = 0;
                while (element_index < parameters[index].array_count)
                {
                    size_t cobol_index;
                    char source_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char source_pointer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_pointer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_capacity[TRANSPILE_STATEMENT_TEXT_MAX];
                    char source_length[TRANSPILE_STATEMENT_TEXT_MAX];
                    char destination_length[TRANSPILE_STATEMENT_TEXT_MAX];

                    cobol_index = element_index + 1;
                    if (std::snprintf(source_buffer, sizeof(source_buffer), "%s-BUF",
                            item->cobol_name) < 0
                        || std::snprintf(destination_buffer, sizeof(destination_buffer),
                            "%s-BUF", parameters[index].cobol_name) < 0
                        || std::snprintf(source_pointer, sizeof(source_pointer),
                            "%s-PTR(%zu)", item->cobol_name, cobol_index) < 0
                        || std::snprintf(destination_pointer, sizeof(destination_pointer),
                            "%s-PTR(%zu)", parameters[index].cobol_name, cobol_index) < 0
                        || std::snprintf(destination_capacity, sizeof(destination_capacity),
                            "%s-CAP(%zu)", parameters[index].cobol_name, cobol_index) < 0
                        || std::snprintf(source_length, sizeof(source_length),
                            "%s-LEN(%zu)", item->cobol_name, cobol_index) < 0
                        || std::snprintf(destination_length, sizeof(destination_length),
                            "%s-LEN(%zu)", parameters[index].cobol_name, cobol_index) < 0)
                        return (FT_FAILURE);
                    if (cblc_string_item_array_uses_dynamic_storage(item))
                    {
                        if (std::snprintf(line, sizeof(line),
                                "           SET ADDRESS OF %s TO %s.",
                                source_buffer, source_pointer) < 0
                            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else if (std::snprintf(source_buffer, sizeof(source_buffer),
                            "%s-BUF(%zu)", item->cobol_name, cobol_index) < 0)
                        return (FT_FAILURE);
                    if (cblc_emit_dynamic_string_ensure_capacity_for_references(
                            destination_capacity, destination_pointer, destination_buffer,
                            parameters[index].length, builder) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           SET ADDRESS OF %s TO %s.",
                            destination_buffer, destination_pointer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                            source_buffer, destination_buffer) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                            destination_length, source_length) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    element_index += 1;
                }
                index += 1;
                continue ;
            }
            if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            {
                const t_cblc_struct_type *struct_type;

                if (item->kind != CBLC_DATA_KIND_STRUCT
                    || std::strncmp(item->struct_type_name, parameters[index].type_name,
                        sizeof(item->struct_type_name)) != 0)
                    return (FT_FAILURE);
                struct_type = cblc_find_struct_type(unit, parameters[index].type_name);
                if (!struct_type)
                    return (FT_FAILURE);
                if (cblc_emit_struct_array_field_copies(unit, struct_type,
                        item->cobol_name, parameters[index].cobol_name,
                        parameters[index].array_count, 0, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
                continue ;
            }
            if (parameters[index].kind != TRANSPILE_FUNCTION_PARAMETER_INT
                || item->kind != CBLC_DATA_KIND_INT)
                return (FT_FAILURE);
            element_index = 0;
            while (element_index < parameters[index].array_count)
            {
                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s(%zu) TO %s(%zu).",
                        item->cobol_name, element_index + 1,
                        parameters[index].cobol_name, element_index + 1) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                element_index += 1;
            }
            index += 1;
            continue ;
        }
        if (source_parameters && !std::strchr(argument, '['))
        {
            const t_cblc_parameter *source_parameter = NULL;
            size_t source_index = 0;

            while (source_index < source_parameter_count)
            {
                const t_cblc_parameter *candidate =
                    &source_parameters[source_index];

                if ((candidate->source_name[0] != '\0'
                        && std::strcmp(argument, candidate->source_name) == 0)
                    || (candidate->actual_source_name[0] != '\0'
                        && std::strcmp(argument,
                            candidate->actual_source_name) == 0)
                    || (candidate->cobol_name[0] != '\0'
                        && std::strcmp(argument, candidate->cobol_name) == 0))
                {
                    source_parameter = candidate;
                    break ;
                }
                source_index += 1;
            }
            if (source_parameter
                && source_parameter->kind == parameters[index].kind
                && (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT
                    || parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_DOUBLE
                    || parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                    || parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
                    || parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER))
            {
                if (parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                    || parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
                    || parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s TO %s.",
                            parameters[index].cobol_name,
                            source_parameter->cobol_name) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line),
                        "           %s %s TO %s.",
                        parameters[index].kind
                            == TRANSPILE_FUNCTION_PARAMETER_DOUBLE
                            ? "COMPUTE" : "MOVE",
                        source_parameter->cobol_name,
                        parameters[index].cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
                continue ;
            }
        }
        if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        {
            const t_cblc_parameter *lexical_source_parameter;
            const t_cblc_data_item *item;

            lexical_source_parameter = NULL;
            if (!std::strchr(argument, '['))
            {
                size_t source_index;

                source_index = 0;
                while (source_parameters
                    && source_index < source_parameter_count)
                {
                    const t_cblc_parameter *candidate =
                        &source_parameters[source_index];

                    if ((candidate->source_name[0] != '\0'
                            && std::strcmp(argument, candidate->source_name) == 0)
                        || (candidate->actual_source_name[0] != '\0'
                            && std::strcmp(argument,
                                candidate->actual_source_name) == 0)
                        || (candidate->cobol_name[0] != '\0'
                            && std::strcmp(argument, candidate->cobol_name) == 0))
                    {
                        lexical_source_parameter = candidate;
                        break ;
                    }
                    source_index += 1;
                }
            }
            /* Resolve a lexical caller parameter before unit-scope data items:
             * exception-constructor emission can otherwise bind a same-named
             * global (often of a different type) instead of the by-value
             * class argument in the active constructor. */
            if (lexical_source_parameter)
            {
                int copy_handled;

                if (lexical_source_parameter->kind
                        != TRANSPILE_FUNCTION_PARAMETER_STRUCT
                    || std::strncmp(lexical_source_parameter->type_name,
                        parameters[index].type_name,
                        sizeof(lexical_source_parameter->type_name)) != 0)
                    return (FT_FAILURE);
                copy_handled = 0;
                if (!borrow_struct_arguments
                    && cblc_emit_struct_copy_return(unit,
                        parameters[index].cobol_name,
                        parameters[index].type_name,
                        lexical_source_parameter->cobol_name, NULL, builder,
                        &copy_handled) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (copy_handled)
                {
                    index += 1;
                    continue ;
                }
                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s TO %s.",
                        lexical_source_parameter->cobol_name,
                        parameters[index].cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                index += 1;
                continue ;
            }
            item = cblc_find_data_item_const(unit, argument);
            if (!item)
                item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (item && item->kind == CBLC_DATA_KIND_STRUCT)
            {
                int copy_handled;

                copy_handled = 0;
                if (!borrow_struct_arguments
                    && cblc_emit_struct_copy_return(unit,
                        parameters[index].cobol_name,
                        parameters[index].type_name, item->cobol_name,
                        NULL, builder, &copy_handled) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (copy_handled)
                {
                    index += 1;
                    continue ;
                }
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                        item->cobol_name, parameters[index].cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (!item
                && !std::strchr(argument, '[')
                && std::strncmp(argument, "CBLC-P-", sizeof("CBLC-P-") - 1) == 0)
            {
                /* A monomorphized template parameter has a concrete ABI
                 * layout even when its generated name is not registered as
                 * a source-level data item. This is a borrowed copy into the
                 * user copy-constructor's parameter slot, not object creation. */
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                        argument, parameters[index].cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (!item && !std::strchr(argument, '[') && source_parameters)
            {
                const t_cblc_parameter *source_parameter;
                size_t source_index;

                source_parameter = NULL;
                source_index = 0;
                while (source_index < source_parameter_count)
                {
                    const t_cblc_parameter *candidate;

                    candidate = &source_parameters[source_index];
                    if ((candidate->source_name[0] != '\0'
                            && std::strcmp(argument, candidate->source_name) == 0)
                        || (candidate->actual_source_name[0] != '\0'
                            && std::strcmp(argument,
                                candidate->actual_source_name) == 0)
                        || (candidate->cobol_name[0] != '\0'
                            && std::strcmp(argument, candidate->cobol_name) == 0))
                    {
                        source_parameter = candidate;
                        break ;
                    }
                    source_index += 1;
                }
                if (!source_parameter)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "struct argument '%.48s' unmatched in %zu caller params (first: src='%.32s' actual='%.32s' cobol='%.48s')",
                        argument, source_parameter_count,
                        source_parameter_count ? source_parameters[0].source_name : "",
                        source_parameter_count ? source_parameters[0].actual_source_name : "",
                        source_parameter_count ? source_parameters[0].cobol_name : "");
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
                if (source_parameter->kind
                        != TRANSPILE_FUNCTION_PARAMETER_STRUCT
                    || std::strncmp(source_parameter->type_name,
                        parameters[index].type_name,
                        sizeof(source_parameter->type_name)) != 0)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "caller parameter '%.64s' type '%.48s' does not match method parameter type '%.48s'",
                        argument, source_parameter->type_name,
                        parameters[index].type_name);
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s TO %s.",
                        source_parameter->cobol_name,
                        parameters[index].cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                const t_cblc_data_item *pointer_item;
                t_cblc_data_item synthetic_pointer_item;
                t_cblc_data_item synthetic_source_context;
                const t_cblc_data_item *source_context;
                const char *normalized_reference;
                char normalized_argument[TRANSPILE_STATEMENT_TEXT_MAX];
                char pointer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char source_root_name[TRANSPILE_IDENTIFIER_MAX];
                char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *open_bracket;
                const char *member_separator;
                size_t pointer_name_length;
                int matched_source_parameter;

                open_bracket = std::strchr(argument, '[');
                pointer_name_length = open_bracket
                    ? static_cast<size_t>(open_bracket - argument) : 0;
                if (pointer_name_length == 0
                    || pointer_name_length >= sizeof(pointer_name))
                    return (FT_FAILURE);
                std::memcpy(pointer_name, argument, pointer_name_length);
                pointer_name[pointer_name_length] = '\0';
                source_context = NULL;
                matched_source_parameter = 0;
                member_separator = std::strchr(pointer_name, '.');
                if (member_separator
                    && static_cast<size_t>(member_separator - pointer_name)
                        < sizeof(source_root_name))
                {
                    size_t root_length;

                    root_length = static_cast<size_t>(member_separator - pointer_name);
                    std::memcpy(source_root_name, pointer_name, root_length);
                    source_root_name[root_length] = '\0';
                    source_context = cblc_find_data_item_for_emit_by_source(unit,
                        source_root_name);
                    if (source_parameters)
                    {
                        size_t source_parameter_index = 0;

                        while (source_parameter_index < source_parameter_count)
                        {
                            const t_cblc_parameter *source_parameter =
                                &source_parameters[source_parameter_index];
                            if (std::strcmp(source_root_name,
                                    source_parameter->source_name) == 0
                                || std::strcmp(source_root_name,
                                    source_parameter->actual_source_name) == 0
                                || std::strcmp(source_root_name,
                                    source_parameter->cobol_name) == 0)
                            {
                                const t_cblc_struct_type *source_type;
                                const char *member_name;
                                const char *member_end;
                                size_t field_index;

                                source_type = cblc_find_struct_type(unit,
                                    source_parameter->type_name);
                                member_name = member_separator + 1;
                                member_end = std::strchr(member_name, '[');
                                if (!member_end)
                                    member_end = member_name + std::strlen(member_name);
                                field_index = 0;
                                while (source_type
                                    && field_index < source_type->field_count)
                                {
                                    const t_cblc_struct_field *field =
                                        &source_type->fields[field_index];
                                    size_t field_name_length =
                                        static_cast<size_t>(member_end - member_name);
                                    const char *field_cobol_name = field->cobol_name;
                                    if (std::strncmp(field_cobol_name, "CBLC-USER-",
                                            sizeof("CBLC-USER-") - 1) == 0)
                                        field_cobol_name += sizeof("CBLC-USER-") - 1;
                                    if ((std::strlen(field->source_name) == field_name_length
                                            && std::strncmp(member_name,
                                                field->source_name, field_name_length) == 0)
                                        || (std::strlen(field_cobol_name) == field_name_length
                                            && std::strncmp(member_name,
                                                field_cobol_name, field_name_length) == 0))
                                    {
                                        size_t suffix_length = std::strlen(member_end);
                                        if (std::snprintf(normalized_argument,
                                                sizeof(normalized_argument), "%s-%s%.*s",
                                                source_parameter->cobol_name,
                                                field->cobol_name,
                                                static_cast<int>(suffix_length), member_end) < 0)
                                            return (FT_FAILURE);
                                        std::memset(&synthetic_source_context, 0,
                                            sizeof(synthetic_source_context));
                                        synthetic_source_context.kind = CBLC_DATA_KIND_STRUCT;
                                        ft_strlcpy(synthetic_source_context.source_name,
                                            source_root_name,
                                            sizeof(synthetic_source_context.source_name));
                                        ft_strlcpy(synthetic_source_context.cobol_name,
                                            source_parameter->cobol_name,
                                            sizeof(synthetic_source_context.cobol_name));
                                        ft_strlcpy(synthetic_source_context.struct_type_name,
                                            source_parameter->type_name,
                                            sizeof(synthetic_source_context.struct_type_name));
                                        source_context = &synthetic_source_context;
                                        normalized_reference = normalized_argument;
                                        matched_source_parameter = 1;
                                        break ;
                                    }
                                    field_index += 1;
                                }
                                break ;
                            }
                            source_parameter_index += 1;
                        }
                    }
                }
                if (!matched_source_parameter)
                {
                    size_t source_parameter_index = 0;

                    normalized_reference = member_separator
                        ? member_separator + 1 : argument;
                    if (ft_strlcpy(normalized_argument, normalized_reference,
                            sizeof(normalized_argument)) >= sizeof(normalized_argument))
                        return (FT_FAILURE);
                    while (source_parameters
                        && source_parameter_index < source_parameter_count)
                    {
                        const t_cblc_parameter *source_parameter =
                            &source_parameters[source_parameter_index];
                        size_t parameter_name_length =
                            std::strlen(source_parameter->cobol_name);

                        if (parameter_name_length > 0
                            && std::strncmp(pointer_name,
                                source_parameter->cobol_name,
                                parameter_name_length) == 0
                            && pointer_name[parameter_name_length] == '-')
                        {
                            std::memset(&synthetic_source_context, 0,
                                sizeof(synthetic_source_context));
                            synthetic_source_context.kind = CBLC_DATA_KIND_STRUCT;
                            ft_strlcpy(synthetic_source_context.cobol_name,
                                source_parameter->cobol_name,
                                sizeof(synthetic_source_context.cobol_name));
                            ft_strlcpy(synthetic_source_context.struct_type_name,
                                source_parameter->type_name,
                                sizeof(synthetic_source_context.struct_type_name));
                            source_context = &synthetic_source_context;
                            break ;
                        }
                        source_parameter_index += 1;
                    }
                }
                open_bracket = std::strchr(normalized_argument, '[');
                pointer_name_length = open_bracket
                    ? static_cast<size_t>(open_bracket - normalized_argument) : 0;
                if (pointer_name_length == 0
                    || pointer_name_length >= sizeof(pointer_name))
                    return (FT_FAILURE);
                std::memcpy(pointer_name, normalized_argument, pointer_name_length);
                pointer_name[pointer_name_length] = '\0';
                pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_name,
                    &synthetic_pointer_item, NULL);
                if (!pointer_item || source_context)
                    pointer_item = cblc_resolve_struct_pointer_item(unit, pointer_name,
                        &synthetic_pointer_item, source_context);
                if (!pointer_item || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                    || std::strcmp(pointer_item->struct_type_name,
                        parameters[index].type_name) != 0)
                {
                    return (FT_FAILURE);
                }
                if (cblc_emit_prepare_struct_pointer_value(unit, normalized_argument,
                        builder, based_name, sizeof(based_name), NULL) != FT_SUCCESS
                    && cblc_emit_prepare_struct_pointer_value(unit, normalized_argument,
                        builder, based_name, sizeof(based_name), source_context)
                        != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
                if (std::snprintf(line, sizeof(line),
                        "           MOVE %s TO %s.", based_name,
                        parameters[index].cobol_name) < 0)
                    return (FT_FAILURE);
            }
        }
        else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER)
        {
            const t_cblc_data_item *item;
            char line_length[256];
            char line_buffer[256];

            item = cblc_find_data_item_for_emit_by_source(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
            {
                return (FT_FAILURE);
            }
            if (std::snprintf(line_buffer, sizeof(line_buffer), "           MOVE %s-BUF TO %s-BUF.",
                    item->cobol_name, parameters[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (std::snprintf(line_length, sizeof(line_length), "           COMPUTE %s-LEN = %s-LEN.",
                    parameters[index].cobol_name, item->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(line, line_length, sizeof(line));
        }
        else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER
            || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
            || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
            || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
        {
            const t_cblc_data_item *item;

            item = cblc_find_data_item_for_emit_by_source(unit, argument);
            /* Substituted class-method bodies pass scope-qualified COBOL
             * spellings for locals (for example a fixed char buffer). Those
             * names are already canonical and must resolve through the COBOL
             * namespace rather than the source-name lookup. */
            if (!item)
                item = cblc_find_data_item_by_cobol(unit, argument);
            if (item && cblc_data_kind_is_pointer(item->kind))
            {
                if (std::snprintf(line, sizeof(line), "           SET %s TO %s.",
                        parameters[index].cobol_name, item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (parameters[index].kind
                    == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                && item && item->kind == CBLC_DATA_KIND_CHAR
                && item->length > 1)
            {
                if (std::snprintf(line, sizeof(line),
                        "           SET %s TO ADDRESS OF %s.",
                        parameters[index].cobol_name, item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (parameters[index].kind
                    == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                && item && item->kind == CBLC_DATA_KIND_CHAR_BUFFER)
            {
                /* A fixed local `char buffer[N]` is represented by a COBOL
                 * group containing its byte storage. Pass the address of that
                 * storage, not the group (or its length field), to a char* ABI.
                 * Dynamically-backed buffers already own a pointer slot. */
                if (cblc_string_item_uses_dynamic_storage(item))
                {
                    if (std::snprintf(line, sizeof(line),
                            "           SET %s TO %s-PTR.",
                            parameters[index].cobol_name,
                            item->cobol_name) < 0)
                        return (FT_FAILURE);
                }
                else if (std::snprintf(line, sizeof(line),
                            "           SET %s TO ADDRESS OF %s-BUF.",
                            parameters[index].cobol_name,
                            item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else if (parameters[index].kind
                    == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
            {
                const char *source_object_name;
                char source_object_cobol_name[TRANSPILE_IDENTIFIER_MAX];

                source_object_name = NULL;
                if (item && item->kind == CBLC_DATA_KIND_STRUCT
                    && std::strcmp(item->struct_type_name,
                        parameters[index].type_name) == 0)
                    source_object_name = item->cobol_name;
                else
                {
                    size_t source_index;

                    source_index = 0;
                    while (source_parameters
                        && source_index < source_parameter_count)
                    {
                        const t_cblc_parameter *source_parameter;

                        source_parameter = &source_parameters[source_index];
                        if (source_parameter->kind
                                == TRANSPILE_FUNCTION_PARAMETER_STRUCT
                            && std::strcmp(source_parameter->type_name,
                                parameters[index].type_name) == 0
                            && ((source_parameter->source_name[0] != '\0'
                                    && std::strcmp(argument,
                                        source_parameter->source_name) == 0)
                                || (source_parameter->actual_source_name[0] != '\0'
                                    && std::strcmp(argument,
                                        source_parameter->actual_source_name) == 0)
                                || (source_parameter->cobol_name[0] != '\0'
                                    && std::strcmp(argument,
                                        source_parameter->cobol_name) == 0)))
                        {
                            ft_strlcpy(source_object_cobol_name,
                                source_parameter->cobol_name,
                                sizeof(source_object_cobol_name));
                            source_object_name = source_object_cobol_name;
                            break ;
                        }
                        source_index += 1;
                    }
                }
                if (!source_object_name
                    || std::snprintf(line, sizeof(line),
                        "           SET %s TO ADDRESS OF %s.",
                        parameters[index].cobol_name, source_object_name) < 0)
                    return (FT_FAILURE);
            }
            else
                return (FT_FAILURE);
        }
        else if (cblc_argument_is_numeric_literal(argument))
        {
            if ((parameters[index].kind
                    == TRANSPILE_FUNCTION_PARAMETER_DOUBLE
                    ? std::snprintf(line, sizeof(line),
                        "           COMPUTE %s = %s.",
                        parameters[index].cobol_name, argument)
                    : std::snprintf(line, sizeof(line),
                        "           MOVE %s TO %s.", argument,
                        parameters[index].cobol_name)) < 0)
                return (FT_FAILURE);
        }
        else
        {
            const char *expression_source = argument;
            const char *member_arrow = std::strstr(argument, "->");
            const t_cblc_data_item *scalar_item = NULL;
            char canonical_member_expression[TRANSPILE_STATEMENT_TEXT_MAX];

            if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT
                || parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
            {
                scalar_item = cblc_find_data_item_const(unit, argument);
                if (!scalar_item)
                    scalar_item = cblc_find_data_item_for_emit_by_source(unit,
                        argument);
                if (scalar_item
                    && ((parameters[index].kind
                            == TRANSPILE_FUNCTION_PARAMETER_INT
                            && scalar_item->kind == CBLC_DATA_KIND_INT)
                        || (parameters[index].kind
                            == TRANSPILE_FUNCTION_PARAMETER_DOUBLE
                            && scalar_item->kind == CBLC_DATA_KIND_DOUBLE)))
                {
                    if (std::snprintf(line, sizeof(line),
                            "           %s %s TO %s.",
                            parameters[index].kind
                                == TRANSPILE_FUNCTION_PARAMETER_DOUBLE
                                ? "COMPUTE" : "MOVE",
                            scalar_item->cobol_name,
                            parameters[index].cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line)
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                    index += 1;
                    continue ;
                }
            }
            else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
            {
                scalar_item = cblc_find_data_item_const(unit, argument);
                if (!scalar_item)
                    scalar_item = cblc_find_data_item_for_emit_by_source(unit,
                        argument);
                if (scalar_item && scalar_item->kind == CBLC_DATA_KIND_CHAR)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           MOVE %s TO %s.", scalar_item->cobol_name,
                            parameters[index].cobol_name) < 0
                        || cobol_text_builder_append_line(builder, line)
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                    index += 1;
                    continue ;
                }
            }
            if (member_arrow && source_parameters)
            {
                char source_root[TRANSPILE_IDENTIFIER_MAX];
                const size_t source_root_length =
                    static_cast<size_t>(member_arrow - argument);
                if (source_root_length > 0
                    && source_root_length < sizeof(source_root))
                {
                    std::memcpy(source_root, argument, source_root_length);
                    source_root[source_root_length] = '\0';
                    for (size_t source_index = 0;
                        source_index < source_parameter_count; ++source_index)
                    {
                        const t_cblc_parameter &source_parameter =
                            source_parameters[source_index];
                        if (std::strcmp(source_root,
                                source_parameter.source_name) == 0
                            || std::strcmp(source_root,
                                source_parameter.actual_source_name) == 0
                            || std::strcmp(source_root,
                                source_parameter.cobol_name) == 0)
                        {
                            const size_t suffix_length =
                                std::strlen(member_arrow);
                            const size_t canonical_root_length =
                                std::strlen(source_parameter.cobol_name);
                            if (source_parameter.cobol_name[0] != '\0'
                                && source_parameter.kind
                                    == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                                && canonical_root_length + suffix_length
                                    < sizeof(canonical_member_expression))
                            {
                                std::memcpy(canonical_member_expression,
                                    source_parameter.cobol_name,
                                    canonical_root_length);
                                std::memcpy(canonical_member_expression
                                        + canonical_root_length,
                                    member_arrow, suffix_length + 1);
                                expression_source = canonical_member_expression;
                            }
                            break;
                        }
                    }
                }
            }
            if (cblc_emit_prepare_struct_pointer_field(unit, expression_source,
                    builder, expression, sizeof(expression), 0) != FT_SUCCESS
                && cblc_translate_expression_for_cobol(expression_source,
                    expression, sizeof(expression)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    parameters[index].cobol_name, expression) < 0)
                return (FT_FAILURE);
        }
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_cobol_text_builder *builder)
{
    if (!unit || !target_function || !statement || !builder)
        return (FT_FAILURE);
    return (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
            statement->call_argument_count, target_function->parameters,
            target_function->parameter_count, builder));
}
