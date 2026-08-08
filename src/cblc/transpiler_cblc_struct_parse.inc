static int cblc_parse_template_header(const char **cursor,
    char parameter_names[][TRANSPILE_IDENTIFIER_MAX], size_t *out_count);
static int cblc_template_argument_mangle_key(const char *argument,
    char *key, size_t key_size);
static int cblc_template_arguments_to_key(
    const char arguments[][TRANSPILE_IDENTIFIER_MAX], size_t argument_count,
    char *key, size_t key_size);
static void cblc_record_template_instantiation_error(
    t_cblc_translation_unit *unit, int code, const char *message,
    const char *template_name, const char *argument_key,
    size_t definition_offset, size_t use_offset);
static int cblc_function_return_kind_from_data_kind(t_cblc_data_kind kind,
    t_cblc_function_return_kind *out_kind);

static int cblc_active_template_parameter_index(const char *name, size_t *out_index)
{
    size_t index;

    if (!name || !out_index)
        return (FT_FAILURE);
    index = 0;
    while (index < g_cblc_active_template_parameter_count)
    {
        if (std::strcmp(name, g_cblc_active_template_parameters[index]) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    if (g_cblc_active_template_parameter
        && std::strcmp(name, g_cblc_active_template_parameter) == 0)
    {
        *out_index = 0;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_parse_borrowed_struct_pointer_field(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_struct_type *type,
    t_cblc_member_visibility visibility, int is_const)
{
    const char *start;
    char base_name[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_kind kind;
    size_t pointer_depth;

    if (!cursor || !*cursor || !unit || !type)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "borrowed"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_match_keyword(cursor, "int"))
    {
        kind = CBLC_DATA_KIND_INT_POINTER;
        ft_strlcpy(base_name, "int", sizeof(base_name));
    }
    else if (cblc_match_keyword(cursor, "char"))
    {
        kind = CBLC_DATA_KIND_CHAR_POINTER;
        ft_strlcpy(base_name, "char", sizeof(base_name));
    }
    else if (cblc_match_keyword(cursor, "void"))
    {
        kind = CBLC_DATA_KIND_VOID_POINTER;
        ft_strlcpy(base_name, "void", sizeof(base_name));
    }
    else if (cblc_parse_identifier(cursor, base_name, sizeof(base_name)) == FT_SUCCESS
        && cblc_find_struct_type(unit, base_name))
        kind = CBLC_DATA_KIND_STRUCT_POINTER;
    else
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    pointer_depth = 0;
    while (**cursor == '*')
    {
        *cursor += 1;
        pointer_depth += 1;
        cblc_skip_whitespace(cursor);
    }
    if (pointer_depth != 1
        || cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
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
    if (cblc_struct_type_append_field(type, identifier, 0, 0, kind,
            kind == CBLC_DATA_KIND_STRUCT_POINTER ? base_name : NULL,
            is_const, visibility) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    type->fields[type->field_count - 1].is_borrowed = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_struct_field_declaration(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type, t_cblc_member_visibility visibility)
{
    const char *start;
    const t_cblc_struct_type *field_type;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t length;
    size_t array_count;
    int is_const;
    if (!cursor || !*cursor || !unit || !type)
        return (FT_FAILURE);
    is_const = 0;
    if (cblc_match_keyword(cursor, "const"))
    {
        is_const = 1;
            cblc_skip_whitespace(cursor);
    }
    {
        const char *pointer_start = *cursor;

        if (cblc_parse_borrowed_struct_pointer_field(cursor, unit, type, visibility,
                is_const) == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = pointer_start;
    }
    if (cblc_match_keyword(cursor, "int"))
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        array_count = 0;
        if (**cursor == '[')
        {
            if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, 0, array_count,
                CBLC_DATA_KIND_INT, NULL,
                is_const, visibility));
    }
    if (cblc_match_keyword(cursor, "char"))
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        length = 1;
        if (**cursor == '[')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            if (cblc_parse_numeric_literal(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ']')
                return (FT_FAILURE);
            *cursor += 1;
            cblc_skip_whitespace(cursor);
        }
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, length, 0, CBLC_DATA_KIND_CHAR, NULL,
                is_const, visibility));
    }
    if (cblc_match_keyword(cursor, "string"))
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        length = 1;
        array_count = 0;
        if (**cursor == '[')
        {
            if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
                return (FT_FAILURE);
            if (**cursor != '(')
                return (FT_FAILURE);
            if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '(')
        {
            if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, length, array_count,
                CBLC_DATA_KIND_STRING, NULL,
                is_const, visibility));
    }
    start = *cursor;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    {
        size_t template_parameter_index;

        if (cblc_active_template_parameter_index(type_identifier,
                &template_parameter_index) == FT_SUCCESS)
        {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
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
        if (cblc_struct_type_append_field(type, identifier, 0, 0, CBLC_DATA_KIND_STRUCT,
                type_identifier, is_const, visibility) != FT_SUCCESS)
            return (FT_FAILURE);
        type->fields[type->field_count - 1].is_template_parameter = 1;
        ft_strlcpy(type->fields[type->field_count - 1].template_parameter_name,
            type_identifier, sizeof(type->fields[type->field_count - 1].template_parameter_name));
            return (FT_SUCCESS);
        }
    }
    field_type = cblc_find_struct_type(unit, type_identifier);
    if (!field_type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, "std::", std::strlen("std::")) == 0)
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
    return (cblc_struct_type_append_field(type, identifier, 0, 0, CBLC_DATA_KIND_STRUCT,
        field_type->source_name, is_const, visibility));
}

static int cblc_parse_lifecycle_declaration(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type)
{
    const char *start;
    t_cblc_constructor *constructor;
    t_cblc_function constructor_function;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char scope_source_name[TRANSPILE_IDENTIFIER_MAX];
    char scope_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    int is_destructor;
    size_t constructor_parameter_count;
    size_t parameter_scope_base;
    t_cblc_parameter constructor_parameters[TRANSPILE_FUNCTION_PARAMETER_MAX];

    if (!cursor || !*cursor || !unit || !type || !type->is_class)
        return (FT_FAILURE);
    start = *cursor;
    is_destructor = 0;
    std::memset(&constructor_function, 0, sizeof(constructor_function));
    constructor_parameter_count = 0;
    parameter_scope_base = unit->data_count;
    std::memset(constructor_parameters, 0, sizeof(constructor_parameters));
    if (**cursor == '~')
    {
        is_destructor = 1;
        *cursor += 1;
    }
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, type->source_name, sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (!is_destructor)
    {
        if (std::snprintf(scope_source_name, sizeof(scope_source_name), "%s__ctor",
                type->source_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (std::snprintf(scope_cobol_name, sizeof(scope_cobol_name), "%s-CTOR",
                type->cobol_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (type->constructor_count > 0)
        {
            cblc_build_constructor_scope_name(type->source_name, "__ctor_",
                type->constructor_count, scope_source_name,
                sizeof(scope_source_name));
            cblc_build_constructor_scope_name(type->cobol_name, "-CTOR-",
                type->constructor_count, scope_cobol_name,
                sizeof(scope_cobol_name));
        }
        if (cblc_parse_parameter_list(cursor, unit, constructor_parameters,
                &constructor_parameter_count, scope_source_name, scope_cobol_name,
                NULL, type) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(constructor_function.source_name, scope_source_name,
            sizeof(constructor_function.source_name));
        ft_strlcpy(constructor_function.cobol_name, scope_cobol_name,
            sizeof(constructor_function.cobol_name));
        std::memcpy(constructor_function.parameters, constructor_parameters,
            sizeof(constructor_parameters));
        constructor_function.parameter_count = constructor_parameter_count;
    }
    else
        cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == ';')
    {
        *cursor += 1;
        if (is_destructor)
            type->has_destructor = 1;
        else
        {
            unit->data_count = parameter_scope_base;
            if (cblc_constructor_signature_exists(type, constructor_parameters,
                    constructor_parameter_count))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            if (type->constructor_count >= type->constructor_capacity)
            {
                if (cblc_struct_type_ensure_constructor_capacity(type,
                        type->constructor_capacity == 0 ? 2 : type->constructor_capacity * 2)
                    != FT_SUCCESS)
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
            }
            constructor = &type->constructors[type->constructor_count];
            std::memset(constructor, 0, sizeof(*constructor));
            constructor->parameter_count = constructor_parameter_count;
            std::memcpy(constructor->parameters, constructor_parameters,
                sizeof(constructor->parameters));
            constructor->has_definition = 0;
            type->constructor_count += 1;
            if (constructor_parameter_count == 0)
                type->has_default_constructor = 1;
        }
        return (FT_SUCCESS);
    }
    if (**cursor == '{' || (!is_destructor && **cursor == ':'))
    {
        t_cblc_statement *statements;
        size_t statement_count;
        size_t statement_capacity;

        statements = NULL;
        statement_count = 0;
        statement_capacity = 0;
        if ((is_destructor
                ? cblc_capture_lifecycle_body(cursor, unit, type, &statements, &statement_count,
                    &statement_capacity)
                : cblc_capture_constructor_body(cursor, unit, type, &constructor_function,
                    &statements, &statement_count, &statement_capacity)) != FT_SUCCESS)
        {
            if (constructor_function.local_destructor_targets)
                cma_free(constructor_function.local_destructor_targets);
            *cursor = start;
            return (FT_FAILURE);
        }
        if (is_destructor)
        {
            if (type->destructor_statements)
                cma_free(type->destructor_statements);
            type->destructor_statements = statements;
            type->destructor_statement_count = statement_count;
            type->destructor_statement_capacity = statement_capacity;
            type->has_destructor = 1;
            type->has_destructor_definition = 1;
        }
        else
        {
            if (cblc_constructor_signature_exists(type, constructor_parameters,
                    constructor_parameter_count))
            {
                cma_free(statements);
                if (constructor_function.local_destructor_targets)
                    cma_free(constructor_function.local_destructor_targets);
                *cursor = start;
                return (FT_FAILURE);
            }
            if (type->constructor_count >= type->constructor_capacity)
            {
                if (cblc_struct_type_ensure_constructor_capacity(type,
                        type->constructor_capacity == 0 ? 2 : type->constructor_capacity * 2)
                    != FT_SUCCESS)
                {
                    cma_free(statements);
                    if (constructor_function.local_destructor_targets)
                        cma_free(constructor_function.local_destructor_targets);
                    *cursor = start;
                    return (FT_FAILURE);
                }
            }
            constructor = &type->constructors[type->constructor_count];
            std::memset(constructor, 0, sizeof(*constructor));
            constructor->statements = statements;
            constructor->statement_count = statement_count;
            constructor->statement_capacity = statement_capacity;
            constructor->parameter_count = constructor_parameter_count;
            std::memcpy(constructor->parameters, constructor_parameters,
                sizeof(constructor->parameters));
            constructor->has_definition = 1;
            type->constructor_count += 1;
            if (constructor_parameter_count == 0)
                type->has_default_constructor = 1;
        }
        if (constructor_function.local_destructor_targets)
            cma_free(constructor_function.local_destructor_targets);
        return (FT_SUCCESS);
    }
    *cursor = start;
    return (FT_FAILURE);
}

static t_cblc_method *cblc_find_mutable_method_on_type(t_cblc_struct_type *type,
    const char *identifier)
{
    size_t index;

    if (!type || !identifier)
        return (NULL);
    index = 0;
    while (index < type->method_count)
    {
        if (std::strncmp(type->methods[index].source_name, identifier,
                sizeof(type->methods[index].source_name)) == 0)
            return (&type->methods[index]);
        index += 1;
    }
    return (NULL);
}

static t_cblc_constructor *cblc_find_mutable_constructor_for_parameters(
    t_cblc_struct_type *type, const t_cblc_parameter *parameters, size_t parameter_count)
{
    size_t index;

    if (!type)
        return (NULL);
    index = 0;
    while (index < type->constructor_count)
    {
        if (cblc_constructor_signatures_match(&type->constructors[index], parameters,
                parameter_count))
            return (&type->constructors[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_parse_plain_identifier(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    length = 0;
    while (cblc_is_identifier_char((*cursor)[length]))
        length += 1;
    if (length == 0 || length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, *cursor, length);
    buffer[length] = '\0';
    *cursor += length;
    return (FT_SUCCESS);
}

static int cblc_method_parameters_match(const t_cblc_method *method,
    const t_cblc_parameter *parameters, size_t parameter_count)
{
    size_t index;

    if (!method || !parameters)
        return (0);
    if (method->parameter_count != parameter_count)
        return (0);
    index = 0;
    while (index < parameter_count)
    {
        if (method->parameters[index].kind != parameters[index].kind)
            return (0);
        if (method->parameters[index].length != parameters[index].length
            || method->parameters[index].array_count != parameters[index].array_count)
            return (0);
        if (std::strncmp(method->parameters[index].type_name, parameters[index].type_name,
                sizeof(method->parameters[index].type_name)) != 0)
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_normalize_call_arguments(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count, char *buffer,
    size_t buffer_size)
{
    size_t index;
    t_cblc_statement statement;

    if (!unit || !call_arguments || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = call_argument_count;
    index = 0;
    while (index < call_argument_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *resolved_argument;
        const t_cblc_data_item *item;

        if (cblc_extract_call_argument(&statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        resolved_argument = argument;
        if (argument[0] != '"')
        {
            item = cblc_find_data_item_const(unit, argument);
            if (item)
            {
                const t_cblc_data_item *actual_item;

                actual_item = cblc_find_data_item_by_cobol(unit, item->cobol_name);
                if (actual_item && !actual_item->is_alias)
                    resolved_argument = actual_item->source_name;
            }
        }
        if (index > 0)
        {
            ft_strlcat(buffer, ", ", buffer_size);
        }
        ft_strlcat(buffer, resolved_argument, buffer_size);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_capture_method_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *method_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity)
{
    size_t saved_data_count;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !method_function
        || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
    {
        return (FT_FAILURE);
    }
    if (cblc_add_parameter_aliases(unit, method_function->parameters,
            method_function->parameter_count, type) != FT_SUCCESS)
    {
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
        return (FT_FAILURE);
    }
    g_cblc_member_access_type = type;
    status = cblc_parse_statement_block(cursor, unit, method_function, 1);
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    g_cblc_member_access_type = saved_access_type;
    if (status != FT_SUCCESS
        || (method_function->return_kind != CBLC_FUNCTION_RETURN_VOID
            && !method_function->saw_return))
    {
        return (FT_FAILURE);
    }
    *out_statements = method_function->statements;
    *out_count = method_function->statement_count;
    *out_capacity = method_function->statement_capacity;
    method_function->statements = NULL;
    method_function->statement_count = 0;
    method_function->statement_capacity = 0;
    return (FT_SUCCESS);
}

static int cblc_parse_method_definition(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type, t_cblc_member_visibility visibility)
{
    const char *start;
    t_cblc_method *method;
    t_cblc_function method_function;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char scope_source_name[TRANSPILE_IDENTIFIER_MAX];
    char scope_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_function_return_kind return_kind;
    char return_type_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;
    size_t parameter_scope_base;

    if (!cursor || !*cursor || !unit || !type || !type->is_class)
        return (FT_FAILURE);
    start = *cursor;
    return_type_name[0] = '\0';
    parameter_scope_base = unit->data_count;
    if (cblc_parse_function_return_type(cursor, unit, &return_kind,
            return_type_name, sizeof(return_type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, type->source_name, sizeof(identifier)) == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    method = cblc_find_mutable_method_on_type(type, identifier);
    if (method && method->has_definition)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    std::memset(&method_function, 0, sizeof(method_function));
    method_function.return_kind = return_kind;
    if (return_kind == CBLC_FUNCTION_RETURN_INT)
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    else if (return_kind == CBLC_FUNCTION_RETURN_STRUCT
        || return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        ft_strlcpy(method_function.return_type_name, return_type_name,
            sizeof(method_function.return_type_name));
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    }
    if (std::snprintf(scope_source_name, sizeof(scope_source_name), "%s__%s",
            type->source_name, identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(scope_cobol_name, sizeof(scope_cobol_name), "%s-%s",
            type->cobol_name, identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(method_function.source_name, scope_source_name,
        sizeof(method_function.source_name));
    ft_strlcpy(method_function.cobol_name, scope_cobol_name,
        sizeof(method_function.cobol_name));
    if (cblc_parse_parameter_list(cursor, unit, method_function.parameters,
            &method_function.parameter_count, scope_source_name, scope_cobol_name,
            NULL, type) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == ';')
    {
        *cursor += 1;
        unit->data_count = parameter_scope_base;
        if (!method)
        {
            if (type->method_count >= type->method_capacity)
            {
                if (cblc_struct_type_ensure_method_capacity(type,
                        type->method_capacity == 0 ? 2 : type->method_capacity * 2) != FT_SUCCESS)
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
            }
            method = &type->methods[type->method_count];
            std::memset(method, 0, sizeof(*method));
            type->method_count += 1;
        }
        ft_strlcpy(method->source_name, identifier, sizeof(method->source_name));
        cblc_identifier_to_cobol(identifier, method->cobol_name, sizeof(method->cobol_name));
        method->return_kind = return_kind;
        ft_strlcpy(method->return_type_name, return_type_name,
            sizeof(method->return_type_name));
        method->visibility = visibility;
        method->parameter_count = method_function.parameter_count;
        std::memcpy(method->parameters, method_function.parameters, sizeof(method->parameters));
        method->has_definition = 0;
        return (FT_SUCCESS);
    }
    if (**cursor != '{')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statements = NULL;
    statement_count = 0;
    statement_capacity = 0;
    if (cblc_capture_method_body(cursor, unit, type, &method_function, &statements,
            &statement_count, &statement_capacity) != FT_SUCCESS)
    {
        if (method_function.local_destructor_targets)
            cma_free(method_function.local_destructor_targets);
        *cursor = start;
        return (FT_FAILURE);
    }
    if (!method && type->method_count >= type->method_capacity)
    {
        if (cblc_struct_type_ensure_method_capacity(type,
                type->method_capacity == 0 ? 2 : type->method_capacity * 2) != FT_SUCCESS)
        {
            cma_free(statements);
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    if (!method)
    {
        method = &type->methods[type->method_count];
        std::memset(method, 0, sizeof(*method));
        type->method_count += 1;
    }
    else if (method->statements)
    {
        cma_free(method->statements);
        method->statements = NULL;
        method->statement_count = 0;
        method->statement_capacity = 0;
    }
    ft_strlcpy(method->source_name, identifier, sizeof(method->source_name));
    cblc_identifier_to_cobol(identifier, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = return_kind;
    ft_strlcpy(method->return_type_name, return_type_name,
        sizeof(method->return_type_name));
    method->visibility = visibility;
    method->parameter_count = method_function.parameter_count;
    std::memcpy(method->parameters, method_function.parameters, sizeof(method->parameters));
    method->statements = statements;
    method->statement_count = statement_count;
    method->statement_capacity = statement_capacity;
    method->has_definition = 1;
    if (method_function.local_destructor_targets)
        cma_free(method_function.local_destructor_targets);
    return (FT_SUCCESS);
}

static int cblc_parse_qualified_lifecycle_definition(const char **cursor,
    t_cblc_translation_unit *unit)
{
    const char *start;
    t_cblc_struct_type *type;
    t_cblc_constructor *constructor;
    t_cblc_function constructor_function;
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;
    char type_name[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char scope_source_name[TRANSPILE_IDENTIFIER_MAX];
    char scope_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    int is_destructor;
    size_t constructor_parameter_count;
    t_cblc_parameter constructor_parameters[TRANSPILE_FUNCTION_PARAMETER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    is_destructor = 0;
    statements = NULL;
    statement_count = 0;
    statement_capacity = 0;
    constructor_parameter_count = 0;
    std::memset(&constructor_function, 0, sizeof(constructor_function));
    std::memset(constructor_parameters, 0, sizeof(constructor_parameters));
    if (cblc_parse_plain_identifier(cursor, type_name, sizeof(type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = const_cast<t_cblc_struct_type *>(cblc_find_struct_type(unit, type_name));
    if (!type || !type->is_class)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if ((*cursor)[0] != ':' || (*cursor)[1] != ':')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 2;
    cblc_skip_whitespace(cursor);
    if (**cursor == '~')
    {
        is_destructor = 1;
        *cursor += 1;
    }
    if (cblc_parse_plain_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, type->source_name, sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (!is_destructor)
    {
        if (std::snprintf(scope_source_name, sizeof(scope_source_name), "%s__ctor",
                type->source_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (std::snprintf(scope_cobol_name, sizeof(scope_cobol_name), "%s-CTOR",
                type->cobol_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_parse_parameter_list(cursor, unit, constructor_parameters,
                &constructor_parameter_count, scope_source_name, scope_cobol_name,
                NULL, type) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(constructor_function.source_name, scope_source_name,
            sizeof(constructor_function.source_name));
        ft_strlcpy(constructor_function.cobol_name, scope_cobol_name,
            sizeof(constructor_function.cobol_name));
        std::memcpy(constructor_function.parameters, constructor_parameters,
            sizeof(constructor_parameters));
        constructor_function.parameter_count = constructor_parameter_count;
    }
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{' && (!is_destructor || **cursor != '{'))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (is_destructor)
    {
        if (type->has_destructor_definition)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_capture_lifecycle_body(cursor, unit, type, &statements, &statement_count,
                &statement_capacity) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (type->destructor_statements)
            cma_free(type->destructor_statements);
        type->destructor_statements = statements;
        type->destructor_statement_count = statement_count;
        type->destructor_statement_capacity = statement_capacity;
        type->has_destructor = 1;
        type->has_destructor_definition = 1;
        return (FT_SUCCESS);
    }
    constructor = cblc_find_mutable_constructor_for_parameters(type, constructor_parameters,
            constructor_parameter_count);
    if (!constructor || constructor->has_definition)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_capture_constructor_body(cursor, unit, type, &constructor_function, &statements,
            &statement_count, &statement_capacity) != FT_SUCCESS)
    {
        if (constructor_function.local_destructor_targets)
            cma_free(constructor_function.local_destructor_targets);
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor->statements)
        cma_free(constructor->statements);
    constructor->statements = statements;
    constructor->statement_count = statement_count;
    constructor->statement_capacity = statement_capacity;
    constructor->has_definition = 1;
    if (constructor_function.local_destructor_targets)
        cma_free(constructor_function.local_destructor_targets);
    return (FT_SUCCESS);
}

static int cblc_parse_qualified_method_definition(const char **cursor,
    t_cblc_translation_unit *unit)
{
    const char *start;
    t_cblc_struct_type *type;
    t_cblc_method *method;
    t_cblc_function method_function;
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;
    char type_name[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char scope_source_name[TRANSPILE_IDENTIFIER_MAX];
    char scope_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char return_type_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_function_return_kind return_kind;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    return_type_name[0] = '\0';
    if (cblc_parse_function_return_type(cursor, unit, &return_kind,
            return_type_name, sizeof(return_type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_plain_identifier(cursor, type_name, sizeof(type_name)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    type = const_cast<t_cblc_struct_type *>(cblc_find_struct_type(unit, type_name));
    if (!type || !type->is_class)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if ((*cursor)[0] != ':' || (*cursor)[1] != ':')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 2;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_plain_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    method = cblc_find_mutable_method_on_type(type, identifier);
    if (!method || method->has_definition)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    std::memset(&method_function, 0, sizeof(method_function));
    method_function.return_kind = return_kind;
    if (return_kind == CBLC_FUNCTION_RETURN_INT)
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    else if (return_kind == CBLC_FUNCTION_RETURN_STRUCT
        || return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    }
    ft_strlcpy(method_function.return_type_name, return_type_name,
        sizeof(method_function.return_type_name));
    if (std::snprintf(scope_source_name, sizeof(scope_source_name), "%s__%s",
            type->source_name, identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(scope_cobol_name, sizeof(scope_cobol_name), "%s-%s",
            type->cobol_name, identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(method_function.source_name, scope_source_name,
        sizeof(method_function.source_name));
    ft_strlcpy(method_function.cobol_name, scope_cobol_name,
        sizeof(method_function.cobol_name));
    if (cblc_parse_parameter_list(cursor, unit, method_function.parameters,
            &method_function.parameter_count, scope_source_name, scope_cobol_name,
            NULL, type) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (method->return_kind != return_kind
        || std::strncmp(method->return_type_name, return_type_name,
            sizeof(method->return_type_name)) != 0
        || !cblc_method_parameters_match(method, method_function.parameters,
            method_function.parameter_count))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statements = NULL;
    statement_count = 0;
    statement_capacity = 0;
    if (cblc_capture_method_body(cursor, unit, type, &method_function, &statements,
            &statement_count, &statement_capacity) != FT_SUCCESS)
    {
        if (method_function.local_destructor_targets)
            cma_free(method_function.local_destructor_targets);
        *cursor = start;
        return (FT_FAILURE);
    }
    if (method->statements)
        cma_free(method->statements);
    method->statements = statements;
    method->statement_count = statement_count;
    method->statement_capacity = statement_capacity;
    method->has_definition = 1;
    if (method_function.local_destructor_targets)
        cma_free(method_function.local_destructor_targets);
    return (FT_SUCCESS);
}

static int cblc_parse_struct_definition(const char **cursor, t_cblc_translation_unit *unit)
{
    t_cblc_struct_type *type;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    int is_class;
    t_cblc_member_visibility current_visibility;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (cblc_match_keyword(cursor, "struct"))
        is_class = 0;
    else if (cblc_match_keyword(cursor, "class"))
        is_class = 1;
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_find_struct_type(unit, identifier)
        || cblc_find_template_struct(unit, identifier))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    if (unit->struct_type_count >= unit->struct_type_capacity)
    {
        if (cblc_translation_unit_ensure_struct_capacity(unit,
                unit->struct_type_capacity == 0 ? 2 : unit->struct_type_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    type = &unit->struct_types[unit->struct_type_count];
    std::memset(type, 0, sizeof(*type));
    ft_strlcpy(type->source_name, identifier, sizeof(type->source_name));
    cblc_identifier_to_cobol(identifier, type->cobol_name, sizeof(type->cobol_name));
    type->is_class = is_class;
    if (is_class)
        current_visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
    else
        current_visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    *cursor += 1;
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '}')
            break ;
        if (type->is_class
            && cblc_parse_member_visibility_label(cursor, &current_visibility) == FT_SUCCESS)
            continue ;
        if (cblc_parse_lifecycle_declaration(cursor, unit, type) == FT_SUCCESS)
            continue ;
        if (cblc_parse_method_definition(cursor, unit, type, current_visibility) == FT_SUCCESS)
            continue ;
        if (cblc_parse_struct_field_declaration(cursor, unit, type, current_visibility)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (**cursor != '}')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    unit->struct_type_count += 1;
    return (FT_SUCCESS);
}

static int cblc_register_template_definition(t_cblc_translation_unit *unit,
    size_t struct_type_index, const char parameter_names[][TRANSPILE_IDENTIFIER_MAX],
    size_t parameter_count)
{
    t_cblc_template_definition *definition;
    const t_cblc_struct_type *type;

    if (!unit || !parameter_names || parameter_count == 0
        || parameter_count > CBLC_TEMPLATE_PARAMETER_MAX
        || struct_type_index >= unit->struct_type_count)
        return (FT_FAILURE);
    if (cblc_translation_unit_ensure_template_definition_capacity(unit,
            unit->template_definition_count + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    type = &unit->struct_types[struct_type_index];
    definition = &unit->template_definitions[unit->template_definition_count];
    std::memset(definition, 0, sizeof(*definition));
    ft_strlcpy(definition->source_name, type->source_name, sizeof(definition->source_name));
    definition->parameter_count = parameter_count;
    size_t parameter_index = 0;
    while (parameter_index < parameter_count)
    {
        ft_strlcpy(definition->parameter_names[parameter_index], parameter_names[parameter_index],
            sizeof(definition->parameter_names[parameter_index]));
        parameter_index += 1;
    }
    ft_strlcpy(definition->parameter_name, parameter_names[0], sizeof(definition->parameter_name));
    definition->struct_type_index = struct_type_index;
    definition->function_index = static_cast<size_t>(-1);
    definition->declaration_kind = 1;
    definition->is_class = type->is_class;
    unit->template_definition_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_template_struct_definition(const char **cursor,
    t_cblc_translation_unit *unit)
{
    const char *start;
    size_t struct_type_index;
    size_t data_scope_base;
    size_t parameter_count;
    size_t active_index;
    char parameter_names[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    int parsed;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_template_header(cursor, parameter_names, &parameter_count) != FT_SUCCESS)
        goto fail;
    cblc_skip_whitespace(cursor);
    if (std::strncmp(*cursor, "struct", std::strlen("struct")) != 0
        && std::strncmp(*cursor, "class", std::strlen("class")) != 0)
        goto fail;
    data_scope_base = unit->data_count;
    g_cblc_active_template_parameter_count = parameter_count;
    active_index = 0;
    while (active_index < parameter_count)
    {
        ft_strlcpy(g_cblc_active_template_parameters[active_index], parameter_names[active_index],
            sizeof(g_cblc_active_template_parameters[active_index]));
        active_index += 1;
    }
    g_cblc_active_template_parameter = g_cblc_active_template_parameters[0];
    parsed = cblc_parse_struct_definition(cursor, unit);
    g_cblc_active_template_parameter = NULL;
    g_cblc_active_template_parameter_count = 0;
    if (parsed != FT_SUCCESS)
        goto fail;
    while (data_scope_base < unit->data_count)
    {
        unit->data_items[data_scope_base].is_active = 0;
        unit->data_items[data_scope_base].is_template_item = 1;
        data_scope_base += 1;
    }
    struct_type_index = unit->struct_type_count - 1;
    unit->struct_types[struct_type_index].is_template = 1;
    unit->struct_types[struct_type_index].template_parameter_count = parameter_count;
    active_index = 0;
    while (active_index < parameter_count)
    {
        ft_strlcpy(unit->struct_types[struct_type_index].template_parameter_names[active_index],
            parameter_names[active_index],
            sizeof(unit->struct_types[struct_type_index].template_parameter_names[active_index]));
        active_index += 1;
    }
    ft_strlcpy(unit->struct_types[struct_type_index].template_parameter_name,
        parameter_names[0], sizeof(unit->struct_types[struct_type_index].template_parameter_name));
    if (cblc_register_template_definition(unit, struct_type_index, parameter_names,
            parameter_count) != FT_SUCCESS)
        return (FT_FAILURE);
    unit->template_definitions[unit->template_definition_count - 1].definition_offset =
        cblc_source_offset(unit, start);
    return (FT_SUCCESS);
fail:
    g_cblc_active_template_parameter = NULL;
    g_cblc_active_template_parameter_count = 0;
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_template_argument_shape(const t_cblc_translation_unit *unit,
    const char *argument, t_cblc_data_kind *kind, size_t *length,
    size_t *pointer_depth, size_t *array_count,
    char *base_name, size_t base_name_size)
{
    char base[TRANSPILE_IDENTIFIER_MAX];
    size_t base_length;
    size_t index;

    if (!unit || !argument || !kind || !length || !pointer_depth || !array_count
        || !base_name || base_name_size == 0)
        return (FT_FAILURE);
    ft_strlcpy(base, argument, sizeof(base));
    base_length = std::strlen(base);
    *array_count = 0;
    while (base_length > 0 && base[base_length - 1] == ']')
    {
        char *open;
        size_t parsed_count;

        open = std::strrchr(base, '[');
        if (!open || open == base)
            return (FT_FAILURE);
        parsed_count = 0;
        index = static_cast<size_t>(open - base) + 1;
        while (base[index] != '\0' && base[index] != ']')
        {
            if (!std::isdigit(static_cast<unsigned char>(base[index])))
                return (FT_FAILURE);
            parsed_count = parsed_count * 10 + static_cast<size_t>(base[index] - '0');
            index += 1;
        }
        if (parsed_count == 0 || base[index] != ']'
            || base[index + 1] != '\0')
            return (FT_FAILURE);
        *open = '\0';
        base_length = std::strlen(base);
        if (*array_count != 0)
            return (FT_FAILURE);
        *array_count = parsed_count;
    }
    *pointer_depth = 0;
    while (base_length > 0 && base[base_length - 1] == '*')
    {
        base[base_length - 1] = '\0';
        base_length -= 1;
        *pointer_depth += 1;
    }
    if (*pointer_depth > 2)
        return (FT_FAILURE);
    *length = 1;
    if (std::strncmp(base, "string(", 7) == 0)
    {
        size_t capacity;
        size_t capacity_index;

        if (base_length < 9 || base[base_length - 1] != ')')
            return (FT_FAILURE);
        capacity = 0;
        capacity_index = 7;
        while (capacity_index + 1 < base_length)
        {
            if (!std::isdigit(static_cast<unsigned char>(base[capacity_index])))
                return (FT_FAILURE);
            capacity = capacity * 10
                + static_cast<size_t>(base[capacity_index] - '0');
            capacity_index += 1;
        }
        if (capacity == 0)
            return (FT_FAILURE);
        *length = capacity;
        base[6] = '\0';
    }
    if (std::strcmp(base, "int") == 0)
        *kind = *pointer_depth == 0 ? CBLC_DATA_KIND_INT
            : (*pointer_depth == 1 ? CBLC_DATA_KIND_INT_POINTER
                : CBLC_DATA_KIND_INT_POINTER_POINTER);
    else if (std::strcmp(base, "char") == 0)
    {
        if (*pointer_depth > 1)
            return (FT_FAILURE);
        *kind = *pointer_depth == 0 ? CBLC_DATA_KIND_CHAR : CBLC_DATA_KIND_CHAR_POINTER;
    }
    else if (std::strcmp(base, "string") == 0)
    {
        if (*pointer_depth != 0)
            return (FT_FAILURE);
        *kind = CBLC_DATA_KIND_STRING;
    }
    else if (std::strcmp(base, "void") == 0)
    {
        if (*pointer_depth != 1)
            return (FT_FAILURE);
        *kind = CBLC_DATA_KIND_VOID_POINTER;
    }
    else if (cblc_find_struct_type(unit, base))
    {
        if (*pointer_depth > 1)
            return (FT_FAILURE);
        *kind = *pointer_depth == 0 ? CBLC_DATA_KIND_STRUCT : CBLC_DATA_KIND_STRUCT_POINTER;
    }
    else
        return (FT_FAILURE);
    ft_strlcpy(base_name, base, base_name_size);
    return (FT_SUCCESS);
}

static int cblc_struct_fixed_array_abi_is_trivial(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t depth)
{
    size_t index;

    if (!unit || !type || depth > CBLC_TEMPLATE_PARAMETER_MAX)
        return (0);
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;

        field = &type->fields[index];
        if (field->array_count > 0
            || (cblc_data_kind_is_pointer(field->kind) && !field->is_borrowed))
            return (0);
        if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *nested_type;

            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!nested_type
                || !cblc_struct_fixed_array_abi_is_trivial(unit, nested_type, depth + 1))
                return (0);
        }
        index += 1;
    }
    return (1);
}

static int cblc_template_argument_kind(const t_cblc_translation_unit *unit,
    const char *argument, t_cblc_data_kind *kind, size_t *length)
{
    size_t pointer_depth;
    size_t array_count;
    char base_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !argument || !kind || !length)
        return (FT_FAILURE);
    if (cblc_template_argument_shape(unit, argument, kind, length,
            &pointer_depth, &array_count, base_name, sizeof(base_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_template_argument_ref(t_cblc_translation_unit *unit,
    const char *argument, size_t *out_ref_id)
{
    t_cblc_type_ref reference;
    t_cblc_data_kind kind;
    size_t length;
    size_t pointer_depth;
    size_t array_count;
    char base_name[TRANSPILE_IDENTIFIER_MAX];
    size_t instantiation_index;

    if (!unit || !argument || !out_ref_id
        || cblc_template_argument_shape(unit, argument, &kind, &length,
            &pointer_depth, &array_count, base_name, sizeof(base_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    instantiation_index = 0;
    while (instantiation_index < unit->template_instantiation_count)
    {
        if (std::strcmp(unit->template_instantiations[instantiation_index].concrete_source_name,
                base_name) == 0
            && unit->template_instantiations[instantiation_index].applied_ref_id
                < unit->type_ref_count)
        {
            reference = unit->type_refs[
                unit->template_instantiations[instantiation_index].applied_ref_id];
            reference.length = length;
            reference.pointer_depth = pointer_depth;
            reference.array_count = array_count;
            return (cblc_type_ref_append(unit, &reference, out_ref_id));
        }
        instantiation_index += 1;
    }
    std::memset(&reference, 0, sizeof(reference));
    if (kind == CBLC_DATA_KIND_STRUCT)
    {
        reference.kind = CBLC_TYPE_REF_NAMED;
        ft_strlcpy(reference.name, base_name, sizeof(reference.name));
        reference.layout_fingerprint = cblc_struct_layout_fingerprint(unit,
            cblc_find_struct_type(unit, base_name), 0);
    }
    else
    {
        reference.kind = CBLC_TYPE_REF_BUILTIN;
        reference.builtin_kind = kind;
    }
    reference.pointer_depth = pointer_depth;
    reference.length = length;
    reference.array_count = array_count;
    return (cblc_type_ref_append(unit, &reference, out_ref_id));
}

static size_t cblc_template_definition_offset(const t_cblc_translation_unit *unit,
    const char *template_name, int declaration_kind)
{
    size_t index;

    if (!unit || !template_name)
        return (static_cast<size_t>(-1));
    index = 0;
    while (index < unit->template_definition_count)
    {
        if (unit->template_definitions[index].declaration_kind == declaration_kind
            && std::strcmp(unit->template_definitions[index].source_name, template_name) == 0)
            return (unit->template_definitions[index].definition_offset);
        index += 1;
    }
    return (static_cast<size_t>(-1));
}

static void cblc_record_struct_template_use(t_cblc_translation_unit *unit,
    const char *template_name, size_t argument_ref_id, size_t use_offset)
{
    size_t index;

    if (!unit || !template_name)
        return ;
    index = 0;
    while (index < unit->template_instantiation_count)
    {
        if (std::strcmp(unit->template_instantiations[index].template_source_name,
                template_name) == 0
            && unit->template_instantiations[index].applied_ref_id == argument_ref_id
            && unit->template_instantiations[index].first_use_offset
                == static_cast<size_t>(-1))
        {
            unit->template_instantiations[index].first_use_offset = use_offset;
            return ;
        }
        index += 1;
    }
}

static void cblc_record_function_template_use(t_cblc_translation_unit *unit,
    const char *template_name, size_t argument_ref_id, size_t use_offset)
{
    size_t index;

    if (!unit || !template_name)
        return ;
    index = 0;
    while (index < unit->function_instantiation_count)
    {
        if (std::strcmp(unit->function_instantiations[index].template_source_name,
                template_name) == 0
            && unit->function_instantiations[index].applied_ref_id == argument_ref_id
            && unit->function_instantiations[index].first_use_offset
                == static_cast<size_t>(-1))
        {
            unit->function_instantiations[index].first_use_offset = use_offset;
            return ;
        }
        index += 1;
    }
}

static int cblc_clone_template_statements(const t_cblc_statement *source,
    size_t source_count, t_cblc_statement **out_statements)
{
    t_cblc_statement *statements;

    if (!out_statements)
        return (FT_FAILURE);
    *out_statements = NULL;
    if (source_count == 0)
        return (FT_SUCCESS);
    if (!source)
        return (FT_FAILURE);
    statements = static_cast<t_cblc_statement *>(cma_calloc(source_count,
            sizeof(*statements)));
    if (!statements)
        return (FT_FAILURE);
    std::memcpy(statements, source, source_count * sizeof(*statements));
    *out_statements = statements;
    return (FT_SUCCESS);
}

static int cblc_substitute_template_parameter_in_parameter_map(
    t_cblc_translation_unit *unit, t_cblc_parameter *parameter,
    const char parameter_names[][TRANSPILE_IDENTIFIER_MAX], size_t parameter_count,
    const char arguments[][TRANSPILE_IDENTIFIER_MAX],
    const t_cblc_data_kind *argument_kinds)
{
    size_t index;

    if (!unit || !parameter || !parameter_names || !arguments || !argument_kinds
        || parameter_count == 0 || parameter_count > CBLC_TEMPLATE_PARAMETER_MAX)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        if (std::strcmp(parameter->type_name, parameter_names[index]) == 0)
        {
            parameter->kind = TRANSPILE_FUNCTION_PARAMETER_UNKNOWN;
            if (cblc_parameter_kind_from_data_kind(argument_kinds[index],
                    &parameter->kind) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(parameter->type_name, arguments[index], sizeof(parameter->type_name));
            {
                t_cblc_data_kind argument_shape_kind;
                size_t argument_length;
                size_t argument_pointer_depth;
                size_t argument_array_count;
                char argument_base_name[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_template_argument_shape(unit, arguments[index],
                        &argument_shape_kind, &argument_length,
                        &argument_pointer_depth, &argument_array_count,
                        argument_base_name, sizeof(argument_base_name)) != FT_SUCCESS)
                    return (FT_FAILURE);
                parameter->length = argument_length;
                parameter->array_count = argument_array_count;
            }
            break ;
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_clone_template_method(const t_cblc_method *source,
    t_cblc_method *destination, t_cblc_translation_unit *unit,
    const char parameter_names[][TRANSPILE_IDENTIFIER_MAX], size_t parameter_count,
    const char arguments[][TRANSPILE_IDENTIFIER_MAX],
    const t_cblc_data_kind *argument_kinds)
{
    size_t index;
    size_t parameter_index;

    if (!source || !destination || !unit || !parameter_names || !arguments
        || !argument_kinds || parameter_count == 0)
        return (FT_FAILURE);
    *destination = *source;
    destination->statements = NULL;
    destination->statement_capacity = source->statement_count;
    if (cblc_clone_template_statements(source->statements, source->statement_count,
            &destination->statements) != FT_SUCCESS)
        return (FT_FAILURE);
    parameter_index = 0;
    while (parameter_index < parameter_count
        && std::strcmp(destination->return_type_name, parameter_names[parameter_index]) != 0)
        parameter_index += 1;
    if (parameter_index < parameter_count)
    {
        t_cblc_function_return_kind argument_return_kind;

        if (cblc_function_return_kind_from_data_kind(argument_kinds[parameter_index],
                &argument_return_kind) != FT_SUCCESS)
        {
            if (destination->statements)
                cma_free(destination->statements);
            destination->statements = NULL;
            return (FT_FAILURE);
        }
        destination->return_kind = argument_return_kind;
        if (argument_return_kind == CBLC_FUNCTION_RETURN_STRUCT
            || argument_return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            ft_strlcpy(destination->return_type_name, arguments[parameter_index],
                sizeof(destination->return_type_name));
        else
            destination->return_type_name[0] = '\0';
    }
    index = 0;
    while (index < destination->parameter_count)
    {
        if (cblc_substitute_template_parameter_in_parameter_map(unit,
                &destination->parameters[index], parameter_names, parameter_count,
                arguments, argument_kinds) != FT_SUCCESS)
        {
            if (destination->statements)
                cma_free(destination->statements);
            destination->statements = NULL;
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_clone_template_constructor(const t_cblc_constructor *source,
    t_cblc_constructor *destination, t_cblc_translation_unit *unit,
    const char parameter_names[][TRANSPILE_IDENTIFIER_MAX], size_t parameter_count,
    const char arguments[][TRANSPILE_IDENTIFIER_MAX],
    const t_cblc_data_kind *argument_kinds)
{
    size_t index;

    if (!source || !destination || !unit || !parameter_names || !arguments
        || !argument_kinds || parameter_count == 0)
        return (FT_FAILURE);
    *destination = *source;
    destination->statements = NULL;
    destination->statement_capacity = source->statement_count;
    if (cblc_clone_template_statements(source->statements, source->statement_count,
            &destination->statements) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < destination->parameter_count)
    {
        if (cblc_substitute_template_parameter_in_parameter_map(unit,
                &destination->parameters[index], parameter_names, parameter_count,
                arguments, argument_kinds) != FT_SUCCESS)
        {
            if (destination->statements)
                cma_free(destination->statements);
            destination->statements = NULL;
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_materialize_template_method_storage(t_cblc_translation_unit *unit,
    t_cblc_method *method, const t_cblc_struct_type *concrete_type)
{
    size_t index;
    char method_cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !method || !concrete_type)
        return (FT_FAILURE);
    if (std::snprintf(method_cobol_name, sizeof(method_cobol_name),
            "%s-%s", concrete_type->cobol_name, method->source_name) < 0)
        return (FT_FAILURE);
    cblc_identifier_to_cobol(method_cobol_name, method->cobol_name,
        sizeof(method->cobol_name));
    index = 0;
    while (index < method->parameter_count)
    {
        char old_cobol[TRANSPILE_IDENTIFIER_MAX];
        char actual_source[TRANSPILE_IDENTIFIER_MAX];
        char actual_cobol[TRANSPILE_IDENTIFIER_MAX];
        char normalized_cobol[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_kind parameter_kind;

        ft_strlcpy(old_cobol, method->parameters[index].cobol_name,
            sizeof(old_cobol));
        if (std::snprintf(actual_source, sizeof(actual_source), "%s__%s__%s",
                concrete_type->source_name, method->source_name,
                method->parameters[index].source_name) < 0
            || std::snprintf(actual_cobol, sizeof(actual_cobol), "%s-%s-%s",
                concrete_type->cobol_name, method->source_name,
                method->parameters[index].source_name) < 0)
            return (FT_FAILURE);
        cblc_identifier_to_cobol(actual_cobol, normalized_cobol,
            sizeof(normalized_cobol));
        ft_strlcpy(actual_cobol, normalized_cobol, sizeof(actual_cobol));
        if (cblc_data_kind_from_parameter_kind(method->parameters[index].kind,
                &parameter_kind) != FT_SUCCESS
            || cblc_add_named_data_item(unit, actual_source, actual_cobol,
                parameter_kind, method->parameters[index].length,
                method->parameters[index].array_count,
                parameter_kind == CBLC_DATA_KIND_STRUCT
                    ? method->parameters[index].type_name : NULL,
                method->source_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (parameter_kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *parameter_type;

            parameter_type = cblc_find_struct_type(unit,
                method->parameters[index].type_name);
            if (!parameter_type
                || cblc_add_struct_instance_field_items(unit, parameter_type,
                    actual_source, actual_cobol, method->source_name, 0, 0)
                    != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (method->statements)
        {
            size_t statement_index;

            statement_index = 0;
            while (statement_index < method->statement_count)
            {
                cblc_replace_template_text(method->statements[statement_index].target,
                    sizeof(method->statements[statement_index].target), old_cobol,
                    actual_cobol);
                cblc_replace_template_text(method->statements[statement_index].source,
                    sizeof(method->statements[statement_index].source), old_cobol,
                    actual_cobol);
                cblc_replace_template_text(method->statements[statement_index].call_arguments,
                    sizeof(method->statements[statement_index].call_arguments), old_cobol,
                    actual_cobol);
                statement_index += 1;
            }
        }
        ft_strlcpy(method->parameters[index].actual_source_name, actual_source,
            sizeof(method->parameters[index].actual_source_name));
        ft_strlcpy(method->parameters[index].cobol_name, actual_cobol,
            sizeof(method->parameters[index].cobol_name));
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_materialize_template_constructor_storage(
    t_cblc_translation_unit *unit, t_cblc_constructor *constructor,
    const t_cblc_struct_type *concrete_type)
{
    size_t index;

    if (!unit || !constructor || !concrete_type)
        return (FT_FAILURE);
    index = 0;
    while (index < constructor->parameter_count)
    {
        char old_cobol[TRANSPILE_IDENTIFIER_MAX];
        char actual_source[TRANSPILE_IDENTIFIER_MAX];
        char actual_cobol[TRANSPILE_IDENTIFIER_MAX];
        char normalized_cobol[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_kind parameter_kind;

        ft_strlcpy(old_cobol, constructor->parameters[index].cobol_name,
            sizeof(old_cobol));
        if (std::snprintf(actual_source, sizeof(actual_source), "%s__ctor__%s",
                concrete_type->source_name,
                constructor->parameters[index].source_name) < 0
            || std::snprintf(actual_cobol, sizeof(actual_cobol), "%s-CTOR-%s",
                concrete_type->cobol_name,
                constructor->parameters[index].source_name) < 0)
            return (FT_FAILURE);
        cblc_identifier_to_cobol(actual_cobol, normalized_cobol,
            sizeof(normalized_cobol));
        ft_strlcpy(actual_cobol, normalized_cobol, sizeof(actual_cobol));
        if (cblc_data_kind_from_parameter_kind(constructor->parameters[index].kind,
                &parameter_kind) != FT_SUCCESS
            || cblc_add_named_data_item(unit, actual_source, actual_cobol,
                parameter_kind, constructor->parameters[index].length,
                constructor->parameters[index].array_count,
                parameter_kind == CBLC_DATA_KIND_STRUCT
                    ? constructor->parameters[index].type_name : NULL,
                concrete_type->source_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (parameter_kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *parameter_type;

            parameter_type = cblc_find_struct_type(unit,
                constructor->parameters[index].type_name);
            if (!parameter_type
                || cblc_add_struct_instance_field_items(unit, parameter_type,
                    actual_source, actual_cobol, concrete_type->source_name, 0, 0)
                    != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (constructor->statements)
        {
            size_t statement_index;

            statement_index = 0;
            while (statement_index < constructor->statement_count)
            {
                cblc_replace_template_text(constructor->statements[statement_index].target,
                    sizeof(constructor->statements[statement_index].target), old_cobol,
                    actual_cobol);
                cblc_replace_template_text(constructor->statements[statement_index].source,
                    sizeof(constructor->statements[statement_index].source), old_cobol,
                    actual_cobol);
                cblc_replace_template_text(constructor->statements[statement_index].call_arguments,
                    sizeof(constructor->statements[statement_index].call_arguments), old_cobol,
                    actual_cobol);
                statement_index += 1;
            }
        }
        ft_strlcpy(constructor->parameters[index].actual_source_name, actual_source,
            sizeof(constructor->parameters[index].actual_source_name));
        ft_strlcpy(constructor->parameters[index].cobol_name, actual_cobol,
            sizeof(constructor->parameters[index].cobol_name));
        index += 1;
    }
    return (FT_SUCCESS);
}

struct s_cblc_template_depth_guard
{
    t_cblc_translation_unit *unit;
    int entered;

    explicit s_cblc_template_depth_guard(t_cblc_translation_unit *value)
        : unit(value), entered(0)
    {
        if (!unit)
            return ;
        if (unit->template_max_instantiation_depth > 0
            && unit->template_instantiation_depth
                >= unit->template_max_instantiation_depth)
        {
            cblc_record_template_parse_error(unit,
                TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED,
                "template instantiation depth exceeded");
            return ;
        }
        unit->template_instantiation_depth += 1;
        entered = 1;
    }

    ~s_cblc_template_depth_guard()
    {
        if (entered && unit && unit->template_instantiation_depth > 0)
            unit->template_instantiation_depth -= 1;
    }
};

static const t_cblc_struct_type *cblc_instantiate_template_struct_arguments(
    t_cblc_translation_unit *unit, const char *template_name,
    const char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX],
    size_t argument_count)
{
    s_cblc_template_depth_guard depth_guard(unit);
    const t_cblc_struct_type *template_type;
    t_cblc_struct_type *type;
    t_cblc_data_kind argument_kinds[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t argument_lengths[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t argument_pointer_depths[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t argument_array_counts[CBLC_TEMPLATE_PARAMETER_MAX];
    char argument_base_names[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    size_t argument_ref_ids[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t expected_parameter_count;
    size_t applied_ref_id;
    size_t index;
    size_t parameter_index;
    char concrete_source_name[TRANSPILE_IDENTIFIER_MAX];
    char concrete_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char argument_key[TRANSPILE_IDENTIFIER_MAX];

    template_type = cblc_find_template_struct(unit, template_name);
    expected_parameter_count = template_type ? template_type->template_parameter_count : 0;
    if (expected_parameter_count == 0)
        expected_parameter_count = 1;
    if (!depth_guard.entered || !template_type || !arguments || argument_count == 0
        || argument_count != expected_parameter_count
        || argument_count > CBLC_TEMPLATE_PARAMETER_MAX)
        return (NULL);
    parameter_index = 0;
    while (parameter_index < argument_count)
    {
        if (arguments[parameter_index][0] == '\0'
            || cblc_template_argument_shape(unit, arguments[parameter_index],
                &argument_kinds[parameter_index], &argument_lengths[parameter_index],
                &argument_pointer_depths[parameter_index],
                &argument_array_counts[parameter_index],
                argument_base_names[parameter_index],
                sizeof(argument_base_names[parameter_index]))
                != FT_SUCCESS
            || cblc_template_argument_ref(unit, arguments[parameter_index],
                &argument_ref_ids[parameter_index]) != FT_SUCCESS)
            return (NULL);
        parameter_index += 1;
    }
    if (cblc_template_arguments_to_key(arguments, argument_count,
            argument_key, sizeof(argument_key)) != FT_SUCCESS)
        return (NULL);
    index = 0;
    while (index < unit->template_instantiation_count)
    {
        if (std::strcmp(unit->template_instantiations[index].template_source_name,
                template_name) == 0
            && unit->template_instantiations[index].argument_count == argument_count)
        {
            parameter_index = 0;
            while (parameter_index < argument_count
                && unit->template_instantiations[index].argument_ref_ids[parameter_index]
                    == argument_ref_ids[parameter_index])
                parameter_index += 1;
            if (parameter_index == argument_count)
            return (&unit->struct_types[unit->template_instantiations[index].struct_type_index]);
        }
        index += 1;
    }
    if (unit->template_max_instantiations > 0
        && unit->template_instantiation_count + unit->function_instantiation_count
            >= unit->template_max_instantiations)
    {
        cblc_record_template_instantiation_error(unit,
            TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED,
            "template instantiation limit exceeded", template_name,
            argument_key, cblc_template_definition_offset(unit, template_name, 1),
            static_cast<size_t>(-1));
        return (NULL);
    }
    if (cblc_build_template_instance_names(template_name, argument_key,
            concrete_source_name, sizeof(concrete_source_name), concrete_cobol_name,
            sizeof(concrete_cobol_name)) != FT_SUCCESS)
        return (NULL);
    if (unit->struct_type_count >= unit->struct_type_capacity
        && cblc_translation_unit_ensure_struct_capacity(unit,
            unit->struct_type_capacity == 0 ? 2 : unit->struct_type_capacity * 2) != FT_SUCCESS)
        return (NULL);
    template_type = cblc_find_template_struct(unit, template_name);
    if (!template_type)
        return (NULL);
    type = &unit->struct_types[unit->struct_type_count];
    std::memset(type, 0, sizeof(*type));
    ft_strlcpy(type->source_name, concrete_source_name, sizeof(type->source_name));
    ft_strlcpy(type->cobol_name, concrete_cobol_name, sizeof(type->cobol_name));
    type->is_class = template_type->is_class;
    type->is_template_instantiation = 1;
    ft_strlcpy(type->template_source_name, template_name, sizeof(type->template_source_name));
    ft_strlcpy(type->template_argument_name, argument_key, sizeof(type->template_argument_name));
    index = 0;
    while (index < template_type->field_count)
    {
        const t_cblc_struct_field *field = &template_type->fields[index];
        t_cblc_data_kind field_kind = field->kind;
        size_t field_length = field->length;
        const char *field_struct_name = field->struct_type_name;

        if (field->is_template_parameter)
        {
            parameter_index = 0;
            while (parameter_index < expected_parameter_count
                && std::strcmp(field->template_parameter_name,
                    template_type->template_parameter_names[parameter_index]) != 0)
                parameter_index += 1;
            if (parameter_index >= expected_parameter_count)
                return (NULL);
            field_kind = argument_kinds[parameter_index];
            field_length = argument_lengths[parameter_index];
            field_struct_name = argument_kinds[parameter_index] == CBLC_DATA_KIND_STRUCT
                || argument_kinds[parameter_index] == CBLC_DATA_KIND_STRUCT_POINTER
                ? argument_base_names[parameter_index] : NULL;
            field_length = argument_lengths[parameter_index];
        }
        if (cblc_struct_type_append_field(type, field->source_name, field_length,
                field->array_count > 0 ? field->array_count
                    : (field->is_template_parameter
                        ? argument_array_counts[parameter_index] : 0),
                field_kind, field_struct_name, field->is_const,
                field->visibility) != FT_SUCCESS)
            return (NULL);
        type->fields[type->field_count - 1].is_borrowed = field->is_borrowed;
        index += 1;
    }
    index = 0;
    while (index < template_type->method_count)
    {
        t_cblc_method *method;
        if (type->method_count >= type->method_capacity
            && cblc_struct_type_ensure_method_capacity(type,
                type->method_capacity == 0 ? 2 : type->method_capacity * 2)
                != FT_SUCCESS)
            return (NULL);
        method = &type->methods[type->method_count];
        if (cblc_clone_template_method(&template_type->methods[index], method, unit,
                template_type->template_parameter_names, expected_parameter_count,
                arguments, argument_kinds) != FT_SUCCESS)
            return (NULL);
        if (cblc_materialize_template_method_storage(unit, method, type)
            != FT_SUCCESS)
        {
            if (method->statements)
                cma_free(method->statements);
            method->statements = NULL;
            return (NULL);
        }
        type->method_count += 1;
        index += 1;
    }
    index = 0;
    while (index < template_type->constructor_count)
    {
        t_cblc_constructor *constructor;

        if (type->constructor_count >= type->constructor_capacity
            && cblc_struct_type_ensure_constructor_capacity(type,
                type->constructor_capacity == 0 ? 2 : type->constructor_capacity * 2)
                != FT_SUCCESS)
            return (NULL);
        constructor = &type->constructors[type->constructor_count];
        if (cblc_clone_template_constructor(&template_type->constructors[index],
                constructor, unit, template_type->template_parameter_names,
                expected_parameter_count, arguments, argument_kinds) != FT_SUCCESS)
            return (NULL);
        if (cblc_materialize_template_constructor_storage(unit, constructor, type)
            != FT_SUCCESS)
        {
            if (constructor->statements)
                cma_free(constructor->statements);
            constructor->statements = NULL;
            return (NULL);
        }
        type->constructor_count += 1;
        index += 1;
    }
    type->has_default_constructor = template_type->has_default_constructor;
    type->has_destructor = template_type->has_destructor;
    type->has_destructor_definition = template_type->has_destructor_definition;
    if (template_type->destructor_statement_count > 0)
    {
        if (cblc_clone_template_statements(template_type->destructor_statements,
                template_type->destructor_statement_count,
                &type->destructor_statements) != FT_SUCCESS)
            return (NULL);
        type->destructor_statement_count = template_type->destructor_statement_count;
        type->destructor_statement_capacity = template_type->destructor_statement_count;
    }
    type->layout_fingerprint = cblc_struct_layout_fingerprint(unit, type, 0);
    unit->struct_type_count += 1;
    {
        t_cblc_type_ref applied_reference;

        std::memset(&applied_reference, 0, sizeof(applied_reference));
        applied_reference.kind = CBLC_TYPE_REF_APPLIED;
        applied_reference.length = 1;
        ft_strlcpy(applied_reference.name, template_name,
            sizeof(applied_reference.name));
        applied_reference.argument_count = argument_count;
        parameter_index = 0;
        while (parameter_index < argument_count)
        {
            applied_reference.argument_ids[parameter_index] = argument_ref_ids[parameter_index];
            parameter_index += 1;
        }
        if (cblc_type_ref_append(unit, &applied_reference, &applied_ref_id) != FT_SUCCESS)
            return (NULL);
    }
    if (cblc_translation_unit_ensure_template_instantiation_capacity(unit,
            unit->template_instantiation_count + 1) != FT_SUCCESS)
        return (NULL);
    std::memset(&unit->template_instantiations[unit->template_instantiation_count], 0,
        sizeof(unit->template_instantiations[unit->template_instantiation_count]));
    unit->template_instantiations[unit->template_instantiation_count].applied_ref_id =
        static_cast<size_t>(-1);
    ft_strlcpy(unit->template_instantiations[unit->template_instantiation_count].template_source_name,
        template_name, sizeof(unit->template_instantiations[unit->template_instantiation_count].template_source_name));
    ft_strlcpy(unit->template_instantiations[unit->template_instantiation_count].argument_name,
        argument_key, sizeof(unit->template_instantiations[unit->template_instantiation_count].argument_name));
    unit->template_instantiations[unit->template_instantiation_count].argument_ref_id = argument_ref_ids[0];
    unit->template_instantiations[unit->template_instantiation_count].argument_count = argument_count;
    parameter_index = 0;
    while (parameter_index < argument_count)
    {
        ft_strlcpy(unit->template_instantiations[unit->template_instantiation_count].argument_names[parameter_index],
            arguments[parameter_index],
            sizeof(unit->template_instantiations[unit->template_instantiation_count].argument_names[parameter_index]));
        unit->template_instantiations[unit->template_instantiation_count].argument_ref_ids[parameter_index] =
            argument_ref_ids[parameter_index];
        parameter_index += 1;
    }
    unit->template_instantiations[unit->template_instantiation_count].definition_offset =
        cblc_template_definition_offset(unit, template_name, 1);
    unit->template_instantiations[unit->template_instantiation_count].first_use_offset =
        static_cast<size_t>(-1);
    ft_strlcpy(unit->template_instantiations[unit->template_instantiation_count].concrete_source_name,
        concrete_source_name, sizeof(unit->template_instantiations[unit->template_instantiation_count].concrete_source_name));
    unit->template_instantiations[unit->template_instantiation_count].struct_type_index =
        unit->struct_type_count - 1;
    unit->template_instantiations[unit->template_instantiation_count].applied_ref_id =
        applied_ref_id;
    unit->template_instantiation_count += 1;
    return (type);
}

static const t_cblc_struct_type *cblc_instantiate_template_struct(
    t_cblc_translation_unit *unit, const char *template_name, const char *argument)
{
    char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];

    if (!argument)
        return (NULL);
    std::memset(arguments, 0, sizeof(arguments));
    ft_strlcpy(arguments[0], argument, sizeof(arguments[0]));
    return (cblc_instantiate_template_struct_arguments(unit, template_name, arguments, 1));
}

static int cblc_parse_template_argument_type(const char **cursor,
    t_cblc_translation_unit *unit, char *argument, size_t argument_size)
{
    char template_name[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_struct_type *type;
    size_t array_count;
    char array_text[64];

    if (!cursor || !*cursor || !unit || !argument || argument_size == 0)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, template_name, sizeof(template_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '<')
    {
        ft_strlcpy(argument, template_name, argument_size);
        if (**cursor == '(')
        {
            size_t capacity;
            char capacity_text[64];

            if (cblc_parse_string_capacity_clause(cursor, &capacity) != FT_SUCCESS
                || std::snprintf(capacity_text, sizeof(capacity_text), "(%zu)", capacity) < 0
                || std::strlen(argument) + std::strlen(capacity_text) >= argument_size)
                return (FT_FAILURE);
            ft_strlcat(argument, capacity_text, argument_size);
            cblc_skip_whitespace(cursor);
        }
    }
    else
    {
        *cursor += 1;
        if (cblc_parse_template_argument_type(cursor, unit, argument, argument_size) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != '>')
            return (FT_FAILURE);
        *cursor += 1;
        type = cblc_instantiate_template_struct(unit, template_name, argument);
        if (!type)
            return (FT_FAILURE);
        ft_strlcpy(argument, type->source_name, argument_size);
    }
    while (**cursor == '*')
    {
        if (std::strlen(argument) + 1 >= argument_size)
            return (FT_FAILURE);
        ft_strlcat(argument, "*", argument_size);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor == '[')
    {
        array_count = 0;
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS
            || std::snprintf(array_text, sizeof(array_text), "[%zu]", array_count) < 0
            || std::strlen(argument) + std::strlen(array_text) >= argument_size)
            return (FT_FAILURE);
        ft_strlcat(argument, array_text, argument_size);
    }
    return (FT_SUCCESS);
}

static int cblc_template_argument_mangle_key(const char *argument,
    char *key, size_t key_size)
{
    const char *cursor;

    if (!argument || !key || key_size == 0)
        return (FT_FAILURE);
    key[0] = '\0';
    cursor = argument;
    while (*cursor != '\0')
    {
        if (*cursor == '*')
            ft_strlcat(key, "_PTR", key_size);
        else if (*cursor == '[')
            ft_strlcat(key, "_ARR", key_size);
        else if (*cursor == '(')
            ft_strlcat(key, "_CAP", key_size);
        else if (*cursor == ')' || *cursor == ']')
        {
            cursor += 1;
            continue ;
        }
        else if (*cursor != ']')
        {
            char character[2];

            character[0] = *cursor;
            character[1] = '\0';
            ft_strlcat(key, character, key_size);
        }
        if (std::strlen(key) >= key_size - 1)
            return (FT_FAILURE);
        cursor += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_template_arguments_to_key(
    const char arguments[][TRANSPILE_IDENTIFIER_MAX], size_t argument_count,
    char *key, size_t key_size)
{
    size_t index;
    char component[TRANSPILE_IDENTIFIER_MAX];

    if (!arguments || argument_count == 0 || !key || key_size == 0)
        return (FT_FAILURE);
    key[0] = '\0';
    index = 0;
    while (index < argument_count)
    {
        if (index > 0)
            ft_strlcat(key, "__", key_size);
        if (cblc_template_argument_mangle_key(arguments[index], component,
                sizeof(component)) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_strlcat(key, component, key_size);
        if (std::strlen(key) >= key_size - 1)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_template_argument_list(const char **cursor,
    t_cblc_translation_unit *unit,
    char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX],
    size_t *out_count)
{
    size_t count;

    if (!cursor || !*cursor || !unit || !arguments || !out_count)
        return (FT_FAILURE);
    count = 0;
    while (1)
    {
        if (count >= CBLC_TEMPLATE_PARAMETER_MAX
            || cblc_parse_template_argument_type(cursor, unit, arguments[count],
                TRANSPILE_IDENTIFIER_MAX) != FT_SUCCESS)
            return (FT_FAILURE);
        count += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor == '>')
            break ;
        if (**cursor != ',')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    *out_count = count;
    return (FT_SUCCESS);
}

static int cblc_add_struct_instance_field_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *instance_source_name, const char *instance_cobol_name,
    const char *owner_function_name, int is_function_local, int is_alias)
{
    size_t index;

    if (!unit || !type || !instance_source_name || !instance_cobol_name)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char field_source_name[TRANSPILE_IDENTIFIER_MAX];
        char field_cobol_name[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_item *item;

        cblc_build_field_source_name(instance_source_name, type->fields[index].source_name,
            field_source_name, sizeof(field_source_name));
        if (std::snprintf(field_cobol_name, sizeof(field_cobol_name), "%s-%s",
                instance_cobol_name, type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (unit->data_count >= unit->data_capacity)
        {
            if (cblc_translation_unit_ensure_data_capacity(unit,
                    unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        item = &unit->data_items[unit->data_count];
        std::memset(item, 0, sizeof(*item));
        ft_strlcpy(item->source_name, field_source_name, sizeof(item->source_name));
        ft_strlcpy(item->cobol_name, field_cobol_name, sizeof(item->cobol_name));
        if (owner_function_name)
            ft_strlcpy(item->owner_function_name, owner_function_name, sizeof(item->owner_function_name));
        if (type->fields[index].declared_type_name[0] != '\0')
            ft_strlcpy(item->declared_type_name, type->fields[index].declared_type_name,
                sizeof(item->declared_type_name));
        item->length = type->fields[index].length;
        item->initial_capacity = type->fields[index].length;
        item->array_count = type->fields[index].array_count;
    item->kind = type->fields[index].kind;
    item->is_const = type->fields[index].is_const;
    item->is_function_local = is_function_local;
    item->is_alias = is_alias;
    item->is_active = 1;
    item->scope_id = unit->active_scope_id;
    item->scope_depth = unit->scopes && unit->active_scope_id < unit->scope_count
        ? unit->scopes[unit->active_scope_id].depth : 0;
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            ft_strlcpy(item->struct_type_name, type->fields[index].struct_type_name,
                sizeof(item->struct_type_name));
            unit->data_count += 1;
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type && (type->is_template || g_cblc_active_template_parameter)
                && type->fields[index].is_template_parameter)
            {
                index += 1;
                continue ;
            }
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_add_struct_instance_field_items(unit, field_type, item->source_name,
                    item->cobol_name, owner_function_name, is_function_local, is_alias) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        unit->data_count += 1;
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit)
{
    const char *start;
    const t_cblc_struct_type *type;
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t constructor_argument_count;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char instance_identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;
    size_t array_count;

    if (!cursor || !*cursor || !unit)
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
        char argument_identifiers[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
        size_t argument_count;

        *cursor += 1;
        if (cblc_parse_template_argument_list(cursor, unit, argument_identifiers,
                &argument_count) != FT_SUCCESS)
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
            argument_identifiers, argument_count);
        if (!type)
        {
            cblc_record_template_parse_error(unit,
                TRANSPILE_ERROR_TEMPLATE_UNKNOWN_ARGUMENT,
                "unknown or unsupported template argument");
            *cursor = start;
            return (FT_FAILURE);
        }
        {
            size_t argument_ref_id;

            if (cblc_template_argument_ref(unit, type->source_name, &argument_ref_id)
                == FT_SUCCESS)
                cblc_record_struct_template_use(unit, type_identifier, argument_ref_id,
                    cblc_source_offset(unit, start));
        }
        ft_strlcpy(type_identifier, type->source_name, sizeof(type_identifier));
    }
    if (!type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, instance_identifier, sizeof(instance_identifier)) != FT_SUCCESS)
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
    if (cblc_find_data_item(unit, instance_identifier))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, instance_identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(instance_identifier, item->cobol_name, sizeof(item->cobol_name));
    ft_strlcpy(item->declared_type_name, type->source_name, sizeof(item->declared_type_name));
    ft_strlcpy(item->struct_type_name, type->source_name, sizeof(item->struct_type_name));
    ft_strlcpy(item->constructor_arguments, constructor_arguments, sizeof(item->constructor_arguments));
    item->constructor_argument_count = constructor_argument_count;
    item->kind = CBLC_DATA_KIND_STRUCT;
    item->array_count = array_count;
    item->is_active = 1;
    item->scope_id = unit->active_scope_id;
    item->scope_depth = unit->scopes && unit->active_scope_id < unit->scope_count
        ? unit->scopes[unit->active_scope_id].depth : 0;
    unit->data_count += 1;
    if (cblc_add_struct_instance_field_items(unit, type, item->source_name, item->cobol_name,
            NULL, 0, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    return (FT_SUCCESS);
}

static t_cblc_data_item *cblc_add_generated_char_item(t_cblc_translation_unit *unit,
    const char *identifier, size_t length)
{
    t_cblc_data_item *item;

    if (!unit || !identifier)
        return (NULL);
    item = cblc_find_data_item(unit, identifier);
    if (item)
        return (item);
    if (length == 0)
        length = 1;
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (NULL);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(identifier, item->cobol_name, sizeof(item->cobol_name));
    item->owner_function_name[0] = '\0';
    item->length = length;
    item->kind = CBLC_DATA_KIND_CHAR;
    item->scope_id = unit->active_scope_id;
    item->scope_depth = unit->scopes && unit->active_scope_id < unit->scope_count
        ? unit->scopes[unit->active_scope_id].depth : 0;
    unit->data_count += 1;
    return (item);
}

static t_cblc_data_item *cblc_add_generated_int_item(t_cblc_translation_unit *unit,
    const char *identifier)
{
    t_cblc_data_item *item;

    if (!unit || !identifier)
        return (NULL);
    item = cblc_find_data_item(unit, identifier);
    if (item)
        return (item);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (NULL);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(identifier, item->cobol_name, sizeof(item->cobol_name));
    item->owner_function_name[0] = '\0';
    item->length = 0;
    item->kind = CBLC_DATA_KIND_INT;
    item->scope_id = unit->active_scope_id;
    item->scope_depth = unit->scopes && unit->active_scope_id < unit->scope_count
        ? unit->scopes[unit->active_scope_id].depth : 0;
    unit->data_count += 1;
    return (item);
}

static t_cblc_data_item *cblc_create_return_item(t_cblc_translation_unit *unit,
    const char *function_identifier, t_cblc_function_return_kind return_kind,
    const char *return_type_name)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !function_identifier)
        return (NULL);
    if (std::snprintf(identifier, sizeof(identifier), "cblc_return_%s",
            function_identifier) < 0)
        return (NULL);
    if (return_kind == CBLC_FUNCTION_RETURN_INT)
        return (cblc_add_generated_int_item(unit, identifier));
    if (return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
    {
        cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
        if (cblc_add_named_data_item(unit, identifier, cobol_name,
                CBLC_DATA_KIND_VOID_POINTER, 0, 0, NULL, NULL, 0, 0, 0) != FT_SUCCESS)
            return (NULL);
        return (&unit->data_items[unit->data_count - 1]);
    }
    if (return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
    {
        cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
        if (cblc_add_named_data_item(unit, identifier, cobol_name,
                CBLC_DATA_KIND_CHAR_POINTER, 0, 0, NULL, NULL, 0, 0, 0) != FT_SUCCESS)
            return (NULL);
        return (&unit->data_items[unit->data_count - 1]);
    }
    if (return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
    {
        cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
        if (cblc_add_named_data_item(unit, identifier, cobol_name,
                CBLC_DATA_KIND_INT_POINTER, 0, 0, NULL, NULL, 0, 0, 0) != FT_SUCCESS)
            return (NULL);
        return (&unit->data_items[unit->data_count - 1]);
    }
    if (return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        if (!return_type_name || return_type_name[0] == '\0')
            return (NULL);
        cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
        if (cblc_add_named_data_item(unit, identifier, cobol_name,
                CBLC_DATA_KIND_STRUCT_POINTER, 0, 0, return_type_name, NULL, 0, 0, 0)
            != FT_SUCCESS)
            return (NULL);
        return (&unit->data_items[unit->data_count - 1]);
    }
    if (return_kind != CBLC_FUNCTION_RETURN_STRUCT || !return_type_name)
        return (NULL);
    cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
    if (cblc_add_named_data_item(unit, identifier, cobol_name, CBLC_DATA_KIND_STRUCT, 0, 0,
            return_type_name, NULL, 0, 0, 0) != FT_SUCCESS)
        return (NULL);
    return (&unit->data_items[unit->data_count - 1]);
}

static int cblc_parse_function_return_type(const char **cursor, const t_cblc_translation_unit *unit,
    t_cblc_function_return_kind *out_kind, char *type_name, size_t type_name_size)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_kind pointer_kind;

    if (!cursor || !*cursor || !unit || !out_kind || !type_name || type_name_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    type_name[0] = '\0';
    if (cblc_parse_pointer_type(cursor, &pointer_kind) == FT_SUCCESS)
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '*')
        {
            *cursor += 1;
            if (pointer_kind == CBLC_DATA_KIND_VOID_POINTER)
            {
                *out_kind = CBLC_FUNCTION_RETURN_VOID_POINTER;
                return (FT_SUCCESS);
            }
            if (pointer_kind == CBLC_DATA_KIND_CHAR_POINTER)
            {
                *out_kind = CBLC_FUNCTION_RETURN_CHAR_POINTER;
                return (FT_SUCCESS);
            }
            if (pointer_kind == CBLC_DATA_KIND_INT_POINTER)
            {
                *out_kind = CBLC_FUNCTION_RETURN_INT_POINTER;
                return (FT_SUCCESS);
            }
        }
    }
    *cursor = start;
    if (cblc_match_keyword(cursor, "void"))
    {
        *out_kind = CBLC_FUNCTION_RETURN_VOID;
        return (FT_SUCCESS);
    }
    if (cblc_match_keyword(cursor, "int"))
    {
        *out_kind = CBLC_FUNCTION_RETURN_INT;
        return (FT_SUCCESS);
    }
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    {
        size_t template_parameter_index;

        if (!cblc_find_struct_type(unit, identifier)
            && cblc_active_template_parameter_index(identifier,
                &template_parameter_index) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '*')
    {
        *cursor += 1;
        *out_kind = CBLC_FUNCTION_RETURN_STRUCT_POINTER;
        ft_strlcpy(type_name, identifier, type_name_size);
        return (FT_SUCCESS);
    }
    *out_kind = CBLC_FUNCTION_RETURN_STRUCT;
    ft_strlcpy(type_name, identifier, type_name_size);
    return (FT_SUCCESS);
}

static int cblc_function_return_matches_target(const t_cblc_function *function,
    const t_cblc_data_item *target_item)
{
    if (!function || !target_item)
        return (0);
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
        return (target_item->kind == CBLC_DATA_KIND_INT);
    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (target_item->kind != CBLC_DATA_KIND_STRUCT)
            return (0);
        if (std::strncmp(target_item->struct_type_name, function->return_type_name,
                sizeof(target_item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER)
        return (cblc_pointer_assignment_is_compatible(target_item->kind,
                CBLC_DATA_KIND_VOID_POINTER));
    if (function->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER)
        return (cblc_pointer_assignment_is_compatible(target_item->kind,
                CBLC_DATA_KIND_CHAR_POINTER));
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER)
        return (cblc_pointer_assignment_is_compatible(target_item->kind,
                CBLC_DATA_KIND_INT_POINTER));
    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        if (target_item->kind != CBLC_DATA_KIND_STRUCT_POINTER)
            return (0);
        if (std::strncmp(target_item->struct_type_name, function->return_type_name,
                sizeof(target_item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    return (0);
}

static int cblc_parameter_kind_from_data_kind(t_cblc_data_kind kind,
    t_transpiler_function_parameter_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (kind == CBLC_DATA_KIND_INT)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_INT;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_STRING)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_STRUCT)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_STRUCT;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_VOID_POINTER)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_CHAR_POINTER)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_INT_POINTER)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_INT_POINTER;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_function_return_kind_from_data_kind(t_cblc_data_kind kind,
    t_cblc_function_return_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (kind == CBLC_DATA_KIND_STRING)
        *out_kind = CBLC_FUNCTION_RETURN_VOID;
    else if (kind == CBLC_DATA_KIND_INT)
        *out_kind = CBLC_FUNCTION_RETURN_INT;
    else if (kind == CBLC_DATA_KIND_STRUCT)
        *out_kind = CBLC_FUNCTION_RETURN_STRUCT;
    else if (kind == CBLC_DATA_KIND_VOID_POINTER)
        *out_kind = CBLC_FUNCTION_RETURN_VOID_POINTER;
    else if (kind == CBLC_DATA_KIND_CHAR_POINTER)
        *out_kind = CBLC_FUNCTION_RETURN_CHAR_POINTER;
    else if (kind == CBLC_DATA_KIND_INT_POINTER)
        *out_kind = CBLC_FUNCTION_RETURN_INT_POINTER;
    else if (kind == CBLC_DATA_KIND_STRUCT_POINTER)
        *out_kind = CBLC_FUNCTION_RETURN_STRUCT_POINTER;
    else
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_parameter_type(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_kind *out_kind, char *type_name, size_t type_name_size,
    const t_cblc_struct_type *self_type)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_struct_type *type;
    const char *start;

    if (!cursor || !*cursor || !out_kind)
        return (FT_FAILURE);
    if (type_name && type_name_size > 0)
        type_name[0] = '\0';
    start = *cursor;
    if (cblc_parse_pointer_type(cursor, out_kind) == FT_SUCCESS)
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '*')
        {
            *cursor += 1;
            return (FT_SUCCESS);
        }
    }
    *cursor = start;
    if (cblc_match_keyword(cursor, "int"))
    {
        *out_kind = CBLC_DATA_KIND_INT;
        return (FT_SUCCESS);
    }
    if (cblc_match_keyword(cursor, "string"))
    {
        *out_kind = CBLC_DATA_KIND_STRING;
        return (FT_SUCCESS);
    }
    if (!unit || !type_name || type_name_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, identifier);
    if (!type)
    {
        {
            size_t template_parameter_index;

            if (cblc_active_template_parameter_index(identifier,
                    &template_parameter_index) != FT_SUCCESS
            && (!self_type
            || std::strncmp(identifier, self_type->source_name, sizeof(identifier)) != 0)
            )
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '*')
    {
        *cursor += 1;
        *out_kind = CBLC_DATA_KIND_STRUCT_POINTER;
        ft_strlcpy(type_name, identifier, type_name_size);
        return (FT_SUCCESS);
    }
    *out_kind = CBLC_DATA_KIND_STRUCT;
    ft_strlcpy(type_name, identifier, type_name_size);
    return (FT_SUCCESS);
}

static int cblc_data_kind_from_parameter_kind(t_transpiler_function_parameter_kind parameter_kind,
    t_cblc_data_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_INT)
    {
        *out_kind = CBLC_DATA_KIND_INT;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
    {
        *out_kind = CBLC_DATA_KIND_STRING;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
    {
        *out_kind = CBLC_DATA_KIND_STRUCT;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_VOID_POINTER;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_CHAR_POINTER;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_INT_POINTER;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
    {
        *out_kind = CBLC_DATA_KIND_STRUCT_POINTER;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_add_parameter_binding(t_cblc_translation_unit *unit,
    t_cblc_parameter *parameters, size_t *parameter_count,
    const char *scope_source_name, const char *scope_cobol_name,
    const char *parameter_name, t_cblc_data_kind kind, const char *type_name,
    const char *owner_function_name, const t_cblc_struct_type *self_type)
{
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_function_parameter_kind parameter_kind;
    const t_cblc_struct_type *struct_type;
    t_cblc_data_item *actual_item;
    t_cblc_data_item *alias_item;

    if (!unit || !parameters || !parameter_count || !scope_source_name
        || !scope_cobol_name || !parameter_name)
        return (FT_FAILURE);
    if (*parameter_count >= TRANSPILE_FUNCTION_PARAMETER_MAX)
        return (FT_FAILURE);
    if (cblc_parameter_kind_from_data_kind(kind, &parameter_kind) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            scope_source_name, parameter_name) < 0)
        return (FT_FAILURE);
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            scope_cobol_name, parameter_name) < 0)
        return (FT_FAILURE);
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name,
        sizeof(actual_cobol_name));
    struct_type = NULL;
    actual_item = cblc_find_data_item(unit, actual_source_name);
    alias_item = cblc_find_data_item(unit, parameter_name);
    if (kind == CBLC_DATA_KIND_STRUCT)
    {
        if (!type_name || type_name[0] == '\0')
            return (FT_FAILURE);
        if (self_type
            && std::strncmp(type_name, self_type->source_name, TRANSPILE_IDENTIFIER_MAX) == 0)
            struct_type = self_type;
        else
            struct_type = cblc_find_struct_type(unit, type_name);
        {
            size_t template_parameter_index;

            if (!struct_type && cblc_active_template_parameter_index(type_name,
                    &template_parameter_index) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (!actual_item)
        {
            if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                    type_name, owner_function_name, 0, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            if (struct_type
                && cblc_add_struct_instance_field_items(unit, struct_type, actual_source_name,
                    actual_cobol_name, owner_function_name, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (!alias_item && struct_type
            && cblc_add_local_struct_alias_items(unit, struct_type, parameter_name, actual_cobol_name)
                != FT_SUCCESS)
            return (FT_FAILURE);
        if (!alias_item && !struct_type)
        {
            size_t template_parameter_index;

            if (cblc_active_template_parameter_index(type_name,
                    &template_parameter_index) == FT_SUCCESS
                && cblc_add_named_data_item(unit, parameter_name, actual_cobol_name, kind, 0, 0,
                    type_name, NULL, 0, 1, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else if (kind == CBLC_DATA_KIND_STRUCT_POINTER)
    {
        if (!type_name || type_name[0] == '\0')
            return (FT_FAILURE);
        if (self_type
            && std::strncmp(type_name, self_type->source_name, TRANSPILE_IDENTIFIER_MAX) == 0)
            struct_type = self_type;
        else
            struct_type = cblc_find_struct_type(unit, type_name);
        if (!struct_type)
            return (FT_FAILURE);
        if (!actual_item
            && cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                    type_name, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!alias_item
            && cblc_add_named_data_item(unit, parameter_name, actual_cobol_name, kind, 0, 0,
                    type_name, NULL, 0, 1, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        if (!actual_item
            && cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                    NULL, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!alias_item
            && cblc_add_named_data_item(unit, parameter_name, actual_cobol_name, kind, 0, 0,
                    NULL, NULL, 0, 1, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(parameters[*parameter_count].source_name, parameter_name,
        sizeof(parameters[*parameter_count].source_name));
    ft_strlcpy(parameters[*parameter_count].actual_source_name,
        actual_source_name,
        sizeof(parameters[*parameter_count].actual_source_name));
    ft_strlcpy(parameters[*parameter_count].cobol_name,
        actual_cobol_name,
        sizeof(parameters[*parameter_count].cobol_name));
    if (type_name)
        ft_strlcpy(parameters[*parameter_count].type_name, type_name,
            sizeof(parameters[*parameter_count].type_name));
    parameters[*parameter_count].kind = parameter_kind;
    parameters[*parameter_count].array_count = 0;
    *parameter_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_parameter_list(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_parameter *parameters,
    size_t *parameter_count, const char *scope_source_name,
    const char *scope_cobol_name, const char *owner_function_name,
    const t_cblc_struct_type *self_type)
{
    t_cblc_data_kind kind;
    char parameter_name[TRANSPILE_IDENTIFIER_MAX];
    char parameter_type_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !parameters || !parameter_count
        || !scope_source_name || !scope_cobol_name)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
        return (FT_SUCCESS);
    while (1)
    {
        if (cblc_parse_parameter_type(cursor, unit, &kind, parameter_type_name,
                sizeof(parameter_type_name), self_type) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, parameter_name, sizeof(parameter_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_parameter_binding(unit, parameters, parameter_count,
                scope_source_name, scope_cobol_name, parameter_name, kind,
                parameter_type_name,
                owner_function_name, self_type) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor == ',')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            continue ;
        }
        if (**cursor == ')')
            return (FT_SUCCESS);
        return (FT_FAILURE);
    }
}

static int cblc_parse_function_parameters(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    return (cblc_parse_parameter_list(cursor, unit, function->parameters,
            &function->parameter_count, function->source_name, function->cobol_name,
            function->source_name, NULL));
}

static int cblc_add_parameter_aliases(t_cblc_translation_unit *unit,
    const t_cblc_parameter *parameters, size_t parameter_count,
    const t_cblc_struct_type *self_type)
{
    size_t index;

    if (!unit || !parameters)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        t_cblc_data_kind kind;

        if (cblc_data_kind_from_parameter_kind(parameters[index].kind, &kind) != FT_SUCCESS)
            return (FT_FAILURE);
        if (kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *type;
            int is_dependent_template_parameter;

            if (self_type
                && std::strncmp(parameters[index].type_name, self_type->source_name,
                    sizeof(parameters[index].type_name)) == 0)
                type = self_type;
            else
                type = cblc_find_struct_type(unit, parameters[index].type_name);
            {
                size_t template_parameter_index;

                is_dependent_template_parameter = !type
                    && cblc_active_template_parameter_index(parameters[index].type_name,
                        &template_parameter_index) == FT_SUCCESS;
            }
            if (!type && !is_dependent_template_parameter)
                return (FT_FAILURE);
            if (type)
            {
                if (cblc_add_local_struct_alias_items(unit, type,
                        parameters[index].source_name,
                        parameters[index].cobol_name) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (cblc_add_named_data_item(unit, parameters[index].source_name,
                    parameters[index].cobol_name, kind, 0, 0,
                    parameters[index].type_name, NULL, 0, 1, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_add_named_data_item(unit, parameters[index].source_name,
                    parameters[index].cobol_name, kind, 0, 0, NULL, NULL, 0, 1, 0)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static void cblc_build_constructor_scope_name(const char *base, const char *separator,
    size_t arity, char *buffer, size_t buffer_size)
{
    char arity_suffix[32];

    if (!buffer || buffer_size == 0)
        return ;
    std::snprintf(arity_suffix, sizeof(arity_suffix), "%zu", arity);
    cblc_build_string_component_name(base, separator, buffer, buffer_size);
    ft_strlcat(buffer, arity_suffix, buffer_size);
}

static int cblc_parse_template_header(const char **cursor,
    char parameter_names[][TRANSPILE_IDENTIFIER_MAX], size_t *out_count)
{
    size_t count;

    if (!cursor || !*cursor || !parameter_names || !out_count
        || !cblc_match_keyword(cursor, "template"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '<')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    count = 0;
    while (1)
    {
        size_t index;

        if (count >= CBLC_TEMPLATE_PARAMETER_MAX
            || !cblc_match_keyword(cursor, "typename"))
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, parameter_names[count],
                TRANSPILE_IDENTIFIER_MAX) != FT_SUCCESS)
            return (FT_FAILURE);
        index = 0;
        while (index < count)
        {
            if (std::strcmp(parameter_names[index], parameter_names[count]) == 0)
                return (FT_FAILURE);
            index += 1;
        }
        count += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor == '>')
        {
            *cursor += 1;
            break ;
        }
        if (**cursor != ',')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    *out_count = count;
    return (FT_SUCCESS);
}

static void cblc_record_template_parse_error(t_cblc_translation_unit *unit,
    int code, const char *message)
{
    if (!unit || unit->parse_error_code != 0)
        return ;
    unit->parse_error_code = code;
    if (message)
        ft_strlcpy(unit->parse_error_message, message,
            sizeof(unit->parse_error_message));
}

static void cblc_record_template_instantiation_error(
    t_cblc_translation_unit *unit, int code, const char *message,
    const char *template_name, const char *argument_key,
    size_t definition_offset, size_t use_offset)
{
    if (!unit || unit->parse_error_code != 0)
        return ;
    cblc_record_template_parse_error(unit, code, message);
    if (template_name)
        ft_strlcpy(unit->template_error_name, template_name,
            sizeof(unit->template_error_name));
    if (argument_key)
        ft_strlcpy(unit->template_error_arguments, argument_key,
            sizeof(unit->template_error_arguments));
    unit->template_error_definition_offset = definition_offset;
    unit->template_error_use_offset = use_offset;
}

static size_t cblc_source_offset(const t_cblc_translation_unit *unit,
    const char *position)
{
    if (!unit || !unit->source_text || !position || position < unit->source_text)
        return (static_cast<size_t>(-1));
    return (static_cast<size_t>(position - unit->source_text));
}

static int cblc_register_template_function_definition(t_cblc_translation_unit *unit,
    size_t function_index,
    const char parameter_names[][TRANSPILE_IDENTIFIER_MAX], size_t parameter_count)
{
    t_cblc_template_definition *definition;
    const t_cblc_function *function;

    if (!unit || !parameter_names || parameter_count == 0
        || parameter_count > CBLC_TEMPLATE_PARAMETER_MAX
        || function_index >= unit->function_count)
        return (FT_FAILURE);
    if (cblc_translation_unit_ensure_template_definition_capacity(unit,
            unit->template_definition_count + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    function = &unit->functions[function_index];
    definition = &unit->template_definitions[unit->template_definition_count];
    std::memset(definition, 0, sizeof(*definition));
    ft_strlcpy(definition->source_name, function->source_name, sizeof(definition->source_name));
    definition->parameter_count = parameter_count;
    size_t parameter_index = 0;
    while (parameter_index < parameter_count)
    {
        ft_strlcpy(definition->parameter_names[parameter_index], parameter_names[parameter_index],
            sizeof(definition->parameter_names[parameter_index]));
        parameter_index += 1;
    }
    ft_strlcpy(definition->parameter_name, parameter_names[0], sizeof(definition->parameter_name));
    definition->struct_type_index = static_cast<size_t>(-1);
    definition->function_index = function_index;
    definition->declaration_kind = 2;
    unit->template_definition_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_template_function_definition(const char **cursor,
    t_cblc_translation_unit *unit)
{
    const char *start;
    char parameter_names[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    size_t parameter_count;
    size_t function_index;
    size_t data_scope_base;
    int status;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_template_header(cursor, parameter_names, &parameter_count) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (std::strncmp(*cursor, "struct", 6) == 0
        || std::strncmp(*cursor, "class", 5) == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    data_scope_base = unit->data_count;
    g_cblc_active_template_parameter_count = 1;
    g_cblc_active_template_parameter_count = parameter_count;
    size_t active_parameter_index = 0;
    while (active_parameter_index < parameter_count)
    {
        ft_strlcpy(g_cblc_active_template_parameters[active_parameter_index],
            parameter_names[active_parameter_index],
            sizeof(g_cblc_active_template_parameters[active_parameter_index]));
        active_parameter_index += 1;
    }
    g_cblc_active_template_parameter = g_cblc_active_template_parameters[0];
    status = cblc_parse_function(cursor, unit);
    g_cblc_active_template_parameter = NULL;
    g_cblc_active_template_parameter_count = 0;
    if (status != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    function_index = unit->function_count - 1;
    while (data_scope_base < unit->data_count)
    {
        unit->data_items[data_scope_base].is_active = 0;
        unit->data_items[data_scope_base].is_template_item = 1;
        data_scope_base += 1;
    }
    unit->functions[function_index].is_template = 1;
    unit->functions[function_index].template_parameter_count = parameter_count;
    active_parameter_index = 0;
    while (active_parameter_index < parameter_count)
    {
        ft_strlcpy(unit->functions[function_index].template_parameter_names[active_parameter_index],
            parameter_names[active_parameter_index],
            sizeof(unit->functions[function_index].template_parameter_names[active_parameter_index]));
        active_parameter_index += 1;
    }
    ft_strlcpy(unit->functions[function_index].template_parameter_name,
        parameter_names[0], sizeof(unit->functions[function_index].template_parameter_name));
    if (cblc_register_template_function_definition(unit, function_index,
            parameter_names, parameter_count) != FT_SUCCESS)
        return (FT_FAILURE);
    unit->template_definitions[unit->template_definition_count - 1].definition_offset =
        cblc_source_offset(unit, start);
    return (FT_SUCCESS);
}

static void cblc_replace_template_text(char *text, size_t text_size,
    const char *from, const char *to)
{
    char replaced[TRANSPILE_STATEMENT_TEXT_MAX];
    const char *cursor;
    size_t from_length;

    if (!text || text_size == 0 || !from || !to || from[0] == '\0')
        return ;
    from_length = std::strlen(from);
    replaced[0] = '\0';
    cursor = text;
    while (*cursor != '\0')
    {
        const char *match = std::strstr(cursor, from);
        size_t prefix_length;

        if (!match)
        {
            ft_strlcat(replaced, cursor, sizeof(replaced));
            break ;
        }
        prefix_length = static_cast<size_t>(match - cursor);
        if (prefix_length >= sizeof(replaced) - std::strlen(replaced))
            break ;
        std::strncat(replaced, cursor, prefix_length);
        ft_strlcat(replaced, to, sizeof(replaced));
        cursor = match + from_length;
    }
    ft_strlcpy(text, replaced, text_size);
}

static int cblc_template_concrete_signature(const t_cblc_translation_unit *unit,
    const char *argument, t_cblc_data_kind *out_kind,
    t_cblc_function_return_kind *out_return_kind, char *out_type_name,
    size_t out_type_name_size)
{
    size_t length;
    size_t pointer_depth;
    size_t array_count;
    char base_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !argument || !out_kind || !out_return_kind || !out_type_name
        || out_type_name_size == 0
        || cblc_template_argument_shape(unit, argument, out_kind, &length,
            &pointer_depth, &array_count, base_name, sizeof(base_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_function_return_kind_from_data_kind(*out_kind, out_return_kind)
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (*out_kind == CBLC_DATA_KIND_STRUCT)
        ft_strlcpy(out_type_name, base_name, out_type_name_size);
    else if (*out_kind == CBLC_DATA_KIND_STRUCT_POINTER)
        ft_strlcpy(out_type_name, base_name, out_type_name_size);
    return (FT_SUCCESS);
}

static unsigned long long cblc_template_name_hash(const char *template_name,
    const char *argument)
{
    unsigned long long hash;
    const char *cursor;

    hash = 1469598103934665603ULL;
    cursor = template_name;
    while (cursor && *cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor);
        hash *= 1099511628211ULL;
        cursor += 1;
    }
    hash ^= static_cast<unsigned char>('|');
    hash *= 1099511628211ULL;
    cursor = argument;
    while (cursor && *cursor != '\0')
    {
        hash ^= static_cast<unsigned char>(*cursor);
        hash *= 1099511628211ULL;
        cursor += 1;
    }
    return (hash);
}

static int cblc_build_template_instance_names(const char *template_name,
    const char *argument, char *source_name, size_t source_size,
    char *cobol_name, size_t cobol_size)
{
    int source_length;
    int cobol_length;
    unsigned long long hash;

    if (!template_name || !argument || !source_name || source_size == 0
        || !cobol_name || cobol_size == 0)
        return (FT_FAILURE);
    source_length = std::snprintf(source_name, source_size, "%s__%s",
        template_name, argument);
    cobol_length = std::snprintf(cobol_name, cobol_size, "CBLC-TPL-%s-%s",
        template_name, argument);
    if (source_length >= 0 && static_cast<size_t>(source_length) < source_size
        && cobol_length >= 0 && static_cast<size_t>(cobol_length) < cobol_size)
    {
        cblc_identifier_to_cobol(cobol_name, cobol_name, cobol_size);
        return (FT_SUCCESS);
    }
    hash = cblc_template_name_hash(template_name, argument);
    if (std::snprintf(source_name, source_size, "TPL-%.*s-H%016llX", 40,
            template_name, hash) < 0
        || std::snprintf(cobol_name, cobol_size, "CBLC-TPL-%.*s-H%016llX", 30,
            template_name, hash) < 0)
        return (FT_FAILURE);
    if (std::strlen(source_name) >= source_size || std::strlen(cobol_name) >= cobol_size)
        return (FT_FAILURE);
    cblc_identifier_to_cobol(cobol_name, cobol_name, cobol_size);
    return (FT_SUCCESS);
}

static const t_cblc_function *cblc_instantiate_template_function_arguments(
    t_cblc_translation_unit *unit, const char *template_name,
    const char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX],
    size_t argument_count)
{
    s_cblc_template_depth_guard depth_guard(unit);
    const t_cblc_function *template_function;
    t_cblc_function *function;
    t_cblc_data_kind argument_kinds[CBLC_TEMPLATE_PARAMETER_MAX];
    t_cblc_function_return_kind argument_return_kinds[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t argument_lengths[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t argument_array_counts[CBLC_TEMPLATE_PARAMETER_MAX];
    char argument_type_names[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    size_t expected_parameter_count;
    char concrete_source_name[TRANSPILE_IDENTIFIER_MAX];
    char concrete_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    size_t argument_ref_ids[CBLC_TEMPLATE_PARAMETER_MAX];
    size_t applied_ref_id;
    size_t index;
    size_t parameter_index;
    char argument_key[TRANSPILE_IDENTIFIER_MAX];

    template_function = cblc_find_template_function(unit, template_name);
    expected_parameter_count = template_function ? template_function->template_parameter_count : 0;
    if (expected_parameter_count == 0)
        expected_parameter_count = 1;
    if (!depth_guard.entered || !template_function || !arguments || argument_count == 0
        || argument_count != expected_parameter_count
        || argument_count > CBLC_TEMPLATE_PARAMETER_MAX)
        return (NULL);
    parameter_index = 0;
    while (parameter_index < argument_count)
    {
        t_cblc_data_kind argument_shape_kind;
        size_t argument_shape_length;
        size_t argument_pointer_depth;
        size_t argument_array_count;
        char argument_base_name[TRANSPILE_IDENTIFIER_MAX];

        argument_type_names[parameter_index][0] = '\0';
        if (cblc_template_argument_shape(unit, arguments[parameter_index],
                &argument_shape_kind, &argument_shape_length,
                &argument_pointer_depth, &argument_array_count,
                argument_base_name, sizeof(argument_base_name)) != FT_SUCCESS
            || cblc_template_argument_ref(unit, arguments[parameter_index],
                &argument_ref_ids[parameter_index]) != FT_SUCCESS
            || cblc_template_concrete_signature(unit, arguments[parameter_index],
                &argument_kinds[parameter_index], &argument_return_kinds[parameter_index],
                argument_type_names[parameter_index],
                sizeof(argument_type_names[parameter_index])) != FT_SUCCESS)
            return (NULL);
        argument_array_counts[parameter_index] = argument_array_count;
        argument_lengths[parameter_index] = argument_shape_length;
        if (argument_array_count > 0)
        {
            int array_abi_supported;

            array_abi_supported = argument_shape_kind == CBLC_DATA_KIND_INT
                || argument_shape_kind == CBLC_DATA_KIND_STRUCT
                || (argument_shape_kind == CBLC_DATA_KIND_STRING
                    && std::strncmp(arguments[parameter_index], "string(", 7) == 0);
            if (!array_abi_supported)
                return (NULL);
        }
        if (argument_array_count > 0 && argument_shape_kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *argument_type;

            argument_type = cblc_find_struct_type(unit, argument_base_name);
            if (!argument_type
                || !cblc_struct_fixed_array_abi_is_trivial(unit, argument_type, 0))
                return (NULL);
        }
        parameter_index += 1;
    }
    if (cblc_template_arguments_to_key(arguments, argument_count,
            argument_key, sizeof(argument_key)) != FT_SUCCESS)
        return (NULL);
    index = 0;
    while (index < unit->function_instantiation_count)
    {
        if (std::strcmp(unit->function_instantiations[index].template_source_name,
                template_name) == 0
            && unit->function_instantiations[index].argument_count == argument_count)
        {
            parameter_index = 0;
            while (parameter_index < argument_count
                && unit->function_instantiations[index].argument_ref_ids[parameter_index]
                    == argument_ref_ids[parameter_index])
                parameter_index += 1;
            if (parameter_index == argument_count)
                return (&unit->functions[unit->function_instantiations[index].function_index]);
        }
        index += 1;
    }
    if (unit->template_max_instantiations > 0
        && unit->template_instantiation_count + unit->function_instantiation_count
            >= unit->template_max_instantiations)
    {
        cblc_record_template_instantiation_error(unit,
            TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED,
            "template instantiation limit exceeded", template_name,
            argument_key, cblc_template_definition_offset(unit, template_name, 2),
            static_cast<size_t>(-1));
        return (NULL);
    }
    parameter_index = 0;
    while (parameter_index < argument_count)
    {
        if (argument_kinds[parameter_index] == CBLC_DATA_KIND_STRING
            && template_function->return_kind == CBLC_FUNCTION_RETURN_STRUCT
            && std::strcmp(template_function->return_type_name,
                template_function->template_parameter_names[parameter_index]) == 0)
            return (NULL);
        parameter_index += 1;
    }
    if (cblc_build_template_instance_names(template_name, argument_key,
            concrete_source_name, sizeof(concrete_source_name), concrete_cobol_name,
            sizeof(concrete_cobol_name)) != FT_SUCCESS)
        return (NULL);
    if (unit->function_count >= unit->function_capacity
        && cblc_translation_unit_ensure_function_capacity(unit,
            unit->function_capacity == 0 ? 2 : unit->function_capacity * 2) != FT_SUCCESS)
        return (NULL);
    template_function = cblc_find_template_function(unit, template_name);
    function = &unit->functions[unit->function_count];
    *function = *template_function;
    function->statements = NULL;
    function->local_destructor_targets = NULL;
    function->is_template = 0;
    function->is_template_instantiation = 1;
    ft_strlcpy(function->template_source_name, template_name,
        sizeof(function->template_source_name));
    ft_strlcpy(function->template_argument_name, argument_key,
        sizeof(function->template_argument_name));
    ft_strlcpy(function->source_name, concrete_source_name, sizeof(function->source_name));
    ft_strlcpy(function->cobol_name, concrete_cobol_name, sizeof(function->cobol_name));
    function->template_parameter_count = 0;
    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT
        || function->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
    {
        parameter_index = 0;
        while (parameter_index < argument_count
            && std::strcmp(function->return_type_name,
                template_function->template_parameter_names[parameter_index]) != 0)
            parameter_index += 1;
        if (parameter_index < argument_count)
            ft_strlcpy(function->return_type_name, argument_type_names[parameter_index],
                sizeof(function->return_type_name));
    }
    parameter_index = 0;
    while (parameter_index < argument_count
        && std::strcmp(template_function->return_type_name,
            template_function->template_parameter_names[parameter_index]) != 0)
        parameter_index += 1;
    if (parameter_index < argument_count)
        function->return_kind = argument_return_kinds[parameter_index];
    {
        t_cblc_data_item *return_item;

        return_item = NULL;
        if (function->return_kind != CBLC_FUNCTION_RETURN_VOID)
            return_item = cblc_create_return_item(unit, function->source_name,
                function->return_kind, function->return_type_name);
        if (function->return_kind != CBLC_FUNCTION_RETURN_VOID && !return_item)
            return (NULL);
        if (return_item)
        {
            function->return_item_index = static_cast<int>(return_item - unit->data_items);
            ft_strlcpy(function->return_cobol_name, return_item->cobol_name,
                sizeof(function->return_cobol_name));
            ft_strlcpy(function->return_source_name, return_item->source_name,
                sizeof(function->return_source_name));
        }
    }
    index = 0;
    while (index < function->parameter_count)
    {
        char old_actual[TRANSPILE_IDENTIFIER_MAX];
        char new_actual[TRANSPILE_IDENTIFIER_MAX];
        char actual_cobol[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_kind parameter_kind;

        ft_strlcpy(old_actual, function->parameters[index].cobol_name, sizeof(old_actual));
        parameter_index = 0;
        while (parameter_index < argument_count
            && std::strcmp(function->parameters[index].type_name,
                template_function->template_parameter_names[parameter_index]) != 0)
            parameter_index += 1;
        if (parameter_index < argument_count)
        {
            parameter_kind = argument_kinds[parameter_index];
            if (parameter_kind == CBLC_DATA_KIND_STRUCT
                || parameter_kind == CBLC_DATA_KIND_STRUCT_POINTER)
                ft_strlcpy(function->parameters[index].type_name,
                    argument_type_names[parameter_index],
                    sizeof(function->parameters[index].type_name));
            else
                ft_strlcpy(function->parameters[index].type_name, arguments[parameter_index],
                    sizeof(function->parameters[index].type_name));
            if (cblc_parameter_kind_from_data_kind(parameter_kind,
                    &function->parameters[index].kind) != FT_SUCCESS)
                return (NULL);
            function->parameters[index].array_count =
                argument_array_counts[parameter_index];
            function->parameters[index].length = argument_lengths[parameter_index];
        }
        else if (cblc_data_kind_from_parameter_kind(function->parameters[index].kind,
                &parameter_kind) != FT_SUCCESS)
            return (NULL);
        if (parameter_index >= argument_count)
        {
            function->parameters[index].length = 0;
            function->parameters[index].array_count = 0;
        }
        if (std::snprintf(new_actual, sizeof(new_actual), "%s__%s",
                function->source_name, function->parameters[index].source_name) < 0
            || std::snprintf(actual_cobol, sizeof(actual_cobol), "%s-%s",
                function->cobol_name, function->parameters[index].source_name) < 0)
            return (NULL);
        cblc_identifier_to_cobol(actual_cobol, function->parameters[index].cobol_name,
            sizeof(function->parameters[index].cobol_name));
        ft_strlcpy(function->parameters[index].actual_source_name, new_actual,
            sizeof(function->parameters[index].actual_source_name));
        {
            const char *parameter_struct_name = NULL;

            if (parameter_index < argument_count
                && (parameter_kind == CBLC_DATA_KIND_STRUCT
                    || parameter_kind == CBLC_DATA_KIND_STRUCT_POINTER))
                parameter_struct_name = argument_type_names[parameter_index];
            if (cblc_add_named_data_item(unit, new_actual,
                    function->parameters[index].cobol_name, parameter_kind,
                    function->parameters[index].length,
                    function->parameters[index].array_count,
                    parameter_struct_name, function->source_name, 0, 0, 0) != FT_SUCCESS)
                return (NULL);
        }
        if (parameter_index < argument_count
            && parameter_kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *parameter_type = cblc_find_struct_type(unit,
                argument_type_names[parameter_index]);

            if (!parameter_type || cblc_add_struct_instance_field_items(unit, parameter_type,
                    new_actual, function->parameters[index].cobol_name, function->source_name,
                    0, 0) != FT_SUCCESS)
                return (NULL);
        }
        if (function->statements)
        {
            size_t statement_index = 0;

            while (statement_index < function->statement_count)
            {
                cblc_replace_template_text(function->statements[statement_index].target,
                    sizeof(function->statements[statement_index].target), old_actual,
                    function->parameters[index].cobol_name);
                cblc_replace_template_text(function->statements[statement_index].source,
                    sizeof(function->statements[statement_index].source), old_actual,
                    function->parameters[index].cobol_name);
                statement_index += 1;
            }
        }
        index += 1;
    }
    if (function->statement_count > 0)
    {
        function->statements = static_cast<t_cblc_statement *>(cma_calloc(
            function->statement_count, sizeof(*function->statements)));
        if (!function->statements)
            return (NULL);
        std::memcpy(function->statements, template_function->statements,
            function->statement_count * sizeof(*function->statements));
        index = 0;
        while (index < function->statement_count)
        {
            parameter_index = 0;

            cblc_replace_template_text(function->statements[index].target,
                sizeof(function->statements[index].target), template_function->return_cobol_name,
                function->return_cobol_name);
            cblc_replace_template_text(function->statements[index].source,
                sizeof(function->statements[index].source), template_function->return_cobol_name,
                function->return_cobol_name);
            while (parameter_index < function->parameter_count)
            {
                cblc_replace_template_text(function->statements[index].target,
                    sizeof(function->statements[index].target),
                    template_function->parameters[parameter_index].cobol_name,
                    function->parameters[parameter_index].cobol_name);
                cblc_replace_template_text(function->statements[index].source,
                    sizeof(function->statements[index].source),
                    template_function->parameters[parameter_index].cobol_name,
                    function->parameters[parameter_index].cobol_name);
                parameter_index += 1;
            }
            index += 1;
        }
    }
    function->statement_capacity = function->statement_count;
    unit->function_count += 1;
    if (cblc_translation_unit_ensure_function_instantiation_capacity(unit,
            unit->function_instantiation_count + 1) != FT_SUCCESS)
        return (NULL);
    std::memset(&unit->function_instantiations[unit->function_instantiation_count], 0,
        sizeof(unit->function_instantiations[unit->function_instantiation_count]));
    ft_strlcpy(unit->function_instantiations[unit->function_instantiation_count].template_source_name,
        template_name, sizeof(unit->function_instantiations[unit->function_instantiation_count].template_source_name));
    ft_strlcpy(unit->function_instantiations[unit->function_instantiation_count].argument_name,
        argument_key, sizeof(unit->function_instantiations[unit->function_instantiation_count].argument_name));
    unit->function_instantiations[unit->function_instantiation_count].argument_ref_id = argument_ref_ids[0];
    unit->function_instantiations[unit->function_instantiation_count].argument_count = argument_count;
    parameter_index = 0;
    while (parameter_index < argument_count)
    {
        ft_strlcpy(unit->function_instantiations[unit->function_instantiation_count].argument_names[parameter_index],
            arguments[parameter_index],
            sizeof(unit->function_instantiations[unit->function_instantiation_count].argument_names[parameter_index]));
        unit->function_instantiations[unit->function_instantiation_count].argument_ref_ids[parameter_index] =
            argument_ref_ids[parameter_index];
        parameter_index += 1;
    }
    {
        t_cblc_type_ref applied_reference;

        std::memset(&applied_reference, 0, sizeof(applied_reference));
        applied_reference.kind = CBLC_TYPE_REF_APPLIED;
        ft_strlcpy(applied_reference.name, template_name, sizeof(applied_reference.name));
        applied_reference.argument_count = argument_count;
        parameter_index = 0;
        while (parameter_index < argument_count)
        {
            applied_reference.argument_ids[parameter_index] = argument_ref_ids[parameter_index];
            parameter_index += 1;
        }
        if (cblc_type_ref_append(unit, &applied_reference,
                &applied_ref_id) != FT_SUCCESS)
            return (NULL);
    }
    unit->function_instantiations[unit->function_instantiation_count].applied_ref_id = applied_ref_id;
    unit->function_instantiations[unit->function_instantiation_count].definition_offset =
        cblc_template_definition_offset(unit, template_name, 2);
    unit->function_instantiations[unit->function_instantiation_count].first_use_offset =
        static_cast<size_t>(-1);
    ft_strlcpy(unit->function_instantiations[unit->function_instantiation_count].concrete_source_name,
        concrete_source_name, sizeof(unit->function_instantiations[unit->function_instantiation_count].concrete_source_name));
    unit->function_instantiations[unit->function_instantiation_count].function_index =
        unit->function_count - 1;
    unit->function_instantiation_count += 1;
    return (function);
}

static const t_cblc_function *cblc_instantiate_template_function(
    t_cblc_translation_unit *unit, const char *template_name, const char *argument)
{
    char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];

    if (!argument)
        return (NULL);
    std::memset(arguments, 0, sizeof(arguments));
    ft_strlcpy(arguments[0], argument, sizeof(arguments[0]));
    return (cblc_instantiate_template_function_arguments(unit, template_name,
        arguments, 1));
}

static int cblc_parse_explicit_function_application(const char **cursor,
    t_cblc_translation_unit *unit, char *identifier, size_t identifier_size)
{
    const char *application_start;
    char arguments[CBLC_TEMPLATE_PARAMETER_MAX][TRANSPILE_IDENTIFIER_MAX];
    size_t argument_count;
    const t_cblc_function *function;

    if (!cursor || !*cursor || !unit || !identifier || identifier_size == 0)
        return (FT_FAILURE);
    application_start = *cursor;
    cblc_skip_whitespace(cursor);
    if (**cursor != '<')
        return (FT_SUCCESS);
    *cursor += 1;
    if (cblc_parse_template_argument_list(cursor, unit, arguments, &argument_count) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '>')
        return (FT_FAILURE);
    *cursor += 1;
    function = cblc_instantiate_template_function_arguments(unit, identifier,
        arguments, argument_count);
    if (!function)
    {
        char argument_key[TRANSPILE_IDENTIFIER_MAX];

        if (cblc_template_arguments_to_key(arguments, argument_count,
                argument_key, sizeof(argument_key)) != FT_SUCCESS)
            argument_key[0] = '\0';
        cblc_record_template_instantiation_error(unit,
            TRANSPILE_ERROR_TEMPLATE_INSTANTIATION_FAILED,
            "template function instantiation failed", identifier,
            argument_key, cblc_template_definition_offset(unit, identifier, 2),
            cblc_source_offset(unit, application_start));
        return (FT_FAILURE);
    }
    {
        size_t argument_ref_id;
        size_t function_instantiation_index;

        argument_ref_id = static_cast<size_t>(-1);
        function_instantiation_index = 0;
        while (function_instantiation_index < unit->function_instantiation_count)
        {
            if (std::strcmp(unit->function_instantiations[function_instantiation_index].concrete_source_name,
                    function->source_name) == 0)
            {
                argument_ref_id = unit->function_instantiations[function_instantiation_index].applied_ref_id;
                break ;
            }
            function_instantiation_index += 1;
        }
        if (argument_ref_id < unit->type_ref_count)
            cblc_record_function_template_use(unit, identifier, argument_ref_id,
                cblc_source_offset(unit, application_start));
    }
    ft_strlcpy(identifier, function->source_name, identifier_size);
    return (FT_SUCCESS);
}

static int cblc_template_argument_from_call_expression(
    const t_cblc_translation_unit *unit, const char *expression,
    char *argument, size_t argument_size)
{
    const t_cblc_data_item *item;
    const char *cursor;
    size_t index;

    if (!unit || !expression || !argument || argument_size == 0)
        return (FT_FAILURE);
    argument[0] = '\0';
    cursor = expression;
    while (*cursor != '\0' && std::isspace(static_cast<unsigned char>(*cursor)))
        cursor += 1;
    if (*cursor == '\0')
        return (FT_FAILURE);
    if (*cursor == '"' || *cursor == '\'')
        return (FT_FAILURE);
    if (*cursor == '-' || std::isdigit(static_cast<unsigned char>(*cursor)))
    {
        if (*cursor == '-')
            cursor += 1;
        if (*cursor == '\0')
            return (FT_FAILURE);
        index = 0;
        while (cursor[index] != '\0')
        {
            if (!std::isdigit(static_cast<unsigned char>(cursor[index])))
                return (FT_FAILURE);
            index += 1;
        }
        ft_strlcpy(argument, "int", argument_size);
        return (FT_SUCCESS);
    }
    item = cblc_find_data_item_const(unit, cursor);
    if (!item || (item->kind != CBLC_DATA_KIND_INT
            && item->kind != CBLC_DATA_KIND_STRING
            && item->kind != CBLC_DATA_KIND_STRUCT))
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_INT)
        ft_strlcpy(argument, "int", argument_size);
    else if (item->kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(argument, "string", argument_size);
    else if (item->struct_type_name[0] != '\0')
        ft_strlcpy(argument, item->struct_type_name, argument_size);
    else
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_infer_template_function_argument(
    const t_cblc_translation_unit *unit, const t_cblc_function *template_function,
    const char *call_arguments, size_t call_argument_count,
    char *argument, size_t argument_size)
{
    t_cblc_statement statement;
    size_t parameter_index;
    int found_dependent_parameter;

    if (!unit || !template_function || !call_arguments || !argument
        || argument_size == 0
        || call_argument_count != template_function->parameter_count)
        return (FT_FAILURE);
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, call_arguments,
        sizeof(statement.call_arguments));
    statement.call_argument_count = call_argument_count;
    argument[0] = '\0';
    found_dependent_parameter = 0;
    parameter_index = 0;
    while (parameter_index < template_function->parameter_count)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];
        char candidate[TRANSPILE_IDENTIFIER_MAX];

        if (cblc_extract_call_argument(&statement, parameter_index, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::strcmp(template_function->parameters[parameter_index].type_name,
                template_function->template_parameter_name) == 0)
        {
            if (cblc_template_argument_from_call_expression(unit, expression,
                    candidate, sizeof(candidate)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] != '\0' && std::strcmp(argument, candidate) != 0)
                return (FT_FAILURE);
            ft_strlcpy(argument, candidate, argument_size);
            found_dependent_parameter = 1;
        }
        parameter_index += 1;
    }
    if (!found_dependent_parameter || argument[0] == '\0')
        return (FT_FAILURE);
    {
        t_cblc_data_kind kind;
        size_t length;

        if (cblc_template_argument_kind(unit, argument, &kind, &length)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}
