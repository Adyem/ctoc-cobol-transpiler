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
        return (FT_FAILURE);
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
    if (cblc_find_struct_type(unit, identifier))
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
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            ft_strlcpy(item->struct_type_name, type->fields[index].struct_type_name,
                sizeof(item->struct_type_name));
            unit->data_count += 1;
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
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

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
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
    item->is_active = 1;
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
    if (!cblc_find_struct_type(unit, identifier))
    {
        *cursor = start;
        return (FT_FAILURE);
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
        if (!self_type
            || std::strncmp(identifier, self_type->source_name, sizeof(identifier)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
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
    if (kind == CBLC_DATA_KIND_STRUCT)
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
        if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                type_name, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_struct_instance_field_items(unit, struct_type, actual_source_name,
                actual_cobol_name, owner_function_name, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_local_struct_alias_items(unit, struct_type, parameter_name, actual_cobol_name)
            != FT_SUCCESS)
            return (FT_FAILURE);
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
        if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                type_name, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_named_data_item(unit, parameter_name, actual_cobol_name, kind, 0, 0,
                type_name, NULL, 0, 1, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                NULL, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_named_data_item(unit, parameter_name, actual_cobol_name, kind, 0, 0,
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

            if (self_type
                && std::strncmp(parameters[index].type_name, self_type->source_name,
                    sizeof(parameters[index].type_name)) == 0)
                type = self_type;
            else
                type = cblc_find_struct_type(unit, parameters[index].type_name);
            if (!type)
                return (FT_FAILURE);
            if (cblc_add_local_struct_alias_items(unit, type, parameters[index].source_name,
                    parameters[index].cobol_name) != FT_SUCCESS)
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
