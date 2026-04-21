static int cblc_parse_function(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_function *function;
    t_cblc_data_item *return_item;
    t_cblc_function_return_kind return_kind;
    char return_type_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;
    size_t parameter_scope_base;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_function_return_type(cursor, unit, &return_kind,
            return_type_name, sizeof(return_type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (unit->function_count >= unit->function_capacity)
    {
        if (cblc_translation_unit_ensure_function_capacity(unit,
                unit->function_capacity == 0 ? 2 : unit->function_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    function = &unit->functions[unit->function_count];
    function->statements = NULL;
    function->statement_count = 0;
    function->statement_capacity = 0;
    function->parameter_count = 0;
    function->saw_return = 0;
    function->return_kind = return_kind;
    function->return_type_name[0] = '\0';
    function->return_item_index = -1;
    function->return_cobol_name[0] = '\0';
    function->return_source_name[0] = '\0';
    function->local_destructor_targets = NULL;
    function->local_destructor_count = 0;
    function->local_destructor_capacity = 0;
    function->source_name[0] = '\0';
    function->cobol_name[0] = '\0';
    ft_strlcpy(function->source_name, identifier, sizeof(function->source_name));
    cblc_identifier_to_cobol(identifier, function->cobol_name, sizeof(function->cobol_name));
    if (return_kind == CBLC_FUNCTION_RETURN_STRUCT
        || return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
        ft_strlcpy(function->return_type_name, return_type_name,
            sizeof(function->return_type_name));
    return_item = NULL;
    if (return_kind != CBLC_FUNCTION_RETURN_VOID)
    {
        return_item = cblc_create_return_item(unit, identifier, return_kind,
                function->return_type_name);
        if (!return_item)
            return (FT_FAILURE);
        function->return_item_index = static_cast<int>(return_item - unit->data_items);
        ft_strlcpy(function->return_cobol_name, return_item->cobol_name,
            sizeof(function->return_cobol_name));
        ft_strlcpy(function->return_source_name, return_item->source_name,
            sizeof(function->return_source_name));
    }
    index = unit->function_count;
    unit->function_count += 1;
    if (unit->entry_function_index == static_cast<size_t>(-1))
        unit->entry_function_index = index;
    if (std::strncmp(identifier, "main", sizeof(identifier)) == 0)
        unit->entry_function_index = index;
    if (unit->entry_function_index == index)
        ft_strlcpy(unit->program_name, function->cobol_name, sizeof(unit->program_name));
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    parameter_scope_base = unit->data_count;
    if (cblc_parse_function_parameters(cursor, unit, function) != FT_SUCCESS)
        return (FT_FAILURE);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    *cursor += 1;
    *cursor -= 1;
    if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    while (parameter_scope_base < unit->data_count)
    {
        if (unit->data_items[parameter_scope_base].is_alias)
            unit->data_items[parameter_scope_base].is_active = 0;
        parameter_scope_base += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_starts_with_function_declaration(const t_cblc_translation_unit *unit,
    const char *cursor)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char type_name[TRANSPILE_IDENTIFIER_MAX];
    const char *lookahead;
    t_cblc_function_return_kind return_kind;

    if (!unit || !cursor)
        return (0);
    lookahead = cursor;
    if (cblc_parse_function_return_type(&lookahead, unit, &return_kind,
            type_name, sizeof(type_name)) != FT_SUCCESS)
        return (0);
    if (return_kind == CBLC_FUNCTION_RETURN_STRUCT && type_name[0] == '\0')
        return (0);
    cblc_skip_whitespace(&lookahead);
    if (cblc_parse_identifier(&lookahead, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (0);
    cblc_skip_whitespace(&lookahead);
    if (*lookahead != '(')
        return (0);
    lookahead += 1;
    while (*lookahead != '\0' && *lookahead != ')')
        lookahead += 1;
    if (*lookahead != ')')
        return (0);
    lookahead += 1;
    cblc_skip_whitespace(&lookahead);
    if (*lookahead != '{')
        return (0);
    return (1);
}

void cblc_translation_unit_init(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->struct_types = NULL;
    unit->struct_type_count = 0;
    unit->struct_type_capacity = 0;
    unit->imports = NULL;
    unit->import_count = 0;
    unit->import_capacity = 0;
    unit->copy_includes = NULL;
    unit->copy_include_count = 0;
    unit->copy_include_capacity = 0;
    unit->functions = NULL;
    unit->function_count = 0;
    unit->function_capacity = 0;
    unit->entry_function_index = static_cast<size_t>(-1);
    unit->helper_literal_counter = 0;
    unit->helper_status_index = -1;
    unit->program_name[0] = '\0';
    cblc_register_builtin_string_type(unit);
}

void cblc_translation_unit_dispose(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    if (unit->data_items)
        cma_free(unit->data_items);
    if (unit->struct_types)
    {
        size_t index;

        index = 0;
        while (index < unit->struct_type_count)
        {
            if (unit->struct_types[index].fields)
                cma_free(unit->struct_types[index].fields);
            if (unit->struct_types[index].methods)
            {
                size_t method_index;

                method_index = 0;
                while (method_index < unit->struct_types[index].method_count)
                {
                    if (unit->struct_types[index].methods[method_index].statements)
                        cma_free(unit->struct_types[index].methods[method_index].statements);
                    method_index += 1;
                }
                cma_free(unit->struct_types[index].methods);
            }
            if (unit->struct_types[index].constructors)
            {
                size_t constructor_index;

                constructor_index = 0;
                while (constructor_index < unit->struct_types[index].constructor_count)
                {
                    if (unit->struct_types[index].constructors[constructor_index].statements)
                        cma_free(unit->struct_types[index].constructors[constructor_index].statements);
                    constructor_index += 1;
                }
                cma_free(unit->struct_types[index].constructors);
            }
            if (unit->struct_types[index].destructor_statements)
                cma_free(unit->struct_types[index].destructor_statements);
            unit->struct_types[index].fields = NULL;
            unit->struct_types[index].field_count = 0;
            unit->struct_types[index].field_capacity = 0;
            unit->struct_types[index].methods = NULL;
            unit->struct_types[index].method_count = 0;
            unit->struct_types[index].method_capacity = 0;
            unit->struct_types[index].constructors = NULL;
            unit->struct_types[index].constructor_count = 0;
            unit->struct_types[index].constructor_capacity = 0;
            unit->struct_types[index].destructor_statements = NULL;
            unit->struct_types[index].destructor_statement_count = 0;
            unit->struct_types[index].destructor_statement_capacity = 0;
            index += 1;
        }
        cma_free(unit->struct_types);
    }
    if (unit->imports)
        cma_free(unit->imports);
    if (unit->copy_includes)
        cma_free(unit->copy_includes);
    if (unit->functions)
    {
        size_t index;

        index = 0;
        while (index < unit->function_count)
        {
            if (unit->functions[index].statements)
                cma_free(unit->functions[index].statements);
            if (unit->functions[index].local_destructor_targets)
                cma_free(unit->functions[index].local_destructor_targets);
            unit->functions[index].statements = NULL;
            unit->functions[index].statement_count = 0;
            unit->functions[index].statement_capacity = 0;
            unit->functions[index].local_destructor_targets = NULL;
            unit->functions[index].local_destructor_count = 0;
            unit->functions[index].local_destructor_capacity = 0;
            index += 1;
        }
        cma_free(unit->functions);
    }
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->struct_types = NULL;
    unit->struct_type_count = 0;
    unit->struct_type_capacity = 0;
    unit->imports = NULL;
    unit->import_count = 0;
    unit->import_capacity = 0;
    unit->copy_includes = NULL;
    unit->copy_include_count = 0;
    unit->copy_include_capacity = 0;
    unit->functions = NULL;
    unit->function_count = 0;
    unit->function_capacity = 0;
    unit->entry_function_index = static_cast<size_t>(-1);
    unit->helper_literal_counter = 0;
    unit->helper_status_index = -1;
    unit->program_name[0] = '\0';
}

int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit)
{
    const char *cursor;

    if (!text || !unit)
        return (FT_FAILURE);
    g_cblc_constructor_parse_state.type = NULL;
    g_cblc_constructor_parse_state.initialized_fields = NULL;
    g_cblc_constructor_parse_state.active = 0;
    g_cblc_member_access_type = NULL;
    cursor = text;
    while (cursor && *cursor != '\0')
    {
        cblc_skip_whitespace(&cursor);
        if (*cursor == '\0')
            break ;
        if (cblc_match_keyword(&cursor, "copy"))
        {
            cursor -= std::strlen("copy");
            if (cblc_parse_copy(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "import"))
        {
            cursor -= std::strlen("import");
            if (cblc_parse_import(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_qualified_lifecycle_definition(&cursor, unit) == FT_SUCCESS)
            continue ;
        if (cblc_parse_qualified_method_definition(&cursor, unit) == FT_SUCCESS)
            continue ;
        if (cblc_starts_with_function_declaration(unit, cursor))
        {
            if (cblc_parse_function(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "struct"))
        {
            cursor -= std::strlen("struct");
            if (cblc_parse_struct_definition(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "class"))
        {
            cursor -= std::strlen("class");
            if (cblc_parse_struct_definition(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "const"))
        {
            cursor -= std::strlen("const");
            if (!cblc_match_keyword(&cursor, "const"))
                return (FT_FAILURE);
            cblc_skip_whitespace(&cursor);
            if (cblc_match_keyword(&cursor, "string"))
            {
                cursor -= std::strlen("string");
                if (cblc_parse_string_declaration(&cursor, unit, 1) != FT_SUCCESS)
                    return (FT_FAILURE);
                continue ;
            }
            if (cblc_match_keyword(&cursor, "char"))
            {
                cursor -= std::strlen("char");
                if (cblc_parse_char_declaration(&cursor, unit, 1) != FT_SUCCESS)
                    return (FT_FAILURE);
                continue ;
            }
            if (cblc_match_keyword(&cursor, "int"))
            {
                cursor -= std::strlen("int");
                if (cblc_parse_int_declaration(&cursor, unit, 1) != FT_SUCCESS)
                    return (FT_FAILURE);
                continue ;
            }
            return (FT_FAILURE);
        }
        if (cblc_match_keyword(&cursor, "void"))
        {
            cursor -= std::strlen("void");
            if (cblc_parse_pointer_declaration(&cursor, unit, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "string"))
        {
            cursor -= std::strlen("string");
            if (cblc_parse_string_declaration(&cursor, unit, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "char"))
        {
            cursor -= std::strlen("char");
            if (cblc_parse_pointer_declaration(&cursor, unit, 0) == FT_SUCCESS)
                continue ;
            if (cblc_parse_char_declaration(&cursor, unit, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "int"))
        {
            cursor -= std::strlen("int");
            if (cblc_parse_pointer_declaration(&cursor, unit, 0) == FT_SUCCESS)
                continue ;
            if (cblc_parse_int_declaration(&cursor, unit, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_struct_pointer_declaration(&cursor, unit, 0) == FT_SUCCESS)
            continue ;
        if (cblc_parse_struct_instance_declaration(&cursor, unit) == FT_SUCCESS)
            continue ;
        return (FT_FAILURE);
    }
    if (unit->function_count > 0)
    {
        if (unit->entry_function_index == static_cast<size_t>(-1)
            || unit->entry_function_index >= unit->function_count)
            unit->entry_function_index = 0;
        if (unit->program_name[0] == '\0')
            ft_strlcpy(unit->program_name,
                unit->functions[unit->entry_function_index].cobol_name,
                sizeof(unit->program_name));
    }
    else
    {
        unit->entry_function_index = static_cast<size_t>(-1);
        if (unit->program_name[0] == '\0')
            ft_strlcpy(unit->program_name, "MAIN", sizeof(unit->program_name));
    }
    if (cblc_inject_entry_lifecycle(unit) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
